# OUTPUTS
#   *_with_phones.csv - deduplicated caller sheet for phone bank staff
#   *_contribution_history.csv - one row per transaction for RFM engineering
#   *_model_features.csv - one row per matched donor for modeling

# ENTRY POINTS
#   validate_inputs() - pre run checks, no API calls
#   main() - full run; resumes from checkpoint if present
#   main(fresh_start = TRUE) - ignore checkpoint, start from record 1

library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(purrr)

# glmnet required downstream for the double-hurdle model - see README.md
# install.packages("glmnet")

# CONFIGURATION

# Credentials are loaded from environment variables - never stored in this file
# See README.md for setup instructions
VAN_API_KEY  <- Sys.getenv("VAN_API_KEY",  unset = "NOT_SET")
VAN_APP_NAME <- Sys.getenv("VAN_APP_NAME", unset = "NOT_SET")
VAN_BASE_URL <- "https://api.securevan.com/v4"

# Mode 0 = My Voters (voter file: party reg, scores, turnout)
# Mode 1 = My Campaign (CRM: contributions, contacts)
HAS_VOTERFILE_ACCESS <- TRUE
VAN_MODE_VOTERFILE   <- "0"
VAN_MODE_MYCAMPAIGN  <- "1"

# Rate limiting - per VAN API documentation (see README)
PERSON_SEARCH_DELAY <- 0.5
GENERAL_DELAY       <- 0.5

# Match confidence threshold. Records below this score are rejected
# 90-100 = exact, 70-89 = high, 50-69 = medium
MIN_CONFIDENCE_THRESHOLD <- 50

# Time budget: loop stops and checkpoints when elapsed minutes exceeds this
TIME_BUDGET_MINS    <- 55
CHECKPOINT_INTERVAL <- 25

# File paths
INPUT_FILE  <- "XXX.csv"
OUTPUT_FILE <- "XXX.csv"
LOG_FILE    <- "XXX.txt"

CONTRIB_HISTORY_FILE <- sub("\\.csv$", "_contribution_history.csv", OUTPUT_FILE)
MODEL_FEATURES_FILE  <- sub("\\.csv$", "_model_features.csv",       OUTPUT_FILE)
CHECKPOINT_FILE      <- sub("\\.csv$", "_checkpoint.rds",           OUTPUT_FILE)

REQUIRED_INPUT_COLS <- c("contributor_name", 
                         "contributor_street_1", 
                         "contributor_city",
                         "contributor_state", 
                         "contributor_zip",
                         "contribution_receipt_amount", 
                         "contribution_receipt_date")

# UTILITY FUNCTIONS

log_message <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  entry     <- paste0("[", timestamp, "] [", level, "] ", message)
  cat(entry, "\n")
  tryCatch(cat(entry, "\n", file = LOG_FILE, append = TRUE),
           error = function(e) invisible(NULL))
}

`%||%` <- function(a, b) {
  if (is.null(a) || (length(a) == 1 && is.na(a))) b
  else a
}

ensure_dir <- function(filepath) {
  dir <- dirname(filepath)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

elapsed_str <- function(start_time) {
  secs <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  if (secs < 60)   
    return(sprintf("%.0f sec", secs))
  if (secs < 3600) 
    return(sprintf("%.1f min", secs / 60))
  return(sprintf("%.1f hr",  secs / 3600))
}

mins_remaining <- function(start_time, budget_mins) {
  budget_mins - as.numeric(difftime(Sys.time(), start_time, units = "mins"))
}

# VAN API REQUEST
# Credentials are read from environment variables set in private file

van_api_request <- function(endpoint, 
                            method = "GET", 
                            body = NULL,
                            mode = VAN_MODE_VOTERFILE,
                            delay_secs = GENERAL_DELAY) {
  url      <- paste0(VAN_BASE_URL, endpoint)
  password <- paste0(gsub("\\|[01]$", "", VAN_API_KEY), "|", mode)
  auth     <- authenticate(user = VAN_APP_NAME, password = password)
  headers  <- add_headers(`Content-Type` = "application/json",
                          `Accept`       = "application/json")

  log_message(sprintf("[%s] %s", method, endpoint), "DEBUG")

  response <- tryCatch({
    if (method == "GET")
      GET(url, auth, headers, timeout(30))
    else
      POST(url, auth, headers,
           body = toJSON(body, auto_unbox = TRUE), timeout(30))
  }, error = function(e) {
    log_message(paste("Network error:", e$message), "ERROR")
    NULL
  })

  Sys.sleep(delay_secs)
  if (is.null(response)) return(NULL)

  status <- status_code(response)

  if (status %in% c(200L, 201L)) {
    raw <- content(response, as = "text", encoding = "UTF-8")
    if (nchar(trimws(raw)) == 0) return(list())
    return(tryCatch(fromJSON(raw, simplifyVector = FALSE),
                    error = function(e) list()))
  }

  if (status == 404L) {
    log_message(sprintf("404 No match: %s", endpoint), "DEBUG")
    return(NULL)
  }

  if (status == 429L) {
    retry_after <- min(max(as.integer(headers(response)[["retry-after"]]) %||% 30L, 5L), 60L)
    log_message(sprintf("Rate limit (429) — waiting %d sec", retry_after), "WARNING")
    Sys.sleep(retry_after)
    retry <- tryCatch({
      if (method == "GET") GET(url, auth, headers, timeout(30))
      else POST(url, 
                auth, 
                headers,
                body = toJSON(body, auto_unbox = TRUE), 
                timeout(30))
    }, error = function(e) NULL)
    Sys.sleep(delay_secs)
    if (!is.null(retry) && status_code(retry) %in% c(200L, 201L)) {
      raw <- content(retry, as = "text", encoding = "UTF-8")
      if (nchar(trimws(raw)) == 0) return(list())
      return(tryCatch(fromJSON(raw, simplifyVector = FALSE), error = function(e) list()))
    }
    return(NULL)
  }

  if (status == 401L) {
    log_message(sprintf("401 Unauthorized — check credentials (mode %s)", mode), "ERROR")
    return(NULL)
  }

  if (status == 403L) {
    log_message(sprintf("403 Forbidden — Tier 2+ access required: %s", endpoint), "ERROR")
    return(NULL)
  }

  log_message(sprintf("API error %d: %s", status, endpoint), "ERROR")
  NULL
}

# PRE RUN VALIDATION
# Checks credentials, input file, required columns, and output directory

validate_inputs <- function(input_file = INPUT_FILE, output_file = OUTPUT_FILE) {
  cat("\n PRE-FLIGHT VALIDATION \n\n")
  passed <- TRUE

  if (VAN_API_KEY == "NOT_SET" || VAN_APP_NAME == "NOT_SET") {
    cat("FAIL Credentials not set - add VAN_API_KEY and VAN_APP_NAME to .Renviron\n")
    passed <- FALSE
  } else {
    cat("OK  Credentials found\n")
  }

  if (!file.exists(input_file)) {
    cat(sprintf("FAIL  Input file not found: %s\n", input_file))
    return(invisible(FALSE))
  }
  cat(sprintf("OK  Input file: %s\n", input_file))

  cols <- tryCatch(names(read_csv(input_file, show_col_types = FALSE, n_max = 1)),
                   error = function(e) NULL)
  if (!is.null(cols)) {
    missing <- setdiff(REQUIRED_INPUT_COLS, cols)
    if (length(missing) > 0) {
      cat("FAIL  Missing columns:", paste(missing, collapse = ", "), "\n")
      passed <- FALSE
    } else {
      cat(sprintf("OK  All %d required columns present\n", length(REQUIRED_INPUT_COLS)))
    }
  }

  out_dir <- dirname(output_file)
  if (!dir.exists(out_dir)) {
    cat(sprintf("INFO  Output directory will be created: %s\n", out_dir))
  } else {
    cat(sprintf("OK  Output directory: %s\n", out_dir))
  }

  if (file.exists(CHECKPOINT_FILE)) {
    cp <- tryCatch(readRDS(CHECKPOINT_FILE), error = function(e) NULL)
    if (!is.null(cp))
      cat(sprintf("INFO  Checkpoint found - %d/%d records done; main() will resume\n",
                  cp$last_index, cp$total_records))
  }

  full <- tryCatch(read_csv(input_file, show_col_types = FALSE), error = function(e) NULL)
  if (!is.null(full)) {
    n        <- nrow(full)
    n_noname <- sum(is.na(full$contributor_name) | full$contributor_name == "", na.rm = TRUE)
    est_mins <- round(n * (PERSON_SEARCH_DELAY + 4 * GENERAL_DELAY) / 60, 1)
    cat(sprintf("\nRecords: %d | Missing name: %d | Est. runtime: %.1f min\n",
                n, n_noname, est_mins))
    if (n > 2000)
      cat("WARNING")
  }

  cat(sprintf("\nResult: %s\n\n",
              if (passed) "PASSED - run main()" 
              else "FAILED - fix issues above"))
  invisible(passed)
}

# MATCH CONFIDENCE SCORING
# Scores 0-100 based on name and address agreement between the original record
# and the VAN candidate returned by POST /people/find.
# Weights: last name 30 | first name 20 | street 20 | city 10 | ZIP 20

calculate_match_score <- function(search_first, 
                                  search_last,
                                  search_street, 
                                  search_city, 
                                  search_zip,
                                  result_first, 
                                  result_last,
                                  result_street, 
                                  result_city, 
                                  result_zip) {
  score <- 0L

  if (!is.na(search_last) && !is.na(result_last)) {
    if (tolower(search_last) == tolower(result_last)) score <- score + 30L
    else if (agrepl(search_last, result_last, max.distance = 1L, ignore.case = TRUE))
      score <- score + 15L
  }
  if (!is.na(search_first) && !is.na(result_first)) {
    if (tolower(search_first) == tolower(result_first)) score <- score + 20L
    else if (agrepl(search_first, result_first, max.distance = 1L, ignore.case = TRUE))
      score <- score + 10L
  }

  if (!is.na(search_street) && !is.na(result_street)) {
    sn <- tolower(gsub("[^a-z0-9]", "", search_street))
    rn <- tolower(gsub("[^a-z0-9]", "", result_street))
    if (sn == rn) score <- score + 20L
    else if (grepl(sn, rn, fixed = TRUE) || grepl(rn, sn, fixed = TRUE))
      score <- score + 10L
  }
  if (!is.na(search_city) && !is.na(result_city))
    if (tolower(search_city) == tolower(result_city)) score <- score + 10L

  if (!is.na(search_zip) && !is.na(result_zip)) {
    sz <- substr(gsub("[^0-9]", "", as.character(search_zip)), 1L, 5L)
    rz <- substr(gsub("[^0-9]", "", as.character(result_zip)), 1L, 5L)
    if (nchar(sz) == 5L && sz == rz) score <- score + 20L
  }

  as.numeric(score)
}

# VAN DATA RETRIEVAL
# Each function targets one endpoint 

get_person_details <- function(van_id) {
  result <- van_api_request(
    paste0("/people/", van_id),
    mode = if (HAS_VOTERFILE_ACCESS) VAN_MODE_VOTERFILE else VAN_MODE_MYCAMPAIGN,
    delay_secs = GENERAL_DELAY
  )
  if (is.null(result) || length(result) == 0) return(NULL)

  addr <- if (!is.null(result$addresses) && length(result$addresses) > 0)
            result$addresses[[1]] else list()

  list(van_id = van_id,
       first_name = result$firstName %||% NA_character_,
       last_name = result$lastName %||% NA_character_,
       street = addr$addressLine1 %||% NA_character_,
       city = addr$city %||% NA_character_,
       zip = addr$zipOrPostalCode %||% NA_character_,
       party_reg = result$party$partyAbbreviation %||% NA_character_,
       date_of_birth = result$dateOfBirth %||% NA_character_,
       sex  = result$sex %||% NA_character_)
}

search_van_person <- function(first_name, 
                              last_name, 
                              street_address,
                              city, 
                              state, 
                              zip,
                              min_confidence = MIN_CONFIDENCE_THRESHOLD) {
  no_match <- list(van_id = NA, 
                   match_found = FALSE, 
                   confidence = NA,
                   match_type = "no_match", 
                   matched_name = NA_character_,
                   matched_address = NA_character_, 
                   party_reg = NA_character_,
                   date_of_birth = NA_character_, 
                   sex = NA_character_)

  result <- van_api_request("/people/find",
                            method = "POST",
                            body = list(firstName = first_name,
                                        lastName = last_name,
                                        addresses = list(list(addressLine1 = street_address,
                                                              city = city,
                                                              state = state,
                                                              zipOrPostalCode = zip)) ),
                            mode = VAN_MODE_MYCAMPAIGN,
                            delay_secs = PERSON_SEARCH_DELAY)

  if (is.null(result) || length(result) == 0 || is.null(result$vanId))
    return(no_match)

  van_id  <- result$vanId
  details <- get_person_details(van_id)

  if (is.null(details)) {
    log_message(sprintf("Could not retrieve details for VAN ID %s", van_id), "WARNING")
    return(list(van_id = van_id, match_found = TRUE, confidence = 50,
                match_type = "unverified", matched_name = NA_character_,
                matched_address = NA_character_, party_reg = NA_character_,
                date_of_birth = NA_character_, sex = NA_character_))
  }

  confidence <- calculate_match_score(first_name, 
                                      last_name, 
                                      street_address, 
                                      city, 
                                      zip,
                                      details$first_name, 
                                      details$last_name,
                                      details$street, 
                                      details$city,
                                      details$zip)

  match_type <- if (confidence >= 90) "exact_match"
                else if (confidence >= 70) "high_confidence"
                else if (confidence >= 50) "medium_confidence"
                else "low_confidence"

  if (confidence < min_confidence) {
    log_message(sprintf("Rejected: %.0f%% < threshold %d%%", confidence, min_confidence), "DEBUG")
    return(modifyList(no_match, list(confidence = confidence,
                                    match_type = "below_threshold")))
  }

  list(van_id = van_id,
       match_found = TRUE,
       confidence = confidence,
       match_type = match_type,
       matched_name = paste(details$first_name, details$last_name),
       matched_address = details$street,
       party_reg = details$party_reg     %||% NA_character_,
       date_of_birth = details$date_of_birth %||% NA_character_,
       sex = details$sex           %||% NA_character_)
}

get_van_phones <- function(van_id) {
  empty <- list(cell_phone = NA_character_, home_phone = NA_character_)
  if (is.na(van_id)) return(empty)

  result <- van_api_request(paste0("/people/", van_id, "/phones"),
                            mode = VAN_MODE_MYCAMPAIGN, delay_secs = GENERAL_DELAY)
  cell <- NA_character_; home <- NA_character_

  if (!is.null(result) && length(result) > 0) {
    for (p in result) {
      type <- p$phoneType$name %||% ""
      pref <- isTRUE(p$isPreferred)
      num  <- p$phoneNumber
      if (type %in% c("Cell", "Mobile")) { if (is.na(cell) || pref) cell <- num }
      else if (type == "Home")           { if (is.na(home) || pref) home <- num }
    }
  }
  list(cell_phone = cell, home_phone = home)
}

get_van_contributions <- function(van_id) {
  if (is.na(van_id)) return(NULL)

  result <- van_api_request(paste0("/people/", van_id, "/contributions"),
                            mode = VAN_MODE_MYCAMPAIGN, delay_secs = GENERAL_DELAY)
  if (is.null(result) || length(result) == 0) return(NULL)

  df <- bind_rows(lapply(result, function(c) {
    data.frame(van_id = as.character(van_id),
               contrib_amount = as.numeric(c$amount %||% NA),
               contrib_date = as.character(c$dateReceived %||% NA),
               batch_id = as.character(c$financialBatchId %||% NA),
               recipient_name = as.character(c$recipientName %||% NA),
               committee_name = as.character(c$committeeName %||% NA),
               election_cycle = as.character(c$electionCycle %||% NA),
               contrib_type   = as.character(c$contributionType$name %||% NA),
               stringsAsFactors = FALSE)
  }))
  df$contrib_date <- as.Date(substr(df$contrib_date, 1, 10), format = "%Y-%m-%d")
  df
}

get_van_scores <- function(van_id) {
  empty <- list(van_voter_score = NA_real_, van_donor_score = NA_real_)
  if (is.na(van_id) || !HAS_VOTERFILE_ACCESS) return(empty)

  result <- van_api_request(paste0("/people/", van_id, "/scores"),
                            mode = VAN_MODE_VOTERFILE, delay_secs = GENERAL_DELAY)
  if (is.null(result) || length(result) == 0) return(empty)

  score_df <- bind_rows(lapply(result, function(s) {
    data.frame(name  = as.character(s$name  %||% NA),
               value = as.numeric(s$value   %||% NA),
               stringsAsFactors = FALSE)
  }))

  get_s <- function(pat) {
    m <- score_df[grepl(pat, score_df$name, ignore.case = TRUE, perl = TRUE), ]
    if (nrow(m) > 0) m$value[1] else NA_real_
  }
  # Pattern strings match VAN organisation's score names
  list(van_voter_score = get_s("voter"),
       van_donor_score = get_s("donor|giving|contribution"))
}

get_voter_history <- function(van_id) {
  empty <- list(turnout_rate = NA_real_, n_voted = NA_integer_)
  if (is.na(van_id) || !HAS_VOTERFILE_ACCESS) return(empty)

  result <- van_api_request(paste0("/people/", van_id, "/voterHistory"),
                            mode = VAN_MODE_VOTERFILE, delay_secs = GENERAL_DELAY)
  if (is.null(result) || length(result) == 0) return(empty)

  n_voted <- length(result)
  vote_years <- as.integer(na.omit(sapply(result, function(e) {
    raw <- e$electionDate %||% e$electionYear %||% NA
    if (!is.na(raw)) as.integer(substr(as.character(raw), 1, 4)) else NA_integer_
  })))

  if (length(vote_years) == 0) return(list(turnout_rate = NA_real_, n_voted = n_voted))

  eligible <- as.integer(format(Sys.Date(), "%Y")) - min(vote_years) + 1L
  turnout_rate <- min(round(n_voted / eligible, 3), 1.0)
  list(turnout_rate = turnout_rate, n_voted = n_voted)
}

# BATCH CONTRIBUTION PULL
# Pulls full contribution history for all matched donors after enrichment

pull_all_contributions <- function(donors_enriched) {
  ids <- donors_enriched %>%
    filter(match_status == "matched", !is.na(van_id)) %>%
    distinct(van_id) %>%
    pull(van_id)

  log_message(sprintf("Pulling contributions for %d matched donors...", length(ids)))

  result <- map_dfr(ids, function(id) {
    r <- get_van_contributions(id)
    Sys.sleep(GENERAL_DELAY)
    r
  })

  if (nrow(result) == 0)
    log_message("No contributions retrieved - confirm MyCampaign (mode 1) access", "WARNING")
  else
    log_message(sprintf("Total contribution records: %d", nrow(result)))
  result
}

# DATA LOADING
# Reads original CSV and parses contributor names into first/last/middle
# Original name format is "LAST, FIRST MIDDLE" - both comma and space formats handled

parse_original_name <- function(full_name) {
  if (is.na(full_name) || full_name == "")
    return(list(first_name = NA_character_, middle_name = NA_character_,
                last_name  = NA_character_))

  name <- str_trim(full_name)

  if (str_detect(name, ",")) {
    parts <- str_split(name, ",")[[1]]
    last_name <- str_trim(parts[1])
    fmp <- str_split(str_trim(parts[2]), "\\s+")[[1]]
    first_name <- if (length(fmp) > 0) fmp[1] else NA_character_
    middle_name <- if (length(fmp) > 1) paste(fmp[-1], collapse = " ") else NA_character_
  } else {
    parts <- str_split(name, "\\s+")[[1]]
    first_name <- if (length(parts) > 0) parts[1] else NA_character_
    last_name <- if (length(parts) > 1) parts[length(parts)] else NA_character_
    middle_name <- if (length(parts) > 2)
                     paste(parts[2:(length(parts) - 1)], collapse = " ")
                   else NA_character_
  }

  list(first_name = first_name, middle_name = middle_name, last_name = last_name)
}

process_donor_data <- function(input_file) {
  log_message(paste("Reading:", input_file))
  donors <- read_csv(input_file, show_col_types = FALSE)
  log_message(sprintf("Records read: %d", nrow(donors)))

  donors <- donors %>%
    rowwise() %>%
    mutate(
      name_parsed = list(parse_original_name(contributor_name)),
      first_name = name_parsed$first_name,
      last_name = name_parsed$last_name,
      middle_name = name_parsed$middle_name
    ) %>%
    ungroup() %>%
    select(-name_parsed) %>%
    mutate(
      street_address = if_else(
        !is.na(contributor_street_2) & contributor_street_2 != "",
        paste(contributor_street_1, contributor_street_2),
        contributor_street_1
      ),
      city  = contributor_city,
      state = contributor_state,
      zip   = str_extract(as.character(contributor_zip), "^\\d{5}")
    )

  donors
}

# CHECKPOINT FUNCTIONS
save_checkpoint <- function(donors, last_index, total_records, checkpoint_file) {
  tryCatch({
    ensure_dir(checkpoint_file)
    saveRDS(list(donors = donors,
                 last_index = last_index,
                 total_records = total_records,
                 saved_at = Sys.time(),
                 script_version = "v5"),
            checkpoint_file)
    log_message(sprintf("Checkpoint saved: %d/%d", last_index, total_records))
  }, error = function(e)
    log_message(paste("Checkpoint save failed:", e$message), "WARNING"))
}

load_checkpoint <- function(checkpoint_file) {
  if (!file.exists(checkpoint_file)) return(NULL)
  cp <- tryCatch(readRDS(checkpoint_file),
                 error = function(e) {
                   log_message(paste("Checkpoint load failed:", e$message), "WARNING")
                   NULL
                 })
  if (!is.null(cp))
    log_message(sprintf("Checkpoint: %d/%d records done (saved %s)",
                        cp$last_index, cp$total_records,
                        format(cp$saved_at, "%Y-%m-%d %H:%M")))
  cp
}

delete_checkpoint <- function(checkpoint_file) {
  if (file.exists(checkpoint_file)) {
    file.remove(checkpoint_file)
    log_message("Checkpoint deleted")
  }
}

# ENRICHMENT LOOP
# Iterates over donor records, calls VAN for each, and writes enriched fields
# Checkpoints every CHECKPOINT_INTERVAL records and respects TIME_BUDGET_MINS

enrich_donors <- function(donors,
                          min_confidence = MIN_CONFIDENCE_THRESHOLD,
                          time_budget_mins = TIME_BUDGET_MINS,
                          checkpoint_file = CHECKPOINT_FILE,
                          resume_donors = NULL,
                          start_index = 1L) {
  loop_start <- Sys.time()
  total      <- nrow(donors)

  if (!is.null(resume_donors)) {
    donors <- resume_donors
  } else {
    donors <- donors %>%
      mutate(
        van_id = NA_character_,
        van_cell_phone = NA_character_,
        van_home_phone = NA_character_,
        match_status = "not_searched",
        match_confidence = NA_real_,
        match_type = NA_character_,
        matched_name = NA_character_,
        matched_address = NA_character_,
        party_reg = NA_character_,
        date_of_birth = NA_character_,
        sex = NA_character_,
        van_voter_score = NA_real_,
        van_donor_score = NA_real_,
        turnout_rate = NA_real_,
        n_elections_voted = NA_integer_)
  }

  budget_active <- is.finite(time_budget_mins)
  time_exceeded <- FALSE

  for (i in seq(start_index, total)) {
    log_message(sprintf("Record %d / %d", i, total))

    first <- donors$first_name[i]
    last <- donors$last_name[i]
    street <- donors$street_address[i]
    city <- donors$city[i]
    state <- donors$state[i]
    zip <- donors$zip[i]

    if (is.na(first) || is.na(last) || is.na(street)) {
      donors$match_status[i] <- "insufficient_data"
      next
    }

    sr <- search_van_person(first, 
                            last, 
                            street, 
                            city, 
                            state, 
                            zip, 
                            min_confidence)
    donors$match_type[i] <- sr$match_type
    donors$match_confidence[i] <- sr$confidence

    if (isTRUE(sr$match_found)) {
      vid <- as.character(sr$van_id)
      donors$van_id[i] <- vid
      donors$match_status[i] <- "matched"
      donors$matched_name[i] <- sr$matched_name %||% NA_character_
      donors$matched_address[i] <- sr$matched_address %||% NA_character_
      donors$party_reg[i] <- sr$party_reg %||% NA_character_
      donors$date_of_birth[i] <- sr$date_of_birth %||% NA_character_
      donors$sex[i] <- sr$sex %||% NA_character_

      ph <- get_van_phones(vid)
      donors$van_cell_phone[i] <- ph$cell_phone
      donors$van_home_phone[i] <- ph$home_phone

      sc <- get_van_scores(vid)
      donors$van_voter_score[i] <- sc$van_voter_score
      donors$van_donor_score[i] <- sc$van_donor_score

      vh <- get_voter_history(vid)
      donors$turnout_rate[i] <- vh$turnout_rate
      donors$n_elections_voted[i] <- vh$n_voted

      log_message(sprintf("Matched: %s %s -> VAN %s (%.0f%%)",
                          first, last, vid, sr$confidence))
    } else {
      donors$match_status[i] <- "no_match"
      log_message(sprintf("No match: %s %s", first, last))
    }

    if (!is.null(checkpoint_file) && i %% CHECKPOINT_INTERVAL == 0) {
      save_checkpoint(donors, i, total, checkpoint_file)

      matched_so_far <- sum(donors$match_status[seq_len(i)] == "matched", na.rm = TRUE)
      rem_str <- if (budget_active)
        sprintf("%.0f min remaining", mins_remaining(loop_start, time_budget_mins))
      else "no time limit"
      cat(sprintf("\n  [%s] %d/%d (%.0f%%) | Matched: %d | %s\n\n",
                  format(Sys.time(), "%H:%M:%S"),
                  i, total, i / total * 100, matched_so_far, rem_str))

      if (budget_active && mins_remaining(loop_start, time_budget_mins) <= 0) {
        log_message(sprintf("Time budget reached - stopping at record %d/%d", i, total), "WARNING")
        time_exceeded <- TRUE
        break
      }
    }
  }

  processed <- sum(donors$match_status != "not_searched", na.rm = TRUE)
  matched   <- sum(donors$match_status == "matched", na.rm = TRUE)
  log_message(sprintf("Enrichment complete: %d processed, %d matched (%.1f%%)",
                      processed, matched, matched / max(processed, 1) * 100))

  if (time_exceeded)
    log_message(sprintf("Incomplete: %d records remain — run main() to resume",
                        total - processed), "WARNING")
  else if (!is.null(checkpoint_file) && file.exists(checkpoint_file))
    delete_checkpoint(checkpoint_file)

  attr(donors, "time_exceeded") <- time_exceeded
  donors
}

# SAVE RESULTS
# Writes the caller sheet and model features file
# Both paths get a _PARTIAL suffix if the run ended before completion

save_results <- function(donors_dedup, output_file, partial = FALSE) {
  add_suffix <- function(path)
    if (partial) sub("(\\.csv)$", "_PARTIAL\\1", path) else path

  caller_cols <- intersect(
    c("contribution_receipt_date", 
      "contribution_receipt_amount",
      "contributor_name", 
      "contributor_street_1", 
      "contributor_city",
      "contributor_state", 
      "contributor_zip",
      "contributor_occupation", 
      "contributor_employer",
      "van_cell_phone", 
      "van_home_phone", 
      "van_id",
      "match_status", 
      "match_confidence", 
      "match_type",
      "party_reg", 
      "van_voter_score",
      "turnout_rate"),
    names(donors_dedup)
  )
  caller_path <- add_suffix(output_file)
  write_csv(donors_dedup %>% select(all_of(caller_cols)), caller_path)
  log_message(sprintf("Caller sheet: %s (%d rows)", caller_path, nrow(donors_dedup)))

  model_cols <- intersect(
    c("van_id", 
      "first_name", 
      "last_name", 
      "zip", 
      "city", 
      "state",
      "party_reg", 
      "date_of_birth", 
      "sex",
      "van_voter_score", 
      "van_donor_score",
      "turnout_rate", 
      "n_elections_voted",
      "contributor_occupation",
      "contributor_employer",
      "match_confidence", 
      "match_type"),
    names(donors_dedup))
  feat_path <- add_suffix(MODEL_FEATURES_FILE)
  write_csv(
    donors_dedup %>% filter(match_status == "matched") %>% select(all_of(model_cols)), feat_path)
  log_message(sprintf("Model features: %s (%d rows)", feat_path,
                      sum(donors_dedup$match_status == "matched")))

  invisible(NULL)
}

# MAIN
main <- function(input_file = INPUT_FILE,
                 output_file = OUTPUT_FILE,
                 min_confidence = MIN_CONFIDENCE_THRESHOLD,
                 time_budget_mins = TIME_BUDGET_MINS,
                 fresh_start = FALSE) {

  session_start <- Sys.time()
  ensure_dir(output_file)
  ensure_dir(LOG_FILE)
  
  log_message(sprintf(" VAN Donor Enrichment v5 - %s",
                      format(session_start, "%Y-%m-%d %H:%M")))
  log_message(sprintf(" Input: %s", input_file))
  log_message(sprintf(" Time budget: %s min | Voter file access: %s",
                      time_budget_mins, HAS_VOTERFILE_ACCESS))

  # Phase 1: Load
  donors_full <- process_donor_data(input_file)
  total_records <- nrow(donors_full)

  # Phase 2: Resume check
  resume_donors <- NULL
  start_index <- 1L

  if (!fresh_start) {
    cp <- load_checkpoint(CHECKPOINT_FILE)
    if (!is.null(cp)) {
      if (cp$total_records != total_records) {
        log_message("Checkpoint record count mismatch - ignoring", "WARNING")
      } else if (cp$last_index >= total_records) {
        resume_donors <- cp$donors
        start_index   <- total_records + 1L
      } else {
        resume_donors <- cp$donors
        start_index   <- cp$last_index + 1L
        log_message(sprintf("Resuming from record %d / %d", start_index, total_records))
      }
    }
  } else {
    delete_checkpoint(CHECKPOINT_FILE)
  }

  # Phase 3: Enrichment
  if (start_index <= total_records) {
    est_mins <- round((total_records - start_index + 1) *
                        (PERSON_SEARCH_DELAY + 4 * GENERAL_DELAY) / 60, 1)
    log_message(sprintf("Records to process: %d | Est. %.1f min",
                        total_records - start_index + 1, est_mins))

    donors_enriched <- enrich_donors(donors_full, 
                                     min_confidence, 
                                     time_budget_mins,
                                     CHECKPOINT_FILE, 
                                     resume_donors, 
                                     start_index)
  } else {
    donors_enriched <- resume_donors
    attr(donors_enriched, "time_exceeded") <- FALSE
  }

  time_exceeded <- isTRUE(attr(donors_enriched, "time_exceeded"))

  # Phase 4: Contribution history
  all_contribs <- pull_all_contributions(donors_enriched)
  contrib_path <- if (time_exceeded)
    sub("(\\.csv)$", "_PARTIAL\\1", CONTRIB_HISTORY_FILE) else CONTRIB_HISTORY_FILE

  if (nrow(all_contribs) > 0) {
    ensure_dir(contrib_path)
    write_csv(all_contribs, contrib_path)
    log_message(sprintf("%d contribution records -> %s", nrow(all_contribs), contrib_path))
  } else {
    log_message("No contributions written - confirm MyCampaign (mode 1) access", "WARNING")
  }

  # Phase 5: Dedup and save
  donors_dedup <- donors_enriched %>%
    filter(match_status == "matched") %>%
    group_by(van_id) %>%
    arrange(desc(contribution_receipt_amount)) %>%
    slice(1) %>%
    ungroup()

  save_results(donors_dedup, output_file, partial = time_exceeded)

  # Summary
  processed <- sum(donors_enriched$match_status != "not_searched", na.rm = TRUE)
  remaining <- total_records - processed

  if (time_exceeded) {
    log_message(sprintf(" SESSION COMPLETE - %d/%d processed | %d remain",
                        processed, total_records, remaining))
    cat(sprintf("\n  %d/%d processed. Run main() to continue.\n\n",
                processed, total_records))
  } else {
    log_message(sprintf(" RUN COMPLETE - %d donors matched in %s",
                        nrow(donors_dedup), elapsed_str(session_start)))
    cat(sprintf("\n  COMPLETE - %d donors matched in %s\n",
                nrow(donors_dedup), elapsed_str(session_start)))
    cat(sprintf("  Caller sheet:    %s\n", output_file))
    cat(sprintf("  Contrib history: %s\n", contrib_path))
    cat(sprintf("  Model features:  %s\n\n", MODEL_FEATURES_FILE))
  }

  invisible(list(donors_enriched = donors_enriched,
                 contribution_history = all_contribs,
                 time_exceeded = time_exceeded,
                 processed = processed,
                 remaining = remaining))
}

# COMMAND LINE ENTRY POINT
# Rscript van_donor_enrichment_v5.R [input.csv] [output.csv] [budget_mins]
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  input_file <- if (length(args) >= 1) args[1] else INPUT_FILE
  output_file <- if (length(args) >= 2) args[2] else OUTPUT_FILE
  time_budget_mins <- if (length(args) >= 3) as.numeric(args[3]) else TIME_BUDGET_MINS

  tryCatch(main(input_file, output_file, time_budget_mins = time_budget_mins),
           error = function(e) {
             cat(sprintf("\nFATAL ERROR: %s\n", e$message))
             tryCatch(log_message(paste("FATAL:", e$message), "ERROR"), error = function(e2) NULL)
             quit(status = 1)
             }
           )
}
