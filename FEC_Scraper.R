# PUBLIC VERSION

# FEC Campaign Finance Data Analysis
# Analysis of campaign contributions and expenditures

# Load libraries
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(stringr)
library(lubridate)

# Output directory
dir.create("fec_output", showWarnings = FALSE, recursive = TRUE)
dir.create("oppo_research_exports", showWarnings = FALSE, recursive = TRUE)

# API Configuration

API_KEY <- "FEC_API_KEY"
COMMITTEE_ID <- "XXXXXXXXX"  # Principal campaign committee
CANDIDATE_ID <- "XXXXXXXXX"   # Candidate ID
CYCLES <- c(2018, 2020, 2022, 2024)
BASE_URL <- "https://api.open.fec.gov/v1/schedules/schedule_a/"

# Main Functions

# Fetch campaign contributions by cycle
fetch_cycle_exact <- function(committee_id, cycle) {
  page <- 1
  all_pages <- list()
  total_records <- 0

  repeat {
    Sys.sleep(1.2)
    resp <- httr::RETRY(verb = "GET",
                        url  = BASE_URL,
                        query = list(api_key = API_KEY,
                                     committee_id = committee_id,
                                     two_year_transaction_period = cycle,
                                     per_page = 100,
                                     page = page),
                        times = 5,
                        pause_base = 2)
    
    if (httr::status_code(resp) != 200) break
    parsed <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), flatten = TRUE)
    
    if (!is.data.frame(parsed$results) || nrow(parsed$results) == 0) break
    all_pages[[page]] <- parsed$results
    total_records <- total_records + nrow(parsed$results)
    
    if (!is.null(parsed$pagination$pages) && page >= parsed$pagination$pages) break
    page <- page + 1
    
    if (page > 1000) break
  }
  
  if (length(all_pages) == 0) return(tibble::tibble())
  df <- dplyr::bind_rows(all_pages)
  
  # Remove duplicates
  df <- df %>%
    dplyr::mutate(txn_key = paste0(committee_id, "|",
                                   dplyr::coalesce(contributor_name, ""), "|",
                                   dplyr::coalesce(as.character(contribution_receipt_amount), ""), "|",
                                   dplyr::coalesce(contribution_receipt_date, ""), "|",
                                   dplyr::row_number() ), cycle = cycle) %>%
    dplyr::distinct(txn_key, .keep_all = TRUE)
  
  return(df)
}

# Fetch PAC/Committee contributions
fetch_committee_contributions <- function(committee_id, cycle) {
 
  page <- 1
  all_pages <- list()
  
  repeat {
    Sys.sleep(1.2)
    
    resp <- httr::RETRY(verb = "GET", 
                        url  = "https://api.open.fec.gov/v1/schedules/schedule_a/",
                        query = list(api_key = API_KEY,
                                     committee_id = committee_id,
                                     two_year_transaction_period = cycle,
                                     per_page = 100,
                                     page = page,
                                     contributor_type = "committee"), 
                        times = 5, pause_base = 2)
    
    if (httr::status_code(resp) != 200) break
    parsed <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), flatten = TRUE)
    
    if (!is.data.frame(parsed$results) || nrow(parsed$results) == 0) break
    all_pages[[page]] <- parsed$results
    
    if (!is.null(parsed$pagination$pages) && page >= parsed$pagination$pages) break
    page <- page + 1
  }
  
  df <- dplyr::bind_rows(all_pages)
  
  if (nrow(df) == 0) return(tibble::tibble())
  df %>% 
    dplyr::mutate(cycle = cycle, contribution_type = "PAC/Committee")
}

# Fetch independent expenditures
fetch_independent_expenditures <- function(candidate_id, cycle) {
  page <- 1
  all_pages <- list()
  
  repeat {
    Sys.sleep(1.2)
    
    resp <- httr::RETRY(verb = "GET",
                        url  = "https://api.open.fec.gov/v1/schedules/schedule_e/by_candidate/",
                        query = list(api_key = API_KEY,
                                     candidate_id = candidate_id,
                                     cycle = cycle,
                                     per_page = 100,
                                     page = page),
                        times = 5, pause_base = 2)
    
    if (httr::status_code(resp) != 200) break
    parsed <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), flatten = TRUE)
    
    if (!is.data.frame(parsed$results) || nrow(parsed$results) == 0) break
    all_pages[[page]] <- parsed$results
    
    if (!is.null(parsed$pagination$pages) && page >= parsed$pagination$pages) break
    page <- page + 1
    
    if (page > 500) break
    }
  
  df <- dplyr::bind_rows(all_pages)
  
  if (nrow(df) == 0) return(tibble::tibble())
  df %>% 
    dplyr::mutate(cycle = cycle)
}

# Fetch disbursements
fetch_disbursements <- function(committee_id, cycle) {
  page <- 1
  all_pages <- list()
  
  repeat {Sys.sleep(1.2)
    resp <- httr::RETRY(verb = "GET",
                        url  = "https://api.open.fec.gov/v1/schedules/schedule_b/",
                        query = list(api_key = API_KEY,
                                     committee_id = committee_id,
                                     two_year_transaction_period = cycle,
                                     per_page = 100,
                                     page = page), 
                        times = 5, pause_base = 2)
    
    if (httr::status_code(resp) != 200) break
    parsed <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), flatten = TRUE)
    
    if (!is.data.frame(parsed$results) || nrow(parsed$results) == 0) break
    all_pages[[page]] <- parsed$results
    
    if (!is.null(parsed$pagination$pages) && page >= parsed$pagination$pages) break
    page <- page + 1
    
    if (page > 500) break
    }
  
  df <- dplyr::bind_rows(all_pages)
  
  if (nrow(df) == 0) return(tibble::tibble())
  df %>% 
    dplyr::mutate(cycle = cycle)
}

# Industry classification function
classify_industry <- function(name, employer = NA, occupation = NA) {
  combined <- paste(toupper(coalesce(name, "")),
                    toupper(coalesce(employer, "")),
                    toupper(coalesce(occupation, "")))
  
  case_when(str_detect(combined, "FAIRSHAKE|COINBASE|BLOCKCHAIN|CRYPTO|BITCOIN|RIPPLE|BINANCE|DIGITAL ASSET|GEMINI|KRAKEN") ~ "Crypto/FinTech",
            str_detect(combined, "BANK|WELLS FARGO|CITI|CITIGROUP|MORGAN STANLEY|GOLDMAN|BMO|PNC|JPMORGAN|CHASE|US BANK|FIFTH THIRD|HUNTINGTON") ~ "Banking",
            str_detect(combined, "SECURITIES|INVESTMENT|CAPITAL MARKETS|MERRILL|SCHWAB|FIDELITY|VANGUARD|BLACKROCK|STATE STREET") ~ "Securities/Investment",
            str_detect(combined, "INSURANCE|NORTHWESTERN MUTUAL|STATE FARM|ALLSTATE|PRUDENTIAL|METLIFE|TRAVELERS|LIBERTY MUTUAL") ~ "Insurance",
            str_detect(combined, "REAL ESTATE|REALTY|PROPERTY|REALTOR|REMAX|KELLER WILLIAMS") ~ "Real Estate",
            str_detect(combined, "MANUFACTURING|INDUSTRIAL|CNH|JOHN DEERE|CASE NEW HOLLAND|HARLEY") ~ "Manufacturing",
            str_detect(combined, "PHARMA|PHARMACEUTICAL|HEALTH|MEDICAL|HOSPITAL|BIOTECH|PFIZER|MERCK|ABBVIE") ~ "Healthcare/Pharma",
            str_detect(combined, "TECH|SOFTWARE|GOOGLE|MICROSOFT|APPLE|AMAZON|META|FACEBOOK") ~ "Tech",
            str_detect(combined, "ENERGY|OIL|PHILLIPS|MARATHON|VALERO|PETROLEUM|EXXON|CHEVRON|KOCH") ~ "Energy/Oil",
            str_detect(combined, "LOBBYIST|LOBBYING|GOVERNMENT RELATIONS|PUBLIC AFFAIRS") ~ "Lobbying",
            TRUE ~ "Other")
}

# Data Collection

# Collect individual contributions
steil_contributions <- purrr::map_dfr(CYCLES, ~ {
  result <- fetch_cycle_exact(COMMITTEE_ID, .x)
  Sys.sleep(3)
  result
})

# Collect PAC contributions
steil_pac_contributions <- purrr::map_dfr(CYCLES, ~ {
  result <- fetch_committee_contributions(COMMITTEE_ID, .x)
  Sys.sleep(3)
  result
})

# Collect independent expenditures
steil_independent_expenditures <- purrr::map_dfr(CYCLES, ~ {
  result <- fetch_independent_expenditures(CANDIDATE_ID, .x)
  Sys.sleep(3)
  result
})

# Collect disbursements
steil_disbursements <- purrr::map_dfr(CYCLES, ~ {
  result <- fetch_disbursements(COMMITTEE_ID, .x)
  Sys.sleep(3)
  result
})

saveRDS(steil_contributions, "fec_output/steil_schedule_a.rds")
saveRDS(steil_independent_expenditures, "fec_output/steil_schedule_e.rds")

# Data Preparation

# Prepare direct contributions
direct_money <- steil_contributions %>%
  transmute(cycle = two_year_transaction_period,
            date = as.Date(contribution_receipt_date),
            source_type = "Direct Contribution",
            contributor_type = case_when(
              entity_type_desc == "INDIVIDUAL" ~ "Individual",
              str_detect(toupper(entity_type_desc), "COMMITTEE|PAC") ~ "PAC/Committee",
              TRUE ~ "Other"),
            contributor_name = contributor_name,
            contributor_employer = contributor_employer,
            contributor_occupation = contributor_occupation,
            contributor_city = contributor_city,
            contributor_state = contributor_state,
            amount = contribution_receipt_amount) %>%
  filter(!is.na(amount), amount > 0)

# Prepare independent expenditures
if (nrow(steil_independent_expenditures) > 0) {
  outside_money <- steil_independent_expenditures %>%
    filter(support_oppose_indicator == "S") %>%
    transmute(cycle = cycle,
              date = as.Date(expenditure_date),
              source_type = "Independent Expenditure",
              contributor_type = "Super PAC/Outside",
              contributor_name = committee.name,
              contributor_employer = NA_character_,
              contributor_occupation = NA_character_,
              contributor_city = committee.city,
              contributor_state = committee.state,
              amount = expenditure_amount) %>%
    filter(!is.na(amount), amount > 0)
  
  all_money <- bind_rows(direct_money, outside_money)
} else {all_money <- direct_money
  }

# Apply industry classification
all_money <- all_money %>%
  mutate(industry = classify_industry(contributor_name, contributor_employer, contributor_occupation))

message(sprintf("\nCombined dataset: %s records", format(nrow(all_money), big.mark = ",")))

# Analysis: Overall Statistics
career_stats <- list(total = sum(all_money$amount),
                     from_individuals = sum(all_money$amount[all_money$contributor_type == "Individual"]),
                     from_pacs = sum(all_money$amount[all_money$contributor_type == "PAC/Committee"]),
                     from_outside = sum(all_money$amount[all_money$contributor_type == "Super PAC/Outside"])
                     )

data.frame(Metric = c("Total Raised", "From Individuals", "From PACs", "From Super PACs"),
                 Amount = sapply(career_stats, function(x) sprintf("$%s", format(x, big.mark = ",")))
           )

# Analysis: Industry Breakdown
industry_summary <- all_money %>%
  group_by(industry) %>%
  summarise(total = sum(amount),
            count = n(),
            .groups = "drop") %>%
  arrange(desc(total)) %>%
  mutate(percentage = (total / sum(total)) * 100)

head(industry_summary)

# Analysis: Geographic Distribution
geo_summary <- all_money %>%
  filter(contributor_type == "Individual") %>%
  mutate(location = case_when(contributor_state == "WI" ~ "Wisconsin",
                              is.na(contributor_state) ~ "Unknown",
                              TRUE ~ "Out of State")) %>%
  group_by(location) %>%
  summarise(total = sum(amount),
            count = n(),
            avg_donation = mean(amount),
            .groups = "drop") %>%
  mutate(percentage = (total / sum(total)) * 100)

geo_summary

# Analysis: Donor Size
donor_size_summary <- all_money %>%
  filter(contributor_type == "Individual") %>%
  mutate(donor_category = case_when(amount >= 2900 ~ "Max Donor ($2,900+)",
                               amount >= 1000 ~ "Large Donor ($1,000-$2,899)",
                               amount >= 200 ~ "Medium Donor ($200-$999)",
                               TRUE ~ "Small Donor (<$200)")) %>%
  group_by(donor_category) %>%
  summarise(total_amount = sum(amount),
    num_donors = n(),
    .groups = "drop") %>%
  mutate(pct_dollars = (total_amount / sum(total_amount)) * 100,
    pct_donors = (num_donors / sum(num_donors)) * 100)

donor_size_summary

# Analysis: Top Donors
top_50_donors <- all_money %>%
  group_by(contributor_name, contributor_type, contributor_employer, contributor_state) %>%
  summarise(career_total = sum(amount),
            total_contributions = n(),
            cycles_active = n_distinct(cycle),
            .groups = "drop") %>%
  arrange(desc(career_total)) %>%
  head(50) %>%
  mutate(industry = classify_industry(contributor_name, contributor_employer))

head(top_50_donors)

# Analysis: Cycle-by-Cycle Breakdown
cycle_summary <- all_money %>%
  group_by(cycle, contributor_type) %>%
  summarise(total = sum(amount),
            count = n(),
            .groups = "drop") %>%
  arrange(cycle, desc(total))

cycle_summary

# Export Results

# Export main dataset
write.csv(all_money, "oppo_research_exports/complete_dataset.csv", row.names = FALSE)

# Export summary tables
write.csv(industry_summary, "oppo_research_exports/industry_summary.csv", row.names = FALSE)
write.csv(geo_summary, "oppo_research_exports/geographic_summary.csv", row.names = FALSE)
write.csv(donor_size_summary, "oppo_research_exports/donor_size_summary.csv", row.names = FALSE)
write.csv(top_50_donors, "oppo_research_exports/top_50_donors.csv", row.names = FALSE)
write.csv(cycle_summary, "oppo_research_exports/cycle_summary.csv", row.names = FALSE)



