# VAN Donor Enrichment Pipeline
### Wisconsin State Senate - Donor Prioritization Model

Matches original individual contribution records against the NGP VAN voter file via the VAN REST API. For each confirmed match, retrieves phone numbers, party registration, demographic data, propensity scores, and full contribution history. Produces three structured output files that feed a double-hurdle donor prioritization model scoring 6,000+ contacts by expected fundraising value.

---

## Pipeline Overview

```
Original bulk export        Identity resolution      VAN enrichment         Model inputs
(individual            ------------------>       ------------->        ------------->
 contributions)        POST /people/find         phones, scores,        caller sheet
                       name + address            demographics,          contrib history
                       fuzzy match (0–100)       voter history,         model features
                                                 contribution history
```

The enrichment script (`van_donor_enrichment_v5.R`) handles everything up to and including model inputs. The double hurdle model consuming those inputs is documented below.

---

## Why a Double Hurdle Model

Donor amount distributions have a structural feature that rules out OLS: a large mass of zeros from lapsed or never converted contacts, combined with a right skewed continuous distribution among those who do give. Fitting a single linear regression on raw amounts conflates two distinct decisions, whether someone gives at all, and how much they give conditional on giving, and produces biased predictions for both.

The double hurdle framework separates them:

**Stage 1 - Logistic LASSO: P(give)**

Binary outcome: did the contact make a contribution in the target cycle? Features include VAN propensity scores, party registration, turnout rate, age, occupation, and RFM variables derived from full contribution history. LASSO regularization (alpha = 1) handles the moderate feature set relative to contact count and performs implicit feature selection, dropping uninformative predictors to zero. Lambda is selected by 10-fold cross-validation on held out deviance.

**Stage 2 - Linear LASSO: E(amount | give)**

Fit only on contacts who cleared the hurdle (gave at least once). The outcome is log(average gift amount), which addresses right skew and stabilizes variance. Back transformed predictions are combined with Stage 1 probabilities to produce a single expected value score.

**Combined score:**

```
EV score = P(give) × E(amount | give)
```

Contacts are ranked descending by EV score. The top decile is delivered to fundraising staff as a prioritized call sheet. This framing aligns the model objective directly with what our campaigns care about, expected dollars raised per call, rather than optimizing an intermediate metric like predicted probability alone.

---

## Downstream Model Implementation

After `main()` completes, join the two output files and build the model:

```r
library(dplyr); library(readr); library(lubridate); library(glmnet)

# Load pipeline outputs
contrib <- read_csv("*_contribution_history.csv") |>
  mutate(contrib_date = as.Date(contrib_date),
         cycle_year   = year(contrib_date))

features <- read_csv("*_model_features.csv") |>
  mutate(
    age_est   = as.numeric(Sys.Date() - as.Date(date_of_birth)) / 365.25,
    party_reg = factor(party_reg, levels = c("D", "R", "NP", "Other"))
  )

# RFM features - one row per donor
# Recency, frequency, and monetary features are the strongest predictors of future giving behaviour in most political donor models.
snapshot_date <- Sys.Date()

rfm <- contrib |>
  group_by(van_id) |>
  summarise(
    recency = as.numeric(snapshot_date - max(contrib_date)), # days since last gift
    frequency = n(),                                         # total gifts on record
    monetary = sum(contrib_amount, na.rm = TRUE),            # lifetime total dollars
    avg_gift = mean(contrib_amount, na.rm = TRUE),
    max_gift = max(contrib_amount, na.rm = TRUE),
    .groups = "drop"
  )

model_base <- features |> left_join(rfm, by = "van_id")

# Stage 1: Logistic LASSO - P(give)
X <- model.matrix(
  ~ age_est + party_reg + van_voter_score + van_donor_score +
    turnout_rate + recency + frequency + monetary + max_gift - 1,
  data = model_base)
y_binary <- as.integer(model_base$frequency > 0)

cv1  <- cv.glmnet(X, y_binary, family = "binomial", alpha = 1, nfolds = 10)
fit1 <- glmnet(X, y_binary, family = "binomial", alpha = 1, lambda = cv1$lambda.min)
model_base$p_give <- as.numeric(predict(fit1, X, type = "response"))

# Stage 2: Linear LASSO - E(amount | give)
# Subset to donors who cleared the hurdle. Log-transform outcome to address right skew; expm1() back-transforms predictions to dollar scale
gave <- model_base |> filter(frequency > 0)

X2 <- model.matrix(
  ~ age_est + party_reg + van_voter_score + van_donor_score +
    turnout_rate + recency + frequency + max_gift - 1,
  data = gave
)
y2 <- log1p(gave$monetary / gave$frequency) # log(avg gift)

cv2  <- cv.glmnet(X2, y2, alpha = 1, nfolds = 10)
fit2 <- glmnet(X2, y2, alpha = 1, lambda = cv2$lambda.min)
gave$pred_log_amt <- as.numeric(predict(fit2, X2))

# Combined EV score and ranked output
scored <- model_base |>
  left_join(gave |> select(van_id, pred_log_amt), by = "van_id") |>
  mutate(
    pred_amount = expm1(coalesce(pred_log_amt, log1p(coalesce(avg_gift, 0)))),
    ev_score    = p_give * pred_amount
  ) |>
  arrange(desc(ev_score))

# Top decile by EV -> priority call list for finance team
call_list <- scored |> slice_head(prop = 0.10)
write_csv(call_list, "priority_call_list.csv")
```

---

## Identity Resolution

Each original record is matched against VAN using `POST /people/find`, a read only endpoint that never creates records. The search body contains parsed first name, last name, street address, city, state, and ZIP. VAN returns a candidate record (HTTP 200) or no match (HTTP 404).

Every candidate match is scored 0-100 against the original fields using a weighted agreement function:

| Field | Weight | Scoring |
|---|---|---|
| Last name | 30 pts | Exact: 30 \| Fuzzy (edit distance ≤ 1): 15 |
| First name | 20 pts | Exact: 20 \| Fuzzy: 10 |
| Street address | 20 pts | Exact (normalized): 20 \| Substring: 10 |
| City | 10 pts | Exact (case-insensitive) |
| ZIP | 20 pts | 5-digit exact match |

Score thresholds:

| Range | Label | Default behaviour |
|---|---|---|
| 90-100 | `exact_match` | Accepted |
| 70-89 | `high_confidence` | Accepted |
| 50-69 | `medium_confidence` | Accepted (review recommended) |
| Below threshold | `below_threshold` | Rejected, not written to output |
| - | `no_match` | VAN returned no candidate |
| - | `insufficient_data` | Missing name or address in input |

The default threshold of 50 can be raised to 70 or 90 in the config block. All match metadata (`match_confidence`, `match_type`, `matched_name`, `matched_address`) is written to every output file for downstream audit.

---

## VAN API Architecture

Each matched record makes up to 5 synchronous API calls:

| Call | Endpoint |
|---|---|
| Person search | `POST /people/find` |
| Person detail | `GET /people/{vanId}` |
| Phones | `GET /people/{vanId}/phones` |
| Scores | `GET /people/{vanId}/scores` |
| Voter history | `GET /people/{vanId}/voterHistory` |
| Contributions | `GET /people/{vanId}/contributions` |

The script is single threaded and synchronous by design, each request completes before the next begins, per VAN's documented requirement. Throttling follows VAN's published limits: 0.5s delay after `POST /people/find` (max 2 req/sec), 0.5s after all GET calls (conservative within the 5 req/sec cap).

On a 429 response, the script reads the `Retry-After` header, waits the specified interval, doubles `PERSON_SEARCH_DELAY` to reduce ongoing pressure, retries once, and logs a warning if errors persist.

**VANID scope:** VANIDs in all output files are scoped to the specific VAN instance used during the run. They cannot be cross referenced against a different committee's VAN, a different state's VAN, SmartVAN, or VoteBuilder. The VAN base URL is recorded in the log at the start of every session to make instance scope permanently traceable.

---

## Setup

**1. Install packages**

```r
install.packages(c("httr", "jsonlite", "dplyr", "readr", "stringr", "lubridate", "purrr"))

# For the downstream double-hurdle model:
install.packages("glmnet")
```

**2. Store credentials**

```r
usethis::edit_r_environ()
```

Add to `.Renviron`:

```
VAN_API_KEY=xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
VAN_APP_NAME=AppName
```

Then add `.Renviron` to `.gitignore` - it contains API key. The script stores credentials exclusively via `Sys.getenv()`. Keys are never logged; the log records only the last 4 digits per VAN guidance.

**3. Configure the script**

Edit the `CONFIGURATION` block at the top of `van_donor_enrichment_v5.R`:

| Setting | Default | Description |
|---|---|---|
| `INPUT_FILE` | `XXX.csv` | Path to original input CSV |
| `OUTPUT_FILE` | `XXX.csv` | Caller sheet output |
| `HAS_VOTERFILE_ACCESS` | `TRUE` | Set `FALSE` if key lacks My Voters access |
| `MIN_CONFIDENCE_THRESHOLD` | `50` | Minimum match score to accept (50–100) |
| `TIME_BUDGET_MINS` | `55` | Stop and checkpoint after this many minutes |
| `CHECKPOINT_INTERVAL` | `25` | Save progress every N records |

---

## Running the Script

Always follow this sequence on first use:

```r
source("van_donor_enrichment_v5.R")

# Step 1 - Credential checks, column validation, data quality report. No API calls.
validate_inputs()

# Step 2 - Confirm VAN authentication and mode access. Makes 2 real API calls.
test_api_connection()

# Step 3 - Process 10 records as a dry run. Writes to TEST_ files only.
run_test(n = 10)

# Step 4 — Full production run. Resumes from checkpoint automatically if one exists.
main()
```

**Resuming across sessions:**

```r
main() # Resume from checkpoint (default)
main(fresh_start = TRUE) # Ignore checkpoint, start from record 1
```

**Command line:**

```bash
Rscript van_donor_enrichment_v5.R input.csv output.csv 55
```

---

## Input Format

A CSV of original individual contribution records. At minimum:

| Column | Description |
|---|---|
| `contributor_name` | Full name - parsed as `"LAST, FIRST"` or `"FIRST LAST"` |
| `contributor_street_1` | Street address |
| `contributor_city` | City |
| `contributor_state` | State abbreviation |
| `contributor_zip` | ZIP code (5 digit or ZIP+4) |
| `contribution_receipt_amount` | Dollar amount |
| `contribution_receipt_date` | Date of contribution |

---

## Output Files

All three files are written to the same directory as `OUTPUT_FILE`.

**`*_with_phones.csv`** - Caller sheet for phone bank staff. One row per person, deduplicated to highest gift. Contains all original fields plus: `van_cell_phone`, `van_home_phone`, `van_id`, `party_reg`, `van_voter_score`, `turnout_rate`, `match_confidence`, `match_type`.

**`*_contribution_history.csv`** - Primary model input. One row per contribution transaction across all matched donors. Fields: `van_id`, `contrib_amount`, `contrib_date`, `batch_id`, `recipient_name`, `committee_name`, `election_cycle`, `contrib_type`. Used for RFM feature engineering.

**`*_model_features.csv`** - Demographic model input. One row per matched donor. Fields: `van_id`, `first_name`, `last_name`, `zip`, `city`, `state`, `party_reg`, `date_of_birth`, `sex`, `van_voter_score`, `van_donor_score`, `turnout_rate`, `n_elections_voted`, `contributor_occupation`, `contributor_employer`, `match_confidence`, `match_type`. Join to contribution history on `van_id`.

If a session ends mid run, partial output files are written with a `_PARTIAL` filename suffix.

---

## Design Tradeoffs
- Synchronous requests: Parallelization was avoided to respect rate limits and ensure predictable recovery behavior
- Precision over recall in matching: The pipeline favors high‑confidence matches to minimize misdirected outreach, accepting that some true matches will be dropped
- Expected‑value optimization: Ranking by EV reflects real fundraising constraints better than probability only targeting
- Checkpoint‑based execution: Throughput is sacrificed in favor of resumability during active fundraising windows

---

## Future Improvements
- Probabilistic record linkage with learned weights rather than fixed heuristics
- Network‑based donor clustering once relational data is available
- Offline back‑testing of EV rankings against realized call outcomes
- Optional asynchronous batching for committees with explicit API approval

---

## Runtime Estimates

Each matched record makes up to 5 API calls at 0.5 seconds each:

| List size | Estimated time |
|---|---|
| 500 donors | ~21 min |
| 1,000 donors | ~42 min |
| 2,000 donors | ~83 min |
| 5,000 donors | ~208 min |

---
