# FEC Campaign Finance Analysis Tool
Comprehensive R-based system for collecting, processing, and analyzing Federal Election Commission campaign finance data for opposition research and strategic campaign planning.

## Overview
This tool was built to support opposition research and campaign finance monitoring for congressional campaign work. It automates the complete workflow from API data collection through comprehensive analysis and export, handling millions of contribution records across multiple election cycles.

## Technical Architecture

### Core Components
1. API Integration Layer
- FEC API integration with official endpoints
- Automated pagination handling for large datasets
- Rate limiting (1.2 seconds between requests)
- Retry logic with exponential backoff (5 attempts, 2-second pause base)
- Comprehensive error handling and status checking

2. Data Collection Modules
- Individual Contributions (Schedule A): Itemized individual donor records
- PAC Contributions (Schedule A, filtered): Committee-to-committee transfers
- Independent Expenditures (Schedule E): Super PAC spending supporting candidate
- Disbursements (Schedule B): Campaign spending and expenditures

3. Data Processing Pipeline
- Duplicate detection using composite transaction keys
- Date parsing and standardization
- Text field cleaning and normalization
- Missing value handling
- Data type validation

4. Classification & Enrichment
- Industry classification algorithm using pattern matching
- Geographic categorization (in-state vs. out-of-state)
- Donor size segmentation (small, medium, large, max donors)
- Contributor type identification (individual, PAC, Super PAC)

5. Analysis & Reporting
- Multi-dimensional summary statistics
- Temporal trend analysis (cycle-by-cycle)
- Geographic distribution analysis
- Industry sector breakdown
- Top donor identification and aggregation
- Donor behavior profiling

## Key Features

### Multi-Cycle Data Collection
```r
# Collects data across multiple election cycles
CYCLES <- c(2018, 2020, 2022, 2024)

# Fetches all records with automatic pagination
steil_contributions <- purrr::map_dfr(CYCLES, ~ {
  result <- fetch_cycle_exact(COMMITTEE_ID, .x)
  Sys.sleep(3)  # Rate limiting between cycles
  result
})
```

### Industry Classification
The tool includes a sophisticated industry classification algorithm that analyzes contributor names, employers, and occupations to categorize contributions:
- Financial Sector: Banking, Securities/Investment, Insurance, Crypto/FinTech
- Corporate: Manufacturing, Tech, Energy/Oil, Real Estate
- Professional: Healthcare/Pharma, Lobbying
- Other: Catch-all for unclassified contributions (Significantly reduced in private seperate document geared torwards specific campaign objectives)

```r
# Example classification logic
classify_industry <- function(name, employer = NA, occupation = NA) {
  combined <- paste(toupper(coalesce(name, "")),
toupper(coalesce(employer, "")),
toupper(coalesce(occupation, ""))
)
  
  case_when(str_detect(combined, "BANK|WELLS FARGO|CITIGROUP...") ~ "Banking",
str_detect(combined, "PHARMA|MEDICAL|HOSPITAL...") ~ "Healthcare/Pharma",
    TRUE ~ "Other")
}
```

### Geographic Analysis
Automatically categorizes donors by location:
- Wisconsin: In-state donors (key grassroots support indicator)
- Out of State: National fundraising reach
- Unknown: Records with missing state information 

### Donor Size Segmentation
Classifies individual donors by contribution amount:
- Max Donors: $2,900+ (maxed-out supporters)
- Large Donors: $1,000-$2,899
- Medium Donors: $200-$999
- Small Donors: <$200

## Installation & Dependencies

### Required R Packages
```r
install.packages(c("httr",      # HTTP requests and API calls
                  "jsonlite",  # JSON parsing
                  "dplyr",     # Data manipulation
                  "purrr",     # Functional programming
                  "tibble",    # Modern data frames
                  "tidyr",     # Data tidying
                  "stringr",   # String operations
                  "lubridate"  # Date/time handling
))
```

### Setup
1. Obtain FEC API key from https://api.open.fec.gov/developers/
2. Update `API_KEY` variable in script
3. Set target `COMMITTEE_ID` and `CANDIDATE_ID`
4. Run script

# Script automatically:
1. Collects data from FEC API for all specified cycles
2. Processes and cleans the data
3. Applies industry classification
4. Generates comprehensive analysis
5. Exports results to CSV files

# Output files created in oppo_research_exports/:
 - complete_dataset.csv (all records with classifications)
 - industry_summary.csv (contributions by industry)
 - geographic_summary.csv (WI vs. out-of-state breakdown)
 - donor_size_summary.csv (small vs. large donor analysis)
 - top_50_donors.csv (career totals for top donors)
 - cycle_summary.csv (year-by-year fundraising trends)

## Analysis Outputs

### 1. Overall Statistics
- Total raised across all cycles
- Breakdown by source type (individuals, PACs, Super PACs)
- Career fundraising totals

### 2. Industry Analysis
- Total contributions by industry sector
- Number of contributions per sector
- Percentage of total fundraising
- Identification of key industry relationships

### 3. Geographic Distribution
- Wisconsin vs. out-of-state contributions
- Average donation size by location
- Donor count by region
- Percentage breakdown of fundraising sources

### 4. Donor Size Analysis
- Distribution of contributions by donor size
- Percentage of dollars from each donor category
- Percentage of donors in each category
- Small-dollar vs. large-dollar fundraising ratio

### 5. Top Donors
- Career-total contributions by donor
- Number of contributions per donor
- Cycles of active giving
- Industry affiliations
- Geographic location

### 6. Temporal Trends
- Cycle-by-cycle fundraising totals
- Contribution patterns over time
- Source mix evolution (individual vs. PAC vs. Super PAC)

## Data Quality & Validation

**Duplicate Detection:**
```r
# Creates composite key for each transaction
txn_key = paste0(committee_id, "|",
                contributor_name, "|",
                contribution_amount, "|",
                contribution_date, "|",
                row_number()
)

# Removes duplicates
df <- df %>% distinct(txn_key, .keep_all = TRUE)
```

**Data Cleaning:**
- Filters out records with missing or invalid amounts
- Removes negative amounts (refunds handled separately)
- Validates date formats
- Standardizes text fields (names, cities, states)

**Error Handling:**
- HTTP request retry logic (up to 5 attempts)
- Status code validation
- Pagination safety limits (max 1000 pages per cycle)
- Graceful handling of empty result sets

## Technical Considerations

### Rate Limiting
The script implements conservative rate limiting:
- 1.2 seconds between API requests (within FEC guidelines)
- 3 second pauses between election cycles
- Prevents API throttling or account suspension

### Memory Management
- Processes data in pages of 100 records
- Uses `map_dfr()` for efficient row-binding
- Removes duplicates incrementally
- Suitable for datasets with millions of records

### Reproducibility
- All analysis parameters clearly defined at top of script
- Deterministic classification algorithms
- Automated directory creation
- Timestamped outputs for version tracking

## Limitations & Extensions

Current Limitations:
- Industry classification is keyword-based (could use ML for improved accuracy)
- Does not analyze disbursement patterns in detail
- Geographic analysis limited to state-level (could enhance to city/zip)
- No temporal sentiment analysis of contribution timing

Potential Extensions:
- Add disbursement analysis module
- Implement donor network analysis
- Create automated reporting with R Markdown
- Add data visualization dashboard
- Build predictive models for fundraising trends
- Integrate with other campaign data sources

## Opposition Research Applications

This tool was used to:
- Track opponent fundraising patterns: Identify major donors and industry relationships
- Monitor Super PAC activity: Detect independent expenditure campaigns
- Analyze donor base composition: Understand grassroots vs. establishment support
- Identify competitive advantages: Compare fundraising sources and strategies
- Inform strategic messaging: Target messaging based on opponent's funding sources
- Support media research: Provide data for press releases and talking points

## Sample Output Statistics
```
Overall Campaign Finance Statistics
Total Raised:        $X,XXX,XXX
From Individuals:    $X,XXX,XXX (XX%)
From PACs:           $XXX,XXX (XX%)
From Super PACs:     $XXX,XXX (XX%)

Top Industries
Banking:             $XXX,XXX (XX%)
Securities:          $XXX,XXX (XX%)
Real Estate:         $XXX,XXX (XX%)
Manufacturing:       $XXX,XXX (XX%)
Healthcare:          $XXX,XXX (XX%)

Geographic Distribution
Wisconsin:           $XXX,XXX (XX%) - X,XXX donors
Out of State:        $XXX,XXX (XX%) - X,XXX donors

Donor Size Analysis
Small (<$200):       X,XXX donors, $XXX,XXX (XX% of donors, XX% of dollars)
Medium ($200-999):   X,XXX donors, $XXX,XXX (XX% of donors, XX% of dollars)
Large ($1000-2899):  XXX donors,   $XXX,XXX (XX% of donors, XX% of dollars)
Max ($2900+):        XXX donors,   $XXX,XXX (XX% of donors, XX% of dollars)
```

## FEC API Resources
- [FEC API Documentation](https://api.open.fec.gov/developers/)
- [FEC Data Catalog](https://www.fec.gov/data/)
- [Schedule A (Contributions) Documentation](https://www.fec.gov/campaign-finance-data/contributions-committees-candidates-file-description/)
- [Schedule E (Independent Expenditures) Documentation](https://www.fec.gov/campaign-finance-data/independent-expenditures-file-description/)
