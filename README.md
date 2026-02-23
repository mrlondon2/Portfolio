# Data Science Portfolio
### Matthew London
### Includes projects from my time at Wisconsin State Senate Democratic Committee & Randy Bryce for Congress

This repository contains data science work built during active campaign roles in the 2025-2026 cycle. Each project has its own README with full methodology, design decisions, and implementation notes.

---

## Projects

**[VAN Donor Enrichment Pipeline](./VAN_donor_enrichment.R) · [README](./VAN_donor_enrichment_README.md)**  
`R` · `NGP VAN API` · `identity resolution` · `LASSO` · `RFM modeling`

Matches original contribution records against the NGP VAN voter file to enrich 6,000+ contacts with phones, demographics, and propensity scores. Output feeds a double hurdle model (logistic + linear LASSO) that ranks contacts by expected donation value and produces a prioritized call sheet for fundraising staff. Built for the Wisconsin State Senate Democratic Committee.

---

**[Bluesky Campaign Analytics](./Bluesky_campaign_analysis.py) · [README](./Bluesky_campaign_analysis_README.md)**  
`Python` · `Google BigQuery` · `Looker Studio` · `NLP` · `sentiment analysis`

End-to-end social media intelligence pipeline for the Randy Bryce for Congress campaign. Collects posts via the Bluesky AT Protocol API, runs sentiment analysis, loads results into a normalized BigQuery schema, and surfaces insights through an interactive Looker Studio dashboard built for non-technical communications and finance staff.

---

**[FEC Campaign Finance Scraper](./FEC_Scraper.R) · [README](./FEC_scraper_README.md)**  
`R` · `FEC API` · `data engineering`

Automated FEC bulk data collection producing clean, analysis ready contribution and expenditure datasets used for internal decision making.

---

mrmatthewlondon@gmail.com · [github.com/mrlondon2](https://github.com/mrlondon2)
