# Bluesky Campaign Analytics

**End-to-end social media intelligence pipeline** - Bluesky AT Protocol -> Python NLP -> Google BigQuery -> Looker Studio

Built for the Randy Bryce WI-01 congressional campaign to track voter sentiment, monitor engagement patterns, and surface rapid-response opportunities on an emerging platform.

[Live Dashboard](https://lookerstudio.google.com/s/uD90FoHE3pc)

---

## What It Does

Collects posts from the Bluesky API via keyword search and candidate feed monitoring, runs TextBlob sentiment analysis, loads structured results into a normalized BigQuery schema, and powers an interactive Looker Studio dashboard for non-technical campaign staff.

**Key results from a 7 day proof-of-concept (Feb 15–22, 2026, n=413 posts):**
- 33.4% positive vs. 12.5% negative sentiment (2.7:1 ratio)
- Feb 7th post referencing President Obama generated over 413 interactions, largest single engagement spike in the window so far (on Bluesky)
- Healthcare dominates conversation: `#MedicalSchool`, `#MedSky`, `#SciSky` were top trending hashtags

> **Statistical note:** 413 posts over 7 days is a proof-of-concept, not a statistically robust sample. Bluesky's limited political adoption means these results should not be generalized to the WI-01 electorate. A production system would require multi-platform coverage and larger sample sizes for inference, taking much more time that I do not have during the spring semester.

---

## Pipeline Architecture

```
Bluesky API          Python              Google Cloud         Visualization
(AT Protocol)   ->   (collection,    ->   (BigQuery-      ->   (Looker Studio:
                     NLP, ETL)           4-table schema)      interactive dashboard)
```

**4 BigQuery tables:** `posts` (row per post, with sentiment scores) · `daily_aggregates` (time-series KPIs) · `comparative_metrics` (candidate head-to-head) · `candidate_profiles` (snapshot)

---

## Technical Stack

| Layer | Tool | Notes |
|---|---|---|
| Data collection | `atproto` SDK | Rate-limited author feed + keyword search |
| Processing / ETL | `pandas`, `numpy` | Deduplication, feature engineering, timestamp normalization |
| NLP | TextBlob | Polarity (−1 to +1), subjectivity, 3-class categorization |
| Data warehouse | Google BigQuery | `WRITE_APPEND` for posts, `WRITE_TRUNCATE` for snapshots |
| Visualization | Looker Studio | Designed for non-technical campaign staff |
| Scheduling | `schedule` | Hourly automated collection (toggleable) |
| Config | `python-dotenv` | Credentials via `.env`, never hardcoded |

---

## Code Structure

```
bluesky_campaign_analysis.py   # nearly 600 lines, fully modular
├── BlueskyConfig              # Env-based config + validation
├── BlueskyDataCollector       # API auth, feed collection, keyword search, deduplication
├── SentimentAnalyzer          # TextBlob NLP: polarity, subjectivity, hashtag/mention extraction
├── DataProcessor              # ETL: post enrichment, daily aggregates, share-of-voice metrics
├── BigQueryExporter           # GCP auth, schema creation, DataFrame -> BQ with error handling
└── CampaignAnalysisTool       # Orchestrator: initialize -> collect -> process -> export -> summarize
```

---

## Setup

**1. Install dependencies**
```bash
pip install atproto textblob pandas google-cloud-bigquery google-auth python-dotenv schedule
python -m textblob.download_corpora
```

**2. Configure credentials**

Create a `.env` file (never commit this):
```
BLUESKY_HANDLE=your-handle.bsky.social
BLUESKY_PASSWORD=your-app-password
GCP_PROJECT_ID=your-project-id
GCP_DATASET_ID=campaign_analytics
GCP_CREDENTIALS_PATH=path/to/service-account.json
```

**3. Run**
```python
from bluesky_campaign_analysis import CampaignAnalysisTool

tool = CampaignAnalysisTool()
if tool.initialize():
    data = tool.run_analysis()
    tool.print_summary(data)
```

Local CSV exports are written automatically alongside BigQuery upload.

To enable hourly scheduled collection, uncomment the `schedule` block in `main()`.

---

## Output

**Console summary**
```
============================================================
CAMPAIGN ANALYSIS SUMMARY
============================================================
CANDIDATE COMPARISON:
 candidate  followers  avg_engagement  avg_sentiment  share_of_voice
Randy Bryce     13516          26.744         0.0895          76.428

TOTAL POSTS ANALYZED: 205
Date Range: 2026-02-15 to 2026-02-22

SENTIMENT BREAKDOWN:
  Neutral:  141 (56.6%)
  Positive:  77 (30.9%)
  Negative:  31 (12.4%)
```

**BigQuery tables** - queryable via SQL, autoconnected to Looker Studio dashboard

**Local CSVs** - timestamped exports (`export_posts_YYYYMMDD_HHMMSS.csv`, etc.)

---

## Limitations

- **Sample size:** 390 posts is a functional demonstration, not a statistically significant dataset
- **Platform bias:** Bluesky users are not representative of WI-01 voters; results can't be generalized
- **Sentiment model:** TextBlob is about 80% accurate and struggles with political sarcasm and nuance; production use would warrant fine-tuning on political text or a transformer-based model
- **Single platform:** A real social listening system would require Twitter/X and Facebook coverage
- **No causal claims:** Engagement and sentiment correlations do not establish causation

---

## Future Work

- Unit test suite (`pytest`) for core pipeline functions
- Fine-tuned BERT or `cardiffnlp/twitter-roberta` for political sentiment classification
- Network analysis of repost/reply graphs to identify high-influence nodes
- Multiplatform ingestion (Twitter/X API, CrowdTangle)
- Statistical significance testing and confidence intervals on daily sentiment shifts
- Geographic filtering for district-level analysis
