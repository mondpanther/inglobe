# Geocode PATSTAT

Geocodes persons from `patbis.fromPATSTAT2025.tls206_person_with_address` using Nominatim, writing results back to BigQuery incrementally.

## Setup

1. Clone the repo
2. Get the `patbis-b85b8ca5802e.json` credentials file from the project owner and drop it in this folder
3. Install dependencies:
```bash
pip install google-cloud-bigquery google-auth geopy pandas tqdm pyarrow python-dotenv db-dtypes
```

## Run

Open `Automatic-Optimized.ipynb` and run all cells.

## Output

Results are written to `patbis.fromPATSTAT2025.tls206_person_with_address_geocoded` in BigQuery. The script resumes automatically if interrupted.
