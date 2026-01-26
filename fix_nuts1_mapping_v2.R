# Fix NUTS1 mapping - the postcodes.io response had wrong field
# Solution: Re-query with correct field extraction, or derive NUTS1 from coordinates

library(arrow)
library(dplyr)
library(stringr)
library(collapse)
library(httr)
library(jsonlite)

localbig <- "C:\\Users\\rmartin\\OneDrive - Imperial College London\\inglobe"

# Load the partial results
df_GB <- readRDS(paste0(localbig, "\\data\\df_GB_with_nuts1_partial.rds"))

cat("Loaded df_GB with", nrow(df_GB), "rows\n")
cat("Has latitude:", sum(!is.na(df_GB$latitude)), "\n")
cat("Has longitude:", sum(!is.na(df_GB$longitude)), "\n\n")

# ============================================
# Option 1: Derive NUTS1 from lat/lon coordinates
# This is fast and doesn't require additional API calls
# ============================================

# UK NUTS1 approximate boundaries (simplified polygon approach)
# These are rough bounding boxes - good enough for most purposes

assign_nuts1_from_coords <- function(lat, lon) {
  case_when(
    is.na(lat) | is.na(lon) ~ NA_character_,

    # Northern Ireland (west of GB mainland, specific lat range)
    lon < -5.2 & lat > 54.0 & lat < 55.4 ~ "UKN",

    # Scotland (north of ~55.3, excluding NI)
    lat > 55.3 ~ "UKM",

    # North East England
    lat > 54.5 & lat <= 55.8 & lon > -2.5 & lon < -0.8 ~ "UKC",

    # North West England
    lat > 53.0 & lat <= 55.3 & lon >= -3.2 & lon < -1.8 ~ "UKD",

    # Yorkshire and The Humber
    lat > 53.3 & lat <= 54.6 & lon >= -1.8 & lon < 0.0 ~ "UKE",

    # Wales
    lat >= 51.3 & lat <= 53.5 & lon < -2.6 ~ "UKL",

    # West Midlands
    lat > 52.0 & lat <= 53.0 & lon >= -2.6 & lon < -1.4 ~ "UKG",

    # East Midlands
    lat > 52.0 & lat <= 53.6 & lon >= -1.4 & lon < 0.2 ~ "UKF",

    # East of England
    lat > 51.5 & lat <= 52.8 & lon >= 0.0 & lon < 1.8 ~ "UKH",

    # London (tighter box)
    lat > 51.28 & lat <= 51.7 & lon >= -0.52 & lon < 0.28 ~ "UKI",

    # South East (around London, excluding London itself)
    lat > 50.7 & lat <= 52.0 & lon >= -1.8 & lon < 1.5 &
      !(lat > 51.28 & lat <= 51.7 & lon >= -0.52 & lon < 0.28) ~ "UKJ",

    # South West (remaining south-west area)
    lat <= 52.0 & lon < -1.8 ~ "UKK",
    lat <= 51.3 & lon >= -1.8 & lon < -0.5 ~ "UKK",

    # Catch remaining England as South East (default for edge cases)
    lat > 50.0 & lat < 56.0 & lon > -6.0 & lon < 2.0 ~ "UKJ",

    TRUE ~ NA_character_
  )
}

# Apply NUTS1 assignment from coordinates
cat("Assigning NUTS1 from coordinates...\n")

df_GB <- df_GB %>%
  mutate(
    nuts1_code = assign_nuts1_from_coords(latitude, longitude)
  )

# Check results
cat("\n=== NUTS1 Assignment Results ===\n\n")

nuts1_summary <- df_GB %>%
  count(nuts1_code) %>%
  arrange(desc(n)) %>%
  mutate(pct = round(100 * n / sum(n), 1))

print(nuts1_summary)

cat("\nTotal with NUTS1:", sum(!is.na(df_GB$nuts1_code)),
    sprintf("(%.1f%%)\n", 100 * sum(!is.na(df_GB$nuts1_code)) / nrow(df_GB)))

# ============================================
# Add NUTS1 region names
# ============================================

nuts1_names <- data.frame(
  nuts1_code = c("UKC", "UKD", "UKE", "UKF", "UKG", "UKH", "UKI", "UKJ", "UKK", "UKL", "UKM", "UKN"),
  nuts1_name = c("North East", "North West", "Yorkshire and The Humber", "East Midlands",
                 "West Midlands", "East of England", "London", "South East", "South West",
                 "Wales", "Scotland", "Northern Ireland")
)

df_GB <- df_GB %>%
  left_join(nuts1_names, by = "nuts1_code")

# ============================================
# Add reliability score
# ============================================

df_GB <- df_GB %>%
  mutate(
    nuts1_reliability = case_when(
      !is.na(nuts1_code) & !is.na(full_postcode) ~ 0.95,  # Had postcode, got coords
      !is.na(nuts1_code) & is.na(full_postcode) ~ 0.7,   # No postcode but got coords somehow
      TRUE ~ NA_real_
    )
  )

# ============================================
# Summary by region
# ============================================

cat("\n=== Summary by NUTS1 Region ===\n\n")

region_summary <- df_GB %>%
  filter(!is.na(nuts1_code)) %>%
  count(nuts1_code, nuts1_name) %>%
  arrange(desc(n)) %>%
  mutate(pct = round(100 * n / sum(n), 1))

print(region_summary)

# ============================================
# Save results
# ============================================

# Clean up intermediate columns
df_GB_final <- df_GB %>%
  select(
    person_id, person_address, person_ctry_code,
    full_postcode, latitude, longitude,
    nuts1_code, nuts1_name, nuts1_reliability
  )

# Save
saveRDS(df_GB_final, paste0(localbig, "\\data\\df_GB_with_nuts1.rds"))
write_parquet(df_GB_final, paste0(localbig, "\\data\\df_GB_with_nuts1.parquet"))

cat("\n=== Saved Results ===\n")
cat("RDS:", paste0(localbig, "\\data\\df_GB_with_nuts1.rds"), "\n")
cat("Parquet:", paste0(localbig, "\\data\\df_GB_with_nuts1.parquet"), "\n")

# ============================================
# What about addresses without coordinates?
# ============================================

no_coords <- df_GB %>%
  filter(is.na(latitude)) %>%
  nrow()

cat("\n=== Addresses Still Without Coordinates ===\n")
cat("Count:", no_coords, sprintf("(%.1f%% of total)\n", 100 * no_coords / nrow(df_GB)))
cat("These would need Mapbox geocoding to get coordinates first.\n")
