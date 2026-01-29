# Postcode extraction analysis for UK addresses
# Goal: See how many addresses can be matched via postcode alone

library(arrow)
library(dplyr)
library(stringr)
library(collapse)

# Set path
localbig <- "C:\\Users\\rmartin\\OneDrive - Imperial College London\\inglobe"

# Load data
df <- read_parquet(paste0(localbig, "\\data\\person_address.parquet"))
df_GB <- df %>% fsubset(person_ctry_code == "GB")

cat("Total GB records:", nrow(df_GB), "\n")
cat("Unique addresses:", n_distinct(df_GB$person_address), "\n\n")

# UK postcode regex patterns (from most to least specific)
# Full UK postcode: AA9A 9AA, A9A 9AA, A9 9AA, A99 9AA, AA9 9AA, AA99 9AA
uk_postcode_full <- "\\b([A-Z]{1,2}[0-9][0-9A-Z]?)\\s*([0-9][A-Z]{2})\\b"

# Outward code only (first part): AA9A, A9A, A9, A99, AA9, AA99
uk_outward_code <- "\\b([A-Z]{1,2}[0-9][0-9A-Z]?)\\b"

# Extract postcodes
df_GB <- df_GB %>%
  mutate(
    address_upper = toupper(person_address),
    # Full postcode extraction
    full_postcode = str_extract(address_upper, uk_postcode_full),
    # Outward code (district) - for partial matches
    outward_code = str_extract(address_upper, "\\b([A-Z]{1,2}[0-9][0-9A-Z]?)\\s+[0-9][A-Z]{2}\\b", group = 1)
  )

# Summary statistics
cat("=== POSTCODE EXTRACTION RESULTS ===\n\n")

full_match <- sum(!is.na(df_GB$full_postcode))
outward_match <- sum(!is.na(df_GB$outward_code))
no_match <- sum(is.na(df_GB$full_postcode))

cat("Full postcode found:", full_match,
    sprintf("(%.1f%%)\n", 100 * full_match / nrow(df_GB)))
cat("No postcode found:", no_match,
    sprintf("(%.1f%%)\n\n", 100 * no_match / nrow(df_GB)))

# Analyze the unmatched addresses
unmatched <- df_GB %>% fsubset(is.na(full_postcode))

cat("=== UNMATCHED ADDRESS ANALYSIS ===\n\n")

# Address length distribution
cat("Address length distribution (unmatched):\n")
unmatched <- unmatched %>%
  mutate(addr_length = nchar(person_address))

length_breaks <- c(0, 5, 10, 15, 20, 30, 50, 100, Inf)
length_labels <- c("0-5", "6-10", "11-15", "16-20", "21-30", "31-50", "51-100", "100+")

length_dist <- unmatched %>%
  mutate(length_cat = cut(addr_length, breaks = length_breaks, labels = length_labels)) %>%
  group_by(length_cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(pct = round(100 * n / sum(n), 1))

print(length_dist)

cat("\n=== SAMPLE UNMATCHED ADDRESSES ===\n\n")

# Show samples by length category
cat("Very short (<=10 chars) - likely unusable:\n")
unmatched %>%
  fsubset(addr_length <= 10) %>%
  pull(person_address) %>%
  head(15) %>%
  print()

cat("\nMedium length (20-40 chars) - might be geocodable:\n")
unmatched %>%
  fsubset(addr_length >= 20 & addr_length <= 40) %>%
  pull(person_address) %>%
  head(15) %>%
  print()

cat("\nLonger addresses (>40 chars) - should be geocodable:\n")
unmatched %>%
  fsubset(addr_length > 40) %>%
  pull(person_address) %>%
  head(15) %>%
  print()

# Check for common patterns in unmatched
cat("\n=== COMMON PATTERNS IN UNMATCHED ===\n\n")

# City-only addresses
common_cities <- c("LONDON", "CAMBRIDGE", "OXFORD", "MANCHESTER", "BIRMINGHAM",
                   "EDINBURGH", "GLASGOW", "BRISTOL", "LEEDS", "LIVERPOOL",
                   "SURREY", "KENT", "ESSEX", "MIDDLESEX")

city_pattern <- paste0("^(", paste(common_cities, collapse = "|"), ")$")

city_only <- unmatched %>%
  fsubset(str_detect(address_upper, city_pattern)) %>%
  nrow()

cat("City/county name only:", city_only, "\n")

# Contains 'deceased'
deceased <- unmatched %>%
  fsubset(str_detect(address_upper, "DECEASED|DECEASE")) %>%
  nrow()

cat("Contains 'deceased':", deceased, "\n")

# Appears to be non-UK
non_uk_patterns <- "JAPAN|USA|GERMANY|FRANCE|CHINA|KOREA|INDIA|AUSTRALIA|CANADA"
non_uk <- unmatched %>%
  fsubset(str_detect(address_upper, non_uk_patterns)) %>%
  nrow()

cat("Appears non-UK:", non_uk, "\n")

# Final summary
cat("\n=== FINAL SUMMARY ===\n\n")
cat("Total GB records:", nrow(df_GB), "\n")
cat("With extractable postcode:", full_match, sprintf("(%.1f%%)\n", 100 * full_match / nrow(df_GB)))
cat("Without postcode:", no_match, sprintf("(%.1f%%)\n", 100 * no_match / nrow(df_GB)))
cat("  - Of which very short (<=15 chars):", sum(unmatched$addr_length <= 15), "\n")
cat("  - Of which potentially geocodable (>15 chars):", sum(unmatched$addr_length > 15), "\n")

# Save intermediate results for next steps
results <- list(
  df_GB = df_GB,
  unmatched = unmatched,
  stats = list(
    total = nrow(df_GB),
    with_postcode = full_match,
    without_postcode = no_match
  )
)

saveRDS(results, paste0(localbig, "\\data\\postcode_analysis_results.rds"))
cat("\nResults saved to postcode_analysis_results.rds\n")
