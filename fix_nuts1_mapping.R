# Fix NUTS1 mapping - diagnose and repair the postcodes.io results

library(arrow)
library(dplyr)
library(stringr)
library(collapse)

localbig <- "C:\\Users\\rmartin\\OneDrive - Imperial College London\\inglobe"

# Load the cached postcodes.io results
postcode_cache_file <- paste0(localbig, "\\data\\postcodes_io_results.rds")
postcode_results <- readRDS(postcode_cache_file)

cat("=== Examining postcodes.io results ===\n\n")
cat("Columns:", paste(names(postcode_results), collapse = ", "), "\n\n")

# Check what the nuts column actually contains
cat("Sample of nuts1_code values:\n")
postcode_results %>%
  filter(!is.na(nuts1_code)) %>%
  pull(nuts1_code) %>%
  head(20) %>%
  print()

cat("\n\nUnique nuts1_code patterns:\n")
postcode_results %>%
  filter(!is.na(nuts1_code)) %>%
  mutate(nuts_pattern = str_extract(nuts1_code, "^UK[A-Z0-9]+")) %>%
  count(nuts_pattern) %>%
  head(20) %>%
  print()

# Check how many have valid NUTS codes
cat("\n\nNUTS code status:\n")
cat("Total rows:", nrow(postcode_results), "\n")
cat("With nuts1_code:", sum(!is.na(postcode_results$nuts1_code)), "\n")
cat("Without nuts1_code:", sum(is.na(postcode_results$nuts1_code)), "\n")
