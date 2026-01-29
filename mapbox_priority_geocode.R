# Mapbox geocoding for priority addresses
# Processes addresses from df_GB_with_nuts1.fst that still need coordinates

library(arrow)
library(dplyr)
library(stringr)
library(collapse)
library(fst)
library(tidygeocoder)

localbig <- "C:\\Users\\rmartin\\OneDrive - Imperial College London\\inglobe"

# ============================================
# Load priority file and check what needs processing
# ============================================

cat("=== Loading Priority File ===\n\n")

priority_file <- paste0(localbig, "\\data\\priority.fst")
df_priority <- read_fst(priority_file)

cat("Total rows in priority file:", nrow(df_priority), "\n")
cat("Columns:", paste(names(df_priority), collapse = ", "), "\n\n")

# Check columns available
cat("Columns:", paste(names(df_priority), collapse = ", "), "\n\n")

# Check if priority file already has person_address
has_address <- "person_address" %in% names(df_priority)
cat("Priority file has person_address:", has_address, "\n")

# Load the nuts file to get coordinates
nuts_file <- paste0(localbig, "\\data\\df_GB_with_nuts1.rds")
if (file.exists(nuts_file)) {
  df_nuts <- readRDS(nuts_file)
  cat("df_nuts columns:", paste(names(df_nuts), collapse = ", "), "\n")

  # Join with nuts data for coordinates (only columns we don't already have)
  df_priority <- df_priority %>%
    left_join(
      df_nuts %>% select(person_id, latitude, longitude, nuts1_code, nuts1_name),
      by = "person_id"
    )
  cat("After joining with nuts data:", nrow(df_priority), "rows\n")
}

cat("Final columns:", paste(names(df_priority), collapse = ", "), "\n\n")

# Verify person_address exists
if (!"person_address" %in% names(df_priority)) {
  stop("ERROR: person_address column not found after joins. Check the data files.")
}

# Check what's missing coordinates
has_lat <- "latitude" %in% names(df_priority)
if (has_lat) {
  needs_geocoding <- df_priority %>%
    filter(is.na(latitude) | is.na(longitude))
  cat("Rows needing geocoding:", nrow(needs_geocoding), "\n")

  # Check what's missing NUTS1
  if ("nuts1_code" %in% names(df_priority)) {
    needs_nuts1 <- df_priority %>%
      filter(is.na(nuts1_code))
    cat("Rows missing NUTS1:", nrow(needs_nuts1), "\n\n")
  }
} else {
  # No coordinates yet - all need geocoding
  needs_geocoding <- df_priority
  cat("No coordinates found - all", nrow(needs_geocoding), "rows need geocoding\n\n")
}

# ============================================
# Prepare unique addresses for Mapbox
# ============================================

cat("needs_geocoding columns:", paste(names(needs_geocoding), collapse = ", "), "\n")
cat("needs_geocoding rows:", nrow(needs_geocoding), "\n\n")

# Get unique addresses to minimize API calls
unique_addresses <- needs_geocoding %>%
  filter(!is.na(person_address) & nchar(person_address) > 5) %>%
  distinct(person_address) %>%
  mutate(addr_length = nchar(person_address)) %>%
  arrange(desc(addr_length))  # Longer addresses first (more detail)

cat("Unique addresses to geocode:", nrow(unique_addresses), "\n")

if (nrow(unique_addresses) > 100000) {
  cat("Note: More than 100k addresses - will process first 100k (Mapbox free tier limit)\n")
  unique_addresses <- unique_addresses %>% head(100000)
}

cat("\nAddress length distribution:\n")
unique_addresses %>%
  mutate(len_cat = cut(addr_length, breaks = c(0, 15, 30, 50, 100, Inf),
                       labels = c("1-15", "16-30", "31-50", "51-100", "100+"))) %>%
  count(len_cat) %>%
  print()

# ============================================
# Mapbox geocoding with progress saving
# ============================================

mapbox_geocode_priority <- function(batch_size = 500) {

  progress_file <- paste0(localbig, "\\data\\mapbox_priority_progress.rds")

  # Check for existing progress
  if (file.exists(progress_file)) {
    existing <- readRDS(progress_file)
    cat("\nFound existing progress:", nrow(existing), "addresses geocoded\n")

    # Filter out already processed
    to_process <- unique_addresses %>%
      anti_join(existing, by = "person_address")

    cat("Remaining to process:", nrow(to_process), "\n")
  } else {
    existing <- data.frame()
    to_process <- unique_addresses
  }

  if (nrow(to_process) == 0) {
    cat("All addresses already processed!\n")
    return(existing)
  }

  # Process in batches
  total_batches <- ceiling(nrow(to_process) / batch_size)
  cat("\nProcessing", nrow(to_process), "addresses in", total_batches, "batches...\n\n")

  for (i in seq_len(total_batches)) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, nrow(to_process))

    batch <- to_process[start_idx:end_idx, ]

    cat("Batch", i, "/", total_batches, "(", nrow(batch), "addresses)...")

    # Geocode with Mapbox - add UK to help accuracy
    geocoded <- tryCatch({
      batch %>%
        mutate(addressstr = paste(person_address, "United Kingdom", sep = ", ")) %>%
        geocode(
          addressstr,
          method = "mapbox",
          lat = latitude,
          long = longitude
        ) %>%
        select(person_address, latitude, longitude)
    }, error = function(e) {
      cat(" ERROR:", e$message, "\n")
      return(NULL)
    })

    if (!is.null(geocoded)) {
      # Count successes
      n_success <- sum(!is.na(geocoded$latitude))
      cat(" Success:", n_success, "/", nrow(batch), "\n")

      # Append to existing
      if (nrow(existing) == 0) {
        existing <- geocoded
      } else {
        existing <- bind_rows(existing, geocoded)
      }

      # Save progress after each batch
      saveRDS(existing, progress_file)
    }

    # Small delay between batches
    Sys.sleep(0.5)
  }

  cat("\n=== Geocoding Complete ===\n")
  cat("Total geocoded:", nrow(existing), "\n")
  cat("With coordinates:", sum(!is.na(existing$latitude)), "\n")

  return(existing)
}

# ============================================
# NUTS1 assignment function (same as before)
# ============================================

assign_nuts1_from_coords <- function(lat, lon) {
  case_when(
    is.na(lat) | is.na(lon) ~ NA_character_,
    lon < -5.2 & lat > 54.0 & lat < 55.4 ~ "UKN",
    lat > 55.3 ~ "UKM",
    lat > 54.5 & lat <= 55.8 & lon > -2.5 & lon < -0.8 ~ "UKC",
    lat > 53.0 & lat <= 55.3 & lon >= -3.2 & lon < -1.8 ~ "UKD",
    lat > 53.3 & lat <= 54.6 & lon >= -1.8 & lon < 0.0 ~ "UKE",
    lat >= 51.3 & lat <= 53.5 & lon < -2.6 ~ "UKL",
    lat > 52.0 & lat <= 53.0 & lon >= -2.6 & lon < -1.4 ~ "UKG",
    lat > 52.0 & lat <= 53.6 & lon >= -1.4 & lon < 0.2 ~ "UKF",
    lat > 51.5 & lat <= 52.8 & lon >= 0.0 & lon < 1.8 ~ "UKH",
    lat > 51.28 & lat <= 51.7 & lon >= -0.52 & lon < 0.28 ~ "UKI",
    lat > 50.7 & lat <= 52.0 & lon >= -1.8 & lon < 1.5 &
      !(lat > 51.28 & lat <= 51.7 & lon >= -0.52 & lon < 0.28) ~ "UKJ",
    lat <= 52.0 & lon < -1.8 ~ "UKK",
    lat <= 51.3 & lon >= -1.8 & lon < -0.5 ~ "UKK",
    lat > 50.0 & lat < 56.0 & lon > -6.0 & lon < 2.0 ~ "UKJ",
    TRUE ~ NA_character_
  )
}

# ============================================
# Merge results back to priority file
# ============================================

finalize_results <- function() {
  progress_file <- paste0(localbig, "\\data\\mapbox_priority_progress.rds")

  if (!file.exists(progress_file)) {
    cat("No Mapbox results found. Run mapbox_geocode_priority() first.\n")
    return(NULL)
  }

  geocoded <- readRDS(progress_file)

  cat("Merging", nrow(geocoded), "geocoded addresses back to priority file...\n")
  cat("Geocoded columns:", paste(names(geocoded), collapse = ", "), "\n")

  # Reload priority file (already has person_address)
  df_priority <- read_fst(paste0(localbig, "\\data\\priority.fst"))
  cat("Priority columns:", paste(names(df_priority), collapse = ", "), "\n")

  # Join with nuts data for existing coordinates (only coord columns)
  df_nuts <- readRDS(paste0(localbig, "\\data\\df_GB_with_nuts1.rds"))
  df_priority <- df_priority %>%
    left_join(
      df_nuts %>% select(person_id, latitude, longitude, nuts1_code, nuts1_name),
      by = "person_id"
    )

  cat("After nuts join columns:", paste(names(df_priority), collapse = ", "), "\n")

  # Merge new coordinates from Mapbox
  df_updated <- df_priority %>%
    left_join(
      geocoded %>% select(person_address, lat_new = latitude, lon_new = longitude),
      by = "person_address"
    ) %>%
    mutate(
      latitude = coalesce(latitude, lat_new),
      longitude = coalesce(longitude, lon_new)
    ) %>%
    select(-lat_new, -lon_new)

  # Assign NUTS1 for any rows that now have coordinates
  df_updated <- df_updated %>%
    mutate(
      nuts1_code = if_else(
        is.na(nuts1_code) & !is.na(latitude),
        assign_nuts1_from_coords(latitude, longitude),
        nuts1_code
      )
    )

  # Add NUTS1 names
  nuts1_names <- data.frame(
    nuts1_code = c("UKC", "UKD", "UKE", "UKF", "UKG", "UKH", "UKI", "UKJ", "UKK", "UKL", "UKM", "UKN"),
    nuts1_name_new = c("North East", "North West", "Yorkshire and The Humber", "East Midlands",
                   "West Midlands", "East of England", "London", "South East", "South West",
                   "Wales", "Scotland", "Northern Ireland")
  )

  df_updated <- df_updated %>%
    left_join(nuts1_names, by = "nuts1_code") %>%
    mutate(nuts1_name = coalesce(nuts1_name, nuts1_name_new)) %>%
    select(-nuts1_name_new)

  # Summary
  cat("\n=== Final Results ===\n")
  cat("Total rows:", nrow(df_updated), "\n")
  cat("With coordinates:", sum(!is.na(df_updated$latitude)), "\n")
  cat("With NUTS1:", sum(!is.na(df_updated$nuts1_code)), "\n")

  cat("\nNUTS1 distribution:\n")
  df_updated %>%
    count(nuts1_code, nuts1_name) %>%
    arrange(desc(n)) %>%
    print()

  # Save
  write_fst(df_updated, paste0(localbig, "\\data\\priority_with_nuts1.fst"))
  cat("\nSaved to priority_with_nuts1.fst\n")

  return(df_updated)
}

# ============================================
# Instructions
# ============================================

cat("\n=== READY TO RUN ===\n\n")
cat("Step 1: Run mapbox_geocode_priority() to geocode addresses\n")
cat("        - Progress is saved after each batch\n
")
cat("        - Can stop and resume anytime\n\n")
cat("Step 2: Run finalize_results() to merge coordinates and assign NUTS1\n\n")
