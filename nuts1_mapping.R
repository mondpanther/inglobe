# UK Address to NUTS1 Region Mapping
# Strategy: postcodes.io + city lookup + Mapbox for remaining

library(arrow)
library(dplyr)
library(stringr)
library(collapse)
library(httr)
library(jsonlite)
library(data.table)

# Set paths
localbig <- "C:\\Users\\rmartin\\OneDrive - Imperial College London\\inglobe"

# Load previous analysis results
results <- readRDS(paste0(localbig, "\\data\\postcode_analysis_results.rds"))
df_GB <- results$df_GB

cat("Loaded", nrow(df_GB), "GB records\n\n")

# ============================================
# NUTS1 Reference Data
# ============================================

nuts1_regions <- data.frame(
  nuts1_code = c("UKC", "UKD", "UKE", "UKF", "UKG", "UKH", "UKI", "UKJ", "UKK", "UKL", "UKM", "UKN"),
  nuts1_name = c("North East", "North West", "Yorkshire and The Humber", "East Midlands",
                 "West Midlands", "East of England", "London", "South East", "South West",
                 "Wales", "Scotland", "Northern Ireland")
)

# City/County to NUTS1 lookup (for short addresses)
city_nuts1_lookup <- data.frame(
  city_upper = c(
    # London
    "LONDON", "GREATER LONDON", "CITY OF LONDON",
    # South East
    "SURREY", "KENT", "SUSSEX", "EAST SUSSEX", "WEST SUSSEX", "HAMPSHIRE",
    "BERKSHIRE", "OXFORDSHIRE", "BUCKINGHAMSHIRE", "HERTFORDSHIRE", "ESSEX",
    "BRIGHTON", "READING", "OXFORD", "MILTON KEYNES", "SOUTHAMPTON", "PORTSMOUTH",
    "GUILDFORD", "SLOUGH", "WATFORD", "CRAWLEY", "MAIDSTONE", "CANTERBURY",
    # East of England
    "CAMBRIDGESHIRE", "NORFOLK", "SUFFOLK", "BEDFORDSHIRE", "LUTON",
    "CAMBRIDGE", "NORWICH", "IPSWICH", "PETERBOROUGH", "COLCHESTER",
    # South West
    "BRISTOL", "DEVON", "CORNWALL", "SOMERSET", "DORSET", "WILTSHIRE", "GLOUCESTERSHIRE",
    "BATH", "EXETER", "PLYMOUTH", "BOURNEMOUTH", "SWINDON", "CHELTENHAM", "GLOUCESTER",
    # West Midlands
    "BIRMINGHAM", "WEST MIDLANDS", "COVENTRY", "WOLVERHAMPTON", "WARWICKSHIRE",
    "STAFFORDSHIRE", "WORCESTERSHIRE", "SHROPSHIRE", "HEREFORDSHIRE",
    "STOKE-ON-TRENT", "DUDLEY", "WALSALL", "SOLIHULL", "WORCESTER", "WARWICK",
    # East Midlands
    "NOTTINGHAMSHIRE", "DERBYSHIRE", "LEICESTERSHIRE", "LINCOLNSHIRE", "NORTHAMPTONSHIRE", "RUTLAND",
    "NOTTINGHAM", "DERBY", "LEICESTER", "LINCOLN", "NORTHAMPTON",
    # Yorkshire
    "YORKSHIRE", "NORTH YORKSHIRE", "SOUTH YORKSHIRE", "WEST YORKSHIRE", "EAST YORKSHIRE",
    "LEEDS", "SHEFFIELD", "BRADFORD", "YORK", "HULL", "KINGSTON UPON HULL", "HUDDERSFIELD", "DONCASTER",
    # North West
    "MANCHESTER", "GREATER MANCHESTER", "LIVERPOOL", "MERSEYSIDE", "LANCASHIRE", "CHESHIRE", "CUMBRIA",
    "SALFORD", "BOLTON", "STOCKPORT", "WARRINGTON", "BLACKPOOL", "PRESTON", "CHESTER", "CARLISLE",
    # North East
    "NEWCASTLE", "NEWCASTLE UPON TYNE", "SUNDERLAND", "DURHAM", "NORTHUMBERLAND",
    "TYNE AND WEAR", "MIDDLESBROUGH", "GATESHEAD", "DARLINGTON",
    # Wales
    "WALES", "CARDIFF", "SWANSEA", "NEWPORT", "WREXHAM", "GWYNEDD", "PEMBROKESHIRE",
    "CARMARTHENSHIRE", "CEREDIGION", "POWYS", "DENBIGHSHIRE", "FLINTSHIRE", "ANGLESEY",
    "MONMOUTHSHIRE", "GLAMORGAN", "SOUTH GLAMORGAN", "WEST GLAMORGAN", "MID GLAMORGAN",
    # Scotland
    "SCOTLAND", "EDINBURGH", "GLASGOW", "ABERDEEN", "DUNDEE", "INVERNESS",
    "STIRLING", "PERTH", "FIFE", "LOTHIAN", "HIGHLANDS", "LANARKSHIRE",
    "AYRSHIRE", "RENFREWSHIRE", "ARGYLL", "BORDERS", "DUMFRIES", "FALKIRK",
    # Northern Ireland
    "NORTHERN IRELAND", "BELFAST", "LONDONDERRY", "DERRY", "ANTRIM", "DOWN",
    "ARMAGH", "TYRONE", "FERMANAGH", "LISBURN", "NEWRY"
  ),
  nuts1_code = c(
    # London
    "UKI", "UKI", "UKI",
    # South East
    "UKJ", "UKJ", "UKJ", "UKJ", "UKJ", "UKJ",
    "UKJ", "UKJ", "UKJ", "UKJ", "UKJ",
    "UKJ", "UKJ", "UKJ", "UKJ", "UKJ", "UKJ",
    "UKJ", "UKJ", "UKJ", "UKJ", "UKJ", "UKJ",
    # East of England
    "UKH", "UKH", "UKH", "UKH", "UKH",
    "UKH", "UKH", "UKH", "UKH", "UKH",
    # South West
    "UKK", "UKK", "UKK", "UKK", "UKK", "UKK", "UKK",
    "UKK", "UKK", "UKK", "UKK", "UKK", "UKK", "UKK",
    # West Midlands
    "UKG", "UKG", "UKG", "UKG", "UKG",
    "UKG", "UKG", "UKG", "UKG",
    "UKG", "UKG", "UKG", "UKG", "UKG", "UKG",
    # East Midlands
    "UKF", "UKF", "UKF", "UKF", "UKF", "UKF",
    "UKF", "UKF", "UKF", "UKF", "UKF",
    # Yorkshire
    "UKE", "UKE", "UKE", "UKE", "UKE",
    "UKE", "UKE", "UKE", "UKE", "UKE", "UKE", "UKE", "UKE",
    # North West
    "UKD", "UKD", "UKD", "UKD", "UKD", "UKD", "UKD",
    "UKD", "UKD", "UKD", "UKD", "UKD", "UKD", "UKD", "UKD",
    # North East
    "UKC", "UKC", "UKC", "UKC", "UKC",
    "UKC", "UKC", "UKC", "UKC",
    # Wales
    "UKL", "UKL", "UKL", "UKL", "UKL", "UKL", "UKL",
    "UKL", "UKL", "UKL", "UKL", "UKL", "UKL",
    "UKL", "UKL", "UKL", "UKL", "UKL",
    # Scotland
    "UKM", "UKM", "UKM", "UKM", "UKM", "UKM",
    "UKM", "UKM", "UKM", "UKM", "UKM", "UKM",
    "UKM", "UKM", "UKM", "UKM", "UKM", "UKM",
    # Northern Ireland
    "UKN", "UKN", "UKN", "UKN", "UKN", "UKN",
    "UKN", "UKN", "UKN", "UKN", "UKN"
  ),
  stringsAsFactors = FALSE
)

# ============================================
# STEP 1: postcodes.io bulk lookup
# ============================================

cat("=== STEP 1: postcodes.io bulk lookup ===\n\n")

# Get unique postcodes to minimize API calls
postcodes_to_lookup <- df_GB %>%
  fsubset(!is.na(full_postcode)) %>%
  mutate(postcode_clean = gsub("\\s+", " ", str_trim(full_postcode))) %>%
  distinct(postcode_clean) %>%
  pull(postcode_clean)

cat("Unique postcodes to lookup:", length(postcodes_to_lookup), "\n")

# Function to batch lookup postcodes (100 per request)
lookup_postcodes_batch <- function(postcodes, batch_size = 100) {
  n_batches <- ceiling(length(postcodes) / batch_size)
  results_list <- vector("list", n_batches)

  cat("Processing", n_batches, "batches...\n")

  for (i in seq_len(n_batches)) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, length(postcodes))
    batch <- postcodes[start_idx:end_idx]

    # API call
    response <- tryCatch({
      POST(
        "https://api.postcodes.io/postcodes",
        body = list(postcodes = batch),
        encode = "json",
        timeout(30)
      )
    }, error = function(e) {
      cat("Error in batch", i, ":", e$message, "\n")
      return(NULL)
    })

    if (!is.null(response) && status_code(response) == 200) {
      content_data <- content(response, as = "parsed")

      # Extract results
      batch_results <- lapply(content_data$result, function(x) {
        if (!is.null(x$result)) {
          data.frame(
            postcode_clean = x$query,
            nuts1_code = x$result$nuts %||% NA_character_,
            latitude = x$result$latitude %||% NA_real_,
            longitude = x$result$longitude %||% NA_real_,
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            postcode_clean = x$query,
            nuts1_code = NA_character_,
            latitude = NA_real_,
            longitude = NA_real_,
            stringsAsFactors = FALSE
          )
        }
      })

      results_list[[i]] <- do.call(rbind, batch_results)
    } else {
      cat("Failed batch", i, "\n")
      results_list[[i]] <- data.frame(
        postcode_clean = batch,
        nuts1_code = NA_character_,
        latitude = NA_real_,
        longitude = NA_real_,
        stringsAsFactors = FALSE
      )
    }

    # Progress update every 50 batches
    if (i %% 50 == 0) {
      cat("  Completed", i, "/", n_batches, "batches\n")
    }

    # Small delay to be nice to the API
    if (i %% 10 == 0) Sys.sleep(0.1)
  }

  do.call(rbind, results_list)
}

# Check if we already have postcodes.io results cached
postcode_cache_file <- paste0(localbig, "\\data\\postcodes_io_results.rds")

if (file.exists(postcode_cache_file)) {
  cat("Loading cached postcodes.io results...\n")
  postcode_results <- readRDS(postcode_cache_file)
} else {
  cat("Running postcodes.io bulk lookup (this may take a few minutes)...\n")
  postcode_results <- lookup_postcodes_batch(postcodes_to_lookup)
  saveRDS(postcode_results, postcode_cache_file)
  cat("Saved results to cache\n")
}

# Extract just NUTS1 code (postcodes.io returns full NUTS hierarchy)
postcode_results <- postcode_results %>%
  mutate(nuts1_code = str_extract(nuts1_code, "^UK[A-Z]"))

cat("\npostcodes.io results:\n")
cat("  Matched:", sum(!is.na(postcode_results$nuts1_code)), "\n")
cat("  Unmatched:", sum(is.na(postcode_results$nuts1_code)), "\n")

# ============================================
# STEP 2: City/County lookup for short addresses
# ============================================

cat("\n=== STEP 2: City/County lookup ===\n\n")

# Prepare for city matching
df_GB <- df_GB %>%
  mutate(
    addr_length = nchar(person_address),
    address_upper = toupper(str_trim(person_address))
  )

# Join postcode results
df_GB <- df_GB %>%
  mutate(postcode_clean = gsub("\\s+", " ", str_trim(full_postcode))) %>%
  left_join(postcode_results %>% select(postcode_clean, nuts1_code, latitude, longitude),
            by = "postcode_clean")

# For short addresses without postcode match, try city lookup
df_GB <- df_GB %>%
  left_join(city_nuts1_lookup, by = c("address_upper" = "city_upper"), suffix = c("", "_city"))

# Combine NUTS1 from different sources
df_GB <- df_GB %>%
  mutate(
    nuts1_code_final = coalesce(nuts1_code, nuts1_code_city),
    nuts1_source = case_when(
      !is.na(nuts1_code) ~ "postcode",
      !is.na(nuts1_code_city) ~ "city_lookup",
      TRUE ~ NA_character_
    )
  )

cat("After postcode + city lookup:\n")
cat("  Matched:", sum(!is.na(df_GB$nuts1_code_final)), "\n")
cat("  Unmatched:", sum(is.na(df_GB$nuts1_code_final)), "\n")

# ============================================
# STEP 3: Identify addresses for Mapbox geocoding
# ============================================

cat("\n=== STEP 3: Prepare Mapbox geocoding ===\n\n")

# Get unmatched addresses that are worth geocoding
mapbox_candidates <- df_GB %>%
  fsubset(is.na(nuts1_code_final) & addr_length > 15) %>%
  fsubset(!str_detect(address_upper, "DECEASED|DECEASE")) %>%
  # Deduplicate by address to minimize API calls
  distinct(person_address, .keep_all = TRUE) %>%
  # Sort by length (longer = more detail = better geocoding)
  arrange(desc(addr_length))

cat("Unique addresses for Mapbox geocoding:", nrow(mapbox_candidates), "\n")
cat("  Will process top 100,000 this month\n")
cat("  Remaining for next month:", max(0, nrow(mapbox_candidates) - 100000), "\n")

# Take top 100k
mapbox_batch <- mapbox_candidates %>% head(100000)

# Save for Mapbox processing
saveRDS(mapbox_batch, paste0(localbig, "\\data\\mapbox_batch_month1.rds"))

cat("\nSaved mapbox_batch_month1.rds with", nrow(mapbox_batch), "addresses\n")

# ============================================
# STEP 4: Mapbox geocoding function
# ============================================

cat("\n=== STEP 4: Mapbox geocoding ===\n")
cat("Run mapbox_geocode() to process the batch\n\n")

# Mapbox geocoding function using tidygeocoder
mapbox_geocode <- function(batch_size = 1000, start_from = 1) {
  library(tidygeocoder)

  mapbox_batch <- readRDS(paste0(localbig, "\\data\\mapbox_batch_month1.rds"))

  # Check for existing progress
  progress_file <- paste0(localbig, "\\data\\mapbox_progress.rds")
  if (file.exists(progress_file)) {
    existing <- readRDS(progress_file)
    cat("Found existing progress:", nrow(existing), "addresses geocoded\n")

    # Filter out already processed
    mapbox_batch <- mapbox_batch %>%
      anti_join(existing, by = "person_address")

    cat("Remaining to process:", nrow(mapbox_batch), "\n")
  } else {
    existing <- NULL
  }

  if (nrow(mapbox_batch) == 0) {
    cat("All addresses already processed!\n")
    return(existing)
  }

  # Process in batches
  n_batches <- ceiling(min(nrow(mapbox_batch), batch_size * 100) / batch_size)

  for (i in seq_len(n_batches)) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, nrow(mapbox_batch))

    batch <- mapbox_batch[start_idx:end_idx, ]

    cat("Processing batch", i, "/", n_batches, "(", nrow(batch), "addresses)...\n")

    # Geocode with Mapbox
    geocoded <- batch %>%
      mutate(addressstr = paste(person_address, "United Kingdom", sep = ", ")) %>%
      geocode(
        addressstr,
        method = "mapbox",
        lat = latitude_mapbox,
        long = longitude_mapbox
      )

    # Append to existing
    if (is.null(existing)) {
      existing <- geocoded
    } else {
      existing <- bind_rows(existing, geocoded)
    }

    # Save progress
    saveRDS(existing, progress_file)
    cat("  Saved progress:", nrow(existing), "total\n")
  }

  return(existing)
}

# ============================================
# STEP 5: Assign NUTS1 from Mapbox coordinates
# ============================================

assign_nuts1_from_coords <- function() {
  library(sf)

  # Load UK NUTS1 boundaries (download from ONS if not present)
  nuts1_shp_file <- paste0(localbig, "\\data\\NUTS1_boundaries.rds")

  if (!file.exists(nuts1_shp_file)) {
    cat("Downloading UK NUTS1 boundaries...\n")
    # Alternative: use a simple bounding box approach
    # For now, create approximate boundaries

    # Approximate NUTS1 regions by lat/lon (rough boundaries)
    # This is a simplified approach - for production, download actual shapefile
    assign_nuts1_approx <- function(lat, lon) {
      case_when(
        is.na(lat) | is.na(lon) ~ NA_character_,
        # Scotland
        lat > 55.3 & lon < -1.5 ~ "UKM",
        # Northern Ireland
        lon < -5.5 & lat > 54 & lat < 55.5 ~ "UKN",
        # North East
        lat > 54.5 & lon > -2.5 ~ "UKC",
        # North West
        lat > 53 & lat <= 55.3 & lon < -1.5 ~ "UKD",
        # Yorkshire
        lat > 53 & lat <= 54.5 & lon >= -1.5 ~ "UKE",
        # Wales
        lat > 51.3 & lat <= 53.5 & lon < -2.7 ~ "UKL",
        # West Midlands
        lat > 52 & lat <= 53 & lon >= -2.7 & lon < -1.2 ~ "UKG",
        # East Midlands
        lat > 52 & lat <= 53.5 & lon >= -1.2 ~ "UKF",
        # East of England
        lat > 51.5 & lat <= 52.5 & lon > 0 ~ "UKH",
        # London
        lat > 51.3 & lat <= 51.7 & lon > -0.5 & lon < 0.3 ~ "UKI",
        # South East
        lat > 50.5 & lat <= 51.8 & lon > -1.5 & lon < 1.5 &
          !(lat > 51.3 & lat <= 51.7 & lon > -0.5 & lon < 0.3) ~ "UKJ",
        # South West
        lat <= 51.8 & lon < -1.5 ~ "UKK",
        TRUE ~ NA_character_
      )
    }

    return(assign_nuts1_approx)
  }
}

# Make function available
nuts1_from_coords <- assign_nuts1_from_coords()

# ============================================
# Summary and next steps
# ============================================

cat("\n=== CURRENT STATUS ===\n\n")

status <- df_GB %>%
  group_by(nuts1_source) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(pct = round(100 * n / sum(n), 1))

print(status)

cat("\n=== NEXT STEPS ===\n")
cat("1. Run: mapbox_geocode() to start Mapbox geocoding\n")
cat("2. This will process up to 100k addresses and save progress\n
")
cat("3. Can be interrupted and resumed - progress is saved\n")
cat("4. After geocoding, run assign_nuts1_from_coords() to get NUTS1\n")

# Save current state
saveRDS(df_GB, paste0(localbig, "\\data\\df_GB_with_nuts1_partial.rds"))
cat("\nSaved intermediate results to df_GB_with_nuts1_partial.rds\n")
