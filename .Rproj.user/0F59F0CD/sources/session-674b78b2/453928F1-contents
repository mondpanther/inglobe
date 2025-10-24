# Random World City Network with 200 Cities and 200 Connections
# Required packages: leaflet, dplyr, geosphere, maps, htmlwidgets

# Install packages if needed
# install.packages(c("leaflet", "dplyr", "geosphere", "maps", "htmlwidgets"))

# Load required packages with error checking
required_packages <- c("leaflet", "dplyr", "geosphere", "maps", "htmlwidgets")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(paste("Missing required packages:", paste(missing_packages, collapse = ", "),
             "\nPlease install them using: install.packages(c('",
             paste(missing_packages, collapse = "', '"), "'))", sep = ""))
}

library(leaflet)
library(dplyr)
library(geosphere)
library(maps)
library(htmlwidgets)

# Set seed for reproducibility
set.seed(123)

cat("=== Random World City Network ===\n\n")

# Step 1: Load and sample world cities
# =====================================

data(world.cities)

# Check if data loaded successfully
if (!exists("world.cities")) {
  stop("Error: world.cities dataset not found. Please install the 'maps' package.")
}

cat("Loaded", nrow(world.cities), "cities from world.cities dataset\n")

# Filter for larger cities and sample 200 random cities
major_cities <- world.cities %>%
  filter(pop > 100000) %>%
  sample_n(200) %>%
  select(name, country.etc, lat, long, pop) %>%
  mutate(
    city_id = 1:n(),
    city_label = paste0(name, ", ", country.etc)
  )

cat("Selected", nrow(major_cities), "cities\n\n")


# Step 2: Generate 200 random directed connections
# =================================================

n_connections <- 200

# Generate connections with replacement (allows multiple routes between same cities)
connections <- data.frame(
  from_id = sample(major_cities$city_id, n_connections, replace = TRUE),
  to_id = sample(major_cities$city_id, n_connections, replace = TRUE)
) %>%
  filter(from_id != to_id)  # Remove self-loops

cat("Generated", nrow(connections), "connections (after removing self-loops)\n")

# Track duplicate connections for offset visualization
connections <- connections %>%
  group_by(from_id, to_id) %>%
  mutate(
    connection_count = n(),
    connection_index = row_number()
  ) %>%
  ungroup()

cat("Connections with duplicates:", sum(connections$connection_count > 1), "\n\n")


# Step 3: Helper functions
# =========================

# Function to adjust coordinates for dateline crossing
adjust_for_dateline <- function(lon1, lat1, lon2, lat2) {
  if (abs(lon2 - lon1) > 180) {
    if (lon1 < lon2) {
      lon1 <- lon1 + 360
    } else {
      lon2 <- lon2 + 360
    }
  }
  return(list(lon1 = lon1, lat1 = lat1, lon2 = lon2, lat2 = lat2))
}

# Function to create smooth offset for parallel routes
offset_route_smooth <- function(route_coords, offset_degrees = 0) {
  if (offset_degrees == 0 || nrow(route_coords) < 3) {
    return(route_coords)
  }

  n_points <- nrow(route_coords)
  offset_route <- route_coords

  # Calculate midpoint
  mid_idx <- round(n_points / 2)

  # Calculate perpendicular direction at midpoint
  if (mid_idx > 1 && mid_idx < n_points) {
    idx_before <- max(1, mid_idx - 10)
    idx_after <- min(n_points, mid_idx + 10)

    dx <- route_coords[idx_after, 1] - route_coords[idx_before, 1]
    dy <- route_coords[idx_after, 2] - route_coords[idx_before, 2]

    length <- sqrt(dx^2 + dy^2)
    if (length > 0) {
      perp_x <- -dy / length
      perp_y <- dx / length

      # Apply smooth offset using a sine curve
      for (i in 2:(n_points - 1)) {
        t <- (i - 1) / (n_points - 1)
        weight <- sin(t * pi)  # Peaks in middle, tapers to 0 at ends

        offset_route[i, 1] <- route_coords[i, 1] + perp_x * offset_degrees * weight
        offset_route[i, 2] <- route_coords[i, 2] + perp_y * offset_degrees * weight
      }
    }
  }

  return(offset_route)
}

# Function to add arrow decorator to polylines
add_arrow_decorator <- function(map, route_coords, color = "#3498db",
                                weight = 2, opacity = 0.6, label = NULL) {
  # Add the main polyline
  map <- map %>%
    addPolylines(
      lng = route_coords[, 1],
      lat = route_coords[, 2],
      color = color,
      weight = weight,
      opacity = opacity,
      label = label
    )

  # Calculate arrow position (85% along the route)
  n_points <- nrow(route_coords)
  arrow_pos <- round(n_points * 0.85)

  if (arrow_pos < n_points - 5 && arrow_pos > 0) {
    # Get direction vector for arrow
    p1 <- route_coords[arrow_pos, ]
    p2 <- route_coords[min(arrow_pos + 5, n_points), ]

    # Calculate arrow head
    dx <- p2[1] - p1[1]
    dy <- p2[2] - p1[2]
    angle <- atan2(dy, dx)

    arrow_length <- 1.5
    arrow_angle <- 25 * pi / 180

    arrow_left <- c(
      p2[1] - arrow_length * cos(angle - arrow_angle),
      p2[2] - arrow_length * sin(angle - arrow_angle)
    )
    arrow_right <- c(
      p2[1] - arrow_length * cos(angle + arrow_angle),
      p2[2] - arrow_length * sin(angle + arrow_angle)
    )

    # Draw arrow head
    map <- map %>%
      addPolylines(
        lng = c(arrow_left[1], p2[1], arrow_right[1]),
        lat = c(arrow_left[2], p2[2], arrow_right[2]),
        color = color,
        weight = weight + 1,
        opacity = opacity
      )
  }

  return(map)
}


# Step 4: Create the network map
# ===============================

cat("Creating interactive map...\n")

# Create base map
network_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView(lng = 0, lat = 20, zoom = 2)

# Add city markers
network_map <- network_map %>%
  addCircleMarkers(
    data = major_cities,
    lng = ~long,
    lat = ~lat,
    radius = 3,
    color = "#e74c3c",
    fillColor = "#e74c3c",
    fillOpacity = 0.7,
    stroke = FALSE,
    popup = ~paste0("<b>", city_label, "</b><br>",
                    "Population: ", format(pop, big.mark = ","))
  )

# Add connections with progress bar
cat("Adding", nrow(connections), "connections...\n")
pb <- txtProgressBar(min = 0, max = nrow(connections), style = 3)

color_palette <- c("#3498db", "#2ecc71", "#9b59b6", "#f39c12", "#1abc9c")

for (i in 1:nrow(connections)) {
  setTxtProgressBar(pb, i)

  from_city <- major_cities[major_cities$city_id == connections$from_id[i], ]
  to_city <- major_cities[major_cities$city_id == connections$to_id[i], ]

  if (nrow(from_city) > 0 && nrow(to_city) > 0) {
    # Adjust for dateline crossing
    adjusted <- adjust_for_dateline(
      from_city$long, from_city$lat,
      to_city$long, to_city$lat
    )

    # Calculate great circle route
    route <- gcIntermediate(
      c(adjusted$lon1, adjusted$lat1),
      c(adjusted$lon2, adjusted$lat2),
      n = 100,
      addStartEnd = TRUE,
      sp = FALSE
    )

    # Normalize longitudes to -180 to 180 range
    route[, 1] <- ifelse(route[, 1] > 180, route[, 1] - 360, route[, 1])
    route[, 1] <- ifelse(route[, 1] < -180, route[, 1] + 360, route[, 1])

    # Apply smooth offset for duplicate routes
    if (connections$connection_count[i] > 1) {
      offset_multiplier <- ceiling(connections$connection_index[i] / 2)
      offset_direction <- ifelse(connections$connection_index[i] %% 2 == 1, 1, -1)
      offset_degrees <- offset_direction * offset_multiplier * 0.8

      route <- offset_route_smooth(route, offset_degrees)
    }

    # Vary colors
    route_color <- sample(color_palette, 1)

    # Add route with arrow
    network_map <- add_arrow_decorator(
      network_map,
      route,
      color = route_color,
      weight = 1.5,
      opacity = 0.4,
      label = paste(from_city$city_label, "→", to_city$city_label)
    )
  }
}

close(pb)
cat("\n\nMap created successfully!\n")


# Step 5: Calculate and display statistics
# =========================================

cat("\n=== Network Statistics ===\n")

# Calculate connection statistics per city
city_stats <- connections %>%
  group_by(from_id) %>%
  summarise(outgoing = n()) %>%
  full_join(
    connections %>%
      group_by(to_id) %>%
      summarise(incoming = n()),
    by = c("from_id" = "to_id")
  ) %>%
  mutate(
    incoming = ifelse(is.na(incoming), 0, incoming),
    outgoing = ifelse(is.na(outgoing), 0, outgoing),
    total = incoming + outgoing
  )

# Merge with cities
major_cities_with_stats <- major_cities %>%
  left_join(city_stats, by = c("city_id" = "from_id")) %>%
  mutate(
    total = ifelse(is.na(total), 0, total),
    incoming = ifelse(is.na(incoming), 0, incoming),
    outgoing = ifelse(is.na(outgoing), 0, outgoing)
  )

cat("Total cities:", nrow(major_cities), "\n")
cat("Total connections:", nrow(connections), "\n")
cat("Average connections per city:", round(mean(major_cities_with_stats$total), 2), "\n")
cat("Most connected city:",
    major_cities_with_stats$city_label[which.max(major_cities_with_stats$total)],
    "with", max(major_cities_with_stats$total), "connections\n")
cat("Cities with no connections:",
    sum(major_cities_with_stats$total == 0), "\n")

# Find city pairs with most connections
duplicate_pairs <- connections %>%
  group_by(from_id, to_id) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(count > 1) %>%
  arrange(desc(count))

if (nrow(duplicate_pairs) > 0) {
  cat("\nTop city pairs with multiple connections:\n")
  for (i in 1:min(5, nrow(duplicate_pairs))) {
    from <- major_cities$city_label[major_cities$city_id == duplicate_pairs$from_id[i]]
    to <- major_cities$city_label[major_cities$city_id == duplicate_pairs$to_id[i]]
    cat(sprintf("  %s → %s: %d connections\n", from, to, duplicate_pairs$count[i]))
  }
}

cat("\n")


# Step 6: Display and optionally save the map
# ============================================

# Display the map
print(network_map)

# Save the map (uncomment to save)
# saveWidget(network_map, "random_world_network.html", selfcontained = TRUE)
# cat("Map saved to: random_world_network.html\n")
