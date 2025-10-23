# Random World City Network with 200 Cities and 200 Connections
# Required packages: leaflet, dplyr, geosphere, maps, RColorBrewer

# Install packages if needed
# install.packages(c("leaflet", "dplyr", "geosphere", "maps", "RColorBrewer", "htmlwidgets"))

# Load required packages with error checking
required_packages <- c("leaflet", "dplyr", "geosphere", "maps", "RColorBrewer", "htmlwidgets")
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

set.seed(123)  # For reproducibility - remove or change for different random networks

# Step 1: Get world cities data
# ================================

# Load world cities from the maps package
data(world.cities)

# Check if data loaded successfully
if (!exists("world.cities")) {
  stop("Error: world.cities dataset not found. Please install the 'maps' package: install.packages('maps')")
}

cat("Loaded", nrow(world.cities), "cities from world.cities dataset\n")

# Filter for larger cities and sample 200 random cities
# Using cities with population > 100,000 for better distribution
major_cities <- world.cities %>%
  filter(pop > 100000) %>%
  sample_n(200) %>%
  select(name, country.etc, lat, long, pop) %>%
  mutate(city_id = 1:n(),
         city_label = paste0(name, ", ", country.etc))

head(major_cities)
cat("Selected", nrow(major_cities), "cities\n")

# Step 2: Generate 200 random directed connections
# =================================================

# Sample connections with replacement (allows multiple connections between same cities)
connections <- data.frame(
  from_id = sample(major_cities$city_id, 200, replace = TRUE),
  to_id = sample(major_cities$city_id, 200, replace = TRUE)
) %>%
  # Remove self-loops (city to itself)
  filter(from_id != to_id) %>%
  mutate(connection_id = 1:n())

cat("Generated", nrow(connections), "connections (after removing self-loops)\n")

# Count connections between same city pairs
connection_counts <- connections %>%
  mutate(pair = paste(pmin(from_id, to_id), pmax(from_id, to_id), sep = "-"),
         direction = ifelse(from_id < to_id, "forward", "backward")) %>%
  group_by(from_id, to_id) %>%
  mutate(
    pair_count = n(),
    pair_index = row_number()
  ) %>%
  ungroup()

# Show some statistics
cat("\nConnection Statistics:\n")
cat("Total connections:", nrow(connection_counts), "\n")
cat("Unique city pairs:", length(unique(connection_counts$pair)), "\n")
cat("Max connections between same pair:", max(connection_counts$pair_count), "\n")

# Cities with most outgoing connections
top_hubs <- connection_counts %>%
  group_by(from_id) %>%
  summarise(outgoing = n()) %>%
  arrange(desc(outgoing)) %>%
  head(5) %>%
  left_join(major_cities, by = c("from_id" = "city_id"))

cat("\nTop 5 hub cities (by outgoing connections):\n")
print(top_hubs %>% select(city_label, outgoing))


# Step 3: Function to offset parallel routes
# ===========================================

# This function creates slightly offset routes for parallel connections
create_offset_route <- function(from_lon, from_lat, to_lon, to_lat, 
                                offset_factor = 0, n_points = 100) {
  
  # Get the great circle route
  route <- gcIntermediate(
    c(from_lon, from_lat),
    c(to_lon, to_lat),
    n = n_points,
    addStartEnd = TRUE,
    sp = FALSE
  )
  
  # If no offset needed, return original route
  if (offset_factor == 0) {
    return(route)
  }
  
  # Calculate perpendicular offset
  # Find midpoint
  mid_idx <- round(nrow(route) / 2)
  
  # Calculate bearing at midpoint
  if (mid_idx < nrow(route)) {
    bearing <- bearing(route[mid_idx, ], route[mid_idx + 1, ])
    
    # Perpendicular bearing (90 degrees offset)
    perp_bearing <- (bearing + 90) %% 360
    
    # Calculate offset distance (in meters, then convert to rough degrees)
    # Base offset of ~100km per offset level
    offset_distance <- offset_factor * 100000  # meters
    
    # Apply offset to middle portion of route (40% to 60% of route)
    start_offset <- round(nrow(route) * 0.4)
    end_offset <- round(nrow(route) * 0.6)
    
    for (i in start_offset:end_offset) {
      # Gradual offset (more in middle, less at edges of offset region)
      offset_proportion <- 1 - abs(i - mid_idx) / (end_offset - start_offset)
      current_offset <- offset_distance * offset_proportion
      
      offset_point <- destPoint(
        route[i, ],
        perp_bearing,
        current_offset
      )
      route[i, ] <- offset_point
    }
  }
  
  return(route)
}


# Step 4: Function to add arrow decorator
# ========================================

add_arrow_decorator <- function(map, route_coords, color = "#3498db", 
                                weight = 1.5, opacity = 0.5, label = NULL,
                                group = NULL) {
  # Add the main polyline
  map <- map %>%
    addPolylines(
      lng = route_coords[, 1],
      lat = route_coords[, 2],
      color = color,
      weight = weight,
      opacity = opacity,
      label = label,
      group = group
    )
  
  # Calculate arrow position (near the end of the line)
  n_points <- nrow(route_coords)
  arrow_pos <- round(n_points * 0.85)
  
  if (arrow_pos < n_points - 5) {
    # Get direction vector for arrow
    p1 <- route_coords[arrow_pos, ]
    p2 <- route_coords[arrow_pos + 5, ]
    
    # Calculate arrow head points
    dx <- p2[1] - p1[1]
    dy <- p2[2] - p1[2]
    angle <- atan2(dy, dx)
    
    # Arrow parameters (smaller for dense networks)
    arrow_length <- 0.8
    arrow_angle <- 25 * pi / 180
    
    # Calculate arrow head points
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
        weight = weight + 0.5,
        opacity = opacity,
        group = group
      )
  }
  
  return(map)
}


# Step 5: Create the network map
# ===============================

cat("\nCreating map... (this may take a minute with 200+ connections)\n")

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
    color = "#f39c12",
    fillColor = "#f39c12",
    fillOpacity = 0.8,
    stroke = TRUE,
    weight = 1,
    popup = ~paste0("<b>", city_label, "</b><br>",
                    "Population: ", format(pop, big.mark = ",")),
    group = "Cities"
  )

# Add all connections with offsets for parallel routes
# Create country-based color palette
# Get unique countries from cities with outgoing connections
countries_with_connections <- connection_counts %>%
  left_join(major_cities, by = c("from_id" = "city_id")) %>%
  distinct(country.etc) %>%
  pull(country.etc)

# Generate distinct colors for each country using a colorful palette
library(RColorBrewer)
# Use a combination of color palettes to get enough distinct colors
n_countries <- length(countries_with_connections)

if (n_countries <= 12) {
  country_colors <- brewer.pal(max(3, n_countries), "Set3")[1:n_countries]
} else if (n_countries <= 24) {
  country_colors <- c(
    brewer.pal(12, "Set3"),
    brewer.pal(max(3, n_countries - 12), "Paired")
  )[1:n_countries]
} else {
  # For many countries, use rainbow with high saturation
  country_colors <- rainbow(n_countries, s = 0.8, v = 0.8)
}

# Create named vector mapping country to color
country_color_map <- setNames(country_colors, countries_with_connections)

cat("\nColor-coding connections from", n_countries, "different countries\n")

for (i in 1:nrow(connection_counts)) {
  # Get from and to cities
  from_city <- major_cities[major_cities$city_id == connection_counts$from_id[i], ]
  to_city <- major_cities[major_cities$city_id == connection_counts$to_id[i], ]
  
  # Calculate offset based on how many connections exist between this pair
  # If there are multiple connections, offset them
  if (connection_counts$pair_count[i] > 1) {
    # Offset alternates sides and increases with each parallel connection
    offset_magnitude <- ceiling(connection_counts$pair_index[i] / 2) * 0.3
    offset_sign <- ifelse(connection_counts$pair_index[i] %% 2 == 0, 1, -1)
    offset_factor <- offset_magnitude * offset_sign
  } else {
    offset_factor <- 0
  }
  
  # Create route with offset
  route <- create_offset_route(
    from_city$long, from_city$lat,
    to_city$long, to_city$lat,
    offset_factor = offset_factor,
    n_points = 100
  )

  # Choose color based on origin country
  route_color <- country_color_map[from_city$country.etc]
  
  # Add route with arrow
  network_map <- add_arrow_decorator(
    network_map,
    route,
    color = route_color,
    weight = 1.5,
    opacity = 0.4,
    label = paste0(from_city$city_label, " → ", to_city$city_label,
                   " (from ", from_city$country.etc, ")"),
    group = "Connections"
  )
  
  # Progress indicator
  if (i %% 100 == 0) {
    cat("  Processed", i, "of", nrow(connection_counts), "connections\n")
  }
}

cat("\nMap creation complete!\n")

# Add layer controls
network_map <- network_map %>%
  addLayersControl(
    overlayGroups = c("Cities", "Connections"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Display the map
print(network_map)

# Step 6: Save the map
# ====================

# Save as HTML file
library(htmlwidgets)
saveWidget(network_map, "/mnt/user-data/outputs/random_world_network.html", 
           selfcontained = TRUE)
cat("\nMap saved to: random_world_network.html\n")


# Step 7: Network Statistics
# ===========================

cat("\n=== NETWORK STATISTICS ===\n")
cat("Total cities:", nrow(major_cities), "\n")
cat("Total connections:", nrow(connection_counts), "\n")
cat("Average connections per city:", 
    round(nrow(connection_counts) / nrow(major_cities), 2), "\n")

# Degree distribution
in_degree <- connection_counts %>% 
  group_by(to_id) %>% 
  summarise(in_deg = n())
out_degree <- connection_counts %>% 
  group_by(from_id) %>% 
  summarise(out_deg = n())

cat("Average in-degree:", round(mean(in_degree$in_deg), 2), "\n")
cat("Average out-degree:", round(mean(out_degree$out_deg), 2), "\n")

# Country-based connection statistics
cat("\n=== COUNTRY COLOR MAPPING ===\n")
country_connection_stats <- connection_counts %>%
  left_join(major_cities, by = c("from_id" = "city_id")) %>%
  group_by(country.etc) %>%
  summarise(
    num_connections = n(),
    num_cities = n_distinct(from_id)
  ) %>%
  arrange(desc(num_connections)) %>%
  mutate(color = country_color_map[country.etc])

cat("Top 15 countries by number of outgoing connections:\n")
top_countries <- head(country_connection_stats, 15)
for (i in 1:nrow(top_countries)) {
  cat(sprintf("  %2d. %-25s: %3d connections, %2d cities (color: %s)\n",
              i,
              top_countries$country.etc[i],
              top_countries$num_connections[i],
              top_countries$num_cities[i],
              top_countries$color[i]))
}

# Parallel connections statistics
parallel_stats <- connection_counts %>%
  filter(pair_count > 1) %>%
  group_by(pair) %>%
  summarise(count = n()) %>%
  group_by(count) %>%
  summarise(num_pairs = n())

cat("\nParallel connection distribution:\n")
print(parallel_stats)

# Show some examples of cities with parallel connections
example_parallel <- connection_counts %>%
  filter(pair_count > 2) %>%
  head(3) %>%
  left_join(major_cities, by = c("from_id" = "city_id")) %>%
  left_join(major_cities, by = c("to_id" = "city_id"), suffix = c("_from", "_to"))

if (nrow(example_parallel) > 0) {
  cat("\nExample of parallel connections:\n")
  print(example_parallel %>% 
          select(city_label_from, city_label_to, pair_count, pair_index))
}


# Optional: Create a simplified version with fewer connections for testing
# =========================================================================

create_smaller_network <- function(n_cities = 50, n_connections = 50) {
  cat("\n=== Creating smaller test network ===\n")
  
  # Sample cities
  small_cities <- world.cities %>%
    filter(pop > 100000) %>%
    sample_n(n_cities) %>%
    select(name, country.etc, lat, long, pop) %>%
    mutate(city_id = 1:n(),
           city_label = paste0(name, ", ", country.etc))
  
  # Generate connections
  small_connections <- data.frame(
    from_id = sample(small_cities$city_id, n_connections, replace = TRUE),
    to_id = sample(small_cities$city_id, n_connections, replace = TRUE)
  ) %>%
    filter(from_id != to_id) %>%
    mutate(connection_id = 1:n()) %>%
    group_by(from_id, to_id) %>%
    mutate(
      pair_count = n(),
      pair_index = row_number()
    ) %>%
    ungroup()
  
  # Create map
  small_map <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(lng = 0, lat = 20, zoom = 2)
  
  # Add cities
  small_map <- small_map %>%
    addCircleMarkers(
      data = small_cities,
      lng = ~long,
      lat = ~lat,
      radius = 5,
      color = "#e74c3c",
      fillColor = "#e74c3c",
      fillOpacity = 0.8,
      popup = ~city_label
    )

  # Create country color mapping for small network
  small_countries <- small_connections %>%
    left_join(small_cities, by = c("from_id" = "city_id")) %>%
    distinct(country.etc) %>%
    pull(country.etc)

  n_small_countries <- length(small_countries)
  if (n_small_countries <= 12) {
    small_country_colors <- brewer.pal(max(3, n_small_countries), "Set3")[1:n_small_countries]
  } else {
    small_country_colors <- rainbow(n_small_countries, s = 0.8, v = 0.8)
  }
  small_country_color_map <- setNames(small_country_colors, small_countries)

  # Add connections
  for (i in 1:nrow(small_connections)) {
    from_city <- small_cities[small_cities$city_id == small_connections$from_id[i], ]
    to_city <- small_cities[small_cities$city_id == small_connections$to_id[i], ]

    # Calculate offset
    if (small_connections$pair_count[i] > 1) {
      offset_magnitude <- ceiling(small_connections$pair_index[i] / 2) * 0.4
      offset_sign <- ifelse(small_connections$pair_index[i] %% 2 == 0, 1, -1)
      offset_factor <- offset_magnitude * offset_sign
    } else {
      offset_factor <- 0
    }

    route <- create_offset_route(
      from_city$long, from_city$lat,
      to_city$long, to_city$lat,
      offset_factor = offset_factor
    )

    # Use country-based color
    route_color <- small_country_color_map[from_city$country.etc]

    small_map <- add_arrow_decorator(
      small_map,
      route,
      color = route_color,
      weight = 2,
      opacity = 0.6,
      label = paste0(from_city$city_label, " → ", to_city$city_label,
                     " (from ", from_city$country.etc, ")")
    )
  }
  
  cat("Small network created:", n_cities, "cities,", nrow(small_connections), "connections\n")
  
  # Save
  saveWidget(small_map, "/mnt/user-data/outputs/random_world_network_small.html", 
             selfcontained = TRUE)
  
  return(small_map)
}

# Uncomment to create a smaller test network:
# small_network_map <- create_smaller_network(n_cities = 50, n_connections = 150)
# print(small_network_map)
