# Random World City Network with 200 Cities and 200 Connections
# Required packages: leaflet, dplyr, geosphere, maps, RColorBrewer

# Install packages if needed
# install.packages(c("leaflet", "dplyr", "geosphere", "maps", "RColorBrewer", "htmlwidgets"))

library(leaflet)
library(dplyr)
library(geosphere)
library(maps)

# Set seed for reproducibility
set.seed(123)

# Load world cities data
data(world.cities)

# Filter to get reasonably sized cities and clean data
world.cities <- world.cities %>%
  filter(pop > 100000) %>%  # Cities with population > 100k
  filter(!is.na(lat) & !is.na(long)) %>%
  select(name, country.etc, lat, long, pop)

# Randomly sample 200 cities
selected_cities <- world.cities %>%
  sample_n(200) %>%
  mutate(city_id = row_number(),
         label = paste0(name, ", ", country.etc))

# Step 2: Generate 200 random directed connections
# =================================================

# Generate 600 random directed connections (with replacement)
n_connections <- 600
connections <- data.frame(
  from_id = sample(major_cities$city_id, 200, replace = TRUE),
  to_id = sample(major_cities$city_id, 200, replace = TRUE)
) %>%
  filter(from_id != to_id)  # Remove self-loops

cat("Generated", nrow(connections), "connections\n")

# Count duplicate connections between same city pairs
connections <- connections %>%
  group_by(from_id, to_id) %>%
  mutate(
    connection_count = n(),
    connection_index = row_number()  # Which duplicate this is (1st, 2nd, 3rd, etc.)
  ) %>%
  ungroup()

cat("Number of duplicate routes:", sum(connections$connection_count > 1), "\n")

# Function to check if route crosses dateline and adjust if needed
adjust_for_dateline <- function(lon1, lat1, lon2, lat2) {
  # If longitude difference is > 180, we're crossing the dateline
  # Adjust coordinates to go the other way around
  if (abs(lon2 - lon1) > 180) {
    if (lon1 < lon2) {
      lon1 <- lon1 + 360
    } else {
      lon2 <- lon2 + 360
    }
  }
  return(list(lon1 = lon1, lat1 = lat1, lon2 = lon2, lat2 = lat2))
}

# Function to create smooth offset for routes (avoiding kinks)
offset_route_smooth <- function(route_coords, offset_degrees = 0) {
  if (offset_degrees == 0 || nrow(route_coords) < 3) {
    return(route_coords)
  }
  
  n_points <- nrow(route_coords)
  offset_route <- route_coords
  
  # Calculate the midpoint of the route
  mid_idx <- round(n_points / 2)
  
  # Calculate the perpendicular direction at midpoint
  if (mid_idx > 1 && mid_idx < n_points) {
    # Use points around midpoint for direction
    idx_before <- max(1, mid_idx - 10)
    idx_after <- min(n_points, mid_idx + 10)
    
    dx <- route_coords[idx_after, 1] - route_coords[idx_before, 1]
    dy <- route_coords[idx_after, 2] - route_coords[idx_before, 2]
    
    # Calculate perpendicular direction
    length <- sqrt(dx^2 + dy^2)
    if (length > 0) {
      perp_x <- -dy / length
      perp_y <- dx / length
      
      # Apply smooth offset using a bell curve
      for (i in 2:(n_points - 1)) {
        # Calculate distance from endpoints (0 to 1, peaking at 0.5)
        t <- (i - 1) / (n_points - 1)
        # Use smooth curve: peaks in middle, tapers to 0 at ends
        weight <- sin(t * pi)  # Smooth curve from 0 to 1 to 0
        
        # Apply weighted offset
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
  
  # Calculate arrow position (near the end of the line)
  n_points <- nrow(route_coords)
  arrow_pos <- round(n_points * 0.85)
  
  if (arrow_pos < n_points - 5 && arrow_pos > 0) {
    # Get direction vector for arrow
    p1 <- route_coords[arrow_pos, ]
    p2 <- route_coords[min(arrow_pos + 5, n_points), ]
    
    # Calculate arrow head points
    dx <- p2[1] - p1[1]
    dy <- p2[2] - p1[2]
    angle <- atan2(dy, dx)
    
    # Arrow parameters
    arrow_length <- 1.5
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
        weight = weight + 1,
        opacity = opacity
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

# Add city markers (smaller to avoid clutter)
map <- map %>%
  addCircleMarkers(
    data = selected_cities,
    lng = ~long,
    lat = ~lat,
    radius = 3,
    color = "#e74c3c",
    fillColor = "#e74c3c",
    fillOpacity = 0.7,
    stroke = FALSE,
    popup = ~label
  )

# Add connections with offset for duplicates
cat("Adding connections...\n")
pb <- txtProgressBar(min = 0, max = nrow(connections), style = 3)

for (i in 1:nrow(connections)) {
  setTxtProgressBar(pb, i)
  
  from_city <- selected_cities[selected_cities$city_id == connections$from_id[i], ]
  to_city <- selected_cities[selected_cities$city_id == connections$to_id[i], ]
  
  if (nrow(from_city) > 0 && nrow(to_city) > 0) {
    # Adjust coordinates for dateline crossing
    adjusted <- adjust_for_dateline(
      from_city$long, from_city$lat,
      to_city$long, to_city$lat
    )
    
    # Calculate great circle route with adjusted coordinates
    route <- gcIntermediate(
      c(adjusted$lon1, adjusted$lat1),
      c(adjusted$lon2, adjusted$lat2),
      n = 100,
      addStartEnd = TRUE,
      sp = FALSE
    )
    
    # Normalize longitudes back to -180 to 180 range
    route[, 1] <- ifelse(route[, 1] > 180, route[, 1] - 360, route[, 1])
    route[, 1] <- ifelse(route[, 1] < -180, route[, 1] + 360, route[, 1])
    
    # Apply smooth offset if this is a duplicate route
    if (connections$connection_count[i] > 1) {
      # Calculate offset based on which duplicate this is
      offset_multiplier <- ceiling(connections$connection_index[i] / 2)
      offset_direction <- ifelse(connections$connection_index[i] %% 2 == 1, 1, -1)
      offset_degrees <- offset_direction * offset_multiplier * 0.8
      
      route <- offset_route_smooth(route, offset_degrees)
    }
    
    # Vary colors slightly for visual interest
    color_options <- c("#3498db", "#2ecc71", "#9b59b6", "#f39c12", "#1abc9c")
    route_color <- sample(color_options, 1)
    
    # Add route with arrow
    map <- add_arrow_decorator(
      map,
      route,
      color = route_color,
      weight = 1.5,
      opacity = 0.4,
      label = paste(from_city$label, "→", to_city$label)
    )
  }
}

close(pb)

cat("\nMap created successfully!\n")
print(map)

# Optional: Save the map
# library(htmlwidgets)
# saveWidget(map, "random_world_network.html", selfcontained = TRUE)


# ============================================================================
# ALTERNATIVE VERSION: Cleaner visualization with connection statistics
# ============================================================================

cat("\n\nCreating alternative version with statistics...\n")

# Calculate connection statistics
city_connections <- connections %>%
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
selected_cities <- selected_cities %>%
  left_join(city_connections, by = c("city_id" = "from_id")) %>%
  mutate(
    total = ifelse(is.na(total), 0, total),
    incoming = ifelse(is.na(incoming), 0, incoming),
    outgoing = ifelse(is.na(outgoing), 0, outgoing)
  )

# Create map with sized markers based on connectivity
map2 <- leaflet() %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView(lng = 0, lat = 20, zoom = 2)

# Add city markers with size based on number of connections
map2 <- map2 %>%
  addCircleMarkers(
    data = selected_cities,
    lng = ~long,
    lat = ~lat,
    radius = ~sqrt(total) + 2,  # Size based on total connections
    color = "#f39c12",
    fillColor = "#f39c12",
    fillOpacity = 0.6,
    stroke = TRUE,
    weight = 1,
    popup = ~paste0(
      "<b>", label, "</b><br>",
      "Outgoing: ", outgoing, "<br>",
      "Incoming: ", incoming, "<br>",
      "Total: ", total
    )
  )

# Add connections (sample subset for cleaner visualization)
sample_connections <- connections %>%
  sample_n(min(300, nrow(connections)))  # Show 300 connections for cleaner view

for (i in 1:nrow(sample_connections)) {
  from_city <- selected_cities[selected_cities$city_id == sample_connections$from_id[i], ]
  to_city <- selected_cities[selected_cities$city_id == sample_connections$to_id[i], ]
  
  if (nrow(from_city) > 0 && nrow(to_city) > 0) {
    # Adjust coordinates for dateline crossing
    adjusted <- adjust_for_dateline(
      from_city$long, from_city$lat,
      to_city$long, to_city$lat
    )
    
    # Calculate great circle route with adjusted coordinates
    route <- gcIntermediate(
      c(adjusted$lon1, adjusted$lat1),
      c(adjusted$lon2, adjusted$lat2),
      n = 100,
      addStartEnd = TRUE,
      sp = FALSE
    )
    
    # Normalize longitudes back to -180 to 180 range
    route[, 1] <- ifelse(route[, 1] > 180, route[, 1] - 360, route[, 1])
    route[, 1] <- ifelse(route[, 1] < -180, route[, 1] + 360, route[, 1])
    
    # Apply smooth offset for duplicates
    if (sample_connections$connection_count[i] > 1) {
      offset_multiplier <- ceiling(sample_connections$connection_index[i] / 2)
      offset_direction <- ifelse(sample_connections$connection_index[i] %% 2 == 1, 1, -1)
      offset_degrees <- offset_direction * offset_multiplier * 0.8
      route <- offset_route_smooth(route, offset_degrees)
    }
    
    map2 <- add_arrow_decorator(
      map2,
      route,
      color = "#3498db",
      weight = 1,
      opacity = 0.3
    )
  }
}

cat("Alternative map created!\n")
print(map2)


# ============================================================================
# PRINT SUMMARY STATISTICS
# ============================================================================

cat("\n=== Network Statistics ===\n")
cat("Total cities:", nrow(selected_cities), "\n")
cat("Total connections:", nrow(connections), "\n")
cat("Average connections per city:", round(mean(selected_cities$total), 2), "\n")
cat("Most connected city:", 
    selected_cities$label[which.max(selected_cities$total)],
    "with", max(selected_cities$total), "connections\n")
cat("Cities with no connections:", 
    sum(selected_cities$total == 0), "\n")

# Find city pairs with most connections
duplicate_pairs <- connections %>%
  group_by(from_id, to_id) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(count > 1) %>%
  arrange(desc(count))

if (nrow(duplicate_pairs) > 0) {
  cat("\nTop city pairs with multiple connections:\n")
  for (i in 1:min(5, nrow(duplicate_pairs))) {
    from <- selected_cities$label[selected_cities$city_id == duplicate_pairs$from_id[i]]
    to <- selected_cities$label[selected_cities$city_id == duplicate_pairs$to_id[i]]
    cat(sprintf("  %s → %s: %d connections\n", from, to, duplicate_pairs$count[i]))
  }
}