# Interactive City Network Shiny App
# Allows filtering connections by country of origin
#
# To run: shiny::runApp("city_network_app.R")

library(shiny)
library(leaflet)
library(dplyr)
library(geosphere)
library(maps)
library(RColorBrewer)

# Set seed for reproducibility
set.seed(123)

# ============================================================================
# DATA PREPARATION (runs once when app starts)
# ============================================================================

cat("Loading data...\n")

# Load world cities
data(world.cities)

# Sample 200 cities
major_cities <- world.cities %>%
  filter(pop > 100000) %>%
  sample_n(200) %>%
  select(name, country.etc, lat, long, pop) %>%
  mutate(
    city_id = 1:n(),
    city_label = paste0(name, ", ", country.etc)
  )

# Create country color mapping
unique_countries <- unique(major_cities$country.etc)
n_countries <- length(unique_countries)

if (n_countries <= 12) {
  country_colors <- brewer.pal(max(3, n_countries), "Set3")[1:n_countries]
} else if (n_countries <= 24) {
  country_colors <- c(
    brewer.pal(12, "Set3"),
    brewer.pal(12, "Paired")
  )[1:n_countries]
} else {
  country_colors <- rainbow(n_countries, s = 0.7, v = 0.9)
}

country_color_map <- data.frame(
  country = unique_countries,
  color = country_colors,
  stringsAsFactors = FALSE
)

major_cities <- major_cities %>%
  left_join(country_color_map, by = c("country.etc" = "country"))

# Generate connections
n_connections <- 200

connections <- data.frame(
  from_id = sample(major_cities$city_id, n_connections, replace = TRUE),
  to_id = sample(major_cities$city_id, n_connections, replace = TRUE)
) %>%
  filter(from_id != to_id)

# Filter out wrap-around connections
connections_with_coords <- connections %>%
  left_join(major_cities %>% select(city_id, long, lat, country.etc, color),
            by = c("from_id" = "city_id")) %>%
  left_join(major_cities %>% select(city_id, long, lat),
            by = c("to_id" = "city_id"), suffix = c("_from", "_to"))

all_connections <- connections_with_coords %>%
  mutate(lon_diff = abs(long_to - long_from)) %>%
  filter(lon_diff <= 150)

# Track duplicates
all_connections <- all_connections %>%
  group_by(from_id, to_id) %>%
  mutate(
    connection_count = n(),
    connection_index = row_number()
  ) %>%
  ungroup()

cat("Data loaded:", nrow(major_cities), "cities,", nrow(all_connections), "connections\n")

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

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

offset_route_smooth <- function(route_coords, offset_degrees = 0) {
  if (offset_degrees == 0 || nrow(route_coords) < 3) {
    return(route_coords)
  }

  n_points <- nrow(route_coords)
  offset_route <- route_coords
  mid_idx <- round(n_points / 2)

  if (mid_idx > 1 && mid_idx < n_points) {
    idx_before <- max(1, mid_idx - 10)
    idx_after <- min(n_points, mid_idx + 10)

    dx <- route_coords[idx_after, 1] - route_coords[idx_before, 1]
    dy <- route_coords[idx_after, 2] - route_coords[idx_before, 2]

    length <- sqrt(dx^2 + dy^2)
    if (length > 0) {
      perp_x <- -dy / length
      perp_y <- dx / length

      for (i in 2:(n_points - 1)) {
        t <- (i - 1) / (n_points - 1)
        weight <- sin(t * pi)

        offset_route[i, 1] <- route_coords[i, 1] + perp_x * offset_degrees * weight
        offset_route[i, 2] <- route_coords[i, 2] + perp_y * offset_degrees * weight
      }
    }
  }

  return(offset_route)
}

add_arrow_decorator <- function(map, route_coords, color = "#3498db",
                                weight = 2, opacity = 0.6, label = NULL) {
  map <- map %>%
    addPolylines(
      lng = route_coords[, 1],
      lat = route_coords[, 2],
      color = color,
      weight = weight,
      opacity = opacity,
      label = label
    )

  n_points <- nrow(route_coords)
  arrow_pos <- round(n_points * 0.85)

  if (arrow_pos < n_points - 5 && arrow_pos > 0) {
    p1 <- route_coords[arrow_pos, ]
    p2 <- route_coords[min(arrow_pos + 5, n_points), ]

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

# ============================================================================
# SHINY UI
# ============================================================================

ui <- fluidPage(
  titlePanel("Interactive World City Network"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("Filter by Origin Country"),

      selectInput(
        "countries",
        "Select Countries:",
        choices = sort(unique_countries),
        selected = NULL,
        multiple = TRUE,
        selectize = TRUE
      ),

      checkboxInput(
        "show_all",
        "Show All Countries",
        value = TRUE
      ),

      hr(),

      actionButton(
        "select_top10",
        "Select Top 10 Countries",
        class = "btn-primary btn-sm"
      ),

      actionButton(
        "clear_selection",
        "Clear Selection",
        class = "btn-secondary btn-sm"
      ),

      hr(),

      h4("Statistics"),
      verbatimTextOutput("stats"),

      hr(),

      h4("Country Legend"),
      htmlOutput("country_legend")
    ),

    mainPanel(
      width = 9,
      leafletOutput("map", height = "800px")
    )
  )
)

# ============================================================================
# SHINY SERVER
# ============================================================================

server <- function(input, output, session) {

  # Reactive: Get filtered connections based on selected countries
  filtered_connections <- reactive({
    if (input$show_all) {
      return(all_connections)
    }

    if (is.null(input$countries) || length(input$countries) == 0) {
      return(data.frame())  # Empty dataframe if no countries selected
    }

    all_connections %>%
      filter(country.etc %in% input$countries)
  })

  # Reactive: Calculate statistics
  connection_stats <- reactive({
    conns <- filtered_connections()

    if (nrow(conns) == 0) {
      return(list(
        total = 0,
        countries = 0,
        avg_per_country = 0
      ))
    }

    list(
      total = nrow(conns),
      countries = length(unique(conns$country.etc)),
      avg_per_country = round(nrow(conns) / length(unique(conns$country.etc)), 1)
    )
  })

  # Observer: Update show_all checkbox when countries are selected/deselected
  observeEvent(input$countries, {
    if (!is.null(input$countries) && length(input$countries) > 0) {
      updateCheckboxInput(session, "show_all", value = FALSE)
    }
  })

  observeEvent(input$show_all, {
    if (input$show_all) {
      updateSelectInput(session, "countries", selected = character(0))
    }
  })

  # Observer: Select top 10 countries button
  observeEvent(input$select_top10, {
    country_counts <- all_connections %>%
      group_by(country.etc) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      head(10)

    updateSelectInput(session, "countries", selected = country_counts$country.etc)
    updateCheckboxInput(session, "show_all", value = FALSE)
  })

  # Observer: Clear selection button
  observeEvent(input$clear_selection, {
    updateSelectInput(session, "countries", selected = character(0))
    updateCheckboxInput(session, "show_all", value = TRUE)
  })

  # Output: Main map
  output$map <- renderLeaflet({
    # Base map (static)
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
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
  })

  # Observer: Update connections when filter changes
  observe({
    conns <- filtered_connections()

    # Create a new map proxy to clear old connections
    map_proxy <- leafletProxy("map") %>%
      clearShapes()

    if (nrow(conns) == 0) {
      return(map_proxy)
    }

    # Add filtered connections
    for (i in 1:nrow(conns)) {
      from_city <- major_cities[major_cities$city_id == conns$from_id[i], ]
      to_city <- major_cities[major_cities$city_id == conns$to_id[i], ]

      if (nrow(from_city) > 0 && nrow(to_city) > 0) {
        adjusted <- adjust_for_dateline(
          from_city$long, from_city$lat,
          to_city$long, to_city$lat
        )

        route <- gcIntermediate(
          c(adjusted$lon1, adjusted$lat1),
          c(adjusted$lon2, adjusted$lat2),
          n = 100,
          addStartEnd = TRUE,
          sp = FALSE
        )

        route[, 1] <- ifelse(route[, 1] > 180, route[, 1] - 360, route[, 1])
        route[, 1] <- ifelse(route[, 1] < -180, route[, 1] + 360, route[, 1])

        if (conns$connection_count[i] > 1) {
          offset_multiplier <- ceiling(conns$connection_index[i] / 2)
          offset_direction <- ifelse(conns$connection_index[i] %% 2 == 1, 1, -1)
          offset_degrees <- offset_direction * offset_multiplier * 0.8
          route <- offset_route_smooth(route, offset_degrees)
        }

        route_color <- conns$color[i]

        map_proxy <- add_arrow_decorator(
          map_proxy,
          route,
          color = route_color,
          weight = 1.5,
          opacity = 0.5,
          label = paste(from_city$city_label, "→", to_city$city_label)
        )
      }
    }
  })

  # Output: Statistics
  output$stats <- renderText({
    stats <- connection_stats()
    paste0(
      "Connections: ", stats$total, "\n",
      "Countries: ", stats$countries, "\n",
      "Avg per country: ", stats$avg_per_country
    )
  })

  # Output: Country legend
  output$country_legend <- renderUI({
    country_counts <- all_connections %>%
      group_by(country.etc, color) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count))

    legend_items <- lapply(1:min(15, nrow(country_counts)), function(i) {
      tags$div(
        style = "margin-bottom: 5px;",
        tags$span(
          style = paste0("display: inline-block; width: 20px; height: 12px; ",
                        "background-color: ", country_counts$color[i], "; ",
                        "margin-right: 8px; border: 1px solid #333;")
        ),
        tags$span(
          style = "font-size: 12px;",
          paste0(country_counts$country.etc[i], " (", country_counts$count[i], ")")
        )
      )
    })

    tags$div(
      style = "max-height: 400px; overflow-y: auto;",
      legend_items,
      if (nrow(country_counts) > 15) {
        tags$div(
          style = "font-size: 11px; color: #666; margin-top: 10px;",
          paste0("... and ", nrow(country_counts) - 15, " more countries")
        )
      }
    )
  })
}

# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui = ui, server = server)
