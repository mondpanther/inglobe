# app.R
library(shiny)
library(leaflet)
library(dplyr)
library(arrow)

# Load data from Parquet file
df_long <- read_parquet("C:/Users/aforest/OneDrive - WBG/Desktop/Research/Open Alex/DataLab/citation_links_long_NEW_expanded_sampled.parquet")

# Debug: Check what's in the data
print(paste("Total rows loaded:", nrow(df_long)))
print("Sample size column unique values:")
print(unique(df_long$sample_size))

# Clean data
df_long <- df_long %>%
  mutate(
    country = as.character(country),
    technology = as.character(technology),
    sample_size = as.numeric(sample_size),  # Ensure it's numeric
    tech_group = as.character(tech_group),
    # Determine if technology is green-related based on tech_group column
    is_green = ifelse(tech_group == "Green", TRUE, FALSE),
    # Ensure group columns are logical
    is_oecd = as.logical(is_oecd),
    is_g7 = as.logical(is_g7),
    is_brics = as.logical(is_brics)
  ) %>%
  filter(!is.na(from_lat) & !is.na(from_lon) & !is.na(to_lat) & !is.na(to_lon))

print("After cleaning - Sample size unique values:")
print(unique(df_long$sample_size))

countries <- sort(unique(df_long$country))
technologies <- sort(unique(df_long$technology))
# Get ONLY the unique values from the sample_size column, nothing else
sample_sizes <- sort(unique(df_long$sample_size))
# Remove any NA values
sample_sizes <- sample_sizes[!is.na(sample_sizes)]
# Print to console for debugging
print("Available sample sizes in data:")
print(sample_sizes)

# Function to create curved path between two points
create_curved_path <- function(from_lon, from_lat, to_lon, to_lat, n_points = 50) {
  # Calculate the control point for the bezier curve
  mid_lon <- (from_lon + to_lon) / 2
  mid_lat <- (from_lat + to_lat) / 2
  
  # Calculate perpendicular offset for the curve
  dx <- to_lon - from_lon
  dy <- to_lat - from_lat
  dist <- sqrt(dx^2 + dy^2)
  
  # Offset proportional to distance (creates nice arcs)
  offset <- dist * 0.15
  
  # Perpendicular direction
  perp_x <- -dy / dist * offset
  perp_y <- dx / dist * offset
  
  # Control point
  ctrl_lon <- mid_lon + perp_x
  ctrl_lat <- mid_lat + perp_y
  
  # Generate points along quadratic bezier curve
  t <- seq(0, 1, length.out = n_points)
  
  lons <- (1-t)^2 * from_lon + 2*(1-t)*t * ctrl_lon + t^2 * to_lon
  lats <- (1-t)^2 * from_lat + 2*(1-t)*t * ctrl_lat + t^2 * to_lat
  
  return(data.frame(lon = lons, lat = lats))
}

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .selectize-input {
        cursor: pointer !important;
      }
      body {
        background-color: #f5f5f5;
      }
      .well {
        background-color: white;
      }
    "))
  ),
  
  titlePanel("Patent Citation Chain Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Selection mode radio buttons
      radioButtons("selection_mode",
                   label = "Filter by:",
                   choices = c("Individual Country" = "country",
                               "Country Group" = "group"),
                   selected = "country"),
      
      # Conditional panel for individual country
      conditionalPanel(
        condition = "input.selection_mode == 'country'",
        selectInput("country", 
                    label = "Select Origin Country:",
                    choices = c("Choose..." = "", countries),
                    selected = NULL)
      ),
      
      # Conditional panel for country group
      conditionalPanel(
        condition = "input.selection_mode == 'group'",
        selectInput("country_group",
                    label = "Select Country Group:",
                    choices = c("Choose..." = "",
                                "OECD" = "is_oecd",
                                "G7" = "is_g7",
                                "BRICS" = "is_brics"),
                    selected = NULL)
      ),
      
      # Technology selection mode radio buttons
      radioButtons("tech_selection_mode",
                   label = "Technology Filter by:",
                   choices = c("Individual Technology" = "technology",
                               "Technology Group" = "tech_group"),
                   selected = "technology"),
      
      # Conditional panel for individual technology
      conditionalPanel(
        condition = "input.tech_selection_mode == 'technology'",
        selectInput("technology",
                    label = "Select Technology:",
                    choices = c("Choose..." = "", technologies),
                    selected = NULL)
      ),
      
      # Conditional panel for technology group
      conditionalPanel(
        condition = "input.tech_selection_mode == 'tech_group'",
        selectInput("tech_group",
                    label = "Select Technology Group:",
                    choices = c("Choose..." = "",
                                "Green" = "Green",
                                "Non-Green" = "Non-Green"),
                    selected = NULL)
      ),
      
      # Sample size selector
      selectInput("sample_size",
                  label = "Select Sample Size:",
                  choices = c("Choose..." = "", sample_sizes),
                  selected = NULL),
      
      # Chain length slider
      sliderInput("max_generation",
                  "Maximum Chain Length:",
                  min = 1,
                  max = 5,
                  value = 5,
                  step = 1),
      
      hr(),
      
      # Action button to update map
      actionButton("update_map", "Update Map", 
                   class = "btn-primary btn-block",
                   style = "margin-bottom: 15px;"),
      
      hr(),
      
      # Statistics
      h4("Summary Statistics"),
      verbatimTextOutput("stats"),
      
      hr(),
      
      # Legend
      h4("Color Legend"),
      p(strong("Green shades:"), "Green technology patents"),
      p(strong("Brown shades:"), "Non-green technology patents"),
      p("Darker = Earlier generation, Lighter = Later generation")
    ),
    
    mainPanel(
      width = 9,
      leafletOutput("map", height = "700px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Initial map render
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 0, lat = 30, zoom = 2) %>%
      addControl("Select filters and click 'Update Map' to visualize patent chains", 
                 position = "topright")
  })
  
  # Reactive filtered data
  filtered_data <- eventReactive(input$update_map, {
    
    # Validate inputs - require sample_size always
    req(input$sample_size)
    
    if (input$sample_size == "") {
      return(NULL)
    }
    
    # Start with base filters
    result <- df_long %>%
      filter(
        sample_size == as.numeric(input$sample_size),
        generation <= input$max_generation
      )
    
    # Apply technology or tech_group filter
    if (input$tech_selection_mode == "technology") {
      req(input$technology)
      if (input$technology == "") {
        return(NULL)
      }
      result <- result %>%
        filter(technology == input$technology)
    } else {  # tech_group mode
      req(input$tech_group)
      if (input$tech_group == "") {
        return(NULL)
      }
      result <- result %>%
        filter(tech_group == input$tech_group)
    }
    
    # Apply country or group filter
    if (input$selection_mode == "country") {
      req(input$country)
      if (input$country == "") {
        return(NULL)
      }
      
      # Get valid chain_ids that start from the selected country in generation 1
      valid_chains <- result %>%
        filter(generation == 1, from_ctry == input$country) %>%
        pull(chain_id) %>%
        unique()
      
    } else {  # group mode
      req(input$country_group)
      if (input$country_group == "") {
        return(NULL)
      }
      
      # Get countries that belong to the selected group
      group_countries <- df_long %>%
        filter(!!sym(input$country_group) == TRUE) %>%
        pull(country) %>%
        unique()
      
      # Get valid chain_ids that start from any country in the group in generation 1
      valid_chains <- result %>%
        filter(generation == 1, from_ctry %in% group_countries) %>%
        pull(chain_id) %>%
        unique()
    }
    
    # Filter to only include those valid chains
    result <- result %>%
      filter(chain_id %in% valid_chains)
    
    return(result)
  })
  
  # Update map when button is clicked
  observeEvent(input$update_map, {
    data <- filtered_data()
    
    if (is.null(data) || nrow(data) == 0) {
      leafletProxy("map") %>%
        clearShapes() %>%
        clearMarkers() %>%
        clearControls() %>%
        addControl("No data available for selected filters.", position = "topright")
      return()
    }
    
    # Green color palette (dark to light green for generations 1-5)
    green_colors <- c("#1a5f30", "#2d7a3f", "#4a9657", "#6bb374", "#8fcf91")
    
    # Brown color palette (dark to light brown for generations 1-5)
    brown_colors <- c("#5c3a1f", "#7d4e2d", "#9e6240", "#bf7a57", "#d99771")
    
    # Prepare node data (unique from and to points)
    nodes_from <- data %>%
      select(patent_id = from_id, country = from_ctry, technology = from_tech, 
             latitude = from_lat, longitude = from_lon, generation, is_green) %>%
      mutate(node_type = "from")
    
    nodes_to <- data %>%
      select(patent_id = to_id, country = to_ctry, technology = to_tech,
             latitude = to_lat, longitude = to_lon, generation, is_green) %>%
      mutate(node_type = "to")
    
    all_nodes <- bind_rows(nodes_from, nodes_to) %>%
      distinct(patent_id, .keep_all = TRUE)
    
    # Update map
    proxy <- leafletProxy("map") %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearControls()
    
    # Add curved lines for citation flows with arrows
    for (i in 1:nrow(data)) {
      # Generate curved path
      curve_points <- create_curved_path(
        data$from_lon[i], data$from_lat[i],
        data$to_lon[i], data$to_lat[i],
        n_points = 50
      )
      
      # Choose color based on whether target is green tech
      gen_idx <- min(data$generation[i], 5)  # Cap at 5
      line_color <- if(data$is_green[i]) {
        green_colors[gen_idx]
      } else {
        brown_colors[gen_idx]
      }
      
      # Add curved line
      proxy <- proxy %>%
        addPolylines(
          lng = curve_points$lon,
          lat = curve_points$lat,
          color = line_color,
          weight = 2,
          opacity = 0.6
        )
      
      # Add arrow at the end point
      # Calculate arrow head position (90% along the curve)
      arrow_idx <- round(nrow(curve_points) * 0.9)
      arrow_start_idx <- arrow_idx - 2
      
      # Simple arrow using a small marker
      proxy <- proxy %>%
        addCircleMarkers(
          lng = data$to_lon[i],
          lat = data$to_lat[i],
          radius = 3,
          color = line_color,
          fillColor = line_color,
          fillOpacity = 0.8,
          stroke = FALSE
        )
    }
    
    # Add points for source nodes (slightly larger)
    proxy <- proxy %>%
      addCircleMarkers(
        data = all_nodes,
        lng = ~longitude,
        lat = ~latitude,
        radius = ~ifelse(node_type == "from", 6, 4),
        fillColor = ~ifelse(is_green, 
                            green_colors[min(generation, 5)], 
                            brown_colors[min(generation, 5)]),
        fillOpacity = 0.8,
        stroke = TRUE,
        weight = 1,
        color = "white",
        popup = ~paste0(
          "<b>Generation:</b> ", generation, "<br>",
          "<b>Country:</b> ", country, "<br>",
          "<b>Green Technology:</b> ", ifelse(is_green, "Yes", "No"), "<br>",
          "<b>Technology:</b> ", technology, "<br>",
          "<b>Patent ID:</b> ", patent_id
        )
      )
  })
  
  # Summary statistics
  output$stats <- renderText({
    
    if (input$update_map == 0) {
      return("Select filters and click 'Update Map'")
    }
    
    data <- filtered_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return("No data available for selected filters.")
    }
    
    n_chains <- length(unique(data$chain_id))
    n_edges <- nrow(data)
    n_countries_total <- length(unique(c(data$from_ctry, data$to_ctry)))
    n_green <- sum(data$is_green)
    pct_green <- round(n_green / n_edges * 100, 1)
    
    paste0(
      "Chains: ", n_chains, "\n",
      "Citation links: ", n_edges, "\n",
      "Countries: ", n_countries_total, "\n",
      "Green tech citations: ", n_green, " (", pct_green, "%)\n", 
      "Generations: ", min(data$generation), " to ", max(data$generation)
    )
  })
}

# Run app
shinyApp(ui = ui, server = server)