###############################################################
# SHINY APP — Curves + Arrows + Chain Colors + Grouped Selector
###############################################################

library(shiny)
library(readxl)
library(dplyr)
library(leaflet)
library(countrycode)
library(shinyBS)   # For collapsible "About this tool" panel

options(jsonlite_legacy_as_json = TRUE)

###############################################################
# 1️⃣ LOAD & PREPARE DATA
###############################################################

library(readr)
library(dplyr)

df_raw <- read_csv(
  "C:/Users/aforest/OneDrive - WBG/Desktop/Research/Open Alex/DataLab/R_things/long_308scenarios.csv"
)


df_raw <- df_raw %>% mutate(wave = as.integer(wave))

df <- df_raw %>%
  arrange(sce_country, sce_tech, source_id, wave) %>%
  mutate(chain_id = paste0(sce_country, "_", sce_tech, "_", sample_size, "_", source_id))

###############################################################
# 2️⃣ COUNTRY GROUP DEFINITIONS (REGION GROUPS — SAME STRUCTURE)
###############################################################

# All ISO2 countries
all_countries <- sort(unique(na.omit(countrycode::codelist$iso2c)))

# Define regional groups (same structure as LMIC/EU/HIC lists)
north_america <- c("US","CA","MX")
south_america <- c("BR","AR","CL","CO","PE","UY","EC","VE")
western_europe <- c("FR","GB","DE","NL","BE","CH","AT","IE","LU")
northern_europe <- c("SE","NO","FI","DK","IS")
southern_europe <- c("IT","ES","PT","GR","MT","CY")
eastern_europe <- c("PL","CZ","HU","SK","SI","HR","RO","BG",
                    "RS","BA","MK","AL","EE","LV","LT","UA","BY","MD")
middle_east <- c("TR","IL","SA","AE","QA","KW","OM","BH","JO","IR","IQ","LB")
africa <- c("ZA","EG","NG","KE","GH","SN","CI","MA","TN","DZ","ET","TZ","UG","ZM","MZ")
central_asia <- c("KZ","UZ","KG","TJ","TM")
south_asia <- c("IN","PK","BD","LK","NP")
east_asia <- c("CN","JP","KR","MN")
south_east_asia <- c("SG","MY","TH","VN","ID","PH","KH","LA","MM","BN")
oceania <- c("AU","NZ","FJ")

# EXACT SAME STRUCTURE AS BEFORE
group_definitions <- list(
  "All countries"     = all_countries,
  "North America"     = north_america,
  "South America"     = south_america,
  "Western Europe"    = western_europe,
  "Northern Europe"   = northern_europe,
  "Southern Europe"   = southern_europe,
  "Eastern Europe"    = eastern_europe,
  "Middle East"       = middle_east,
  "Africa"            = africa,
  "Central Asia"      = central_asia,
  "South Asia"        = south_asia,
  "East Asia"         = east_asia,
  "South East Asia"   = south_east_asia,
  "Oceania"           = oceania
)

# Countries in data
vals <- sort(unique(df$sce_country))
country_choices <- setNames(vals, vals)

# SAME STRUCTURE for choice grouping
grouped_choices <- list(
  "Predefined Groups"   = as.list(names(group_definitions)),
  "Individual Countries" = as.list(country_choices)
)

# SAME expansion logic
expand_country_selection <- function(selected) {
  unique(unlist(lapply(selected, function(x) {
    if (x %in% names(group_definitions)) group_definitions[[x]] else x
  })))
}

###############################################################
# 3️⃣ COLOR LOGIC
###############################################################

get_scenario_color <- function(tech) {
  tech <- tolower(tech)
  if (tech == "all")        return("blue")
  if (tech == "non-green")  return("brown")
  if (tech == "green")      return("green")
  return("black")
}

get_random_shade <- function(base_color) {
  
  base_hue <- switch(base_color,
                     "blue"  = 210,
                     "green" = 120,
                     "brown" = 30,
                     0)
  
  hue <- base_hue + runif(1, -8, 8)
  sat <- runif(1, 0.55, 0.75)
  lig <- runif(1, 0.40, 0.65)
  
  h <- (hue %% 360) / 360
  q <- if (lig < 0.5) lig * (1 + sat) else lig + sat - lig * sat
  p <- 2 * lig - q
  
  hue_to_rgb <- function(t) {
    t <- ifelse(t < 0, t + 1, ifelse(t > 1, t - 1, t))
    if (t < 1/6)      return(p + (q - p) * 6 * t)
    else if (t < 1/2) return(q)
    else if (t < 2/3) return(p + (q - p) * (2/3 - t) * 6)
    else              return(p)
  }
  
  r <- hue_to_rgb(h + 1/3)
  g <- hue_to_rgb(h)
  b <- hue_to_rgb(h - 1/3)
  
  sprintf("#%02X%02X%02X", round(r*255), round(g*255), round(b*255))
}

###############################################################
# 4️⃣ CURVE FUNCTION
###############################################################

make_curve <- function(sx, sy, tx, ty, n = 40, bend = 0.3) {
  if (sx == tx && sy == ty)
    return(data.frame(lon = c(sx, tx), lat = c(sy, ty)))
  
  mx <- (sx + tx) / 2
  my <- (sy + ty) / 2
  
  dx <- tx - sx
  dy <- ty - sy
  d  <- sqrt(dx^2 + dy^2)
  if (d == 0) d <- 1
  
  px <- -dy / d
  py <-  dx / d
  
  h <- bend * d
  
  cx <- mx + px * h
  cy <- my + py * h
  
  t <- seq(0, 1, length.out = n)
  
  lon <- (1 - t)^2 * sx + 2 * (1 - t) * t * cx + t^2 * tx
  lat <- (1 - t)^2 * sy + 2 * (1 - t) * t * cy + t^2 * ty
  
  data.frame(lon = lon, lat = lat)
}

###############################################################
# 5️⃣ UI — NEW LAYOUT
###############################################################

ui <- fluidPage(
  
  titlePanel("Patent Propagation Mapping Tool"),
  
  # ABOUT SECTION
  bsCollapse(
    bsCollapsePanel(
      "About this tool", style = "info",
      p("This tool visualizes propagation chains between innovators..."),
      p("It allows selecting country groups, technologies, waves, and sample sizes.")
    )
  ),
  br(),
  
  # TOP FILTERS (above map)
  fluidRow(
    column(6,
           selectInput(
             "sce_country", "Country or Group:",
             choices = grouped_choices, multiple = TRUE,
             selected = "All countries"
           )
    ),
    column(6,
           selectInput("sce_tech", "Technology:",
                       choices = sort(unique(df$sce_tech)),
                       selected = unique(df$sce_tech)[1])
    )
  ),
  
  br(),
  
  # MAP
  leafletOutput("map", height = 650),
  
  br(),
  
  # BOTTOM FILTERS (below map)
  fluidRow(
    column(6,
           sliderInput(
             "max_wave", "Max Wave",
             min = min(df$wave), max = max(df$wave), value = 1,
             step = 1  # INTEGER STEPS ONLY
           )
    ),
    column(6,
           selectInput("sample_size", "Sample Size:",
                       choices = sort(unique(df$sample_size)),
                       selected = sort(unique(df$sample_size))[1])
    )
  )
)

###############################################################
# 6️⃣ SERVER
###############################################################

server <- function(input, output, session) {
  
  edges_filtered <- reactive({
    selected_countries <- expand_country_selection(input$sce_country)
    
    df %>%
      filter(
        sce_country %in% selected_countries,
        sce_tech == input$sce_tech,
        wave <= input$max_wave,
        sample_size == input$sample_size
      )
  })
  
  nodes_filtered <- reactive({
    edges <- edges_filtered()
    
    bind_rows(
      edges %>% select(lon = source_lon, lat = source_lat),
      edges %>% select(lon = target_lon, lat = target_lat)
    ) %>% distinct(lon, lat)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      setView(10, 20, zoom = 3)
  })
  
  observe({
    
    edges <- edges_filtered()
    nodes <- nodes_filtered()
    base_color <- get_scenario_color(input$sce_tech)
    
    m <- leafletProxy("map") %>% clearShapes() %>% clearMarkers()
    
    # NODES
    m <- m %>% addCircleMarkers(
      data = nodes,
      lng = ~lon, lat = ~lat,
      radius = 5,
      fillColor = "white", fillOpacity = 1,
      color = "black", weight = 1
    )
    
    # ONE SHADE PER CHAIN
    chain_colors <- edges %>%
      distinct(chain_id) %>%
      mutate(color = sapply(1:n(), function(i) get_random_shade(base_color)))
    
    # DRAW CURVES + ARROWS
    for (i in seq_len(nrow(edges))) {
      
      chain <- edges$chain_id[i]
      this_color <- chain_colors$color[chain_colors$chain_id == chain]
      
      sx <- edges$source_lon[i]
      sy <- edges$source_lat[i]
      tx <- edges$target_lon[i]
      ty <- edges$target_lat[i]
      
      curve_pts <- make_curve(sx, sy, tx, ty)
      n_pts <- nrow(curve_pts)
      
      # CURVE
      m <- m %>% addPolylines(
        lng = curve_pts$lon,
        lat = curve_pts$lat,
        color = this_color,
        weight = 2.5,
        opacity = 1
      )
      
      # ARROWHEAD IN MIDDLE
      if (n_pts >= 7) {
        
        mid <- round(n_pts / 2)
        p1 <- curve_pts[mid - 3, ]
        p2 <- curve_pts[mid + 3, ]
        
        dx <- p2$lon - p1$lon
        dy <- p2$lat - p1$lat
        seg_len <- sqrt(dx^2 + dy^2)
        if (seg_len == 0) next
        
        angle <- atan2(dy, dx)
        
        arrow_len <- min(max(seg_len * 0.45, 0.45), 1.5)
        arrow_angle <- 28 * pi / 180
        
        left_lon  <- p2$lon - arrow_len * cos(angle - arrow_angle)
        left_lat  <- p2$lat - arrow_len * sin(angle - arrow_angle)
        right_lon <- p2$lon - arrow_len * cos(angle + arrow_angle)
        right_lat <- p2$lat - arrow_len * sin(angle + arrow_angle)
        
        m <- m %>% addPolylines(
          lng = c(left_lon, p2$lon, right_lon),
          lat = c(left_lat, p2$lat, right_lat),
          color = this_color,
          weight = 2,
          opacity = 1
        )
      }
    }
  })
}

###############################################################
# 7️⃣ RUN APP
###############################################################

shinyApp(ui, server)
