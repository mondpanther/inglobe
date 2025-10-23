# Random World City Network Visualization

Interactive visualization of random connections between 200 major world cities.

## Files

- **`random_city_network.R`** - Static visualization script
- **`city_network_app.R`** - Interactive Shiny app with country filtering
- **`run_app.R`** - Simple launcher for the Shiny app

## Required Packages

```r
install.packages(c("leaflet", "dplyr", "geosphere", "maps", "htmlwidgets", "RColorBrewer", "shiny"))
```

## Usage

### Option 1: Interactive Shiny App (Recommended)

The Shiny app allows you to filter connections by country interactively.

**Quick Start:**
```r
source("run_app.R")
```

**Or manually:**
```r
shiny::runApp("city_network_app.R")
```

**Features:**
- Multi-select country filter
- "Show All Countries" toggle
- "Select Top 10 Countries" quick filter
- Real-time statistics
- Color-coded country legend
- Interactive Leaflet map

### Option 2: Static Visualization

Run the static script to generate a non-interactive visualization:

```r
source("random_city_network.R")
```

## Features

### Data Generation
- Samples 200 random cities from world cities database (population > 100,000)
- Generates up to 200 random directed connections
- Filters out connections that would wrap around the back of the map (>150° longitude difference)
- Color-codes connections by origin country

### Visualization
- Great circle routes between cities
- Smooth offsets for duplicate connections between same city pairs
- Directional arrows showing connection direction
- Dark theme Leaflet map
- City markers with population popups

### Statistics
- Network statistics (total connections, cities, etc.)
- Top connected cities
- Duplicate connection analysis
- Country-based connection statistics

## How It Works

1. **City Selection**: Randomly samples 200 cities with population > 100,000
2. **Color Mapping**: Assigns unique colors to each country using RColorBrewer palettes
3. **Connection Generation**: Creates random directed connections between cities
4. **Wrap-around Filtering**: Removes connections that span >150° longitude to prevent visual artifacts
5. **Visualization**: Draws great circle routes with country-based colors and directional arrows

## Interactive App Controls

- **Select Countries**: Choose one or multiple countries from the dropdown
- **Show All Countries**: Toggle to display all connections
- **Select Top 10 Countries**: Quick filter for countries with most connections
- **Clear Selection**: Reset filters and show all connections

## Customization

### Change Number of Cities/Connections

Edit the values in the script:
```r
# In random_city_network.R or city_network_app.R
sample_n(200)  # Change to desired number of cities
n_connections <- 200  # Change to desired number of connections
```

### Change Wrap-around Threshold

Edit the filter threshold:
```r
filter(lon_diff <= 150)  # Change 150 to desired degree threshold
```

### Modify Colors

The app uses RColorBrewer palettes. You can customize in the color mapping section:
```r
country_colors <- brewer.pal(12, "Set3")  # Try "Paired", "Dark2", etc.
```

## Notes

- Set seed (123) is used for reproducibility - change or remove for different random networks
- Connections that wrap around the map are automatically filtered to prevent visual artifacts
- The same random seed is used in both scripts to ensure consistent results
