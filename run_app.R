# Simple launcher for the Interactive City Network Shiny App
#
# Usage: source("run_app.R") or Rscript run_app.R

# Check and install required packages
required_packages <- c("shiny", "leaflet", "dplyr", "geosphere", "maps", "RColorBrewer")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages)
}

# Run the app
cat("\n==============================================\n")
cat("Starting Interactive City Network App...\n")
cat("==============================================\n\n")
cat("The app will open in your web browser.\n")
cat("Press Ctrl+C or Esc to stop the app.\n\n")

shiny::runApp("city_network_app.R", launch.browser = TRUE)
