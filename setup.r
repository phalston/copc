# setup.r - Load necessary packages for COPC Experience Tracker
# Run this script before launching the Shiny app

# Define required packages
required_packages <- c(
  "shiny",      # Web application framework

"bslib",      # Bootstrap theming for Shiny
  "tidyverse",  # Data manipulation (dplyr, ggplot2, readr, etc.)
  "lubridate",  # Date/time manipulation
  "plotly"      # Interactive visualizations
)

# Function to install missing packages
install_if_missing <- function(packages) {
  missing <- packages[!packages %in% installed.packages()[, "Package"]]
  if (length(missing) > 0) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing, dependencies = TRUE)
  }
}

# Install any missing packages
install_if_missing(required_packages)

# Load all packages
invisible(lapply(required_packages, function(pkg) {
  library(pkg, character.only = TRUE)
  message("Loaded: ", pkg)
}))

message("\nAll packages loaded successfully!")
message("You can now run the app with: shiny::runApp('app.R')")
