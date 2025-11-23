# Setup script for Experience Tracker App
# Run this script to install all required packages

# List of required packages
packages <- c(
  "shiny",
  "bslib",
  "tidyverse",
  "lubridate",
  "plotly"
)

# Function to install packages if not already installed
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing", pkg, "..."))
    install.packages(pkg, repos = "https://cloud.r-project.org")
  } else {
    message(paste(pkg, "is already installed"))
  }
}

# Install all packages
message("Setting up Experience Tracker App...\n")
invisible(lapply(packages, install_if_missing))

message("\nSetup complete! Run the app with: shiny::runApp()")
