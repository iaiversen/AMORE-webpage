# Last updated: 07.10.2025
# ==============================================================================================
# AMORE Project Setup Script
# This script installs all necessary components for the AMORE website
# ==============================================================================================

# ----------------------------------------------------------------------------------------------
# Prerequisites Installation Instructions
# ----------------------------------------------------------------------------------------------
#
# 1. R (version 4.0.0 or higher) from https://cran.r-project.org/
#    - Required for all R packages
#
# 2. RStudio (recommended) from https://posit.co/download/rstudio-desktop/
#    - Recommended IDE for R and Quarto
# 
# 3. Installing packages is global in RStudio, but loading libraries is specific to each script. 

(sass = TRUE)

# ----------------------------------------------------------------------------------------------
# Individualized functions for AMORE 
# ----------------------------------------------------------------------------------------------

# Function to install and load R packages
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

# Check, install, and update TinyTeX with major version handling
check_and_update_tinytex <- function() {
  if (!tinytex::is_tinytex()) {
    message("TinyTeX not found. Installing TinyTeX...")
    tinytex::install_tinytex()
    message("TinyTeX installed successfully")
  } else {
    message("TinyTeX is already installed")
    message("Checking for updates...")
    
    # Try to update packages and capture output
    tryCatch({
      # Capture the console output
      update_output <- capture.output({
        tinytex::tlmgr_update(all = TRUE, self = TRUE)
      }, type = "message")
      
      # Combine all output lines
      update_text <- paste(update_output, collapse = "\n")
      
      # Check if major version upgrade is needed
      if (grepl("A new version of TeX Live has been released", update_text)) {
        message("\n*** MAJOR VERSION UPDATE AVAILABLE ***")
        message("A new TeX Live version is available.")
        message("To upgrade, run: tinytex::reinstall_tinytex(repository = 'illinois')")
        message("\nWould you like to upgrade now? (This will take a few minutes)")
        
        # For interactive use - prompt user
        if (interactive()) {
          response <- readline(prompt = "Upgrade now? (y/n): ")
          if (tolower(trimws(response)) == "y") {
            message("Upgrading TinyTeX to latest version...")
            tinytex::reinstall_tinytex(repository = "illinois")
            message("TinyTeX upgraded successfully!")
          } else {
            message("Skipping upgrade. You can upgrade later with:")
            message("tinytex::reinstall_tinytex(repository = 'illinois')")
          }
        } else {
          message("Run tinytex::reinstall_tinytex(repository = 'illinois') to upgrade")
        }
      } else if (length(update_text) > 0) {
        message("TinyTeX packages checked and updated")
      } else {
        message("TinyTeX is up to date")
      }
    }, error = function(e) {
      message("Note: Update check completed")
    })
  }
}

# Run the check and update
check_and_update_tinytex()


# ----------------------------------------------------------------------------------------------
# Neccessary packages for R Markdown, Quarto, and Shiny 
# installed and/or loaded with the function above
# ----------------------------------------------------------------------------------------------


# rmarkdown: Basic package for R Markdown and Quarto document processing
install_and_load("rmarkdown")

# knitr: Engine for dynamic report generation in R
install_and_load("knitr")

# bslib: Framework for Bootstrap-based HTML theming
install_and_load("bslib")

# tinytex: LaTeX distribution for R Markdown/Quarto
install_and_load("tinytex") 

# shiny app for interactive interfaces 
install_and_load("shiny")

# quarto package for shiny.qmd. files for interactiveness
install_and_load("quarto")

# shiny app package to deploy and manage shiny applications directly from my local envitonment
install_and_load("rsconnect")

# Additional useful packages for Shiny apps
install_and_load("DT")

# For reading and writing YAML files (often used in configuration)
install_and_load("yaml")

# For file system operations
install_and_load("fs")

# For web content processing
install_and_load("htmltools")

# For data manipulation (often needed in Shiny apps)
install_and_load("dplyr")

# For file path handling
install_and_load("here")

# For working with JSON data
install_and_load("jsonlite")

# For handling HTTP requests if your Shiny app makes API calls
install_and_load("httr")

