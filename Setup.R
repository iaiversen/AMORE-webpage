# Last updated: 20.02.2025 | 02/19/2025 
# =================================================================
# AMORE Project Setup Script
# This script installs all necessary components for the AMORE website
# =================================================================

# Prerequisites Installation Instructions
# -------------------------------------
#
# 1. R (version 4.0.0 or higher) from https://cran.r-project.org/
#    - Required for all R packages
#
# 2. RStudio (recommended) from https://posit.co/download/rstudio-desktop/
#    - Recommended IDE for R and Quarto
# 
# 3. Installing packages is global in RStudio, but loading libraries is specific to each script. 

(sass = TRUE)

# -------------------------------------------------------------
# Individualized functions for AMORE 
# -------------------------------------------------------------

# Function to install and load R packages
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

# Check and install TinyTeX if not present
if (!tinytex::is_tinytex()) {
  message("TinyTeX not found. Installing TinyTeX...")
  tinytex::install_tinytex()
} else {
  message("TinyTeX is already installed")
}

# Check and install TinyTeX distribution if not present
# This is different from the R package 'tinytex' installed above
if (!tinytex::is_tinytex()) {
  message("TinyTeX distribution not found. Installing TinyTeX...")
  tinytex::install_tinytex()
} else {
  message("TinyTeX distribution is already installed")
}

# Function to synchronize the styles.scss files across different folders
sync_styles <- function() {
  # Save current directory
  old_dir <- getwd()
  
  # Change to the correct directory
  setwd("C:/Users/iaive/AMORE-webpage")
  
  # Run the batch file
  system("sync-styles.bat")
  
  # Return to original directory
  setwd(old_dir)
  
  cat("✓ Styles synchronized!\n")
}


# Core packages
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

install_and_load("DT")

install_and_load("yaml")

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
