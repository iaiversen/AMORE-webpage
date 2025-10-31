# ============================================================================
# AMORE Website - Maintenance Script
# ============================================================================
# Purpose: Safely update R packages and verify AMORE functionality
# Location: scripts/Maintenance.R
# Last updated: 30/10/25
# 
# USAGE:
# 1. Ensure all work is committed to Git before running
# 2. Run entire script OR run sections individually
# 3. Review output and test results carefully
# 4. Commit changes only after successful tests
# ============================================================================

# Load required libraries
library(quarto)
library(shiny)
library(rsconnect)

# ============================================================================
# SECTION 1: PRE-UPDATE CHECKS AND BACKUP
# ============================================================================

cat("\n========================================\n")
cat("SECTION 1: PRE-UPDATE CHECKS\n")
cat("========================================\n\n")

# 1.1: Check Git status
cat("1.1 Checking Git status...\n")
git_status <- system("git status --short", intern = TRUE)

if (length(git_status) > 0) {
  cat("⚠️  WARNING: You have uncommitted changes:\n")
  print(git_status)
  cat("\nPlease commit your work before running updates:\n")
  cat("  git add .\n")
  cat("  git commit -m 'Backup before package updates'\n")
  cat("  git push origin main\n\n")
  
  response <- readline(prompt = "Continue anyway? (yes/no): ")
  if (tolower(response) != "yes") {
    stop("Maintenance script stopped. Please commit changes first.")
  }
} else {
  cat("✓ Git working directory is clean\n\n")
}

# 1.2: Document current package versions
cat("1.2 Documenting current package state...\n")
installed_pkgs <- as.data.frame(installed.packages()[, c("Package", "Version", "Built")])
backup_file <- paste0("docs/packages-backup-", format(Sys.Date(), "%Y%m%d"), ".csv")
write.csv(installed_pkgs, backup_file, row.names = FALSE)
cat("✓ Package versions saved to:", backup_file, "\n\n")

# 1.3: Check for outdated packages
cat("1.3 Checking for outdated packages...\n")
old_pkgs <- old.packages()

if (is.null(old_pkgs)) {
  cat("✓ All packages are up to date!\n\n")
  cat("========================================\n")
  cat("NO UPDATES NEEDED - Script Complete\n")
  cat("========================================\n")
  quit(save = "no")
} else {
  cat("Found", nrow(old_pkgs), "outdated packages:\n\n")
  
  # Create summary table
  update_summary <- data.frame(
    Package = old_pkgs[, "Package"],
    Current = old_pkgs[, "Installed"],
    Available = old_pkgs[, "ReposVer"],
    stringsAsFactors = FALSE
  )
  
  # Flag major version updates
  update_summary$UpdateType <- sapply(1:nrow(update_summary), function(i) {
    current_major <- as.numeric(strsplit(update_summary$Current[i], "\\.")[[1]][1])
    available_major <- as.numeric(strsplit(update_summary$Available[i], "\\.")[[1]][1])
    
    if (available_major > current_major) {
      return("🚨 MAJOR")
    } else {
      return("✓ Minor")
    }
  })
  
  print(update_summary[order(update_summary$UpdateType, decreasing = TRUE), ])
  cat("\n")
}

# 1.4: Identify critical AMORE packages
cat("1.4 Identifying critical AMORE packages...\n")
critical_pkgs <- c("quarto", "rmarkdown", "knitr", "bslib", "shiny", 
                   "rsconnect", "DT", "jsonlite", "sass", "tinytex")

critical_updates <- update_summary[update_summary$Package %in% critical_pkgs, ]

if (nrow(critical_updates) > 0) {
  cat("⚠️  Critical AMORE packages need updating:\n")
  print(critical_updates)
  cat("\n")
} else {
  cat("✓ All critical AMORE packages are current\n\n")
}


# ============================================================================
# SECTION 2: PACKAGE UPDATES
# ============================================================================

cat("\n========================================\n")
cat("SECTION 2: UPDATE PACKAGES\n")
cat("========================================\n\n")

# Ask user which update strategy to use (You must run this section)
cat("Choose update strategy:\n")
cat("  1 = Update ONLY critical AMORE packages (safest)\n")
cat("  2 = Update critical + high priority packages (recommended)\n")
cat("  3 = Update ALL packages except major versions (comprehensive)\n")
cat("  4 = Update EVERYTHING including major versions (risky)\n")
cat("  5 = Custom selection\n")
cat("  0 = Skip updates (just run tests)\n\n")

update_choice <- readline(prompt = "Enter choice (0-5): ")

# Define package groups
critical_group <- c("quarto", "rmarkdown", "knitr", "bslib", 
                    "shiny", "rsconnect", "DT", "jsonlite", 
                    "sass", "tinytex")

high_priority_group <- c("dplyr", "tidyr", "data.table", "ggplot2", 
                         "curl", "httr", "httr2", "cli", "gert",
                         "purrr", "stringr", "readr", "fs")

# Identify major version updates
major_updates <- update_summary$Package[update_summary$UpdateType == "🚨 MAJOR"]

# Execute chosen strategy
if (update_choice == "1") {
  cat("\n=== Updating CRITICAL packages only ===\n")
  pkgs_to_update <- intersect(critical_group, update_summary$Package)
  
} else if (update_choice == "2") {
  cat("\n=== Updating CRITICAL + HIGH PRIORITY packages ===\n")
  pkgs_to_update <- intersect(c(critical_group, high_priority_group), 
                              update_summary$Package)
  
} else if (update_choice == "3") {
  cat("\n=== Updating ALL packages EXCEPT major versions ===\n")
  pkgs_to_update <- setdiff(update_summary$Package, major_updates)
  
} else if (update_choice == "4") {
  cat("\n=== Updating EVERYTHING including major versions ===\n")
  cat("⚠️  WARNING: This may introduce breaking changes!\n\n")
  response <- readline(prompt = "Are you sure? (yes/no): ")
  if (tolower(response) != "yes") {
    stop("Update cancelled by user")
  }
  pkgs_to_update <- update_summary$Package
  
} else if (update_choice == "5") {
  cat("\n=== Custom package selection ===\n")
  cat("Available packages:\n")
  cat(paste(1:nrow(update_summary), update_summary$Package, 
            update_summary$UpdateType, sep = ". "), sep = "\n")
  cat("\n")
  selection <- readline(prompt = "Enter package numbers (comma-separated, e.g., 1,3,5): ")
  selected_indices <- as.numeric(unlist(strsplit(selection, ",")))
  pkgs_to_update <- update_summary$Package[selected_indices]
  
} else if (update_choice == "0") {
  cat("\n=== Skipping updates, proceeding to tests ===\n\n")
  pkgs_to_update <- character(0)
  
} else {
  stop("Invalid choice. Please run script again and choose 0-5.")
}

# Perform updates
if (length(pkgs_to_update) > 0) {
  cat("\nPackages to update:\n")
  print(pkgs_to_update)
  cat("\n")
  
  # Show major version warnings
  major_in_selection <- intersect(pkgs_to_update, major_updates)
  if (length(major_in_selection) > 0) {
    cat("⚠️  WARNING: The following have MAJOR version updates:\n")
    print(major_in_selection)
    cat("\n")
  }
  
  response <- readline(prompt = "Proceed with updates? (yes/no): ")
  if (tolower(response) == "yes") {
    cat("\nUpdating packages...\n")
    update.packages(pkgs_to_update, ask = FALSE, checkBuilt = TRUE)
    cat("\n✓ Package updates complete!\n\n")
  } else {
    cat("Updates cancelled by user\n\n")
  }
} else {
  cat("No packages selected for update\n\n")
}


# ============================================================================
# SECTION 3: POST-UPDATE TESTING
# ============================================================================

cat("\n========================================\n")
cat("SECTION 3: POST-UPDATE TESTING\n")
cat("========================================\n\n")

# Initialize test results
test_results <- list()

# 3.1: Test basic rendering
cat("3.1 Testing basic page rendering...\n")
tryCatch({
  quarto::quarto_render("index.qmd", quiet = TRUE, as_job = FALSE)
  cat("✓ index.qmd renders successfully\n")
  test_results$index_render <- "PASS"
}, error = function(e) {
  cat("✗ ERROR rendering index.qmd:\n")
  cat("  ", as.character(e), "\n")
  test_results$index_render <- "FAIL"
})

# 3.2: Test project page rendering
cat("3.2 Testing project page rendering...\n")
lma_files <- list.files("LMAs", pattern = "\\.qmd$", full.names = TRUE)
lma_files <- lma_files[!grepl("^_", basename(lma_files, ))]  # Exclude templates

if (length(lma_files) > 0) {
  test_file <- lma_files[1]  # Test first available LMA file
  tryCatch({
    quarto::quarto_render(test_file, quiet = TRUE)
    cat("✓", basename(test_file), "renders successfully\n")
    test_results$lma_render <- "PASS"
  }, error = function(e) {
    cat("✗ ERROR rendering", basename(test_file), ":\n")
    cat("  ", as.character(e), "\n")
    test_results$lma_render <- "FAIL"
  })
} else {
  cat("⚠️  No LMA files found to test\n")
  test_results$lma_render <- "SKIP"
}

# 3.3: Test full site render
cat("3.3 Testing full site render...\n")
cat("   (This may take a few minutes...)\n")
tryCatch({
  quarto::quarto_render(quiet = TRUE, as_job = FALSE)
  cat("✓ Full site renders successfully\n")
  test_results$full_render <- "PASS"
}, error = function(e) {
  cat("✗ ERROR during full site render:\n")
  cat("  ", as.character(e), "\n")
  test_results$full_render <- "FAIL"
})

# 3.4: Test Shiny app
cat("3.4 Testing Shiny app loading...\n")
tryCatch({
  # Source the app to check for errors
  source("shiny-meta/app.R", local = TRUE)
  cat("✓ Shiny app sources without errors\n")
  test_results$shiny_app <- "PASS"
  
  # Note: Can't fully test interactive features in batch mode
  cat("   Note: Interactive testing required - please test manually:\n")
  cat("   Run: shiny::runApp('shiny-meta/app.R')\n")
  
}, error = function(e) {
  cat("✗ ERROR loading Shiny app:\n")
  cat("  ", as.character(e), "\n")
  test_results$shiny_app <- "FAIL"
})

# 3.5: Test ShinyApps.io connection (if credentials exist)
cat("3.5 Testing ShinyApps.io connection...\n")
if (file.exists("shiny-meta/.Renviron")) {
  tryCatch({
    readRenviron("shiny-meta/.Renviron")
    rsconnect::accountInfo(Sys.getenv("SHINYAPPS_ACCOUNT"))
    cat("✓ ShinyApps.io credentials valid\n")
    test_results$shinyapps_connection <- "PASS"
  }, error = function(e) {
    cat("✗ ERROR connecting to ShinyApps.io:\n")
    cat("  ", as.character(e), "\n")
    test_results$shinyapps_connection <- "FAIL"
  })
} else {
  cat("⚠️  No .Renviron file found - skipping ShinyApps.io test\n")
  test_results$shinyapps_connection <- "SKIP"
}

# 3.6: Check for broken links (optional, can be slow)
cat("3.6 Checking for broken links...\n")
response <- readline(prompt = "Run link checker? (yes/no - can be slow): ")
if (tolower(response) == "yes") {
  if (requireNamespace("urlchecker", quietly = TRUE)) {
    tryCatch({
      broken_links <- urlchecker::url_check(".")
      if (nrow(broken_links) > 0) {
        cat("⚠️  Found", nrow(broken_links), "broken links:\n")
        print(broken_links)
        test_results$link_check <- "WARN"
      } else {
        cat("✓ No broken links found\n")
        test_results$link_check <- "PASS"
      }
    }, error = function(e) {
      cat("✗ Error running link checker:\n")
      cat("  ", as.character(e), "\n")
      test_results$link_check <- "FAIL"
    })
  } else {
    cat("⚠️  urlchecker package not installed - skipping\n")
    cat("   Install with: install.packages('urlchecker')\n")
    test_results$link_check <- "SKIP"
  }
} else {
  cat("⚠️  Link check skipped by user\n")
  test_results$link_check <- "SKIP"
}


# ============================================================================
# SECTION 4: RESULTS SUMMARY
# ============================================================================

cat("\n========================================\n")
cat("SECTION 4: RESULTS SUMMARY\n")
cat("========================================\n\n")

# Create results table
results_df <- data.frame(
  Test = names(test_results),
  Result = unlist(test_results),
  stringsAsFactors = FALSE
)

# Add status symbols
results_df$Status <- sapply(results_df$Result, function(x) {
  switch(x,
         "PASS" = "✓ PASS",
         "FAIL" = "✗ FAIL",
         "WARN" = "⚠️  WARN",
         "SKIP" = "⊘ SKIP",
         "? UNKNOWN")
})

print(results_df[, c("Test", "Status")])

# Determine overall result
if (any(results_df$Result == "FAIL")) {
  cat("\n⚠️  OVERALL: TESTS FAILED\n")
  cat("Please review errors above and fix before committing.\n")
  overall_status <- "FAIL"
} else if (any(results_df$Result == "WARN")) {
  cat("\n⚠️  OVERALL: TESTS PASSED WITH WARNINGS\n")
  cat("Review warnings above. Site should work but may have issues.\n")
  overall_status <- "WARN"
} else {
  cat("\n✓ OVERALL: ALL TESTS PASSED\n")
  cat("Safe to commit and push changes.\n")
  overall_status <- "PASS"
}


# ============================================================================
# SECTION 5: NEXT STEPS
# ============================================================================

cat("\n========================================\n")
cat("SECTION 5: RECOMMENDED NEXT STEPS\n")
cat("========================================\n\n")

if (overall_status == "PASS") {
  cat("1. ✓ Review _site/ output visually\n")
  cat("   Run: quarto::quarto_preview()\n\n")
  
  cat("2. ✓ Test Shiny app interactively\n")
  cat("   Run: shiny::runApp('shiny-meta/app.R')\n")
  cat("   - Test all filter categories\n")
  cat("   - Verify project pages load\n\n")
  
  cat("3. ✓ Document changes\n")
  cat("   Create file: docs/maintenance-log-", format(Sys.Date(), "%Y%m%d"), ".txt\n")
  cat("   List: packages updated, tests run, any issues\n\n")
  
  cat("4. ✓ Commit changes\n")
  cat("   git add .\n")
  cat("   git commit -m 'Update R packages: [list key packages]'\n")
  cat("   git push origin main\n\n")
  
  cat("5. ✓ Monitor Netlify deployment\n")
  cat("   Visit: https://app.netlify.com/\n")
  cat("   Check: Build succeeds without errors\n\n")
  
  cat("6. ✓ Verify live site\n")
  cat("   Visit: https://amore-project.org\n")
  cat("   Check: Homepage, project pages, directory\n\n")
  
} else if (overall_status == "WARN") {
  cat("1. ⚠️  Review warnings carefully\n\n")
  
  cat("2. ⚠️  Test functionality manually\n")
  cat("   Run: quarto::quarto_preview()\n")
  cat("   Check: All pages render correctly\n\n")
  
  cat("3. ⚠️  Decide whether to proceed\n")
  cat("   - If minor warnings: Safe to commit\n")
  cat("   - If major warnings: Fix issues first\n\n")
  
  cat("4. ⚠️  Commit with note\n")
  cat("   git commit -m 'Update packages (with warnings - see log)'\n\n")
  
} else {
  cat("1. ✗ DO NOT COMMIT\n\n")
  
  cat("2. ✗ Review error messages above\n\n")
  
  cat("3. ✗ Rollback problematic packages\n")
  cat("   Check backup file:", backup_file, "\n")
  cat("   Reinstall old versions:\n")
  cat("   devtools::install_version('package_name', version = 'old_version')\n\n")
  
  cat("4. ✗ Run maintenance script again\n\n")
}


# ============================================================================
# SECTION 6: SAVE MAINTENANCE LOG
# ============================================================================

cat("\n========================================\n")
cat("SECTION 6: SAVING MAINTENANCE LOG\n")
cat("========================================\n\n")

log_file <- paste0("docs/maintenance-log-", format(Sys.Date(), "%Y%m%d"), ".txt")

log_content <- c(
  "AMORE Website Maintenance Log",
  paste("Date:", Sys.Date()),
  paste("Time:", Sys.time()),
  "",
  "========================================",
  "PACKAGES UPDATED",
  "========================================",
  if (length(pkgs_to_update) > 0) {
    paste("Updated packages:", paste(pkgs_to_update, collapse = ", "))
  } else {
    "No packages updated"
  },
  "",
  "========================================",
  "TEST RESULTS",
  "========================================",
  capture.output(print(results_df)),
  "",
  "========================================",
  "OVERALL STATUS",
  "========================================",
  paste("Overall:", overall_status),
  "",
  "========================================",
  "NOTES",
  "========================================",
  "Add any additional observations here..."
)

writeLines(log_content, log_file)
cat("✓ Maintenance log saved to:", log_file, "\n")


# ============================================================================
# END OF MAINTENANCE SCRIPT
# ============================================================================

cat("\n========================================\n")
cat("MAINTENANCE SCRIPT COMPLETE\n")
cat("========================================\n\n")

cat("Thank you for maintaining AMORE! 🌟\n\n")