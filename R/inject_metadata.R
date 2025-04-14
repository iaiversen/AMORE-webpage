# Script to extract YAML metadata from QMD files and inject it into HTML as meta tags

install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

install_and_load("yaml")
install_and_load("xml2")
install_and_load("fs")
install_and_load("purrr")
install_and_load("jsonlite")


# Paths
QMD_DIR <- "LMAs"
HTML_DIR <- "_site/LMAs"

# Ensure output directory exists
dir_create(HTML_DIR, recurse = TRUE)

# Helper function for NULL handling
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Process a single QMD file
process_file <- function(qmd_file) {
  # Extract filename without extension
  base_filename <- path_file(qmd_file) %>% path_ext_remove()
  html_file <- path(HTML_DIR, paste0(base_filename, ".html"))
  
  # Read QMD file
  qmd_content <- readLines(qmd_file, warn = FALSE)
  
  # Find YAML front matter
  yaml_start <- which(qmd_content == "---")[1]
  yaml_end <- which(qmd_content == "---")[2]
  
  if (is.na(yaml_start) || is.na(yaml_end)) {
    warning("Could not find YAML section in ", qmd_file)
    return(NULL)
  }
  
  # Extract YAML content
  yaml_text <- qmd_content[(yaml_start + 1):(yaml_end - 1)]
  
  # Use tryCatch to catch YAML parsing errors
  meta <- tryCatch({
    yaml::yaml.load(paste(yaml_text, collapse = "\n"))
  }, error = function(e) {
    warning("YAML parsing error in ", qmd_file, ": ", e$message)
    return(NULL)
  })
  
  if (is.null(meta)) return(NULL)
  
  # Check if HTML file exists yet
  if (!file_exists(html_file)) {
    message("HTML file not found for ", qmd_file, ". Will check again during the next build.")
    return(NULL)
  }
  
  # Read HTML file
  html_content <- read_html(html_file)
  
  # Create meta tags for each metadata field
  add_meta_tags <- function(html, meta, parent_key = NULL) {
    for (key in names(meta)) {
      full_key <- if (is.null(parent_key)) key else paste(parent_key, key, sep = "_")
      value <- meta[[key]]
      
      if (is.list(value)) {
        # Recursive call for nested objects
        html <- add_meta_tags(html, value, full_key)
      } else if (is.character(value) || is.numeric(value) || is.logical(value)) {
        # Add meta tag for simple values
        html <- xml_add_child(
          html, 
          "meta", 
          name = full_key, 
          content = as.character(value)
        )
      } else if (is.vector(value) && !is.list(value)) {
        # For vector values, join them with commas
        html <- xml_add_child(
          html, 
          "meta", 
          name = full_key, 
          content = paste(value, collapse = ", ")
        )
      }
    }
    return(html)
  }
  
  # Apply metadata to HTML
  head_node <- xml_find_first(html_content, "//head")
  
  # Delete any existing metadata tags we might have added before
  # Fix the XPath query - this was generating a warning
  existing_meta <- xml_find_all(head_node, 
                                xpath = "//meta[starts-with(@name, 'biobehavioral_outcomes') or @name='framework' or @name='status' or starts-with(@name, 'oxytocin_') or starts-with(@name, 'population_') or @name='update-frequency']")
  
  if (length(existing_meta) > 0) {
    xml_remove(existing_meta)
  }
  
  # Add our metadata tags
  head_node <- add_meta_tags(head_node, meta)
  
  # Optional: Add JSON-LD for structured data
  json_ld <- list(
    "@context" = "https://schema.org",
    "@type" = "ScholarlyArticle",
    "headline" = meta$title,
    "dateModified" = meta$`last-updated` %||% format(Sys.Date(), "%Y-%m-%d")
  )
  
  # Convert to JSON
  json_ld_text <- jsonlite::toJSON(json_ld, auto_unbox = TRUE)
  
  # Remove existing JSON-LD if present
  existing_jsonld <- xml_find_all(head_node, "script[@type='application/ld+json']")
  if (length(existing_jsonld) > 0) {
    xml_remove(existing_jsonld)
  }
  
  # Add JSON-LD script
  xml_add_child(
    head_node,
    "script",
    json_ld_text,
    type = "application/ld+json"
  )
  
  # Write the modified HTML back to file
  write_html(html_content, html_file)
  
  message("Processed: ", qmd_file, " -> ", html_file)
  return(TRUE)
}

# Process all QMD files
# Use safely to continue processing even if one file fails
safe_process <- safely(process_file)
results <- map(qmd_files, safe_process)

# Report on any errors
errors <- map_lgl(results, ~!is.null(.x$error))
if (any(errors)) {
  error_files <- qmd_files[errors]
  cat("Errors occurred while processing the following files:\n")
  walk(error_files, ~cat(" - ", .x, "\n"))
}

cat("Metadata injection complete!\n")
