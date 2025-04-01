#====== Load required libraries =========# 
library(shiny)
library(DT)
library(yaml)  # For reading YAML headers from qmd files
library(fs)    # For file operations
library(rsconnect)

#============================= UI section ==============================# 
ui <- fluidPage(
  tags$head(
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    tags$style(HTML("
      /* Main colors from your SCSS */
      :root {
        --main-blue: #094074ff;
        --secondary-blue: #3c6997ff;
      }
      .title {
        font-size: 1.8rem;
        color: #0A2A5E;
        margin-top: 1.8rem;
        margin-bottom: 1rem;
        font-weight: 700;
      }
      /* Filter section styling */
      .filter-section {
        background: #f8f9fa;
        padding: 15px;
        border-radius: 4px;
        margin-bottom: 15px;
      }
      
      .filter-title {
        color: var(--main-blue);
        font-weight: bold;
        margin-bottom: 15px;
      }
      
      /* Results styling */
      .lma-container {
        display: flex;
        flex-direction: column;
        gap: 20px;
      }
      
      .lma-entry {
        background: #FFFFFF;
        padding: 20px;
        border: 1px solid #eee;
        border-radius: 4px;
      }
      
      .lma-title {
        color: var(--main-blue);
        font-size: 1.2em;
        margin-bottom: 10px;
        text-decoration: none;
        cursor: pointer;
      }

      .lma-title:hover {
        text-decoration: underline;
      }
      
      .lma-meta {
        color: #666;
        font-size: 0.9em;
        margin-bottom: 10px;
      }

      .lma-abstract {
        font-size: 0.95em;
        line-height: 1.5;
        margin-top: 10px;
        color: #333;
      }
      
      .no-results {
        padding: 30px;
        text-align: center;
        color: #666;
        background: #f8f9fa;
        border-radius: 4px;
      }
    "))
  ),
  
  titlePanel(tags$h1("Living Systematic Reviews Directory", class = "title")),
  
  sidebarLayout(
    sidebarPanel(
      div(class = "filter-section",
          h4("Search", class = "filter-title"),
          textInput("search_text", NULL, placeholder = "Search across all fields...")
      ),
      
      div(class = "filter-section",
          h4("Analysis Framework", class = "filter-title"),
          checkboxGroupInput("analysis_type", NULL,
                             choices = c("Bayesian", "Frequentist", "Mixed Methods"),
                             selected = NULL
          )
      ),
      
      div(class = "filter-section",
          h4("Intervention Type", class = "filter-title"),
          checkboxGroupInput("intervention_type", NULL,
                             choices = c("Intranasal oxytocin", 
                                         "Intravenous oxytocin", 
                                         "Endogenous oxytocin measurement", 
                                         "Genetic studies",
                                         "Technology exposure"),
                             selected = NULL
          )
      ),
      
      div(class = "filter-section",
          h4("Biobehavioral Outcome", class = "filter-title"),
          checkboxGroupInput("outcome_category", NULL,
                             choices = c("Social behavior", 
                                         "Emotional processing", 
                                         "Memory and learning", 
                                         "Anxiety and fear", 
                                         "Trust and cooperation",
                                         "Stress reactivity",
                                         "Blood plasma",
                                         "Oxytocin",
                                         "Hormone levels"),
                             selected = NULL
          )
      ),
      
      div(class = "filter-section",
          h4("Study Type", class = "filter-title"),
          checkboxGroupInput("study_type", NULL,
                             choices = c("Quantitative", "Qualitative", "Mixed Methods"),
                             selected = NULL
          )
      ),
      
      div(class = "filter-section",
          h4("Population", class = "filter-title"),
          checkboxGroupInput("population", NULL,
                             choices = c("Healthy Adults", "Clinical Populations", "Mixed Populations", "Adults (18-65)"),
                             selected = NULL
          )
      ),
      
      div(class = "filter-section",
          h4("Status", class = "filter-title"),
          checkboxGroupInput("status_filter", NULL,
                             choices = c("Pre-registered", "Pre-print", "Published"),
                             selected = NULL
          )
      ),
      
      div(class = "filter-section",
          h4("Update Frequency", class = "filter-title"),
          checkboxGroupInput("update_freq", NULL,
                             choices = c("3 months", "6 months", "12 months", "18 months", "24 months"),
                             selected = NULL
          )
      )
    ),
    
    mainPanel(
      uiOutput("lma_list")
    )
  )
)

#====== Server section ==============================# 
server <- function(input, output) {
  # Helper function for NULL handling
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
  
  # Function to read meta-analysis files
  read_meta_analyses <- function() {
    base_dir <- file.path("..", "LMAs")
    files <- list.files(path = base_dir, 
                        pattern = "*.qmd", 
                        full.names = TRUE)
    
    # Print debug info
    print(paste("Found", length(files), "QMD files in", base_dir))
    
    # Add error handling
    if (length(files) == 0) {
      warning("No .qmd files found in LMAs directory")
      return(data.frame(
        Title = character(),
        Status = character(),
        `Analysis Framework` = character(),
        `Intervention Type` = character(),
        `Biobehavioral Outcome` = character(),
        `Study Type` = character(),
        Population = character(),
        `Update Frequency` = character(),
        `Last Updated` = character(),
        Keywords = character(),
        Abstract = character(),
        stringsAsFactors = FALSE
      ))
    }
    
    # Read metadata from each file
    meta_data <- lapply(files, function(file) {
      tryCatch({
        # Print file being processed
        print(paste("Processing file:", file))
        
        # Read the content
        content <- readLines(file, warn = FALSE)
        yaml_start <- which(content == "---")[1]
        yaml_end <- which(content == "---")[2]
        
        if (is.na(yaml_start) || is.na(yaml_end)) {
          warning(paste("Could not find YAML section in", file))
          return(NULL)
        }
        
        # Get YAML metadata
        yaml_text <- content[(yaml_start + 1):(yaml_end - 1)]
        meta <- yaml::yaml.load(paste(yaml_text, collapse = "\n"))
        
        # Store the full content for later use
        meta$full_content <- content
        
        # Extract keywords if present
        if (!is.null(meta$keywords)) {
          meta$keywords_string <- paste(meta$keywords, collapse = ", ")
        } else {
          meta$keywords_string <- NA
        }
        
        # Find date from Timeline section in the content
        timeline_section <- grep("Timeline", content)
        if (length(timeline_section) > 0) {
          # Look for Last update line
          last_update_indices <- grep("Last update", content)
          if (length(last_update_indices) > 0) {
            for (idx in last_update_indices) {
              if (idx > timeline_section[1]) {
                last_update_line <- content[idx]
                date_match <- regexpr("\\d{4}-\\d{2}-\\d{2}", last_update_line)
                if (date_match > 0) {
                  meta$last_updated <- regmatches(last_update_line, date_match)
                  break
                } else {
                  # Try XX.XX.XXXX format
                  date_match <- regexpr("\\d{2}\\.\\d{2}\\.\\d{4}", last_update_line)
                  if (date_match > 0) {
                    meta$last_updated <- regmatches(last_update_line, date_match)
                    break
                  }
                }
              }
            }
          }
        }
        
        # Extract abstract section
        abstract_section <- grep("## Abstract", content)
        if (length(abstract_section) > 0) {
          # Get all lines after Abstract heading
          abstract_lines <- content[(abstract_section[1] + 1):length(content)]
          
          # Find the next heading
          next_heading <- grep("^##", abstract_lines)
          if (length(next_heading) > 0) {
            abstract_end <- next_heading[1] - 1
          } else {
            # If no next heading, use all content
            abstract_end <- length(abstract_lines)
          }
          
          # Get abstract content
          abstract_text <- abstract_lines[1:abstract_end]
          
          # Clean up abstract text
          abstract_text <- abstract_text[!grepl("^:::", abstract_text)]
          abstract_text <- abstract_text[nzchar(trimws(abstract_text))]
          
          meta$abstract <- paste(abstract_text, collapse = "\n")
        }
        
        return(meta)
      }, error = function(e) {
        warning(paste("Error reading file:", file, "-", e$message))
        return(NULL)
      })
    })
    
    # Remove NULL entries
    meta_data <- meta_data[!sapply(meta_data, is.null)]
    
    # Create data frame
    meta_df <- data.frame(
      Title = character(length(meta_data)),
      Status = character(length(meta_data)),
      `Analysis Framework` = character(length(meta_data)),
      `Intervention Type` = character(length(meta_data)),
      `Biobehavioral Outcome` = character(length(meta_data)),
      `Study Type` = character(length(meta_data)),
      Population = character(length(meta_data)),
      `Update Frequency` = character(length(meta_data)),
      `Last Updated` = character(length(meta_data)),
      Keywords = character(length(meta_data)),
      DOI = character(length(meta_data)),
      Abstract = character(length(meta_data)),
      Filename = character(length(meta_data)),
      stringsAsFactors = FALSE
    )
    
    # Update the tryCatch block in meta_df filling loop:
    
    for (i in seq_along(meta_data)) {
      tryCatch({
        # Add filename to metadata for easier debugging
        meta_data[[i]]$filename <- files[i]
        
        # Handle title - checking if it's the template
        if (!is.null(meta_data[[i]]$title)) {
          if (grepl("Title of [Yy]our", meta_data[[i]]$title)) {
            meta_df$Title[i] <- "LMA Template (Example)"
          } else {
            meta_df$Title[i] <- as.character(meta_data[[i]]$title)
          }
        } else {
          meta_df$Title[i] <- paste("Untitled Entry", i)
        }
        
        # Use as.character and NA_character_ for all fields
        meta_df$Status[i] <- if (!is.null(meta_data[[i]]$status)) as.character(meta_data[[i]]$status) else NA_character_
        meta_df$`Analysis Framework`[i] <- if (!is.null(meta_data[[i]]$framework)) as.character(meta_data[[i]]$framework) else NA_character_
        meta_df$`Intervention Type`[i] <- if (!is.null(meta_data[[i]]$intervention_type)) as.character(meta_data[[i]]$intervention_type) else NA_character_
        
        bio_outcome <- meta_data[[i]]$biobehavioral_outcome
        if (is.null(bio_outcome)) {
          meta_df$`Biobehavioral Outcome`[i] <- NA_character_
        } else {
          tryCatch({
            if (is.character(bio_outcome)) {
              meta_df$`Biobehavioral Outcome`[i] <- paste(bio_outcome, collapse = ", ")
            } else if (is.list(bio_outcome)) {
              # Convert complex list to character safely
              outcome_texts <- c()
              for (item in bio_outcome) {
                if (is.list(item)) {
                  item_text <- paste(unlist(lapply(item, as.character)), collapse = ", ")
                } else {
                  item_text <- as.character(item)
                }
                outcome_texts <- c(outcome_texts, item_text)
              }
              meta_df$`Biobehavioral Outcome`[i] <- paste(outcome_texts, collapse = ", ")
            } else {
              # Convert any other type to string representation
              meta_df$`Biobehavioral Outcome`[i] <- as.character(bio_outcome)
            }
          }, error = function(e) {
            warning(paste("Error processing biobehavioral outcome for entry", i, ":", e$message))
            meta_df$`Biobehavioral Outcome`[i] <<- NA_character_
          })
        }
        
        meta_df$`Study Type`[i] <- if (!is.null(meta_data[[i]]$study_type)) as.character(meta_data[[i]]$study_type) else NA_character_
        meta_df$Population[i] <- if (!is.null(meta_data[[i]]$population)) as.character(meta_data[[i]]$population) else NA_character_
        meta_df$`Update Frequency`[i] <- if (!is.null(meta_data[[i]]$`update-frequency`)) as.character(meta_data[[i]]$`update-frequency`) else NA_character_
        
        # Handle last_updated field carefully
        if (is.null(meta_data[[i]]$last_updated)) {
          if (!is.null(meta_data[[i]]$`last-updated`)) {
            meta_df$`Last Updated`[i] <- as.character(meta_data[[i]]$`last-updated`)
          } else {
            meta_df$`Last Updated`[i] <- NA_character_
          }
        } else {
          meta_df$`Last Updated`[i] <- as.character(meta_data[[i]]$last_updated)
        }
        
        meta_df$Keywords[i] <- if (!is.null(meta_data[[i]]$keywords_string)) as.character(meta_data[[i]]$keywords_string) else NA_character_
        
        if (!is.null(meta_data[[i]]$dois) && !is.null(meta_data[[i]]$dois$publication)) {
          meta_df$DOI[i] <- as.character(meta_data[[i]]$dois$publication)
        } else {
          meta_df$DOI[i] <- NA_character_
        }
        
        if (!is.null(meta_data[[i]]$abstract)) {
          meta_df$Abstract[i] <- as.character(meta_data[[i]]$abstract)
        } else {
          meta_df$Abstract[i] <- NA_character_
        }
        
        meta_df$Filename[i] <- basename(files[i])
      }, error = function(e) {
        warning(paste("Error processing entry", i, ":", e$message))
        meta_df$Title[i] <<- paste("Entry", i, "(Error)")
        meta_df$Abstract[i] <<- paste("Error processing this entry:", e$message) 
        meta_df$Status[i] <<- NA_character_
        meta_df$`Analysis Framework`[i] <<- NA_character_
        meta_df$`Intervention Type`[i] <<- NA_character_
        meta_df$`Biobehavioral Outcome`[i] <<- NA_character_
        meta_df$`Study Type`[i] <<- NA_character_
        meta_df$Population[i] <<- NA_character_
        meta_df$`Update Frequency`[i] <<- NA_character_
        meta_df$`Last Updated`[i] <<- NA_character_
        meta_df$Keywords[i] <<- NA_character_
        meta_df$DOI[i] <<- NA_character_
        meta_df$Filename[i] <<- basename(files[i])
      })
    }
    
    valid_rows <- rowSums(is.na(meta_df[, c("Title", "Status", "Analysis Framework")])) < 2
    if (any(!valid_rows)) {
      print(paste("Filtering out", sum(!valid_rows), "invalid entries"))
      meta_df <- meta_df[valid_rows, ]
    }
    
    return(meta_df)
  }
  
  # Reactive expression for filtered data
  filtered_data <- reactive({
    meta_df <- read_meta_analyses()
    
    # Hide template entry unless specifically searching for it
    if (!grepl("template", tolower(input$search_text %||% ""))) {
      meta_df <- meta_df[!grepl("Template", meta_df$Title), ]
    }
    
    # Apply filters only if selections are made
    if (length(input$analysis_type) > 0) {
      meta_df <- meta_df[meta_df$`Analysis Framework` %in% input$analysis_type, ]
    }
    if (length(input$intervention_type) > 0) {
      meta_df <- meta_df[meta_df$`Intervention Type` %in% input$intervention_type, ]
    }
    if (length(input$outcome_category) > 0) {
      meta_df <- meta_df[sapply(meta_df$`Biobehavioral Outcome`, function(outcomes) {
        if (is.na(outcomes)) return(FALSE)
        outcome_list <- trimws(unlist(strsplit(as.character(outcomes), ",")))
        return(any(input$outcome_category %in% outcome_list))
      }), ]
    }
    if (length(input$study_type) > 0) {
      meta_df <- meta_df[meta_df$`Study Type` %in% input$study_type, ]
    }
    if (length(input$population) > 0) {
      meta_df <- meta_df[meta_df$Population %in% input$population, ]
    }
    if (length(input$status_filter) > 0) {
      meta_df <- meta_df[meta_df$Status %in% input$status_filter, ]
    }
    if (length(input$update_freq) > 0) {
      meta_df <- meta_df[meta_df$`Update Frequency` %in% input$update_freq, ]
    }
    
    if (!is.null(input$search_text) && input$search_text != "") {
      search_pattern <- tolower(input$search_text)
      meta_df <- meta_df[grep(search_pattern, 
                              tolower(paste(
                                meta_df$Title, 
                                meta_df$Abstract,
                                meta_df$`Biobehavioral Outcome`,
                                meta_df$`Intervention Type`,
                                meta_df$`Analysis Framework`,
                                meta_df$`Study Type`,
                                meta_df$Population,
                                meta_df$Status,
                                meta_df$Keywords,
                                sep = " "
                              ))), ]
    }
    
    return(meta_df)
  })
  
  # Custom HTML output for LMA list with clickable titles and abstracts
  output$lma_list <- renderUI({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(div(
        class = "no-results",
        "No meta-analyses match your current filter criteria. Please adjust your filters."
      ))
    }
    
    div(class = "lma-container",
        lapply(1:nrow(data), function(i) {
          if (grepl("Template", data$Title[i])) {
            filename <- "lma-template.qmd"
          } else {
            filename <- paste0(gsub(" ", "-", tolower(data$Title[i])), ".qmd")
          }
          
          div(class = "lma-entry",
              tags$a(
                href = paste0("../LMAs/", filename),
                target = "_blank",  # Open in new tab
                class = "lma-title",
                data$Title[i]
              ),
              div(class = "lma-meta",
                  span("Status: ", data$Status[i]), br(),
                  span("Framework: ", data$`Analysis Framework`[i]), br(),
                  span("Intervention Type: ", data$`Intervention Type`[i]), br(),
                  span("Biobehavioral Outcome: ", data$`Biobehavioral Outcome`[i]), br(),
                  span("Study Type: ", data$`Study Type`[i]), br(),
                  span("Population: ", data$Population[i]), br(),
                  span("Update Frequency: ", data$`Update Frequency`[i]), br(),
                  span("Last Updated: ", data$`Last Updated`[i])
              ),
              div(class = "lma-abstract",
                  data$Abstract[i]
              )
          )
        })
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)