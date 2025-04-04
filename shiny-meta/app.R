#====== Load required libraries =========# 
library(shiny)
library(DT)
library(yaml)  # For reading YAML headers from qmd files
library(fs)    # For file operations
library(httr)  # For GitHub API requests
library(jsonlite)  # For JSON parsing
library(base64enc)  # For decoding base64 content from GitHub API

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
      
      /* Subcategory styling */
      .subcategory {
        margin-left: 20px;
        margin-top: 10px;
      }
      
      .subcategory-title {
        font-weight: bold;
        margin-bottom: 5px;
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
      
      # Biobehavioral Outcomes
      div(class = "filter-section",
          h4("Biobehavioral Outcomes", class = "filter-title"),
          
          # Biological Outcomes
          div(class = "subcategory",
              div(class = "subcategory-title", "Biological Outcomes"),
              checkboxGroupInput("biological_outcomes", NULL,
                                 choices = c("HRV", 
                                             "Cortisol levels", 
                                             "Pain perception", 
                                             "Anti-inflammatory effects", 
                                             "Insulin-sensitivity",
                                             "Neural activation",
                                             "Blood pressure",
                                             "Immune function",
                                             "Sleep physiology",
                                             "Endocrine function"),
                                 selected = NULL
              )
          ),
          
          # Behavioral Outcomes (with Social Outcomes as a subcategory)
          div(class = "subcategory",
              div(class = "subcategory-title", "Behavioral Outcomes"),
              checkboxGroupInput("behavioral_outcomes", NULL,
                                 choices = c("Mood", 
                                             "Aggression", 
                                             "Stress response", 
                                             "Eating behavior", 
                                             "Fear and anxiety",
                                             "Memory and learning",
                                             "Risk-taking",
                                             "Parental care/behavior"),
                                 selected = NULL
              ),
              
              # Social Outcomes as a nested subcategory of Behavioral Outcomes
              div(class = "subcategory", style = "margin-left: 20px;",
                  div(class = "subcategory-title", "Social Outcomes"),
                  checkboxGroupInput("social_outcomes", NULL,
                                     choices = c("Trust and cooperation", 
                                                 "Empathy", 
                                                 "Sexual behavior", 
                                                 "Social flexibility", 
                                                 "Social bonding",
                                                 "Social recognition",
                                                 "Conflict resolution"),
                                     selected = NULL
                  )
              )
          )
      ),
      
      # Oxytocin Assessment Method
      div(class = "filter-section",
          h4("Oxytocin Assessment Method", class = "filter-title"),
          checkboxGroupInput("assessment_method", NULL,
                             choices = c("Intranasal oxytocin", 
                                         "Intravenous oxytocin", 
                                         "Endogenous oxytocin measurement", 
                                         "Genetic studies"),
                             selected = NULL
          )
      ),
      
      # Oxytocin Route
      div(class = "filter-section",
          h4("Oxytocin Route", class = "filter-title"),
          checkboxGroupInput("oxytocin_route", NULL,
                             choices = c("Central", 
                                         "Peripheral", 
                                         "Both"),
                             selected = NULL
          )
      ),
      
      # Population Health Status
      div(class = "filter-section",
          h4("Population Health Status", class = "filter-title"),
          checkboxGroupInput("population_status", NULL,
                             choices = c("Healthy", 
                                         "Clinical", 
                                         "Mixed"),
                             selected = NULL
          )
      ),
      
      # Population Age Group
      div(class = "filter-section",
          h4("Population Age Group", class = "filter-title"),
          checkboxGroupInput("population_age", NULL,
                             choices = c("Children", 
                                         "Adults", 
                                         "Older Adults",
                                         "Mixed Age Groups"),
                             selected = NULL
          )
      ),
      
      # Analysis Framework
      div(class = "filter-section",
          h4("Analysis Framework", class = "filter-title"),
          checkboxGroupInput("analysis_framework", NULL,
                             choices = c("Bayesian", 
                                         "Frequentist", 
                                         "Mixed Methods"),
                             selected = NULL
          )
      ),
      
      # Status
      div(class = "filter-section",
          h4("Status", class = "filter-title"),
          checkboxGroupInput("status_filter", NULL,
                             choices = c("Pre-registered", 
                                         "Pre-print", 
                                         "Published"),
                             selected = NULL
          )
      ),
      
      # Update Frequency
      div(class = "filter-section",
          h4("Update Frequency", class = "filter-title"),
          checkboxGroupInput("update_freq", NULL,
                             choices = c("3 months", 
                                         "6 months", 
                                         "12 months", 
                                         "18 months", 
                                         "24 months"),
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
server <- function(input, output, session) {
  # Helper function for NULL handling
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
  
  # Helper function to safely extract nested YAML values
  safe_extract <- function(list, path, default = NA_character_) {
    current <- list
    for (key in path) {
      if (is.null(current) || !is.list(current) || is.null(current[[key]])) {
        return(default)
      }
      current <- current[[key]]
    }
    return(current)
  }
  
  # Function to fetch QMD files from GitHub repository
  fetch_github_qmd_files <- function(repo = "iaiversen/AMORE-webpage", path = "LMAs") {
    # Get the contents of the LMAs directory
    url <- paste0("https://api.github.com/repos/", repo, "/contents/", path)
    response <- GET(url)
    
    if (http_error(response)) {
      warning("Error fetching files from GitHub: ", http_status(response)$message)
      return(list())
    }
    
    # Parse the response
    contents <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    # Filter for QMD files
    qmd_files <- contents[grep("\\.qmd$", contents$name), ]
    
    if (length(qmd_files) == 0) {
      warning("No .qmd files found in GitHub repository")
      return(list())
    }
    
    # Fetch the content of each QMD file
    qmd_contents <- lapply(qmd_files$download_url, function(download_url) {
      file_response <- GET(download_url)
      if (http_error(file_response)) {
        warning("Error fetching file: ", http_status(file_response)$message)
        return(NULL)
      }
      return(content(file_response, "text", encoding = "UTF-8"))
    })
    
    # Combine file names with their contents
    names(qmd_contents) <- qmd_files$name
    
    return(qmd_contents)
  }
  
  # Function to extract list values as comma-separated string
  extract_list_as_string <- function(list_val) {
    if (is.null(list_val)) {
      return(NA_character_)
    }
    
    if (is.list(list_val)) {
      # Handle nested lists (for social outcomes)
      flat_list <- unlist(list_val)
      if (length(flat_list) == 0) {
        return(NA_character_)
      }
      return(paste(flat_list, collapse = ", "))
    } else if (is.character(list_val)) {
      if (length(list_val) == 0) {
        return(NA_character_)
      }
      return(paste(list_val, collapse = ", "))
    }
    
    return(NA_character_)
  }
  
  # Function to parse metadata from QMD files
  parse_qmd_metadata <- function(qmd_contents) {
    # Initialize empty data frame
    meta_df <- data.frame(
      Title = character(),
      Status = character(),
      Framework = character(),
      AssessmentMethod = character(),
      OxytocinRoute = character(),
      PopulationStatus = character(),
      PopulationAge = character(),
      PopulationClinicalType = character(),
      BiologicalOutcomes = character(),
      BehavioralOutcomes = character(),
      SocialOutcomes = character(),
      UpdateFrequency = character(),
      LastUpdated = character(),
      Abstract = character(),
      Filename = character(),
      stringsAsFactors = FALSE
    )
    
    if (length(qmd_contents) == 0) {
      return(meta_df)
    }
    
    # Process each QMD file
    meta_list <- lapply(names(qmd_contents), function(filename) {
      content <- qmd_contents[[filename]]
      
      tryCatch({
        # Split content into lines
        lines <- strsplit(content, "\n")[[1]]
        
        # Extract YAML front matter
        yaml_start <- which(lines == "---")[1]
        yaml_end <- which(lines == "---")[2]
        
        if (is.na(yaml_start) || is.na(yaml_end)) {
          warning(paste("Could not find YAML section in", filename))
          return(NULL)
        }
        
        # Get YAML metadata
        yaml_text <- lines[(yaml_start + 1):(yaml_end - 1)]
        meta <- yaml::yaml.load(paste(yaml_text, collapse = "\n"))
        
        # Extract abstract section
        abstract_section <- grep("## Abstract", lines)
        abstract <- ""
        if (length(abstract_section) > 0) {
          # Get all lines after Abstract heading
          abstract_lines <- lines[(abstract_section[1] + 1):length(lines)]
          
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
          
          abstract <- paste(abstract_text, collapse = "\n")
        }
        
        # Find last update date from timeline section
        last_updated <- NA_character_
        timeline_section <- grep("Timeline", lines)
        if (length(timeline_section) > 0) {
          # Look for Last update line
          last_update_indices <- grep("Last update", lines)
          if (length(last_update_indices) > 0) {
            for (idx in last_update_indices) {
              if (idx > timeline_section[1]) {
                last_update_line <- lines[idx]
                date_match <- regexpr("\\d{4}-\\d{2}-\\d{2}", last_update_line)
                if (date_match > 0) {
                  last_updated <- regmatches(last_update_line, date_match)
                  break
                } else {
                  # Try XX.XX.XXXX format
                  date_match <- regexpr("\\d{2}\\.\\d{2}\\.\\d{4}", last_update_line)
                  if (date_match > 0) {
                    last_updated <- regmatches(last_update_line, date_match)
                    break
                  }
                }
              }
            }
          }
        }
        
        # Extract biological outcomes - handle as separate items for filtering
        biological_outcomes <- safe_extract(meta, c("biobehavioral_outcomes", "biological"))
        biological_outcomes_str <- extract_list_as_string(biological_outcomes)
        
        # Extract behavioral outcomes - excluding nested social outcomes
        behavioral_outcomes <- safe_extract(meta, c("biobehavioral_outcomes", "behavioral"))
        # Remove social outcomes if it's a list
        if (is.list(behavioral_outcomes) && "social" %in% names(behavioral_outcomes)) {
          social_outcomes <- behavioral_outcomes$social
          behavioral_outcomes$social <- NULL
        } else {
          social_outcomes <- NULL
        }
        
        # Convert remaining behavioral outcomes to string
        behavioral_items <- list()
        for (name in names(behavioral_outcomes)) {
          if (is.character(name) && name != "social") {
            behavioral_items <- c(behavioral_items, behavioral_outcomes[[name]])
          }
        }
        behavioral_outcomes_str <- extract_list_as_string(behavioral_items)
        
        # Convert social outcomes to string
        social_outcomes_str <- extract_list_as_string(social_outcomes)
        
        # Extract data into a structured format
        entry <- list(
          Title = meta$title %||% "Untitled",
          Status = meta$status %||% NA_character_,
          Framework = meta$framework %||% NA_character_,
          AssessmentMethod = safe_extract(meta, c("oxytocin", "assessment_method")),
          OxytocinRoute = safe_extract(meta, c("oxytocin", "route")),
          PopulationStatus = safe_extract(meta, c("population", "status")),
          PopulationAge = safe_extract(meta, c("population", "age_group")),
          PopulationClinicalType = safe_extract(meta, c("population", "clinical_type")),
          BiologicalOutcomes = biological_outcomes_str,
          BehavioralOutcomes = behavioral_outcomes_str,
          SocialOutcomes = social_outcomes_str,
          UpdateFrequency = meta$`update-frequency` %||% NA_character_,
          LastUpdated = last_updated,
          Abstract = abstract,
          Filename = filename
        )
        
        return(entry)
      }, error = function(e) {
        warning(paste("Error processing file:", filename, "-", e$message))
        return(NULL)
      })
    })
    
    # Remove NULL entries
    meta_list <- meta_list[!sapply(meta_list, is.null)]
    
    # Convert list to data frame
    if (length(meta_list) > 0) {
      meta_df <- do.call(rbind, lapply(meta_list, function(x) {
        data.frame(
          Title = x$Title,
          Status = x$Status,
          Framework = x$Framework,
          AssessmentMethod = x$AssessmentMethod,
          OxytocinRoute = x$OxytocinRoute,
          PopulationStatus = x$PopulationStatus,
          PopulationAge = x$PopulationAge,
          PopulationClinicalType = x$PopulationClinicalType,
          BiologicalOutcomes = x$BiologicalOutcomes,
          BehavioralOutcomes = x$BehavioralOutcomes,
          SocialOutcomes = x$SocialOutcomes,
          UpdateFrequency = x$UpdateFrequency,
          LastUpdated = x$LastUpdated,
          Abstract = x$Abstract,
          Filename = x$Filename,
          stringsAsFactors = FALSE
        )
      }))
    }
    
    return(meta_df)
  }
  
  # Reactive expression to get and parse metadata from GitHub
  meta_data <- reactive({
    # Fetch QMD files from GitHub
    qmd_contents <- fetch_github_qmd_files()
    
    # Parse metadata from QMD files
    meta_df <- parse_qmd_metadata(qmd_contents)
    
    return(meta_df)
  })
  
  # Helper function to check if any term in a list matches a pattern
  contains_any <- function(terms, pattern) {
    if (is.na(terms)) return(FALSE)
    
    term_list <- trimws(unlist(strsplit(as.character(terms), ",")))
    return(any(sapply(term_list, function(term) grepl(pattern, term, ignore.case = TRUE))))
  }
  
  # Reactive expression for filtered data
  filtered_data <- reactive({
    meta_df <- meta_data()
    
    if (nrow(meta_df) == 0) {
      return(meta_df)
    }
    
    # Hide template entry unless specifically searching for it
    if (!grepl("template", tolower(input$search_text %||% ""))) {
      meta_df <- meta_df[!grepl("Template", meta_df$Title), ]
    }
    
    # Apply filters for biological outcomes
    if (length(input$biological_outcomes) > 0) {
      matching_rows <- sapply(1:nrow(meta_df), function(i) {
        outcomes <- meta_df$BiologicalOutcomes[i]
        if (is.na(outcomes)) return(FALSE)
        
        any(sapply(input$biological_outcomes, function(pattern) {
          contains_any(outcomes, pattern)
        }))
      })
      
      meta_df <- meta_df[matching_rows, ]
    }
    
    # Apply filters for behavioral outcomes
    if (length(input$behavioral_outcomes) > 0) {
      matching_rows <- sapply(1:nrow(meta_df), function(i) {
        outcomes <- meta_df$BehavioralOutcomes[i]
        if (is.na(outcomes)) return(FALSE)
        
        any(sapply(input$behavioral_outcomes, function(pattern) {
          contains_any(outcomes, pattern)
        }))
      })
      
      meta_df <- meta_df[matching_rows, ]
    }
    
    # Apply filters for social outcomes
    if (length(input$social_outcomes) > 0) {
      matching_rows <- sapply(1:nrow(meta_df), function(i) {
        outcomes <- meta_df$SocialOutcomes[i]
        if (is.na(outcomes)) return(FALSE)
        
        any(sapply(input$social_outcomes, function(pattern) {
          contains_any(outcomes, pattern)
        }))
      })
      
      meta_df <- meta_df[matching_rows, ]
    }
    
    # Apply filter for assessment method
    if (length(input$assessment_method) > 0) {
      meta_df <- meta_df[!is.na(meta_df$AssessmentMethod) & meta_df$AssessmentMethod %in% input$assessment_method, ]
    }
    
    # Apply filter for oxytocin route
    if (length(input$oxytocin_route) > 0) {
      meta_df <- meta_df[!is.na(meta_df$OxytocinRoute) & meta_df$OxytocinRoute %in% input$oxytocin_route, ]
    }
    
    # Apply filter for population status
    if (length(input$population_status) > 0) {
      meta_df <- meta_df[!is.na(meta_df$PopulationStatus) & meta_df$PopulationStatus %in% input$population_status, ]
    }
    
    # Apply filter for population age
    if (length(input$population_age) > 0) {
      meta_df <- meta_df[!is.na(meta_df$PopulationAge) & meta_df$PopulationAge %in% input$population_age, ]
    }
    
    # Apply filter for analysis framework
    if (length(input$analysis_framework) > 0) {
      meta_df <- meta_df[!is.na(meta_df$Framework) & meta_df$Framework %in% input$analysis_framework, ]
    }
    
    # Apply filter for status
    if (length(input$status_filter) > 0) {
      meta_df <- meta_df[!is.na(meta_df$Status) & meta_df$Status %in% input$status_filter, ]
    }
    
    # Apply filter for update frequency
    if (length(input$update_freq) > 0) {
      meta_df <- meta_df[!is.na(meta_df$UpdateFrequency) & meta_df$UpdateFrequency %in% input$update_freq, ]
    }
    
    # Apply text search across all fields
    if (!is.null(input$search_text) && input$search_text != "") {
      search_pattern <- tolower(input$search_text)
      meta_df <- meta_df[grep(search_pattern, 
                              tolower(paste(
                                meta_df$Title, 
                                meta_df$Abstract,
                                meta_df$BiologicalOutcomes,
                                meta_df$BehavioralOutcomes,
                                meta_df$SocialOutcomes,
                                meta_df$AssessmentMethod,
                                meta_df$OxytocinRoute,
                                meta_df$PopulationStatus,
                                meta_df$PopulationAge,
                                meta_df$PopulationClinicalType,
                                meta_df$Framework,
                                meta_df$Status,
                                meta_df$UpdateFrequency,
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
          div(class = "lma-entry",
              tags$a(
                href = paste0("https://github.com/iaiversen/AMORE-webpage/blob/main/LMAs/", data$Filename[i]),
                target = "_blank",  # Open in new tab
                class = "lma-title",
                data$Title[i]
              ),
              div(class = "lma-meta",
                  # Only show non-NA values
                  if (!is.na(data$Status[i])) span("Status: ", data$Status[i], br()) else NULL,
                  if (!is.na(data$Framework[i])) span("Framework: ", data$Framework[i], br()) else NULL,
                  if (!is.na(data$AssessmentMethod[i])) span("Oxytocin Assessment: ", data$AssessmentMethod[i], br()) else NULL,
                  if (!is.na(data$OxytocinRoute[i])) span("Oxytocin Route: ", data$OxytocinRoute[i], br()) else NULL,
                  
                  # Outcomes - only show if they exist
                  if (!is.na(data$BiologicalOutcomes[i])) span("Biological Outcomes: ", data$BiologicalOutcomes[i], br()) else NULL,
                  if (!is.na(data$BehavioralOutcomes[i])) span("Behavioral Outcomes: ", data$BehavioralOutcomes[i], br()) else NULL,
                  if (!is.na(data$SocialOutcomes[i])) span("Social Outcomes: ", data$SocialOutcomes[i], br()) else NULL,
                  
                  # Population info - only show if they exist
                  if (!is.na(data$PopulationStatus[i])) span("Population Status: ", data$PopulationStatus[i], br()) else NULL,
                  if (!is.na(data$PopulationAge[i])) span("Population Age: ", data$PopulationAge[i], br()) else NULL,
                  if (!is.na(data$PopulationClinicalType[i])) span("Clinical Type: ", data$PopulationClinicalType[i], br()) else NULL,
                  
                  # Other metadata
                  if (!is.na(data$UpdateFrequency[i])) span("Update Frequency: ", data$UpdateFrequency[i], br()) else NULL,
                  if (!is.na(data$LastUpdated[i])) span("Last Updated: ", data$LastUpdated[i], br()) else NULL
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