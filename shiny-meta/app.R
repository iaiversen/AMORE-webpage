#====== Load required libraries =========# 
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

install_and_load("shiny") # For reading YAML headers from qmd files
install_and_load("DT")
install_and_load("yaml")
install_and_load("fs")# For file operations
install_and_load("httr")# For GitHub API requests
install_and_load("jsonlite")# For JSON parsing
install_and_load("base64enc")
install_and_load("xml2")
install_and_load("rvest")# For decoding base64 content from GitHub API


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
                                         "Adolescents",
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
  
  # Function to fetch HTML files from the deployed site
  fetch_site_html_files <- function(base_url = "https://amore-project.org/LMAs/") {
    # We'll use a list to store our findings
    html_contents <- list()
    
    # Try to access the directory listing
    tryCatch({
      # Get the list of LMA pages
      index_url <- base_url
      index_response <- GET(index_url, config = httr::config(ssl_verifypeer = FALSE))
      
      if (!http_error(index_response)) {
        index_html <- read_html(content(index_response, "text"))
        
        # Find all links to HTML files
        links <- index_html %>% 
          html_nodes("a") %>%
          html_attr("href")
        
        # Filter for HTML files and normalize paths
        lma_files <- links[grepl("\\.html$", links)]
        lma_files <- lma_files[!grepl("index\\.html$", lma_files)]
        
        # Fetch each HTML file
        for (file in lma_files) {
          # Handle relative vs absolute URLs
          if (!grepl("^https?://", file)) {
            file_url <- paste0(base_url, file)
          } else {
            file_url <- file
          }
          
          file_response <- GET(file_url, config = httr::config(ssl_verifypeer = FALSE))
          
          if (!http_error(file_response)) {
            html_contents[[basename(file)]] <- content(file_response, "text", encoding = "UTF-8")
          } else {
            warning("Error fetching file: ", http_status(file_response)$message)
          }
        }
      } else {
        warning("Error accessing index page: ", http_status(index_response)$message)
      }
    }, error = function(e) {
      warning("Error accessing site: ", e$message)
      
      # Fallback to a few known files if we can't get a directory listing
      sample_files <- c(
        "example-hugging.oxytocin-lma.html",
        "technology-oxytocin-lma.html"
      )
      
      for (file in sample_files) {
        file_url <- paste0(base_url, file)
        tryCatch({
          file_response <- GET(file_url, config = httr::config(ssl_verifypeer = FALSE))
          
          if (!http_error(file_response)) {
            html_contents[[file]] <- content(file_response, "text", encoding = "UTF-8")
          }
        }, error = function(e) {
          warning("Error fetching file: ", e$message)
        })
      }
    })
    
    # If we couldn't fetch any files, use sample data
    if (length(html_contents) == 0) {
      warning("No files could be fetched, using sample data")
      html_contents[["sample1.html"]] <- "<html><head><title>Sample Meta-Analysis</title><meta name='status' content='Published'></head><body><h2>Abstract</h2><p>Sample abstract.</p></body></html>"
      html_contents[["sample2.html"]] <- "<html><head><title>Another Sample</title><meta name='status' content='Pre-print'></head><body><h2>Abstract</h2><p>Another sample abstract.</p></body></html>"
    }
    
    return(html_contents)
  }
  
  # Function to parse metadata from HTML files
  parse_html_metadata <- function(html_contents) {
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
      URL = character(),
      stringsAsFactors = FALSE
    )
    
    if (length(html_contents) == 0) {
      return(meta_df)
    }
    
    # Process each HTML file
    meta_list <- lapply(names(html_contents), function(filename) {
      content <- html_contents[[filename]]
      
      tryCatch({
        # Parse HTML content
        html_doc <- read_html(content)
        
        # Extract title
        title_node <- html_doc %>% html_node("title")
        title <- if (!is.na(title_node)) {
          title_text <- html_text(title_node, trim = TRUE)
          # Remove the site suffix if present
          gsub(" \\| Active Monitoring of Oxytocin Research Evidence$", "", title_text)
        } else {
          "Untitled"
        }
        
        # Function to extract metadata from meta tags
        get_meta_content <- function(name) {
          meta_tags <- html_doc %>% html_nodes(sprintf("meta[name='%s']", name))
          if (length(meta_tags) > 0) {
            return(html_attr(meta_tags[[1]], "content"))
          }
          return(NA_character_)
        }
        
        # Extract abstract from the content
        abstract <- NA_character_
        abstract_section <- html_doc %>% html_nodes(".abstract-section p, #abstract p")
        if (length(abstract_section) > 0) {
          abstract <- paste(html_text(abstract_section, trim = TRUE), collapse = " ")
        } else {
          # Try finding by heading
          abstract_heading <- html_doc %>% 
            html_nodes("h2, h3") %>% 
            .[grepl("Abstract", html_text(., trim = TRUE))]
          
          if (length(abstract_heading) > 0) {
            # Get the next paragraph
            next_p <- html_doc %>% 
              html_node(paste0("h2:contains('Abstract') + p, h3:contains('Abstract') + p"))
            
            if (!is.na(next_p)) {
              abstract <- html_text(next_p, trim = TRUE)
            }
          }
        }
        
        # Base URL for links
        base_url <- "https://amore-project.org/LMAs/"
        url <- paste0(base_url, filename)
        
        # Create entry with extracted metadata
        entry <- list(
          Title = title,
          Status = get_meta_content("status"),
          Framework = get_meta_content("framework"),
          AssessmentMethod = get_meta_content("oxytocin_assessment_method"),
          OxytocinRoute = get_meta_content("oxytocin_route"),
          PopulationStatus = get_meta_content("population_status"),
          PopulationAge = get_meta_content("population_age_group"),
          PopulationClinicalType = get_meta_content("population_clinical_type"),
          BiologicalOutcomes = get_meta_content("biobehavioral_outcomes_biological"),
          BehavioralOutcomes = get_meta_content("biobehavioral_outcomes_behavioral"),
          SocialOutcomes = get_meta_content("biobehavioral_outcomes_social"),
          UpdateFrequency = get_meta_content("update-frequency"),
          LastUpdated = get_meta_content("last-updated"),
          Abstract = abstract,
          Filename = filename,
          URL = url
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
          URL = x$URL,
          stringsAsFactors = FALSE
        )
      }))
    }
    
    return(meta_df)
  }
  
  # Reactive expression to get and parse metadata from HTML
  meta_data <- reactive({
    # Fetch HTML files from the deployed site
    html_contents <- fetch_site_html_files()
    
    # Parse metadata from HTML files
    meta_df <- parse_html_metadata(html_contents)
    
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
                href = data$URL[i],
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