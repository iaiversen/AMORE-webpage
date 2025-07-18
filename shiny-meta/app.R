#====== Load required libraries =========# 
library(shiny)
library(DT)
library(yaml)  # For reading YAML headers from qmd files
library(fs)    # For file operations
library(httr)  # For GitHub API requests
library(jsonlite)  # For JSON parsing
library(base64enc)  # For decoding base64 content from GitHub API
library(stringdist)  # For fuzzy search matching

#============================= UI section ==============================# 
ui <- fluidPage(
  tags$head(
    tags$link(rel = "icon", type = "image/x-icon", href = "amore.favicon.ico"),
    tags$style(HTML("
      /* Main colors from your SCSS */
      :root {
        --main-blue: #094074ff;
        --secondary-blue: #3c6997ff;
      }
      .title {
        font-size: 2.2rem;
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
  
  titlePanel(tags$h1("Living Meta-Analysis Directory", class = "title")),
  
  sidebarLayout(
    sidebarPanel(
      div(class = "filter-section",
          h4("Search", class = "filter-title"),
          textInput("search_text", NULL, placeholder = "Search across all fields...")
      ),
      
      # Biological Outcomes
      div(class = "filter-section",
          h4("Biological Outcomes", class = "filter-title"),
          checkboxGroupInput("biological_outcomes", NULL,
                             choices = c("Cardiovascular" = "cardiovascular", 
                                         "Neuroendocrine" = "neuroendocrine", 
                                         "Neurological" = "neurological", 
                                         "Metabolic" = "metabolic", 
                                         "Immune & Inflammatory" = "immune_inflammatory",
                                         "Pain & Sensory" = "pain_sensory",
                                         "Sleep & Circadian" = "sleep_circadian"),
                             selected = NULL
          )
      ),
      
      # Psychological & Behavioral Outcomes  
      div(class = "filter-section",
          h4("Psychological & Behavioral Outcomes", class = "filter-title"),
          checkboxGroupInput("psychological_behavioral_outcomes", NULL,
                             choices = c("Mood & Emotion" = "mood_emotion",
                                         "Cognition & Memory" = "cognition_memory", 
                                         "Stress & Coping" = "stress_coping", 
                                         "Eating & Appetite" = "eating_appetite", 
                                         "Risk & Decision-Making" = "risk_decision",
                                         "Sleep Behavior & Quality" = "sleep_behavior_quality",
                                         "Bonding & Attachment" = "bonding_attachment",
                                         "Trust & Cooperation" = "trust_cooperation",
                                         "Communication & Empathy" = "communication_empathy",
                                         "Aggression & Conflict" = "aggression_conflict"),
                             selected = NULL
          )
      ),
      
      # Clinical Outcomes
      div(class = "filter-section",
          h4("Clinical Outcomes", class = "filter-title"),
          checkboxGroupInput("clinical_outcomes", NULL,
                             choices = c("Neurodevelopmental" = "neurodevelopmental", 
                                         "Mood Disorders" = "mood_disorders", 
                                         "Psychotic Disorders" = "psychotic_disorders",
                                         "Addiction & Substance Use" = "addiction_substance",
                                         "Eating Disorders" = "eating_disorders",
                                         "Other Clinical Conditions" = "other_clinical"),
                             selected = NULL
          )
      ),
      
      # Oxytocin Assessment Method
      div(class = "filter-section",
          h4("Oxytocin Assessment Method", class = "filter-title"),
          checkboxGroupInput("assessment_method", NULL,
                             choices = c("Intranasal oxytocin", 
                                         "Intravenous oxytocin", 
                                         "Endogenous oxytocin measurement", 
                                         "Genetic studies", 
                                         "Perinatal oxytocin exposure"),
                             selected = NULL
          )
      ),
      
      # Oxytocin Route
      div(class = "filter-section",
          h4("Oxytocin Route", class = "filter-title"),
          checkboxGroupInput("oxytocin_route", NULL,
                             choices = c("Central", 
                                         "Peripheral", 
                                         "Various administration routes", 
                                         "Administration method unspecified"),
                             selected = NULL
          )
      ),
      
      # Oxytocin Dosage
      div(class = "filter-section",
          h4("Oxytocin Dosage", class = "filter-title"),
          checkboxGroupInput("oxytocin_dosage", NULL,
                             choices = c("8 IU", 
                                         "16 IU", 
                                         "24 IU",
                                         "32 IU",
                                         "40 IU",
                                         "Variable dosage",
                                         "Dose-response analysis"),
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
      
      # Analytical Framework
      div(class = "filter-section",
          h4("Analytical Framework", class = "filter-title"),
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
  ),
  
  # JavaScript to handle iframe height adjustment
  tags$script("
    // Send height to parent frame
    function sendHeight() {
      const height = document.body.scrollHeight;
      window.parent.postMessage({frameHeight: height}, '*');
    }
    
    // Send initial height and then periodically update
    window.addEventListener('load', function() {
      sendHeight();
      // Set up monitoring for changes in content height
      setInterval(sendHeight, 300);
      
      // Also send height when UI changes (filtering, etc.)
      const observer = new MutationObserver(sendHeight);
      observer.observe(document.body, {childList: true, subtree: true});
    });
  ")
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
  
  # Enhanced search function with fuzzy matching and synonyms
  enhanced_search <- function(search_term, text_fields) {
    if (is.null(search_term) || search_term == "") return(rep(TRUE, length(text_fields)))
    
    search_term <- tolower(trimws(search_term))
    
    # Synonym dictionary for common terms
    synonyms <- list(
      "stress" = c("anxiety", "tension", "pressure", "cortisol"),
      "bonding" = c("attachment", "connection", "relationship"),
      "trust" = c("cooperation", "prosocial", "altruism"),
      "mood" = c("emotion", "affect", "depression", "happiness"),
      "brain" = c("neural", "neurological", "cognitive", "cerebral"),
      "heart" = c("cardiac", "cardiovascular", "hrv"),
      "pain" = c("nociception", "analgesia", "discomfort"),
      "sleep" = c("circadian", "insomnia", "rest", "slumber"),
      "memory" = c("learning", "cognition", "recall", "recognition"),
      "eating" = c("appetite", "food", "hunger", "nutrition"),
      "social" = c("interpersonal", "relationship", "communication")
    )
    
    # Expand search terms with synonyms
    expanded_terms <- c(search_term)
    for (key in names(synonyms)) {
      if (grepl(key, search_term) || any(sapply(synonyms[[key]], function(syn) grepl(syn, search_term)))) {
        expanded_terms <- c(expanded_terms, synonyms[[key]])
      }
    }
    
    # Check for matches (exact, partial, and fuzzy)
    matches <- sapply(text_fields, function(text) {
      if (is.na(text)) return(FALSE)
      text_lower <- tolower(text)
      
      # Exact or partial matches
      if (any(sapply(expanded_terms, function(term) grepl(term, text_lower)))) {
        return(TRUE)
      }
      
      # Fuzzy matching for spelling mistakes (distance <= 2)
      words_in_text <- unlist(strsplit(text_lower, "\\W+"))
      words_in_text <- words_in_text[nchar(words_in_text) >= 4]  # Only check words with 4+ chars
      
      any(sapply(expanded_terms, function(term) {
        if (nchar(term) < 4) return(FALSE)  # Skip short search terms for fuzzy matching
        any(stringdist(term, words_in_text, method = "lv") <= 2)
      }))
    })
    
    return(matches)
  }
  
  # Function to categorize outcomes based on content
  categorize_outcome <- function(outcome_text, category_type) {
    if (is.na(outcome_text)) return(FALSE)
    
    outcome_lower <- tolower(outcome_text)
    
    if (category_type == "cardiovascular") {
      return(any(sapply(c("hrv", "heart rate", "blood pressure", "cardiac", "cardiovascular"), 
                        function(term) grepl(term, outcome_lower))))
    } else if (category_type == "neuroendocrine") {
      return(any(sapply(c("cortisol", "hormone", "endocrine", "stress marker", "testosterone", "estrogen"), 
                        function(term) grepl(term, outcome_lower))))
    } else if (category_type == "neurological") {
      return(any(sapply(c("brain", "neural", "neurological", "connectivity", "activation", "fmri", "eeg"), 
                        function(term) grepl(term, outcome_lower))))
    } else if (category_type == "metabolic") {
      return(any(sapply(c("insulin", "glucose", "metabolic", "metabolism", "diabetes"), 
                        function(term) grepl(term, outcome_lower))))
    } else if (category_type == "immune_inflammatory") {
      return(any(sapply(c("immune", "inflammatory", "inflammation", "cytokine", "antibody"), 
                        function(term) grepl(term, outcome_lower))))
    } else if (category_type == "pain_sensory") {
      return(any(sapply(c("pain", "sensory", "nociception", "analgesia", "threshold"), 
                        function(term) grepl(term, outcome_lower))))
    } else if (category_type == "sleep_circadian") {
      return(any(sapply(c("sleep", "circadian", "rem", "nrem", "sleep architecture", "melatonin"), 
                        function(term) grepl(term, outcome_lower))))
    } else if (category_type == "mood_emotion") {
      return(any(sapply(c("mood", "emotion", "depression", "anxiety", "affect", "emotional regulation"), 
                        function(term) grepl(term, outcome_lower))))
    } else if (category_type == "cognition_memory") {
      return(any(sapply(c("cognition", "memory", "learning", "attention", "executive", "cognitive"), 
                        function(term) grepl(term, outcome_lower))))
    } else if (category_type == "stress_coping") {
      return(any(sapply(c("stress", "coping", "resilience", "adaptation", "response"), 
                        function(term) grepl(term, outcome_lower))))
    } else if (category_type == "eating_appetite") {
      return(any(sapply(c("eating", "appetite", "food", "hunger", "nutrition", "feeding"), 
                        function(term) grepl(term, outcome_lower))))
    } else if (category_type == "risk_decision") {
      return(any(sapply(c("risk", "decision", "impulsivity", "choice", "gambling"), 
                        function(term) grepl(term, outcome_lower))))
    } else if (category_type == "sleep_behavior_quality") {
      return(any(sapply(c("sleep quality", "sleep duration", "sleep hygiene", "insomnia", "sleep behavior"), 
                        function(term) grepl(term, outcome_lower))))
    } else if (category_type == "bonding_attachment") {
      return(any(sapply(c("bonding", "attachment", "parent", "child", "romantic", "relationship"), 
                        function(term) grepl(term, outcome_lower))))
    } else if (category_type == "trust_cooperation") {
      return(any(sapply(c("trust", "cooperation", "prosocial", "altruism", "helping"), 
                        function(term) grepl(term, outcome_lower))))
    } else if (category_type == "communication_empathy") {
      return(any(sapply(c("empathy", "communication", "social cognition", "perspective", "theory of mind"), 
                        function(term) grepl(term, outcome_lower))))
    } else if (category_type == "aggression_conflict") {
      return(any(sapply(c("aggression", "conflict", "violence", "hostility", "anger"), 
                        function(term) grepl(term, outcome_lower))))
    } else if (category_type == "neurodevelopmental") {
      return(any(sapply(c("autism", "adhd", "developmental", "neurodevelopmental"), 
                        function(term) grepl(term, outcome_lower))))
    } else if (category_type == "mood_disorders") {
      return(any(sapply(c("depression", "anxiety disorder", "bipolar", "mood disorder"), 
                        function(term) grepl(term, outcome_lower))))
    } else if (category_type == "psychotic_disorders") {
      return(any(sapply(c("schizophrenia", "psychosis", "psychotic"), 
                        function(term) grepl(term, outcome_lower))))
    } else if (category_type == "addiction_substance") {
      return(any(sapply(c("addiction", "substance", "drug", "alcohol", "dependence"), 
                        function(term) grepl(term, outcome_lower))))
    } else if (category_type == "eating_disorders") {
      return(any(sapply(c("anorexia", "bulimia", "eating disorder", "binge"), 
                        function(term) grepl(term, outcome_lower))))
    } else if (category_type == "other_clinical") {
      return(any(sapply(c("clinical", "disorder", "syndrome", "condition", "pathology"), 
                        function(term) grepl(term, outcome_lower))))
    }
    
    return(FALSE)
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
      # Handle nested lists
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
      OxytocinDosage = character(),
      PopulationStatus = character(),
      PopulationAge = character(),
      PopulationClinicalType = character(),
      BiologicalOutcomes = character(),
      PsychologicalBehavioralOutcomes = character(),
      ClinicalOutcomes = character(),
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
        
        # Extract outcomes from new structure
        biological_outcomes <- safe_extract(meta, c("outcomes", "biological"))
        biological_outcomes_str <- extract_list_as_string(biological_outcomes)
        
        psychological_behavioral_outcomes <- safe_extract(meta, c("outcomes", "psychological_behavioral"))
        psychological_behavioral_outcomes_str <- extract_list_as_string(psychological_behavioral_outcomes)
        
        clinical_outcomes <- safe_extract(meta, c("outcomes", "clinical"))
        clinical_outcomes_str <- extract_list_as_string(clinical_outcomes)
        
        # Handle legacy structure as fallback
        if (is.na(biological_outcomes_str)) {
          legacy_biological <- safe_extract(meta, c("biobehavioral_outcomes", "biological"))
          biological_outcomes_str <- extract_list_as_string(legacy_biological)
        }
        
        if (is.na(psychological_behavioral_outcomes_str)) {
          legacy_behavioral <- safe_extract(meta, c("biobehavioral_outcomes", "behavioral"))
          legacy_social <- safe_extract(meta, c("biobehavioral_outcomes", "social"))
          combined_legacy <- c(legacy_behavioral, legacy_social)
          psychological_behavioral_outcomes_str <- extract_list_as_string(combined_legacy)
        }
        
        # Extract data into a structured format
        entry <- list(
          Title = meta$title %||% "Untitled",
          Status = meta$status %||% NA_character_,
          Framework = meta$analytical_framework %||% meta$framework %||% NA_character_,
          AssessmentMethod = safe_extract(meta, c("oxytocin", "assessment_method")),
          OxytocinRoute = safe_extract(meta, c("oxytocin", "route")),
          OxytocinDosage = safe_extract(meta, c("oxytocin", "dosage")),
          PopulationStatus = safe_extract(meta, c("population", "status")),
          PopulationAge = safe_extract(meta, c("population", "age_group")),
          PopulationClinicalType = safe_extract(meta, c("population", "clinical_type")),
          BiologicalOutcomes = biological_outcomes_str,
          PsychologicalBehavioralOutcomes = psychological_behavioral_outcomes_str,
          ClinicalOutcomes = clinical_outcomes_str,
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
          OxytocinDosage = x$OxytocinDosage,
          PopulationStatus = x$PopulationStatus,
          PopulationAge = x$PopulationAge,
          PopulationClinicalType = x$PopulationClinicalType,
          BiologicalOutcomes = x$BiologicalOutcomes,
          PsychologicalBehavioralOutcomes = x$PsychologicalBehavioralOutcomes,
          ClinicalOutcomes = x$ClinicalOutcomes,
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
  
  # Reactive expression for filtered data with combined filtering
  filtered_data <- reactive({
    meta_df <- meta_data()
    
    if (nrow(meta_df) == 0) {
      return(meta_df)
    }
    
    # Hide template entry unless specifically searching for it
    template_files <- c("lma-template.qmd", "template.qmd", "Template.qmd")
    if (!grepl("template", tolower(input$search_text %||% ""))) {
      meta_df <- meta_df[!meta_df$Filename %in% template_files, ]
      # Also check title for template patterns
      meta_df <- meta_df[!grepl("Template|template", meta_df$Title), ]
    }
    
    # Create a logical vector for all rows initially TRUE
    keep_rows <- rep(TRUE, nrow(meta_df))
    
    # Apply biological outcomes filter
    if (length(input$biological_outcomes) > 0) {
      bio_matches <- sapply(1:nrow(meta_df), function(i) {
        all_outcomes <- paste(meta_df$BiologicalOutcomes[i], meta_df$PsychologicalBehavioralOutcomes[i], meta_df$ClinicalOutcomes[i], sep = " ")
        any(sapply(input$biological_outcomes, function(category) {
          categorize_outcome(all_outcomes, category)
        }))
      })
      keep_rows <- keep_rows & bio_matches
    }
    
    # Apply psychological & behavioral outcomes filter
    if (length(input$psychological_behavioral_outcomes) > 0) {
      psych_behav_matches <- sapply(1:nrow(meta_df), function(i) {
        all_outcomes <- paste(meta_df$BiologicalOutcomes[i], meta_df$PsychologicalBehavioralOutcomes[i], meta_df$ClinicalOutcomes[i], sep = " ")
        any(sapply(input$psychological_behavioral_outcomes, function(category) {
          categorize_outcome(all_outcomes, category)
        }))
      })
      keep_rows <- keep_rows & psych_behav_matches
    }
    
    # Apply clinical outcomes filter
    if (length(input$clinical_outcomes) > 0) {
      clinical_matches <- sapply(1:nrow(meta_df), function(i) {
        all_outcomes <- paste(meta_df$BiologicalOutcomes[i], meta_df$PsychologicalBehavioralOutcomes[i], meta_df$ClinicalOutcomes[i], sep = " ")
        any(sapply(input$clinical_outcomes, function(category) {
          categorize_outcome(all_outcomes, category)
        }))
      })
      keep_rows <- keep_rows & clinical_matches
    }
    
    # Apply filter for assessment method
    if (length(input$assessment_method) > 0) {
      assessment_matches <- !is.na(meta_df$AssessmentMethod) & meta_df$AssessmentMethod %in% input$assessment_method
      keep_rows <- keep_rows & assessment_matches
    }
    
    # Apply filter for oxytocin route
    if (length(input$oxytocin_route) > 0) {
      route_matches <- !is.na(meta_df$OxytocinRoute) & meta_df$OxytocinRoute %in% input$oxytocin_route
      keep_rows <- keep_rows & route_matches
    }
    
    # Apply filter for oxytocin dosage
    if (length(input$oxytocin_dosage) > 0) {
      dosage_matches <- !is.na(meta_df$OxytocinDosage) & meta_df$OxytocinDosage %in% input$oxytocin_dosage
      keep_rows <- keep_rows & dosage_matches
    }
    
    # Apply filter for population status
    if (length(input$population_status) > 0) {
      pop_status_matches <- !is.na(meta_df$PopulationStatus) & meta_df$PopulationStatus %in% input$population_status
      keep_rows <- keep_rows & pop_status_matches
    }
    
    # Apply filter for population age
    if (length(input$population_age) > 0) {
      pop_age_matches <- !is.na(meta_df$PopulationAge) & meta_df$PopulationAge %in% input$population_age
      keep_rows <- keep_rows & pop_age_matches
    }
    
    # Apply filter for analysis framework
    if (length(input$analysis_framework) > 0) {
      framework_matches <- !is.na(meta_df$Framework) & meta_df$Framework %in% input$analysis_framework
      keep_rows <- keep_rows & framework_matches
    }
    
    # Apply filter for status
    if (length(input$status_filter) > 0) {
      status_matches <- !is.na(meta_df$Status) & meta_df$Status %in% input$status_filter
      keep_rows <- keep_rows & status_matches
    }
    
    # Apply filter for update frequency
    if (length(input$update_freq) > 0) {
      freq_matches <- !is.na(meta_df$UpdateFrequency) & meta_df$UpdateFrequency %in% input$update_freq
      keep_rows <- keep_rows & freq_matches
    }
    
    # Apply enhanced text search across all fields
    if (!is.null(input$search_text) && input$search_text != "") {
      search_text_combined <- paste(
        meta_df$Title, 
        meta_df$Abstract,
        meta_df$BiologicalOutcomes,
        meta_df$PsychologicalBehavioralOutcomes,
        meta_df$ClinicalOutcomes,
        meta_df$AssessmentMethod,
        meta_df$OxytocinRoute,
        meta_df$OxytocinDosage,
        meta_df$PopulationStatus,
        meta_df$PopulationAge,
        meta_df$PopulationClinicalType,
        meta_df$Framework,
        meta_df$Status,
        meta_df$UpdateFrequency,
        sep = " "
      )
      
      search_matches <- enhanced_search(input$search_text, search_text_combined)
      keep_rows <- keep_rows & search_matches
    }
    
    # Apply the combined filter
    meta_df <- meta_df[keep_rows, ]
    
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
          # Get the base filename without extension
          base_filename <- tools::file_path_sans_ext(data$Filename[i])
          
          # Construct the URL to the live website LMA page
          lma_url <- paste0("https://amore-project.org/lmas/", base_filename)
          
          div(class = "lma-entry",
              tags$a(
                href = lma_url,
                target = "_blank",
                class = "lma-title",
                data$Title[i]
              ),
              div(class = "lma-meta",
                  if (!is.na(data$Status[i])) span("Status: ", data$Status[i], br()) else NULL,
                  if (!is.na(data$Framework[i])) span("Analytical Framework: ", data$Framework[i], br()) else NULL,
                  if (!is.na(data$AssessmentMethod[i])) span("Oxytocin Assessment: ", data$AssessmentMethod[i], br()) else NULL,
                  if (!is.na(data$OxytocinRoute[i])) span("Oxytocin Route: ", data$OxytocinRoute[i], br()) else NULL,
                  if (!is.na(data$OxytocinDosage[i])) span("Oxytocin Dosage: ", data$OxytocinDosage[i], br()) else NULL,
                  if (!is.na(data$BiologicalOutcomes[i])) span("Biological Outcomes: ", data$BiologicalOutcomes[i], br()) else NULL,
                  if (!is.na(data$PsychologicalBehavioralOutcomes[i])) span("Psychological & Behavioral Outcomes: ", data$PsychologicalBehavioralOutcomes[i], br()) else NULL,
                  if (!is.na(data$ClinicalOutcomes[i])) span("Clinical Outcomes: ", data$ClinicalOutcomes[i], br()) else NULL,
                  if (!is.na(data$PopulationStatus[i])) span("Population Status: ", data$PopulationStatus[i], br()) else NULL,
                  if (!is.na(data$PopulationAge[i])) span("Population Age: ", data$PopulationAge[i], br()) else NULL,
                  if (!is.na(data$PopulationClinicalType[i])) span("Clinical Type: ", data$PopulationClinicalType[i], br()) else NULL,
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