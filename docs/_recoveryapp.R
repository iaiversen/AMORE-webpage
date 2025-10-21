############################################################################
#
#       Shiny app for testing changes to app.R before copy paste to app.R 
#       and publsihing to shinyapps.io server
#
############################################################################

#========================== Load required libraries ====================# 
library(shiny)
library(shinyjs)  # Added for enable/disable functionality
library(DT)
library(yaml)  # For reading YAML headers from qmd files
library(fs)    # For file operations
library(httr)  # For GitHub API requests
library(jsonlite)  # For JSON parsing
library(base64enc)  # For decoding base64 content from GitHub API
library(stringdist)  # For fuzzy search matching
#========================== Helper Functions ====================# 

check_hierarchical_match <- function(stored_value, selected_values, field_type) {
  if (is.na(stored_value) || length(selected_values) == 0) {
    return(FALSE)
  }
  
  stored_values <- strsplit(stored_value, " \\| ")[[1]]
  stored_values <- trimws(stored_values)
  
  hierarchical_parents <- list(
    dosage = c("Variable dosage"),
    route = c("Various administration routes"),
    population_status = c("Mixed"),
    analytical_framework = ("Mixed methods"),
    population_age = c("Mixed age groups")
  )
  
  if (!is.null(hierarchical_parents[[field_type]])) {
    has_parent <- any(stored_values %in% hierarchical_parents[[field_type]])
    if (has_parent) return(TRUE)
    
    selected_parent <- any(selected_values %in% hierarchical_parents[[field_type]])
    if (selected_parent) return(TRUE)
  }
  
  return(any(selected_values %in% stored_values))
}

#============================= UI section ==============================# 

ui <- fluidPage(
  # Initialize shinyjs
  useShinyjs(),
  
  tags$head(
    tags$link(rel = "icon", type = "image/x-icon", href = "amore.favicon.ico"),
    tags$style(HTML("
      /* Import modern fonts */
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');
      
      /* Main colors - modernized palette */
      :root {
        --primary-blue: #0A2A5E;        
        --secondary-blue: #1E50A0;      
        --accent-blue: #3c6997ff;       
        --light-blue: #f0f6ff;          
        --gray-50: #f9fafb;
        --gray-100: #f3f4f6;
        --gray-200: #e5e7eb;
        --gray-300: #d1d5db;
        --gray-600: #4b5563;
        --gray-700: #374151;
        --gray-800: #1f2937;
        --success-green: #10b981;
        --warning-orange: #f59e0b;
      }

      body {
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        background: #FFFFFF;
        min-height: 100vh;
        font-size: 12px; 
        overflow-x: hidden;
      }
      
      /* Main container with responsive padding */
      .container-fluid {
        min-height: 200;
        max-height: none;
        display: flex;
        flex-direction: column;
        overflow: visible;
        max-width: 1600px;
        margin: 0 auto;
        padding: 0 1rem;
      }
      
      /* ===== SEARCH INPUT STYLING ===== */
      .form-control, .search-input {
        border: var(--gray-600) !important;
        box-shadow: 0 0 0 3px rgba(0, 0, 0, 0.05) !important;
        border-radius: 6px;
        padding: 0.75rem 1rem;
        transition: all 0.3s ease;
        font-size: 1rem;
        background: white;
      }

      .form-control:focus, .search-input:focus {
        border-color: var(--gray-600) !important;
        box-shadow: 0 0 0 3px rgba(0, 0, 0, 0.05) !important;
        outline: none !important;
        background: #fafafa;
      }

      .form-control::placeholder, .search-input::placeholder {
        color: #666;
        opacity: 0.7;
      }
      
      /* Title styling */
      .title {
        font-size: 2.5rem;
        color: var(--primary-blue);
        margin: 9rem 0 0.5rem 0;
        font-weight: 700;
        text-align: center;
        letter-spacing: -0.025em;
        background: linear-gradient(135deg, var(--primary-blue) 0%, var(--secondary-blue) 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
        flex-shrink: 0;
      }
      
      /* Main search section - simplified */
      .search-section {
        background: white;
        padding: 1.5rem;
        border-radius: 12px;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
        margin: 1rem auto;
        max-width: 1600px;
        width: 100%;
        box-sizing: border-box;
        flex-shrink: 0;
      }
            
      /* Horizontal filter tabs - simplified design */
      .filter-tabs {
        display: flex;
        flex-wrap: wrap;
        gap: 0.5rem;
        margin: 1rem 0;
        justify-content: center;
      }
      
      .filter-tab {
        background: white;
        border: 2px solid var(--gray-200);
        border-radius: 25px;
        padding: 1rem 2rem;
        cursor: pointer;
        transition: all 0.3s ease;
        font-weight: 500;
        font-size: 1.2rem !important;
        color: var(--gray-700);
        user-select: none;
      }
      
      .filter-tab:hover {
        border-color: var(--primary-blue);
        background: var(--light-blue);
      }
      
      .filter-tab.active {
        background: var(--primary-blue);
        border-color: var(--primary-blue);
        color: white;
        transform: translateY(-1px);
        box-shadow: 0 4px 8px rgba(10, 42, 94, 0.3);
      }
      
      /* Simplified filter panels */
      .filter-panels {
        background: white;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
        margin-top: 1rem;
        overflow: hidden;
        max-height: 0;
        transition: max-height 0.3s ease-out;
      }
      
      .filter-panels.show {
        max-height: 400px;
        overflow-y: auto;
      }
      
      /* Ranking notice styling */
      .ranking-notice {
        background: linear-gradient(135deg, #10b981 0%, #059669 100%);
        color: white;
        padding: 0.75rem 1.5rem;
        border-radius: 8px;
        margin: 1rem auto;
        text-align: center;
        font-weight: 500;
        display: none;
        animation: fadeIn 0.3s ease;
        max-width: 1600px;
        box-shadow: 0 2px 8px rgba(16, 185, 129, 0.3);
      }
      
      @keyframes fadeIn {
        from { opacity: 0; transform: translateY(-10px); }
        to { opacity: 1; transform: translateY(0); }
      }
      
      /* Relevance badge styling */
      .relevance-badge {
        display: inline-block;
        background: linear-gradient(135deg, #10b981 0%, #059669 100%);
        color: white;
        padding: 0.4rem 0.8rem;
        border-radius: 20px;
        font-size: 0.85rem;
        font-weight: 600;
        margin-bottom: 0.75rem;
        letter-spacing: 0.3px;
        box-shadow: 0 2px 4px rgba(16, 185, 129, 0.2);
      }
      
      .lma-entry[data-score=\"0\"] .relevance-badge {
        background: linear-gradient(135deg, #6b7280 0%, #4b5563 100%);
      }
      
      .filter-panel {
        display: none;
        padding: 1.5rem;
        border-top: 2px solid var(--primary-blue);
      }
      
      .filter-panel.active {
        display: block;
      }
      
      /* Remove extra nested styling */
      .filter-section {
        background: none;
        padding: 0;
        border-radius: 0;
        margin-bottom: 1.5rem;
        border: none;
      }
      
      .filter-title {
        color: var(--primary-blue);
        font-weight: 600;
        margin-bottom: 1rem;
        font-size: 1.4rem;
        border-bottom: 1px solid var(--gray-200);
        padding-bottom: 0.5rem;
      }
      
      /* Checkbox styling - simplified */
      .shiny-input-checkboxgroup {
        background: none;
        border: none;
        padding: 0;
      }
      
      .shiny-input-checkboxgroup label {
        font-weight: 400;
        color: var(--gray-700);
        margin-bottom: 0.5rem;
        cursor: pointer;
        transition: all 0.2s ease;
        font-size: 1.2rem;
        display: flex;
        align-items: center;
        line-height: 1.4;
        touch-action: manipulation;
        width: 100%;
        max-width: 100%;
        box-sizing: border-box;
        word-wrap: break-word;
        white-space: normal;
        border-radius: 4px;
      }
      
      .shiny-input-checkboxgroup label:hover {
        background: rgba(10, 42, 94, 0.05);
      }
      
      .shiny-input-checkboxgroup label:hover {
        color: var(--primary-blue);
      }
      
      .shiny-input-checkboxgroup input[type='checkbox'] {
        margin-right: 0.75rem;
        transform: scale(1.1);
        accent-color: var(--primary-blue);
      }
      
      /* Pagination styling */
      .pagination-container {
        display: flex;
        justify-content: space-between;
        align-items: center;
        padding: 1rem 0;
        margin: 0;
        border-bottom: 1px solid var(--gray-200);
        flex-shrink: 0;
        background: white;
        z-index: 10;
      }
      
      .pagination-container.bottom {
        border-bottom: none;
        border-top: 1px solid var(--gray-200);
        margin-top: auto;
      }
      
      .pagination-info {
        color: var(--gray-600);
        font-size: 0.9rem;
        font-weight: 500;
      }
      
      .pagination-controls {
        display: flex;
        align-items: center;
        gap: 1rem;
      }
      
      .pagination-btn {
        background: var(--primary-blue);
        color: white;
        border: none;
        padding: 0.5rem 1rem;
        border-radius: 6px;
        font-size: 0.9rem;
        cursor: pointer;
        transition: all 0.3s ease;
        font-weight: 500;
      }
      
      .pagination-btn:hover:not(:disabled) {
        background: var(--secondary-blue);
        transform: translateY(-1px);
      }
      
      .pagination-btn:disabled {
        background: var(--gray-300);
        color: var(--gray-600);
        cursor: not-allowed;
        transform: none;
      }
      
      .page-indicator {
        color: var(--gray-700);
        font-weight: 600;
        font-size: 0.9rem;
        min-width: 100px;
        text-align: center;
      }
      
      /* Results section styling */
      .results-section {
        max-width: 1600px;
        margin: 1rem auto;
        flex: 1;
        display: flex;
        flex-direction: column;
        min-height: 0;
        overflow: hidden;
        background: white;
      }
      
      .lma-container {
        display: flex;
        flex-direction: column;
        gap: 1.5rem;
        flex: 1;
        min-height: 0;
        overflow-y: visible;
        padding: 1rem 0;
      }
      
      .lma-entry {
        background: white;
        padding: 2rem;
        border-radius: 12px;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
        border-left: 4px solid var(--accent-blue);
        transition: all 0.3s ease;
        min-height: 150px;
        flex-shrink: 0;
      }
      
      .lma-entry:hover {
        transform: translateY(-2px);
        box-shadow: 0 8px 25px rgba(0, 0, 0, 0.15);
      }
      
      .lma-title {
        color: var(--primary-blue);
        font-size: 1.5rem;
        font-weight: 600;
        margin-bottom: 1rem;
        text-decoration: none;
        cursor: pointer;
        line-height: 1.4;
        display: block;
      }

      .lma-title:hover {
        color: var(--secondary-blue);
        text-decoration: underline;
      }
      
      .lma-meta {
        color: var(--gray-600);
        font-size: 1.1rem;
        margin-bottom: 1rem;
        line-height: 1.6;
      }
      
      .lma-meta span {
        display: inline-block;
        margin-right: 1rem;
        margin-bottom: 0.25rem;
      }

      .lma-abstract {
        font-size: 1.2rem;
        line-height: 1.6;
        color: var(--gray-700);
        margin-top: 1rem;
        padding-top: 1rem;
        border-top: 1px solid var(--gray-200);
      }
      
      .no-results {
        padding: 2rem;
        text-align: center;
        color: var(--gray-600);
        background: white;
        border-radius: 12px;
        font-size: 1.1rem;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
        margin: 2rem auto;
        min-height: 200px;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      
      /* ===== RESPONSIVE DESIGN ===== */
      
      /* Desktop screens (1200px and up) */
      @media (min-width: 1200px) {
        .container-fluid {
          padding: 0 2rem;
          max-width: 1400px;
          margin: 0 auto;
        }
        
        .search-section {
          padding: 2rem;
        }
        
        .filter-tab {
          padding: 1rem 2rem;
          font-size: 1.1rem;
        }
        
        .filter-panel {
          padding: 2rem;
        }
        
        .lma-entry {
          padding: 2.5rem;
        }
        
        .lma-title {
          font-size: 1.7rem;
        }
        
        .lma-meta {
          font-size: 1rem;
        }
        
        .lma-abstract {
          font-size: 1.1rem;
        }
        
        .form-control, .search-input {
          font-size: 1.1rem;
          padding: 0.8rem 1.25rem;
        }
      }

      /* Large tablets and small laptops (992px to 1199px) */
      @media (min-width: 992px) and (max-width: 1199px) {
        .container-fluid {
          padding: 0 1.5rem;
        }
        
        .filter-tab {
          padding: 0.8rem 1.5rem;
          font-size: 1rem;
        }
        
        .search-section {
          padding: 1.5rem;
        }
        
        .lma-entry {
          padding: 2rem;
        }
        
        .lma-title {
          font-size: 1.6rem;
        }
      }

      /* Tablets (768px to 991px) */
      @media (min-width: 768px) and (max-width: 991px) {
        .container-fluid {
          padding: 0 1rem;
        }
        
        .title {
          font-size: 2rem;
        }
        
        .filter-tabs {
          justify-content: flex-start;
          overflow-x: auto;
          padding-bottom: 0.5rem;
        }
        
        .filter-tab {
          padding: 0.7rem 1.2rem;
          font-size: 0.9rem;
          min-width: 150px;
          text-align: center;
        }
        
        .search-section {
          margin: 0.5rem;
          padding: 1rem;
        }
        
        .lma-entry {
          padding: 1.5rem;
        }
        
        .lma-title {
          font-size: 1.4rem;
        }
        
        .lma-meta {
          font-size: 0.85rem;
        }
        
        .filter-panels.show {
          max-height: 350px;
        }
      }

      /* Large phones (576px to 767px) */
      @media (min-width: 576px) and (max-width: 767px) {
        .container-fluid {
          padding: 0 0.75rem;
        }
        
        .title {
          font-size: 1.8rem;
        }
        
        .filter-tabs {
          flex-direction: column;
          gap: 0.5rem;
        }
        
        .filter-tab {
          padding: 0.8rem 1rem;
          font-size: 0.9rem;
          border-radius: 8px;
          text-align: center;
          margin: 0;
        }
        
        .selectize-dropdown {
          position: fixed !important;
          z-index: 10000 !important;
          max-height: 300px !important;
          overflow-y: auto !important;
          -webkit-overflow-scrolling: touch !important;
        }
  
        .selectize-input {
          z-index: 9999 !important;
        }
        
  /* Prevent dropdown from closing too quickly */
  .selectize-dropdown-content {
    pointer-events: auto !important;
    touch-action: manipulation !important;
  }
  
  /* Make dropdown items more touchable */
  .selectize-dropdown .option {
    padding: 12px 15px !important;
    min-height: 44px !important;
    line-height: 20px !important;
  }
        
        .search-section {
          padding: 1rem;
          margin: 0.5rem 0;
        }
        
        .form-control, .search-input {
          font-size: 16px; /* Prevents zoom on iOS */
          padding: 0.75rem;
        }
        
        .lma-entry {
          padding: 1.25rem;
        }
        
        .lma-title {
          font-size: 1.3rem;
        }
        
        .lma-meta {
          font-size: 0.8rem;
        }
        
        .lma-abstract {
          font-size: 0.9rem;
        }
        
        .pagination-container {
          flex-direction: column;
          gap: 1rem;
          align-items: stretch;
        }
        
        .pagination-controls {
          justify-content: space-between;
        }
        
        .filter-panels.show {
          max-height: 300px;
        }
      }

      /* Small phones (up to 575px) */
      @media (max-width: 575px) {
        .container-fluid {
          padding: 0 0.5rem;
        }
        
        .title {
          font-size: 1.6rem;
          margin: 0.5rem 0;
        }
        
        .filter-tabs {
          flex-direction: column;
          gap: 0.5rem;
        }
        
        .filter-tab {
          padding: 0.75rem 1rem;
          font-size: 0.85rem;
          border-radius: 6px;
          text-align: center;
        }
        
        .filter-tab.active {
          transform: none;
        }
        
        .search-section {
          padding: 0.75rem;
          margin: 0.5rem 0;
          border-radius: 8px;
        }
        
        .form-control, .search-input {
          font-size: 16px;
          padding: 0.75rem;
        }
        
        .filter-panel {
          padding: 1rem;
        }
        
        .filter-title {
          font-size: 1.1rem;
        }
        
        .shiny-input-checkboxgroup label {
          font-size: 0.85rem;
          margin-bottom: 0.75rem;
        }
        
        .lma-entry {
          padding: 1rem;
          margin-bottom: 1rem;
        }
        
        .lma-title {
          font-size: 1.2rem;
        }
        
        .lma-meta {
          font-size: 0.75rem;
        }
        
        .lma-meta span {
          display: block;
          margin-right: 0;
          margin-bottom: 0.5rem;
        }
        
        .lma-abstract {
          font-size: 0.85rem;
        }
        
        .pagination-container {
          flex-direction: column;
          gap: 0.75rem;
          padding: 0.75rem 0;
        }
        
        .pagination-controls {
          justify-content: center;
          gap: 0.5rem;
        }
        
        .pagination-btn {
          padding: 0.5rem 0.75rem;
          font-size: 0.8rem;
        }
        
        .page-indicator {
          font-size: 0.8rem;
          min-width: 80px;
        }
        
        .pagination-info {
          font-size: 0.8rem;
          text-align: center;
        }
        
        .filter-panels.show {
          max-height: 250px;
        }
      }

      /* Extra small phones (up to 400px) */
      @media (max-width: 400px) {
        .container-fluid {
          padding: 0 0.25rem;
        }
        
        .title {
          font-size: 1.4rem;
        }
        
        .search-section {
          padding: 0.5rem;
        }
        
        .filter-tab {
          padding: 0.6rem 0.8rem;
          font-size: 0.8rem;
        }
        
        .form-control, .search-input {
          padding: 0.6rem;
          font-size: 16px;
        }
        
        .lma-entry {
          padding: 0.75rem;
        }
        
        .lma-title {
          font-size: 1.1rem;
        }
        
        .lma-meta {
          font-size: 0.7rem;
        }
        
        .lma-abstract {
          font-size: 0.8rem;
        }
      }

      /* TV screens (1400px and up) */
      @media (min-width: 1400px) {
        .container-fluid {
          max-width: 1600px;
          padding: 0 3rem;
        }
        
        .search-section {
          padding: 2.5rem;
        }
        
        .filter-tab {
          padding: 1.2rem 2.5rem;
          font-size: 1.2rem;
        }
        
        .form-control, .search-input {
          font-size: 1.2rem;
          padding: 1rem 1.5rem;
        }
        
        .lma-entry {
          padding: 3rem;
        }
        
        .lma-title {
          font-size: 2rem;
        }
        
        .lma-meta {
          font-size: 1.1rem;
        }
        
        .lma-abstract {
          font-size: 1.2rem;
        }
      }

      /* Landscape orientation adjustments for mobile */
      @media (max-width: 767px) and (orientation: landscape) {
        .search-section {
          padding: 0.75rem 1rem;
        }
        
        .filter-tab {
          padding: 0.5rem 1rem;
          font-size: 0.8rem;
        }
        
        .lma-entry {
          padding: 1rem;
        }
        
        .filter-panels.show {
          max-height: 200px;
        }
      }
    "))
  ), 
  
  
  div(class = "container-fluid",
      titlePanel(tags$h1("Living Meta-Analysis Directory", class = "title")),
      
      # Main search section
      div(class = "search-section",
          div(class = "row",
              div(class = "col-12",
                  tags$input(type = "text", 
                             id = "search_text", 
                             class = "form-control search-input", 
                             placeholder = "Search across all fields...")
              )
          ),
          
          # Filter tabs with simpler approach
          div(class = "filter-tabs",
              tags$button("Biological Outcomes", 
                          class = "filter-tab", 
                          type = "button",
                          `data-target` = "biological"),
              tags$button("Psychological and Behavioral", 
                          class = "filter-tab", 
                          type = "button",
                          `data-target` = "psychological"),
              tags$button("Clinical Outcomes", 
                          class = "filter-tab", 
                          type = "button",
                          `data-target` = "clinical"),
              tags$button("Oxytocin Assessment", 
                          class = "filter-tab", 
                          type = "button",
                          `data-target` = "oxytocin"),
              tags$button("Study Population", 
                          class = "filter-tab", 
                          type = "button",
                          `data-target` = "population"),
              tags$button("Study Details", 
                          class = "filter-tab", 
                          type = "button",
                          `data-target` = "details")
          ),
          
          # Simplified filter panels
          div(class = "filter-panels",
              # Biological Outcomes Panel
              div(class = "filter-panel", id = "biological-panel",
                  div(class = "filter-section",
                      h4("Biological Outcomes", class = "filter-title"),
                      checkboxGroupInput("biological_outcomes", NULL,
                                         choices = c("Cardiovascular" = "cardiovascular", 
                                                     "Neuroendocrine" = "neuroendocrine", 
                                                     "Neurological" = "neurological", 
                                                     "Metabolic" = "metabolic", 
                                                     "Immune and Inflammatory" = "immune_inflammatory",
                                                     "Pain and Sensory" = "pain_sensory",
                                                     "Sleep and Circadian" = "sleep_circadian"),
                                         selected = NULL
                      )
                  )
              ),
              
              # Psychological & Behavioral Panel
              div(class = "filter-panel", id = "psychological-panel",
                  div(class = "filter-section",
                      h4("Psychological and Behavioral Outcomes", class = "filter-title"),
                      checkboxGroupInput("psychological_behavioral_outcomes", NULL,
                                         choices = c("Mood and Emotion" = "mood_emotion",
                                                     "Cognition and Memory" = "cognition_memory", 
                                                     "Stress and Coping" = "stress_coping", 
                                                     "Eating and Appetite" = "eating_appetite", 
                                                     "Risk and Decision-Making" = "risk_decision",
                                                     "Sleep Behavior and Quality" = "sleep_behavior_quality",
                                                     "Bonding and Attachment" = "bonding_attachment",
                                                     "Trust and Cooperation" = "trust_cooperation",
                                                     "Communication and Empathy" = "communication_empathy",
                                                     "Aggression and Conflict" = "aggression_conflict"),
                                         selected = NULL
                      )
                  )
              ),
              
              # Clinical Outcomes Panel
              div(class = "filter-panel", id = "clinical-panel",
                  div(class = "filter-section",
                      h4("Clinical Outcomes", class = "filter-title"),
                      checkboxGroupInput("clinical_outcomes", NULL,
                                         choices = c("Neurodevelopmental" = "neurodevelopmental", 
                                                     "Mood Disorders" = "mood_disorders", 
                                                     "Psychotic Disorders" = "psychotic_disorders",
                                                     "Addiction and Substance Use" = "addiction_substance",
                                                     "Eating Disorders" = "eating_disorders",
                                                     "Other Clinical Conditions" = "other_clinical"),
                                         selected = NULL
                      )
                  )
              ),
              
              # Oxytocin Assessment Panel
              div(class = "filter-panel", id = "oxytocin-panel",
                  div(class = "filter-section",
                      h4("Oxytocin Intervention", class = "filter-title"),
                      checkboxGroupInput("oxytocin_intervention", NULL,
                                         choices = c("Intranasal oxytocin administration", 
                                                     "Oral oxytocin administration",
                                                     "Intravenous/injection oxytocin administration", 
                                                     "Environmental/behavioral oxytocin manipulation",
                                                     "Perinatal oxytocin exposure"),
                                         selected = NULL
                      )
                  ),
                  div(class = "filter-section",
                      h4("Oxytocin Assessment Method", class = "filter-title"),
                      checkboxGroupInput("assessment_method", NULL,
                                         choices = c("Biological sample collection", 
                                                     "Behavioral assessment", 
                                                     "Physiological response",
                                                     "Genetic studies", 
                                                     "Neural/imaging measurement"),
                                         selected = NULL
                      )
                  ),
                  div(class = "filter-section",
                      h4("Oxytocin Route", class = "filter-title"),
                      checkboxGroupInput("oxytocin_route", NULL,
                                         choices = c("Central", 
                                                     "Peripheral", 
                                                     "Various administration routes"), 
                                         selected = NULL
                      )
                  ),
                  div(class = "filter-section",
                      h4("Oxytocin Dosage", class = "filter-title"),
                      checkboxGroupInput("oxytocin_dosage", NULL,
                                         choices = c("8 IU", 
                                                     "16 IU", 
                                                     "24 IU",
                                                     "32 IU",
                                                     "40 IU",
                                                     "Variable dosage"),
                                         selected = NULL
                      )
                  )
              ),
              
              # Study Population Panel
              div(class = "filter-panel", id = "population-panel",
                  div(class = "filter-section",
                      h4("Health Status", class = "filter-title"),
                      checkboxGroupInput("population_status", NULL,
                                         choices = c("Healthy", 
                                                     "Clinical", 
                                                     "Mixed"),
                                         selected = NULL
                      )
                  ),
                  div(class = "filter-section",
                      h4("Age Group", class = "filter-title"),
                      checkboxGroupInput("population_age", NULL,
                                         choices = c("Children", 
                                                     "Adolescents",
                                                     "Adults", 
                                                     "Older Adults",
                                                     "Mixed age groups"),
                                         selected = NULL
                      )
                  )
              ),
              
              # Study Details Panel
              div(class = "filter-panel", id = "details-panel",
                  div(class = "filter-section",
                      h4("Analytical Framework", class = "filter-title"),
                      checkboxGroupInput("analysis_framework", NULL,
                                         choices = c("Bayesian", 
                                                     "Frequentist", 
                                                     "Mixed methods"),
                                         selected = NULL
                      )
                  )
              )  # ← closes details-panel
          )  # ← closes filter-panels div
      ),  # ← closes search-section div
      
      # Ranking notice (hidden by default, shown when filters are active)
      div(id = "ranking-notice", 
          class = "ranking-notice",
          "Ranking by criteria relevance - projects with more matches appear first"
      ),  # ← closes ranking-notice div (note: COMMA not double closing parentheses)
      
      # Results section with pagination
      div(class = "results-section",
          # Pagination controls (top)
          div(class = "pagination-container",
              div(class = "pagination-info",
                  textOutput("results_info", inline = TRUE)
              ),
              div(class = "pagination-controls",
                  actionButton("prev_page", "← Previous", class = "pagination-btn"),
                  span(class = "page-indicator",
                       textOutput("page_info", inline = TRUE)
                  ),
                  actionButton("next_page", "Next →", class = "pagination-btn")
              )
          ),
          
          # Results list
          uiOutput("lma_list"),
          
          # Pagination controls (bottom)
          div(class = "pagination-container bottom",
              div(class = "pagination-controls",
                  actionButton("prev_page_bottom", "← Previous", class = "pagination-btn"),
                  span(class = "page-indicator",
                       textOutput("page_info_bottom", inline = TRUE)
                  ),
                  actionButton("next_page_bottom", "Next →", class = "pagination-btn")
              )
          )
      )
  ),
  
  # Enhanced JavaScript with responsive functionality
  tags$script(HTML("
    $(document).ready(function() {
      console.log('Responsive JavaScript loaded');
      
      // Mobile-optimized tab functionality with touch support
      $('.filter-tab').on('click touchend', function(e) {
      e.preventDefault();
      e.stopPropagation();
      
      var target = $(this).data('target');
      var $filterPanels = $('.filter-panels');
      var $clickedTab = $(this);
      
      console.log('Tab clicked/touched:', target);
      
      if ($clickedTab.hasClass('active') && $filterPanels.hasClass('show')) {
        // Close if clicking active tab
        $filterPanels.removeClass('show');
        $('.filter-tab').removeClass('active');
        $('.filter-panel').removeClass('active');
      } else {
        // Open selected tab
        $filterPanels.addClass('show');
        $('.filter-tab').removeClass('active');
        $clickedTab.addClass('active');
        $('.filter-panel').removeClass('active');
        $('#' + target + '-panel').addClass('active');
        
        // Scroll to filter panel on mobile
        if ($(window).width() <= 767) {
          setTimeout(function() {
            $filterPanels[0].scrollIntoView({
              behavior: 'smooth',
              block: 'nearest'
            });
          }, 100);
        }
      }
      
      setTimeout(adjustFilterPanelHeight, 50);
    });
    
    // Prevent default touch behavior
    $('.filter-tab').on('touchstart', function(e) {
      $(this).addClass('touching');
    }).on('touchend touchcancel', function(e) {
      $(this).removeClass('touching');
    });
    
    // Enhanced filter panel height management
    function adjustFilterPanelHeight() {
      var $activePanel = $('.filter-panel.active');
      if ($activePanel.length) {
        var panelHeight = $activePanel.outerHeight();
        var windowHeight = $(window).height();
        var maxHeight = Math.min(panelHeight + 50, windowHeight * 0.5);
        $('.filter-panels.show').css('max-height', maxHeight + 'px');
      }
    }
    
    // Close filter panels when clicking outside on mobile
    $(document).on('click touchend', function(e) {
      if ($(window).width() <= 767) {
        if (!$(e.target).closest('.filter-tabs, .filter-panels').length) {
          $('.filter-panels').removeClass('show');
          $('.filter-tab').removeClass('active');
          $('.filter-panel').removeClass('active');
        }
      }
    });
    
    // Handle window resize
    var resizeTimer;
    $(window).on('resize orientationchange', function() {
      clearTimeout(resizeTimer);
      resizeTimer = setTimeout(function() {
        if ($(window).width() <= 767) {
          $('.filter-panels').removeClass('show');
          $('.filter-tab').removeClass('active');
          $('.filter-panel').removeClass('active');
        }
        adjustFilterPanelHeight();
      }, 250);
    });
    
    // Optimize touch scrolling
    if ($(window).width() <= 767) {
      $('.filter-panels').css({
        '-webkit-overflow-scrolling': 'touch',
        'overscroll-behavior': 'contain'
      });
    }
    
    // Keyboard navigation
    $('.filter-tab').on('keydown', function(e) {
      if (e.key === 'Enter' || e.key === ' ') {
        e.preventDefault();
        $(this).trigger('click');
      }
    });
    
    // Show/hide ranking notice based on filter activity
    $('input[type=\"checkbox\"]').on('change', function() {
      var anyFiltersActive = $('input[type=\"checkbox\"]:checked').length > 0;
      if (anyFiltersActive) {
        $('#ranking-notice').slideDown(300);
      } else {
        $('#ranking-notice').slideUp(300);
      }
    });
  });
")),
  
  # Height tracking and connection management script
  tags$script(HTML("
  // Send height to parent frame
  function sendHeight() {
    const height = document.body.scrollHeight;
    window.parent.postMessage({frameHeight: height}, '*');
  }
  
  // Send initial height and then periodically update
  window.addEventListener('load', function() {
    sendHeight();
    setInterval(sendHeight, 300);
    
    const observer = new MutationObserver(sendHeight);
    observer.observe(document.body, {childList: true, subtree: true});
  });

  // Handle disconnections
  $(document).on('shiny:disconnected', function(event) {
    console.log('Shiny disconnected, attempting reconnection...');
    setTimeout(function() {
      if (!Shiny.shinyapp.isConnected()) {
        location.reload();
      }
    }, 2000);
  });

  // Enhanced keepalive and reconnection handling
  setInterval(function() {
    if (Shiny.shinyapp && Shiny.shinyapp.isConnected()) {
      Shiny.setInputValue('keepalive', new Date().getTime(), {priority: 'event'});
    } else {
      // Try to reconnect if disconnected
      console.log('Shiny disconnected, attempting to reconnect...');
      try {
        Shiny.shinyapp.reconnect();
      } catch(e) {
        setTimeout(function() {
          location.reload();
        }, 1000);
      }
    }
  }, 10000); // Every 10 seconds

  // Additional connection monitoring
  $(document).on('shiny:connected', function() {
    console.log('Shiny connected successfully');
  });
  
  $(document).on('shiny:disconnected', function() {
    console.log('Shiny disconnected - will attempt reconnection');
    setTimeout(function() {
      if (!Shiny.shinyapp.isConnected()) {
        location.reload();
      }
    }, 3000);
  });
"))
)  


#====================================================== Server section ==============================# 
server <- function(input, output, session) {
  # Extend session timeout
  session$allowReconnect(TRUE)
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
  
  # Pagination settings
  ITEMS_PER_PAGE <- 5 
  
  # Reactive value for current page
  current_page <- reactiveVal(1)
  
  # Reset to page 1 when filters change
  observeEvent(list(
    input$search_text,
    input$biological_outcomes,
    input$psychological_behavioral_outcomes,
    input$clinical_outcomes,
    input$oxytocin_intervention,
    input$assessment_method,
    input$oxytocin_route,
    input$oxytocin_dosage,
    input$population_status,
    input$population_age,
    input$analysis_framework
  ), {
    current_page(1)
  })
  
  # Pagination button handlers
  observeEvent(input$prev_page, {
    if (current_page() > 1) {
      current_page(current_page() - 1)
    }
  })
  
  observeEvent(input$next_page, {
    total_pages <- ceiling(nrow(filtered_data()) / ITEMS_PER_PAGE)
    if (current_page() < total_pages) {
      current_page(current_page() + 1)
    }
  })
  
  # Bottom pagination handlers (same as top)
  observeEvent(input$prev_page_bottom, {
    if (current_page() > 1) {
      current_page(current_page() - 1)
    }
  })
  
  observeEvent(input$next_page_bottom, {
    total_pages <- ceiling(nrow(filtered_data()) / ITEMS_PER_PAGE)
    if (current_page() < total_pages) {
      current_page(current_page() + 1)
    }
  })
  
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
  
  # Helper function to handle array values in YAML
  extract_array_as_string <- function(yaml_value) {
    if (is.null(yaml_value)) {
      return(NA_character_)
    }
    
    # Convert to character vector first
    if (is.list(yaml_value)) {
      # Handle nested lists - flatten completely
      flat_list <- unlist(yaml_value, recursive = TRUE, use.names = FALSE)
      if (length(flat_list) == 0) {
        return(NA_character_)
      }
      # Convert to character and remove any NA or empty values
      char_values <- as.character(flat_list)
      clean_values <- char_values[!is.na(char_values) & nzchar(trimws(char_values))]
      if (length(clean_values) == 0) {
        return(NA_character_)
      }
      return(paste(unique(clean_values), collapse = " | "))
    } else if (is.vector(yaml_value) && length(yaml_value) > 1) {
      # Handle vectors (including character vectors)
      char_values <- as.character(yaml_value)
      clean_values <- char_values[!is.na(char_values) & nzchar(trimws(char_values))]
      if (length(clean_values) == 0) {
        return(NA_character_)
      }
      return(paste(unique(clean_values), collapse = " | "))
    } else {
      # Single value
      char_value <- as.character(yaml_value)
      if (is.na(char_value) || !nzchar(trimws(char_value))) {
        return(NA_character_)
      }
      return(char_value)
    }
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
      "memory" = c("learning", "cognition", "recall", "recognition", "attention", "amnesia"),
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
      
      # Proportional fuzzy matching for spelling mistakes
      words_in_text <- unlist(strsplit(text_lower, "\\W+"))
      words_in_text <- words_in_text[nchar(words_in_text) >= 3]  # Check words with 3+ chars
      
      any(sapply(expanded_terms, function(term) {
        term_length <- nchar(term)
        if (term_length < 3) return(FALSE)  # Skip very short search terms
        
        # Proportional threshold based on word length
        threshold <- if(term_length <= 4) 1 else if(term_length <= 8) 2 else 3
        
        any(stringdist(term, words_in_text, method = "lv") <= threshold)
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
    # Get the contents of the LMAs directory with cache-busting
    url <- paste0("https://api.github.com/repos/", repo, "/contents/", path)
    
    # Add cache-busting and headers
    response <- GET(url, 
                    add_headers(
                      "Cache-Control" = "no-cache",
                      "User-Agent" = paste("R-shiny-app", Sys.time())
                    ),
                    query = list(ref = "main", t = as.numeric(Sys.time())))
    
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
      OxytocinIntervention = character(),
      AssessmentMethod = character(),
      OxytocinRoute = character(),
      OxytocinDosage = character(),
      PopulationStatus = character(),
      PopulationAge = character(),
      PopulationClinicalType = character(),
      BiologicalOutcomes = character(),
      PsychologicalBehavioralOutcomes = character(),
      ClinicalOutcomes = character(),
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
        yaml_string <- paste(yaml_text, collapse = "\n")
        
        # Debug: Print YAML for problematic files
        if (grepl("Neurodevelopmental", filename, ignore.case = TRUE)) {
          cat("Debug - Processing file:", filename, "\n")
          cat("YAML content:\n", yaml_string, "\n")
        }
        
        meta <- yaml::yaml.load(yaml_string)
        
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
        
        # Extract outcomes from new structure with array support
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
        
        # Extract multi-value fields with array support
        oxytocin_intervention <- safe_extract(meta, c("oxytocin", "intervention"))
        oxytocin_intervention_str <- extract_array_as_string(oxytocin_intervention)
        
        assessment_method <- safe_extract(meta, c("oxytocin", "assessment_method"))
        assessment_method_str <- extract_array_as_string(assessment_method)
        
        oxytocin_route <- safe_extract(meta, c("oxytocin", "route"))
        oxytocin_route_str <- extract_array_as_string(oxytocin_route)
        
        # Debug: Print route processing for problematic files
        if (grepl("Neurodevelopmental", filename, ignore.case = TRUE)) {
          cat("Debug - Route extracted:", class(oxytocin_route), "\n")
          cat("Debug - Route value:", paste(oxytocin_route, collapse = ", "), "\n")
          cat("Debug - Route string:", oxytocin_route_str, "\n")
        }
        
        oxytocin_dosage <- safe_extract(meta, c("oxytocin", "dosage"))
        oxytocin_dosage_str <- extract_array_as_string(oxytocin_dosage)
        
        population_status <- safe_extract(meta, c("population", "status"))
        population_status_str <- extract_array_as_string(population_status)
        
        population_age <- safe_extract(meta, c("population", "age_group"))
        population_age_str <- extract_array_as_string(population_age)
        
        # Handle status mapping
        status <- meta$status %||% NA_character_
        if (!is.na(status)) {
          if (status == "Pre-registered") {
            status <- "Preregistered"
          }
        }
        
        framework <- meta$analytical_framework %||% meta$framework %||% NA_character_
        framework_str <- extract_array_as_string(framework)
        
        # Extract data into a structured format - CREATE ONLY ONE ENTRY PER FILE
        entry <- list(
          Title = meta$title %||% "Untitled",
          Status = status,
          Framework = framework_str,
          OxytocinIntervention = oxytocin_intervention_str,
          AssessmentMethod = assessment_method_str,
          OxytocinRoute = oxytocin_route_str,
          OxytocinDosage = oxytocin_dosage_str,
          PopulationStatus = population_status_str,
          PopulationAge = population_age_str,
          PopulationClinicalType = safe_extract(meta, c("population", "clinical_type")),
          BiologicalOutcomes = biological_outcomes_str,
          PsychologicalBehavioralOutcomes = psychological_behavioral_outcomes_str,
          ClinicalOutcomes = clinical_outcomes_str,
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
    
    # Convert list to data frame - ENSURE NO DUPLICATES
    if (length(meta_list) > 0) {
      meta_df <- do.call(rbind, lapply(meta_list, function(x) {
        data.frame(
          Title = x$Title,
          Status = x$Status,
          Framework = x$Framework,
          OxytocinIntervention = x$OxytocinIntervention,
          AssessmentMethod = x$AssessmentMethod,
          OxytocinRoute = x$OxytocinRoute,
          OxytocinDosage = x$OxytocinDosage,
          PopulationStatus = x$PopulationStatus,
          PopulationAge = x$PopulationAge,
          PopulationClinicalType = x$PopulationClinicalType,
          BiologicalOutcomes = x$BiologicalOutcomes,
          PsychologicalBehavioralOutcomes = x$PsychologicalBehavioralOutcomes,
          ClinicalOutcomes = x$ClinicalOutcomes,
          LastUpdated = x$LastUpdated,
          Abstract = x$Abstract,
          Filename = x$Filename,
          stringsAsFactors = FALSE
        )
      }))
      
      # Remove any potential duplicate rows based on Title and Filename
      meta_df <- meta_df[!duplicated(meta_df[c("Title", "Filename")]), ]
    }
    
    return(meta_df)
  }
  
  # Helper function to check if any selected values match any stored values
  # (This wrapper calls check_hierarchical_match when needed)
  check_multi_value_match <- function(stored_value, selected_values, field_type = NULL) {
    if (is.na(stored_value) || length(selected_values) == 0) {
      return(FALSE)
    }
    
    # Use hierarchical matching for dosage and route
    if (!is.null(field_type) && field_type %in% c("dosage", "route", "population_status", "population_age")) {
      return(check_hierarchical_match(stored_value, selected_values, field_type))
    }
    
    # Standard matching for other fields
    stored_values <- strsplit(stored_value, " \\| ")[[1]]
    stored_values <- trimws(stored_values)
    return(any(selected_values %in% stored_values))
  }
  
  # Reactive expression to get and parse metadata from GitHub
  meta_data <- reactive({
    
    # Fetch QMD files from GitHub
    qmd_contents <- fetch_github_qmd_files()
    
    # Debug: Print all fetched filenames
    cat("Debug - Fetched files at", Sys.time(), ":\n")
    for(filename in names(qmd_contents)) {
      cat("  -", filename, "\n")
    }
    
    # Parse metadata from QMD files
    meta_df <- parse_qmd_metadata(qmd_contents)
    
    # Debug: Print final dataframe info
    cat("Debug - Final dataframe:\n")
    cat("  - Total rows:", nrow(meta_df), "\n")
    if(nrow(meta_df) > 0) {
      for(i in 1:nrow(meta_df)) {
        cat("  - Row", i, ":", meta_df$Title[i], "(", meta_df$Filename[i], ")\n")
        cat("    Route:", meta_df$OxytocinRoute[i], "\n")
      }
      
      # Check for duplicates
      duplicated_titles <- meta_df$Title[duplicated(meta_df$Title)]
      if(length(duplicated_titles) > 0) {
        cat("Debug - Found duplicate titles:\n")
        for(dup_title in unique(duplicated_titles)) {
          matching_rows <- which(meta_df$Title == dup_title)
          cat("  - Title:", dup_title, "\n")
          for(row in matching_rows) {
            cat("    File:", meta_df$Filename[row], "Route:", meta_df$OxytocinRoute[row], "\n")
          }
        }
      }
    }
    
    return(meta_df)
  })
  
  # Reactive expression for filtered data with RANKING system
  filtered_data <- reactive({
    meta_df <- meta_data()
    
    if (nrow(meta_df) == 0) {
      return(meta_df)
    }
    
    # Hide template entry unless specifically searching for it
    template_files <- c("_lma-template.qmd", "lma-template.qmd", "template.qmd", "Template.qmd")
    if (!grepl("template", tolower(input$search_text %||% ""))) {
      meta_df <- meta_df[!meta_df$Filename %in% template_files, ]
      meta_df <- meta_df[!grepl("Template|template", meta_df$Title), ]
    }
    
    # Initialize relevance score vector
    score_vector <- rep(0, nrow(meta_df))
    
    # Score for biological outcomes (1 point per match)
    if (length(input$biological_outcomes) > 0) {
      bio_scores <- sapply(1:nrow(meta_df), function(i) {
        all_outcomes <- paste(meta_df$BiologicalOutcomes[i], 
                              meta_df$PsychologicalBehavioralOutcomes[i], 
                              meta_df$ClinicalOutcomes[i], sep = " ")
        sum(sapply(input$biological_outcomes, function(category) {
          categorize_outcome(all_outcomes, category)
        }))
      })
      score_vector <- score_vector + bio_scores
    }
    
    # Score for psychological & behavioral outcomes
    if (length(input$psychological_behavioral_outcomes) > 0) {
      psych_behav_scores <- sapply(1:nrow(meta_df), function(i) {
        all_outcomes <- paste(meta_df$BiologicalOutcomes[i], 
                              meta_df$PsychologicalBehavioralOutcomes[i], 
                              meta_df$ClinicalOutcomes[i], sep = " ")
        sum(sapply(input$psychological_behavioral_outcomes, function(category) {
          categorize_outcome(all_outcomes, category)
        }))
      })
      score_vector <- score_vector + psych_behav_scores
    }
    
    # Score for clinical outcomes
    if (length(input$clinical_outcomes) > 0) {
      clinical_scores <- sapply(1:nrow(meta_df), function(i) {
        all_outcomes <- paste(meta_df$BiologicalOutcomes[i], 
                              meta_df$PsychologicalBehavioralOutcomes[i], 
                              meta_df$ClinicalOutcomes[i], sep = " ")
        sum(sapply(input$clinical_outcomes, function(category) {
          categorize_outcome(all_outcomes, category)
        }))
      })
      score_vector <- score_vector + clinical_scores
    }
    
    # Score for oxytocin intervention
    if (length(input$oxytocin_intervention) > 0) {
      intervention_scores <- sapply(1:nrow(meta_df), function(i) {
        as.numeric(check_multi_value_match(meta_df$OxytocinIntervention[i], 
                                           input$oxytocin_intervention))
      })
      score_vector <- score_vector + intervention_scores
    }
    
    # Score for assessment method
    if (length(input$assessment_method) > 0) {
      assessment_scores <- sapply(1:nrow(meta_df), function(i) {
        as.numeric(check_multi_value_match(meta_df$AssessmentMethod[i], 
                                           input$assessment_method))
      })
      score_vector <- score_vector + assessment_scores
    }
    
    # Score for oxytocin route (WITH hierarchical matching)
    if (length(input$oxytocin_route) > 0) {
      route_scores <- sapply(1:nrow(meta_df), function(i) {
        as.numeric(check_multi_value_match(meta_df$OxytocinRoute[i], 
                                           input$oxytocin_route,
                                           field_type = "route"))
      })
      score_vector <- score_vector + route_scores
    }
    
    # Score for oxytocin dosage (WITH hierarchical matching)
    if (length(input$oxytocin_dosage) > 0) {
      dosage_scores <- sapply(1:nrow(meta_df), function(i) {
        as.numeric(check_multi_value_match(meta_df$OxytocinDosage[i], 
                                           input$oxytocin_dosage,
                                           field_type = "dosage"))
      })
      score_vector <- score_vector + dosage_scores
    }
    
    # Score for population status (WITH hierarchical matching)
    if (length(input$population_status) > 0) {
      pop_status_scores <- sapply(1:nrow(meta_df), function(i) {
        as.numeric(check_multi_value_match(meta_df$PopulationStatus[i], 
                                           input$population_status,
                                           field_type = "population_status"))
      })
      score_vector <- score_vector + pop_status_scores
    }
    
    # Score for population age (WITH hierarchical matching)
    if (length(input$population_age) > 0) {
      pop_age_scores <- sapply(1:nrow(meta_df), function(i) {
        as.numeric(check_multi_value_match(meta_df$PopulationAge[i], 
                                           input$population_age,
                                           field_type = "population_age"))
      })
      score_vector <- score_vector + pop_age_scores
    }
    
    # Score for analysis framework
    if (length(input$analysis_framework) > 0) {
      framework_scores <- sapply(1:nrow(meta_df), function(i) {
        as.numeric(check_multi_value_match(meta_df$Framework[i], 
                                           input$analysis_framework))
      })
      score_vector <- score_vector + framework_scores
    }
    
    # Apply enhanced text search (does not contribute to score, but filters)
    if (!is.null(input$search_text) && input$search_text != "") {
      search_text_combined <- paste(
        meta_df$Title, 
        meta_df$Abstract,
        meta_df$BiologicalOutcomes,
        meta_df$PsychologicalBehavioralOutcomes,
        meta_df$ClinicalOutcomes,
        meta_df$OxytocinIntervention,
        meta_df$AssessmentMethod,
        meta_df$OxytocinRoute,
        meta_df$OxytocinDosage,
        meta_df$PopulationStatus,
        meta_df$PopulationAge,
        meta_df$PopulationClinicalType,
        meta_df$Framework,
        sep = " "
      )
      
      search_matches <- enhanced_search(input$search_text, search_text_combined)
      # Keep only rows that match search
      meta_df <- meta_df[search_matches, ]
      score_vector <- score_vector[search_matches]
    }
    
    # Add relevance score to dataframe
    meta_df$relevance_score <- score_vector
    
    # Sort by relevance score (highest first)
    meta_df <- meta_df[order(-meta_df$relevance_score), ]
    
    return(meta_df)
  })
  
  # Paginated data reactive
  paginated_data <- reactive({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(data)
    }
    
    # Calculate pagination
    total_items <- nrow(data)
    total_pages <- ceiling(total_items / ITEMS_PER_PAGE)
    page <- current_page()
    
    # Ensure page is within bounds
    if (page > total_pages) {
      current_page(total_pages)
      page <- total_pages
    }
    if (page < 1) {
      current_page(1)
      page <- 1
    }
    
    # Calculate start and end indices
    start_idx <- (page - 1) * ITEMS_PER_PAGE + 1
    end_idx <- min(page * ITEMS_PER_PAGE, total_items)
    
    # Return paginated data
    data[start_idx:end_idx, ]
  })
  
  # Results info output
  output$results_info <- renderText({
    data <- filtered_data()
    paginated <- paginated_data()
    
    if (nrow(data) == 0) {
      return("No results found")
    }
    
    total_items <- nrow(data)
    start_idx <- (current_page() - 1) * ITEMS_PER_PAGE + 1
    end_idx <- min(current_page() * ITEMS_PER_PAGE, total_items)
    
    paste("Showing", start_idx, "-", end_idx, "of", total_items, "results")
  })
  
  # Page info outputs (for both top and bottom)
  output$page_info <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0) return("Page 0 of 0")
    
    total_pages <- ceiling(nrow(data) / ITEMS_PER_PAGE)
    paste("Page", current_page(), "of", total_pages)
  })
  
  output$page_info_bottom <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0) return("Page 0 of 0")
    
    total_pages <- ceiling(nrow(data) / ITEMS_PER_PAGE)
    paste("Page", current_page(), "of", total_pages)
  })
  
  # Enable/disable pagination buttons
  observe({
    data <- filtered_data()
    total_pages <- ceiling(nrow(data) / ITEMS_PER_PAGE)
    page <- current_page()
    
    # Top buttons
    if (page <= 1) {
      shinyjs::disable("prev_page")
    } else {
      shinyjs::enable("prev_page")
    }
    
    if (page >= total_pages || total_pages == 0) {
      shinyjs::disable("next_page")
    } else {
      shinyjs::enable("next_page")
    }
    
    # Bottom buttons
    if (page <= 1) {
      shinyjs::disable("prev_page_bottom")
    } else {
      shinyjs::enable("prev_page_bottom")
    }
    
    if (page >= total_pages || total_pages == 0) {
      shinyjs::disable("next_page_bottom")
    } else {
      shinyjs::enable("next_page_bottom")
    }
  })
  
  # Custom HTML output for LMA list with clickable titles and abstracts
  output$lma_list <- renderUI({
    data <- paginated_data()  # Use paginated data instead of filtered data
    
    if (nrow(data) == 0) {
      return(div(
        class = "no-results",
        "No meta-analyses match your current filter criteria. Please adjust your filters."
      ))
    }
    
    # Calculate total active filters for badge display
    total_active_filters <- length(input$biological_outcomes) +
                            length(input$psychological_behavioral_outcomes) +
                            length(input$clinical_outcomes) +
                            length(input$oxytocin_intervention) +
                            length(input$assessment_method) +
                            length(input$oxytocin_route) +
                            length(input$oxytocin_dosage) +
                            length(input$population_status) +
                            length(input$population_age) +
                            length(input$analysis_framework)
    
    div(class = "lma-container",
        lapply(1:nrow(data), function(i) {
          # Get the base filename without extension
          base_filename <- tools::file_path_sans_ext(data$Filename[i])
          
          # Construct the URL to the live website LMA page
          lma_url <- paste0("https://amore-project.org/lmas/", base_filename)
          
          # Helper function to check if value should be hidden
          is_empty_value <- function(value) {
            if (is.null(value)) return(TRUE)
            if (is.na(value)) return(TRUE)
            if (is.character(value)) {
              cleaned <- trimws(tolower(value))
              if (cleaned %in% c("", "na", "null", "n/a")) return(TRUE)
            }
            return(FALSE)
          }
          
          # Helper function to format multi-value fields for display
          format_display_value <- function(value) {
            if (is_empty_value(value)) return(NULL)
            # Replace " | " with ", " for better display
            gsub(" \\| ", ", ", value)
          }
          
          div(class = "lma-entry",
              `data-score` = data$relevance_score[i],
              
              # ADD RELEVANCE BADGE
              if(total_active_filters > 0) {
                div(class = "relevance-badge",
                    if(data$relevance_score[i] > 0) {
                      sprintf("✓ %d/%d criteria matched", 
                              data$relevance_score[i], 
                              total_active_filters)
                    } else {
                      "No filter matches"
                    }
                )
              },
              
              tags$a(
                href = lma_url,
                target = "_blank",
                class = "lma-title",
                data$Title[i]
              ),
              div(class = "lma-meta",
                  if (!is_empty_value(data$Framework[i])) span("Analytical Framework: ", format_display_value(data$Framework[i])),
                  if (!is_empty_value(data$OxytocinIntervention[i])) span("Oxytocin Intervention: ", format_display_value(data$OxytocinIntervention[i])),
                  if (!is_empty_value(data$AssessmentMethod[i])) span("Oxytocin Assessment: ", format_display_value(data$AssessmentMethod[i])),
                  if (!is_empty_value(data$OxytocinRoute[i])) span("Oxytocin Route: ", format_display_value(data$OxytocinRoute[i])),
                  if (!is_empty_value(data$OxytocinDosage[i])) span("Oxytocin Dosage: ", format_display_value(data$OxytocinDosage[i])),
                  if (!is_empty_value(data$BiologicalOutcomes[i])) span("Biological Outcomes: ", format_display_value(data$BiologicalOutcomes[i])),
                  if (!is_empty_value(data$PsychologicalBehavioralOutcomes[i])) span("Psychological and Behavioral Outcomes: ", format_display_value(data$PsychologicalBehavioralOutcomes[i])),
                  if (!is_empty_value(data$ClinicalOutcomes[i])) span("Clinical Outcomes: ", format_display_value(data$ClinicalOutcomes[i])),
                  if (!is_empty_value(data$PopulationStatus[i])) span("Population Status: ", format_display_value(data$PopulationStatus[i])),
                  if (!is_empty_value(data$PopulationAge[i])) span("Population Age: ", format_display_value(data$PopulationAge[i])),
                  if (!is_empty_value(data$PopulationClinicalType[i])) span("Clinical Type: ", format_display_value(data$PopulationClinicalType[i])),
                  if (!is_empty_value(data$LastUpdated[i])) span("Last Updated: ", data$LastUpdated[i])
              ),
              div(class = "lma-abstract",
                  data$Abstract[i]
              )
          )
        })
    )
  })
  
  # Keepalive observer  
  observeEvent(input$keepalive, {
    # Just acknowledge the ping to keep session active
    cat("Session keepalive ping received at:", as.character(Sys.time()), "\n")
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
}

# Run the application
shinyApp(ui = ui, server = server)