#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# PART 3c - EXCLUSION KEYWORD LIST----------------------------------------------
# 
# Summary: this is an RShiny script that 
#
# Output:
#///////////////////////////////////////////////////////////////////////////////

library(shiny)
library(data.table)
library(here)
library(sortable)  # Enables drag-and-drop functionality

# Define file path for saving results
exclusion_rds_path <- here("exclusion_keyword_selection.RData")

# Load previous exclusion keyword selections if they exist
if (file.exists(exclusion_rds_path)) {
  load(exclusion_rds_path)  
} else {
  exclusion_csv_path <- here("Exclusion_keywords.csv")
  
  if (file.exists(exclusion_csv_path)) {
    exclusion_keywords_dt <- fread(exclusion_csv_path, select = "Keyword")
    
    # Clean up keyword column
    exclusion_keywords_dt[, Keyword := gsub("\\\\b", "", Keyword)]
    exclusion_keywords_dt[, Keyword := gsub('["]', "", Keyword)]
    exclusion_keywords_dt[, Keyword := trimws(Keyword)]
    
    # Initialize tables with consistent row counts
    included_exclusion_dt <- data.table(Keyword = character(0), Specificity = NA_integer_)
    removed_exclusion_dt <- exclusion_keywords_dt[!is.na(Keyword) & Keyword != ""]
  } else {
    stop("Error: Exclusion_keywords.csv not found. Ensure it exists in the expected directory.")
  }
}

# Ensure Specificity column remains assigned across iterations
if (!"Specificity" %in% colnames(included_exclusion_dt)) {
  included_exclusion_dt[, Specificity := NA_integer_]
}

# Assign to Global Environment for iterative use
assign("exclusion_keywords_dt", exclusion_keywords_dt, envir = .GlobalEnv)
assign("included_exclusion_dt", included_exclusion_dt, envir = .GlobalEnv)
assign("removed_exclusion_dt", removed_exclusion_dt, envir = .GlobalEnv)

# Define UI
ui <- fluidPage(
  titlePanel("Iteratively Update Exclusion Keywords"),
  
  tags$div(
    tags$p("Keyword exclusion filtering:"),
    tags$ul(
      tags$li("These keywords determine which sections to exclude."),
      tags$li("Choose words that SHOULD NOT be included."),
      tags$li("Selections persist across sessions and iterations."),
      tags$li("Click 'Finalize' button at bottom when finished.")
    )
  ),
  
  fluidRow(
    textInput("new_exclusion_keyword", "Add a new exclusion keyword:", ""),
    actionButton("add_exclusion_keyword", "Add to Included Keywords")
  ),
  
  uiOutput("exclusion_keyword_ui"),
  
  actionButton("submit_exclusion", "Finalize Selection"),
  verbatimTextOutput("summary_exclusion_output")
)

# Define Server
server <- function(input, output, session) {
  exclusion_data <- reactiveValues(
    included = included_exclusion_dt$Keyword,
    removed = removed_exclusion_dt$Keyword,
    full_exclusion_list = exclusion_keywords_dt$Keyword  # Track full list iteratively
  )
  
  # Observe adding new exclusion keyword
  observeEvent(input$add_exclusion_keyword, {
    new_word <- trimws(input$new_exclusion_keyword)
    
    if (nzchar(new_word) && !(new_word %in% exclusion_data$included)) {
      exclusion_data$included <- c(exclusion_data$included, new_word)
      
      included_exclusion_dt <- data.table(Keyword = exclusion_data$included, Specificity = NA_integer_)
      assign("included_exclusion_dt", included_exclusion_dt, envir = .GlobalEnv)
      
      # Update the exclusion keywords list iteratively
      exclusion_data$full_exclusion_list <- unique(c(exclusion_data$full_exclusion_list, new_word))
      exclusion_keywords_dt <- data.table(Keyword = exclusion_data$full_exclusion_list)
      assign("exclusion_keywords_dt", exclusion_keywords_dt, envir = .GlobalEnv)
    }
  })
  
  # Observe submission and save results iteratively
  observeEvent(input$submit_exclusion, {
    exclusion_data$included <- input$included_exclusion_keywords %||% exclusion_data$included
    exclusion_data$removed <- input$removed_exclusion_keywords %||% exclusion_data$removed
    
    # Ensure row consistency before merging
    included_exclusion_dt <- data.table(
      Keyword = exclusion_data$included, 
      Specificity = included_exclusion_dt[Keyword %in% exclusion_data$included, Specificity]
    )
    
    removed_exclusion_dt <- data.table(Keyword = exclusion_data$removed)
    
    # Update the exclusion list iteratively before saving
    exclusion_data$full_exclusion_list <- unique(c(exclusion_data$full_exclusion_list, exclusion_data$included))
    exclusion_keywords_dt <- data.table(Keyword = exclusion_data$full_exclusion_list)
    
    # Use rbindlist(fill=TRUE) to prevent row mismatches
    included_exclusion_dt <- rbindlist(list(included_exclusion_dt), fill = TRUE)
    removed_exclusion_dt <- rbindlist(list(removed_exclusion_dt), fill = TRUE)
    exclusion_keywords_dt <- rbindlist(list(exclusion_keywords_dt), fill = TRUE)
    
    assign("included_exclusion_dt", included_exclusion_dt, envir = .GlobalEnv)
    assign("removed_exclusion_dt", removed_exclusion_dt, envir = .GlobalEnv)
    assign("exclusion_keywords_dt", exclusion_keywords_dt, envir = .GlobalEnv)
    
    print("Saving iterative exclusion keyword selections...")
    
    tryCatch(
      {
        save(exclusion_keywords_dt, included_exclusion_dt, removed_exclusion_dt, file = exclusion_rds_path)
        flush.console()
        print("Save successful!")
        
        Sys.sleep(1)  # Small delay before closing
        session$close()  # Closes Shiny session
        stopApp()  # Fully stops the app
      },
      error = function(e) {
        showNotification(paste("Save Error:", e$message), type = "error")
        print(paste("Save Error:", e$message))
      }
    )
  })
  
  # Render keyword UI dynamically
  output$exclusion_keyword_ui <- renderUI({
    bucket_list(
      header = "Drag exclusion keywords between lists:",
      
      add_rank_list("included_exclusion_list", 
                    labels = exclusion_data$included, 
                    input_id = "included_exclusion_keywords", 
                    text = "Exclusion Keywords"),
      
      add_rank_list("removed_exclusion_list", 
                    labels = exclusion_data$removed, 
                    input_id = "removed_exclusion_keywords", 
                    text = "Ignore")
    )
  })
}

# Run the App
shinyApp(ui, server)