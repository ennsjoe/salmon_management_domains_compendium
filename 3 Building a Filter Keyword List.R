#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# PART 3 - BUILDING A FILTER KEYWORD LIST---------------------------------------
# 
# Summary: this is an RShiny script that walks the user through steps to filter
# IUCN Threat keywords along with words extracted from Type A legislation to build 
# a keyword list that will be used to filter Procedural Elements from the parsed 
# legislation from Part 2
#
# Output: a curated Filter Keyword List with Specificity assigned
#///////////////////////////////////////////////////////////////////////////////


library(shiny)
library(data.table)
library(here)
library(sortable)  # Enables drag-and-drop functionality

# Define file path for saving results
rds_path <- here("keyword_selection.RData")

# Check if keyword_selection.RData exists
if (file.exists(rds_path)) {
  load(rds_path)  # Load previous keyword selections
} else {
  # Load mgmt_d_1.RData and extract keywords if keyword_selection.RData does not exist
  mgmt_d_1_path <- here("mgmt_d_1.RData")
  
  if (file.exists(mgmt_d_1_path)) {
    load(mgmt_d_1_path)  # Load previous management data
    
    # Combine keywords and ensure Specificity stays assigned
    removed_keywords_dt <- rbindlist(list(
      salmon_keywords[, .(Keyword)], 
      mgmt_d_keywords[, .(Keyword)], 
      Type_A_unigrams_highest_quartile[, .(Keyword)]
    ), fill = TRUE)[!is.na(Keyword) & Keyword != ""]
    
    removed_keywords_dt[, Keyword := gsub("\\\\b", "", Keyword)]
    removed_keywords_dt[, Keyword := gsub('["]', "", Keyword)]
    removed_keywords_dt[, Keyword := trimws(Keyword)]
    
    # Ensure Specificity column is properly initialized
    included_keywords_dt <- data.table(Keyword = character(0), Specificity = NA_integer_)
  } else {
    stop("Error: mgmt_d_1.RData not found. Ensure it exists in the expected directory.")
  }
}

# Ensure Specificity values remain assigned across iterations
if (!"Specificity" %in% colnames(included_keywords_dt)) {
  included_keywords_dt[, Specificity := NA_integer_]
}

# Assign to Global Environment for iterative use
assign("included_keywords_dt", included_keywords_dt, envir = .GlobalEnv)
assign("removed_keywords_dt", removed_keywords_dt, envir = .GlobalEnv)

# Define UI
ui <- fluidPage(
  titlePanel("Select Most Specific Salmon Management Keywords"),
  
  tags$div(
    tags$p("Pacific salmon legislation filtering:"),
    tags$ul(
      tags$li("These keywords filter which sections to include."),
      tags$li("Choose words that MUST include salmon and/or habitat."),
      tags$li("The 'removed' words will still be used later to assign IUCN Threat categories."),
      tags$li("The selections will be stored for future iterations."),
      tags$li("Click 'Finalize' button at bottom when finished.")
    )
  ),
  
  fluidRow(
    textInput("new_keyword", "Add a new keyword:", ""),
    actionButton("add_keyword", "Add to Included Keywords")
  ),
  
  uiOutput("keyword_ui"),  # Dynamically rendered UI
  
  actionButton("submit", "Finalize Selection"),
  verbatimTextOutput("summary_output")
)

# Define Server
server <- function(input, output, session) {
  keywords_data <- reactiveValues(
    included = included_keywords_dt$Keyword,
    removed = removed_keywords_dt$Keyword
  )
  
  # Observe adding new keyword
  observeEvent(input$add_keyword, {
    new_word <- trimws(input$new_keyword)
    
    if (nzchar(new_word) && !(new_word %in% keywords_data$included)) {
      keywords_data$included <- c(keywords_data$included, new_word)
      
      included_keywords_dt <- data.table(Keyword = keywords_data$included, Specificity = NA_integer_)
      assign("included_keywords_dt", included_keywords_dt, envir = .GlobalEnv)
      
      updateRankListInput(session, "included_list", labels = keywords_data$included)
    }
  })
  
  # Observe submission and save results
  observeEvent(input$submit, {
    keywords_data$included <- input$included_keywords %||% keywords_data$included
    keywords_data$removed <- input$removed_keywords %||% keywords_data$removed
    
    # Ensure Specificity values remain when saving
    included_keywords_dt <- data.table(
      Keyword = keywords_data$included, 
      Specificity = included_keywords_dt[Keyword %in% keywords_data$included, Specificity]
    )
    
    removed_keywords_dt <- data.table(Keyword = keywords_data$removed)
    
    assign("included_keywords_dt", included_keywords_dt, envir = .GlobalEnv)
    assign("removed_keywords_dt", removed_keywords_dt, envir = .GlobalEnv)
    
    print("Saving keyword selections...")  # Debugging print
    print(included_keywords_dt)
    print(removed_keywords_dt)
    
    if (length(keywords_data$included) == 0 && length(keywords_data$removed) == 0) {
      showNotification("Error: No keywords found to save.", type = "error")
    } else {
      tryCatch(
        {
          save(included_keywords_dt, removed_keywords_dt, file = rds_path)
          flush.console()  # Ensure save completes before closing
          print("Save successful!")
          Sys.sleep(1)  # Small delay before closing
          stopApp()  # Close app after successful save
        },
        error = function(e) {
          showNotification(paste("Save Error:", e$message), type = "error")
          print(paste("Save Error:", e$message))  # Debugging output
        }
      )
    }
  })
  
  # Initial render of the keyword UI
  output$keyword_ui <- renderUI({
    bucket_list(
      header = "Drag keywords between lists:",
      
      add_rank_list("included_list", 
                    labels = keywords_data$included, 
                    input_id = "included_keywords", 
                    text = "Included Keywords"),
      
      add_rank_list("removed_list", 
                    labels = keywords_data$removed, 
                    input_id = "removed_keywords", 
                    text = "Removed Keywords")
    )
  })
}

# Run the App
shinyApp(ui, server)