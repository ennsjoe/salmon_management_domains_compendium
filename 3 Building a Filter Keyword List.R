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

# Check if previous selections exist
if (file.exists(rds_path)) {
  load(rds_path)
} else {
  mgmt_d_1_path <- here("mgmt_d_1.RData")
  
  if (file.exists(mgmt_d_1_path)) {
    load(mgmt_d_1_path)
    
    removed_keywords_dt <- rbindlist(list(
      salmon_keywords[, .(Keyword)], 
      mgmt_d_keywords[, .(Keyword)], 
      Type_A_unigrams_highest_quartile[, .(Keyword)]
    ), fill = TRUE)[!is.na(Keyword) & Keyword != ""]
    
    removed_keywords_dt[, Keyword := gsub("\\\\b", "", Keyword)]
    removed_keywords_dt[, Keyword := gsub('["]', "", Keyword)]
    removed_keywords_dt[, Keyword := trimws(Keyword)]
    
    included_keywords_dt <- data.table(Keyword = character(0), Specificity = NA_integer_)
  } else {
    stop("Error: mgmt_d_1.RData not found.")
  }
}

if (!"Specificity" %in% colnames(included_keywords_dt)) {
  included_keywords_dt[, Specificity := NA_integer_]
}

assign("included_keywords_dt", included_keywords_dt, envir = .GlobalEnv)
assign("removed_keywords_dt", removed_keywords_dt, envir = .GlobalEnv)

# Define UI
ui <- fluidPage(
  titlePanel("Select Most Specific Salmon Management Keywords"),
  
  tags$div(
    tags$p("Pacific salmon legislation filtering:"),
    tags$ul(
      tags$li("Choose words that MUST include salmon and/or habitat."),
      tags$li("Removed words will still be used to assign IUCN Threat categories."),
      tags$li("Selections will be stored for future iterations."),
      tags$li("Click 'Finalize' when finished.")
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
  
  observeEvent(input$add_keyword, {
    new_word <- trimws(input$new_keyword)
    
    if (nzchar(new_word) && !(new_word %in% keywords_data$included)) {
      keywords_data$included <- c(keywords_data$included, new_word)
      
      included_keywords_dt <- data.table(Keyword = keywords_data$included, Specificity = NA_integer_)
      assign("included_keywords_dt", included_keywords_dt, envir = .GlobalEnv)
      
      # Re-render UI dynamically to update displayed keywords
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
      
      # Clear input field after adding the word
      updateTextInput(session, "new_keyword", value = "")
    }
  })
  
  observeEvent(input$submit, {
    keywords_data$included <- input$included_keywords %||% keywords_data$included
    keywords_data$removed <- input$removed_keywords %||% keywords_data$removed
    
    included_keywords_dt <- data.table(
      Keyword = keywords_data$included, 
      Specificity = included_keywords_dt[Keyword %in% keywords_data$included, Specificity]
    )
    
    removed_keywords_dt <- data.table(Keyword = keywords_data$removed)
    
    assign("included_keywords_dt", included_keywords_dt, envir = .GlobalEnv)
    assign("removed_keywords_dt", removed_keywords_dt, envir = .GlobalEnv)
    
    tryCatch({
      save(included_keywords_dt, removed_keywords_dt, file = rds_path)
      flush.console()
      print("Save successful!")
      Sys.sleep(1)
      stopApp()
    }, error = function(e) {
      showNotification(paste("Save Error:", e$message), type = "error")
      print(paste("Save Error:", e$message))
    })
  })
  
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