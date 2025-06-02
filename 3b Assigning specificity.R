#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# PART 3b - BUILDING A FILTER KEYWORD LIST---------------------------------------
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

# Define file path for saved keyword selections
rds_path <- here("keyword_selection.RData")

# Check if keyword_selection.RData exists before loading
if (file.exists(rds_path)) {
  load(rds_path)  # Load previous keyword selections
} else {
  stop("Error: keyword_selection.RData does not exist. Please run the keyword selection script first.")
}

# Ensure the dataset exists before using it
if (!exists("included_keywords_dt", envir = .GlobalEnv)) {
  stop("Error: included_keywords_dt does not exist in the global environment.")
}

# Add a blank Specificity column for numeric values
included_keywords_dt[, Specificity := NA_integer_]

# Assign it to the global environment for iterative updates
assign("included_keywords_dt", included_keywords_dt, envir = .GlobalEnv)

# Define UI
ui <- fluidPage(
  titlePanel("Assign Specificity to Keywords"),
  
  tags$div(
    tags$p("Select a Specificity score for each keyword."),
    tags$ul(
      tags$li("Scores range from 1 (low specificity) to 4 (high specificity)."),
      tags$li("Make selections carefully, as they will be saved for further analysis."),
      tags$li("Click 'Submit Selection' when finished.")
    )
  ),
  
  uiOutput("questionnaire_ui"),  # Display keyword list dynamically
  
  actionButton("submit", "Submit Selection"),
  verbatimTextOutput("summary_output")
)

# Define Server Logic
server <- function(input, output, session) {
  
  # Dynamically generate questionnaire-style UI
  output$questionnaire_ui <- renderUI({
    tagList(
      lapply(1:nrow(included_keywords_dt), function(i) {
        fluidRow(
          column(6, h4(included_keywords_dt$Keyword[i])),  # Display keyword
          column(6, radioButtons(
            paste0("specificity_", i), label = "Select Specificity:",
            choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4),
            selected = 1, inline = TRUE
          ))
        )
      })
    )
  })
  
  # Capture user selections & ensure each keyword retains its chosen value
  observeEvent(input$submit, {
    for (i in 1:nrow(included_keywords_dt)) {
      included_keywords_dt[i, Specificity := as.integer(input[[paste0("specificity_", i)]])]
    }
    
    # Save the updated table in the global environment
    assign("included_keywords_dt", included_keywords_dt, envir = .GlobalEnv)
    
    # Save changes back to keyword_selection.RData
    save(included_keywords_dt, removed_keywords_dt, file = rds_path)
    
    showNotification("Specificity assigned & saved!", type = "message")
    output$summary_output <- renderPrint({ print(included_keywords_dt) })
    
    stopApp()
  })
}

# Run the Shiny app
shinyApp(ui, server)