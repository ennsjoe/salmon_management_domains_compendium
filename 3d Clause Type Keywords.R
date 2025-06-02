#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# PART 3d - CLAUSE TYPE KEYWORDS------------------------------------------------
# 
# Summary: this is an RShiny script 
#
# Output: 
#///////////////////////////////////////////////////////////////////////////////

library(shiny)
library(data.table)
library(here)

# Define file paths
clause_type_csv_path <- here("Clause Type Keywords.csv")
clause_type_rds_path <- here("clause_type_selection.RData")

# Load Clause Type Keywords CSV
if (file.exists(clause_type_csv_path)) {
  clause_keywords_dt <- fread(clause_type_csv_path)
  print("Loaded: Clause Type Keywords.csv")
} else {
  stop("Error: Clause Type Keywords.csv not found.")
}

# Ensure Clause_Type column exists
if (!"Clause_Type" %in% colnames(clause_keywords_dt)) {
  stop("Error: Clause_Type column missing from Clause_Type_Keywords.csv.")
}

# Load previous selections if available
if (file.exists(clause_type_rds_path)) {
  load(clause_type_rds_path)  # Load previous clause type selections
  print("Loaded: clause_type_selection.RData")
} else {
  # Initialize selection data if no previous file exists
  clause_selection_dt <- copy(clause_keywords_dt)
  clause_selection_dt[, Assigned_Type := NA_character_]
}

# Assign to global environment for iterative updates
assign("clause_selection_dt", clause_selection_dt, envir = .GlobalEnv)

# Define UI
ui <- fluidPage(
  titlePanel("Assign Clause Types to Keywords"),
  
  tags$div(
    tags$p("Select a Clause Type for each Keyword."),
    tags$ul(
      tags$li("Clause types are sourced from the CSV file."),
      tags$li("Your selections will be saved for future iterations."),
      tags$li("Click 'Submit' when finished.")
    )
  ),
  
  uiOutput("questionnaire_ui"),  # Display keyword list dynamically
  
  actionButton("submit", "Submit Selection"),
  verbatimTextOutput("summary_output")
)

# Define Server Logic
server <- function(input, output, session) {
  
  # Dynamically generate UI with dropdowns for Clause Type selection
  output$questionnaire_ui <- renderUI({
    tagList(
      lapply(1:nrow(clause_selection_dt), function(i) {
        fluidRow(
          column(6, h4(clause_selection_dt$Keyword[i])),  # Display keyword
          column(6, selectInput(
            paste0("clause_type_", i), label = "Select Clause Type:",
            choices = unique(clause_selection_dt$Clause_Type),
            selected = clause_selection_dt$Assigned_Type[i]
          ))
        )
      })
    )
  })
  
  # Capture user selections & update Assigned_Type
  observeEvent(input$submit, {
    for (i in 1:nrow(clause_selection_dt)) {
      clause_selection_dt[i, Assigned_Type := input[[paste0("clause_type_", i)]]]
    }
    
    # Debugging print before saving
    print("Before saving:")
    print(clause_selection_dt)
    
    # Save the updated table in the global environment
    assign("clause_selection_dt", clause_selection_dt, envir = .GlobalEnv)
    
    # Save changes back to clause_type_selection.RData
    save(clause_selection_dt, file = clause_type_rds_path)
    
    # Debugging print after saving
    print("After saving:")
    load(clause_type_rds_path)
    print(clause_selection_dt)
    
    showNotification("Clause Types assigned & saved!", type = "message")
    output$summary_output <- renderPrint({ print(clause_selection_dt) })
    
    stopApp()  # Close app after finalization
  })
}

# Run the Shiny app
shinyApp(ui, server)