#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# PART 3b - ASSIGNING SPECIFICITY-----------------------------------------------
# 
# Summary: this is an RShiny script that assigns a Scificity score for each Filter 
# Keyword or allows the user to assign a 1,2,3, or 4 value to each Keyword
#
# Output: Updated included_keywords_dt with Specificity scores assigned
#///////////////////////////////////////////////////////////////////////////////

library(shiny)
library(data.table)
library(here)

# Define file paths
rds_path <- here("keyword_selection.RData")
mgmt_d_1_path <- here("mgmt_d_1.RData")

# Load mgmt_d_1.RData if available
if (file.exists(mgmt_d_1_path)) {
  load(mgmt_d_1_path)  # Load management data
} else {
  stop("Error: mgmt_d_1.RData not found. Ensure it exists in the expected directory.")
}

# Load keyword selections if available, otherwise initialize
if (file.exists(rds_path)) {
  load(rds_path)  # Load previous keyword selections
} else {
  stop("Error: keyword_selection.RData does not exist. Please run the keyword selection script first.")
}

# Ensure dataset exists before using it
if (!exists("included_keywords_dt", envir = .GlobalEnv)) {
  stop("Error: included_keywords_dt does not exist in the global environment.")
}

# Ensure Specificity column exists and retains values
if (!"Specificity" %in% colnames(included_keywords_dt)) {
  included_keywords_dt[, Specificity := NA_integer_]
}

# Assign Specificity only if missing (prevent overwriting existing values)
included_keywords_dt[, Specificity := fifelse(
  is.na(Specificity) & Keyword %in% salmon_keywords$Keyword, 4, 
  fifelse(is.na(Specificity) & Keyword %in% mgmt_d_keywords$Keyword, 3, Specificity)
)]

# Assign to the global environment for iterative updates
assign("included_keywords_dt", included_keywords_dt, envir = .GlobalEnv)

# Define UI
ui <- fluidPage(
  titlePanel("Assign Specificity to Keywords"),
  
  # Add CSS for alternating row colors
  tags$style(HTML("
    .striped-row:nth-child(odd) { background-color: #f9f9f9; padding: 10px; }
    .striped-row:nth-child(even) { background-color: #e3e3e3; padding: 10px; }
  ")),
  
  tags$div(
    tags$p("Select a Specificity score for each Filter Keyword."),
    tags$ul(
      tags$li("Some keywords have been automatically assigned Specificity based on their matches."),
      tags$li("1 = Pacific salmon most specifically (salmon, chinook, etc.)."),
      tags$li("2 = Fish and parts of fish that include salmon."),
      tags$li("3 = Habitat and external direct influences on fish including salmon."),
      tags$li("4 = Indirect management that can influence salmon.")
    )
  ),
  
  uiOutput("questionnaire_ui"),  # Display keyword list dynamically
  
  actionButton("submit", "Submit Selection"),
  verbatimTextOutput("summary_output")
)

# Define Server Logic
server <- function(input, output, session) {
  
  # Dynamically generate questionnaire-style UI with row shading
  output$questionnaire_ui <- renderUI({
    tagList(
      lapply(1:nrow(included_keywords_dt), function(i) {
        fluidRow(class = "striped-row",
                 column(6, h4(included_keywords_dt$Keyword[i])),  # Display keyword
                 column(6, radioButtons(
                   paste0("specificity_", i), label = "Select Specificity:",
                   choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4),
                   selected = included_keywords_dt$Specificity[i], inline = TRUE
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
    
    # Debugging prints to verify Specificity retention
    print("Before saving: Included Keywords Table")
    print(included_keywords_dt)
    
    # Save the updated table in the global environment
    assign("included_keywords_dt", included_keywords_dt, envir = .GlobalEnv)
    
    # Save changes back to keyword_selection.RData
    save(included_keywords_dt, removed_keywords_dt, file = rds_path)
    
    # Debugging print after saving
    print("After saving and reloading:")
    load(rds_path)
    print(included_keywords_dt)
    
    showNotification("Specificity assigned & saved!", type = "message")
    output$summary_output <- renderPrint({ print(included_keywords_dt) })
    
    stopApp()
  })
}

# Run the Shiny app
shinyApp(ui, server)