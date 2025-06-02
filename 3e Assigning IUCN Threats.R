#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# PART 3e - Assigning IUCN Threats----------------------------------------------
# 
# Summary: this is an RShiny script 
#
# Output: 
#///////////////////////////////////////////////////////////////////////////////

library(shiny)
library(data.table)
library(here)

# Define file paths
md_csv_path <- here("Management Domain Threats and Keywords.csv")
md_rds_path <- here("management_domain_selection.RData")

# Load Management Domain Keywords CSV
if (file.exists(md_csv_path)) {
  md_keywords_dt <- fread(md_csv_path)
  print("Loaded: Management Domain Threats and Keywords.csv")
} else {
  stop("Error: Management Domain Threats and Keywords.csv not found.")
}

# Ensure required columns exist
required_cols <- c("Keyword", "Management Domain", "L1", "L2", "Specificity")
missing_cols <- setdiff(required_cols, colnames(md_keywords_dt))
if (length(missing_cols) > 0) {
  stop(paste("Error: Missing columns:", paste(missing_cols, collapse=", ")))
}

# Load previous selections if available
if (file.exists(md_rds_path)) {
  load(md_rds_path)  # Load previous domain selections
  print("Loaded: management_domain_selection.RData")
} else {
  # Initialize selection data with pre-assigned values
  md_selection_dt <- copy(md_keywords_dt)
  md_selection_dt[, Assigned_Domain := `Management Domain`]
  md_selection_dt[, Assigned_L1 := L1]
  md_selection_dt[, Assigned_L2 := L2]
  
  # Ensure Assigned_Specificity starts as character before numeric conversion
  md_selection_dt[, Assigned_Specificity := as.character(Specificity)]
}

# Assign to global environment for iterative updates
assign("md_selection_dt", md_selection_dt, envir = .GlobalEnv)

# Define UI
ui <- fluidPage(
  titlePanel("Assign Management Domains to Keywords"),
  
  tags$div(
    tags$p("Select values for each Keyword or add a new one."),
    tags$ul(
      tags$li("Domains are sourced from the CSV file."),
      tags$li("Your selections will be saved for future iterations."),
      tags$li("Click 'Submit' when finished.")
    )
  ),
  
  # Input field to add a new keyword
  textInput("new_keyword", "Enter new keyword:", ""),
  actionButton("add_keyword", "Add Keyword"),
  
  uiOutput("questionnaire_ui"),  # Display keyword list dynamically
  
  actionButton("submit", "Submit Selection"),
  verbatimTextOutput("summary_output")
)

# Define Server Logic
server <- function(input, output, session) {
  
  # Reactive data table to dynamically update keywords list
  md_selection_dt_reactive <- reactiveVal(md_selection_dt)
  
  # Add a new keyword to the dataset
  observeEvent(input$add_keyword, {
    new_word <- trimws(input$new_keyword)
    if (new_word != "" && !(new_word %in% md_selection_dt_reactive()$Keyword)) {
      new_row <- data.table(
        Keyword = new_word,
        `Management Domain` = NA_character_,
        L1 = NA_character_,
        L2 = NA_character_,
        Specificity = NA_character_,
        Assigned_Domain = NA_character_,
        Assigned_L1 = NA_character_,
        Assigned_L2 = NA_character_,
        Assigned_Specificity = NA_character_
      )
      md_selection_dt_reactive(rbind(md_selection_dt_reactive(), new_row))
      updateTextInput(session, "new_keyword", value = "")
    }
  })
  
  # Dynamically generate UI with dropdowns for selection and alternating row shading
  output$questionnaire_ui <- renderUI({
    tagList(
      lapply(1:nrow(md_selection_dt_reactive()), function(i) {
        row_style <- ifelse(i %% 2 == 0, "background-color:#f2f2f2;", "background-color:#ffffff;")
        div(style = row_style,
            fluidRow(
              column(4, h4(md_selection_dt_reactive()$Keyword[i]), 
                     style = "overflow-x: auto; white-space: nowrap;"),
              column(2, selectInput(
                paste0("domain_", i), label = "Select Management Domain:",
                choices = unique(md_selection_dt_reactive()$`Management Domain`),
                selected = md_selection_dt_reactive()$Assigned_Domain[i]
              ), style = "min-width: 150px; max-width: 250px;"),
              column(2, selectInput(
                paste0("l1_", i), label = "Select L1:",
                choices = unique(md_selection_dt_reactive()$L1),
                selected = md_selection_dt_reactive()$Assigned_L1[i]
              ), style = "min-width: 150px; max-width: 250px;"),
              column(2, selectInput(
                paste0("l2_", i), label = "Select L2:",
                choices = unique(md_selection_dt_reactive()$L2),
                selected = md_selection_dt_reactive()$Assigned_L2[i]
              ), style = "min-width: 150px; max-width: 250px;"),
              column(2, selectInput(
                paste0("specificity_", i), label = "Select Specificity:",
                choices = unique(md_selection_dt_reactive()$Specificity),
                selected = md_selection_dt_reactive()$Assigned_Specificity[i]
              ), style = "min-width: 150px; max-width: 250px;")
            )
        )
      })
    )
  })
  
  # Capture user selections & update assigned values
  observeEvent(input$submit, {
    dt <- md_selection_dt_reactive()
    for (i in 1:nrow(dt)) {
      dt[i, Assigned_Domain := input[[paste0("domain_", i)]]]
      dt[i, Assigned_L1 := input[[paste0("l1_", i)]]]
      dt[i, Assigned_L2 := input[[paste0("l2_", i)]]]
      
      # Convert Assigned_Specificity to integer before storing
      dt[i, Assigned_Specificity := as.integer(input[[paste0("specificity_", i)]])]
    }
    
    # Debugging print before saving
    print("Before saving:")
    print(dt)
    
    # Save the updated table in the global environment
    assign("md_selection_dt", dt, envir = .GlobalEnv)
    
    # Save changes back to management_domain_selection.RData
    save(dt, file = md_rds_path)
    
    # Debugging print after saving
    print("After saving:")
    load(md_rds_path)
    print(md_selection_dt)
    
    showNotification("Management Domains assigned & saved!", type = "message")
    output$summary_output <- renderPrint({ print(dt) })
    
    stopApp()  # Close app after finalization
  })
}

# Run the Shiny app
shinyApp(ui, server)