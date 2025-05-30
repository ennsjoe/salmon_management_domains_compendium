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

# Load R Object using `here()`
rds_path <- here("mgmt_d_1.RData")

# Check if file exists before loading
if (!file.exists(rds_path)) stop("Error: mgmt_d_1.RData file not found.")

# Load the R object
load(rds_path)

# Ensure required data tables exist
if (!exists("salmon_keywords") || !exists("mgmt_d_keywords") || !exists("Type_A_unigrams_highest_quartile")) {
  stop("Error: One or more required datasets are missing.")
}

# Extract only the `Keyword` column and merge them
merged_keywords <- rbindlist(list(
  salmon_keywords[, .(Keyword)], 
  mgmt_d_keywords[, .(Keyword)], 
  Type_A_unigrams_highest_quartile[, .(Keyword)]
), fill = TRUE)[!is.na(Keyword) & Keyword != ""]

# Clean `Keyword` column
merged_keywords[, Keyword := gsub("\\\\b", "", Keyword)]  # Remove boundary markers
merged_keywords[, Keyword := gsub('["]', "", Keyword)]  # Remove double quotes
merged_keywords[, Keyword := trimws(Keyword)]  # Trim leading/trailing spaces

# Ensure `Keyword` column exists and has values
if (nrow(merged_keywords) == 0) {
  stop("Error: No Keywords found in merged dataset.")
}

# Define UI
ui <- fluidPage(
  titlePanel("Drag-and-Drop Keyword Selection"),
  
  fluidRow(
    bucket_list(
      header = "Drag keywords between lists:",
      
      add_rank_list("included_list", 
                    labels = merged_keywords$Keyword, 
                    input_id = "included_keywords", 
                    text = "Included Keywords"),
      
      add_rank_list("removed_list", 
                    labels = character(0), 
                    input_id = "removed_keywords", 
                    text = "Removed Keywords")
    )
  ),
  
  actionButton("submit", "Finalize Selection"),  # Submit button
  
  verbatimTextOutput("summary_output")  # Display selected keywords
)

# Define Server
server <- function(input, output, session) {
  
  # Observe changes in lists and update reactively
  observe({
    included_keywords <- input$included_keywords
    removed_keywords <- input$removed_keywords
    
    # Save final selections as a data table
    final_data <- data.table(
      Keyword = c(included_keywords, removed_keywords),
      Status = c(rep("Included", length(included_keywords)), rep("Removed", length(removed_keywords)))
    )
    
    output$summary_output <- renderPrint({
      cat("Final Keyword Selection:\n")
      print(final_data)
    })
  })
}

# Run the App
shinyApp(ui, server)