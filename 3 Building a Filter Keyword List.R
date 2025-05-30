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

# Load previous selections if available, else start fresh
if (file.exists(rds_path)) {
  load(rds_path)
} else {
  merged_keywords <- rbindlist(list(
    salmon_keywords[, .(Keyword)], 
    mgmt_d_keywords[, .(Keyword)], 
    Type_A_unigrams_highest_quartile[, .(Keyword)]
  ), fill = TRUE)[!is.na(Keyword) & Keyword != ""]
  
  merged_keywords[, Keyword := gsub("\\\\b", "", Keyword)]
  merged_keywords[, Keyword := gsub('["]', "", Keyword)]
  merged_keywords[, Keyword := trimws(Keyword)]
  
  included_keywords <- merged_keywords$Keyword
  removed_keywords <- character(0)
}

# Define UI
ui <- fluidPage(
  titlePanel("Refining Keyword Selection Iteratively"),
  
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
    included = included_keywords,
    removed = removed_keywords
  )
  
  # Observe adding new keyword
  observeEvent(input$add_keyword, {
    new_word <- trimws(input$new_keyword)
    
    if (new_word != "" && !(new_word %in% keywords_data$included)) {
      keywords_data$included <- c(keywords_data$included, new_word)
      
      # Dynamically re-render UI instead of using update functions
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
  })
  
  # Observe submission and save results
  observeEvent(input$submit, {
    keywords_data$included <- input$included_keywords
    keywords_data$removed <- input$removed_keywords
    
    save(keywords_data$included, keywords_data$removed, file = rds_path)
    
    # Display final keyword selection
    output$summary_output <- renderPrint({
      final_data <- data.table(
        Keyword = c(keywords_data$included, keywords_data$removed),
        Status = c(rep("Included", length(keywords_data$included)), rep("Removed", length(keywords_data$removed)))
      )
      print(final_data)
    })
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