# app.R

library(shiny)
library(here)
library(ggplot2)
library(dplyr)
library(summarytools)
library(DT)

# Load the data
data <- readRDS(here("Full_legislation_parsed_compendium.rds")) |>
  mutate(across(c("jurisdiction", "legislation_type"), as.character)) |>
  select(act_name, legislation_name, jurisdiction, clause_type, specificity, l1, l2, management_domain)

# Define UI
ui <- fluidPage(
  titlePanel("Pacific Salmon Management Domain Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("leg_type", "Select Act:", 
                  choices = unique(data$act_name), 
                  selected = unique(data$act_name)[1]),
      checkboxGroupInput("fields", "Select Fields to Summarize:", 
                         choices = names(data)[!names(data) %in% c("act_name")],
                         selected = c("jurisdiction", "clause_type"))
    ),
    
    mainPanel(
      h3("Summary Table"),
      htmlOutput("summary_table"),
      #h3("Bar Charts"),
      #uiOutput("plots_ui")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  options(summarytools.view.method = "browser", summarytools.use.viewer = FALSE)
  
  filtered_data <- reactive({
    df <- data
    selected <- input$leg_type
    df_filtered <- df[df$act_name == selected, ]
    print(paste("Selected:", selected, "Rows:", nrow(df_filtered)))
    df_filtered
  })
  
  
output$summary_table <- renderUI({
  req(input$fields)
  df <- filtered_data()
  df_subset <- select(df, input$fields)
  #df_subset[] <- lapply(df_subset, as.character) # Convert all columns to character
  # stview <- dfSummary(df_subset,
  #                     plain.ascii = FALSE,
  #                     style = "grid",
  #                     varnumbers = FALSE,
  #                     valid.col = FALSE)
  print(dfSummary(df,
                       varnumbers = FALSE,
                       valid.col = FALSE),
             method = "render",
             headings = FALSE,
             bootstrap.css = FALSE)
})

  
  # output$plots_ui <- renderUI({
  #   req(input$fields)
  #   plot_output_list <- lapply(input$fields, function(field) {
  #     plotname <- paste0("plot_", field)
  #     plotOutput(plotname)
  #   })
  #   do.call(tagList, plot_output_list)
  # })
  # 
  # observe({
  #   req(input$fields)
  #   for (field in input$fields) {
  #     local({
  #       f <- field
  #       output[[paste0("plot_", f)]] <- renderPlot({
  #         df <- filtered_data()
  #         ggplot(df, aes_string(x = f)) +
  #           geom_bar(fill = "steelblue") +
  #           theme_minimal() +
  #           labs(title = paste("Frequency of", f), x = f, y = "Count") +
  #           theme(axis.text.x = element_text(angle = 45, hjust = 1))
  #       })
  #     })
  #   }
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)
