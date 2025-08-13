# app.R

library(shiny)
library(here)
library(ggplot2)
library(dplyr)
library(summarytools)
library(DT)

# Load the data
data <- readRDS(here("Full_legislation_compendium.rds")) #|>

legislation <- data$Full_legislation_parsed_DT |>
  select(`Act Name`, `Legislation Name`, Jurisdiction, `Clause_Type`, Scope, L1, L2, `Management Domain`)

# Define UI
ui <- fluidPage(
  titlePanel("Pacific Salmon Management Domain Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("leg_type", "Select Act:", 
                  choices = unique(legislation$`Act Name`), 
                  selected = unique(legislation$`Act Name`)[1]),
      checkboxGroupInput("fields", "Select Fields to Summarize:", 
                         choices = names(legislation)[!names(legislation) %in% c("Act Name")],
                         selected = c("Jurisdiction", "clause Type"))
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
    df <- legislation
    selected <- input$leg_type
    df_filtered <- df[df$`Act Name` == selected, ]
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
