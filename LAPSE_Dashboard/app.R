library(shiny)
library(DBI)
library(RSQLite)
library(DT)

# Connect to the SQLite database (assumed to be in the app folder)
db_path <- "legislation.db"
conn <- dbConnect(SQLite(), db_path)

# Load filter options
label_data <- dbReadTable(conn, "paragraph_label_table")
legislation_data <- dbReadTable(conn, "LegislationMetadata")

management_domains <- unique(label_data$label_value[label_data$label_type == "Management Domain"])
iucn_threats <- unique(label_data$label_value[label_data$label_type == "IUCN"])
clause_types <- unique(label_data$label_value[label_data$label_type == "Clause Type"])
legislation_names <- unique(legislation_data$legislation_name)
act_names <- unique(legislation_data$act_name)
jurisdictions <- unique(legislation_data$jurisdiction)

# UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app_style.css")
  ),
  
  titlePanel("LAPSE Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      selectInput("domain", "Management Domain", choices = c("All", management_domains)),
      selectInput("iucn", "IUCN Threat", choices = c("All", iucn_threats)),
      selectInput("clause", "Clause Type", choices = c("All", clause_types)),
      selectInput("leg_name", "Legislation Name", choices = c("All", legislation_names)),
      selectInput("act_name", "Act Name", choices = c("All", act_names)),
      selectInput("jurisdiction", "Jurisdiction", choices = c("All", jurisdictions))
    ),
    
    mainPanel(
      class = "main-panel",
      dataTableOutput("filtered_table")
    )
  )
)

# Server
server <- function(input, output, session) {
  output$filtered_table <- renderDataTable({
    query <- "
      SELECT p.paragraph_id, p.Paragraph, l.legislation_name, l.act_name, l.jurisdiction, pl.label_type, pl.label_value
      FROM LegislationParagraphs p
      JOIN LegislationMetadata l ON p.legislation_id = l.legislation_id
      JOIN paragraph_label_table pl ON p.paragraph_id = pl.paragraph_id
      WHERE 1=1
    "
    
    filters <- list()
    if (input$domain != "All") filters <- c(filters, sprintf("pl.label_type = 'Management Domain' AND pl.label_value = '%s'", input$domain))
    if (input$iucn != "All") filters <- c(filters, sprintf("pl.label_type = 'IUCN' AND pl.label_value = '%s'", input$iucn))
    if (input$clause != "All") filters <- c(filters, sprintf("pl.label_type = 'Clause Type' AND pl.label_value = '%s'", input$clause))
    if (input$leg_name != "All") filters <- c(filters, sprintf("l.legislation_name = '%s'", input$leg_name))
    if (input$act_name != "All") filters <- c(filters, sprintf("l.act_name = '%s'", input$act_name))
    if (input$jurisdiction != "All") filters <- c(filters, sprintf("l.jurisdiction = '%s'", input$jurisdiction))
    
    if (length(filters) > 0) {
      query <- paste(query, "AND", paste(filters, collapse = " AND "))
    }
    
    result <- dbGetQuery(conn, query)
    datatable(result, options = list(pageLength = 10))
  })
  
  # Disconnect from the database when the app stops
  onStop(function() {
    dbDisconnect(conn)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
