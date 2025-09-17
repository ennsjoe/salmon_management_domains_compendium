library(shiny)
library(DBI)
library(RSQLite)

# Connect to the SQLite database
db_path <- "legislation.db"
conn <- dbConnect(SQLite(), db_path)

# Load data
label_data <- dbReadTable(conn, "paragraph_label_table")
legislation_data <- dbReadTable(conn, "LegislationMetadata")
paragraph_data <- dbReadTable(conn, "LegislationParagraphs")

management_domains_all <- unique(label_data$label_value[label_data$label_type == "Management Domain"])
jurisdictions <- unique(legislation_data$jurisdiction)

# UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app_style.css")
  ),
  
  titlePanel("LAPSE Dashboard"),
  
  fluidRow(
    column(
      width = 3,
      div(
        class = "domain-panel",
        h4("Management Domains"),
        actionButton("reset_filters", "Show All", class = "domain-button"),
        uiOutput("domain_buttons")
      )
    ),
    
    column(
      width = 9,
      div(
        class = "main-panel",
        h4("Acts Filtered by Domain and Jurisdiction"),
        selectInput("jurisdiction_filter", "Jurisdiction", choices = c("All", jurisdictions)),
        uiOutput("act_buttons"),
        hr(),
        h4("Legislation Filtered by Act"),
        uiOutput("legislation_buttons"),
        hr(),
        h4("Sections Filtered by Legislation and Domain"),
        uiOutput("section_buttons")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  selected_domain <- reactiveVal(NULL)
  selected_act <- reactiveVal(NULL)
  selected_legislation <- reactiveVal(NULL)
  
  observeEvent(input$reset_filters, {
    selected_domain(NULL)
    selected_act(NULL)
    selected_legislation(NULL)
  })
  
  filtered_legislation <- reactive({
    data <- legislation_data
    if (input$jurisdiction_filter != "All") {
      data <- data[data$jurisdiction == input$jurisdiction_filter, ]
    }
    
    if (!is.null(selected_domain())) {
      domain_paragraphs <- label_data$paragraph_id[
        label_data$label_type == "Management Domain" &
          label_data$label_value == selected_domain()
      ]
      if (length(domain_paragraphs) > 0) {
        query <- sprintf(
          "SELECT DISTINCT legislation_id FROM LegislationParagraphs WHERE paragraph_id IN (%s)",
          paste(domain_paragraphs, collapse = ",")
        )
        leg_ids <- dbGetQuery(conn, query)$legislation_id
        data <- data[data$legislation_id %in% leg_ids, ]
      } else {
        data <- data[0, ]
      }
    }
    
    if (!is.null(selected_act())) {
      data <- data[data$act_name == selected_act(), ]
    }
    
    data
  })
  
  filtered_domains <- reactive({
    if (is.null(selected_act())) return(management_domains_all)
    
    leg_ids <- legislation_data$legislation_id[legislation_data$act_name == selected_act()]
    if (length(leg_ids) == 0) return(character(0))
    
    query <- sprintf(
      "SELECT DISTINCT paragraph_id FROM LegislationParagraphs WHERE legislation_id IN (%s)",
      paste(leg_ids, collapse = ",")
    )
    paragraph_ids <- dbGetQuery(conn, query)$paragraph_id
    
    domains <- unique(label_data$label_value[
      label_data$label_type == "Management Domain" &
        label_data$paragraph_id %in% paragraph_ids
    ])
    domains
  })
  
  output$domain_buttons <- renderUI({
    domains <- filtered_domains()
    if (length(domains) == 0) return(div("No domains match the selected act."))
    
    tagList(
      lapply(seq_along(domains), function(i) {
        domain <- domains[i]
        btn_id <- paste0("domain_", i)
        observeEvent(input[[btn_id]], {
          selected_domain(domain)
          selected_act(NULL)
          selected_legislation(NULL)
        }, ignoreInit = TRUE)
        
        actionButton(
          inputId = btn_id,
          label = domain,
          class = "domain-button"
        )
      })
    )
  })
  
  output$act_buttons <- renderUI({
    acts <- unique(filtered_legislation()$act_name)
    if (length(acts) == 0) return(div("No acts match the selected filters."))
    
    tagList(
      lapply(seq_along(acts), function(i) {
        act <- acts[i]
        btn_id <- paste0("act_", i)
        observeEvent(input[[btn_id]], {
          selected_act(act)
          selected_domain(NULL)
          selected_legislation(NULL)
        }, ignoreInit = TRUE)
        
        actionButton(
          inputId = btn_id,
          label = act,
          class = "act-button"
        )
      })
    )
  })
  
  output$legislation_buttons <- renderUI({
    legislation <- unique(filtered_legislation()$legislation_name)
    if (length(legislation) == 0) return(div("No legislation found."))
    
    tagList(
      lapply(seq_along(legislation), function(i) {
        name <- legislation[i]
        btn_id <- paste0("leg_", i)
        observeEvent(input[[btn_id]], {
          selected_legislation(name)
        }, ignoreInit = TRUE)
        
        actionButton(
          inputId = btn_id,
          label = name,
          class = "legislation-button"
        )
      })
    )
  })
  
  output$section_buttons <- renderUI({
    leg_name <- selected_legislation()
    domain <- selected_domain()
    
    if (is.null(leg_name)) return(div("Select legislation to view sections."))
    
    leg_id <- legislation_data$legislation_id[legislation_data$legislation_name == leg_name]
    
    domain_paragraphs <- if (!is.null(domain)) {
      label_data$paragraph_id[
        label_data$label_type == "Management Domain" &
          label_data$label_value == domain
      ]
    } else {
      unique(paragraph_data$paragraph_id)
    }
    
    filtered <- paragraph_data[
      paragraph_data$legislation_id == leg_id &
        paragraph_data$paragraph_id %in% domain_paragraphs,
    ]
    
    section_labels <- unique(paste(filtered$Section, filtered$Heading, sep = " | "))
    
    if (length(section_labels) == 0) return(div("No sections match the selected filters."))
    
    tagList(
      lapply(seq_along(section_labels), function(i) {
        label <- section_labels[i]
        btn_id <- paste0("section_", i)
        
        observeEvent(input[[btn_id]], {
          parts <- strsplit(label, " \\| ")[[1]]
          section <- parts[1]
          heading <- parts[2]
          
          matching <- filtered[
            filtered$Section == section & filtered$Heading == heading,
          ]
          
          paragraph_text <- if (nrow(matching) > 0) {
            paste(matching$Paragraph, collapse = "\n\n")
          } else {
            "No paragraphs found."
          }
          
          if (!is.null(domain)) {
            keywords <- unique(label_data$keyword[
              label_data$label_type == "Management Domain" &
                label_data$label_value == domain
            ])
            for (kw in keywords) {
              if (!is.na(kw) && nzchar(kw)) {
                pattern <- paste0("\\b", kw, "\\b")
                paragraph_text <- gsub(
                  pattern,
                  paste0("<span class='highlight'>", kw, "</span>"),
                  paragraph_text,
                  ignore.case = TRUE
                )
              }
            }
          }
          
          showModal(modalDialog(
            title = paste("Section:", section, "| Heading:", heading),
            div(style = "white-space: pre-wrap; max-height: 400px; overflow-y: auto;", HTML(paragraph_text)),
            easyClose = TRUE,
            size = "l"
          ))
        }, ignoreInit = TRUE)
        
        actionButton(
          inputId = btn_id,
          label = label,
          class = "section-button"
        )
      })
    )
  })
  
  onStop(function() {
    dbDisconnect(conn)
  })
}

shinyApp(ui = ui, server = server)
