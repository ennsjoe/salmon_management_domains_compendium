library(shiny)
library(DBI)
library(RSQLite)
library(ggplot2)
library(here)

db_path <- here("output", "legislation.db")
conn <- dbConnect(RSQLite::SQLite(), dbname = db_path)

# Disconnect when app stops
onStop(function() {
  dbDisconnect(conn)
})

# Load data
label_data <- dbReadTable(conn, "paragraph_label_table")
legislation_data <- dbReadTable(conn, "LegislationMetadata")
paragraph_data <- dbReadTable(conn, "LegislationParagraphs")
clause_data <- dbReadTable(conn, "clause_type_keywords")

# Prepare UI choices
management_domains <- unique(label_data$label_value[label_data$label_type == "Management Domain"])
jurisdictions <- unique(legislation_data$jurisdiction)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app_style.css")
  ),
  
  titlePanel("LAPSE Dashboard"),
  
  fluidRow(
    # Sidebar: Management Domains
    column(
      width = 2,
      div(class = "domain-panel",
          h4("Management Domains"),
          uiOutput("domain_buttons")
      )
    ),
    
    # Main Panel: Filters and Section Navigation
    column(
      width = 6,
      div(class = "main-panel",
          h4("Filter by Jurisdiction"),
          selectInput("jurisdiction_filter", "Jurisdiction", choices = c("All", jurisdictions)),
          hr(),
          
          h4("Acts Matching Filters"),
          uiOutput("act_buttons"),
          hr(),
          
          h4("Legislation Matching Acts"),
          uiOutput("legislation_buttons"),
          hr(),
          
          h4("Sections Matching Selection"),
          uiOutput("section_buttons")
      )
    ),
    
    # Graph Panel: Visualizations
    column(
      width = 4,
      div(class = "graph-panel",
          h4("IUCN Threats"),
          plotOutput("iucn_plot", height = "200px"),
          hr(),
          
          h4("Clause Type Distribution"),
          plotOutput("clause_plot", height = "200px"),
          hr(),
          
          h4("Section Counts by IUCN"),
          plotOutput("tornado_plot", height = "200px"),
          hr(),
          
          h4("Keyword Frequency"),
          plotOutput("keyword_plot", height = "200px")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  selected_domain <- reactiveVal(NULL)
  selected_act <- reactiveVal(NULL)
  selected_legislation <- reactiveVal(NULL)
  
  # Render domain buttons
  output$domain_buttons <- renderUI({
    selected <- selected_domain()
    if (is.null(selected)) {
      lapply(seq_along(management_domains), function(i) {
        domain <- management_domains[i]
        actionButton(inputId = paste0("domain_", i), label = domain, class = "domain-button")
      })
    } else {
      actionButton(inputId = "reset_domain", label = selected, class = "domain-button")
    }
  })
  
  # Observe domain selection
  observe({
    lapply(seq_along(management_domains), function(i) {
      observeEvent(input[[paste0("domain_", i)]], {
        selected_domain(management_domains[i])
        selected_act(NULL)
        selected_legislation(NULL)
      }, ignoreInit = TRUE)
    })
  })
  
  observeEvent(input$reset_domain, {
    selected_domain(NULL)
    selected_act(NULL)
    selected_legislation(NULL)
  })
  
  # Reactive: Filter legislation
  filtered_legislation <- reactive({
    data <- legislation_data
    if (input$jurisdiction_filter != "All") {
      data <- subset(data, jurisdiction == input$jurisdiction_filter)
    }
    
    if (!is.null(selected_domain())) {
      domain_paragraphs <- label_data$paragraph_id[
        label_data$label_type == "Management Domain" &
          label_data$label_value == selected_domain()
      ]
      leg_ids <- unique(paragraph_data$legislation_id[
        paragraph_data$paragraph_id %in% domain_paragraphs
      ])
      data <- subset(data, legislation_id %in% leg_ids)
    }
    
    if (!is.null(selected_act())) {
      data <- subset(data, act_name == selected_act())
    }
    
    data
  })
  
  # Render act buttons
  output$act_buttons <- renderUI({
    acts <- unique(filtered_legislation()$act_name)
    if (length(acts) == 0) return(div("No acts match the selected filters."))
    lapply(seq_along(acts), function(i) {
      actionButton(inputId = paste0("act_", i), label = acts[i], class = "act-button")
    })
  })
  
  # Observe act selection
  observe({
    acts <- unique(legislation_data$act_name)
    lapply(seq_along(acts), function(i) {
      observeEvent(input[[paste0("act_", i)]], {
        selected_act(acts[i])
        selected_legislation(NULL)
      }, ignoreInit = TRUE)
    })
  })
  
  # Render legislation buttons
  output$legislation_buttons <- renderUI({
    laws <- unique(filtered_legislation()$legislation_name)
    if (length(laws) == 0) return(div("No legislation found."))
    lapply(seq_along(laws), function(i) {
      actionButton(inputId = paste0("leg_", i), label = laws[i], class = "legislation-button")
    })
  })
  
  # Observe legislation selection
  observe({
    laws <- unique(legislation_data$legislation_name)
    lapply(seq_along(laws), function(i) {
      observeEvent(input[[paste0("leg_", i)]], {
        selected_legislation(laws[i])
      }, ignoreInit = TRUE)
    })
  })
  
  # Render section buttons
  output$section_buttons <- renderUI({
    leg_name <- selected_legislation()
    domain <- selected_domain()
    if (is.null(leg_name) || is.null(domain)) return(div("Select both a Management Domain and Legislation to view sections."))
    
    leg_id <- legislation_data$legislation_id[legislation_data$legislation_name == leg_name]
    domain_paragraphs <- label_data$paragraph_id[
      label_data$label_type == "Management Domain" &
        label_data$label_value == domain
    ]
    filtered <- subset(paragraph_data,
                       legislation_id == leg_id &
                         paragraph_id %in% domain_paragraphs
    )
    
    section_labels <- unique(paste(filtered$Section, filtered$Heading, sep = " | "))
    if (length(section_labels) == 0) return(div("No sections match the selected filters."))
    
    lapply(seq_along(section_labels), function(i) {
      actionButton(inputId = paste0("section_", i), label = section_labels[i], class = "section-button")
    })
  })
  
  # Observe section selection
  observe({
    sections <- unique(paragraph_data$Section)
    lapply(seq_along(sections), function(i) {
      observeEvent(input[[paste0("section_", i)]], {
        label <- input[[paste0("section_", i)]]
        parts <- strsplit(label, " \\| ")[[1]]
        section <- parts[1]
        heading <- parts[2]
        leg_id <- legislation_data$legislation_id[legislation_data$legislation_name == selected_legislation()]
        matching <- paragraph_data[
          paragraph_data$Section == section &
            paragraph_data$Heading == heading &
            paragraph_data$legislation_id == leg_id,
        ]
        
        paragraph_text <- if (nrow(matching) > 0) {
          paste(matching$Paragraph, collapse = "\n\n")
        } else {
          "No paragraphs found."
        }
        
        keywords <- unique(label_data$keyword[
          label_data$label_type == "Management Domain" &
            label_data$label_value == selected_domain()
        ])
        for (kw in keywords) {
          if (!is.na(kw) && nzchar(kw)) {
            safe_kw <- gsub("([\\W])", "\\\\\\1", kw, perl = TRUE)
            pattern <- paste0("\\b", safe_kw, "\\b")
            paragraph_text <- gsub(
              pattern,
              paste0("<span class='highlight'>", kw, "</span>"),
              paragraph_text,
              ignore.case = TRUE,
              perl = TRUE
            )
          }
        }
        
        showModal(modalDialog(
          title = paste("Section:", section, "| Heading:", heading),
          div(style = "white-space: pre-wrap; max-height: 400px; overflow-y: auto;", HTML(paragraph_text)),
          easyClose = TRUE,
          size = "l"
        ))
      }, ignoreInit = TRUE)
    })
  })
  
  # IUCN Plot
  output$iucn_plot <- renderPlot({
    domain <- selected_domain()
    leg_ids <- filtered_legislation()$legislation_id
    
    df <- label_data[label_data$label_type == "IUCN", ]
    if (!is.null(domain)) {
      domain_paragraphs <- label_data$paragraph_id[
        label_data$label_type == "Management Domain" &
          label_data$label_value == domain
      ]
      df <- df[df$paragraph_id %in% domain_paragraphs, ]
    }
    if (length(leg_ids) > 0) {
      df <- df[df$paragraph_id %in% paragraph_data$paragraph_id[
        paragraph_data$legislation_id %in% leg_ids
      ], ]
    }
    
    validate(need(nrow(df) > 0, "No data available for IUCN plot."))
    ggplot(df, aes(x = label_value)) +
      geom_bar(fill = "#2c3e50") +
      theme_minimal() +
      labs(x = "IUCN Level 2", y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Clause Type Plot
  output$clause_plot <- renderPlot({
    domain <- selected_domain()
    df <- clause_data
    if (!is.null(domain)) {
      domain_keywords <- unique(label_data$keyword[
        label_data$label_type == "Management Domain" &
          label_data$label_value == domain
      ])
      df <- df[df$keyword %in% domain_keywords, ]
    }
    
    clause_counts <- as.data.frame(table(df$clause_type))
    colnames(clause_counts) <- c("clause_type", "count")
    
    validate(need(nrow(clause_counts) > 0, "No clause data available."))
    ggplot(clause_counts, aes(x = "", y = count, fill = clause_type)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      theme_void() +
      labs(title = "Clause Types")
  })
  
  # Tornado Plot
  output$tornado_plot <- renderPlot({
    domain <- selected_domain()
    leg_ids <- filtered_legislation()$legislation_id
    
    iucn_labels <- label_data[label_data$label_type == "IUCN", ]
    if (!is.null(domain)) {
      domain_paragraphs <- label_data$paragraph_id[
        label_data$label_type == "Management Domain" &
          label_data$label_value == domain
      ]
      iucn_labels <- iucn_labels[iucn_labels$paragraph_id %in% domain_paragraphs, ]
    }
    
    para_filtered <- paragraph_data[paragraph_data$legislation_id %in% leg_ids, ]
    df <- merge(para_filtered, iucn_labels, by = "paragraph_id")
    df <- aggregate(paragraph_id ~ Section + label_value, data = df, FUN = length)
    
    validate(need(nrow(df) > 0, "No data available for Section Counts by IUCN."))
    ggplot(df, aes(x = paragraph_id, y = Section, fill = label_value)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(x = "Paragraph Count", y = "Section", fill = "IUCN Threat") +
      theme(axis.text.y = element_text(size = 8))
  })
  
  # Keyword Frequency Plot
  output$keyword_plot <- renderPlot({
    domain <- selected_domain()
    leg_ids <- filtered_legislation()$legislation_id
    
    df <- label_data[label_data$label_type == "Management Domain", ]
    if (!is.null(domain)) {
      df <- df[df$label_value == domain, ]
    }
    if (length(leg_ids) > 0) {
      para_ids <- paragraph_data$paragraph_id[paragraph_data$legislation_id %in% leg_ids]
      df <- df[df$paragraph_id %in% para_ids, ]
    }
    
    keyword_counts <- as.data.frame(table(df$keyword))
    colnames(keyword_counts) <- c("keyword", "count")
    keyword_counts <- keyword_counts[order(-keyword_counts$count), ][1:min(10, nrow(keyword_counts)), ]
    
    validate(need(nrow(keyword_counts) > 0, "No keyword data available."))
    ggplot(keyword_counts, aes(x = reorder(keyword, count), y = count)) +
      geom_bar(stat = "identity", fill = "#e67e22") +
      coord_flip() +
      theme_minimal() +
      labs(x = "Keyword", y = "Frequency")
  })
  
  # Disconnect SQLite when app stops
  onStop(function() {
    dbDisconnect(conn)
  })
}

# Launch the app
shinyApp(ui = ui, server = server)


