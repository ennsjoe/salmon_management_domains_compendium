library(shiny)
library(DBI)
library(RPostgres)
library(pool)
library(ggplot2)

# Create a connection pool to PostgreSQL
pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "your_db",
  host = "your_host",
  user = "your_user",
  password = "your_password"
)

# Close pool when app stops
onStop(function() {
  poolClose(pool)
})

# Load data from PostgreSQL
label_data <- dbGetQuery(pool, "SELECT * FROM paragraph_label_table")
legislation_data <- dbGetQuery(pool, "SELECT * FROM LegislationMetadata")
paragraph_data <- dbGetQuery(pool, "SELECT * FROM LegislationParagraphs")
clause_data <- dbGetQuery(pool, "SELECT * FROM clause_type_keywords")

# Prepare UI choices
management_domains <- unique(label_data$label_value[label_data$label_type == "Management Domain"])
jurisdictions <- unique(legislation_data$jurisdiction)

# UI
ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "app_style.css")),
  titlePanel("LAPSE Dashboard"),
  fluidRow(
    column(
      width = 2,
      div(class = "domain-panel", h4("Management Domains"), uiOutput("domain_buttons"))
    ),
    column(
      width = 6,
      div(class = "main-panel",
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
    ),
    column(
      width = 4,
      div(class = "graph-panel",
          h4("IUCN Value Tally"),
          plotOutput("iucn_plot", height = "200px"),
          h4("Clause Type Distribution"),
          plotOutput("clause_plot", height = "200px"),
          h4("Section Counts by IUCN"),
          plotOutput("tornado_plot", height = "200px"),
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
  
  # Domain buttons
  output$domain_buttons <- renderUI({
    selected <- selected_domain()
    if (is.null(selected)) {
      lapply(seq_along(management_domains), function(i) {
        domain <- management_domains[i]
        btn_id <- paste0("domain_", i)
        actionButton(inputId = btn_id, label = domain, class = "domain-button")
      })
    } else {
      actionButton(inputId = "reset_domain", label = selected, class = "domain-button")
    }
  })
  
  observe({
    lapply(seq_along(management_domains), function(i) {
      btn_id <- paste0("domain_", i)
      observeEvent(input[[btn_id]], {
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
  
  # Filtered legislation
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
      if (length(domain_paragraphs) > 0) {
        query <- sprintf(
          "SELECT DISTINCT legislation_id FROM LegislationParagraphs WHERE paragraph_id IN (%s)",
          paste(domain_paragraphs, collapse = ",")
        )
        leg_ids <- tryCatch({
          result <- dbGetQuery(pool, query)
          if (!is.null(result) && "legislation_id" %in% names(result)) {
            result$legislation_id
          } else character(0)
        }, error = function(e) {
          message("DB query failed: ", e$message)
          character(0)
        })
        data <- subset(data, legislation_id %in% leg_ids)
      } else {
        data <- data[0, ]
      }
    }
    
    if (!is.null(selected_act())) {
      data <- subset(data, act_name == selected_act())
    }
    
    data
  })
  
  # Act buttons
  output$act_buttons <- renderUI({
    acts <- unique(filtered_legislation()$act_name)
    if (length(acts) == 0) return(div("No acts match the selected filters."))
    lapply(seq_along(acts), function(i) {
      act <- acts[i]
      btn_id <- paste0("act_", i)
      actionButton(inputId = btn_id, label = act, class = "act-button")
    })
  })
  
  observe({
    lapply(seq_along(unique(legislation_data$act_name)), function(i) {
      btn_id <- paste0("act_", i)
      observeEvent(input[[btn_id]], {
        selected_act(unique(legislation_data$act_name)[i])
        selected_legislation(NULL)
      }, ignoreInit = TRUE)
    })
  })
  
  # Legislation buttons
  output$legislation_buttons <- renderUI({
    legislation <- unique(filtered_legislation()$legislation_name)
    if (length(legislation) == 0) return(div("No legislation found."))
    lapply(seq_along(legislation), function(i) {
      name <- legislation[i]
      btn_id <- paste0("leg_", i)
      actionButton(inputId = btn_id, label = name, class = "legislation-button")
    })
  })
  
  observe({
    lapply(seq_along(unique(legislation_data$legislation_name)), function(i) {
      btn_id <- paste0("leg_", i)
      observeEvent(input[[btn_id]], {
        selected_legislation(unique(legislation_data$legislation_name)[i])
      }, ignoreInit = TRUE)
    })
  })
  
  # Section buttons
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
      label <- section_labels[i]
      btn_id <- paste0("section_", i)
      actionButton(inputId = btn_id, label = label, class = "section-button")
    })
  })
  
  observe({
    lapply(seq_along(unique(paragraph_data$Section)), function(i) {
      btn_id <- paste0("section_", i)
      observeEvent(input[[btn_id]], {
        label <- input[[btn_id]]
        parts <- strsplit(label, " \\| ")[[1]]
        section <- parts[1]
        heading <- parts[2]
        matching <- paragraph_data[paragraph_data$Section == section, ]
        
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
  
  # Plots
  output$iucn_plot <- renderPlot({
    domain <- selected_domain()
    leg_ids <- filtered_legislation()$legislation_id
    
    df <- label_data[label_data$label_type == "IUCN", ]
    if (!is.null(domain)) {
      df <- df[df$paragraph_id %in% label_data$paragraph_id[
        label_data$label_type == "Management Domain" &
          label_data$label_value == domain
      ], ]
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
      labs(x = "IUCN Level 2", y = "Count")
  })
  
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
  
  output$tornado_plot <- renderPlot({
    domain <- selected_domain()
    leg_ids <- filtered_legislation()$legislation_id
    
    # Filter IUCN-labeled paragraphs
    iucn_labels <- label_data[label_data$label_type == "IUCN", ]
    if (!is.null(domain)) {
      domain_paragraphs <- label_data$paragraph_id[
        label_data$label_type == "Management Domain" &
          label_data$label_value == domain
      ]
      iucn_labels <- iucn_labels[iucn_labels$paragraph_id %in% domain_paragraphs, ]
    }
    
    # Filter paragraph_data by legislation
    para_filtered <- paragraph_data[paragraph_data$legislation_id %in% leg_ids, ]
    
    # Merge and aggregate
    df <- merge(para_filtered, iucn_labels, by = "paragraph_id")
    df <- aggregate(paragraph_id ~ Section + label_value, data = df, FUN = length)
    
    validate(need(nrow(df) > 0, "No data available for Section Counts by IUCN."))
    ggplot(df, aes(x = paragraph_id, y = Section, fill = label_value)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Count", y = "Section")
  })
  
  output$keyword_plot <- renderPlot({
    domain <- selected_domain()
    leg_ids <- filtered_legislation()$legislation_id
    
    # Filter label_data to Management Domain keywords
    df <- label_data[label_data$label_type == "Management Domain", ]
    if (!is.null(domain)) {
      df <- df[df$label_value == domain, ]
    }
    if (length(leg_ids) > 0) {
      para_ids <- paragraph_data$paragraph_id[paragraph_data$legislation_id %in% leg_ids]
      df <- df[df$paragraph_id %in% para_ids, ]
    }
    
    # Count and plot top 10 keywords
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
  
  # Properly close the PostgreSQL pool when the app stops
  onStop(function() {
    poolClose(pool)
  })
}

# Launch the app
shinyApp(ui = ui, server = server)

