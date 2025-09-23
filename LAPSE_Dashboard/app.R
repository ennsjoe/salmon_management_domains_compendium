library(shiny)
library(DBI)
library(RSQLite)
library(ggplot2)
library(data.table)

# Define database path
db_path <- "legislation.db"

# Check if database exists before connecting
if (!file.exists(db_path)) {
  stop("Database file not found: ", db_path)
}

# Connect to SQLite database
conn <- dbConnect(RSQLite::SQLite(), dbname = db_path)

# Disconnect when app stops
onStop(function() {
  if (DBI::dbIsValid(conn)) {
    dbDisconnect(conn)
  }
})

# Load data safely
label_data <- dbReadTable(conn, "paragraph_label_table")
legislation_data <- dbReadTable(conn, "LegislationMetadata")
paragraph_data <- dbReadTable(conn, "LegislationParagraphs")
clause_data <- dbReadTable(conn, "clause_type_keywords")

# Prepare UI choices
management_domains <- unique(label_data$label_value[label_data$label_type == "Management Domain"])
jurisdictions <- unique(legislation_data$jurisdiction)

################################################################################
################################################################################
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
          
          h4("Acts"),
          uiOutput("act_buttons"),
          hr(),
          
          h4("Acts and Regulations"),
          uiOutput("legislation_buttons"),
          hr(),
          
          h4("Sections"),
          uiOutput("section_buttons")
      )
    ),

################################################################################        
    # Graph Panel: Visualizations
    column(
      width = 4,
      div(class = "graph-panel",
          h4("Keyword Frequency"),
          plotOutput("keyword_plot", height = "250px"),
          
          h4("IUCN Co-occurence"),
          plotOutput("iucn_plot", height = "250px"),
          hr(),
          
          h4("Clause Type Distribution"),
          plotOutput("clause_plot", height = "250px"),
          hr()
      )
    )
  )
)

################################################################################
################################################################################
# Server----
server <- function(input, output, session) {
  selected_domain <- reactiveVal(NULL)
  selected_act <- reactiveVal(NULL)
  selected_legislation <- reactiveVal(NULL)
  
################################################################################  
  # Render domain buttons----
  output$domain_buttons <- renderUI({
    selected <- selected_domain()
    
    tagList(
      # Always show the reset button
      div(
        class = "domain-button reset",
        `onclick` = "Shiny.setInputValue('reset_domain', Math.random())",
        tagList(icon("sync"), "All")
      ),
      tags$hr(),
      
      # Show either all buttons or just the selected one
      if (is.null(selected)) {
        lapply(seq_along(management_domains), function(i) {
          domain <- management_domains[i]
          btn_id <- paste0("domain_", i)
          div(
            class = "domain-button",
            `onclick` = paste0("Shiny.setInputValue('", btn_id, "', Math.random())"),
            domain
          )
        })
      } else {
        div(
          class = "domain-button selected",
          title = "Currently selected domain",
          selected
        )
      }
    )
  })
  
  # Observe domain selection
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
  
  # Observe reset button
  observeEvent(input$reset_domain, {
    selected_domain(NULL)
    selected_act(NULL)
    selected_legislation(NULL)
  })
  
################################################################################  
  # Reactive: Filter legislation----
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
  
################################################################################  
  # Render act buttons
  output$act_buttons <- renderUI({
    acts <- unique(filtered_legislation()$act_name)
    selected <- selected_act()
    
    if (length(acts) == 0) {
      return(div("No acts match the selected filters."))
    }
    
    # If no act is selected, show all available act buttons
    if (is.null(selected)) {
      lapply(seq_along(acts), function(i) {
        act <- acts[i]
        div(
          class = "act-button",
          `onclick` = paste0("Shiny.setInputValue('act_click', '", act, "', {priority: 'event'})"),
          act
        )
      })
    } else {
      # Show only the selected act if it's still in the filtered list
      if (selected %in% acts) {
        div(
          class = "act-button selected",
          selected
        )
      } else {
        # If selected act is no longer valid, reset selection
        selected_act(NULL)
        return(div("Selected act is no longer available."))
      }
    }
  })
  
  # Observe act selection
  observeEvent(input$act_click, {
    selected_act(input$act_click)
    selected_legislation(NULL)
  })
  
################################################################################  
  # Render legislation buttons----
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
  
################################################################################  
  # Render section buttons
  output$section_buttons <- renderUI({
    req(selected_legislation(), selected_domain())
    
    leg_id <- legislation_data$legislation_id[
      legislation_data$legislation_name == selected_legislation()
    ]
    
    domain_ids <- label_data$paragraph_id[
      label_data$label_type == "Management Domain" &
        label_data$label_value == selected_domain()
    ]
    
    # Filter only for button display
    filtered <- paragraph_data[
      paragraph_data$legislation_id == leg_id &
        paragraph_data$paragraph_id %in% domain_ids,
    ]
    
    if (nrow(filtered) == 0 || all(is.na(filtered$Section))) {
      return(div("No sections match the selected filters."))
    }
    
    section_labels <- unique(na.omit(filtered$Section))
    
    lapply(seq_along(section_labels), function(i) {
      div(
        class = "section-button",
        `onclick` = paste0(
          "Shiny.setInputValue('section_click', '",
          section_labels[i],
          "', {priority: 'event'})"
        ),
        paste("Section", section_labels[i])
      )
    })
  })
  
################################################################################  
  observeEvent(input$section_click, {
    selected_section <- input$section_click
    req(selected_section)
    
    leg_id <- legislation_data$legislation_id[
      legislation_data$legislation_name == selected_legislation()
    ]
    
    # ✅ Show all paragraphs for the section, regardless of domain
    section_paragraphs <- paragraph_data[
      paragraph_data$legislation_id == leg_id &
        paragraph_data$Section == selected_section,
    ]
    
    aggregated_text <- if (nrow(section_paragraphs) > 0) {
      paste(unique(na.omit(section_paragraphs$Paragraph)), collapse = "\n\n")
    } else {
      "No paragraphs found for this section."
    }
    
    showModal(modalDialog(
      title = paste("Section", selected_section, "|", selected_legislation()),
      div(
        style = "white-space: pre-wrap; max-height: 400px; overflow-y: auto;",
        HTML(aggregated_text)
      ),
      easyClose = TRUE,
      size = "l"
    ))
  })
  
################################################################################  
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
  
################################################################################  
  # Clause Type Plot----
  output$clause_plot <- renderPlot({
    domain <- selected_domain()
    df <- clause_data
    
    # Filter by selected domain
    if (!is.null(domain)) {
      domain_keywords <- unique(label_data$keyword[
        label_data$label_type == "Management Domain" &
          label_data$label_value == domain
      ])
      df <- df[df$keyword %in% domain_keywords, ]
    }
    
    # Safely generate clause counts
    clause_counts <- as.data.frame(table(df$clause_type))
    if (ncol(clause_counts) == 2) {
      colnames(clause_counts) <- c("clause_type", "count")
    } else {
      clause_counts <- data.frame(clause_type = character(0), count = numeric(0))
    }
    
    # Validate and plot
    validate(need(nrow(clause_counts) > 0, "No clause data available."))
    ggplot(clause_counts, aes(x = "", y = count, fill = clause_type)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      theme_void() +
      labs(title = "Clause Types") +
      theme(legend.title = element_blank())
  })
  
################################################################################  
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
      geom_bar(stat = "identity", fill = "#2c3e50") +
      coord_flip() +
      theme_minimal() +
      labs(x = "Keyword", y = "Frequency")
  })

################################################################################  
  # Disconnect SQLite when app stops
  onStop(function() {
    dbDisconnect(conn)
  })
}

# Launch the app
shinyApp(ui = ui, server = server)


