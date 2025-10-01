################################################################################
# Title: LAPSE Dashboard Web App
# Authors: Joe Enns and Cory Lagasse
# Date Created: 2025-08-26
# Purpose / Description: this script creates a Shiny web application to visualize 
#   and interact with legislative data stored in a SQLite database.
# Dependencies: shiny, DBI, RSQLite, ggplot2, data.table
# Database: legislation.db
################################################################################

library(shiny)
library(DBI)
library(RSQLite)
library(ggplot2)
library(data.table)
library(stringi)
library(stringr)
library(dplyr)
library(tidyr)

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
          
          h4("Sections and Paragraphs"),
          uiOutput("section_paragraphs")  # ✅ New inline display
      )
    ),
    
    # Graph Panel: Visualizations
    column(
      width = 4,
      div(class = "graph-panel",
          h4("Keyword Frequency"),
          plotOutput("keyword_plot", height = "250px"),
          
          h4("IUCN Co-occurrence"),
          plotOutput("iucn_plot", height = "250px"),
          hr(),
          
          h4("Clause Type Distribution"),
          plotOutput("clause_plot", height = "250px"),
          hr()
      )
    )
  )
)

# Server----
server <- function(input, output, session) {
  selected_domain <- reactiveVal(NULL)
  selected_act <- reactiveVal(NULL)
  selected_legislation <- reactiveVal(NULL)
  
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
  
  output$section_paragraphs <- renderUI({
    req(selected_legislation(), selected_domain())
    
    # Normalize domain for matching
    selected_domain_clean <- trimws(tolower(selected_domain()))
    label_data$label_value <- trimws(tolower(label_data$label_value))  # normalize only label_value
    
    # Get legislation ID
    leg_id <- legislation_data$legislation_id[
      legislation_data$legislation_name == selected_legislation()
    ]
    
    # Filter paragraph_data by legislation only
    filtered <- paragraph_data[
      paragraph_data$legislation_id == leg_id,
    ]
    
    if (nrow(filtered) == 0 || all(is.na(filtered$Section))) {
      return(div("No sections or paragraphs found for this legislation."))
    }
    
    # Get domain-labeled paragraph IDs (for keyword highlighting only)
    domain_para_ids <- label_data$paragraph_id[
      label_data$label_type == "Management Domain" &
        label_data$label_value == selected_domain_clean
    ]
    
    section_groups <- split(filtered, filtered$Section)
    
    return(
      tagList(
        lapply(names(section_groups), function(sec) {
          section_data <- section_groups[[sec]]
          heading <- unique(na.omit(section_data$Heading))
          heading_text <- if (length(heading) > 0) heading[1] else "No heading available"
          paragraphs <- section_data$Paragraph
          
          print(paste("Rendering section:", sec, "| Paragraphs:", length(paragraphs)))
          
          aggregated_text <- paste(na.omit(paragraphs), collapse = "\n\n")
          
          # Get keywords for this section's paragraphs (only those labeled with domain)
          section_para_ids <- section_data$paragraph_id
          keywords <- label_data[
            label_data$paragraph_id %in% section_para_ids &
              label_data$paragraph_id %in% domain_para_ids &
              label_data$label_type %in% c("Management Domain", "IUCN", "Clause Type", "Salmon Scope"),
          ]$keyword
          
          # Highlight keywords in the aggregated text
          highlighted_text <- aggregated_text
          for (kw in unique(keywords)) {
            highlighted_text <- str_replace_all(
              string = highlighted_text,
              pattern = fixed(kw, ignore_case = TRUE),
              replacement = paste0("<span class='highlight'>", kw, "</span>")
            )
          }
          
          div(
            class = "section-block",
            h5(paste("Section", sec)),
            h6(heading_text),
            div(
              style = "white-space: pre-wrap; margin-bottom: 20px;",
              HTML(highlighted_text)
            )
          )
        })
      )
    )
  })
  
  # IUCN Plot----
  output$iucn_plot <- renderPlot({
    domain <- selected_domain()
    domain_clean <- if (!is.null(domain) && !is.na(domain)) trimws(tolower(domain)) else ""
    leg_ids <- filtered_legislation()$legislation_id
    if (is.null(leg_ids)) leg_ids <- character(0)
    
    # Filter IUCN labels
    df <- label_data[label_data$label_type == "IUCN", ]
    
    # Filter by domain
    if (nzchar(domain_clean)) {
      domain_paragraphs <- label_data$paragraph_id[
        label_data$label_type == "Management Domain" &
          trimws(tolower(label_data$label_value)) == domain_clean
      ]
      df <- df[df$paragraph_id %in% domain_paragraphs, ]
    }
    
    # Filter by legislation
    if (length(leg_ids) > 0) {
      df <- df[df$paragraph_id %in% paragraph_data$paragraph_id[
        paragraph_data$legislation_id %in% leg_ids
      ], ]
    }
    
    validate(need(nrow(df) > 0, "No data available for IUCN plot."))
    
    # Count and reorder
    iucn_counts <- df %>%
      count(label_value) %>%
      arrange(desc(n)) %>%
      mutate(label_value = factor(label_value, levels = label_value))
    
    ggplot(iucn_counts, aes(x = label_value, y = n)) +
      geom_bar(stat = "identity", fill = "#2c3e50") +
      theme_minimal() +
      labs(x = "IUCN Level 2", y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Clause Type Plot----
  output$clause_plot <- renderPlot({
    # Ensure required packages are loaded
    library(dplyr)
    library(tidyr)
    
    # Normalize selected domain
    domain <- selected_domain()
    domain_clean <- if (!is.null(domain) && !is.na(domain)) trimws(tolower(domain)) else ""
    
    # Normalize label_data values
    label_data <- label_data %>%
      mutate(label_value = trimws(tolower(label_value)))
    
    # Filter paragraph IDs for selected domain
    domain_paragraphs <- label_data %>%
      filter(label_type == "Management Domain", label_value == domain_clean) %>%
      pull(paragraph_id)
    
    # Exit early if no matching paragraphs
    validate(need(length(domain_paragraphs) > 0, "No paragraphs found for this domain."))
    
    # Filter label_data to relevant label types within selected paragraphs
    co_labels <- label_data %>%
      filter(paragraph_id %in% domain_paragraphs,
             label_type %in% c("Management Domain", "Clause Type")) %>%
      select(paragraph_id, label_type, label_value)
    
    # Pivot to wide format and filter valid co-occurrences
    co_occurrence <- co_labels %>%
      group_by(paragraph_id, label_type) %>%
      summarise(label_value = paste(unique(label_value), collapse = ";"), .groups = "drop") %>%
      pivot_wider(names_from = label_type, values_from = label_value, values_fill = "") %>%
      filter(`Management Domain` != "", `Clause Type` != "")
    
    # Split multi-labels and count co-occurrences
    co_counts <- co_occurrence %>%
      separate_rows(`Management Domain`, sep = ";") %>%
      separate_rows(`Clause Type`, sep = ";") %>%
      count(`Management Domain`, `Clause Type`, name = "Count")
    
    # Validate and plot
    validate(need(nrow(co_counts) > 0, "No clause type co-occurrence data available for this domain."))
    
    ggplot(co_counts, aes(x = `Management Domain`, y = `Clause Type`, fill = Count)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "lightblue", high = "#2c3e50") +
      labs(title = "Clause Type Co-occurrence by Management Domain",
           x = "Management Domain", y = "Clause Type", fill = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(size = 10))
  })
  
  # Keyword Frequency Plot----
  output$keyword_plot <- renderPlot({
    domain <- selected_domain()
    domain_clean <- if (!is.null(domain) && !is.na(domain)) trimws(tolower(domain)) else ""
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

  # Disconnect SQLite when app stops
  onStop(function() {
    dbDisconnect(conn)
  })
}
# Launch the app
shinyApp(ui = ui, server = server)


