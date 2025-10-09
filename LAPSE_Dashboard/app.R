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
library(shinyWidgets)

# Define database path
db_path <- "legislation.db"

# Check if database exists before connecting
if (!file.exists(db_path)) {
  stop("Database file not found: ", db_path)
}

# 🔧 Connect to SQLite database
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

# 💻 Define UI----

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app_style.css"),
    tags$style(HTML("
      /* Jurisdiction color coding */
      .act-button.federal,
      .legislation-button.federal {
        background-color: #996666;
        border-left: 4px solid #773333;
        color: white;
      }
      
      .act-button.provincial,
      .legislation-button.provincial {
        background-color: #668899;
        border-left: 4px solid #446677;
        color: white;
      }
      
      .act-button.unknown,
      .legislation-button.unknown {
        background-color: #999999;
        border-left: 4px solid #777777;
        color: white;
      }
      
      .act-button.federal:hover,
      .legislation-button.federal:hover {
        background-color: #aa7777;
        color: white;
      }
      
      .act-button.provincial:hover,
      .legislation-button.provincial:hover {
        background-color: #7799aa;
        color: white;
      }
      
      .act-button.unknown:hover,
      .legislation-button.unknown:hover {
        background-color: #aaaaaa;
        color: white;
      }
      
      /* Jurisdiction toggle switch color coding */
      #jurisdiction_filter .radiobtn-all {
        background-color: #cccccc !important;
        border-color: #999999 !important;
        color: white !important;
      }
      
      #jurisdiction_filter .radiobtn-federal {
        background-color: #996666 !important;
        border-color: #773333 !important;
        color: white !important;
      }
      
      #jurisdiction_filter .radiobtn-provincial {
        background-color: #668899 !important;
        border-color: #446677 !important;
        color: white !important;
      }
      
      #jurisdiction_filter .radiobtn-all:hover {
        background-color: #bbbbbb !important;
      }
      
      #jurisdiction_filter .radiobtn-federal:hover {
        background-color: #aa7777 !important;
      }
      
      #jurisdiction_filter .radiobtn-provincial:hover {
        background-color: #7799aa !important;
      }
    ")),
    tags$script(HTML("
      $(document).ready(function() {
        // Wait for shinyWidgets to render
        setTimeout(function() {
          // Find all buttons in the jurisdiction filter
          $('#jurisdiction_filter .btn').each(function() {
            var btnText = $(this).text().trim();
            $(this).removeClass('radiobtn-all radiobtn-federal radiobtn-provincial');
            
            if (btnText === 'All') {
              $(this).addClass('radiobtn-all');
              $(this).css({
                'background-color': '#cccccc',
                'border-color': '#999999',
                'color': 'white'
              });
            } else if (btnText === 'Federal') {
              $(this).addClass('radiobtn-federal');
              $(this).css({
                'background-color': '#996666',
                'border-color': '#773333',
                'color': 'white'
              });
            } else if (btnText === 'Provincial') {
              $(this).addClass('radiobtn-provincial');
              $(this).css({
                'background-color': '#668899',
                'border-color': '#446677',
                'color': 'white'
              });
            }
          });
        }, 100);
      });
    "))
  ),
  
  titlePanel("LAPSE Dashboard"),
  
  # Information link below title
  div(
    style = "text-align: left; margin-bottom: 20px; padding: 10px; background-color: #f0f0f0; border-radius: 5px;",
    tags$a(
      href = "https://ennsjoe.github.io/salmon_management_domains_compendium/LAPSE-Technical-Brief.html",
      target = "_blank",
      style = "color: #996666; font-weight: 600; text-decoration: none; font-size: 16px;",
      icon("info-circle"),
      " About: LAPSE Technical Brief"
    )
  ),
  
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
          radioGroupButtons(
            inputId = "jurisdiction_filter",
            label = NULL,
            choices = c("All", "Federal", "Provincial"),
            selected = "All",
            justified = TRUE,
            size = "sm",
            individual = TRUE
          ),
          
          h4("Acts"),
          uiOutput("act_buttons"),
          hr(),
          
          h4("Acts and Regulations"),
          uiOutput("legislation_buttons"),
          hr(),
          
          h4("Sections and Paragraphs"),
          uiOutput("section_paragraphs")
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

# ⚙️ Server----
server <- function(input, output, session) {
  selected_domain <- reactiveVal(NULL)
  selected_act <- reactiveVal(NULL)
  selected_legislation <- reactiveVal(NULL)
  
  # 📼 Render domain buttons----
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
  
  # Observe act reset button
  observeEvent(input$reset_act, {
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
  
  # 📼 Render act buttons----
  output$act_buttons <- renderUI({
    acts_data <- filtered_legislation()[, c("act_name", "jurisdiction")]
    acts_data <- unique(acts_data)
    selected <- selected_act()
    
    if (nrow(acts_data) == 0) {
      return(div("No acts match the selected filters."))
    }
    
    tagList(
      # Always show the reset button
      div(
        class = "act-button reset",
        `onclick` = "Shiny.setInputValue('reset_act', Math.random())",
        tagList(icon("sync"), "All")
      ),
      tags$hr(),
      
      # Show either all buttons or just the selected one
      if (is.null(selected)) {
        lapply(seq_len(nrow(acts_data)), function(i) {
          act <- acts_data$act_name[i]
          jurisdiction <- acts_data$jurisdiction[i]
          jurisdiction_class <- if (jurisdiction == "Federal") "federal" else if (jurisdiction == "Provincial") "provincial" else "unknown"
          
          div(
            class = paste("act-button", jurisdiction_class),
            `onclick` = paste0("Shiny.setInputValue('act_click', '", act, "', {priority: 'event'})"),
            act
          )
        })
      } else {
        # Show only the selected act if it's still in the filtered list
        if (selected %in% acts_data$act_name) {
          jurisdiction <- acts_data$jurisdiction[acts_data$act_name == selected][1]
          jurisdiction_class <- if (jurisdiction == "Federal") "federal" else if (jurisdiction == "Provincial") "provincial" else "unknown"
          
          div(
            class = paste("act-button selected", jurisdiction_class),
            title = "Currently selected act",
            selected
          )
        } else {
          # If selected act is no longer valid, reset selection
          selected_act(NULL)
          return(div("Selected act is no longer available."))
        }
      }
    )
  })
  
  # Observe act selection
  observeEvent(input$act_click, {
    selected_act(input$act_click)
    selected_legislation(NULL)
  })
  
  # 📼 Render legislation buttons----
  output$legislation_buttons <- renderUI({
    laws <- filtered_legislation()
    if (nrow(laws) == 0) return(div("No legislation found."))
    
    lapply(seq_len(nrow(laws)), function(i) {
      jurisdiction <- laws$jurisdiction[i]
      jurisdiction_class <- if (jurisdiction == "Federal") "federal" else if (jurisdiction == "Provincial") "provincial" else "unknown"
      
      actionButton(
        inputId = paste0("leg_", i), 
        label = laws$legislation_name[i], 
        class = paste("legislation-button", jurisdiction_class)
      )
    })
  })
  
  # Observe legislation selection
  observe({
    laws <- filtered_legislation()
    lapply(seq_len(nrow(laws)), function(i) {
      observeEvent(input[[paste0("leg_", i)]], {
        selected_legislation(laws$legislation_id[i])  # store ID directly
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
  
  # âœ… Output section paragraphs with keyword highlighting----
  output$section_paragraphs <- renderUI({
    req(selected_legislation())
    
    # Get legislation ID
    leg_id <- selected_legislation()
    all_paragraphs <- paragraph_data[paragraph_data$legislation_id == leg_id, ]
    
    if (nrow(all_paragraphs) == 0 || all(is.na(all_paragraphs$Section))) {
      return(div("No sections or paragraphs found for this legislation."))
    }
    
    # Check if a domain is selected
    domain <- selected_domain()
    domain_clean <- if (!is.null(domain) && !is.na(domain)) trimws(tolower(domain)) else NULL
    
    # Normalize domain for matching if selected
    if (!is.null(domain_clean)) {
      label_data$label_value <- trimws(tolower(label_data$label_value))
      
      # Get paragraph IDs tagged with the selected domain
      domain_para_ids <- label_data$paragraph_id[
        label_data$label_type == "Management Domain" &
          label_data$label_value == domain_clean
      ]
    } else {
      # No domain filter - include all paragraphs
      domain_para_ids <- all_paragraphs$paragraph_id
    }
    
    # Group all paragraphs by section
    section_groups <- split(all_paragraphs, all_paragraphs$Section)
    
    # Sort sections numerically if possible
    section_names <- names(section_groups)
    section_order <- suppressWarnings(as.numeric(section_names))
    sorted_names <- if (any(!is.na(section_order))) {
      section_names[order(section_order, na.last = TRUE)]
    } else {
      sort(section_names)
    }
    
    section_groups <- section_groups[sorted_names]
    
    tagList(
      lapply(sorted_names, function(sec) {
        section_data <- section_groups[[sec]]
        
        # Check if this section has any domain-tagged paragraphs
        matched_ids <- intersect(section_data$paragraph_id, domain_para_ids)
        
        # If domain is selected and no matches in this section, skip it
        if (!is.null(domain_clean) && length(matched_ids) == 0) return(NULL)
        
        heading <- unique(na.omit(section_data$Heading))
        heading_text <- if (length(heading) > 0) heading[1] else "No heading available"
        
        # Sort paragraphs by paragraph_id to maintain order
        section_data <- section_data[order(section_data$paragraph_id), ]
        
        # Always aggregate ALL paragraphs in the section in order
        aggregated_text <- paste(section_data$Paragraph[!is.na(section_data$Paragraph)], collapse = "\n\n")
        
        # Only apply highlighting if a domain is selected
        highlighted_text <- aggregated_text
        
        if (!is.null(domain_clean)) {
          # Filter label_data to Management Domain keywords in this section
          domain_labels <- label_data[
            label_data$paragraph_id %in% section_data$paragraph_id &
              label_data$label_type == "Management Domain",
          ]
          
          # Highlight Management Domain keywords
          for (kw in unique(domain_labels$keyword)) {
            # Only highlight if keyword appears in text (case-insensitive)
            if (grepl(kw, highlighted_text, ignore.case = TRUE)) {
              # Create a temporary marker to avoid nested replacements
              temp_marker <- paste0("###HIGHLIGHT_DOMAIN_", gsub(" ", "_", kw), "###")
              highlighted_text <- gsub(
                pattern = paste0("(?i)\\b", kw, "\\b"),
                replacement = temp_marker,
                x = highlighted_text,
                perl = TRUE
              )
            }
          }
          
          # Filter label_data to Clause Type keywords in this section
          clause_labels <- label_data[
            label_data$paragraph_id %in% section_data$paragraph_id &
              label_data$label_type == "Clause Type",
          ]
          
          # Highlight Clause Type keywords
          for (kw in unique(clause_labels$keyword)) {
            # Only highlight if keyword appears in text (case-insensitive)
            if (grepl(kw, highlighted_text, ignore.case = TRUE)) {
              # Create a temporary marker to avoid nested replacements
              temp_marker <- paste0("###HIGHLIGHT_CLAUSE_", gsub(" ", "_", kw), "###")
              highlighted_text <- gsub(
                pattern = paste0("(?i)\\b", kw, "\\b"),
                replacement = temp_marker,
                x = highlighted_text,
                perl = TRUE
              )
            }
          }
          
          # Replace all domain markers with actual HTML
          domain_kws <- unique(domain_labels$keyword)
          for (kw in domain_kws) {
            temp_marker <- paste0("###HIGHLIGHT_DOMAIN_", gsub(" ", "_", kw), "###")
            highlighted_text <- gsub(
              pattern = temp_marker,
              replacement = paste0("<span class='highlight-domain'>", kw, "</span>"),
              x = highlighted_text,
              fixed = TRUE
            )
          }
          
          # Replace all clause markers with actual HTML
          clause_kws <- unique(clause_labels$keyword)
          for (kw in clause_kws) {
            temp_marker <- paste0("###HIGHLIGHT_CLAUSE_", gsub(" ", "_", kw), "###")
            highlighted_text <- gsub(
              pattern = temp_marker,
              replacement = paste0("<span class='highlight-clause'>", kw, "</span>"),
              x = highlighted_text,
              fixed = TRUE
            )
          }
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
  })
  
  # 📈 IUCN Plot----
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
  
  # 📈 Clause Type Plot----
  output$clause_plot <- renderPlot({
    # Ensure required packages are loaded
    library(dplyr)
    library(tidyr)
    
    # Normalize selected domain
    domain <- selected_domain()
    domain_clean <- if (!is.null(domain) && !is.na(domain)) trimws(tolower(domain)) else NULL
    
    # Get filtered legislation IDs
    leg_ids <- filtered_legislation()$legislation_id
    if (is.null(leg_ids) || length(leg_ids) == 0) {
      leg_ids <- legislation_data$legislation_id
    }
    
    # Determine which paragraphs to include based on selections
    if (!is.null(selected_legislation())) {
      # Legislation is selected - filter to that legislation
      leg_id <- selected_legislation()
      relevant_paragraphs <- paragraph_data[paragraph_data$legislation_id == leg_id, ]
      
      # If domain is selected, further filter to sections containing that domain
      if (!is.null(domain_clean)) {
        # Get paragraph IDs tagged with the selected domain
        label_data_normalized <- label_data
        label_data_normalized$label_value <- trimws(tolower(label_data_normalized$label_value))
        
        domain_para_ids <- label_data_normalized$paragraph_id[
          label_data_normalized$label_type == "Management Domain" &
            label_data_normalized$label_value == domain_clean
        ]
        
        # Find sections that contain domain-tagged paragraphs
        sections_with_domain <- unique(relevant_paragraphs$Section[
          relevant_paragraphs$paragraph_id %in% domain_para_ids
        ])
        
        # Filter to only paragraphs in those sections
        relevant_paragraphs <- relevant_paragraphs[
          relevant_paragraphs$Section %in% sections_with_domain,
        ]
      }
      
      domain_paragraphs <- relevant_paragraphs$paragraph_id
      
    } else if (!is.null(domain_clean)) {
      # Only domain selected (no legislation)
      label_data_normalized <- label_data
      label_data_normalized$label_value <- trimws(tolower(label_data_normalized$label_value))
      
      domain_paragraphs <- label_data_normalized$paragraph_id[
        label_data_normalized$label_type == "Management Domain" &
          label_data_normalized$label_value == domain_clean
      ]
    } else {
      # Nothing selected - use all paragraphs from filtered legislation
      domain_paragraphs <- paragraph_data$paragraph_id[
        paragraph_data$legislation_id %in% leg_ids
      ]
    }
    
    # Exit early if no matching paragraphs
    validate(need(length(domain_paragraphs) > 0, "No paragraphs found."))
    
    # Normalize label_data values
    label_data_plot <- label_data %>%
      mutate(label_value = trimws(tolower(label_value)))
    
    # Filter label_data to relevant label types within selected paragraphs
    co_labels <- label_data_plot %>%
      filter(paragraph_id %in% domain_paragraphs,
             label_type %in% c("Management Domain", "Clause Type")) %>%
      select(paragraph_id, label_type, label_value)
    
    # Exit if no co-occurrence data
    validate(need(nrow(co_labels) > 0, "No label data available."))
    
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
    validate(need(nrow(co_counts) > 0, "No clause type co-occurrence data available."))
    
    ggplot(co_counts, aes(x = `Management Domain`, y = `Clause Type`, fill = Count)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "lightblue", high = "#2c3e50") +
      labs(title = "Clause Type Co-occurrence by Management Domain",
           x = "Management Domain", y = "Clause Type", fill = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(size = 10))
  })
  
  # 📈 Keyword Frequency Plot----
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