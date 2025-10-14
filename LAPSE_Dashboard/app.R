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
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
    tags$style(HTML("
      /* Dropdown styling */
      .dropdown-container {
        display: flex;
        gap: 20px;
        margin-bottom: 20px;
      }
      
      .dropdown-wrapper {
        flex: 1;
      }
      
      .dropdown-wrapper label {
        font-weight: 600;
        color: #2c3e50;
        margin-bottom: 5px;
        display: block;
      }
      
      /* Search highlight styling */
      .highlight-search {
        background-color: #ffff00;
        font-weight: bold;
        padding: 2px 4px;
        border-radius: 3px;
      }
      
      /* Jurisdiction color coding for dropdowns */
      select.federal {
        border-left: 4px solid #996666;
      }
      
      select.provincial {
        border-left: 4px solid #668899;
      }
      
      select.mixed {
        border-left: 4px solid #999999;
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
      
      /* === Collapsible Section Styles === */
      .section-block {
        transition: box-shadow 0.2s ease;
      }
      
      .section-block:hover {
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
      }
      
      .section-header:hover {
        background-color: #e8e8e8 !important;
      }
      
      /* Rotate chevron when expanded */
      .section-header[aria-expanded='true'] i {
        transform: rotate(180deg);
      }
      
      /* Smooth transition for content */
      .collapse {
        transition: height 0.3s ease;
      }
      
      /* Style for section content */
      .section-content {
        border-top: 1px solid #e0e0e0;
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
          h4("Search Legislation"),
          div(
            style = "display: flex; align-items: center; gap: 10px; margin-bottom: 20px;",
            textInput(
              inputId = "search_text",
              label = NULL,
              placeholder = "Enter word or phrase to search...",
              width = "100%"
            ),
            actionButton(
              inputId = "clear_search",
              label = NULL,
              icon = icon("backspace"),
              style = "background-color: #2c3e50; color: white; border: none; padding: 8px 12px; border-radius: 4px; cursor: pointer;",
              title = "Clear Search"
            )
          ),
          
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
          
          h4("Select Legislation"),
          div(
            style = "margin-bottom: 20px;",
            div(class = "dropdown-container",
                div(
                  class = "dropdown-wrapper",
                  div(
                    style = "display: flex; align-items: center; gap: 10px;",
                    div(style = "flex-grow: 1;", uiOutput("act_dropdown")),
                    actionButton(
                      inputId = "reset_act_btn",
                      label = NULL,
                      icon = icon("sync"),
                      style = "background-color: #2c3e50; color: white; border: none; padding: 8px 12px; border-radius: 4px; cursor: pointer;",
                      title = "Reset to All Acts"
                    )
                  )
                ),
                div(
                  class = "dropdown-wrapper",
                  div(
                    style = "display: flex; align-items: center; gap: 10px;",
                    div(style = "flex-grow: 1;", uiOutput("regulation_dropdown")),
                    actionButton(
                      inputId = "reset_regulation_btn",
                      label = NULL,
                      icon = icon("sync"),
                      style = "background-color: #2c3e50; color: white; border: none; padding: 8px 12px; border-radius: 4px; cursor: pointer;",
                      title = "Reset to View Act Only"
                    )
                  )
                )
            )
          ),
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
  selected_regulation <- reactiveVal(NULL)
  search_term <- reactiveVal("")
  
  # Observe search text input
  observe({
    search_term(input$search_text)
  })
  
  # Observe clear search button
  observeEvent(input$clear_search, {
    updateTextInput(session, "search_text", value = "")
    search_term("")
  })
  
  # 🔼 Render domain buttons----
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
        selected_regulation(NULL)
      }, ignoreInit = TRUE)
    })
  })
  
  # Observe reset button
  observeEvent(input$reset_domain, {
    selected_domain(NULL)
    selected_act(NULL)
    selected_regulation(NULL)
  })
  
  # Reactive: Get paragraphs matching search term
  search_matching_paragraphs <- reactive({
    search <- search_term()
    if (is.null(search) || search == "") {
      return(NULL)
    }
    
    # Find paragraphs containing the search term (case insensitive)
    matching <- paragraph_data[grepl(search, paragraph_data$Paragraph, ignore.case = TRUE), ]
    return(matching$paragraph_id)
  })
  
  # Reactive: Filter legislation----
  filtered_legislation <- reactive({
    data <- legislation_data
    if (input$jurisdiction_filter != "All") {
      data <- subset(data, jurisdiction == input$jurisdiction_filter)
    }
    
    # Filter by domain
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
    
    # Filter by search term
    search_para_ids <- search_matching_paragraphs()
    if (!is.null(search_para_ids) && length(search_para_ids) > 0) {
      search_leg_ids <- unique(paragraph_data$legislation_id[
        paragraph_data$paragraph_id %in% search_para_ids
      ])
      data <- subset(data, legislation_id %in% search_leg_ids)
    }
    
    data
  })
  
  # 🔼 Render act dropdown----
  output$act_dropdown <- renderUI({
    acts_data <- filtered_legislation()
    acts_data <- acts_data[, c("act_name", "jurisdiction")]
    acts_data <- unique(acts_data)
    
    # Sort alphabetically by act_name
    acts_data <- acts_data[order(acts_data$act_name), ]
    
    if (nrow(acts_data) == 0) {
      return(div("No acts match the selected filters."))
    }
    
    # Always include the "All Acts" option at the top
    choices <- c("-- All Acts --" = "", setNames(acts_data$act_name, acts_data$act_name))
    
    # Set the selected value, defaulting to empty string if NULL
    selected_value <- if (is.null(selected_act())) "" else selected_act()
    
    selectInput(
      inputId = "act_select",
      label = "Acts",
      choices = choices,
      selected = selected_value,
      width = "100%"
    )
  })
  
  # Observe act selection
  observeEvent(input$act_select, {
    if (is.null(input$act_select) || input$act_select == "") {
      selected_act(NULL)
      selected_regulation(NULL)
    } else {
      selected_act(input$act_select)
      selected_regulation(NULL)
    }
  }, ignoreNULL = FALSE)
  
  # Observe act reset button
  observeEvent(input$reset_act_btn, {
    selected_act(NULL)
    selected_regulation(NULL)
  })
  
  # 🔼 Render regulation dropdown----
  output$regulation_dropdown <- renderUI({
    # Show disabled dropdown if no act is selected
    if (is.null(selected_act())) {
      return(selectInput(
        inputId = "regulation_select",
        label = "Regulations",
        choices = c("-- Select an Act First --" = ""),
        selected = "",
        width = "100%"
      ))
    }
    
    # Filter to regulations under the selected act
    regs_data <- filtered_legislation()
    regs_data <- subset(regs_data, 
                        act_name == selected_act() & 
                          legislation_type == "Regulations")
    
    if (nrow(regs_data) == 0) {
      return(selectInput(
        inputId = "regulation_select",
        label = "Regulations",
        choices = c("-- View Act Only --" = ""),
        selected = "",
        width = "100%"
      ))
    }
    
    # Always include the "View Act Only" option at the top
    choices <- c("-- View Act Only --" = "", setNames(regs_data$legislation_name, regs_data$legislation_name))
    
    # Set the selected value, defaulting to empty string if NULL
    selected_value <- if (is.null(selected_regulation())) "" else selected_regulation()
    
    selectInput(
      inputId = "regulation_select",
      label = "Regulations",
      choices = choices,
      selected = selected_value,
      width = "100%"
    )
  })
  
  # Observe regulation selection
  observeEvent(input$regulation_select, {
    if (is.null(input$regulation_select) || input$regulation_select == "") {
      selected_regulation(NULL)
    } else {
      selected_regulation(input$regulation_select)
    }
  }, ignoreNULL = FALSE)
  
  # Observe regulation reset button
  observeEvent(input$reset_regulation_btn, {
    selected_regulation(NULL)
  })
  
  # Get current legislation ID (either Act or Regulation)
  current_legislation_id <- reactive({
    if (!is.null(selected_regulation()) && selected_regulation() != "") {
      # Show regulation
      leg_data <- filtered_legislation()
      leg_id <- leg_data$legislation_id[leg_data$legislation_name == selected_regulation()]
      return(leg_id)
    } else if (!is.null(selected_act()) && selected_act() != "") {
      # Show act
      leg_data <- filtered_legislation()
      leg_id <- leg_data$legislation_id[
        leg_data$act_name == selected_act() & 
          leg_data$legislation_type == "Act"
      ]
      if (length(leg_id) > 0) {
        return(leg_id)
      }
    }
    return(NULL)
  })
  
  # ✅ Output section paragraphs with keyword highlighting and collapsible sections----
  output$section_paragraphs <- renderUI({
    leg_id <- current_legislation_id()
    
    # Get all relevant paragraphs based on filters
    search <- search_term()
    search_active <- !is.null(search) && search != ""
    
    # If search is active but no legislation selected, show all matching
    if (search_active && is.null(leg_id)) {
      leg_ids <- filtered_legislation()$legislation_id
      if (length(leg_ids) == 0) {
        return(div("No legislation matches your search."))
      }
      all_paragraphs <- paragraph_data[paragraph_data$legislation_id %in% leg_ids, ]
    } else if (!is.null(leg_id)) {
      all_paragraphs <- paragraph_data[paragraph_data$legislation_id == leg_id, ]
    } else {
      return(div("Please select an Act or enter a search term to view sections."))
    }
    
    if (nrow(all_paragraphs) == 0 || all(is.na(all_paragraphs$Section))) {
      return(div("No sections or paragraphs found."))
    }
    
    domain <- selected_domain()
    domain_clean <- if (!is.null(domain) && !is.na(domain)) trimws(tolower(domain)) else NULL
    
    if (!is.null(domain_clean)) {
      label_data$label_value <- trimws(tolower(label_data$label_value))
      domain_para_ids <- label_data$paragraph_id[
        label_data$label_type == "Management Domain" &
          label_data$label_value == domain_clean
      ]
    } else {
      domain_para_ids <- all_paragraphs$paragraph_id
    }
    
    # Further filter by search term if active
    if (search_active) {
      search_para_ids <- search_matching_paragraphs()
      domain_para_ids <- intersect(domain_para_ids, search_para_ids)
    }
    
    if (length(domain_para_ids) == 0) {
      return(div("No sections match your filters."))
    }
    
    # Create unique section identifier combining legislation_id and Section
    all_paragraphs$section_key <- paste(all_paragraphs$legislation_id, all_paragraphs$Section, sep = "_")
    
    section_groups <- split(all_paragraphs, all_paragraphs$section_key)
    
    # Sort by legislation_id first, then by section number
    section_keys <- names(section_groups)
    section_info <- data.frame(
      key = section_keys,
      leg_id = sapply(strsplit(section_keys, "_"), `[`, 1),
      section = sapply(strsplit(section_keys, "_"), function(x) paste(x[-1], collapse = "_")),
      stringsAsFactors = FALSE
    )
    section_info$section_num <- suppressWarnings(as.numeric(section_info$section))
    section_info <- section_info[order(section_info$leg_id, section_info$section_num, section_info$section, na.last = TRUE), ]
    sorted_keys <- section_info$key
    
    section_groups <- section_groups[sorted_keys]
    
    tagList(
      lapply(seq_along(sorted_keys), function(idx) {
        key <- sorted_keys[idx]
        section_data <- section_groups[[key]]
        
        matched_ids <- intersect(section_data$paragraph_id, domain_para_ids)
        if (length(matched_ids) == 0) return(NULL)
        
        sec <- section_data$Section[1]
        heading <- unique(na.omit(section_data$Heading))
        heading_text <- if (length(heading) > 0) heading[1] else "No heading available"
        
        # Get legislation info for this section
        leg_info <- legislation_data[legislation_data$legislation_id == section_data$legislation_id[1], ]
        section_label <- if (search_active && is.null(leg_id) && nrow(leg_info) > 0) {
          paste0(leg_info$legislation_name[1], " - Section ", sec)
        } else {
          paste("Section", sec)
        }
        
        aggregated_text <- paste(na.omit(section_data$Paragraph), collapse = "\n\n")
        highlighted_text <- aggregated_text
        
        # Highlight search term first (if active)
        if (search_active) {
          temp_marker <- "###HIGHLIGHT_SEARCH###"
          highlighted_text <- gsub(
            pattern = paste0("(?i)(", search, ")"),
            replacement = temp_marker,
            x = highlighted_text,
            perl = TRUE
          )
        }
        
        if (!is.null(domain_clean)) {
          domain_labels <- label_data[
            label_data$paragraph_id %in% section_data$paragraph_id &
              label_data$label_type == "Management Domain",
          ]
          
          for (kw in unique(domain_labels$keyword)) {
            if (grepl(kw, highlighted_text, ignore.case = TRUE)) {
              temp_marker <- paste0("###HIGHLIGHT_DOMAIN_", gsub(" ", "_", kw), "###")
              highlighted_text <- gsub(
                pattern = paste0("(?i)\\b", kw, "\\b"),
                replacement = temp_marker,
                x = highlighted_text,
                perl = TRUE
              )
            }
          }
          
          clause_labels <- label_data[
            label_data$paragraph_id %in% section_data$paragraph_id &
              label_data$label_type == "Clause Type",
          ]
          
          for (kw in unique(clause_labels$keyword)) {
            if (grepl(kw, highlighted_text, ignore.case = TRUE)) {
              temp_marker <- paste0("###HIGHLIGHT_CLAUSE_", gsub(" ", "_", kw), "###")
              highlighted_text <- gsub(
                pattern = paste0("(?i)\\b", kw, "\\b"),
                replacement = temp_marker,
                x = highlighted_text,
                perl = TRUE
              )
            }
          }
          
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
        
        # Replace search highlight marker
        if (search_active) {
          highlighted_text <- gsub(
            pattern = "###HIGHLIGHT_SEARCH###",
            replacement = paste0("<span class='highlight-search'>", search, "</span>"),
            x = highlighted_text,
            fixed = TRUE
          )
        }
        
        collapse_id <- paste0("collapse_section_", idx)
        
        div(
          class = "section-block",
          style = "margin-bottom: 15px; border: 1px solid #ddd; border-radius: 5px; overflow: hidden;",
          
          div(
            class = "section-header",
            style = "background-color: #f5f5f5; padding: 12px 15px; cursor: pointer; display: flex; justify-content: space-between; align-items: center;",
            `data-toggle` = "collapse",
            `data-target` = paste0("#", collapse_id),
            `aria-expanded` = "false",
            `aria-controls` = collapse_id,
            
            div(
              style = "flex-grow: 1;",
              h5(
                style = "margin: 0; display: inline-block;",
                section_label
              ),
              h6(
                style = "margin: 5px 0 0 0; color: #555; font-style: italic;",
                heading_text
              )
            ),
            
            tags$i(
              class = "fas fa-chevron-down",
              style = "transition: transform 0.3s ease;"
            )
          ),
          
          div(
            id = collapse_id,
            class = "collapse",
            div(
              class = "section-content",
              style = "padding: 15px; background-color: white; white-space: pre-wrap;",
              HTML(highlighted_text)
            )
          )
        )
      })
    )
  })
  
  # 📈 IUCN Plot----
  output$iucn_plot <- renderPlot({
    domain <- selected_domain()
    domain_clean <- if (!is.null(domain) && !is.na(domain)) trimws(tolower(domain)) else ""
    leg_id <- current_legislation_id()
    
    df <- label_data[label_data$label_type == "IUCN", ]
    
    if (nzchar(domain_clean)) {
      domain_paragraphs <- label_data$paragraph_id[
        label_data$label_type == "Management Domain" &
          trimws(tolower(label_data$label_value)) == domain_clean
      ]
      df <- df[df$paragraph_id %in% domain_paragraphs, ]
    }
    
    # Filter by search
    search_para_ids <- search_matching_paragraphs()
    if (!is.null(search_para_ids) && length(search_para_ids) > 0) {
      df <- df[df$paragraph_id %in% search_para_ids, ]
    }
    
    if (!is.null(leg_id)) {
      df <- df[df$paragraph_id %in% paragraph_data$paragraph_id[
        paragraph_data$legislation_id == leg_id
      ], ]
    } else {
      leg_ids <- filtered_legislation()$legislation_id
      if (length(leg_ids) > 0) {
        df <- df[df$paragraph_id %in% paragraph_data$paragraph_id[
          paragraph_data$legislation_id %in% leg_ids
        ], ]
      }
    }
    
    validate(need(nrow(df) > 0, "No data available for IUCN plot."))
    
    iucn_counts <- df %>%
      count(label_value) %>%
      arrange(desc(n)) %>%
      mutate(label_value = factor(label_value, levels = label_value))
    
    ggplot(iucn_counts, aes(x = label_value, y = n)) +
      geom_bar(stat = "identity", fill = "#2c3e50") +
      theme_minimal() +
      labs(x = "IUCN Level 2", y = "Clause Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # 📈 Clause Type Plot----
  output$clause_plot <- renderPlot({
    library(dplyr)
    library(tidyr)
    
    domain <- selected_domain()
    domain_clean <- if (!is.null(domain) && !is.na(domain)) trimws(tolower(domain)) else NULL
    
    leg_id <- current_legislation_id()
    
    if (!is.null(leg_id)) {
      relevant_paragraphs <- paragraph_data[paragraph_data$legislation_id == leg_id, ]
      
      if (!is.null(domain_clean)) {
        label_data_normalized <- label_data
        label_data_normalized$label_value <- trimws(tolower(label_data_normalized$label_value))
        
        domain_para_ids <- label_data_normalized$paragraph_id[
          label_data_normalized$label_type == "Management Domain" &
            label_data_normalized$label_value == domain_clean
        ]
        
        sections_with_domain <- unique(relevant_paragraphs$Section[
          relevant_paragraphs$paragraph_id %in% domain_para_ids
        ])
        
        relevant_paragraphs <- relevant_paragraphs[
          relevant_paragraphs$Section %in% sections_with_domain,
        ]
      }
      
      # Filter by search
      search_para_ids <- search_matching_paragraphs()
      if (!is.null(search_para_ids) && length(search_para_ids) > 0) {
        relevant_paragraphs <- relevant_paragraphs[
          relevant_paragraphs$paragraph_id %in% search_para_ids,
        ]
      }
      
      domain_paragraphs <- relevant_paragraphs$paragraph_id
      
    } else if (!is.null(domain_clean)) {
      label_data_normalized <- label_data
      label_data_normalized$label_value <- trimws(tolower(label_data_normalized$label_value))
      
      domain_paragraphs <- label_data_normalized$paragraph_id[
        label_data_normalized$label_type == "Management Domain" &
          label_data_normalized$label_value == domain_clean
      ]
      
      # Filter by search
      search_para_ids <- search_matching_paragraphs()
      if (!is.null(search_para_ids) && length(search_para_ids) > 0) {
        domain_paragraphs <- intersect(domain_paragraphs, search_para_ids)
      }
    } else {
      leg_ids <- filtered_legislation()$legislation_id
      domain_paragraphs <- paragraph_data$paragraph_id[
        paragraph_data$legislation_id %in% leg_ids
      ]
      
      # Filter by search
      search_para_ids <- search_matching_paragraphs()
      if (!is.null(search_para_ids) && length(search_para_ids) > 0) {
        domain_paragraphs <- intersect(domain_paragraphs, search_para_ids)
      }
    }
    
    validate(need(length(domain_paragraphs) > 0, "No paragraphs found."))
    
    label_data_plot <- label_data %>%
      mutate(label_value = trimws(tolower(label_value)))
    
    co_labels <- label_data_plot %>%
      filter(paragraph_id %in% domain_paragraphs,
             label_type %in% c("Management Domain", "Clause Type")) %>%
      select(paragraph_id, label_type, label_value)
    
    validate(need(nrow(co_labels) > 0, "No label data available."))
    
    # Pivot to wide format with renamed columns to avoid backtick issues
    co_occurrence <- co_labels %>%
      group_by(paragraph_id, label_type) %>%
      summarise(label_value = paste(unique(label_value), collapse = ";"), .groups = "drop") %>%
      pivot_wider(names_from = label_type, values_from = label_value, values_fill = "") %>%
      rename(ManagementDomain = `Management Domain`, ClauseType = `Clause Type`) %>%
      filter(ManagementDomain != "", ClauseType != "")
    
    # Split multi-labels and count co-occurrences
    co_counts <- co_occurrence %>%
      separate_rows(ManagementDomain, sep = ";") %>%
      separate_rows(ClauseType, sep = ";") %>%
      count(ManagementDomain, ClauseType, name = "Count")
    
    validate(need(nrow(co_counts) > 0, "No clause type co-occurrence data available."))
    
    ggplot(co_counts, aes(x = ManagementDomain, y = ClauseType, fill = Count)) +
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
    leg_id <- current_legislation_id()
    
    df <- label_data[label_data$label_type == "Management Domain", ]
    if (!is.null(domain)) {
      df <- df[df$label_value == domain, ]
    }
    
    # Filter by search
    search_para_ids <- search_matching_paragraphs()
    if (!is.null(search_para_ids) && length(search_para_ids) > 0) {
      df <- df[df$paragraph_id %in% search_para_ids, ]
    }
    
    if (!is.null(leg_id)) {
      para_ids <- paragraph_data$paragraph_id[paragraph_data$legislation_id == leg_id]
      df <- df[df$paragraph_id %in% para_ids, ]
    } else {
      leg_ids <- filtered_legislation()$legislation_id
      if (length(leg_ids) > 0) {
        para_ids <- paragraph_data$paragraph_id[paragraph_data$legislation_id %in% leg_ids]
        df <- df[df$paragraph_id %in% para_ids, ]
      }
    }
    
    # Check if df has any rows before proceeding
    validate(need(nrow(df) > 0, "No keyword data available."))
    
    keyword_counts <- as.data.frame(table(df$keyword), stringsAsFactors = FALSE)
    
    # Check if keyword_counts has rows
    validate(need(nrow(keyword_counts) > 0, "No keyword data available."))
    
    colnames(keyword_counts) <- c("keyword", "count")
    keyword_counts <- keyword_counts[order(-keyword_counts$count), ]
    keyword_counts <- keyword_counts[1:min(10, nrow(keyword_counts)), ]
    
    ggplot(keyword_counts, aes(x = reorder(keyword, count), y = count)) +
      geom_bar(stat = "identity", fill = "#2c3e50") +
      coord_flip() +
      theme_minimal() +
      labs(x = "Keyword", y = "Frequency")
  })
  
  onStop(function() {
    dbDisconnect(conn)
  })
}

shinyApp(ui = ui, server = server)