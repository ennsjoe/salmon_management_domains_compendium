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
library(plotly)

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

# Load legislation URLs
legislation_url <- tryCatch({
  message("Attempting to load legislation_url table...")
  dbReadTable(conn, "legislation_url")
}, error = function(e) {
  message("Error loading legislation_url table: ", e$message)
  data.frame(legislation_name = character(), url = character(), stringsAsFactors = FALSE)
})

# Debug: Check what we loaded
message("Debug - legislation_url rows loaded: ", nrow(legislation_url))

# Create a named vector for quick URL lookup - filter out empty URLs
if (nrow(legislation_url) > 0) {
  message("Debug - Processing URL data...")
  
  # Remove rows with empty, NA, or whitespace-only URLs
  legislation_url$url <- trimws(legislation_url$url)
  legislation_url_clean <- legislation_url[
    !is.na(legislation_url$url) & 
      legislation_url$url != "" & 
      nchar(legislation_url$url) > 0, 
  ]
  message("Debug - Clean rows (non-empty URLs): ", nrow(legislation_url_clean))
  
  if (nrow(legislation_url_clean) > 0) {
    url_lookup <- setNames(legislation_url_clean$url, legislation_url_clean$legislation_name)
    message("Debug - url_lookup created with length: ", length(url_lookup))
    message("Debug - Sample names: ", paste(head(names(url_lookup), 3), collapse = ", "))
  } else {
    url_lookup <- character(0)
    message("Debug - All URLs were empty, url_lookup is empty")
  }
} else {
  url_lookup <- character(0)
  message("Debug - No data in legislation_url, url_lookup is empty")
}

# Prepare UI choices
management_domains <- unique(label_data$label_value[label_data$label_type == "Management Domain"])
jurisdictions <- unique(legislation_data$jurisdiction)

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
    "))
    ,
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
  
  titlePanel("Legislation Applicable to Pacific Salmon and Ecosystems (LAPSE)"),
  
  # About page button
  div(
    style = "margin-bottom: 20px; text-align: center;",
    tags$a(
      href = "https://ennsjoe.github.io/salmon_management_domains_compendium/LAPSE-Dashboard-About.html",
      target = "_blank",
      style = "display: inline-block; padding: 12px 24px; background-color: #996666; color: white; font-weight: 600; text-decoration: none; font-size: 16px; border-radius: 5px; transition: background-color 0.3s;",
      onmouseover = "this.style.backgroundColor='#aa7777'",
      onmouseout = "this.style.backgroundColor='#996666'",
      icon("info-circle"),
      " About LAPSE Dashboard"
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
            ),
            # Add link button below dropdowns
            div(
              style = "margin-top: 10px;",
              uiOutput("legislation_link_button")
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
          tabsetPanel(
            tabPanel("Keyword Frequency",
                     br(),
                     plotlyOutput("keyword_plot", height = "400px")
            ),
            tabPanel("IUCN Threats",
                     br(),
                     plotlyOutput("iucn_plot", height = "400px")
            )
          )
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
    tryCatch({
      selected <- selected_domain()
      
      tagList(
        # Always show the reset button
        div(
          class = "domain-button reset",
          `onclick` = "Shiny.setInputValue('reset_domain', Math.random())",
          tagList(icon("refresh"), "All")
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
    }, error = function(e) {
      div("Error in domain buttons:", e$message)
    })
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
  
  # Render legislation link button
  output$legislation_link_button <- renderUI({
    leg_id <- current_legislation_id()
    
    if (is.null(leg_id) || length(leg_id) == 0) {
      return(NULL)
    }
    
    # Get legislation info
    leg_info <- legislation_data[legislation_data$legislation_id == leg_id, ]
    
    if (nrow(leg_info) == 0) {
      return(NULL)
    }
    
    leg_name <- leg_info$legislation_name[1]
    leg_url <- url_lookup[[leg_name]]
    
    if (is.null(leg_url) || is.na(leg_url) || leg_url == "") {
      return(NULL)
    }
    
    # Create button
    tags$a(
      href = leg_url,
      target = "_blank",
      class = "btn btn-primary",
      style = "background-color: #0074D9; border: none; display: inline-flex; align-items: center; gap: 8px; padding: 8px 16px;",
      icon("external-link-alt"),
      "View Full Legislation"
    )
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
        
        # Determine jurisdiction color
        jurisdiction_color <- if (nrow(leg_info) > 0) {
          switch(leg_info$jurisdiction[1],
                 "Federal" = "#996666",
                 "Provincial" = "#668899",
                 "Unknown" = "#999999",
                 "#999999")
        } else {
          "#999999"
        }
        
        # Build section label with jurisdiction indicator
        section_label <- if (search_active && is.null(leg_id) && nrow(leg_info) > 0) {
          # Full label with act name and jurisdiction
          paste0(
            leg_info$act_name[1], 
            " (", leg_info$jurisdiction[1], ") | ",
            leg_info$legislation_name[1], 
            " - Section ", sec
          )
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
            style = sprintf(
              "background-color: #f5f5f5; padding: 12px 15px; cursor: pointer; display: flex; justify-content: space-between; align-items: center; border-left: 4px solid %s;",
              jurisdiction_color
            ),
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
  # output$iucn_plot <- renderPlot({
  #   domain <- selected_domain()
  #   domain_clean <- if (!is.null(domain) && !is.na(domain)) trimws(tolower(domain)) else ""
  #   leg_id <- current_legislation_id()
  # 
  # 
  # 
  #   df <- label_data[label_data$label_type == "IUCN", ]
  # 
  #   if (nzchar(domain_clean)) {
  #     domain_paragraphs <- label_data$paragraph_id[
  #       label_data$label_type == "Management Domain" &
  #         trimws(tolower(label_data$label_value)) == domain_clean
  #     ]
  #     df <- df[df$paragraph_id %in% domain_paragraphs, ]
  #   }
  # 
  #   # Filter by search
  #   search_para_ids <- search_matching_paragraphs()
  #   if (!is.null(search_para_ids) && length(search_para_ids) > 0) {
  #     df <- df[df$paragraph_id %in% search_para_ids, ]
  #   }
  # 
  #   if (!is.null(leg_id)) {
  #     df <- df[df$paragraph_id %in% paragraph_data$paragraph_id[
  #       paragraph_data$legislation_id == leg_id
  #     ], ]
  #   } else {
  #     leg_ids <- filtered_legislation()$legislation_id
  #     if (length(leg_ids) > 0) {
  #       df <- df[df$paragraph_id %in% paragraph_data$paragraph_id[
  #         paragraph_data$legislation_id %in% leg_ids
  #       ], ]
  #     }
  #   }
  # 
  #   validate(need(nrow(df) > 0, "No data available for IUCN plot."))
  # 
  #   iucn_counts <- df %>%
  #     count(label_value) %>%
  #     arrange(desc(n)) %>%
  #     mutate(label_value = factor(label_value, levels = label_value))
  # 
  #   ggplot(iucn_counts, aes(x = label_value, y = n)) +
  #     geom_bar(stat = "identity", fill = "#2c3e50") +
  #     theme_minimal() +
  #     labs(x = "", y = "Clause Count") +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # })
  # 
  # 
  
  output$iucn_plot <- renderPlotly({
    tryCatch({
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
      
      # Prepare data with short codes and tooltip text
      iucn_counts <- df %>%
        count(label_value) %>%
        arrange(desc(n)) %>%  # Sort by count (largest first)
        mutate(
          threat_code = sub("^(\\d+\\.\\d+).*", "\\1", label_value),
          # Create factor to preserve sorted order
          threat_code = factor(threat_code, levels = threat_code),
          # Create custom tooltip text
          tooltip_text = paste0(
            "Threat: ", label_value, "\n",
            "Count: ", n
          )
        )
      
      # Create ggplot with custom tooltip text in aes
      p <- ggplot(iucn_counts, aes(x = threat_code, y = n, 
                                   text = tooltip_text)) +
        geom_col(fill = "#2c3e50") +
        theme_minimal() +
        labs(x = "IUCN Threat Code", y = "Clause Count") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      # Convert to plotly with custom tooltip
      ggplotly(p, tooltip = "text")
    }, error = function(e) {
      p <- ggplot() + 
        annotate("text", x = 0, y = 0, label = paste("Error in IUCN plot:", e$message), size = 4) + 
        theme_void()
      ggplotly(p)
    })
  })
  # 📈 Clause Type Plot----
  output$clause_plot <- renderPlotly({
    tryCatch({
      
      domain <- selected_domain()
      domain_clean <- if (!is.null(domain) && !is.na(domain)) trimws(tolower(domain)) else NULL
      
      leg_id <- current_legislation_id()
      
      # 🎯 Extract Clause Type and Management Domain labels
      clause_labels <- label_data[label_data$label_type == "Clause Type", 
                                  c("paragraph_id", "label_value")]
      names(clause_labels) <- c("paragraph_id", "clause_type")
      
      domain_labels <- label_data[label_data$label_type == "Management Domain", 
                                  c("paragraph_id", "label_value")]
      names(domain_labels) <- c("paragraph_id", "management_domain")
      
      # 🔗 Merge clause and domain labels
      labeled_paragraphs <- merge(clause_labels, domain_labels, by = "paragraph_id")
      
      # 🔗 Add jurisdiction and section context
      paragraph_context <- merge(
        paragraph_data[, c("paragraph_id", "legislation_id", "Section")],
        legislation_data[, c("legislation_id", "jurisdiction")],
        by = "legislation_id",
        all.x = TRUE
      )
      labeled_paragraphs <- merge(labeled_paragraphs, paragraph_context, by = "paragraph_id", all.x = TRUE)
      
      # Apply filters
      if (!is.null(leg_id)) {
        labeled_paragraphs <- labeled_paragraphs[labeled_paragraphs$legislation_id == leg_id, ]
      } else {
        leg_ids <- filtered_legislation()$legislation_id
        if (length(leg_ids) > 0) {
          labeled_paragraphs <- labeled_paragraphs[labeled_paragraphs$legislation_id %in% leg_ids, ]
        }
      }
      
      if (!is.null(domain_clean)) {
        labeled_paragraphs$management_domain_lower <- trimws(tolower(labeled_paragraphs$management_domain))
        labeled_paragraphs <- labeled_paragraphs[labeled_paragraphs$management_domain_lower == domain_clean, ]
      }
      
      # Filter by search
      search_para_ids <- search_matching_paragraphs()
      if (!is.null(search_para_ids) && length(search_para_ids) > 0) {
        labeled_paragraphs <- labeled_paragraphs[labeled_paragraphs$paragraph_id %in% search_para_ids, ]
      }
      
      validate(need(nrow(labeled_paragraphs) > 0, "No clause type data available."))
      
      # 📊 Aggregate counts by unique Section-legislation_id combinations
      labeled_paragraphs$section_key <- paste(labeled_paragraphs$Section, labeled_paragraphs$legislation_id)
      clause_domain_counts <- labeled_paragraphs %>%
        filter(!is.na(clause_type) & !is.na(management_domain) & !is.na(Section)) %>%
        group_by(clause_type, management_domain) %>%
        summarise(section_count = n_distinct(section_key), .groups = "drop")
      
      validate(need(nrow(clause_domain_counts) > 0, "No clause type co-occurrence data available."))
      
      # 📈 Calculate percentages within each management domain
      percent_data <- clause_domain_counts %>%
        group_by(management_domain) %>%
        mutate(
          total = sum(section_count),
          percent = 100 * section_count / total
        ) %>%
        ungroup()
      
      # 🧮 Order management domains by total section count (largest to smallest)
      domain_order <- percent_data %>%
        group_by(management_domain) %>%
        summarise(total = sum(section_count), .groups = "drop") %>%
        arrange(desc(total))
      
      percent_data$management_domain <- factor(percent_data$management_domain, 
                                               levels = rev(domain_order$management_domain))
      
      # Get number of clause types for color palette
      n_clause_types <- length(unique(percent_data$clause_type))
      
      # 🎨 Generate colors from RColorBrewer Set2
      if (n_clause_types <= 8) {
        clause_colors <- RColorBrewer::brewer.pal(max(3, n_clause_types), "Set2")[1:n_clause_types]
      } else {
        clause_colors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_clause_types)
      }
      names(clause_colors) <- unique(percent_data$clause_type)
      
      # Create hover text
      percent_data$hover_text <- paste0(
        "Management Domain: ", percent_data$management_domain, "<br>",
        "Clause Type: ", percent_data$clause_type, "<br>",
        "Sections: ", percent_data$section_count, "<br>",
        "Percentage: ", round(percent_data$percent, 1), "%"
      )
      
      # 🎨 Create ggplot stacked percentage bar chart
      p <- ggplot(percent_data, aes(x = management_domain, y = percent, 
                                    fill = clause_type, text = hover_text)) +
        geom_bar(stat = "identity", color = "white", linewidth = 0.3) +
        coord_flip() +
        scale_fill_manual(values = clause_colors, name = "Clause type") +
        scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
        labs(
          x = NULL,
          y = "Percentage of sections (%)"
        ) +
        theme_minimal() +
        theme(
          axis.text.y = element_text(size = 7, hjust = 1),
          axis.text.x = element_text(size = 8, angle = 0),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 10, margin = margin(t = 10)),
          legend.position = "bottom",
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = margin(t = 5, r = 10, b = 5, l = 5)
        )
      
      # Convert to plotly with custom tooltip
      ggplotly(p, tooltip = "text") %>%
        layout(
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.2,
            font = list(size = 9),
            itemsizing = "constant",
            tracegroupgap = 5,
            itemwidth = 40
          ),
          margin = list(l = 10, r = 50, t = 30, b = 100),
          xaxis = list(
            title = list(standoff = 15),
            tickmode = "linear",
            dtick = 25,
            tickfont = list(size = 9)
          ),
          yaxis = list(title = "")
        )
      
    }, error = function(e) {
      plot_ly() %>%
        add_annotations(
          text = paste("Error in clause plot:", e$message),
          showarrow = FALSE,
          font = list(size = 14)
        ) %>%
        layout(
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
    })
  })  
  
  
  # 📈 Keyword Frequency Plot----
  output$keyword_plot <- renderPlotly({  # Changed from renderPlot
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
    
    # Add tooltip text
    keyword_counts$tooltip_text <- paste0(
      "Keyword: ", keyword_counts$keyword, "\n",
      "Count: ", keyword_counts$count
    )
    
    # Create ggplot with tooltip aesthetic
    p <- ggplot(keyword_counts, aes(x = reorder(keyword, count), y = count,
                                    text = tooltip_text)) +
      geom_bar(stat = "identity", fill = "#2c3e50") +
      coord_flip() +
      theme_minimal() +
      labs(x = "Keyword", y = "Count")
    
    # Convert to plotly
    ggplotly(p, tooltip = "text")
  })
  
  onStop(function() {
    dbDisconnect(conn)
  })
}

shinyApp(ui = ui, server = server)