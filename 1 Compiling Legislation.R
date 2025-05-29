#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# PART 1 - COMPILING LEGISLATION
# This script uses HTML files of legislation specific to Pacific salmon (Type A) to map 
# keywords to build further queries in CanLii (https://www.canlii.org/)
# Outputs: HTML Files saved to 2 folders (Type A Legislation, Type B Legislation)
# Note: Downloading HTML files is a manual process, but could be automated in the
# future using APIs
#///////////////////////////////////////////////////////////////////////////////

## **BEFORE STARTING - Download Type A Legislation HTML files ------------------
# Query CanLii website for (salmon OR sockeye OR chinook OR coho OR chum) - Type A Keywords
# Download html files to folder called Type A Legislation in working directory

# SETUP ------------------------------------------------------------------------
## i) Set Working Directory ----------------------------------------------------
library(here)

# Get the root directory of the project
here()

## ii) Load Libraries ----------------------------------------------------------
library(data.table)
library(udpipe)
library(xml2)
library(rvest)


library(shiny)
library(DT)
library(shinyWidgets)
library(miniUI)  # Required for runGadget()
library(stringr)
library(tidytext)  # For NLP tasks
library(dplyr)
library(tidyr)

## iii) Load Udpipe model for word frequency analysis---------------------------
udpipe_model_path <- here("english-ewt-ud-2.5-191206.udpipe")

## iv) Create the salmon_keywords data table------------------------------------
salmon_keywords <- data.table(
  Keyword = c("salmon", "chinook", "sockeye", "coho", "chum"),
  Specificity = 1,
  Frequency = NA
)

## v) Read in Type A Legislation Folder in working directory--------------------
# Define the folder path using `here()`
html_dir <- here("Type A Legislation")

# Check if directory exists
if (!dir.exists(html_dir)) {
  stop("Error: Directory not found! Please check the path.")
}

# Get list of all HTML files in the directory (including subdirectories)
html_files <- list.files(path = html_dir, pattern = "\\.html$", full.names = TRUE, recursive = TRUE)

# Debugging: Confirm files found
cat("Total HTML files detected:", length(html_files), "\n")

# If no files are found, stop execution
if (length(html_files) == 0) {
  stop("No HTML files found in the specified directory.")
}

# Read files safely with error handling
file_contents <- lapply(html_files, function(f) {
  tryCatch(readLines(f, encoding = "UTF-8"), error = function(e) {
    warning(paste("Could not read file:", f))
    return(NULL)
  })
})

# 1.1 - Use Keyword Mapping to Compile Type B Legislation ------------
  # 1.1.1 - Parse Type A Legislation (heading, grouping paragraphs, and assigning section numbers) ----


# Define the folder dynamically using `here()`
html_dir <- here("Type A Legislation")

# Ensure the directory exists
if (!dir.exists(html_dir)) stop("Error: Directory does not exist. Check the path.")

# Read all HTML files (handling special characters)
html_files <- list.files(path = html_dir, pattern = "\\.html$", full.names = TRUE, recursive = TRUE)

# Normalize paths to handle special characters
html_files <- normalizePath(html_files, winslash = "/", mustWork = FALSE)

# Debugging print: Confirm files found
cat("Total HTML files detected:", length(html_files), "\n")

# Check if no files are found
if (length(html_files) == 0) stop("No HTML files found in the specified directory.")

############################################

# Load required packages
library(rvest)
library(xml2)
library(data.table)

# Initialize Paragraphs_DT
Paragraphs_DT <- data.table(
  `Legislation Name` = character(),
  `Section Number` = character(),
  `XPath` = character(),
  `Paragraph Text` = character()
)

# Function to extract section numbers inside the same `<p>` tag
extract_inline_section <- function(node) {
  section_label <- node %>% html_nodes("a.sectionLabel span.sectionLabel") %>% html_text(trim = TRUE)
  if (length(section_label) > 0) return(section_label) else return(NA)
}

# Function to extract the legislation name correctly
extract_legislation_name <- function(html_file) {
  legislation_name <- html_file %>% html_nodes("h1.HeadTitle, div#title h2") %>% html_text(trim = TRUE)
  legislation_name <- ifelse(length(legislation_name) > 0, legislation_name[1], "Unknown Legislation")
  return(trimws(legislation_name))
}

# Process each HTML file
for (file in html_files) {
  html_file <- read_html(file)
  
  # **Extract legislation name**
  legislation_name <- extract_legislation_name(html_file)
  
  # Extract paragraph nodes
  paragraph_nodes <- html_file %>% html_nodes("p")
  
  last_section_number <- NA
  last_section_xpath <- NA
  
  # Assign paragraphs to correct section
  for (i in seq_along(paragraph_nodes)) {
    # Check if a section number exists within this paragraph
    inline_section_number <- extract_inline_section(paragraph_nodes[i])
    
    if (!is.na(inline_section_number)) {
      last_section_number <- inline_section_number
      last_section_xpath <- xml_path(paragraph_nodes[i])  # Use the current paragraph's XPath
    } else {
      # If no inline section found, locate the nearest preceding section label
      preceding_section_node <- xml_find_first(paragraph_nodes[i], xpath = "preceding::*[self::span[@class='sectionLabel'] or self::a[@class='sectionLabel']][1]")
      if (!is.null(preceding_section_node)) {
        last_section_xpath <- xml_path(preceding_section_node)
        last_section_number <- xml_text(preceding_section_node, trim = TRUE)
      }
    }
    
    paragraph_text <- xml_text(paragraph_nodes[i], trim = TRUE)
    
    if (nzchar(paragraph_text)) {
      Paragraphs_DT <- rbind(Paragraphs_DT, data.table(
        `Legislation Name` = legislation_name,
        `Section Number` = last_section_number,
        `XPath` = xml_path(paragraph_nodes[i]),
        `Paragraph Text` = paragraph_text
      ), fill = TRUE)
    }
  }
}

# Replace missing section numbers with NA
Paragraphs_DT[is.na(`Section Number`), `Section Number` := NA]

# Debugging print: Preview final structured data
print(Paragraphs_DT)











#####################################################
#####################################################






normalized_files <- normalizePath(html_files, winslash = "/")

# Initialize datatables with the correct column order
headings_dt_a <- data.table(
  Heading = character(),
  `Section Number` = character(),
  XPath = character()
)
paragraphs_dt_a <- data.table(
  `Legislation Name` = character(),
  `Act Name` = character(),
  Heading = character(),
  Paragraphs = character(),
  XPath = character()
)

# Function to clean text
clean_text <- function(text) {
  text <- gsub("[Ââ€™]", "", text)
  text <- iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT")
  trimws(text)
}

# Process each HTML file
for (file in html_files) {
  html_file <- read_html(file)
  
  # Extract jurisdiction
  head_attrs <- xml_attrs(html_file %>% html_node("head"))
  meta_description <- html_file %>% html_node("meta[name='description']") %>% html_attr("content")
  meta_breadcrumb <- html_file %>% html_node("meta[name='breadcrumb']") %>% html_attr("content")
  
  jurisdiction <- ifelse(any(grepl("www.gov.bc.ca", head_attrs)) || grepl("British Columbia", meta_breadcrumb), "Provincial",
                         ifelse(!is.na(meta_description) && grepl("Federal laws of Canada", meta_description), "Federal", "Unknown"))
  
  # Extract legislation name
  legislation_name <- html_file %>% html_nodes("h1, div#title h2") %>% html_text(trim = TRUE)
  legislation_name <- ifelse(length(legislation_name) > 0, clean_text(legislation_name[1]), "Unknown Legislation")
  
  # Extract legislation type
  legislation_type <- fifelse(
    grepl("\\bRegulation\\b", legislation_name, ignore.case = TRUE) | 
      grepl("\\bRegulations\\b", legislation_name, ignore.case = TRUE), "Regulations",
    fifelse(grepl("\\bOrder\\b", legislation_name, ignore.case = TRUE) & grepl("\\bAct\\b", legislation_name, ignore.case = TRUE), "Order",
            fifelse(grepl("\\bOrder\\b", legislation_name, ignore.case = TRUE), "Order",
                    fifelse(grepl("\\bAct\\b", legislation_name, ignore.case = TRUE), "Act", ""))))
  
  # Assign Act Name: If Legislation Type is "Act", use Legislation Name, else extract from HTML
  act_name <- fifelse(legislation_type == "Act", legislation_name, 
                      html_file %>% html_node("p.EnablingAct a, div#actname h2") %>% html_text(trim = TRUE))
  
  act_name <- ifelse(length(act_name) > 0, clean_text(act_name), legislation_name)
  
  # Extract headings, keeping Marginal Notes
  heading_nodes <- html_file %>% html_nodes("p.MarginalNote, h4, h3, h2")
  heading_texts <- clean_text(gsub("^Marginal note:\\s*", "", heading_nodes %>% html_text(trim = TRUE)))
  heading_xpaths <- sapply(heading_nodes, xml_path, USE.NAMES = FALSE)
  
  # Extract section numbers efficiently
  last_section_num <- NA
  section_numbers <- character(length(heading_nodes))
  
  for (i in seq_along(heading_nodes)) {
    section_node <- xml_find_first(heading_nodes[i], xpath = "following-sibling::*[self::p or self::div or self::ul][1]//*[contains(@class, 'sectionLabel') or contains(@class, 'secnumholder')]")
    section_num <- ifelse(!is.null(section_node), xml_text(section_node, trim = TRUE), NA)
    
    section_numbers[i] <- ifelse(!is.na(section_num) && section_num != "", section_num, last_section_num)
    if (!is.na(section_num) && section_num != "") {
      last_section_num <- section_num
    }
  }
  
  # Store headings data
  headings_dt_a <- rbind(headings_dt_a, data.table(
    Jurisdiction = jurisdiction,
    `Legislation Name` = legislation_name,
    `Legislation Type` = legislation_type,
    `Act Name` = act_name,
    Heading = heading_texts,
    `Section Number` = section_numbers,
    XPath = as.character(heading_xpaths)
  ), fill = TRUE)
  
  # Extract paragraphs, removing Marginal Notes
  paragraph_nodes <- html_file %>% html_nodes("p.Section, p.Subsection, p.para, ul p, div.paragWrapper p, p.sec.nosubsecnum, p.sec")
  
  paragraph_texts <- clean_text(paragraph_nodes %>% html_text(trim = TRUE))
  paragraph_xpaths <- sapply(paragraph_nodes, xml_path, USE.NAMES = FALSE)
  
  # Assign paragraphs to the closest preceding heading
  assigned_paragraphs <- data.table(
    `Legislation Name` = character(),
    `Act Name` = character(),
    Heading = character(),
    Paragraphs = character(),
    XPath = character()
  )
  
  last_heading <- NA
  last_xpath <- NA
  
  for (i in seq_along(paragraph_nodes)) {
    preceding_heading <- xml_find_all(paragraph_nodes[i], xpath = "preceding::*[self::p[@class='MarginalNote'] or self::h4 or self::h3 or self::h2][1]")
    assigned_heading <- ifelse(length(preceding_heading) > 0, xml_text(preceding_heading[length(preceding_heading)], trim = TRUE), last_heading)
    assigned_heading <- gsub("^Marginal note:\\s*", "", assigned_heading)
    
    if (!is.na(assigned_heading) && assigned_heading != "") {
      last_heading <- assigned_heading
      last_xpath <- ifelse(length(preceding_heading) > 0, xml_path(preceding_heading[length(preceding_heading)]), last_xpath)
    }
    
    cleaned_paragraph <- clean_text(paragraph_texts[i])
    
    assigned_paragraphs <- rbind(assigned_paragraphs, data.table(
      `Legislation Name` = legislation_name,
      `Act Name` = act_name,
      Heading = assigned_heading,
      Paragraphs = cleaned_paragraph,
      XPath = as.character(last_xpath)
    ), fill = TRUE)
  }
  
  paragraphs_dt_a <- rbind(paragraphs_dt_a, assigned_paragraphs, fill = TRUE)
}

# Remove Marginal Note paragraphs from Paragraphs
paragraphs_dt_a <- paragraphs_dt_a[!grepl("^Marginal note:", Paragraphs, ignore.case = TRUE)]

# Remove rows where Paragraphs contains "repealed" or "revoked"
paragraphs_dt_a <- paragraphs_dt_a[!grepl("\\b(repealed|revoked)\\b", Paragraphs, ignore.case = TRUE)]

# Remove rows from headings_dt_a where `Section Number` is NA
headings_dt_a <- headings_dt_a[!is.na(`Section Number`)]

# Merge paragraphs with headings
grouped_paragraphs <- paragraphs_dt_a[, .(Paragraphs = paste(Paragraphs, collapse = " ")), by = .(`Legislation Name`, XPath)]
Type_A_parsed <- merge(headings_dt_a, grouped_paragraphs, by = c("Legislation Name", "XPath"), all.x = TRUE)[, -"XPath"]

# Ensure structured ordering
Type_A_parsed <- Type_A_parsed[order(`Legislation Name`, as.numeric(`Section Number`), na.last = TRUE)]

# Remove rows where Paragraphs is NA
Type_A_parsed <- Type_A_parsed[!is.na(Paragraphs)]

### 1.1.3 - Filter for Type A Procedural Elements ------------------------------
# Filter so that only paragraphs with salmon/chinook/coho/sockeye/chum remain

# Filter Type_A_parsed to keep only rows containing stand-alone words
Type_A_filtered <- Type_A_parsed[
  grepl("\\b(salmon|coho|chinook|sockeye|chum)\\b", Paragraphs, ignore.case = TRUE)
]

### 1.1.4 - Extract Type A Unigrams --------------------------------------------

ud_model <- udpipe_load_model(udpipe_model_path)

# Perform annotation on the Paragraph column
annotation <- udpipe_annotate(ud_model, x = tolower(Type_A_filtered$Paragraph))  # Convert text to lowercase before processing
annotation <- as.data.table(annotation)

# Filter for unigrams that are only nouns
filtered_unigrams <- annotation[upos == "NOUN", .(Keyword = tolower(lemma))]  # Ensure lemmas are lowercase

# Exclude unigrams containing punctuation or single-letter unigrams
filtered_unigrams <- filtered_unigrams[!grepl("[[:punct:]]", Keyword) & nchar(Keyword) > 1]

# Compute frequency count and exclude unigrams with a frequency of 1
unigram_freq <- filtered_unigrams[, .(Frequency = .N), by = Keyword]
unigram_freq <- unigram_freq[Frequency > 1]

# Create the final datatable
Type_A_unigrams <- unigram_freq

### 1.1.5 - Filter S1 and IUCN Keywords-----------------------------------------

# Compute frequency quartiles
freq_threshold <- quantile(Type_A_unigrams$Frequency, 0.75)

# Filter to include only unigrams in the top quartile frequency
Type_A_unigrams_high_freq <- Type_A_unigrams[Frequency >= freq_threshold]

# Remove unigrams that exist in mgmt_d_keywords and salmon_keywords Keyword columns
Type_A_unigrams_filtered <- Type_A_unigrams_high_freq[
  !Keyword %in% mgmt_d_keywords$Keyword & !Keyword %in% salmon_keywords$Keyword
]

### 1.1.6 - USER: Curate the keyword list --------------------------------------

# Ensure all datasets exist
if (!exists("Type_A_unigrams_filtered") || !exists("mgmt_d_keywords")) {
  stop("Error: One or more required datasets do not exist in the global environment.")
}

# Convert datasets to data.table format
Type_A_unigrams_filtered <- as.data.table(Type_A_unigrams_filtered)
mgmt_d_keywords <- as.data.table(mgmt_d_keywords)

# Assign unique row IDs for Type_A_unigrams_filtered
Type_A_unigrams_filtered[, ID := .I]

# Define UI for Shiny App
ui <- fluidPage(
  titlePanel("Select Keywords Most Relevant to Salmon Management"),
  
  actionButton("submit", "Submit Selection"),
  actionButton("select_all", "Select All / Deselect All"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("checkbox_ui")  # Dynamically generated checkboxes
    ),
    mainPanel(HTML(""))  # Empty main panel
  )
)

# Define Server Logic for Shiny App
server <- function(input, output, session) {
  
  # Dynamically generate checkboxes for keyword selection
  output$checkbox_ui <- renderUI({
    checkboxGroupInput("selected_keywords", "Select Other Salmon-Specific Keywords to Filter Legislation:", 
                       choices = Type_A_unigrams_filtered$Keyword)
  })
  
  # Select All / Deselect All functionality
  observeEvent(input$select_all, {
    if (length(input$selected_keywords) == nrow(Type_A_unigrams_filtered)) {
      updateCheckboxGroupInput(session, "selected_keywords", selected = character(0))  # Deselect all
    } else {
      updateCheckboxGroupInput(session, "selected_keywords", selected = Type_A_unigrams_filtered$Keyword)  # Select all
    }
  })
  
  # Reactive dataset for storing selected keywords
  Type_A_keywords_curated <- reactive({
    req(input$selected_keywords)  # Ensure selection is not empty
    Type_A_unigrams_filtered[Keyword %in% input$selected_keywords, .(Keyword)]
  })
  
  # Assign selected keywords to the global environment and stop the app
  observeEvent(input$submit, {
    showNotification("Selection saved!", type = "message")
    assign("Type_A_keywords_curated", Type_A_keywords_curated(), envir = .GlobalEnv)
    stopApp()
  })
}

# Run the Shiny app
shinyApp(ui, server)

### 1.1.7 - Merge for Type A Keyword List---------------------------------------

# Convert data tables (if not already)
Type_A_keywords_curated <- as.data.table(Type_A_keywords_curated)
mgmt_d_keywords <- as.data.table(mgmt_d_keywords)

# Extract only Keyword column from both datasets
Type_A_unigrams <- Type_A_keywords_curated[, .(Keyword)]
Mgmt_Unigrams <- mgmt_d_keywords[, .(Keyword, Specificity)]

# Merge unigram lists and remove duplicates
Type_A_Keywords <- unique(rbind(Type_A_unigrams, Mgmt_Unigrams, fill = TRUE))

# **Ensure Specificity column exists, defaulting to NA where missing**
if (!"Specificity" %in% names(Type_A_Keywords)) {
  Type_A_Keywords[, Specificity := NA_integer_]
}

# **Compute word frequency using Type_A_filtered**
full_text <- tolower(paste(Type_A_filtered$Paragraph, collapse = " "))
Type_A_Keywords[, Frequency := str_count(full_text, paste0("\\b", Keyword, "\\b"))]

# **Remove keywords with Frequency of 0**
Type_A_Keywords <- Type_A_Keywords[Frequency > 0]

### 1.1.8 - Use Keyword List to Query Type B Legislation -----------------------

# Ensure datasets exist
if (!exists("mgmt_d_keywords") || !exists("Type_A_keywords_curated")) {
  stop("Error: Required datasets do not exist in the global environment.")
}

# Convert datasets to data.tables if necessary
mgmt_d_keywords <- as.data.table(mgmt_d_keywords)
Type_A_keywords_curated <- as.data.table(Type_A_keywords_curated)

# Select only the 'Keyword' column from both datasets
query_keywords <- data.table(
  mgmt_d_keywords = mgmt_d_keywords[, .(Keyword)],
  Type_A_keywords_curated = Type_A_keywords_curated[, .(Keyword)]
)

# Save to CSV in the working directory
fwrite(query_keywords, "Type_B_CanLii_query_keywords.csv")

## 1.2 - Processing Type B Legislation -----------------------------------------
### **1.2.1 - Download Type B Legislation HTML files ---------------------------

### 1.2.2 - Parse Type B Legislation -------------------------------------------

# Define the directory containing HTML files
html_dir <- "C:/Users/ennsj/Documents/3. Management Domains/Interactive Tool/R Code/FA Trial/html_files/Type B Legislation"

# Get list of all HTML files in the directory (including subdirectories if needed)
html_files <- list.files(path = html_dir, pattern = "\\.html$", full.names = TRUE, recursive = TRUE)

# Debugging print: Confirm files found
cat("Total HTML files detected:", length(html_files), "\n")

# Initialize datatables with consistent column ordering
headings_b_dt <- data.table(
  Jurisdiction = character(),
  `Legislation Name` = character(),
  `Legislation Type` = character(),
  `Act Name` = character(),
  Heading = character(),
  `Section Number` = character(),
  XPath = character()
)
paragraphs_b_dt <- data.table(
  `Legislation Name` = character(),
  `Act Name` = character(),
  Heading = character(),
  Paragraphs = character(),
  XPath = character()
)

# Function to clean text
clean_text <- function(text) {
  text <- gsub("[Ââ€™]", "", text)
  text <- iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT")
  trimws(text)
}

# Process each HTML file
for (file in html_files) {
  html_file <- read_html(file)
  
  # Extract jurisdiction
  head_attrs <- xml_attrs(html_file %>% html_node("head"))
  meta_description <- html_file %>% html_node("meta[name='description']") %>% html_attr("content")
  meta_breadcrumb <- html_file %>% html_node("meta[name='breadcrumb']") %>% html_attr("content")
  
  jurisdiction <- ifelse(any(grepl("www.gov.bc.ca", head_attrs)) || grepl("British Columbia", meta_breadcrumb), "Provincial",
                         ifelse(!is.na(meta_description) && grepl("Federal laws of Canada", meta_description), "Federal", "Unknown"))
  
  # Extract legislation name
  legislation_name <- html_file %>% html_nodes("h1, div#title h2") %>% html_text(trim = TRUE)
  legislation_name <- ifelse(length(legislation_name) > 0, clean_text(legislation_name[1]), "Unknown Legislation")
  
  # Extract legislation type
  legislation_type <- fifelse(
    grepl("\\bRegulation\\b", legislation_name, ignore.case = TRUE) | 
      grepl("\\bRegulations\\b", legislation_name, ignore.case = TRUE), "Regulations",
    fifelse(grepl("\\bOrder\\b", legislation_name, ignore.case = TRUE) & grepl("\\bAct\\b", legislation_name, ignore.case = TRUE), "Order",
            fifelse(grepl("\\bOrder\\b", legislation_name, ignore.case = TRUE), "Order",
                    fifelse(grepl("\\bAct\\b", legislation_name, ignore.case = TRUE), "Act", ""))))
  
  # Assign Act Name: If Legislation Type is "Act", use Legislation Name, else extract from HTML
  act_name <- fifelse(legislation_type == "Act", legislation_name, 
                      html_file %>% html_node("p.EnablingAct a, div#actname h2") %>% html_text(trim = TRUE))
  
  act_name <- ifelse(length(act_name) > 0, clean_text(act_name), legislation_name)
  
  # Extract headings: Prioritize Marginal Notes first, then h4, h3, h2
  heading_nodes <- html_file %>% html_nodes("p.MarginalNote, h4, h3, h2")
  heading_texts <- clean_text(gsub("^Marginal note:\\s*", "", heading_nodes %>% html_text(trim = TRUE)))
  heading_xpaths <- sapply(heading_nodes, xml_path, USE.NAMES = FALSE)
  
  # Extract section numbers efficiently
  last_section_num <- NA
  section_numbers <- character(length(heading_nodes))
  
  for (i in seq_along(heading_nodes)) {
    section_node <- xml_find_first(heading_nodes[i], xpath = "following-sibling::*[self::p or self::div or self::ul][1]//*[contains(@class, 'sectionLabel') or contains(@class, 'secnumholder')]")
    section_num <- ifelse(!is.null(section_node), xml_text(section_node, trim = TRUE), NA)
    
    section_numbers[i] <- ifelse(!is.na(section_num) && section_num != "", section_num, last_section_num)
    if (!is.na(section_num) && section_num != "") {
      last_section_num <- section_num
    }
  }
  
  # Store headings data
  headings_b_dt <- rbind(headings_b_dt, data.table(
    Jurisdiction = jurisdiction,
    `Legislation Name` = legislation_name,
    `Legislation Type` = legislation_type,
    `Act Name` = act_name,
    Heading = heading_texts,
    `Section Number` = section_numbers,
    XPath = as.character(heading_xpaths)
  ), fill = TRUE)
  
  # Extract paragraphs, including those with <p class="sec ">
  paragraph_nodes <- html_file %>% html_nodes("p.Section, p.Subsection, p.para, ul p, div.paragWrapper p, p.sec.nosubsecnum, p.sec")
  
  paragraph_texts <- clean_text(paragraph_nodes %>% html_text(trim = TRUE))
  paragraph_xpaths <- sapply(paragraph_nodes, xml_path, USE.NAMES = FALSE)
  
  # Assign paragraphs to the closest preceding heading
  assigned_paragraphs <- data.table(
    `Legislation Name` = character(),
    `Act Name` = character(),
    Heading = character(),
    Paragraphs = character(),
    XPath = character()
  )
  
  last_heading <- NA
  last_xpath <- NA
  
  for (i in seq_along(paragraph_nodes)) {
    preceding_heading <- xml_find_all(paragraph_nodes[i], xpath = "preceding::*[self::p[@class='MarginalNote'] or self::h4 or self::h3 or self::h2][1]")
    assigned_heading <- ifelse(length(preceding_heading) > 0, xml_text(preceding_heading[length(preceding_heading)], trim = TRUE), last_heading)
    assigned_heading <- gsub("^Marginal note:\\s*", "", assigned_heading)
    
    if (!is.na(assigned_heading) && assigned_heading != "") {
      last_heading <- assigned_heading
      last_xpath <- ifelse(length(preceding_heading) > 0, xml_path(preceding_heading[length(preceding_heading)]), last_xpath)
    }
    
    cleaned_paragraph <- clean_text(paragraph_texts[i])
    
    assigned_paragraphs <- rbind(assigned_paragraphs, data.table(
      `Legislation Name` = legislation_name,
      `Act Name` = act_name,
      Heading = assigned_heading,
      Paragraphs = cleaned_paragraph,
      XPath = as.character(last_xpath)
    ), fill = TRUE)
  }
  
  paragraphs_b_dt <- rbind(paragraphs_b_dt, assigned_paragraphs, fill = TRUE)
}

# Remove rows where Paragraphs contains "repealed" or "revoked"
paragraphs_b_dt <- paragraphs_b_dt[!grepl("\\b(repealed|revoked)\\b", Paragraphs, ignore.case = TRUE)]

# Remove rows from headings_b_dt where `Section Number` is NA
headings_b_dt <- headings_b_dt[!is.na(`Section Number`)]

# Merge paragraphs with headings
grouped_paragraphs <- paragraphs_b_dt[, .(Paragraphs = paste(Paragraphs, collapse = " ")), by = .(`Legislation Name`, XPath)]
Type_B_parsed <- merge(headings_b_dt, grouped_paragraphs, by = c("Legislation Name", "XPath"), all.x = TRUE)[, -"XPath"]

# Ensure structured ordering
Type_B_parsed <- Type_B_parsed[order(`Legislation Name`, as.numeric(`Section Number`), na.last = TRUE)]

# Remove rows where Paragraphs is NA
Type_B_parsed <- Type_B_parsed[!is.na(Paragraphs)]

### 1.2.3 - Merge parsed directories to create Full_parsed_legislation ---------

# Merge datasets
Full_parsed_legislation <- rbind(Type_A_parsed, Type_B_parsed, fill = TRUE)

# Order data for consistency
Full_parsed_legislation <- Full_parsed_legislation[order(`Legislation Name`, as.numeric(`Section Number`), na.last = TRUE)]

#///////////////////////////////////////////////////////////////////////////////
# 2 - BUILD FILTERING KEYWORD LIST ----
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

## 2.1 - USER - Curate IUCN Keywords--------------------------------------------

# Read mgmt_d_keywords from CSV (actual data source)
file_path_mgmt <- "C:/Users/ennsj/Documents/3. Management Domains/Interactive Tool/R Code/FA Trial/FA Trial/Management Domain Threats and Keywords.csv"

# Check if file exists before reading
if (!file.exists(file_path_mgmt)) stop("Error: Management Domain Threats and Keywords file not found.")

# Read CSV into a data table without modifying it
mgmt_d_keywords <- fread(file_path_mgmt)

# Organize keywords by Management Domain
mgmt_d_keywords_list <- split(mgmt_d_keywords$Keyword, mgmt_d_keywords$`Management Domain`)

# Define UI
ui <- fluidPage(
  titlePanel("Select IUCN Threat Keywords to Filter Legislation"),
  
  # Submit button at the top
  actionButton("submit", "Submit Selection"),
  actionButton("select_all", "Select All / Deselect All"),
  
  # Sidebar with keyword checkboxes grouped by Management Domain
  sidebarLayout(
    sidebarPanel(
      lapply(names(mgmt_d_keywords_list), function(domain) {
        checkboxGroupInput(paste0("selected_", gsub(" ", "_", domain)), domain, 
                           choices = mgmt_d_keywords_list[[domain]])
      })
    ),
    mainPanel()  # Empty main panel to remove extra display
  )
)

# Define Server Logic
server <- function(input, output, session) {
  
  # Track selection state for "Select All" functionality
  observeEvent(input$select_all, {
    all_selected <- unlist(lapply(names(mgmt_d_keywords_list), function(domain) {
      input[[paste0("selected_", gsub(" ", "_", domain))]]
    }))
    
    if (length(all_selected) == nrow(mgmt_d_keywords)) {
      lapply(names(mgmt_d_keywords_list), function(domain) {
        updateCheckboxGroupInput(session, paste0("selected_", gsub(" ", "_", domain)), selected = character(0))  # Deselect all
      })
    } else {
      lapply(names(mgmt_d_keywords_list), function(domain) {
        updateCheckboxGroupInput(session, paste0("selected_", gsub(" ", "_", domain)), selected = mgmt_d_keywords_list[[domain]])  # Select all
      })
    }
  })
  
  # Reactive dataset for storing selected keywords
  mgmt_d_keywords_curated <- reactive({
    selected <- unlist(lapply(names(mgmt_d_keywords_list), function(domain) {
      input[[paste0("selected_", gsub(" ", "_", domain))]]
    }))
    
    req(selected)  # Ensure selection is not empty
    mgmt_d_keywords[Keyword %in% selected]
  })
  
  # Assign selected words to the global environment and stop the app
  observeEvent(input$submit, {
    showNotification("Selection saved!", type = "message")
    assign("mgmt_d_keywords_curated", mgmt_d_keywords_curated(), envir = .GlobalEnv)
    stopApp()
  })
}

# Run the Shiny app
shinyApp(ui, server)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.2 - USER - Assign Specificity to Type A curated keywords-------------------

# Check if the dataset exists before using it
if (!exists("Type_A_keywords_curated")) {
  stop("Error: Type_A_keywords_curated does not exist in the global environment.")
}

# Add a blank Specificity column for numeric values
Type_A_keywords_curated[, Specificity := NA_integer_]

# Define UI
ui <- fluidPage(
  titlePanel("Assign Specificity to Keywords"),
  
  uiOutput("questionnaire_ui"),  # Display one keyword at a time
  
  actionButton("submit", "Submit Selection")
)

# Define Server Logic
server <- function(input, output, session) {
  
  # Dynamically generate questionnaire-style UI
  output$questionnaire_ui <- renderUI({
    tagList(
      lapply(1:nrow(Type_A_keywords_curated), function(i) {
        fluidRow(
          column(6, h4(Type_A_keywords_curated$Keyword[i])),  # Display keyword
          column(6, radioButtons(
            paste0("specificity_", i), label = "Select Specificity:",
            choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4),
            selected = 1, inline = TRUE
          ))
        )
      })
    )
  })
  
  # Capture user selections & ensure each keyword retains its chosen value
  observeEvent(input$submit, {
    for (i in 1:nrow(Type_A_keywords_curated)) {
      Type_A_keywords_curated[i, Specificity := as.integer(input[[paste0("specificity_", i)]])]
    }
    
    showNotification("Specificity assigned & saved!", type = "message")
    assign("Type_A_keywords_final", Type_A_keywords_curated[, .(Keyword, Specificity)], envir = .GlobalEnv)
    stopApp()
  })
}

# Run the Shiny app
shinyApp(ui, server)

## 2.3 - Merge S1, Curated IUCN, and Curated Type A Keywords--------------------
### 2.3.1 - Salmon Keywords (S1): Assign frequency values-----------------------

# Ensure datasets exist
if (!exists("salmon_keywords") || !exists("Full_parsed_legislation")) {
  stop("Error: Required datasets do not exist in the global environment.")
}

# Convert datasets to data.table format
salmon_keywords <- as.data.table(salmon_keywords)
Full_parsed_legislation <- as.data.table(Full_parsed_legislation)

# Ensure Frequency is correctly initialized as numeric
salmon_keywords[, Frequency := 0L]  

# Combine all paragraphs into a single text block for faster searching
full_text <- tolower(paste(Full_parsed_legislation$Paragraph, collapse = " "))

# Count occurrences of each keyword in the full dataset **at once**
salmon_keywords[, Frequency := str_count(full_text, paste0("\\b", Keyword, "\\b"))]

### 2.3.2 - Management Domain Curated Keywords (S2-4): Assign frequency values----

# Ensure datasets exist
if (!exists("mgmt_d_keywords_curated") || !exists("Full_parsed_legislation")) {
  stop("Error: Required datasets do not exist in the global environment.")
}

# Convert datasets to data.table format
mgmt_d_keywords_curated <- as.data.table(mgmt_d_keywords_curated)
Full_parsed_legislation <- as.data.table(Full_parsed_legislation)

# **Create a new datatable excluding L1 and L2**
mgmt_keywords_filtered <- mgmt_d_keywords_curated[, .(Keyword, Specificity)]

# **Add Frequency column initialized to zero**
mgmt_keywords_filtered[, Frequency := 0L]

# **Combine all paragraphs into a single text block for efficient searching**
full_text <- tolower(paste(Full_parsed_legislation$Paragraph, collapse = " "))

# **Compute word frequency for each keyword**
mgmt_keywords_filtered[, Frequency := str_count(full_text, paste0("\\b", Keyword, "\\b"))]

# Filter out keywords with Frequency of 0
mgmt_keywords_filtered <- mgmt_keywords_filtered[Frequency > 0]

### 2.3.3 - Type A Keywords: Re-attach Frequency values-------------------------

# Remove existing Frequency column before merging to avoid duplicates
Type_A_keywords_final[, Frequency := NULL]

# Merge Frequency values from Type_A_Keywords using Keyword as the key
Type_A_keywords_final <- merge(Type_A_keywords_final, 
                               Type_A_Keywords[, .(Keyword, Frequency)], 
                               by = "Keyword", 
                               all.x = TRUE)

### 2.3.4 - Merge salmon_keywords, mgmt_keywords_filtered, and Type_A_keywords_curated----

# Ensure all datasets exist
if (!exists("salmon_keywords") || !exists("mgmt_keywords_filtered") || !exists("Type_A_keywords_curated")) {
  stop("Error: One or more required datasets do not exist in the global environment.")
}

# Convert to data.table format if needed
salmon_keywords <- as.data.table(salmon_keywords)
mgmt_keywords_filtered <- as.data.table(mgmt_keywords_filtered)
Type_A_keywords_final <- as.data.table(Type_A_keywords_final)

# Ensure `Specificity` and `Frequency` columns exist, but DO NOT overwrite existing values
if (!"Specificity" %in% names(Type_A_keywords_final)) {
  Type_A_keywords_final[, Specificity := NA_integer_]
}
if (!"Frequency" %in% names(Type_A_keywords_final)) {
  Type_A_keywords_final[, Frequency := NA_integer_]
}

# Select only relevant columns before merging
salmon_keywords <- salmon_keywords[, .(Keyword, Specificity, Frequency)]
mgmt_keywords_filtered <- mgmt_keywords_filtered[, .(Keyword, Specificity, Frequency)]
Type_A_keywords_final <- Type_A_keywords_final[, .(Keyword, Specificity, Frequency)]

# Merge all keyword lists into one, preserving existing values
Ultimate_Keyword_List <- rbind(salmon_keywords, mgmt_keywords_filtered, Type_A_keywords_final, fill = TRUE)

## 2.4 - Output Ultimate Filter Keyword CSV-------------------------------------

# Define the file path in the working directory
file_path <- file.path(getwd(), "Ultimate_Keyword_List.csv")

# Write the data to CSV
fwrite(Ultimate_Keyword_List, file_path)

#///////////////////////////////////////////////////////////////////////////////
# 3 - APPLY KEYWORDS TO FULL PARSED LEGISLATION---------------------------------
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

## 3.1 - Filter Full_parsed_legislation by all Ultimate_Keyword_List keywords----

# Ensure all necessary datasets exist
if (!exists("Full_parsed_legislation") || !exists("Ultimate_Keyword_List")) {
  stop("Error: Required datasets do not exist in the global environment.")
}

# Convert to data.table format if needed
Full_parsed_legislation <- as.data.table(Full_parsed_legislation)
Ultimate_Keyword_List <- as.data.table(Ultimate_Keyword_List)

# Extract all keywords (regardless of Specificity)
all_keywords <- Ultimate_Keyword_List[, Keyword]

# Filter Full_parsed_legislation to retain only rows where Paragraphs contain at least one keyword
S_all_PE <- Full_parsed_legislation[
  grepl(paste0("\\b(", paste(all_keywords, collapse = "|"), ")\\b"), Paragraphs, ignore.case = TRUE)
]

# Add Specificity column (currently uninitialized)
S_all_PE[, Specificity := NA_integer_]

## 3.2 - Assign S1 Values-------------------------------------------------------

# Extract keywords where Specificity == 1
specificity_1_keywords <- Ultimate_Keyword_List[Specificity == 1, Keyword]

# Assign Specificity = 1 to rows containing these keywords
S_all_PE[
  grepl(paste0("\\b(", paste(specificity_1_keywords, collapse = "|"), ")\\b"), Paragraphs, ignore.case = TRUE),
  Specificity := 1
]

## 3.3 - Assign S2 Values-------------------------------------------------------

# Extract keywords where Specificity == 2
specificity_2_keywords <- Ultimate_Keyword_List[Specificity == 2, Keyword]

# Assign Specificity = 2 **only to rows that do not already have a Specificity value**
S_all_PE[
  is.na(Specificity) & grepl(paste0("\\b(", paste(specificity_2_keywords, collapse = "|"), ")\\b"), Paragraphs, ignore.case = TRUE),
  Specificity := 2
]

## 3.4 - Assign S3 Values-------------------------------------------------------

# Extract keywords where Specificity == 3
specificity_3_keywords <- Ultimate_Keyword_List[Specificity == 3, Keyword]

# Assign Specificity = 3 **only to rows that do not already have a Specificity value**
S_all_PE[
  is.na(Specificity) & grepl(paste0("\\b(", paste(specificity_3_keywords, collapse = "|"), ")\\b"), Paragraphs, ignore.case = TRUE),
  Specificity := 3
]

## 3.5 - Assign S4 Values-------------------------------------------------------

# Extract keywords where Specificity == 4
specificity_4_keywords <- Ultimate_Keyword_List[Specificity == 4, Keyword]

# Assign Specificity = 4 **only to rows that do not already have a Specificity value**
S_all_PE[
  is.na(Specificity) & grepl(paste0("\\b(", paste(specificity_4_keywords, collapse = "|"), ")\\b"), Paragraphs, ignore.case = TRUE),
  Specificity := 4
]

## 3.6 - Filter by Exclusion Keywords-------------------------------------------

# Ensure exclusion_keywords dataset exists
if (!exists("exclusion_keywords")) {
  stop("Error: Exclusion_keywords dataset does not exist in the global environment.")
}

# Convert to data.table format if needed
exclusion_keywords <- as.data.table(exclusion_keywords)

# Extract exclusion keywords
exclusion_list <- exclusion_keywords[, Keyword]

# Remove rows where Paragraphs contain any exclusion keyword
S_all_PE <- S_all_PE[!grepl(paste0("\\b(", paste(exclusion_list, collapse = "|"), ")\\b"), Paragraphs, ignore.case = TRUE)]

#///////////////////////////////////////////////////////////////////////////////
# 4 - ASSIGNING THREAT AND CLAUSE TYPE CATEGORIES-------------------------------
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

## 4.1 - Create "Pacific Salmon Management Domain Compendium" datatable---------

# Ensure S_all_PE exists
if (!exists("S_all_PE")) {
  stop("Error: S_all_PE dataset does not exist in the global environment.")
}

# Convert to data.table format if needed
S_all_PE <- as.data.table(S_all_PE)

# Add new columns with default NA values
S_all_PE[, `Management Domain` := NA_character_]
S_all_PE[, IUCN_L1 := NA_character_]
S_all_PE[, IUCN_L2 := NA_character_]
S_all_PE[, `Clause Type` := NA_character_]

# Rename the data table
Pacific_Salmon_Management_Domain_Compendium <- S_all_PE

## 4.2 - Assign Management Domains----------------------------------------------
### 4.2.1 - Assign MGMT-D to S1 PEs---------------------------------------------

# Ensure required datasets exist
if (!exists("Pacific_Salmon_Management_Domain_Compendium") || !exists("mgmt_d_keywords")) {
  stop("Error: Required datasets do not exist in the global environment.")
}

# Convert to data.table format if needed
Pacific_Salmon_Management_Domain_Compendium <- as.data.table(Pacific_Salmon_Management_Domain_Compendium)
mgmt_d_keywords <- as.data.table(mgmt_d_keywords)

# Convert keywords in mgmt_d_keywords to lowercase for case-insensitive matching
mgmt_d_keywords[, Keyword := tolower(Keyword)]

# Extract rows where Specificity == 1
rows_to_update <- Pacific_Salmon_Management_Domain_Compendium[Specificity == 1]

# Iterate through rows and assign 'Management Domain' based on the first matched keyword (ignoring case)
for (i in seq_len(nrow(rows_to_update))) {
  # Extract words from the Paragraphs column (convert to lowercase for matching)
  paragraph_words <- unlist(strsplit(tolower(rows_to_update[i, Paragraphs]), "\\s+"))
  
  # Find the first word that matches a keyword in mgmt_d_keywords
  match_word <- paragraph_words[paragraph_words %in% mgmt_d_keywords$Keyword][1]
  
  # If a match is found, update the Management Domain column using only the first matching value
  if (!is.na(match_word)) {
    first_domain_match <- mgmt_d_keywords[Keyword == match_word, `Management Domain`][1]  # Ensure only one value
    Pacific_Salmon_Management_Domain_Compendium[Paragraphs == rows_to_update[i, Paragraphs], `Management Domain` := first_domain_match]
  }
}

### 4.2.2 - Assign MGMT-D to S2 PEs---------------------------------------------

# Extract rows where Specificity == 2
rows_to_update <- Pacific_Salmon_Management_Domain_Compendium[Specificity == 2]

# Convert keywords in mgmt_d_keywords to lowercase for case-insensitive matching
mgmt_d_keywords[, Keyword := tolower(Keyword)]

# Iterate through rows and assign 'Management Domain' based on the first matched keyword (ignoring case)
for (i in seq_len(nrow(rows_to_update))) {
  # Extract words from the Paragraphs column (convert to lowercase for matching)
  paragraph_words <- unlist(strsplit(tolower(rows_to_update[i, Paragraphs]), "\\s+"))
  
  # Find the first word that matches a keyword in mgmt_d_keywords
  match_word <- paragraph_words[paragraph_words %in% mgmt_d_keywords$Keyword][1]
  
  # If a match is found, update the Management Domain column using only the first matching value
  if (!is.na(match_word)) {
    first_domain_match <- mgmt_d_keywords[Keyword == match_word, `Management Domain`][1]  # Ensure only one value
    Pacific_Salmon_Management_Domain_Compendium[Paragraphs == rows_to_update[i, Paragraphs], `Management Domain` := first_domain_match]
  }
}

### 4.2.3 - Assign MGMT-D to S3 PEs---------------------------------------------

# Extract rows where Specificity == 3
rows_to_update <- Pacific_Salmon_Management_Domain_Compendium[Specificity == 3]

# Convert keywords in mgmt_d_keywords to lowercase for case-insensitive matching
mgmt_d_keywords[, Keyword := tolower(Keyword)]

# Iterate through rows and assign 'Management Domain' based on the first matched keyword (ignoring case)
for (i in seq_len(nrow(rows_to_update))) {
  # Extract words from the Paragraphs column (convert to lowercase for matching)
  paragraph_words <- unlist(strsplit(tolower(rows_to_update[i, Paragraphs]), "\\s+"))
  
  # Find the first word that matches a keyword in mgmt_d_keywords
  match_word <- paragraph_words[tolower(paragraph_words) %in% mgmt_d_keywords$Keyword][1]
  
  # If a match is found, update the Management Domain column using only the first matching value
  if (!is.na(match_word)) {
    first_domain_match <- mgmt_d_keywords[Keyword == tolower(match_word), `Management Domain`][1]  # Ensure only one value
    Pacific_Salmon_Management_Domain_Compendium[Paragraphs == rows_to_update[i, Paragraphs], `Management Domain` := first_domain_match]
  }
}

### 4.2.4 - Assign MGMT-D to S4 PEs---------------------------------------------

# Extract rows where Specificity == 4
rows_to_update <- Pacific_Salmon_Management_Domain_Compendium[Specificity == 4]

# Convert keywords in mgmt_d_keywords to lowercase for case-insensitive matching
mgmt_d_keywords[, Keyword := tolower(Keyword)]

# Iterate through rows and assign 'Management Domain' based on the first matched keyword (ignoring case)
for (i in seq_len(nrow(rows_to_update))) {
  # Extract words from the Paragraphs column (convert to lowercase for matching)
  paragraph_words <- unlist(strsplit(tolower(rows_to_update[i, Paragraphs]), "\\s+"))
  
  # Find the first word that matches a keyword in mgmt_d_keywords (ignoring case)
  match_word <- paragraph_words[tolower(paragraph_words) %in% mgmt_d_keywords$Keyword][1]
  
  # If a match is found, update the Management Domain column using only the first matching value
  if (!is.na(match_word)) {
    first_domain_match <- mgmt_d_keywords[Keyword == tolower(match_word), `Management Domain`][1]  # Ensure only one value
    Pacific_Salmon_Management_Domain_Compendium[Paragraphs == rows_to_update[i, Paragraphs], `Management Domain` := first_domain_match]
  }
}

## 4.3 - Assign IUCN Threats----------------------------------------------------

# Iterate through rows and assign IUCN_L1 and IUCN_L2 based on first matching keyword
for (i in seq_len(nrow(Pacific_Salmon_Management_Domain_Compendium))) {
  # Extract words from the Paragraphs column
  paragraph_words <- unlist(strsplit(Pacific_Salmon_Management_Domain_Compendium[i, Paragraphs], "\\s+"))
  
  # Find the first word that matches a keyword in mgmt_d_keywords
  match_word <- paragraph_words[paragraph_words %in% mgmt_d_keywords$Keyword][1]
  
  # If a match is found, assign IUCN_L1 and IUCN_L2 using the corresponding Management Domain values
  if (!is.na(match_word)) {
    domain_match <- mgmt_d_keywords[Keyword == match_word, `Management Domain`][1]
    if (!is.na(domain_match)) {
      Pacific_Salmon_Management_Domain_Compendium[i, IUCN_L1 := mgmt_d_keywords[`Management Domain` == domain_match, L1][1]]
      Pacific_Salmon_Management_Domain_Compendium[i, IUCN_L2 := mgmt_d_keywords[`Management Domain` == domain_match, L2][1]]
    }
  }
}

## 4.4 - Assign Clause Types----------------------------------------------------

# Ensure required datasets exist
if (!exists("Pacific_Salmon_Management_Domain_Compendium") || !exists("clause_keywords")) {
  stop("Error: Required datasets do not exist in the global environment.")
}

# Convert to data.table format if needed
Pacific_Salmon_Management_Domain_Compendium <- as.data.table(Pacific_Salmon_Management_Domain_Compendium)
clause_keywords <- as.data.table(clause_keywords)

# Remove quotation marks from Clause_Type values
clause_keywords[, Clause_Type := gsub('["]', '', Clause_Type)]

# Iterate through rows and assign Clause Type based on first matched keyword
for (i in seq_len(nrow(Pacific_Salmon_Management_Domain_Compendium))) {
  # Extract words from the Paragraphs column
  paragraph_words <- unlist(strsplit(Pacific_Salmon_Management_Domain_Compendium[i, Paragraphs], "\\s+"))
  
  # Find the first word that matches a keyword in clause_keywords
  match_word <- paragraph_words[paragraph_words %in% clause_keywords$Keyword][1]
  
  # If a match is found, update the Clause Type column using only the first matching value
  if (!is.na(match_word)) {
    first_clause_match <- clause_keywords[Keyword == match_word, Clause_Type][1]  # Ensure only one value
    Pacific_Salmon_Management_Domain_Compendium[i, `Clause Type` := first_clause_match]
  }
}

## 4.5 - Export Compendium to CSV-----------------------------------------------

# Define file path using the working directory
output_file_path <- file.path(getwd(), "Pacific_Salmon_Management_Domain_Compendium.csv")

# Ensure the dataset exists
if (!exists("Pacific_Salmon_Management_Domain_Compendium")) {
  stop("Error: Dataset Pacific_Salmon_Management_Domain_Compendium does not exist in the global environment.")
}

# Write the data table to a CSV file
fwrite(Pacific_Salmon_Management_Domain_Compendium, output_file_path)