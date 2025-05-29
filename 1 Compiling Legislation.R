#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# PART 1 - COMPILING LEGISLATION------------------------------------------------
# This script uses HTML files of legislation specific to Pacific salmon (Type A) to map 
# keywords to build further queries in CanLii (https://www.canlii.org/)
# Outputs: HTML Files saved to 2 folders (Type A Legislation, Type B Legislation)
# Note: Downloading HTML files is a manual process, but could be automated in the
# future using APIs
#///////////////////////////////////////////////////////////////////////////////

## **BEFORE STARTING - Download Type A Legislation HTML files
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
library(stringi)  # For text cleaning


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

## v) Read in Management Domain Keywords and IUCN Threats-----------------------

# Define file path using the `here()` function
file_path_mgmt <- here("Management Domain Threats and Keywords.csv")

# Check if file exists before reading
if (!file.exists(file_path_mgmt)) stop("Error: Management Domain Threats and Keywords file not found.")

# Read CSV into a data table while preserving multi-word phrases in double quotes
mgmt_d_keywords <- tryCatch({
  fread(file_path_mgmt, quote = "\"")
}, error = function(e) {
  stop("Error reading CSV file: ", e$message)
})

# Verify expected columns exist
required_columns <- c("Management Domain", "L1", "L2", "Keyword", "Specificity")
if (!all(required_columns %in% colnames(mgmt_d_keywords))) {
  stop("Error: Expected columns missing from the dataset.")
}

# 1.1 - Parse HTML Files into paragraphs ---------------------------------------
## 1.1.1 - Read in HTML files---------------------------------------------------
 
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

## 1.1.2 - Parse paragraph nodes and assign section numbers--------------------

library(rvest)
library(xml2)
library(data.table)
library(stringi)  # For text cleaning

# Initialize Paragraphs_DT
Paragraphs_DT <- data.table(
  `Legislation Name` = character(),
  `Section Number` = character(),
  `XPath` = character(),
  `Paragraph` = character()  # Updated column name
)

# Function to clean text and remove special characters
clean_text <- function(text) {
  text <- stri_trans_general(text, "Latin-ASCII")  # Convert special characters to ASCII equivalents
  text <- gsub("[^[:print:]]", "", text)  # Remove any remaining non-printable characters
  return(trimws(text))
}

# Function to extract section numbers inside `<p>` tags
extract_inline_section <- function(node) {
  section_label <- node %>% html_nodes("span.secnum span.secnumholder b, a.sectionLabel span.sectionLabel") %>% html_text(trim = TRUE)
  if (length(section_label) > 0) return(clean_text(section_label)) else return(NA)
}

# Function to extract the legislation name
extract_legislation_name <- function(html_file) {
  legislation_name <- html_file %>% html_nodes("h1.HeadTitle, div#title h2") %>% html_text(trim = TRUE)
  legislation_name <- ifelse(length(legislation_name) > 0, clean_text(legislation_name[1]), "Unknown Legislation")
  return(legislation_name)
}

# Function to compute XPath depth
get_xpath_depth <- function(xpath) {
  return(length(strsplit(xpath, "/")[[1]]))
}

# Process each HTML file
for (file in html_files) {
  html_file <- read_html(file)
  
  # Extract legislation name
  legislation_name <- extract_legislation_name(html_file)
  
  # Extract all paragraph nodes
  all_paragraphs <- html_file %>% html_nodes("p")
  
  last_section_number <- NA
  last_section_xpath <- NA
  last_xpath_depth <- NA
  
  # Step 1: Capture Section Numbers and Ensure Correct Assignment
  for (node in all_paragraphs) {
    current_xpath <- xml_path(node)
    paragraph_class <- xml_attr(node, "class")  # Check paragraph class
    
    inline_section_number <- extract_inline_section(node)
    
    # If the paragraph is a "division", treat it separately and reset section tracking
    if (!is.na(paragraph_class) && grepl("division", paragraph_class, ignore.case = TRUE)) {
      last_section_number <- NA   # Reset section number
      last_section_xpath <- NA    # Reset XPath tracking
    } else if (!is.na(inline_section_number)) {
      last_section_number <- inline_section_number
      last_section_xpath <- current_xpath
    } 
    
    # Assign previous section number only if the paragraph is not a division
    paragraph_text <- clean_text(xml_text(node, trim = TRUE))
    
    if (nzchar(paragraph_text)) {
      Paragraphs_DT <- rbind(Paragraphs_DT, data.table(
        `Legislation Name` = legislation_name,
        `Section Number` = last_section_number,  # Avoid propagating NA section numbers
        `XPath` = current_xpath,
        `Paragraph` = paragraph_text  # Updated column name
      ), fill = TRUE)
    }
    
    last_xpath_depth <- current_xpath_depth
  }
}

# Step 2: Remove Rows with NA Section Numbers
Paragraphs_DT <- Paragraphs_DT[!is.na(`Section Number`)]

# Step 3: Remove Rows Containing "repeal", "repealed", or "revoked"
filter_words <- c("repeal", "repealed", "revoked")
Paragraphs_DT <- Paragraphs_DT[!grepl(paste(filter_words, collapse = "|"), `Paragraph`, ignore.case = TRUE)]

## 1.1.3 - Group paragraphs by section number-----------------------------------

# Group Paragraphs by Section Number and Legislation Name
Grouped_DT <- Paragraphs_DT[, .(
  `Paragraph` = paste(`Paragraph`, collapse = " ")  # Updated column name
), by = .(`Legislation Name`, `Section Number`)]

# 1.2 - Filter paragraphs by Salmon Keywords (salmon, chinook, coho, sockeye, chum)----

# Ensure keywords are properly formatted as stand-alone words, case insensitive
salmon_keywords[, Keyword := paste0("\\b", tolower(Keyword), "\\b")]

# Create a pattern to match any of the keywords
keyword_pattern <- paste(salmon_keywords$Keyword, collapse = "|")

# Convert Paragraph column to lowercase for case-insensitive matching
Type_A_PE <- Grouped_DT[grepl(keyword_pattern, tolower(Paragraph), ignore.case = FALSE)]

# 1.3 - Extract salmon-adjacent unigrams----------------------------------------

# Define the path to your UDPipe model using the `here()` function
udpipe_model_path <- here("english-ewt-ud-2.5-191206.udpipe")
ud_model <- udpipe_load_model(udpipe_model_path)

# Perform annotation on the Paragraph column of Type_A_PE
annotation <- udpipe_annotate(ud_model, x = tolower(Type_A_PE$Paragraph))  # Convert text to lowercase before processing
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

############################################################################
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

## 1.1.3 - Filter for Type A Procedural Elements ------------------------------
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
