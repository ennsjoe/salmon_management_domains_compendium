#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# PART 2 - PARSING LEGISALTION--------------------------------------------------
# Assuming completion of Part 1, and the user has compiled salmon relevant HTML 
# files into folders in the working directory called Type 1 Legislation and
# Type 2 Legislation. 
# Outputs: datatable of salmon relevant legislation parsed by section and paragraph
# with assigned Legislation Name, Legislation Type, Jursidiction, Act Name, and Heading
#///////////////////////////////////////////////////////////////////////////////

rm(list = ls())  # Removes all objects in the global environment

# Setup -----------------------------------------------------------------

## i) Set Working Directory ----------------------------------------------------
library(here)
# Get the root directory of the project
here()

## ii) Load Libraries ----------------------------------------------------------
library(data.table)
library(xml2)
library(rvest)
library(stringi)


library(udpipe)
library(shiny)
library(DT)
library(shinyWidgets)
library(miniUI)  # Required for runGadget()
library(stringr)
library(tidytext)  # For NLP tasks
library(dplyr)
library(tidyr)

## iii) Load R Objects

# Define the file path using Here
input_file <- here("mgmt_d_1.RData")

# Load the R object
load(input_file)

# 2.1 - Read in all HTML files from both folders and parse by paragraph/section--------------------------------

library(here)
library(data.table)
library(xml2)
library(rvest)
library(stringi)

# Define the folders dynamically using `here()`
html_dirs <- c(here("Type A Legislation"), here("Type B Legislation"))

# Ensure both directories exist
missing_dirs <- html_dirs[!dir.exists(html_dirs)]
if (length(missing_dirs) > 0) stop(paste("Error: The following directories do not exist:", paste(missing_dirs, collapse = ", ")))

# Read all HTML files from both directories
html_files <- unlist(lapply(html_dirs, function(dir) {
  list.files(path = dir, pattern = "\\.html$", full.names = TRUE, recursive = TRUE)
}))

# Normalize paths to handle special characters
html_files <- normalizePath(html_files, winslash = "/", mustWork = FALSE)

# Debugging print: Confirm files found
cat("Total HTML files detected:", length(html_files), "\n")

# Check if no files are found
if (length(html_files) == 0) stop("No HTML files found in the specified directories.")

# Initialize Paragraphs_DT
Paragraphs_DT <- data.table(
  `Jurisdiction` = character(),
  `Act Name` = character(),
  `Legislation Name` = character(),
  `Legislation Type` = character(),
  `Heading` = character(),
  `Section Number` = character(),
  `Paragraph` = character(),
  `XPath` = character()
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

# Function to extract headings, including Marginal Notes
extract_headings <- function(html_file) {
  heading_nodes <- html_file %>% html_nodes("p.MarginalNote, h4, h3, h2")
  heading_texts <- clean_text(gsub("^Marginal note:\\s*", "", heading_nodes %>% html_text(trim = TRUE)))
  heading_xpaths <- sapply(heading_nodes, xml_path, USE.NAMES = FALSE)
  return(data.table(XPath = heading_xpaths, Heading = heading_texts))
}

# Function to extract jurisdiction
extract_jurisdiction <- function(html_file) {
  head_attrs <- xml_attrs(html_file %>% html_node("head"))
  meta_description <- html_file %>% html_node("meta[name='description']") %>% html_attr("content")
  meta_breadcrumb <- html_file %>% html_node("meta[name='breadcrumb']") %>% html_attr("content")
  
  jurisdiction <- ifelse(any(grepl("www.gov.bc.ca", head_attrs)) || grepl("British Columbia", meta_breadcrumb), "Provincial",
                         ifelse(!is.na(meta_description) && grepl("Federal laws of Canada", meta_description), "Federal", "Unknown"))
  return(jurisdiction)
}

# Function to extract legislation type
extract_legislation_type <- function(legislation_name) {
  legislation_type <- fifelse(
    grepl("\\bRegulation\\b", legislation_name, ignore.case = TRUE) | 
      grepl("\\bRegulations\\b", legislation_name, ignore.case = TRUE), "Regulations",
    fifelse(grepl("\\bOrder\\b", legislation_name, ignore.case = TRUE) & grepl("\\bAct\\b", legislation_name, ignore.case = TRUE), "Order",
            fifelse(grepl("\\bOrder\\b", legislation_name, ignore.case = TRUE), "Order",
                    fifelse(grepl("\\bAct\\b", legislation_name, ignore.case = TRUE), "Act", ""))))
  return(legislation_type)
}

# Function to extract act name
extract_act_name <- function(html_file, legislation_name, legislation_type) {
  act_name <- fifelse(legislation_type == "Act", legislation_name, 
                      html_file %>% html_node("p.EnablingAct a, div#actname h2") %>% html_text(trim = TRUE))
  
  act_name <- ifelse(length(act_name) > 0, clean_text(act_name), legislation_name)
  return(act_name)
}

# Process each HTML file
for (file in html_files) {
  html_file <- read_html(file)
  
  # Extract metadata
  legislation_name <- extract_legislation_name(html_file)
  jurisdiction <- extract_jurisdiction(html_file)
  legislation_type <- extract_legislation_type(legislation_name)
  act_name <- extract_act_name(html_file, legislation_name, legislation_type)
  
  # Extract all paragraph nodes
  all_paragraphs <- html_file %>% html_nodes("p")
  
  # Extract headings
  headings_DT <- extract_headings(html_file)
  
  last_section_number <- NA
  last_section_xpath <- NA
  
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
    
    # Assign the nearest preceding header
    nearest_heading <- headings_DT[XPath < current_xpath, .SD[.N], .SDcols = "Heading"]$Heading
    if (length(nearest_heading) == 0) nearest_heading <- NA  # Handle case with no preceding heading
    
    if (nzchar(paragraph_text)) {
      Paragraphs_DT <- rbind(Paragraphs_DT, data.table(
        `Legislation Name` = legislation_name,
        `Section Number` = last_section_number,  
        `XPath` = current_xpath,
        `Paragraph` = paragraph_text,
        `Heading` = nearest_heading,
        `Jurisdiction` = jurisdiction,
        `Legislation Type` = legislation_type,
        `Act Name` = act_name
      ), fill = TRUE)
    }
  }
}

# Step 2: Remove Rows with NA Section Numbers
Paragraphs_DT <- Paragraphs_DT[!is.na(`Section Number`)]

# Step 3: Remove Rows Containing "repeal", "repealed", or "revoked"
filter_words <- c("repeal", "repealed", "revoked")
Paragraphs_DT <- Paragraphs_DT[!grepl(paste(filter_words, collapse = "|"), `Paragraph`, ignore.case = TRUE)]


# **Next - Alter Act Name for consistency
# ** Fix Heading allocation - totally bonkers - look at headings_DT
# ** Paragraphs missing when subsections are involved








###################################################################
##################################################################

## iii) Read in files and models------------------------------------------------

### Read in Management Domain Keywords and IUCN Threats-------------------------

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




############################################################################

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
