#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# PART 2 - PARSING LEGISALTION--------------------------------------------------
# Summary: Assuming completion of Part 1, and the user has compiled salmon relevant HTML 
# files into folders in the working directory called Type 1 Legislation and
# Type 2 Legislation. 
# Outputs: datatable of salmon relevant legislation parsed by section and paragraph
# with assigned Legislation Name, Legislation Type, Jursidiction, Act Name, and Heading
#///////////////////////////////////////////////////////////////////////////////

# Set Working Directory ----
library(here)
# Get the root directory of the project
here()

# Load Libraries ----
library(data.table)
library(xml2)
library(rvest)
library(stringi)
library(stringr)
library(writexl)

# Load R Object----------------------------------------------------------------
# Define file path using here()
rds_path <- here("Full_legislation_compendium.rds")

# Check if the file exists before loading
if (file.exists(rds_path)) {
  loaded_data <- readRDS(rds_path)
  message("File loaded successfully!")
} else {
  message("File does not exist: ", rds_path)
}

# Optional: Inspect loaded data structure
if (exists("loaded_data")) {
  str(loaded_data)
}

#-------------------------------------------------------------------------------
# Define the folders dynamically using `here()`----
html_dirs <- c(here("Type A Legislation"), here("Type B Legislation"))

# Create the salmon_keywords data table-----------------------------------------
salmon_keywords <- data.table(
  Keyword = c("salmon", "chinook", "sockeye", "coho", "chum", "salmonid"),
  Scope = '1 - Salmon'
)

# Load Management Domain Keyword CSV file---------------------------------------
md_threats_keywords <- fread(here("Management Domain Threats and Keywords.csv"))
file_path <- here("Clause Type Keywords.csv")

# Read CSV file while ensuring columns are characters
clause_type_keywords <- fread(file_path, colClasses = c("Keyword" = "character", "Clause_Type" = "character"))

# Ensure both directories exist----
missing_dirs <- html_dirs[!dir.exists(html_dirs)]
if (length(missing_dirs) > 0) stop(paste("Error: The following directories do not exist:", paste(missing_dirs, collapse = ", ")))

# Read all HTML files from both directories----
html_files <- unlist(lapply(html_dirs, function(dir) {
  list.files(path = dir, pattern = "\\.html$", full.names = TRUE, recursive = TRUE)
}))

# Normalize paths to handle special characters
html_files <- normalizePath(html_files, winslash = "/", mustWork = FALSE)

# Debugging print: Confirm files found
cat("Total HTML files detected:", length(html_files), "\n")

# Check if no files are found
if (length(html_files) == 0) stop("No HTML files found in the specified directories.")

# Initialize Paragraphs_DT----
Paragraphs_DT <- data.table(
  `Jurisdiction` = character(),
  `Act Name` = character(),
  `Legislation Name` = character(),
  `Legislation Type` = character(),
  `Section` = character(),
  `Subsection` = character(),
  `Heading` = character(),
  `Paragraph` = character(),
  `XPath` = character()
)

################################################################################
# Function to clean text and remove special characters
clean_text <- function(text) {
  text <- stri_trans_general(text, "Latin-ASCII")  # Convert special characters to ASCII equivalents
  text <- gsub("[^[:print:]]", "", text)  # Remove any remaining non-printable characters
  return(trimws(text))
}

# Function to format Act Name----
format_act_name <- function(act_name) {
  act_name <- gsub("\\s*\\(.*?\\)|\\s*\\[.*?\\]", "", act_name)  # Remove anything in brackets
  act_name <- tolower(act_name)  # Convert to lowercase
  act_name <- gsub("(^|\\s)([a-z])", "\\1\\U\\2", act_name, perl = TRUE)  # Capitalize first letter of each word
  return(trimws(act_name))
}

# Function to extract section numbers inside `<p>` tags----
extract_inline_section <- function(node) {
  section_label <- node %>% html_nodes("span.secnum span.secnumholder b, a.sectionLabel span.sectionLabel") %>% html_text(trim = TRUE)
  if (length(section_label) > 0) return(clean_text(section_label)) else return(NA)
}

# Function to extract subsection numbers----
extract_subsection <- function(node) {
  subsection_label <- node %>% html_nodes("span.lawlabel, span.num span.holder") %>% html_text(trim = TRUE)
  if (length(subsection_label) > 0) return(clean_text(subsection_label)) else return(NA)
}

# Function to extract headings----
extract_headings <- function(html_file) {
  heading_nodes <- html_file %>% html_nodes("p.MarginalNote, h4, h3, h2")
  heading_texts <- clean_text(gsub("^Marginal note:\\s*", "", heading_nodes %>% html_text(trim = TRUE)))
  heading_xpaths <- sapply(heading_nodes, xml_path, USE.NAMES = FALSE)
  return(data.table(XPath = heading_xpaths, Heading = heading_texts))
}

# Function to extract the legislation name----
extract_legislation_name <- function(html_file) {
  legislation_name <- html_file %>% html_nodes("h1.HeadTitle, div#title h2") %>% html_text(trim = TRUE)
  legislation_name <- ifelse(length(legislation_name) > 0, clean_text(legislation_name[1]), "Unknown Legislation")
  return(legislation_name)
}

# Function to extract jurisdiction----
extract_jurisdiction <- function(html_file) {
  head_attrs <- xml_attrs(html_file %>% html_node("head"))
  meta_description <- html_file %>% html_node("meta[name='description']") %>% html_attr("content")
  meta_breadcrumb <- html_file %>% html_node("meta[name='breadcrumb']") %>% html_attr("content")
  
  jurisdiction <- ifelse(any(grepl("www.gov.bc.ca", head_attrs)) || grepl("British Columbia", meta_breadcrumb), "Provincial",
                         ifelse(!is.na(meta_description) && grepl("Federal laws of Canada", meta_description), "Federal", "Unknown"))
  return(jurisdiction)
}

# Function to extract legislation type----
extract_legislation_type <- function(legislation_name) {
  legislation_type <- fifelse(
    grepl("\\bRegulation\\b", legislation_name, ignore.case = TRUE) | 
      grepl("\\bRegulations\\b", legislation_name, ignore.case = TRUE), "Regulations",
    fifelse(grepl("\\bOrder\\b", legislation_name, ignore.case = TRUE) & grepl("\\bAct\\b", legislation_name, ignore.case = TRUE), "Order",
            fifelse(grepl("\\bOrder\\b", legislation_name, ignore.case = TRUE), "Order",
                    fifelse(grepl("\\bAct\\b", legislation_name, ignore.case = TRUE), "Act", ""))))
  return(legislation_type)
}

# Function to extract act name----
extract_act_name <- function(html_file, legislation_name, legislation_type) {
  act_name <- fifelse(legislation_type == "Act", legislation_name, 
                      html_file %>% html_node("p.EnablingAct a, div#actname h2") %>% html_text(trim = TRUE))
  
  act_name <- ifelse(length(act_name) > 0, clean_text(act_name), legislation_name)
  act_name <- format_act_name(act_name)  # Apply formatting
  return(act_name)
}

################################################################################
# Process each HTML file----
for (file in html_files) {
  html_file <- read_html(file)
  
  # Extract metadata
  legislation_name <- extract_legislation_name(html_file)
  jurisdiction <- extract_jurisdiction(html_file)
  legislation_type <- extract_legislation_type(legislation_name)
  act_name <- extract_act_name(html_file, legislation_name, legislation_type)
  
  # Extract all paragraph nodes, including nested elements inside lists, definitions, anchors, and spans
  all_paragraphs <- html_file %>% html_nodes("p, div p, dl p, dd p, li p, ul p, dfn p, a p, span p")  
  
  # Extract headings----
  headings_DT <- extract_headings(html_file)
  
  last_section <- NA
  last_section_xpath <- NA
  last_heading <- NA
  last_xpath <- NA
  
  # Step 1: Capture Section Numbers, Subsections, and Assign Headings----
  for (node in all_paragraphs) {
    current_xpath <- xml_path(node)
    paragraph_class <- xml_attr(node, "class")  # Check paragraph class
    
    inline_section_number <- extract_inline_section(node)
    subsection_number <- extract_subsection(node)
    
    # Determine appropriate heading using preceding Marginal Notes, h4, h3, or h2
    preceding_heading <- xml_find_all(node, xpath = "preceding::*[self::p[@class='MarginalNote'] or self::h4 or self::h3 or self::h2][1]")
    assigned_heading <- ifelse(length(preceding_heading) > 0, xml_text(preceding_heading[length(preceding_heading)], trim = TRUE), last_heading)
    assigned_heading <- gsub("^Marginal note:\\s*", "", assigned_heading)
    
    # Track previous heading details for next iteration
    if (!is.na(assigned_heading) && assigned_heading != "") {
      last_heading <- assigned_heading
      last_xpath <- ifelse(length(preceding_heading) > 0, xml_path(preceding_heading[length(preceding_heading)]), last_xpath)
    }
    
    # If the paragraph is a "division", treat it separately and reset section tracking
    if (!is.na(paragraph_class) && grepl("division", paragraph_class, ignore.case = TRUE)) {
      last_section <- NA   # Reset section
      last_section_xpath <- NA  # Reset XPath tracking
    } else if (!is.na(inline_section_number)) {
      last_section <- inline_section_number
      last_section_xpath <- current_xpath
    } 
    
    paragraph_text <- clean_text(xml_text(node, trim = TRUE))
    
    if (nzchar(paragraph_text)) {
      Paragraphs_DT <- rbind(Paragraphs_DT, data.table(
        `Legislation Name` = legislation_name,
        `Section` = last_section,  
        `Subsection` = subsection_number,
        `Heading` = assigned_heading,
        `Paragraph` = paragraph_text,
        `XPath` = current_xpath,
        `Jurisdiction` = jurisdiction,
        `Legislation Type` = legislation_type,
        `Act Name` = act_name
      ), fill = TRUE)
    }
  }
}

################################################################################
# Remove Rows with NA Sections----
Paragraphs_DT <- Paragraphs_DT[!is.na(`Section`)]

# Define keywords to filter
filter_words <- c("repeal", "repealed", "revoked", "Marginal note", "Not in force")

# Ensure proper word-boundary matching and case insensitivity
Paragraphs_DT <- Paragraphs_DT[!grepl(paste0("\\b(", paste(filter_words, collapse = "|"), ")\\b"), Paragraph, ignore.case = TRUE)]

# Remove Subsection column----
Paragraphs_DT[, Subsection := NULL]

################################################################################
# Convert keywords into a lookup table for faster matching
keyword_lookup <- md_threats_keywords[, .(Keyword, `Management Domain`, L1, L2, Scope)]

# Function to assign attributes based on first matched keyword----
assign_attributes <- function(paragraph) {
  words <- unlist(strsplit(paragraph, "\\s+"))
  matches <- md_threats_keywords[Keyword %in% words]  
  
  if (nrow(matches) > 0) {
    # Find the first matching word in the paragraph
    first_match <- words[words %in% matches$Keyword][1]
    selected_row <- matches[Keyword == first_match][1]  # Retrieve attributes from first match
    return(selected_row[, .(`Management Domain`, L1, L2, Scope)])
  } else {
    return(data.table(
      `Management Domain` = NA_character_,
      L1 = NA_character_,
      L2 = NA_character_,
      Scope = NA_character_
    ))
  }
}

# Apply function to extract the first match per rowAdd commentMore actions
Paragraphs_DT[, c("Management Domain", "L1", "L2", "Scope") := assign_attributes(Paragraph), by = Paragraph]

################################################################################
# Combine Paragraphs while keeping all original columns except XPath----
Full_legislation_parsed_DT <- Paragraphs_DT[, .(
  Paragraph = paste(Paragraph, collapse = "\n\n")  # Add line breaks between paragraphs
), by = .(`Management Domain`, Section, Heading, `Legislation Name`, `Legislation Type`, `Act Name`, `Jurisdiction`, L1, L2, Scope)]  # Grouping in specified order

##########################################
# Function to update Scope only for matching rows
update_scope_salmon <- function(paragraph, existing_scope, keywords_dt) {
  # Standardize text: Remove punctuation and convert to lowercase
  clean_paragraph <- tolower(gsub("[[:punct:]]", " ", paragraph))
  
  # Split paragraph into words
  words <- unlist(strsplit(clean_paragraph, "\\s+"))
  
  # Find matches in keyword list
  matches <- keywords_dt[Keyword %in% words]
  
  if (nrow(matches) > 0) {
    # Get first matched word
    first_match <- words[words %in% matches$Keyword][1]
    
    # Retrieve Scope corresponding to the first matched keyword
    scope_value <- matches[Keyword == first_match, Scope][1]
    
    return(scope_value)  # Update only for matched rows
  } else {
    return(existing_scope)  # Keep original value for non-matching rows
  }
}

# Apply function to update Scope only where a match is found
Full_legislation_parsed_DT[, Scope := mapply(update_scope_salmon, Paragraph, Scope, MoreArgs = list(keywords_dt = salmon_keywords))]

################################################################################
# Function to assign Clause_Type based on first matched word with improved matching----
assign_clause_type <- function(paragraph, keywords_dt) {
  # Standardize text: Remove punctuation (except word boundaries) and convert to lowercase
  clean_paragraph <- str_to_lower(gsub("[[:punct:]]", " ", paragraph))
  
  # Split paragraph into words
  words <- unlist(strsplit(clean_paragraph, "\\s+"))  
  
  # Find matches in keyword list (case-insensitive)
  matches <- keywords_dt[Keyword %in% words]
  
  if (nrow(matches) > 0) {
    # Get first matched word
    first_match <- words[words %in% matches$Keyword][1]
    
    # Retrieve Clause_Type corresponding to the first matched keyword
    clause_type <- matches[Keyword == first_match, Clause_Type][1]
    
    return(clause_type)
  } else {
    return(NA_character_)
  }
}

# Apply function to assign Clause_Type----
Full_legislation_parsed_DT[, Clause_Type := sapply(Paragraph, assign_clause_type, keywords_dt = clause_type_keywords)]

################################################################################
# Trim Paragraph column to a maximum of 5,000 characters----
Full_legislation_parsed_DT[, Paragraph := substr(Paragraph, 1, 5000)]

# Reorder the columns----
setcolorder(Full_legislation_parsed_DT, c(
  "Jurisdiction", "Legislation Type", "Act Name", "Legislation Name",
  "Heading", "Section", "Paragraph",
  "Management Domain", "L1", "L2", "Scope", "Clause_Type"
))


# Save datatables as an R object------------------------------------------------
saved_data<- list(
  Full_legislation_parsed_DT = Full_legislation_parsed_DT,
  salmon_keywords = salmon_keywords,
  md_threats_keywords = md_threats_keywords,
  clause_type_keywords = clause_type_keywords,
  Paragraphs_DT = Paragraphs_DT
)
saveRDS(saved_data, "Full_legislation_compendium.rds")

# Define file path using here()
file_path <- here("Full_legislation_compendium.xlsx")

# Export data table to XLSX
write_xlsx(Full_legislation_parsed_DT, path = file_path)

