#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# PART 2 - PARSING LEGISALTION--------------------------------------------------
# Summary: Assuming completion of Part 1, and the user has compiled salmon relevant HTML 
# files into folders in the working directory called Type 1 Legislation and
# Type 2 Legislation. 
# Outputs: datatable of salmon relevant legislation parsed by section and paragraph
# with assigned Legislation Name, Legislation Type, Jursidiction, Act Name, and Heading
#///////////////////////////////////////////////////////////////////////////////

## i) Set Working Directory ----------------------------------------------------
library(here)
# Get the root directory of the project
here()

## ii) Load Libraries ----------------------------------------------------------
library(data.table)
library(xml2)
library(rvest)
library(stringi)

# Define the folders dynamically using `here()`----
html_dirs <- c(here("Type A Legislation"), here("Type B Legislation"))

# Load the CSV file---
md_threats_keywords <- fread(here("Management Domain Threats and Keywords.csv"))

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
  
  # Step 1: Capture Section Numbers, Subsections, and Assign Headings
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

# Step 2: Remove Rows with NA Sections
Paragraphs_DT <- Paragraphs_DT[!is.na(`Section`)]

# Step 3: Remove Rows Containing "repeal", "repealed", or "revoked"
filter_words <- c("repeal", "repealed", "revoked", "Marginal note", "Not in force")
Paragraphs_DT <- Paragraphs_DT[!grepl(paste(filter_words, collapse = "|"), `Paragraph`, ignore.case = TRUE)]

# Remove Subsection column----
Paragraphs_DT[, Subsection := NULL]

# Convert keywords into a lookup table for faster matching
keyword_lookup <- md_threats_keywords[, .(Keyword, `Management Domain`, L1, L2, Specificity)]

assign_attributes <- function(paragraph) {
  words <- unlist(strsplit(paragraph, "\\s+"))  
  matches <- md_threats_keywords[Keyword %in% words]  
  
  if (nrow(matches) > 0) {
    # Find the first matching word in the paragraph
    first_match <- words[words %in% matches$Keyword][1]
    selected_row <- matches[Keyword == first_match][1]  # Retrieve attributes from first match
    return(selected_row[, .(`Management Domain`, L1, L2, Specificity)])  
  } else {
    return(data.table(
      `Management Domain` = NA_character_,
      L1 = NA_character_,
      L2 = NA_character_,
      Specificity = NA_character_
    ))
  }
}

# Apply function to extract the first match per row
Paragraphs_DT[, c("Management Domain", "L1", "L2", "Specificity") := assign_attributes(Paragraph), by = Paragraph]

# Combine Paragraphs while keeping all original columns except XPath----
Full_legislation_parsed_DT <- Paragraphs_DT[, .(
  Paragraph = paste(Paragraph, collapse = "\n\n")  # Add line breaks between paragraphs
), by = .(`Management Domain`, Section, Heading, `Legislation Name`, `Legislation Type`, `Act Name`, `Jurisdiction`, L1, L2, Specificity)]  # Grouping in specified order

# Reorder the columns----
setcolorder(Full_legislation_parsed_DT, c(
  "Jurisdiction", "Legislation Type", "Act Name", "Legislation Name",
  "Heading", "Section", "Paragraph",
  "Management Domain", "L1", "L2", "Specificity"
))

# Save Full_legislation_parsed_DT as an R object----
saveRDS(Full_legislation_parsed_DT, "Full_legislation_parsed_DT.rds")


