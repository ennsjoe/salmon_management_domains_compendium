################################################################################
# Title: Database Paragraph Table (Enhanced for Schedules and Appendices)
# Authors: Joe Enns, Cory Lagasse, Max Elinson
# Date Created: 2025-08-07
# Last Updated: 2025-01-XX
# Purpose / Description: 
#   This script processes HTML files containing legislative paragraphs,
#   extracts relevant information, and saves it into a structured SQLite database.
#   ENHANCED: Now handles complex section numbering (1.01, 1.01.01, 3.12.2, etc.)
#   and properly handles Schedule and Appendix sections separately from main sections.
#   ENHANCED: Filters out editorial sections and amendment references
################################################################################

## Load Libraries ----
library(here)
library(data.table)
library(xml2)
library(rvest)
library(stringi)
library(stringr)
library(RSQLite)
library(beepr)

## Define the folders dynamically using `here()`
html_dirs <- here("legislation_html")

## Read all HTML files from the directory
html_files <- unlist(lapply(html_dirs, function(dir) {
  list.files(path = dir, pattern = "\\.html$", full.names = TRUE, recursive = TRUE)
}))

## Normalize paths to handle special characters
html_files <- normalizePath(html_files, winslash = "/", mustWork = FALSE)

## Debugging print: Confirm files found
cat("Total HTML files detected:", length(html_files), "\n")

## Stop if no files are found
if (length(html_files) == 0) stop("No HTML files found in the specified directories.")

## Initialize paragraph_table ----
paragraph_table <- data.table(
  legislation_id = integer(),
  Section = character(),
  Heading = character(),
  Paragraph = character(),
  XPath = character()
)

## Track problematic files ----
bad_files <- character()

## Utility Functions ----
clean_text <- function(text) {
  text <- stri_enc_toutf8(text)
  text <- stri_trans_general(text, "Latin-ASCII")
  text <- gsub("[^[:print:]]", "", text)
  return(trimws(text))
}

# Function to check if a paragraph is footer/metadata content
is_footer_or_metadata <- function(text) {
  if (is.na(text) || text == "") return(TRUE)
  
  # Patterns that indicate footer/metadata content
  footer_patterns <- c(
    "King's Printer",
    "Queen's Printer",
    "Copyright ©",
    "Provisions relevant to the enactment",
    "^\\[Provisions relevant",
    "Victoria, British Columbia, Canada$"
  )
  
  # Check if text matches any footer pattern
  for (pattern in footer_patterns) {
    if (grepl(pattern, text, ignore.case = TRUE, perl = TRUE)) {
      return(TRUE)
    }
  }
  
  # Check for very short paragraphs that are just numbers
  if (nchar(text) < 5 && grepl("^[0-9]+$", text)) {
    return(TRUE)
  }
  
  return(FALSE)
}

# Function to check if paragraph is an amendment reference or citation
is_amendment_reference <- function(text) {
  if (is.na(text) || text == "") return(FALSE)
  
  # Pattern for amendment references like "[Amendments]" or "150 [Amendments]"
  # Often includes year references like "2001, c. 26"
  amendment_patterns <- c(
    "\\[Amendments\\]",
    "^\\d+\\s+\\[Amendments\\]",
    "^\\d{4},\\s*c\\.\\s*\\d+",  # Year and chapter references
    "^S\\.C\\.\\s*\\d{4}",        # Statute citation format
    "^S\\.B\\.C\\.\\s*\\d{4}",    # BC statute citation format
    "^—\\s*\\d{4},\\s*c\\.\\s*\\d+",  # Em dash citation format
    "^-\\s*\\d{4},\\s*c\\.\\s*\\d+"   # Hyphen citation format
  )
  
  for (pattern in amendment_patterns) {
    if (grepl(pattern, text, perl = TRUE)) {
      return(TRUE)
    }
  }
  
  # Check if text is very short and contains only statute references
  if (nchar(text) < 50 && grepl("c\\.\\s*\\d+", text) && grepl("\\[Amendments\\]", text)) {
    return(TRUE)
  }
  
  # Check for short citation-only paragraphs like "— 2023, c. 26, s. 386"
  # Format: em dash or hyphen, year, chapter, section
  if (nchar(text) < 100 && grepl("^[—-]\\s*\\d{4},\\s*c\\.\\s*\\d+", text)) {
    return(TRUE)
  }
  
  return(FALSE)
}

# Function to check if heading is an editorial/non-legislative section
is_editorial_heading <- function(text) {
  if (is.na(text) || text == "") return(FALSE)
  
  # Patterns that indicate editorial/non-legislative sections
  editorial_patterns <- c(
    "^RELATED PROVISIONS$",
    "^Related Provisions$",
    "^AMENDMENTS NOT IN FORCE$",
    "^Amendments Not In Force$",
    "^Amendments not in force$",
    "^\\[Amendments\\]$",
    "^Amendments$",
    "^TRANSITIONAL PROVISIONS$",
    "^Transitional Provisions$",
    "^COMING INTO FORCE$",
    "^Coming Into Force$",
    "^Coming into force$",
    "^CONSEQUENTIAL AMENDMENTS$",
    "^Consequential Amendments$",
    "^COORDINATING AMENDMENTS$",
    "^Coordinating Amendments$",
    "^TABLE OF CONTENTS$",
    "^Table of Contents$",
    "^CONDITIONAL AMENDMENTS$",
    "^Conditional Amendments$"
  )
  
  for (pattern in editorial_patterns) {
    if (grepl(pattern, text, perl = TRUE)) {
      return(TRUE)
    }
  }
  
  return(FALSE)
}

# Function to detect schedule headers
is_schedule_header <- function(text) {
  if (is.na(text) || text == "") return(FALSE)
  
  # Patterns that indicate a schedule header
  schedule_patterns <- c(
    "^Schedule [A-Z0-9]",
    "^SCHEDULE [A-Z0-9]"
  )
  
  for (pattern in schedule_patterns) {
    if (grepl(pattern, text, perl = TRUE)) {
      return(TRUE)
    }
  }
  
  return(FALSE)
}

# Function to detect appendix headers
is_appendix_header <- function(text) {
  if (is.na(text) || text == "") return(FALSE)
  
  # Patterns that indicate an appendix header
  appendix_patterns <- c(
    "^Appendix [A-Z0-9]",
    "^APPENDIX [A-Z0-9]"
  )
  
  for (pattern in appendix_patterns) {
    if (grepl(pattern, text, perl = TRUE)) {
      return(TRUE)
    }
  }
  
  return(FALSE)
}

# Extract schedule name from heading
extract_schedule_name <- function(text) {
  if (grepl("^Schedule ([A-Z0-9]+)", text, ignore.case = TRUE, perl = TRUE)) {
    match <- regmatches(text, regexpr("^Schedule ([A-Z0-9]+)", text, ignore.case = TRUE, perl = TRUE))
    return(match[1])
  }
  return(NA)
}

# Extract appendix name from heading
extract_appendix_name <- function(text) {
  if (grepl("^Appendix ([A-Z0-9]+)", text, ignore.case = TRUE, perl = TRUE)) {
    match <- regmatches(text, regexpr("^Appendix ([A-Z0-9]+)", text, ignore.case = TRUE, perl = TRUE))
    return(match[1])
  }
  return(NA)
}

# ENHANCED: Extract section numbers from various formats
extract_inline_section <- function(node) {
  paragraph_text <- xml_text(node, trim = TRUE)
  
  # STRATEGY 1: Extract from the very beginning of paragraph text
  # This handles sections like 1.01, 1.01.01, 3.12.2 that appear at the start
  # Pattern matches: 1, 1.1, 1.01, 1.01.1, 1.01.01, 3.12.2, etc.
  section_match <- str_extract(paragraph_text, "^(\\d+(?:\\.\\d+){0,3})(?=\\s|\\[|\\()")
  
  if (!is.na(section_match) && nchar(section_match) > 0) {
    # Validate it's a reasonable section number (not just a random number)
    if (grepl("^\\d+(?:\\.\\d+)*$", section_match)) {
      return(section_match)
    }
  }
  
  # STRATEGY 2: Try structured HTML elements
  section_label <- node %>% 
    html_nodes("span.secnum span.secnumholder b, 
                a.sectionLabel span.sectionLabel,
                span.sectionLabel,
                span.secnum b,
                span.secnumholder,
                .secnum") %>% 
    html_text(trim = TRUE)
  
  if (length(section_label) > 0 && nchar(section_label[1]) > 0) {
    cleaned <- clean_text(section_label[1])
    # Remove trailing periods
    cleaned <- gsub("\\.$", "", trimws(cleaned))
    # Validate it's actually a section number
    if (grepl("^\\d+(?:\\.\\d+)*$", cleaned)) {
      return(cleaned)
    }
  }
  
  # STRATEGY 3: Section number after specific keywords
  # Handles "Section 1.01" or similar patterns
  section_match <- str_extract(paragraph_text, "(?<=^Section\\s)\\d+(?:\\.\\d+){0,3}")
  
  if (!is.na(section_match) && nchar(section_match) > 0) {
    return(section_match)
  }
  
  # STRATEGY 4: Look for bracketed section references like [(1.01)]
  section_match <- str_extract(paragraph_text, "(?<=^\\[\\()\\d+(?:\\.\\d+){0,3}(?=\\)\\])")
  
  if (!is.na(section_match) && nchar(section_match) > 0) {
    return(section_match)
  }
  
  return(NA)
}

extract_headings <- function(html_file) {
  heading_nodes <- html_file %>% html_nodes("p.MarginalNote, h4, h3, h2")
  heading_texts <- heading_nodes %>% html_text(trim = TRUE)
  heading_xpaths <- sapply(heading_nodes, xml_path, USE.NAMES = FALSE)
  return(data.table(XPath = heading_xpaths, Heading = heading_texts))
}

## Process Each HTML File ----
for (i in seq_along(html_files)) {
  file <- html_files[i]
  legislation_id <- i
  
  cat(sprintf("Processing file %d of %d: %s\n", i, length(html_files), basename(file)))
  
  tryCatch({
    raw_text <- tryCatch(readLines(file, warn = FALSE, encoding = "UTF-8"), error = function(e) return(NULL))
    if (is.null(raw_text)) {
      message(sprintf("Failed to read file: %s", file))
      bad_files <- c(bad_files, file)
      next
    }
    html_file <- read_html(paste(raw_text, collapse = "\n"))
    
    # Extract all paragraph nodes
    all_paragraphs <- html_file %>% html_nodes("p, div p, dl p, dd p, li p, ul p, dfn p, a p, span p")  
    headings_DT <- extract_headings(html_file)
    
    last_section <- NA
    last_heading <- NA
    last_xpath <- NA
    current_schedule <- NA  # Track which schedule we're in
    current_appendix <- NA  # Track which appendix we're in
    in_editorial_section <- FALSE  # Track if we're in an editorial section
    
    for (node in all_paragraphs) {
      current_xpath <- xml_path(node)
      paragraph_class <- xml_attr(node, "class")
      
      # Skip structural paragraphs like "part"
      if (!is.na(paragraph_class) && tolower(paragraph_class) %in% c("part")) {
        next
      }
      
      paragraph_text <- xml_text(node, trim = TRUE)
      paragraph_text <- stri_enc_toutf8(paragraph_text)
      
      # CRITICAL: Skip amendment references
      if (is_amendment_reference(paragraph_text)) {
        next
      }
      
      # Check if this is a schedule header
      if (is_schedule_header(paragraph_text)) {
        current_schedule <- extract_schedule_name(paragraph_text)
        current_appendix <- NA  # Clear appendix when entering schedule
        in_editorial_section <- FALSE  # Clear editorial flag
        last_section <- NA  # Reset section when entering a new schedule
        next  # Don't add the schedule header itself as a paragraph
      }
      
      # Check if this is an appendix header
      if (is_appendix_header(paragraph_text)) {
        current_appendix <- extract_appendix_name(paragraph_text)
        current_schedule <- NA  # Clear schedule when entering appendix
        in_editorial_section <- FALSE  # Clear editorial flag
        last_section <- NA  # Reset section when entering a new appendix
        next  # Don't add the appendix header itself as a paragraph
      }
      
      # CRITICAL: Skip footer/metadata content
      if (is_footer_or_metadata(paragraph_text)) {
        next
      }
      
      # Extract section number using enhanced function
      inline_section_number <- extract_inline_section(node)
      
      # Extract heading information
      preceding_heading <- xml_find_all(node, xpath = "preceding::*[self::p[@class='MarginalNote'] or self::h4 or self::h3 or self::h2][1]")
      assigned_heading <- ifelse(length(preceding_heading) > 0, xml_text(preceding_heading[length(preceding_heading)], trim = TRUE), last_heading)
      assigned_heading <- gsub("^Marginal note:\\s*", "", assigned_heading)
      
      # Check if the current heading is an editorial section
      if (!is.na(assigned_heading) && is_editorial_heading(assigned_heading)) {
        in_editorial_section <- TRUE
        last_section <- NA  # Don't carry sections through editorial headings
      }
      
      # If we have a new non-editorial heading, clear the editorial flag
      if (!is.na(assigned_heading) && assigned_heading != "" && !is_editorial_heading(assigned_heading)) {
        in_editorial_section <- FALSE
        last_heading <- assigned_heading
        last_xpath <- ifelse(length(preceding_heading) > 0, xml_path(preceding_heading[length(preceding_heading)]), last_xpath)
      }
      
      # Skip paragraphs in editorial sections
      if (in_editorial_section) {
        next
      }
      
      # Update section tracking
      if (!is.na(paragraph_class) && grepl("division", paragraph_class, ignore.case = TRUE)) {
        last_section <- NA
      } else if (!is.na(inline_section_number)) {
        last_section <- inline_section_number
      } 
      
      # Only add paragraphs that have valid content and section numbers
      if (nzchar(paragraph_text) && !is.na(last_section)) {
        # Determine section identifier based on context (main body, schedule, or appendix)
        section_identifier <- if (!is.na(current_schedule)) {
          paste0(current_schedule, "-", last_section)
        } else if (!is.na(current_appendix)) {
          paste0(current_appendix, "-", last_section)
        } else {
          last_section
        }
        
        paragraph_table <- rbind(paragraph_table, data.table(
          legislation_id = legislation_id,
          Section = section_identifier,
          Heading = last_heading,
          Paragraph = paragraph_text,
          XPath = current_xpath
        ), fill = TRUE)
      }
    }
    
  }, error = function(e) {
    message(sprintf("Error processing file %s: %s", file, e$message))
    bad_files <<- c(bad_files, file)
  })
}

## Filter and Clean ----
paragraph_table <- paragraph_table[!is.na(Section)]

# Escape filter words safely
filter_words <- c("repeal", "repealed", "revoked", "Marginal note", "Not in force")
escaped_words <- sapply(filter_words, function(w) paste0("\\b", stringr::str_replace_all(w, "([\\W])", "\\\\\\1"), "\\b"))
paragraph_table <- paragraph_table[!grepl(paste(escaped_words, collapse = "|"), Paragraph, ignore.case = TRUE)]

# Additional filter: Remove any remaining footer/metadata that slipped through
paragraph_table <- paragraph_table[!sapply(paragraph_table$Paragraph, is_footer_or_metadata)]

# ENHANCED: Remove paragraphs with editorial headings
paragraph_table <- paragraph_table[!sapply(paragraph_table$Heading, is_editorial_heading)]

# ENHANCED: Remove amendment references
paragraph_table <- paragraph_table[!sapply(paragraph_table$Paragraph, is_amendment_reference)]

## Add Unique paragraph_id ----
paragraph_table[, paragraph_id := .I]

## Remove XPath column ----
paragraph_table[, XPath := NULL]

## Save to SQLite Database ----
output_dir <- here("output")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

db_path <- file.path(output_dir, "legislation.db")
conn <- dbConnect(SQLite(), dbname = db_path)
dbWriteTable(conn, "LegislationParagraphs", paragraph_table, overwrite = TRUE)
dbDisconnect(conn)

## Save list of bad files ----
if (length(bad_files) > 0) {
  writeLines(bad_files, file.path(output_dir, "bad_html_files.txt"))
  cat("Some files failed to process. See 'bad_html_files.txt' for details.\n")
} else {
  cat("All files processed successfully.\n")
}

## Notify Completion ----
cat("✅ Labeling complete. Table saved to SQLite.\n")
cat(sprintf("Total paragraphs saved: %d\n", nrow(paragraph_table)))
beep(sound = 1)