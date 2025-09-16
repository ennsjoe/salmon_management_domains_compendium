################################################################################
# Title: Database Paragraph Table
# Authors: Joe Enns, Cory Lagasse, Max Elinson
# Date Created: 2025-08-07
# Purpose / Description: 
#   This script processes HTML files containing legislative paragraphs,
#   extracts relevant information, and saves it into a structured SQLite database.
# Dependencies: DBI, RSQLite, data.table, here, xml2, rvest, stringi, stringr
# Execution: Run in RStudio or via Rscript; ensure working directory is project root
# Inputs:
#   HTML files located in the "legislation_html" directory.
# Outputs:
#   A SQLite database file named "legislation.db" in the "output" directory,
#   containing a table with metadata about the legislative paragraphs.
################################################################################

## Set Working Directory ----
library(here)

## Load Libraries ----
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
  text <- stri_enc_toutf8(text)  # Normalize to UTF-8
  text <- stri_trans_general(text, "Latin-ASCII")
  text <- gsub("[^[:print:]]", "", text)
  return(trimws(text))
}

extract_inline_section <- function(node) {
  section_label <- node %>% html_nodes("span.secnum span.secnumholder b, a.sectionLabel span.sectionLabel") %>% html_text(trim = TRUE)
  if (length(section_label) > 0) return(clean_text(section_label)) else return(NA)
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
    
    all_paragraphs <- html_file %>% html_nodes("p, div p, dl p, dd p, li p, ul p, dfn p, a p, span p")  
    headings_DT <- extract_headings(html_file)
    
    last_section <- NA
    last_heading <- NA
    last_xpath <- NA
    
    for (node in all_paragraphs) {
      current_xpath <- xml_path(node)
      paragraph_class <- xml_attr(node, "class")
      
      # Skip structural paragraphs like "part"
      if (!is.na(paragraph_class) && tolower(paragraph_class) %in% c("part")) {
        next
      }
      
      inline_section_number <- extract_inline_section(node)
      
      preceding_heading <- xml_find_all(node, xpath = "preceding::*[self::p[@class='MarginalNote'] or self::h4 or self::h3 or self::h2][1]")
      assigned_heading <- ifelse(length(preceding_heading) > 0, xml_text(preceding_heading[length(preceding_heading)], trim = TRUE), last_heading)
      assigned_heading <- gsub("^Marginal note:\\s*", "", assigned_heading)
      
      if (!is.na(assigned_heading) && assigned_heading != "") {
        last_heading <- assigned_heading
        last_xpath <- ifelse(length(preceding_heading) > 0, xml_path(preceding_heading[length(preceding_heading)]), last_xpath)
      }
      
      if (!is.na(paragraph_class) && grepl("division", paragraph_class, ignore.case = TRUE)) {
        last_section <- NA
      } else if (!is.na(inline_section_number)) {
        last_section <- inline_section_number
      } 
      
      paragraph_text <- xml_text(node, trim = TRUE)
      paragraph_text <- stri_enc_toutf8(paragraph_text)  # Normalize here too
      
      if (nzchar(paragraph_text)) {
        paragraph_table <- rbind(paragraph_table, data.table(
          legislation_id = legislation_id,
          Section = last_section,
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
beep(sound = 1)
