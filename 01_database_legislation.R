################################################################################
# Title: Database Legislation Table
# Authors: Joe Enns, Cory Lagasse, Max Elinson
# Date Created: 2025-08-07
# Last Updated: 2025-01-XX
# Purpose / Description: 
#   This script processes HTML files containing metadata about legislation, 
#   extracts relevant information, and saves it into a structured SQLite database.
#   ENHANCED: Now extracts "current to" dates from legislation HTML and stores
#   a processing timestamp for data provenance tracking.
# Dependencies: DBI, RSQLite, data.table, here
# Execution: Run in RStudio or via Rscript; ensure working directory is project root
# Inputs: 
#   HTML files located in the "data/legislation_html" directory.
# Outputs:
#   A SQLite database file named "legislation.db" in the "output" directory,
#   containing:
#     - LegislationMetadata table with legislation info and currency dates
#     - processing_metadata table with run timestamps
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

cat("=====================================\n")
cat("Database Legislation Table Processing\n")
cat("=====================================\n\n")

## Record processing start time ----
processing_timestamp <- Sys.time()
cat(sprintf("Processing started: %s\n\n", format(processing_timestamp, "%Y-%m-%d %H:%M:%S %Z")))

## Define the folders dynamically using `here()` ----
html_dirs <- here("data", "legislation_html")

## Read all HTML files from the directory ----
html_files <- unlist(lapply(html_dirs, function(dir) {
  list.files(path = dir, pattern = "\\.html$", full.names = TRUE, recursive = TRUE)
}))

## Normalize paths to handle special characters ----
html_files <- normalizePath(html_files, winslash = "/", mustWork = FALSE)

## Debugging print: Confirm files found ----
cat("Total HTML files detected:", length(html_files), "\n")

## Stop if no files are found ----
if (length(html_files) == 0) stop("No HTML files found in the specified directories.")

## Initialize legislation_table with new current_to_date column ----
legislation_table <- data.table(
  legislation_id = integer(),
  jurisdiction = character(),
  legislation_type = character(),
  act_name = character(),
  legislation_name = character(),
  current_to_date = character()
)

## Utility Functions ----
clean_text <- function(text) {
  text <- stri_enc_toutf8(text)  # Convert to UTF-8 safely
  text <- stri_trans_general(text, "Latin-ASCII")
  text <- gsub("[^[:print:]]", "", text)
  return(trimws(text))
}

format_act_name <- function(act_name) {
  act_name <- gsub(r"(\s*\(.*?\))", "", act_name, perl = TRUE)
  act_name <- gsub(r"(\s*\[.*?\])", "", act_name, perl = TRUE)
  act_name <- tolower(act_name)
  act_name <- gsub("(^|\\s)([a-z])", "\\1\\U\\2", act_name, perl = TRUE)
  return(trimws(act_name))
}

extract_legislation_name <- function(html_file) {
  legislation_name <- html_file %>% html_nodes("h1.HeadTitle, div#title h2") %>% html_text(trim = TRUE)
  legislation_name <- ifelse(length(legislation_name) > 0, clean_text(legislation_name[1]), "Unknown Legislation")
  legislation_name <- gsub(r"(\s*\(.*?$)", "", legislation_name, perl = TRUE)
  return(trimws(legislation_name))
}

extract_jurisdiction <- function(html_file) {
  head_attrs <- xml_attrs(html_file %>% html_node("head"))
  meta_description <- html_file %>% html_node("meta[name='description']") %>% html_attr("content")
  meta_breadcrumb <- html_file %>% html_node("meta[name='breadcrumb']") %>% html_attr("content")
  
  jurisdiction <- ifelse(any(grepl("www.gov.bc.ca", head_attrs)) || grepl("British Columbia", meta_breadcrumb), "Provincial",
                         ifelse(!is.na(meta_description) && grepl("Federal laws of Canada", meta_description), "Federal", "Unknown"))
  return(jurisdiction)
}

extract_legislation_type <- function(legislation_name) {
  legislation_type <- fifelse(
    grepl("\\bRegulation\\b", legislation_name, ignore.case = TRUE) | 
      grepl("\\bRegulations\\b", legislation_name, ignore.case = TRUE), "Regulations",
    fifelse(grepl("\\bOrder\\b", legislation_name, ignore.case = TRUE) & grepl("\\bAct\\b", legislation_name, ignore.case = TRUE), "Order",
            fifelse(grepl("\\bOrder\\b", legislation_name, ignore.case = TRUE), "Order",
                    fifelse(grepl("\\bCode\\b", legislation_name, ignore.case = TRUE) & grepl("\\bAct\\b", legislation_name, ignore.case = TRUE), "Code",
                            fifelse(grepl("\\bCode\\b", legislation_name, ignore.case = TRUE), "Code",
                                    fifelse(grepl("\\bAct\\b", legislation_name, ignore.case = TRUE), "Act", ""))))))
  return(legislation_type)
}

extract_act_name <- function(html_file, legislation_name, legislation_type) {
  act_name <- if (legislation_type == "Act") {
    legislation_name
  } else {
    html_file %>% html_node("p.EnablingAct a, div#actname h2") %>% html_text(trim = TRUE)
  }
  
  act_name <- ifelse(length(act_name) > 0, clean_text(act_name), legislation_name)
  act_name <- format_act_name(act_name)
  return(act_name)
}

## NEW FUNCTION: Extract "current to" date from HTML ----
extract_current_to_date <- function(html_file, jurisdiction) {
  current_to_date <- NA_character_
  
  tryCatch({
    if (jurisdiction == "Federal") {
      # Federal format: <div class="info"><p id="assentedDate">Act current to 2025-09-29...
      # Also try: <p id="assentedDate">...current to YYYY-MM-DD...
      
      # Try multiple selectors for federal
      assented_node <- html_file %>% html_node("p#assentedDate")
      if (is.null(assented_node)) {
        assented_node <- html_file %>% html_node("div.info p")
      }
      
      if (!is.null(assented_node)) {
        assented_text <- html_text(assented_node, trim = TRUE)
        
        # Extract date in format YYYY-MM-DD
        date_match <- str_extract(assented_text, "current to\\s+(\\d{4}-\\d{2}-\\d{2})")
        if (!is.na(date_match)) {
          current_to_date <- str_extract(date_match, "\\d{4}-\\d{2}-\\d{2}")
        }
      }
      
    } else if (jurisdiction == "Provincial") {
      # Provincial format: <td class="currencysingle" colspan="4">This Act is current to May 20, 2025</td>
      # Also try: This Regulation is current to...
      
      currency_node <- html_file %>% html_node("td.currencysingle")
      if (is.null(currency_node)) {
        # Try alternative selectors
        currency_node <- html_file %>% html_node("td.currency")
      }
      
      if (!is.null(currency_node)) {
        currency_text <- html_text(currency_node, trim = TRUE)
        
        # Extract date in format "Month DD, YYYY" (e.g., "May 20, 2025")
        date_match <- str_extract(currency_text, "(January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{1,2},\\s+\\d{4}")
        
        if (!is.na(date_match)) {
          # Convert to ISO format YYYY-MM-DD
          parsed_date <- tryCatch({
            as.Date(date_match, format = "%B %d, %Y")
          }, error = function(e) NA)
          
          if (!is.na(parsed_date)) {
            current_to_date <- format(parsed_date, "%Y-%m-%d")
          } else {
            # Keep original format if parsing fails
            current_to_date <- date_match
          }
        }
      }
    }
  }, error = function(e) {
    # Return NA if any error occurs
    current_to_date <- NA_character_
  })
  
  return(current_to_date)
}

## Track problematic files ----
bad_files <- character()

## Process Each HTML File ----
cat("\nProcessing HTML files...\n")

for (i in seq_along(html_files)) {
  file <- html_files[i]
  legislation_id <- i
  
  # Progress indicator every 50 files
  if (i %% 50 == 0 || i == 1) {
    cat(sprintf("  Processing file %d of %d...\n", i, length(html_files)))
  }
  
  tryCatch({
    raw_text <- readLines(file, warn = FALSE, encoding = "UTF-8")
    html_file <- read_html(paste(raw_text, collapse = "\n"))
    legislation_name <- gsub("_", " ", tools::file_path_sans_ext(basename(file)))
    jurisdiction <- extract_jurisdiction(html_file)
    legislation_type <- extract_legislation_type(legislation_name)
    act_name <- extract_act_name(html_file, legislation_name, legislation_type)
    
    # Extract current_to_date
    current_to_date <- extract_current_to_date(html_file, jurisdiction)
    
    legislation_table <- rbind(legislation_table, data.table(
      legislation_id = legislation_id,
      jurisdiction = jurisdiction,
      legislation_type = legislation_type,
      act_name = act_name,
      legislation_name = legislation_name,
      current_to_date = current_to_date
    ), fill = TRUE)
    
  }, error = function(e) {
    message(sprintf("Error processing file %s: %s", file, e$message))
    bad_files <<- c(bad_files, file)
  })
}

## Create processing metadata table ----
processing_end_time <- Sys.time()
processing_duration <- difftime(processing_end_time, processing_timestamp, units = "secs")

processing_metadata <- data.table(
  run_id = 1,
  run_timestamp = format(processing_timestamp, "%Y-%m-%d %H:%M:%S"),
  run_timezone = Sys.timezone(),
  run_date = format(processing_timestamp, "%Y-%m-%d"),
  total_files_processed = length(html_files),
  successful_files = length(html_files) - length(bad_files),
  failed_files = length(bad_files),
  processing_duration_seconds = as.numeric(processing_duration),
  r_version = paste(R.version$major, R.version$minor, sep = "."),
  script_name = "01_database_legislation.R"
)

## Summary of current_to_date extraction ----
cat("\n--- Current-To Date Extraction Summary ---\n")
dates_extracted <- sum(!is.na(legislation_table$current_to_date))
cat(sprintf("  - Dates successfully extracted: %d / %d (%.1f%%)\n", 
            dates_extracted, nrow(legislation_table), 
            100 * dates_extracted / nrow(legislation_table)))

# Summary by jurisdiction
date_summary <- legislation_table[, .(
  total = .N,
  with_date = sum(!is.na(current_to_date)),
  without_date = sum(is.na(current_to_date))
), by = jurisdiction]
cat("\n  By jurisdiction:\n")
for (i in seq_len(nrow(date_summary))) {
  cat(sprintf("    %s: %d/%d with dates\n", 
              date_summary$jurisdiction[i],
              date_summary$with_date[i],
              date_summary$total[i]))
}

## Save to SQLite Database ----
cat("\n--- Saving to SQLite Database ---\n")

output_dir <- here("output")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

db_path <- file.path(output_dir, "legislation.db")
conn <- dbConnect(SQLite(), dbname = db_path)

# Write legislation metadata table
dbWriteTable(conn, "LegislationMetadata", legislation_table, overwrite = TRUE)
cat("  ✓ LegislationMetadata table saved\n")

# Write or append processing metadata
# Check if table exists and get max run_id
if (dbExistsTable(conn, "processing_metadata")) {
  existing_metadata <- dbReadTable(conn, "processing_metadata")
  max_run_id <- max(existing_metadata$run_id, na.rm = TRUE)
  processing_metadata$run_id <- max_run_id + 1
  
  # Append new record
  dbWriteTable(conn, "processing_metadata", processing_metadata, append = TRUE)
  cat(sprintf("  ✓ Processing metadata appended (run_id: %d)\n", processing_metadata$run_id))
} else {
  # Create new table
  dbWriteTable(conn, "processing_metadata", processing_metadata, overwrite = TRUE)
  cat("  ✓ Processing metadata table created (run_id: 1)\n")
}

dbDisconnect(conn)

## Save list of bad files ----
if (length(bad_files) > 0) {
  writeLines(bad_files, file.path(output_dir, "bad_html_files.txt"))
  cat("Some files failed to process. See 'bad_html_files.txt' for details.\n")
} else {
  cat("All files processed successfully.\n")
}

## Final Summary ----
cat("\n=====================================\n")
cat("PROCESSING COMPLETE\n")
cat("=====================================\n\n")
cat(sprintf("Total legislation records: %d\n", nrow(legislation_table)))
cat(sprintf("  - Federal: %d\n", sum(legislation_table$jurisdiction == "Federal")))
cat(sprintf("  - Provincial: %d\n", sum(legislation_table$jurisdiction == "Provincial")))
cat(sprintf("  - Unknown: %d\n", sum(legislation_table$jurisdiction == "Unknown")))
cat(sprintf("\nProcessing timestamp: %s\n", format(processing_timestamp, "%Y-%m-%d %H:%M:%S %Z")))
cat(sprintf("Database saved to: %s\n", db_path))

## Notify Completion ----
cat("\n✅ Labeling complete. Table saved to SQLite.\n")
beep(sound = 1)
