################################################################################
# Title: Database Legislation Table
# Authors: Joe Enns, Cory Lagasse, Max Elinson
# Date Created: 2025-08-07
# Purpose / Description: 
#   This script processes HTML files containing metadata about legislation, 
#   extracts relevant information, and saves it into a structured SQLite database.
# Dependencies: DBI, RSQLite, data.table, here
# Execution: Run in RStudio or via Rscript; ensure working directory is project root
# Inputs: 
#   HTML files located in the "legislation_html" directory.
# Outputs:
#   A SQLite database file named "legislation.db" in the "output" directory,
#   containing a table with metadata about the legislation.
# Notes:
#  - The script uses the `here` package to manage file paths dynamically.
#  - It extracts information such as legislation ID, jurisdiction, type, act name,
#    and legislation name from the HTML files.
#  - The script handles both provincial and federal legislation based on metadata.
#  - It normalizes text to remove special characters and formats act names.
#  - The script uses `data.table` for efficient data manipulation and `RSQLite` for database operations.
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

## Initialize legislation_table ----
legislation_table <- data.table(
  legislation_id = integer(),
  jurisdiction = character(),
  legislation_type = character(),
  act_name = character(),
  legislation_name = character()
)

## Utility Functions ----
clean_text <- function(text) {
  text <- stri_trans_general(text, "Latin-ASCII")
  text <- gsub("[^[:print:]]", "", text)
  return(trimws(text))
}

format_act_name <- function(act_name) {
  act_name <- gsub("\\s*\\(.*?\\)|\\s*\\[.*?\\]", "", act_name)
  act_name <- tolower(act_name)
  act_name <- gsub("(^|\\s)([a-z])", "\\1\\U\\2", act_name, perl = TRUE)
  return(trimws(act_name))
}

extract_legislation_name <- function(html_file) {
  # Extract raw text from the title node
  legislation_name <- html_file %>% html_nodes("h1.HeadTitle, div#title h2") %>% html_text(trim = TRUE)
  
  # Clean and isolate the title portion before any parentheses
  legislation_name <- ifelse(length(legislation_name) > 0, clean_text(legislation_name[1]), "Unknown Legislation")
  
  # Remove anything in parentheses and trailing whitespace
  legislation_name <- gsub("\\s*\\(.*?$", "", legislation_name)
  legislation_name <- trimws(legislation_name)
  
  return(legislation_name)
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
                    fifelse(grepl("\\bAct\\b", legislation_name, ignore.case = TRUE), "Act", ""))))
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

## Process Each HTML File ----
for (i in seq_along(html_files)) {
  file <- html_files[i]
  legislation_id <- i
  html_file <- read_html(file)
  
  legislation_name <- extract_legislation_name(html_file)
  jurisdiction <- extract_jurisdiction(html_file)
  legislation_type <- extract_legislation_type(legislation_name)
  act_name <- extract_act_name(html_file, legislation_name, legislation_type)
  
  legislation_table <- rbind(legislation_table, data.table(
    legislation_id = legislation_id,
    jurisdiction = jurisdiction,
    legislation_type = legislation_type,
    act_name = act_name,
    legislation_name = legislation_name
  ), fill = TRUE)
}

## Save to SQLite Database ----
output_dir <- here("output")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

db_path <- file.path(output_dir, "legislation.db")
conn <- dbConnect(SQLite(), dbname = db_path)
dbWriteTable(conn, "LegislationMetadata", legislation_table, overwrite = TRUE)
dbDisconnect(conn)
