################################################################################
# Title: Agency, CU, and Supporting Data Processing Script
# Authors: Joe Enns, Cory Lagasse
# Date Created: 2025-08-26
# Last Modified: 2025-01-XX
# Purpose / Description:
#   Loads supporting data tables (agencies, conservation units, legislation URLs, 
#   and actionable clause keywords) into the SQLite database for use in analysis 
#   and reporting.
# Dependencies: DBI, RSQLite, data.table, here, beepr
# Execution: Run in RStudio or via Rscript; ensure working directory is project root
# Inputs: 
#   - CSV files in "data/" directory:
#     * agencies.csv
#     * cu_ranking.csv
#     * legislation_url.csv
#     * actionable_clause_keywords.csv
# Outputs:
#   - Updates SQLite database "output/legislation.db" with four tables:
#     * agencies
#     * cu_ranking
#     * legislation_url
#     * actionable_clause_keywords
################################################################################

## Load Libraries ----
library(here)
library(data.table)
library(RSQLite)
library(beepr)

## Define file paths ----
agencies_path <- here("data", "agencies.csv")
cu_ranking_path <- here("data", "cu_ranking.csv")
legislation_url_path <- here("data", "legislation_url.csv")
actionable_clause_keywords_path <- here("data", "actionable_clause_keywords.csv")

## Read CSV files ----
## Note: Using file= explicitly to handle paths with spaces
## Also cleaning BOM characters and non-printable chars from key columns
cat("Reading CSV files...\n")

# Helper function to read CSV with BOM handling
read_csv_clean <- function(filepath) {
  dt <- fread(file = filepath, encoding = "UTF-8")
  # Clean BOM from first column name if present
  old_names <- names(dt)
  new_names <- gsub("^\ufeff", "", old_names)
  new_names <- gsub("^\xef\xbb\xbf", "", new_names)
  if(!identical(old_names, new_names)) {
    setnames(dt, new_names)
  }
  return(dt)
}

agencies <- read_csv_clean(agencies_path)
# Clean join column
if("act_name" %in% names(agencies)) {
  agencies[, act_name := trimws(gsub("[^[:print:]]", "", act_name))]
}
cat("  agencies:", nrow(agencies), "rows\n")

cu_ranking <- read_csv_clean(cu_ranking_path)
cat("  cu_ranking:", nrow(cu_ranking), "rows\n")

legislation_url <- read_csv_clean(legislation_url_path)
# Clean join column
if("legislation_name" %in% names(legislation_url)) {
  legislation_url[, legislation_name := trimws(gsub("[^[:print:]]", "", legislation_name))]
}
cat("  legislation_url:", nrow(legislation_url), "rows\n")

actionable_clause_keywords <- read_csv_clean(actionable_clause_keywords_path)
cat("  actionable_clause_keywords:", nrow(actionable_clause_keywords), "rows\n")

cat("CSV files loaded\n")

## Connect to SQLite database ----
output_dir <- here("output")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

db_path <- file.path(output_dir, "legislation.db")
conn <- dbConnect(SQLite(), dbname = db_path)

## Write tables to database ----
cat("\nWriting tables to database...\n")
dbWriteTable(conn, "agencies", agencies, overwrite = TRUE)
cat("  ✓ agencies table saved\n")

dbWriteTable(conn, "cu_ranking", cu_ranking, overwrite = TRUE)
cat("  ✓ cu_ranking table saved\n")

dbWriteTable(conn, "legislation_url", legislation_url, overwrite = TRUE)
cat("  ✓ legislation_url table saved\n")

dbWriteTable(conn, "actionable_clause_keywords", actionable_clause_keywords, overwrite = TRUE)
cat("  ✓ actionable_clause_keywords table saved\n")

## Disconnect ----
dbDisconnect(conn)

## Notify Completion ----
cat("\n✅ All tables saved to SQLite database.\n")
beep(sound = 1)