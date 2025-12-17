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
cat("Reading CSV files...\n")
agencies <- fread(file = agencies_path, colClasses = "character")
cu_ranking <- fread(file = cu_ranking_path, colClasses = "character")
legislation_url <- fread(file = legislation_url_path, colClasses = "character")
actionable_clause_keywords <- fread(file = actionable_clause_keywords_path, colClasses = "character")
cat("✓ CSV files loaded\n")

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