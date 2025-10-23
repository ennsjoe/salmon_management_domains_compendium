################################################################################
# Title: Agency, CU, and RAMS Processing Script
# Authors: Joe Enns, Cory Lagasse
# Date Created: 2025-08-26
# Purpose / Description:
#   Loads supporting data tables (agencies, conservation units, ecological concern,
#   and legislation URLs) into the SQLite database for use in analysis and reporting.
# Dependencies: DBI, RSQLite, data.table, here, beepr
# Execution: Run in RStudio or via Rscript; ensure working directory is project root
# Inputs: 
#   - CSV files in "data/" directory:
#     * agencies.csv
#     * cu_ranking.csv
#     * NOAA_ecological_concern.csv
#     * legislation_url.csv
# Outputs:
#   - Updates SQLite database "output/legislation.db" with four tables:
#     * agencies
#     * cu_ranking
#     * NOAA_ecological_concern
#     * legislation_url
################################################################################

## Load Libraries ----
library(here)
library(data.table)
library(RSQLite)
library(beepr)

## Define file paths ----
agencies_path <- here("data", "agencies.csv")
cu_ranking_path <- here("data", "cu_ranking.csv")
ecological_concern_path <- here("data", "NOAA_ecological_concern.csv")
legislation_url_path <- here("data", "legislation_url.csv")

## Read CSV files ----
cat("Reading CSV files...\n")
agencies <- fread(agencies_path, colClasses = "character")
cu_ranking <- fread(cu_ranking_path, colClasses = "character")
ecological_concern <- fread(ecological_concern_path, colClasses = "character")
legislation_url <- fread(legislation_url_path, colClasses = "character")
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

dbWriteTable(conn, "NOAA_ecological_concern", ecological_concern, overwrite = TRUE)
cat("  ✓ NOAA_ecological_concern table saved\n")

dbWriteTable(conn, "legislation_url", legislation_url, overwrite = TRUE)
cat("  ✓ legislation_url table saved\n")

## Disconnect ----
dbDisconnect(conn)

## Notify Completion ----
cat("\n✅ All tables saved to SQLite database.\n")
beep(sound = 1)