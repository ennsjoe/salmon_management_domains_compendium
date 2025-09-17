################################################################################
# Title: Agency, CU, and RAMS Processing Script
# Authors: Joe Enns, Cory Lagasse
# Date Created: 2025-08-26
# Purpose / Description:
# 
################################################################################

## Load Libraries ----
library(here)
library(data.table)
library(RSQLite)
library(beepr)

## Define file paths ----
agencies_path <- here("agencies.csv")
cu_ranking_path <- here("cu_ranking.csv")
ecological_concern_path <- here("NOAA_ecological_concern.csv")

## Read CSV files ----
agencies <- fread(agencies_path, colClasses = "character")
cu_ranking <- fread(cu_ranking_path, colClasses = "character")
ecological_concern <- fread(ecological_concern_path, colClasses = "character")

## Connect to SQLite database ----
output_dir <- here("output")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

db_path <- file.path(output_dir, "legislation.db")
conn <- dbConnect(SQLite(), dbname = db_path)

## Write tables to database ----
dbWriteTable(conn, "agencies", agencies, overwrite = TRUE)
dbWriteTable(conn, "cu_ranking", cu_ranking, overwrite = TRUE)
dbWriteTable(conn, "NOAA_ecological_concern", ecological_concern, overwrite = TRUE)

## Disconnect ----
dbDisconnect(conn)

## Notify Completion ----
cat("✅ Labeling complete. Table saved to SQLite.\n")
beep(sound = 1)