################################################################################
# Title: Full Data CSV Export for TypeScript/Java/Vite App
# Authors: [Your Name]
# Date Created: 2025-01-19
# Last Modified: 2025-01-20
# Purpose / Description:
#   Exports CSV files from the legislation database for use in a TypeScript/Java/Vite
#   application. Outputs two files:
#   - legislation_output.csv: Legislation info (joinable by legislation_id)
#   - paragraph_output.csv: Paragraphs with labels and actionable clauses (one row per paragraph)
#
# Outputs:
#   - output/legislation_output.csv : Legislation metadata (id, jurisdiction, type, names, url, agencies)
#   - output/paragraph_output.csv : Paragraphs with all labels aggregated (one row per paragraph)
#
# Dependencies: DBI, RSQLite, data.table, here, stringi
################################################################################

## Load Libraries ----
library(DBI)
library(RSQLite)
library(data.table)
library(here)
library(stringi)

cat("====================================\n")
cat("Full Data CSV Export for App\n")
cat("====================================\n\n")

## Connect to Database ----
db_path <- file.path(here("output"), "legislation.db")

if (!file.exists(db_path)) {
  stop("Database not found at: ", db_path, "\nRun scripts 01-05 first.")
}

conn <- dbConnect(SQLite(), dbname = db_path)
cat("Connected to database\n")

## Create output directory ----
output_dir <- file.path(here("output"))
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

## ============================================================================
## TEXT CLEANING FUNCTION
## ============================================================================

clean_text <- function(text) {
  text <- stri_enc_toutf8(text)                          # Ensure UTF-8
  text <- gsub("\u00c3\u0082", "", text)                 # Fix mojibake
  text <- gsub("\u00A0", " ", text)                      # Non-breaking space
  text <- stri_trans_general(text, "Latin-ASCII")
  text <- gsub("[^[:print:]\n\t]", "", text)
  text <- trimws(text)
  return(text)
}

## ============================================================================
## FILE 1: LEGISLATION OUTPUT (SEPARATE CSV)
## ============================================================================
cat("\n--- Exporting legislation_output.csv ---\n")

legislation_metadata <- as.data.table(dbReadTable(conn, "LegislationMetadata"))
setnames(legislation_metadata, names(legislation_metadata), tolower(names(legislation_metadata)))

# Add URL from legislation_url table if available
if ("legislation_url" %in% dbListTables(conn)) {
  legislation_url <- as.data.table(dbReadTable(conn, "legislation_url"))
  setnames(legislation_url, names(legislation_url), tolower(names(legislation_url)))
  legislation_url[, legislation_name := trimws(legislation_name)]
  legislation_metadata[, legislation_name := trimws(legislation_name)]
  
  legislation_metadata <- merge(
    legislation_metadata,
    legislation_url,
    by = "legislation_name",
    all.x = TRUE
  )
  cat("  Joined URLs\n")
}

# Add agencies
if ("agencies" %in% dbListTables(conn)) {
  agencies <- as.data.table(dbReadTable(conn, "agencies"))
  setnames(agencies, names(agencies), tolower(names(agencies)))
  agencies[, act_name := trimws(act_name)]
  agencies[, agency := trimws(agency)]
  
  agencies_agg <- agencies[, .(agencies = paste(unique(agency), collapse = "; ")), by = act_name]
  legislation_metadata[, act_name := trimws(act_name)]
  
  legislation_metadata <- merge(
    legislation_metadata,
    agencies_agg,
    by = "act_name",
    all.x = TRUE
  )
  cat("  Joined agencies\n")
}

# Reorder columns
desired_meta_cols <- c("legislation_id", "jurisdiction", "legislation_type", 
                       "act_name", "legislation_name", "url", "agencies")
existing_meta_cols <- intersect(desired_meta_cols, names(legislation_metadata))
other_meta_cols <- setdiff(names(legislation_metadata), desired_meta_cols)
setcolorder(legislation_metadata, c(existing_meta_cols, other_meta_cols))

# Write legislation_output.csv
meta_output_file <- file.path(output_dir, "legislation_output.csv")
fwrite(legislation_metadata, meta_output_file, sep = ",", na = "", quote = TRUE)

meta_size_kb <- round(file.info(meta_output_file)$size / 1024, 1)
cat(sprintf("  Exported: %d rows (%.1f KB)\n", nrow(legislation_metadata), meta_size_kb))

## ============================================================================
## FILE 2: PARAGRAPH OUTPUT (PARAGRAPHS + LABELS + ACTIONABLE CLAUSES)
## ============================================================================

## STEP 1: LOAD PARAGRAPHS (BASE TABLE)
cat("\n--- Building paragraph_output.csv ---\n")
cat("Step 1: Loading paragraphs (base table)\n")

paragraphs <- as.data.table(dbReadTable(conn, "LegislationParagraphs"))
setnames(paragraphs, names(paragraphs), tolower(names(paragraphs)))

# Clean text encoding
paragraphs[, paragraph := clean_text(paragraph)]
paragraphs[, heading := clean_text(heading)]

cat(sprintf("  Loaded %d paragraphs\n", nrow(paragraphs)))

# Start with paragraphs as base (keep legislation_id as foreign key)
full_data <- copy(paragraphs)

## STEP 2: LOAD LABELS TABLE
cat("\nStep 2: Loading labels\n")

labels <- as.data.table(dbReadTable(conn, "paragraph_label_table"))
setnames(labels, names(labels), tolower(names(labels)))

## STEP 3: JOIN MANAGEMENT DOMAIN LABELS (AGGREGATED)
cat("\nStep 3: Aggregating Management Domain labels\n")

# Filter for Management Domain labels and aggregate by paragraph_id
mgmt_domain_labels <- labels[label_type == "Management Domain", 
                             .(management_domain = paste(unique(label_value[!is.na(label_value)]), collapse = "; "),
                               mgmt_d_keyword = paste(unique(keyword[!is.na(keyword)]), collapse = "; ")),
                             by = paragraph_id]

# Clean up empty strings from aggregation
mgmt_domain_labels[management_domain == "", management_domain := NA_character_]
mgmt_domain_labels[mgmt_d_keyword == "", mgmt_d_keyword := NA_character_]

cat(sprintf("  Aggregated Management Domain labels for %d paragraphs\n", nrow(mgmt_domain_labels)))

# Join - no row duplication since values are aggregated
full_data <- merge(
  full_data,
  mgmt_domain_labels,
  by = "paragraph_id",
  all.x = TRUE
)

cat(sprintf("  After Management Domain join: %d rows\n", nrow(full_data)))

## STEP 4: JOIN CLAUSE TYPE LABELS (AGGREGATED)
cat("\nStep 4: Aggregating Clause Type labels\n")

# Filter for Clause Type labels and aggregate by paragraph_id
clause_type_labels <- labels[label_type == "Clause Type", 
                             .(clause_type = paste(unique(label_value[!is.na(label_value)]), collapse = "; "),
                               clause_type_keyword = paste(unique(keyword[!is.na(keyword)]), collapse = "; ")),
                             by = paragraph_id]

# Clean up empty strings
clause_type_labels[clause_type == "", clause_type := NA_character_]
clause_type_labels[clause_type_keyword == "", clause_type_keyword := NA_character_]

cat(sprintf("  Aggregated Clause Type labels for %d paragraphs\n", nrow(clause_type_labels)))

# Join - no row duplication
full_data <- merge(
  full_data,
  clause_type_labels,
  by = "paragraph_id",
  all.x = TRUE
)

cat(sprintf("  After Clause Type join: %d rows\n", nrow(full_data)))

## STEP 5: JOIN ACTIONABLE CLAUSES
cat("\nStep 5: Joining actionable clauses\n")

if ("actionable_clauses" %in% dbListTables(conn)) {
  actionable_clauses <- as.data.table(dbReadTable(conn, "actionable_clauses"))
  setnames(actionable_clauses, names(actionable_clauses), tolower(names(actionable_clauses)))
  
  # Check if paragraph_id exists in actionable_clauses table
  if ("paragraph_id" %in% names(actionable_clauses)) {
    # Keep only the columns we need for joining
    ac_cols <- c("paragraph_id", "actionable_type", "responsible_official", "discretion_type")
    ac_cols <- intersect(ac_cols, names(actionable_clauses))
    actionable_clauses <- actionable_clauses[, ..ac_cols]
    
    full_data <- merge(
      full_data,
      actionable_clauses,
      by = "paragraph_id",
      all.x = TRUE
    )
    
    cat(sprintf("  Joined actionable clauses: %d rows\n", nrow(full_data)))
  } else {
    # If paragraph_id doesn't exist, match by paragraph text
    cat("  Warning: paragraph_id not found in actionable_clauses table\n")
    cat("  Attempting to match by paragraph text...\n")
    
    if ("paragraph" %in% names(actionable_clauses)) {
      ac_cols <- c("paragraph", "actionable_type", "responsible_official", "discretion_type")
      ac_cols <- intersect(ac_cols, names(actionable_clauses))
      actionable_clauses <- actionable_clauses[, ..ac_cols]
      
      full_data <- merge(
        full_data,
        actionable_clauses,
        by = "paragraph",
        all.x = TRUE
      )
      cat(sprintf("  Joined actionable clauses by paragraph text: %d rows\n", nrow(full_data)))
    } else {
      cat("  Warning: Could not join actionable_clauses - no matching column found\n")
      full_data[, actionable_type := NA_character_]
      full_data[, responsible_official := NA_character_]
      full_data[, discretion_type := NA_character_]
    }
  }
} else {
  cat("  Warning: actionable_clauses table not found - run 05_actionable_clauses.R first\n")
  full_data[, actionable_type := NA_character_]
  full_data[, responsible_official := NA_character_]
  full_data[, discretion_type := NA_character_]
}

## STEP 6: REORDER AND CLEAN UP COLUMNS
cat("\nStep 6: Finalizing output\n")

# Define desired column order (legislation_id is foreign key to legislation_metadata.csv)
desired_cols <- c(
  # Identifiers
  "paragraph_id", "legislation_id",
  # Paragraph content
  "section", "heading", "paragraph",
  # Management Domain labels (aggregated)
  "management_domain", "mgmt_d_keyword",
  # Clause Type labels (aggregated)
  "clause_type", "clause_type_keyword",
  # Actionable clauses
  "actionable_type", "responsible_official", "discretion_type"
)

# Keep only columns that exist
existing_cols <- intersect(desired_cols, names(full_data))
other_cols <- setdiff(names(full_data), desired_cols)
setcolorder(full_data, c(existing_cols, other_cols))

# Sort by paragraph_id
setkey(full_data, paragraph_id)

cat(sprintf("  Final dataset: %d rows, %d columns\n", nrow(full_data), ncol(full_data)))

## STEP 7: EXPORT TO CSV
cat("\nStep 7: Exporting paragraph_output.csv\n")

output_file <- file.path(output_dir, "paragraph_output.csv")
fwrite(full_data, output_file, sep = ",", na = "", quote = TRUE)

file_size_kb <- round(file.info(output_file)$size / 1024, 1)
file_size_mb <- round(file_size_kb / 1024, 2)
cat(sprintf("  Exported to: %s\n", output_file))
cat(sprintf("  File size: %.1f KB (%.2f MB)\n", file_size_kb, file_size_mb))

## ============================================================================
## SUMMARY
## ============================================================================
cat("\n====================================\n")
cat("EXPORT COMPLETE\n")
cat("====================================\n\n")

cat("Output files:\n")
cat(sprintf("  1. legislation_output.csv: %d rows (%.1f KB)\n", 
            nrow(legislation_metadata), meta_size_kb))
cat(sprintf("  2. paragraph_output.csv: %d rows (%.1f KB)\n", 
            nrow(full_data), file_size_kb))
cat(sprintf("\nTotal size: %.1f KB (%.2f MB)\n", 
            meta_size_kb + file_size_kb, (meta_size_kb + file_size_kb) / 1024))

cat("\nparagraph_output.csv columns:\n")
for (col in names(full_data)) {
  non_na <- sum(!is.na(full_data[[col]]) & full_data[[col]] != "")
  cat(sprintf("  - %s: %d non-empty values\n", col, non_na))
}

cat("\nJoin key: legislation_id (paragraph_output.csv -> legislation_output.csv)\n")

## Close Database Connection ----
dbDisconnect(conn)
cat("\nDatabase connection closed\n")