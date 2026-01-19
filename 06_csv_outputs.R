################################################################################
# Title: CSV Outputs for TypeScript App
# Authors: [Your Name]
# Date Created: 2025-01-19
# Purpose / Description:
#   Exports normalized CSV files from the legislation database for use in a
#   TypeScript application. Outputs separate, normalized tables that can be
#   joined in the app layer.
#
# Outputs:
#   - legislation_metadata.csv  : Core legislation info (id, jurisdiction, type, names, url, agencies)
#   - paragraphs.csv            : Paragraph text with foreign key to legislation
#   - labels.csv                : Labels/tags with foreign key to paragraphs
#   - actionable_clauses.csv    : Actionable clause data (from 05_actionable_clauses.R)
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
cat("CSV Export for TypeScript App\n")
cat("====================================\n\n")

## Connect to Database ----
db_path <- file.path(here("output"), "legislation.db")

if (!file.exists(db_path)) {
  
  stop("Database not found at: ", db_path, "\nRun scripts 01-03 first.")
}

conn <- dbConnect(SQLite(), dbname = db_path)
cat("✓ Connected to database\n")

## Create output directory for CSVs ----
csv_output_dir <- file.path(here("output"), "csv")
if (!dir.exists(csv_output_dir)) {
  dir.create(csv_output_dir, recursive = TRUE)
  cat("✓ Created output directory:", csv_output_dir, "\n")
}

## ============================================================================
## 1. LEGISLATION METADATA
## ============================================================================
cat("\n--- Exporting legislation_metadata.csv ---\n")

legislation_metadata <- as.data.table(dbReadTable(conn, "LegislationMetadata"))

# Ensure consistent column naming (snake_case)
setnames(legislation_metadata, names(legislation_metadata), tolower(names(legislation_metadata)))

# Add URL from legislation_url table if available
if ("legislation_url" %in% dbListTables(conn)) {
  legislation_url <- as.data.table(dbReadTable(conn, "legislation_url"))
  setnames(legislation_url, names(legislation_url), tolower(names(legislation_url)))
  
  # Trim whitespace for clean joins
  legislation_url[, legislation_name := trimws(legislation_name)]
  legislation_metadata[, legislation_name := trimws(legislation_name)]
  
  legislation_metadata <- merge(
    legislation_metadata,
    legislation_url,
    by = "legislation_name",
    all.x = TRUE
  )
  cat("  ✓ Joined URLs to legislation metadata\n")
}

# Add agencies (concatenated for metadata view, separate table for lookups)
if ("agencies" %in% dbListTables(conn)) {
  agencies <- as.data.table(dbReadTable(conn, "agencies"))
  setnames(agencies, names(agencies), tolower(names(agencies)))
  agencies[, act_name := trimws(act_name)]
  agencies[, agency := trimws(agency)]
  
  # Aggregate agencies per act_name
  agencies_agg <- agencies[, .(agencies = paste(unique(agency), collapse = "; ")), by = act_name]
  legislation_metadata[, act_name := trimws(act_name)]
  
  legislation_metadata <- merge(
    legislation_metadata,
    agencies_agg,
    by = "act_name",
    all.x = TRUE
  )
  cat("  ✓ Joined agencies to legislation metadata\n")
}

# Reorder columns for clarity
desired_cols <- c("legislation_id", "jurisdiction", "legislation_type", 
                  "act_name", "legislation_name", "url", "agencies")
existing_cols <- intersect(desired_cols, names(legislation_metadata))
other_cols <- setdiff(names(legislation_metadata), desired_cols)
setcolorder(legislation_metadata, c(existing_cols, other_cols))

# Write CSV
fwrite(legislation_metadata, file.path(csv_output_dir, "legislation_metadata.csv"))
cat("  ✓ Exported:", nrow(legislation_metadata), "rows\n")

## ============================================================================
## 2. PARAGRAPHS
## ============================================================================
cat("\n--- Exporting paragraphs.csv ---\n")

paragraphs <- as.data.table(dbReadTable(conn, "LegislationParagraphs"))

# Ensure consistent column naming (snake_case)
setnames(paragraphs, names(paragraphs), tolower(names(paragraphs)))

# Clean text encoding issues (Â artifacts, smart quotes, etc.)
clean_text <- function(text) {
  text <- stri_enc_toutf8(text)                          # Ensure UTF-8
  text <- gsub("Â", "", text)
  text <- gsub("\u00A0", " ", text)
  text <- stri_trans_general(text, "Latin-ASCII")
  text <- gsub("[^[:print:]\n\t]", "", text)
  text <- trimws(text)
  return(text)
}

paragraphs[, paragraph := clean_text(paragraph)]
paragraphs[, heading := clean_text(heading)]
cat("  ✓ Cleaned text encoding\n")

# Reorder columns: foreign key first, then identifiers, then content
desired_cols <- c("paragraph_id", "legislation_id", "section", "heading", "paragraph")
existing_cols <- intersect(desired_cols, names(paragraphs))
other_cols <- setdiff(names(paragraphs), desired_cols)
setcolorder(paragraphs, c(existing_cols, other_cols))

# Write CSV
fwrite(paragraphs, file.path(csv_output_dir, "paragraphs.csv"))
cat("  ✓ Exported:", nrow(paragraphs), "rows\n")

## ============================================================================
## 3. LABELS
## ============================================================================
cat("\n--- Exporting labels.csv ---\n")

labels <- as.data.table(dbReadTable(conn, "paragraph_label_table"))

# Ensure consistent column naming (snake_case)
setnames(labels, names(labels), tolower(names(labels)))

# Reorder columns: foreign key first, then label info
desired_cols <- c("label_id", "paragraph_id", "label_type", "label_value", "keyword", "scope")
existing_cols <- intersect(desired_cols, names(labels))
other_cols <- setdiff(names(labels), desired_cols)
setcolorder(labels, c(existing_cols, other_cols))

# Write CSV
fwrite(labels, file.path(csv_output_dir, "labels.csv"))
cat("  ✓ Exported:", nrow(labels), "rows\n")



## ============================================================================
## 4. ACTIONABLE CLAUSES
## ============================================================================
cat("\n--- Exporting actionable_clauses.csv ---\n")

# Read from database table created by 05_actionable_clauses.R
if ("actionable_clauses" %in% dbListTables(conn)) {
  actionable_clauses <- as.data.table(dbReadTable(conn, "actionable_clauses"))
  setnames(actionable_clauses, names(actionable_clauses), tolower(names(actionable_clauses)))
  
  # Keep only essential columns (other data can be joined via paragraph_id)
  keep_cols <- c("paragraph_id", "actionable_type", "responsible_official", "discretion_type")
  actionable_clauses <- actionable_clauses[, ..keep_cols]
  
  fwrite(actionable_clauses, file.path(csv_output_dir, "actionable_clauses.csv"))
  cat("  ✓ Exported:", nrow(actionable_clauses), "rows\n")
} else {
  cat("  ⚠ actionable_clauses table not found - run 05_actionable_clauses.R first\n")
}


## ============================================================================
## SUMMARY
## ============================================================================
cat("\n====================================\n")
cat("EXPORT COMPLETE\n")
cat("====================================\n")
cat("Output directory:", csv_output_dir, "\n\n")

# List all exported files with sizes
csv_files <- list.files(csv_output_dir, pattern = "\\.csv$", full.names = TRUE)
for (f in csv_files) {
  size_kb <- round(file.info(f)$size / 1024, 1)
  cat(sprintf("  %s (%.1f KB)\n", basename(f), size_kb))
}

## Close Database Connection ----
dbDisconnect(conn)
cat("\n✓ Database connection closed\n")