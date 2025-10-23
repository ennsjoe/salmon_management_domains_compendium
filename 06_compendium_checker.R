################################################################################
# Title: Compendium Checker Export
# Author: Copilot
# Date: 2025-08-27
# Description:
#   Queries the legislation database and exports an Excel file containing
#   paragraph-level metadata, semantic labels, scope values, and matched keywords.
# Dependencies: DBI, RSQLite, data.table, here, openxlsx, beepr
# Execution: Run in RStudio or via Rscript; ensure working directory is project root
# Inputs: 
#   - SQLite database: "output/legislation.db"
#     * LegislationMetadata table
#     * LegislationParagraphs table
#     * paragraph_label_table
# Outputs:
#   - Excel file: "output/Compendium Checker.xlsx"
#     Contains merged data with legislation name, sections, headings, paragraphs,
#     labels (Management Domain, IUCN, Clause Type), scope, and keywords
################################################################################

## Load Libraries ----
library(DBI)
library(RSQLite)
library(data.table)
library(here)
library(openxlsx)
library(beepr)

## Connect to Database ----
cat("Connecting to database...\n")
db_path <- file.path(here("output"), "legislation.db")
conn <- dbConnect(SQLite(), dbname = db_path)

## Load Tables ----
cat("Loading tables from database...\n")
legislation_table <- as.data.table(dbReadTable(conn, "LegislationMetadata"))
paragraph_table <- as.data.table(dbReadTable(conn, "LegislationParagraphs"))
paragraph_label_table <- as.data.table(dbReadTable(conn, "paragraph_label_table"))
dbDisconnect(conn)
cat("✓ Tables loaded\n")

## Merge Paragraphs with Legislation Metadata ----
cat("\nMerging paragraphs with legislation metadata...\n")
paragraphs_with_legislation <- merge(
  paragraph_table[, .(paragraph_id, legislation_id, Section, Heading, Paragraph)],
  legislation_table[, .(legislation_id, legislation_name)],
  by = "legislation_id",
  all.x = TRUE
)
cat(sprintf("  ✓ %d paragraphs with metadata\n", nrow(paragraphs_with_legislation)))

## Reshape Labels ----
cat("Reshaping label data...\n")
labels_wide <- dcast(
  paragraph_label_table[
    label_type %in% c("Management Domain", "IUCN", "Clause Type") & !is.na(label_value)
  ],
  paragraph_id ~ label_type,
  value.var = "label_value",
  fun.aggregate = function(x) paste(unique(x), collapse = "; ")
)
cat(sprintf("  ✓ %d paragraphs with labels\n", nrow(labels_wide)))

## Extract Scope from All Label Types ----
cat("Extracting scope values...\n")
scope_labels <- paragraph_label_table[
  !is.na(scope),
  .(paragraph_id, scope)
]
scope_labels <- scope_labels[, .(scope = paste(unique(scope), collapse = "; ")), by = paragraph_id]
cat(sprintf("  ✓ %d paragraphs with scope\n", nrow(scope_labels)))

## Extract Keywords ----
cat("Extracting keywords...\n")
keyword_labels <- paragraph_label_table[
  !is.na(keyword),
  .(paragraph_id, keyword)
]
keyword_labels <- keyword_labels[, .(Keywords = paste(unique(keyword), collapse = "; ")), by = paragraph_id]
cat(sprintf("  ✓ %d paragraphs with keywords\n", nrow(keyword_labels)))

## Final Merge ----
cat("\nMerging all data...\n")
compendium_data <- merge(paragraphs_with_legislation, labels_wide, by = "paragraph_id", all.x = TRUE)
compendium_data <- merge(compendium_data, scope_labels, by = "paragraph_id", all.x = TRUE)
compendium_data <- merge(compendium_data, keyword_labels, by = "paragraph_id", all.x = TRUE)
cat(sprintf("  ✓ Final dataset: %d rows, %d columns\n", nrow(compendium_data), ncol(compendium_data)))

## Reorder Columns ----
setcolorder(compendium_data, c(
  "legislation_name", "Section", "Heading", "Paragraph",
  "Management Domain", "IUCN", "Clause Type", "scope", "Keywords"
))

## Export to Excel in Output Directory ----
cat("\nCreating Excel file...\n")
output_dir <- here("output")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

output_file <- file.path(output_dir, "Compendium Checker.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Compendium")
writeDataTable(wb, "Compendium", compendium_data)
saveWorkbook(wb, output_file, overwrite = TRUE)

cat(sprintf("\n✅ Excel file 'Compendium Checker.xlsx' has been saved to: %s\n", output_dir))
beep(sound = 1)