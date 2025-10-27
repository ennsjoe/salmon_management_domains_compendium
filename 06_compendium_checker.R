################################################################################
# Title: Compendium Checker Export
# Author: Copilot
# Date: 2025-08-27
# Description:
#   Queries the legislation database and exports an Excel file containing
#   paragraph-level metadata, semantic labels, scope values, and matched keywords.
################################################################################

## Load Libraries ----
library(DBI)
library(RSQLite)
library(data.table)
library(here)
library(openxlsx)
library(beepr)

## Connect to Database ----
db_path <- file.path(here("output"), "legislation.db")
conn <- dbConnect(SQLite(), dbname = db_path)

## Load Tables ----
tryCatch({
  legislation_table <- as.data.table(dbReadTable(conn, "LegislationMetadata"))
  paragraph_table <- as.data.table(dbReadTable(conn, "LegislationParagraphs"))
  paragraph_label_table <- as.data.table(dbReadTable(conn, "paragraph_label_table"))
}, finally = {
  dbDisconnect(conn)
  cat("✅ Database connection closed.\n")
})

## Merge Paragraphs with Legislation Metadata ----
paragraphs_with_legislation <- merge(
  paragraph_table[, .(paragraph_id, legislation_id, Section, Heading, Paragraph)],
  legislation_table[, .(legislation_id, act_name, legislation_name)],
  by = "legislation_id",
  all.x = TRUE
)

## Reshape Labels ----
labels_wide <- dcast(
  paragraph_label_table[
    label_type %in% c("Management Domain", "IUCN", "Clause Type") & !is.na(label_value)
  ],
  paragraph_id ~ label_type,
  value.var = "label_value",
  fun.aggregate = function(x) paste(unique(x), collapse = "; ")
)

## Extract Scope from All Label Types ----
scope_labels <- paragraph_label_table[
  !is.na(scope),
  .(paragraph_id, scope)
]
scope_labels <- scope_labels[, .(scope = paste(unique(scope), collapse = "; ")), by = paragraph_id]

## Extract Keywords ----
keyword_labels <- paragraph_label_table[
  !is.na(keyword),
  .(paragraph_id, keyword)
]
keyword_labels <- keyword_labels[, .(Keywords = paste(unique(keyword), collapse = "; ")), by = paragraph_id]

## Final Merge ----
compendium_data <- merge(paragraphs_with_legislation, labels_wide, by = "paragraph_id", all.x = TRUE)
compendium_data <- merge(compendium_data, scope_labels, by = "paragraph_id", all.x = TRUE)
compendium_data <- merge(compendium_data, keyword_labels, by = "paragraph_id", all.x = TRUE)

## Reorder Columns ----
setcolorder(compendium_data, c(
  "act_name", "legislation_name", "Section", "Heading", "Paragraph",
  "Management Domain", "IUCN", "Clause Type", "scope", "Keywords"
))

## Export to Excel in Output Directory ----
output_file <- file.path(here("output"), "Compendium Checker.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Compendium")
writeDataTable(wb, "Compendium", compendium_data)
saveWorkbook(wb, output_file, overwrite = TRUE)

cat("✅ Excel file 'Compendium Checker.xlsx' has been saved to the output directory.\n")

# At the end of your script
beep(sound = 1)  # You can choose from 1 to 11