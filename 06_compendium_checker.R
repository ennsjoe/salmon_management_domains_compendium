################################################################################
# Title: Compendium Checker Export (User-Friendly Version with Chunking)
# Author: Copilot (Modified)
# Date: 2025-08-27
# Description:
#   Queries the legislation database and exports an Excel file containing
#   paragraph-level metadata, semantic labels, scope values, and matched keywords.
#   Modified to aggregate paragraphs by section and expand Management Domain/IUCN
#   into separate rows. Handles Excel's 32,767 character limit by chunking.
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

## Merge with Labels ----
paragraphs_with_labels <- merge(
  paragraphs_with_legislation,
  paragraph_label_table[
    label_type %in% c("Management Domain", "IUCN", "Clause Type") & !is.na(label_value),
    .(paragraph_id, label_type, label_value)
  ],
  by = "paragraph_id",
  all.x = TRUE,
  allow.cartesian = TRUE
)

## Extract and Aggregate Scope ----
scope_labels <- paragraph_label_table[
  !is.na(scope),
  .(paragraph_id, scope)
]
scope_labels <- scope_labels[, .(scope_col = paste(unique(scope), collapse = "; ")), by = paragraph_id]

## Extract and Aggregate Keywords ----
keyword_labels <- paragraph_label_table[
  !is.na(keyword),
  .(paragraph_id, keyword)
]
keyword_labels <- keyword_labels[, .(keywords_col = paste(unique(keyword), collapse = "; ")), by = paragraph_id]

## Merge keywords and scope to paragraph data ----
paragraphs_with_meta <- merge(paragraphs_with_legislation, scope_labels, by = "paragraph_id", all.x = TRUE)
paragraphs_with_meta <- merge(paragraphs_with_meta, keyword_labels, by = "paragraph_id", all.x = TRUE)

## Aggregate keywords and scope by section ----
meta_aggregated <- paragraphs_with_meta[, .(
  scope_col = paste(unique(na.omit(scope_col)), collapse = "; "),
  keywords_col = paste(unique(na.omit(keywords_col)), collapse = "; ")
), by = .(act_name, legislation_name, Section, Heading)]

# Replace empty strings with NA
meta_aggregated[scope_col == "", scope_col := NA]
meta_aggregated[keywords_col == "", keywords_col := NA]

## Function to chunk paragraphs if they exceed Excel's limit ----
chunk_paragraphs <- function(paragraphs, max_chars = 30000) {
  # Leave some buffer below the 32,767 limit
  if (sum(nchar(paragraphs)) + length(paragraphs) * 2 <= max_chars) {
    return(list(paste(paragraphs, collapse = "\n\n")))
  }
  
  chunks <- list()
  current_chunk <- character(0)
  current_length <- 0
  
  for (p in paragraphs) {
    p_length <- nchar(p)
    # Check if adding this paragraph would exceed limit
    if (current_length + p_length + 2 > max_chars && length(current_chunk) > 0) {
      # Save current chunk and start new one
      chunks[[length(chunks) + 1]] <- paste(current_chunk, collapse = "\n\n")
      current_chunk <- character(0)
      current_length <- 0
    }
    current_chunk <- c(current_chunk, p)
    current_length <- current_length + p_length + 2
  }
  
  # Add the last chunk
  if (length(current_chunk) > 0) {
    chunks[[length(chunks) + 1]] <- paste(current_chunk, collapse = "\n\n")
  }
  
  return(chunks)
}

## Step 1: Aggregate Paragraphs by Section with Chunking ----
# First, get all unique labels per section
section_labels <- unique(paragraphs_with_labels[, .(
  act_name, legislation_name, Section, Heading, label_type, label_value
)])

# Aggregate paragraphs with chunking
paragraphs_aggregated <- paragraphs_with_legislation[, {
  chunks <- chunk_paragraphs(unique(Paragraph))
  list(
    Paragraph = chunks,
    chunk_id = seq_along(chunks),
    total_chunks = length(chunks)
  )
}, by = .(act_name, legislation_name, Section, Heading)]

# Convert list columns to regular columns
paragraphs_aggregated <- paragraphs_aggregated[, .(
  Paragraph = unlist(Paragraph),
  chunk_id = unlist(chunk_id),
  total_chunks = unlist(total_chunks)
), by = .(act_name, legislation_name, Section, Heading)]

## Step 2: Merge Labels Back ----
# Merge the section-level labels with each chunk
paragraphs_with_all_labels <- merge(
  paragraphs_aggregated,
  section_labels,
  by = c("act_name", "legislation_name", "Section", "Heading"),
  all.x = TRUE,
  allow.cartesian = TRUE
)

## Step 3: Reshape Labels ----
# Separate Clause Type (will be aggregated)
clause_type_labels <- paragraphs_with_all_labels[
  label_type == "Clause Type" & !is.na(label_value),
  .(Clause_Type = paste(unique(label_value), collapse = "; ")),
  by = .(act_name, legislation_name, Section, Heading, chunk_id)
]

# Keep Management Domain and IUCN separate (one row per value)
mgmt_iucn_labels <- unique(paragraphs_with_all_labels[
  label_type %in% c("Management Domain", "IUCN") & !is.na(label_value),
  .(act_name, legislation_name, Section, Heading, Paragraph, chunk_id, total_chunks, label_type, label_value)
])

# Reshape to wide format
if(nrow(mgmt_iucn_labels) > 0) {
  mgmt_iucn_wide <- dcast(
    mgmt_iucn_labels,
    act_name + legislation_name + Section + Heading + Paragraph + chunk_id + total_chunks ~ label_type,
    value.var = "label_value",
    fun.aggregate = function(x) if(length(x) > 0) x[1] else NA_character_
  )
  compendium_data <- copy(mgmt_iucn_wide)
} else {
  # If no Management Domain or IUCN labels exist
  compendium_data <- unique(paragraphs_aggregated)
  compendium_data[, Management_Domain := NA_character_]
  compendium_data[, IUCN := NA_character_]
}

# Merge with Clause Type
compendium_data <- merge(
  compendium_data,
  clause_type_labels,
  by = c("act_name", "legislation_name", "Section", "Heading", "chunk_id"),
  all.x = TRUE
)

# Merge with aggregated scope and keywords
compendium_data <- merge(
  compendium_data,
  meta_aggregated,
  by = c("act_name", "legislation_name", "Section", "Heading"),
  all.x = TRUE
)

# Update Section column in place (don't create new column)
compendium_data[, Section := ifelse(
  total_chunks > 1,
  paste0(Section, " (Part ", chunk_id, " of ", total_chunks, ")"),
  Section
)]

# Remove helper columns
compendium_data[, c("chunk_id", "total_chunks") := NULL]

# Rename to final desired names
setnames(compendium_data, "scope_col", "Scope", skip_absent = TRUE)
setnames(compendium_data, "keywords_col", "Keywords", skip_absent = TRUE)
setnames(compendium_data, "Clause_Type", "Clause Type", skip_absent = TRUE)

# Ensure Management Domain and IUCN exist
if(!"Management Domain" %in% names(compendium_data)) {
  compendium_data[, `Management Domain` := NA_character_]
}
if(!"IUCN" %in% names(compendium_data)) {
  compendium_data[, IUCN := NA_character_]
}

# Reorder columns
desired_order <- c("act_name", "legislation_name", "Section", "Heading", "Paragraph",
                   "Management Domain", "IUCN", "Clause Type", "Scope", "Keywords")
existing_order <- intersect(desired_order, names(compendium_data))
setcolorder(compendium_data, existing_order)

cat("✅ Final columns:", paste(names(compendium_data), collapse = ", "), "\n")

## Sort by legislation and section ----
setorder(compendium_data, act_name, legislation_name, Section)

## Export to Excel in Output Directory ----
output_file <- file.path(here("output"), "Compendium Checker.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Compendium")

# Write data with formatting
writeDataTable(wb, "Compendium", compendium_data)

# Set column widths for better readability
setColWidths(wb, "Compendium", cols = 1:ncol(compendium_data), widths = "auto")
setColWidths(wb, "Compendium", cols = 5, widths = 60)  # Paragraph column wider

# Enable text wrap for Paragraph column
paragraphStyle <- createStyle(wrapText = TRUE, valign = "top")
addStyle(wb, "Compendium", paragraphStyle, rows = 2:(nrow(compendium_data) + 1), cols = 5, gridExpand = TRUE)

saveWorkbook(wb, output_file, overwrite = TRUE)
cat("✅ Excel file 'Compendium Checker.xlsx' has been saved to the output directory.\n")
cat(sprintf("   Total rows: %d\n", nrow(compendium_data)))

# Check for any remaining character limit issues
char_counts <- nchar(compendium_data$Paragraph)
if(any(char_counts > 32767)) {
  cat("⚠️  Warning: Some cells still exceed Excel's limit. Consider further chunking.\n")
} else {
  cat("✅ All cells are within Excel's character limit.\n")
}

# Beep when done
beep(sound = 1)