################################################################################
# Title: Compendium Checker Export (User-Friendly Version with Chunking)
# Author: Copilot (Modified)
# Date: 2025-08-27
# Description:
#   Queries the legislation database and exports an Excel file containing
#   paragraph-level metadata, semantic labels, scope values, and matched keywords.
#   Modified to aggregate paragraphs by section and expand Management Domain/IUCN/Scope
#   into separate rows. Handles Excel's 32,767 character limit by chunking.
#   Governance-based management domains have blank IUCN values.
#   Separates keywords into management_domain_keywords and clause_type_keywords.
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
  
  # Load management domain threat table if it exists
  if("management_domain_threat_table" %in% dbListTables(conn)) {
    management_domain_threat <- as.data.table(dbReadTable(conn, "management_domain_threat_table"))
    cat("✅ Loaded management_domain_threat_table from database\n")
  } else {
    management_domain_threat <- NULL
    cat("⚠️  management_domain_threat_table not found in database\n")
  }
}, finally = {
  dbDisconnect(conn)
  cat("✅ Database connection closed.\n")
})

## Identify Governance-Only Management Domains ----
if(!is.null(management_domain_threat)) {
  # Get threat-based domains from the authoritative table
  threat_based_domains <- unique(management_domain_threat$management_domain)
  
  # Get all management domains from labels
  all_mgmt_domains <- unique(paragraph_label_table[
    label_type == "Management Domain" & !is.na(label_value),
    label_value
  ])
  
  # Governance-only domains are those NOT in the threat table
  governance_only_domains <- setdiff(all_mgmt_domains, threat_based_domains)
  
  cat("Governance-only domains:", paste(governance_only_domains, collapse = ", "), "\n")
  cat("Threat-based domains:", paste(threat_based_domains, collapse = ", "), "\n")
} else {
  governance_only_domains <- character(0)
  cat("⚠️  Cannot identify governance domains without management_domain_threat_table\n")
}

## Merge Paragraphs with Legislation Metadata ----
paragraphs_with_legislation <- merge(
  paragraph_table[, .(paragraph_id, legislation_id, Section, Heading, Paragraph)],
  legislation_table[, .(legislation_id, jurisdiction, act_name, legislation_name)],
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

## Extract Scope (will be split into separate rows later) ----
scope_labels <- paragraph_label_table[
  !is.na(scope),
  .(paragraph_id, scope)
]

## Extract and Aggregate Keywords by Type ----
# Management Domain Keywords
mgmt_domain_keyword_labels <- paragraph_label_table[
  label_type == "Management Domain" & !is.na(keyword),
  .(paragraph_id, keyword)
]
mgmt_domain_keyword_labels <- mgmt_domain_keyword_labels[, 
                                                         .(management_domain_keywords = paste(unique(keyword), collapse = "; ")), 
                                                         by = paragraph_id
]

# Clause Type Keywords
clause_type_keyword_labels <- paragraph_label_table[
  label_type == "Clause Type" & !is.na(keyword),
  .(paragraph_id, keyword)
]
clause_type_keyword_labels <- clause_type_keyword_labels[, 
                                                         .(clause_type_keywords = paste(unique(keyword), collapse = "; ")), 
                                                         by = paragraph_id
]

## Merge keywords to paragraph data ----
paragraphs_with_meta <- merge(paragraphs_with_legislation, mgmt_domain_keyword_labels, by = "paragraph_id", all.x = TRUE)
paragraphs_with_meta <- merge(paragraphs_with_meta, clause_type_keyword_labels, by = "paragraph_id", all.x = TRUE)

## Aggregate keywords by section ----
meta_aggregated <- paragraphs_with_meta[, .(
  management_domain_keywords = paste(unique(na.omit(management_domain_keywords)), collapse = "; "),
  clause_type_keywords = paste(unique(na.omit(clause_type_keywords)), collapse = "; ")
), by = .(jurisdiction, act_name, legislation_name, Section, Heading)]

# Replace empty strings with NA
meta_aggregated[management_domain_keywords == "", management_domain_keywords := NA]
meta_aggregated[clause_type_keywords == "", clause_type_keywords := NA]

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
# First, get all unique labels per section (including scope)
section_labels <- unique(paragraphs_with_labels[, .(
  jurisdiction, act_name, legislation_name, Section, Heading, label_type, label_value
)])

# Get unique scope values per section
section_scope <- unique(scope_labels[paragraph_id %in% paragraphs_with_legislation$paragraph_id, .(
  paragraph_id, scope
)])
section_scope <- merge(
  section_scope,
  paragraphs_with_legislation[, .(paragraph_id, jurisdiction, act_name, legislation_name, Section, Heading)],
  by = "paragraph_id"
)
section_scope <- unique(section_scope[, .(jurisdiction, act_name, legislation_name, Section, Heading, scope)])

# Aggregate paragraphs with chunking
paragraphs_aggregated <- paragraphs_with_legislation[, {
  chunks <- chunk_paragraphs(unique(Paragraph))
  list(
    Paragraph = chunks,
    chunk_id = seq_along(chunks),
    total_chunks = length(chunks)
  )
}, by = .(jurisdiction, act_name, legislation_name, Section, Heading)]

# Convert list columns to regular columns
paragraphs_aggregated <- paragraphs_aggregated[, .(
  Paragraph = unlist(Paragraph),
  chunk_id = unlist(chunk_id),
  total_chunks = unlist(total_chunks)
), by = .(jurisdiction, act_name, legislation_name, Section, Heading)]

## Step 2: Merge Labels Back ----
# Merge the section-level labels with each chunk
paragraphs_with_all_labels <- merge(
  paragraphs_aggregated,
  section_labels,
  by = c("jurisdiction", "act_name", "legislation_name", "Section", "Heading"),
  all.x = TRUE,
  allow.cartesian = TRUE
)

## Step 3: Reshape Labels ----
# Separate Clause Type (will be aggregated)
clause_type_labels <- paragraphs_with_all_labels[
  label_type == "Clause Type" & !is.na(label_value),
  .(Clause_Type = paste(unique(label_value), collapse = "; ")),
  by = .(jurisdiction, act_name, legislation_name, Section, Heading, chunk_id)
]

# Keep Management Domain and IUCN separate (one row per combination)
mgmt_iucn_labels <- unique(paragraphs_with_all_labels[
  label_type %in% c("Management Domain", "IUCN") & !is.na(label_value),
  .(jurisdiction, act_name, legislation_name, Section, Heading, Paragraph, chunk_id, total_chunks, label_type, label_value)
])

# Reshape to wide format
if(nrow(mgmt_iucn_labels) > 0) {
  mgmt_iucn_wide <- dcast(
    mgmt_iucn_labels,
    jurisdiction + act_name + legislation_name + Section + Heading + Paragraph + chunk_id + total_chunks ~ label_type,
    value.var = "label_value",
    fun.aggregate = function(x) if(length(x) > 0) x[1] else NA_character_
  )
  
  # CRITICAL: Set IUCN to NA for governance-only management domains
  if("Management Domain" %in% names(mgmt_iucn_wide) && "IUCN" %in% names(mgmt_iucn_wide) && length(governance_only_domains) > 0) {
    rows_to_clear <- mgmt_iucn_wide$`Management Domain` %in% governance_only_domains
    mgmt_iucn_wide[rows_to_clear, IUCN := NA_character_]
    cat("✅ Set IUCN to NA for", sum(rows_to_clear), "governance-only rows\n")
  }
  
  compendium_data <- copy(mgmt_iucn_wide)
} else {
  # If no Management Domain or IUCN labels exist
  compendium_data <- unique(paragraphs_aggregated)
  compendium_data[, `Management Domain` := NA_character_]
  compendium_data[, IUCN := NA_character_]
}

# Merge with Clause Type
compendium_data <- merge(
  compendium_data,
  clause_type_labels,
  by = c("jurisdiction", "act_name", "legislation_name", "Section", "Heading", "chunk_id"),
  all.x = TRUE
)

# Merge with scope (each scope value gets its own row)
compendium_data <- merge(
  compendium_data,
  section_scope,
  by = c("jurisdiction", "act_name", "legislation_name", "Section", "Heading"),
  all.x = TRUE,
  allow.cartesian = TRUE
)

# Merge with aggregated keywords
compendium_data <- merge(
  compendium_data,
  meta_aggregated,
  by = c("jurisdiction", "act_name", "legislation_name", "Section", "Heading"),
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

# Rename columns to final desired names (lowercase with underscores)
setnames(compendium_data, old = c("Section", "Heading", "Paragraph", "Management Domain", "IUCN", "Clause_Type", "scope"),
         new = c("section", "heading", "aggregate_paragraph", "management_domain", "iucn_threat", "clause_type", "scope"),
         skip_absent = TRUE)

# Ensure all required columns exist
required_cols <- c("jurisdiction", "management_domain", "iucn_threat", "clause_type", "scope", "management_domain_keywords", "clause_type_keywords")
for(col in required_cols) {
  if(!col %in% names(compendium_data)) {
    compendium_data[, (col) := NA_character_]
  }
}

# Reorder columns
desired_order <- c("jurisdiction", "act_name", "legislation_name", "section", "heading", "aggregate_paragraph",
                   "management_domain", "iucn_threat", "clause_type", "scope", 
                   "management_domain_keywords", "clause_type_keywords")
existing_order <- intersect(desired_order, names(compendium_data))
setcolorder(compendium_data, existing_order)

cat("✅ Final columns:", paste(names(compendium_data), collapse = ", "), "\n")

## Sort by jurisdiction, legislation and section ----
setorder(compendium_data, jurisdiction, act_name, legislation_name, section)

## Export to Excel in Output Directory ----
output_file <- file.path(here("output"), "LAPSE_full_compendium.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Compendium")

# Write data with default formatting
writeDataTable(wb, "Compendium", compendium_data)

saveWorkbook(wb, output_file, overwrite = TRUE)
cat("✅ Excel file 'LAPSE_full_compendium.xlsx' has been saved to the output directory.\n")
cat(sprintf("   Total rows: %d\n", nrow(compendium_data)))

# Check for any remaining character limit issues
char_counts <- nchar(compendium_data$aggregate_paragraph)
if(any(char_counts > 32767)) {
  cat("⚠️  Warning: Some cells still exceed Excel's limit. Consider further chunking.\n")
} else {
  cat("✅ All cells are within Excel's character limit.\n")
}

# Beep when done
beep(sound = 1)