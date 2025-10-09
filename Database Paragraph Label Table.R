################################################################################
# Title: Simplified Paragraph Label Table
# Authors: Joe Enns, Cory Lagasse, Max Elinson
# Date Created: 2025-08-07
# Last Modified: 2025-10-09
# Purpose / Description:
#   Processes legislative paragraphs from SQLite, matches keywords across categories
#   from both paragraph and heading text, assigns scope and label values, and builds
#   a labeled table with deduplicated entries.
# Dependencies: DBI, RSQLite, data.table, here, stringr, quanteda, beepr
################################################################################

## Load Libraries ----
library(here)
library(data.table)
library(RSQLite)
library(stringr)
library(quanteda)
library(beepr)

## Connect to SQLite and Load Paragraphs ----
db_path <- file.path(here("output"), "legislation.db")
conn <- dbConnect(SQLite(), dbname = db_path)
paragraph_table <- as.data.table(dbReadTable(conn, "LegislationParagraphs"))

## Load Keyword Tables ----
clause_type_keywords <- fread(here("clause_type_keywords.csv"), colClasses = "character")
iucn_l2_keywords <- fread(here("iucn_l2_keywords.csv"), colClasses = "character")
salmon_scope_keywords <- fread(here("salmon_scope_keywords.csv"), colClasses = "character")
governance_keywords <- fread(here("governance_keywords.csv"), colClasses = "character")
management_domain_threat_table <- fread(here("management_domain_threat_table.csv"), colClasses = "character")

## Normalize keyword tables to match text cleaning ----
# Apply consistent normalization: lowercase and collapse whitespace
# This ensures keywords will match the cleaned paragraph text
# IMPORTANT: Only normalize 'keyword' columns, NOT label_value columns
# (e.g., iucn_l2, management_domain, clause_type should keep original case)
cat("Normalizing keyword tables...\n")

# Normalize keywords only - keep label values in original case
iucn_l2_keywords[, keyword := tolower(str_squish(keyword))]
# Do NOT normalize: iucn_l2 (keep original case for queries)

governance_keywords[, keyword := tolower(str_squish(keyword))]
# Do NOT normalize: management_domain (keep original case for queries)

salmon_scope_keywords[, keyword := tolower(str_squish(keyword))]

clause_type_keywords[, keyword := tolower(str_squish(keyword))]
# Do NOT normalize: clause_type (keep original case for queries)

# For lookup table: trim whitespace but keep case
management_domain_threat_table[, iucn_l2 := str_squish(iucn_l2)]
management_domain_threat_table[, management_domain := str_squish(management_domain)]

cat("✓ Keyword normalization complete\n")

## Save Normalized Keyword Tables to SQLite ----
# Save after normalization so database contains consistent data
cat("Saving keyword tables to SQLite...\n")

dbWriteTable(conn, "clause_type_keywords", clause_type_keywords, overwrite = TRUE)
dbWriteTable(conn, "iucn_l2_keywords", iucn_l2_keywords, overwrite = TRUE)
dbWriteTable(conn, "salmon_scope_keywords", salmon_scope_keywords, overwrite = TRUE)
dbWriteTable(conn, "governance_keywords", governance_keywords, overwrite = TRUE)
dbWriteTable(conn, "management_domain_threat_table", management_domain_threat_table, overwrite = TRUE)

cat("✓ Keyword tables saved to database\n")

## Sort keywords by length for optimal token compounding ----
# Longer phrases should be compounded first to prevent shorter phrases
# from consuming parts of longer ones (e.g., "water quality" before "water")
iucn_l2_keywords <- iucn_l2_keywords[order(-nchar(keyword))]
governance_keywords <- governance_keywords[order(-nchar(keyword))]
salmon_scope_keywords <- salmon_scope_keywords[order(-nchar(keyword))]
clause_type_keywords <- clause_type_keywords[order(-nchar(keyword))]

## Matching Function ----
match_keywords <- function(tokens, paragraph_id, keyword_list, label_type, value_column = NULL) {
  matches <- list()
  for (kw in keyword_list$keyword) {
    # Keywords are already normalized to lowercase
    if (kw %in% gsub("_", " ", tokens)) {
      label_value <- if (!is.null(value_column) && value_column %in% names(keyword_list)) {
        keyword_list[keyword == kw, get(value_column)]
      } else {
        NA_character_
      }
      matches <- append(matches, list(data.table(
        paragraph_id = paragraph_id,
        label_type = label_type,
        keyword = kw,
        label_value = label_value
      )))
    }
  }
  return(rbindlist(matches))
}

## Tokenize and Match Paragraph Text ----
cat("\nProcessing paragraph text...\n")

cleaned_paragraphs <- str_squish(tolower(str_replace_all(paragraph_table$Paragraph, "[[:punct:]]", " ")))
corpus_obj <- corpus(data.frame(text = cleaned_paragraphs))
tokens_obj <- tokens(corpus_obj, remove_punct = TRUE)

# Compound multi-word phrases in order of decreasing length
# Keywords are already normalized, no need for tolower()
tokens_obj <- tokens_compound(tokens_obj, pattern = phrase(iucn_l2_keywords$keyword))
tokens_obj <- tokens_compound(tokens_obj, pattern = phrase(governance_keywords$keyword))
tokens_obj <- tokens_compound(tokens_obj, pattern = phrase(salmon_scope_keywords$keyword))
tokens_obj <- tokens_compound(tokens_obj, pattern = phrase(clause_type_keywords$keyword))

token_list <- lapply(as.list(tokens_obj), as.character)

cat("Matching keywords to paragraphs...\n")

paragraph_label_table <- rbindlist(
  mapply(function(tokens, pid) {
    rbindlist(list(
      match_keywords(tokens, pid, iucn_l2_keywords, "IUCN", "iucn_l2"),
      match_keywords(tokens, pid, governance_keywords, "Management Domain", "management_domain"),
      match_keywords(tokens, pid, salmon_scope_keywords, "Salmon Scope", NULL),
      match_keywords(tokens, pid, clause_type_keywords, "Clause Type", "clause_type")
    ), fill = TRUE)
  }, token_list, paragraph_table$paragraph_id, SIMPLIFY = FALSE)
)

cat("✓ Paragraph text matching complete\n")

## Tokenize and Match Heading Text ----
cat("Processing heading text...\n")

cleaned_headings <- str_squish(tolower(str_replace_all(paragraph_table$Heading, "[[:punct:]]", " ")))
heading_corpus <- corpus(data.frame(text = cleaned_headings))
heading_tokens <- tokens(heading_corpus, remove_punct = TRUE)

# Compound multi-word phrases in order of decreasing length
heading_tokens <- tokens_compound(heading_tokens, pattern = phrase(iucn_l2_keywords$keyword))
heading_tokens <- tokens_compound(heading_tokens, pattern = phrase(governance_keywords$keyword))
heading_tokens <- tokens_compound(heading_tokens, pattern = phrase(salmon_scope_keywords$keyword))
heading_tokens <- tokens_compound(heading_tokens, pattern = phrase(clause_type_keywords$keyword))

heading_token_list <- lapply(as.list(heading_tokens), as.character)

cat("Matching keywords to headings...\n")

heading_label_table <- rbindlist(
  mapply(function(tokens, pid) {
    rbindlist(list(
      match_keywords(tokens, pid, iucn_l2_keywords, "IUCN", "iucn_l2"),
      match_keywords(tokens, pid, governance_keywords, "Management Domain", "management_domain"),
      match_keywords(tokens, pid, salmon_scope_keywords, "Salmon Scope", NULL),
      match_keywords(tokens, pid, clause_type_keywords, "Clause Type", "clause_type")
    ), fill = TRUE)
  }, heading_token_list, paragraph_table$paragraph_id, SIMPLIFY = FALSE)
)

cat("✓ Heading text matching complete\n")

## Assign Scope to Both Tables ----
cat("\nAssigning scope values...\n")

# Create lookup tables for scope assignment
iucn_scope_lookup <- merge(
  iucn_l2_keywords[, .(keyword, iucn_l2)],
  management_domain_threat_table[, .(iucn_l2, scope)],
  by = "iucn_l2",
  all.x = TRUE
)
gov_scope_lookup <- governance_keywords[, .(keyword, scope)]
salmon_scope_lookup <- salmon_scope_keywords[, .(keyword, scope)]

# Assign scope to both paragraph and heading label tables
for (tbl in list(paragraph_label_table, heading_label_table)) {
  tbl[, scope := NA_character_]
  tbl[label_type == "IUCN", scope := iucn_scope_lookup[.SD, on = "keyword"]$scope]
  tbl[label_type == "Management Domain", scope := gov_scope_lookup[.SD, on = "keyword"]$scope]
  tbl[label_type == "Salmon Scope", scope := salmon_scope_lookup[.SD, on = "keyword"]$scope]
}

cat("✓ Scope assignment complete\n")

## Duplicate IUCN Rows to Management Domain ----
# This creates Management Domain labels from IUCN threats based on the 
# management_domain_threat_table mapping. This allows IUCN threats to be 
# queried through their corresponding management domains.
cat("\nCreating Management Domain labels from IUCN threats...\n")

duplicate_iucn <- function(label_table) {
  iucn_rows <- label_table[label_type == "IUCN" & !is.na(label_value)]
  iucn_with_domain <- merge(
    iucn_rows,
    management_domain_threat_table[, .(iucn_l2, management_domain)],
    by.x = "label_value", by.y = "iucn_l2",
    all.x = TRUE
  )
  iucn_with_domain <- iucn_with_domain[!is.na(management_domain)]
  duplicated_rows <- iucn_with_domain[, .(
    paragraph_id,
    label_type = "Management Domain",
    keyword,
    label_value = management_domain,
    scope,
    label_id = NA_integer_
  )]
  return(duplicated_rows)
}

paragraph_label_table <- rbind(paragraph_label_table, duplicate_iucn(paragraph_label_table), fill = TRUE)
heading_label_table <- rbind(heading_label_table, duplicate_iucn(heading_label_table), fill = TRUE)

cat("✓ IUCN duplication complete\n")

## Combine and Finalize ----
cat("\nCombining and deduplicating labels...\n")

# Combine paragraph and heading labels
paragraph_label_table <- rbind(paragraph_label_table, heading_label_table, fill = TRUE)

# Remove duplicate labels (same paragraph_id, label_type, keyword, label_value)
paragraph_label_table <- unique(paragraph_label_table)

# Assign unique label_id to each row
paragraph_label_table[, label_id := .I]

# Reorder columns for clarity
setcolorder(paragraph_label_table, c("paragraph_id", "label_type", "keyword", "label_value", "scope", "label_id"))

cat("✓ Finalization complete\n")

## Validate Scope Assignments ----
cat("\nValidating scope assignments...\n")

# Check for missing scope values across all relevant label types
unmatched_iucn <- paragraph_label_table[label_type == "IUCN" & is.na(scope)]
if (nrow(unmatched_iucn) > 0) {
  warning(sprintf("⚠️  %d IUCN labels have no scope assigned", nrow(unmatched_iucn)))
  cat(sprintf("   Unique keywords affected: %d\n", uniqueN(unmatched_iucn$keyword)))
}

unmatched_domain <- paragraph_label_table[label_type == "Management Domain" & is.na(scope)]
if (nrow(unmatched_domain) > 0) {
  warning(sprintf("⚠️  %d Management Domain labels have no scope assigned", nrow(unmatched_domain)))
  cat(sprintf("   Unique keywords affected: %d\n", uniqueN(unmatched_domain$keyword)))
}

unmatched_salmon <- paragraph_label_table[label_type == "Salmon Scope" & is.na(scope)]
if (nrow(unmatched_salmon) > 0) {
  warning(sprintf("⚠️  %d Salmon Scope labels have no scope assigned", nrow(unmatched_salmon)))
  cat(sprintf("   Unique keywords affected: %d\n", uniqueN(unmatched_salmon$keyword)))
}

# Summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")
cat(sprintf("Total labels created: %d\n", nrow(paragraph_label_table)))
cat(sprintf("Unique paragraphs labeled: %d\n", uniqueN(paragraph_label_table$paragraph_id)))
cat(sprintf("Labels by type:\n"))
label_counts <- paragraph_label_table[, .N, by = label_type][order(-N)]
for (i in seq_len(nrow(label_counts))) {
  cat(sprintf("  - %s: %d\n", label_counts$label_type[i], label_counts$N[i]))
}

## Save to SQLite ----
cat("\nSaving to database...\n")
dbWriteTable(conn, "paragraph_label_table", paragraph_label_table, overwrite = TRUE)
dbDisconnect(conn)

## Notify Completion ----
cat("\n✅ Labeling complete. Table saved to SQLite.\n")
beep(sound = 1)