################################################################################
# Title: Database Provision Label Table
# Authors: Joe Enns, Cory Lagasse, Max Elinson
# Date Created: 2025-08-07
# Purpose / Description:
#   This script processes legislative provisions stored in a SQLite database,
#   matches keywords from various categories, and creates a labeled provision table.
# Dependencies: DBI, RSQLite, data.table, here, stringr, quanteda
# Execution: Run in RStudio or via Rscript; ensure working directory is project root
# Inputs:
#   - A SQLite database file named "legislation.db" in the "output" directory,
#     containing a table "LegislationParagraphs" with legislative provisions.
#   - CSV files containing keyword tables:
#       - "managament_domain_threat_table.csv"
#       - "clause_type_keywords.csv"
#       - "iucn_l2_keywords.csv"
#       - "salmon_scope_keywords.csv"
#       - "governance_keywords.csv"
# Outputs:
#   - A SQLite database file named "legislation.db" in the "output" directory,
#     containing a new table "provision_label_table" with labeled provisions.
# Notes:
#  - The script uses the `here` package to manage file paths dynamically.
#  - It cleans and preprocesses the text of legislative provisions.
#  - The script uses the `quanteda` package for text tokenization and phrase compounding.
#  - It matches keywords from various categories to label the provisions.
################################################################################

## Set Working Directory ----
library(here)

## Load Libraries ----
library(data.table)
library(RSQLite)
library(stringr)
library(quanteda)

## Connect to SQLite and load provision_table ----
db_path <- file.path(here("output"), "legislation.db")
conn <- dbConnect(SQLite(), dbname = db_path)

provision_table <- as.data.table(dbReadTable(conn, "LegislationParagraphs"))

## Load Keyword Tables ----
management_domain_threat_table <- fread(here("managament_domain_threat_table.csv"))
clause_type_keywords <- fread(here("clause_type_keywords.csv"), colClasses = c("keyword" = "character", "clause_type" = "character"))
iucn_l2_keywords <- fread(here("iucn_l2_keywords.csv"), colClasses = c("keyword" = "character", "iucn_l2" = "character"))
salmon_scope_keywords <- fread(here("salmon_scope_keywords.csv"), colClasses = c("keyword" = "character", "scope" = "character"))
governance_keywords <- fread(here("governance_keywords.csv"), colClasses = c("keyword" = "character", "management_domain" = "character", "scope" = "character"))

## Save Keyword Tables to Database ----
dbWriteTable(conn, "management_domain_threat_table", management_domain_threat_table, overwrite = TRUE)
dbWriteTable(conn, "clause_type_keywords", clause_type_keywords, overwrite = TRUE)
dbWriteTable(conn, "iucn_l2_keywords", iucn_l2_keywords, overwrite = TRUE)
dbWriteTable(conn, "salmon_scope_keywords", salmon_scope_keywords, overwrite = TRUE)
dbWriteTable(conn, "governance_keywords", governance_keywords, overwrite = TRUE)

## Clean and preprocess Paragraph text ----
cleaned_paragraphs <- str_squish(tolower(str_replace_all(provision_table$Paragraph, "[[:punct:]]", " ")))

## Tokenize and compound phrases ----
ct_phrases <- phrase(tolower(clause_type_keywords$keyword))
iucn_phrases <- phrase(tolower(iucn_l2_keywords$keyword))
scope_phrases <- phrase(tolower(salmon_scope_keywords$keyword))
gov_phrases <- phrase(tolower(governance_keywords$keyword))

corpus_obj <- corpus(data.frame(text = cleaned_paragraphs))
tokens_obj <- tokens(corpus_obj, remove_punct = TRUE)
tokens_comp <- tokens_compound(tokens_obj, pattern = iucn_phrases)
tokens_comp <- tokens_compound(tokens_comp, pattern = ct_phrases)
tokens_comp <- tokens_compound(tokens_comp, pattern = scope_phrases)
tokens_comp <- tokens_compound(tokens_comp, pattern = gov_phrases)

token_list <- lapply(as.list(tokens_comp), as.character)

## Match IUCN keywords ----
match_labels_iucn <- function(tokens, provision_id, cleaned_paragraph) {
  matches <- list()
  
  for (kw in iucn_l2_keywords$keyword) {
    if (tolower(kw) %in% gsub("_", " ", tokens)) {
      row_iucn <- iucn_l2_keywords[keyword == kw]
      
      clause_type <- NA_character_
      clause_type_keyword <- NA_character_
      for (ct_kw in clause_type_keywords$keyword) {
        if (tolower(ct_kw) %in% gsub("_", " ", tokens)) {
          row_ct <- clause_type_keywords[keyword == ct_kw]
          clause_type <- row_ct$clause_type
          clause_type_keyword <- ct_kw
          break
        }
      }
      
      scope_val <- NA_character_
      for (sc_kw in salmon_scope_keywords$keyword) {
        if (tolower(sc_kw) %in% gsub("_", " ", tokens)) {
          row_scope <- salmon_scope_keywords[keyword == sc_kw]
          scope_val <- row_scope$scope
          break
        }
      }
      
      matches <- append(matches, list(data.table(
        provision_id = provision_id,
        cleaned_paragraph = cleaned_paragraph,
        scope = scope_val,
        iucn_l2 = row_iucn$iucn_l2,
        iucn_keyword = kw,
        clause_type = clause_type,
        clause_type_keyword = clause_type_keyword
      )))
    }
  }
  
  return(rbindlist(matches))
}

## Apply IUCN matching ----
provision_label_table <- rbindlist(
  mapply(match_labels_iucn,
         token_list,
         provision_table$provision_id,
         cleaned_paragraphs,
         SIMPLIFY = FALSE)
)

## Add label_id ----
provision_label_table[, label_id := .I]

## Fill missing scope and assign management_domain ----
management_domain_threat_table_unique <- management_domain_threat_table[
  , .SD[1], by = iucn_l2
]

provision_label_table <- merge(
  provision_label_table,
  management_domain_threat_table_unique[, .(iucn_l2, fallback_scope = scope, management_domain)],
  by = "iucn_l2",
  all.x = TRUE
)

provision_label_table[is.na(scope), scope := fallback_scope]
provision_label_table[, fallback_scope := NULL]

## Identify unmatched provision_ids ----
matched_ids <- unique(provision_label_table$provision_id)
unmatched_indices <- which(!(provision_table$provision_id %in% matched_ids))

## Match governance keywords for unmatched rows ----
match_labels_governance <- function(tokens, provision_id, cleaned_paragraph) {
  matches <- list()
  
  for (kw in governance_keywords$keyword) {
    if (tolower(kw) %in% gsub("_", " ", tokens)) {
      row_gov <- governance_keywords[keyword == kw]
      
      clause_type <- NA_character_
      clause_type_keyword <- NA_character_
      for (ct_kw in clause_type_keywords$keyword) {
        if (tolower(ct_kw) %in% gsub("_", " ", tokens)) {
          row_ct <- clause_type_keywords[keyword == ct_kw]
          clause_type <- row_ct$clause_type
          clause_type_keyword <- ct_kw
          break
        }
      }
      
      matches <- append(matches, list(data.table(
        provision_id = provision_id,
        cleaned_paragraph = cleaned_paragraph,
        scope = row_gov$scope,
        management_domain = row_gov$management_domain,
        iucn_l2 = NA_character_,
        iucn_keyword = NA_character_,
        clause_type = clause_type,
        clause_type_keyword = clause_type_keyword
      )))
    }
  }
  
  return(rbindlist(matches))
}

## Apply governance matching ----
governance_matches <- rbindlist(
  mapply(match_labels_governance,
         token_list[unmatched_indices],
         provision_table$provision_id[unmatched_indices],
         cleaned_paragraphs[unmatched_indices],
         SIMPLIFY = FALSE)
)

## Add governance matches to label table ----
if (nrow(governance_matches) > 0) {
  governance_matches[, label_id := max(provision_label_table$label_id, 0) + seq_len(.N)]
  provision_label_table <- rbind(provision_label_table, governance_matches, fill = TRUE)
}

## Reorder columns ----
setcolorder(provision_label_table, c(
  "provision_id",
  "cleaned_paragraph",
  "scope",
  "management_domain",
  "iucn_l2",
  "iucn_keyword",
  "clause_type",
  "clause_type_keyword",
  "label_id"
))

## Save to SQLite Database ----
dbWriteTable(conn, "provision_label_table", provision_label_table, overwrite = TRUE)
dbDisconnect(conn)