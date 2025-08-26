################################################################################
# Title: Simplified Paragraph Label Table
# Authors: Joe Enns, Cory Lagasse, Max Elinson
# Date Created: 2025-08-07
# Purpose / Description:
#   This script processes legislative paragraphs stored in a SQLite database,
#   matches keywords from four categories, and creates a simplified labeled table.
# Dependencies: DBI, RSQLite, data.table, here, stringr, quanteda
################################################################################

## Set Working Directory ----
library(here)

## Load Libraries ----
library(data.table)
library(RSQLite)
library(stringr)
library(quanteda)

## Connect to SQLite and load paragraph_table ----
db_path <- file.path(here("output"), "legislation.db")
conn <- dbConnect(SQLite(), dbname = db_path)

paragraph_table <- as.data.table(dbReadTable(conn, "LegislationParagraphs"))

## Load Keyword Tables ----
clause_type_keywords <- fread(here("clause_type_keywords.csv"), colClasses = "character")
iucn_l2_keywords <- fread(here("iucn_l2_keywords.csv"), colClasses = "character")
salmon_scope_keywords <- fread(here("salmon_scope_keywords.csv"), colClasses = "character")
governance_keywords <- fread(here("governance_keywords.csv"), colClasses = "character")
mgmt_d_iucn <- fread(here("management_domain_threat_table.csv"), colClasses = "character")

## Save Keyword Tables to SQLite ----
dbWriteTable(conn, "clause_type_keywords", clause_type_keywords, overwrite = TRUE)
dbWriteTable(conn, "iucn_l2_keywords", iucn_l2_keywords, overwrite = TRUE)
dbWriteTable(conn, "salmon_scope_keywords", salmon_scope_keywords, overwrite = TRUE)
dbWriteTable(conn, "governance_keywords", governance_keywords, overwrite = TRUE)
dbWriteTable(conn, "management_domain_threat_table", mgmt_d_iucn, overwrite = TRUE)

## Clean and preprocess Paragraph text ----
cleaned_paragraphs <- str_squish(tolower(str_replace_all(paragraph_table$Paragraph, "[[:punct:]]", " ")))

## Tokenize and compound phrases ----
corpus_obj <- corpus(data.frame(text = cleaned_paragraphs))
tokens_obj <- tokens(corpus_obj, remove_punct = TRUE)

tokens_obj <- tokens_compound(tokens_obj, pattern = phrase(tolower(iucn_l2_keywords$keyword)))
tokens_obj <- tokens_compound(tokens_obj, pattern = phrase(tolower(governance_keywords$keyword)))
tokens_obj <- tokens_compound(tokens_obj, pattern = phrase(tolower(salmon_scope_keywords$keyword)))
tokens_obj <- tokens_compound(tokens_obj, pattern = phrase(tolower(clause_type_keywords$keyword)))

token_list <- lapply(as.list(tokens_obj), as.character)

## Matching Function ----
match_keywords <- function(tokens, paragraph_id, keyword_list, label_type) {
  matches <- list()
  for (kw in keyword_list$keyword) {
    if (tolower(kw) %in% gsub("_", " ", tokens)) {
      matches <- append(matches, list(data.table(
        paragraph_id = paragraph_id,
        label_type = label_type,
        keyword = kw
      )))
    }
  }
  return(rbindlist(matches))
}

## Apply Matching Across All Categories ----
paragraph_label_table <- rbindlist(
  mapply(function(tokens, pid) {
    rbindlist(list(
      match_keywords(tokens, pid, iucn_l2_keywords, "IUCN"),
      match_keywords(tokens, pid, governance_keywords, "Governance"),
      match_keywords(tokens, pid, salmon_scope_keywords, "Salmon Scope"),
      match_keywords(tokens, pid, clause_type_keywords, "Clause Type")
    ), fill = TRUE)
  }, token_list, paragraph_table$paragraph_id, SIMPLIFY = FALSE)
)

## Assign Unique label_id ----
paragraph_label_table[, label_id := .I]

## Reorder Columns ----
setcolorder(paragraph_label_table, c("paragraph_id", "label_type", "keyword", "label_id"))

## Save to SQLite Database ----
dbWriteTable(conn, "paragraph_label_table", paragraph_label_table, overwrite = TRUE)
dbDisconnect(conn)
