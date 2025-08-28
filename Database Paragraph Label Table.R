################################################################################
# Title: Simplified Paragraph Label Table
# Authors: Joe Enns, Cory Lagasse, Max Elinson
# Date Created: 2025-08-07
# Purpose / Description:
#   Processes legislative paragraphs from SQLite, matches keywords across categories
#   from both paragraph and heading text, assigns scope and label values, and builds
#   a labeled table with deduplicated entries.
# Dependencies: DBI, RSQLite, data.table, here, stringr, quanteda, beepr
################################################################################

## Set Working Directory ----
library(here)

## Load Libraries ----
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

## Save Keyword Tables to SQLite ----
dbWriteTable(conn, "clause_type_keywords", clause_type_keywords, overwrite = TRUE)
dbWriteTable(conn, "iucn_l2_keywords", iucn_l2_keywords, overwrite = TRUE)
dbWriteTable(conn, "salmon_scope_keywords", salmon_scope_keywords, overwrite = TRUE)
dbWriteTable(conn, "governance_keywords", governance_keywords, overwrite = TRUE)
dbWriteTable(conn, "management_domain_threat_table", management_domain_threat_table, overwrite = TRUE)

## Matching Function ----
match_keywords <- function(tokens, paragraph_id, keyword_list, label_type, value_column = NULL) {
  matches <- list()
  for (kw in keyword_list$keyword) {
    if (tolower(kw) %in% gsub("_", " ", tokens)) {
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
cleaned_paragraphs <- str_squish(tolower(str_replace_all(paragraph_table$Paragraph, "[[:punct:]]", " ")))
corpus_obj <- corpus(data.frame(text = cleaned_paragraphs))
tokens_obj <- tokens(corpus_obj, remove_punct = TRUE)

tokens_obj <- tokens_compound(tokens_obj, pattern = phrase(tolower(iucn_l2_keywords$keyword)))
tokens_obj <- tokens_compound(tokens_obj, pattern = phrase(tolower(governance_keywords$keyword)))
tokens_obj <- tokens_compound(tokens_obj, pattern = phrase(tolower(salmon_scope_keywords$keyword)))
tokens_obj <- tokens_compound(tokens_obj, pattern = phrase(tolower(clause_type_keywords$keyword)))

token_list <- lapply(as.list(tokens_obj), as.character)

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

## Tokenize and Match Heading Text ----
cleaned_headings <- str_squish(tolower(str_replace_all(paragraph_table$Heading, "[[:punct:]]", " ")))
heading_corpus <- corpus(data.frame(text = cleaned_headings))
heading_tokens <- tokens(heading_corpus, remove_punct = TRUE)

heading_tokens <- tokens_compound(heading_tokens, pattern = phrase(tolower(iucn_l2_keywords$keyword)))
heading_tokens <- tokens_compound(heading_tokens, pattern = phrase(tolower(governance_keywords$keyword)))
heading_tokens <- tokens_compound(heading_tokens, pattern = phrase(tolower(salmon_scope_keywords$keyword)))
heading_tokens <- tokens_compound(heading_tokens, pattern = phrase(tolower(clause_type_keywords$keyword)))

heading_token_list <- lapply(as.list(heading_tokens), as.character)

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

## Assign Scope to Both Tables ----
iucn_scope_lookup <- merge(
  iucn_l2_keywords[, .(keyword, iucn_l2)],
  management_domain_threat_table[, .(iucn_l2, scope)],
  by = "iucn_l2",
  all.x = TRUE
)
gov_scope_lookup <- governance_keywords[, .(keyword, scope)]
salmon_scope_lookup <- salmon_scope_keywords[, .(keyword, scope)]

for (tbl in list(paragraph_label_table, heading_label_table)) {
  tbl[, scope := NA_character_]
  tbl[label_type == "IUCN", scope := iucn_scope_lookup[.SD, on = "keyword"]$scope]
  tbl[label_type == "Management Domain", scope := gov_scope_lookup[.SD, on = "keyword"]$scope]
  tbl[label_type == "Salmon Scope", scope := salmon_scope_lookup[.SD, on = "keyword"]$scope]
}

## Duplicate IUCN Rows to Management Domain ----
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

## Combine and Finalize ----
paragraph_label_table <- rbind(paragraph_label_table, heading_label_table, fill = TRUE)
paragraph_label_table <- unique(paragraph_label_table)
paragraph_label_table[, label_id := .I]
setcolorder(paragraph_label_table, c("paragraph_id", "label_type", "keyword", "label_value", "scope", "label_id"))

## Save to SQLite ----
dbWriteTable(conn, "paragraph_label_table", paragraph_label_table, overwrite = TRUE)
dbDisconnect(conn)

## Notify Completion ----
cat("✅ Labeling complete. Table saved to SQLite.\n")
beep(sound = 1)
