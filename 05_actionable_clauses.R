################################################################################
# Title: Actionable Clauses Extraction
# Authors: Joe Enns, Cory Lagasse
# Date Created: 2025-01-XX
# Purpose / Description:
#   Extracts actionable clauses from legislation by filtering paragraphs that:
#   1. Are from Acts (not regulations)
#   2. Have Management Domain labels
#   3. Have specific Clause Type labels (Designation, Instruction, etc.)
#   4. Contain "responsible official" keywords (from actionable_clause_keywords)
#   5. Contain "discretionary language" keywords (from actionable_clause_keywords)
#   Then extracts implement types and creates output with metadata.
# Dependencies: DBI, RSQLite, data.table, here, stringr, beepr
# Outputs:
#   "actionable_clauses.csv" in the output directory
#   "actionable_clauses" table in the SQLite database
################################################################################

## Load Libraries ----
library(here)
library(DBI)
library(RSQLite)
library(data.table)
library(stringr)
library(beepr)

cat("=====================================\n")
cat("Actionable Clauses Extraction\n")
cat("=====================================\n\n")

## ============================================================================
## TEXT CLEANING FUNCTION: Fix encoding issues (mojibake)
## ============================================================================

clean_encoding <- function(text) {
  if (is.na(text) || text == "") return(text)
  
  original_text <- text
  result <- iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  
  if (is.na(result)) {
    result <- iconv(original_text, from = "latin1", to = "ASCII//TRANSLIT", sub = "")
  }
  
  if (is.na(result)) {
    result <- gsub("[^\x20-\x7E]", "", original_text, perl = TRUE)
  }
  
  result <- gsub("[^\x20-\x7E]", "", result, perl = TRUE)
  result <- trimws(result)
  
  return(result)
}

## Connect to Database ----
db_path <- file.path(here("output"), "legislation.db")
if (!file.exists(db_path)) {
  stop("Database file not found at: ", db_path)
}

conn <- dbConnect(SQLite(), dbname = db_path)

## Load Tables ----
cat("Loading data from database...\n")
paragraph_table <- as.data.table(dbReadTable(conn, "LegislationParagraphs"))
legislation_table <- as.data.table(dbReadTable(conn, "LegislationMetadata"))
paragraph_label_table <- as.data.table(dbReadTable(conn, "paragraph_label_table"))
actionable_keywords <- as.data.table(dbReadTable(conn, "actionable_clause_keywords"))

## Keep connection open for later write operation

cat(sprintf("  - Loaded %d paragraphs\n", nrow(paragraph_table)))
cat(sprintf("  - Loaded %d legislation records\n", nrow(legislation_table)))
cat(sprintf("  - Loaded %d paragraph labels\n", nrow(paragraph_label_table)))
cat(sprintf("  - Loaded %d actionable clause keywords\n", nrow(actionable_keywords)))

## ============================================================================
## DEFINE IMPLEMENT PATTERNS
## ============================================================================

cat("\nDefining implement patterns...\n")

implement_patterns <- list(
  # --- Regulatory Instruments ---
  regulation = list(
    pattern = "\\b(regulation|regulations|regulatory)\\b"
  ),
  order = list(
    pattern = "\\b(order|orders)\\b(?!\\s+in\\s+council)"
  ),
  order_in_council = list(
    pattern = "\\b(order in council|orders in council|lieutenant governor in council|governor in council)\\b"
  ),
  bylaw = list(
    pattern = "\\b(bylaw|by-law|bylaws|by-laws)\\b"
  ),
  
  # --- Planning Documents ---
  plan = list(
    pattern = "\\b(plan|plans|planning)\\b(?!\\s+of)"
  ),
  strategy = list(
    pattern = "\\b(strateg(y|ies))\\b"
  ),
  program = list(
    pattern = "\\b(program|programme|programs|programmes)\\b"
  ),
  policy = list(
    pattern = "\\b(polic(y|ies))\\b"
  ),
  framework = list(
    pattern = "\\b(framework|frameworks)\\b"
  ),
  guideline = list(
    pattern = "\\b(guideline|guidelines)\\b"
  ),
  standard = list(
    pattern = "\\b(standard|standards)\\b"
  ),
  code = list(
    pattern = "\\b(code|codes)\\b(?!\\s+of)"
  ),
  
  # --- Spatial Designations ---
  designation = list(
    pattern = "\\b(designat(e|ion|ions|ed|ing)|designated area|designated areas)\\b"
  ),
  reserve = list(
    pattern = "\\b(reserve|reserves)\\b"
  ),
  sanctuary = list(
    pattern = "\\b(sanctuar(y|ies))\\b"
  ),
  park = list(
    pattern = "\\b(park|parks)\\b"
  ),
  protected_area = list(
    pattern = "\\b(protected area|conservation area|management area|special area)\\b"
  ),
  
  # --- Authorization Mechanisms ---
  agreement = list(
    pattern = "\\b(agreement|agreements)\\b"
  ),
  permit = list(
    pattern = "\\b(permit|permits)\\b"
  ),
  licence = list(
    pattern = "\\b(licen[cs]e|licen[cs]es)\\b"
  ),
  authorization = list(
    pattern = "\\b(authori[sz]ation|authori[sz]ations)\\b"
  ),
  approval = list(
    pattern = "\\b(approval|approvals)\\b"
  ),
  certificate = list(
    pattern = "\\b(certificate|certificates)\\b"
  ),
  exemption = list(
    pattern = "\\b(exemption|exemptions|exempt|exempted|exempting)\\b"
  ),
  notice = list(
    pattern = "\\b(notice|notices)\\b"
  ),
  
  # --- Reporting Requirements ---
  report = list(
    pattern = "\\b(report|reports|reporting)\\b"
  ),
  assessment = list(
    pattern = "\\b(assessment|assessments)\\b"
  ),
  review = list(
    pattern = "\\b(review|reviews)\\b"
  ),
  study = list(
    pattern = "\\b(stud(y|ies))\\b"
  )
)

## ============================================================================
## STEP 1: FILTER BY ACTS ONLY
## ============================================================================

cat("\n--- STEP 1: Filter by Acts only ---\n")

acts_only <- legislation_table[legislation_type == "Act"]
act_paragraphs <- paragraph_table[legislation_id %in% acts_only$legislation_id]

cat(sprintf("  - Acts: %d\n", nrow(acts_only)))
cat(sprintf("  - Paragraphs from Acts: %d\n", nrow(act_paragraphs)))

## ============================================================================
## STEP 2: FILTER BY MANAGEMENT DOMAIN
## ============================================================================

cat("\n--- STEP 2: Filter by Management Domain labels ---\n")

management_domain_paragraph_ids <- unique(
  paragraph_label_table[label_type == "Management Domain", paragraph_id]
)

cat(sprintf("  - Paragraphs with Management Domain labels: %d\n", 
            length(management_domain_paragraph_ids)))

## ============================================================================
## STEP 3: FILTER BY CLAUSE TYPE
## ============================================================================

cat("\n--- STEP 3: Filter by Clause Type labels ---\n")

included_clause_types <- c(
  "Designation",
  "Instruction",
  "Licence, Permitting, & Exemptions",
  "Authorization & Mandate"
)

cat(sprintf("  - Including Clause Types: %s\n", 
            paste(included_clause_types, collapse = ", ")))

clause_type_paragraph_ids <- unique(
  paragraph_label_table[
    label_type == "Clause Type" & label_value %in% included_clause_types, 
    paragraph_id
  ]
)

cat(sprintf("  - Paragraphs with specified Clause Type labels: %d\n", 
            length(clause_type_paragraph_ids)))

## ============================================================================
## STEP 4: COMBINE FILTERS (INTERSECTION)
## ============================================================================

cat("\n--- STEP 4: Combine filters (intersection) ---\n")

filtered_paragraph_ids <- intersect(
  act_paragraphs$paragraph_id,
  intersect(management_domain_paragraph_ids, clause_type_paragraph_ids)
)

cat(sprintf("  - Paragraphs after combining filters: %d\n", 
            length(filtered_paragraph_ids)))

filtered_paragraphs <- act_paragraphs[paragraph_id %in% filtered_paragraph_ids]

## Exclude paragraphs where Heading contains "definition" ----
cat("  - Excluding paragraphs with 'definition' in Heading...\n")
rows_before <- nrow(filtered_paragraphs)
filtered_paragraphs <- filtered_paragraphs[
  is.na(Heading) | !grepl("definition", Heading, ignore.case = TRUE)
]
cat(sprintf("  - Removed %d paragraphs with 'definition' in Heading\n", 
            rows_before - nrow(filtered_paragraphs)))
cat(sprintf("  - Paragraphs remaining: %d\n", nrow(filtered_paragraphs)))

## ============================================================================
## STEP 5: FILTER BY RESPONSIBLE OFFICIAL KEYWORDS
## ============================================================================

cat("\n--- STEP 5: Filter by 'responsible official' keywords ---\n")

official_keywords <- actionable_keywords[
  actionable_clause_category == "responsible official", 
  keyword
]

cat(sprintf("  - Responsible official keywords: %d\n", length(official_keywords)))

official_pattern <- paste0("\\b(", paste(official_keywords, collapse = "|"), ")\\b")

filtered_paragraphs[, has_official := grepl(official_pattern, Paragraph, ignore.case = TRUE)]

paragraphs_with_official <- filtered_paragraphs[has_official == TRUE]

cat(sprintf("  - Paragraphs with responsible official keywords: %d\n", 
            nrow(paragraphs_with_official)))

## ============================================================================
## STEP 6: FILTER BY DISCRETIONARY LANGUAGE KEYWORDS
## ============================================================================

cat("\n--- STEP 6: Filter by 'discretionary language' keywords ---\n")

discretionary_keywords <- actionable_keywords[
  actionable_clause_category == "discretionary language", 
  keyword
]

cat(sprintf("  - Discretionary language keywords: %d\n", length(discretionary_keywords)))
cat(sprintf("    Keywords: %s\n", paste(discretionary_keywords, collapse = ", ")))

discretionary_pattern <- paste0("\\b(", paste(discretionary_keywords, collapse = "|"), ")\\b")

paragraphs_with_official[, has_discretionary := grepl(discretionary_pattern, Paragraph, ignore.case = TRUE)]

actionable_paragraphs <- paragraphs_with_official[has_discretionary == TRUE]

cat(sprintf("  - Paragraphs with BOTH official AND discretionary keywords: %d\n", 
            nrow(actionable_paragraphs)))

## ============================================================================
## STEP 6b: FILTER BY WORD ORDER (Official BEFORE Discretionary Verb, within 5 words)
## ============================================================================

cat("\n--- STEP 6b: Filter by word order (official before discretionary verb, within 5 words) ---\n")

## Function: Count words between two character positions in text
count_words_between <- function(text, start_pos, end_pos) {
  if (start_pos >= end_pos) return(Inf)
  
  # Extract substring between the two positions
  between_text <- substr(text, start_pos, end_pos - 1)
  
  # Count words by splitting on whitespace
  words <- unlist(strsplit(trimws(between_text), "\\s+"))
  
  # Return word count (subtract 1 because we don't count the keyword itself)
  return(length(words) - 1)
}

## Function: Check if any official keyword appears before any discretionary keyword within 5 words
check_official_before_discretionary <- function(paragraph_text, official_kws, discretionary_kws, max_words = 5) {
  if (is.na(paragraph_text) || paragraph_text == "") return(FALSE)
  
  text_lower <- tolower(paragraph_text)
  
  # Find positions and lengths of all official keywords
  official_matches <- list()
  for (kw in official_kws) {
    kw_lower <- tolower(kw)
    matches <- gregexpr(paste0("\\b", kw_lower, "\\b"), text_lower, perl = TRUE)[[1]]
    if (matches[1] != -1) {
      for (pos in matches) {
        official_matches <- append(official_matches, list(list(
          pos = pos,
          end_pos = pos + nchar(kw_lower)
        )))
      }
    }
  }
  
  # Find positions of all discretionary keywords
  discretionary_positions <- integer()
  for (kw in discretionary_kws) {
    matches <- gregexpr(paste0("\\b", tolower(kw), "\\b"), text_lower, perl = TRUE)[[1]]
    if (matches[1] != -1) {
      discretionary_positions <- c(discretionary_positions, matches)
    }
  }
  
  # Check if no matches found
  if (length(official_matches) == 0 || length(discretionary_positions) == 0) {
    return(FALSE)
  }
  
  # Check if any official keyword appears before any discretionary keyword within max_words
  for (official in official_matches) {
    for (disc_pos in discretionary_positions) {
      # Check if discretionary comes after official
      if (disc_pos > official$end_pos) {
        # Count words between end of official and start of discretionary
        words_between <- count_words_between(text_lower, official$end_pos, disc_pos)
        
        if (words_between <= max_words) {
          return(TRUE)
        }
      }
    }
  }
  
  return(FALSE)
}

cat("  - Checking word order for each paragraph...\n")

actionable_paragraphs[, official_before_verb := sapply(
  Paragraph,
  check_official_before_discretionary,
  official_kws = official_keywords,
  discretionary_kws = discretionary_keywords
)]

rows_before_order_filter <- nrow(actionable_paragraphs)
actionable_paragraphs <- actionable_paragraphs[official_before_verb == TRUE]

cat(sprintf("  - Removed %d paragraphs where verb not within 5 words after official\n", 
            rows_before_order_filter - nrow(actionable_paragraphs)))
cat(sprintf("  - Paragraphs remaining (official before verb, within 5 words): %d\n", 
            nrow(actionable_paragraphs)))

## Validate we have data to process ----
if (nrow(actionable_paragraphs) == 0) {
  stop("No paragraphs remaining after filtering. Check your keyword filters.")
}

## ============================================================================
## STEP 7: EXTRACT IMPLEMENT TYPES, OFFICIALS, AND DISCRETION
## ============================================================================

cat("\n--- STEP 7: Extracting implement types, officials, and discretion ---\n")

## Function: Extract implement types ----
extract_implement_types <- function(paragraph_text) {
  if (is.na(paragraph_text) || paragraph_text == "") return(NA_character_)
  
  text_lower <- tolower(paragraph_text)
  implements_found <- character()
  
  for (impl_name in names(implement_patterns)) {
    impl_info <- implement_patterns[[impl_name]]
    pattern <- impl_info$pattern
    
    if (grepl(pattern, text_lower, perl = TRUE)) {
      implements_found <- c(implements_found, impl_name)
    }
  }
  
  if (length(implements_found) > 0) {
    return(paste(implements_found, collapse = "; "))
  } else {
    return(NA_character_)
  }
}

## Function: Extract matched official keywords ----
extract_officials <- function(paragraph_text, official_kws) {
  if (is.na(paragraph_text) || paragraph_text == "") return(NA_character_)
  
  officials_found <- character()
  
  for (kw in official_kws) {
    pattern <- paste0("\\b", kw, "\\b")
    if (grepl(pattern, paragraph_text, ignore.case = TRUE)) {
      officials_found <- c(officials_found, kw)
    }
  }
  
  if (length(officials_found) > 0) {
    return(paste(unique(officials_found), collapse = "; "))
  } else {
    return(NA_character_)
  }
}

## Function: Extract discretion type (mandatory vs discretionary) ----
extract_discretion_type <- function(paragraph_text, disc_keywords_dt) {
  if (is.na(paragraph_text) || paragraph_text == "") return(NA_character_)
  
  text_lower <- tolower(paragraph_text)
  discretions_found <- character()
  
  for (i in seq_len(nrow(disc_keywords_dt))) {
    kw <- disc_keywords_dt$keyword[i]
    dtype <- disc_keywords_dt$type[i]
    pattern <- paste0("\\b", tolower(kw), "\\b")
    
    if (grepl(pattern, text_lower, perl = TRUE)) {
      discretions_found <- c(discretions_found, dtype)
    }
  }
  
  if (length(discretions_found) > 0) {
    return(paste(unique(discretions_found), collapse = "; "))
  } else {
    return(NA_character_)
  }
}

# Get discretionary keywords with type
discretionary_keywords_dt <- actionable_keywords[
  actionable_clause_category == "discretionary language",
  .(keyword, type)
]

cat("  - Extracting implement types...\n")
actionable_paragraphs[, implement_type := sapply(Paragraph, extract_implement_types)]

cat("  - Extracting responsible officials...\n")
actionable_paragraphs[, responsible_official := sapply(
  Paragraph, 
  extract_officials, 
  official_kws = official_keywords
)]

cat("  - Extracting discretion types...\n")
actionable_paragraphs[, discretion_type := sapply(
  Paragraph, 
  extract_discretion_type, 
  disc_keywords_dt = discretionary_keywords_dt
)]

## ============================================================================
## STEP 8: MERGE WITH METADATA AND PREPARE OUTPUT
## ============================================================================

cat("\n--- STEP 8: Merging with legislation metadata ---\n")

## Merge with legislation metadata ----
actionable_output <- merge(
  actionable_paragraphs[, .(paragraph_id, legislation_id, Section, Heading, Paragraph, 
                            implement_type, responsible_official, discretion_type)],
  acts_only[, .(legislation_id, act_name, jurisdiction)],
  by = "legislation_id",
  all.x = TRUE
)

## Remove unwanted columns from output ----
actionable_output[, c("paragraph_id", "legislation_id") := NULL]

## Reorder columns ----
setcolorder(actionable_output, c(
  "act_name", "jurisdiction", "Section", "Heading", 
  "implement_type", "responsible_official", "discretion_type", 
  "Paragraph"
))

## Clean encoding issues in Heading and Paragraph columns ----
cat("  - Cleaning text encoding issues...\n")
actionable_output[, Heading := sapply(Heading, clean_encoding)]
actionable_output[, Paragraph := sapply(Paragraph, clean_encoding)]

## Sort ----
actionable_output <- actionable_output[order(act_name, Section)]

cat(sprintf("  - Final output rows: %d\n", nrow(actionable_output)))

## ============================================================================
## STEP 9: CREATE SUMMARIES
## ============================================================================

cat("\n--- STEP 9: Creating summary statistics ---\n")

## Summary by implement type ----
summary_by_implement <- actionable_output[!is.na(implement_type), .N, by = implement_type][order(-N)]
setnames(summary_by_implement, c("Implement Type", "Count"))

## Summary by official ----
summary_by_official <- actionable_output[!is.na(responsible_official), .N, by = responsible_official][order(-N)]
setnames(summary_by_official, c("Responsible Official", "Count"))

## Summary by discretion type ----
summary_by_discretion <- actionable_output[!is.na(discretion_type), .N, by = discretion_type][order(-N)]
setnames(summary_by_discretion, c("Discretion Type", "Count"))

## Summary by act ----
summary_by_act <- actionable_output[, .N, by = .(act_name, jurisdiction)][order(-N)]
setnames(summary_by_act, c("Act Name", "Jurisdiction", "Count"))

## ============================================================================
## STEP 10: EXPORT TO CSV AND DATABASE
## ============================================================================

cat("\n--- STEP 10: Exporting results ---\n")

## --- Export to CSV ---
output_file <- file.path(here("output"), "actionable_clauses.csv")
data.table::fwrite(actionable_output, output_file, sep = ",", na = "", quote = TRUE)
cat(sprintf("  - CSV file saved to: %s\n", output_file))

## --- Export to SQLite Database ---
cat("  - Writing to SQLite database...\n")
dbWriteTable(conn, "actionable_clauses", actionable_output, overwrite = TRUE)
cat(sprintf("  - Table 'actionable_clauses' saved to database\n"))
cat(sprintf("  - Rows written: %d\n", nrow(actionable_output)))

## Disconnect from database ----
dbDisconnect(conn)
cat("  - Database connection closed.\n")

## ============================================================================
## SUMMARY
## ============================================================================

cat("\n=====================================\n")
cat("SUMMARY\n")
cat("=====================================\n\n")

cat("FILTERING PIPELINE:\n")
cat(sprintf("  1. Total paragraphs in database:     %d\n", nrow(paragraph_table)))
cat(sprintf("  2. Paragraphs from Acts:             %d\n", nrow(act_paragraphs)))
cat(sprintf("  3. With Management Domain labels:    %d\n", length(management_domain_paragraph_ids)))
cat(sprintf("  4. With specified Clause Types:      %d\n", length(clause_type_paragraph_ids)))
cat(sprintf("  5. Combined filter (intersection):   %d\n", length(filtered_paragraph_ids)))
cat(sprintf("  6. Excluding 'definition' headings:  (see above)\n"))
cat(sprintf("  7. With responsible official:        %d\n", nrow(paragraphs_with_official)))
cat(sprintf("  8. With discretionary language:      %d\n", rows_before_order_filter))
cat(sprintf("  9. Official before verb (within 5 words): %d\n", nrow(actionable_paragraphs)))

cat("\nINCLUDED CLAUSE TYPES:\n")
for (ct in included_clause_types) {
  cat(sprintf("  - %s\n", ct))
}

cat("\nTop 10 Implement Types:\n")
print(head(summary_by_implement, 10))

cat("\nTop 10 Responsible Officials:\n")
print(head(summary_by_official, 10))

cat("\nDiscretion Type Summary:\n")
print(summary_by_discretion)

cat("\nTop 10 Acts by Actionable Clause Count:\n")
print(head(summary_by_act, 10))

cat("\n=====================================\n")
cat("Analysis complete!\n")
cat(sprintf("Output saved to: %s\n", output_file))
cat("=====================================\n")

## Notify Completion ----
beep(sound = 1)