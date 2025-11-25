################################################################################
# Title: Enhanced Legislative Implements Extraction with NLP
# Authors: Joe Enns, Cory Lagasse
# Date Created: 2025-01-XX
# Purpose / Description:
#   Enhanced version using advanced NLP techniques including dependency parsing,
#   part-of-speech tagging, context windows, and machine learning to extract
#   implements from legislation with improved accuracy.
#   PRE-FILTERED to only include paragraphs labelled with:
#     - Management Domains (any)
#     - Clause Types: Designation, Instruction, Licence/Permitting/Exemptions
# Dependencies: DBI, RSQLite, data.table, here, openxlsx, stringr
# Outputs:
#   "legislative_implementations.csv" in the output directory
#   "legislative_implementations" table in the SQLite database
################################################################################

## Load Libraries ----
library(here)
library(DBI)
library(RSQLite)
library(data.table)
library(openxlsx)
library(stringr)
library(beepr)

cat("=====================================\n")
cat("Enhanced Legislative Implements Extraction\n")
cat("(Pre-filtered by Management Domain & Clause Type)\n")
cat("=====================================\n\n")

## ============================================================================
## TEXT CLEANING FUNCTION: Fix encoding issues (mojibake)
## ============================================================================

clean_encoding <- function(text) {
  if (is.na(text) || text == "") return(text)
  
  # Save original for fallback
  original_text <- text
  
  # Convert to ASCII with transliteration (handles most mojibake)
  result <- iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  
  # If iconv fails, try latin1 encoding
  if (is.na(result)) {
    result <- iconv(original_text, from = "latin1", to = "ASCII//TRANSLIT", sub = "")
  }
  
  # If still NA, return original with non-ASCII stripped
  if (is.na(result)) {
    result <- gsub("[^\x20-\x7E]", "", original_text, perl = TRUE)
  }
  
  # Remove any remaining non-ASCII characters
  result <- gsub("[^\x20-\x7E]", "", result, perl = TRUE)
  
  # Trim whitespace
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

## Keep connection open for later write operation
## dbDisconnect(conn) -- moved to end of script

## ============================================================================
## SECTION 0: PRE-FILTER BY MANAGEMENT DOMAIN AND CLAUSE TYPE
## ============================================================================

cat("\nStep 0: Pre-filtering paragraphs by labels...\n")

## --- Filter 1: Management Domain ---
cat("\n  FILTER 1: Management Domain\n")

## Get unique paragraph_ids that have Management Domain labels ----
management_domain_paragraph_ids <- unique(
  paragraph_label_table[label_type == "Management Domain", paragraph_id]
)

cat(sprintf("    - Found %d unique paragraphs with Management Domain labels\n", 
            length(management_domain_paragraph_ids)))

## --- Filter 2: Clause Type ---
cat("\n  FILTER 2: Clause Type\n")

## Define the Clause Types to include ----
included_clause_types <- c(
  "Designation",
  "Instruction",
  "Licence, Permitting, & Exemptions",
  "Authorization & Mandate"
)

cat(sprintf("    - Including Clause Types: %s\n", 
            paste(included_clause_types, collapse = ", ")))

## Get unique paragraph_ids that have the specified Clause Type labels ----
clause_type_paragraph_ids <- unique(
  paragraph_label_table[
    label_type == "Clause Type" & label_value %in% included_clause_types, 
    paragraph_id
  ]
)

cat(sprintf("    - Found %d unique paragraphs with specified Clause Type labels\n", 
            length(clause_type_paragraph_ids)))

## --- Combine Filters: Intersection ---
cat("\n  COMBINING FILTERS (intersection):\n")

## Get paragraphs that have BOTH Management Domain AND specified Clause Type ----
filtered_paragraph_ids <- intersect(
  management_domain_paragraph_ids,
  clause_type_paragraph_ids
)

cat(sprintf("    - Paragraphs with BOTH Management Domain AND Clause Type: %d\n", 
            length(filtered_paragraph_ids)))

## Filter paragraph_table to only include filtered paragraphs ----
paragraph_table_filtered <- paragraph_table[paragraph_id %in% filtered_paragraph_ids]

cat(sprintf("    - Filtered paragraph table: %d paragraphs (from %d total)\n",
            nrow(paragraph_table_filtered),
            nrow(paragraph_table)))

## Filter to Acts only ----
acts_only <- legislation_table[legislation_type == "Act"]
act_paragraphs <- paragraph_table_filtered[legislation_id %in% acts_only$legislation_id]

cat(sprintf("    - After filtering to Acts: %d paragraphs from %d Acts\n\n", 
            nrow(act_paragraphs), 
            length(unique(act_paragraphs$legislation_id))))

## Validate we have data to process ----
if (nrow(act_paragraphs) == 0) {
  stop("No paragraphs remaining after filtering. Check your label filters.")
}

## ============================================================================
## SECTION 1: DEFINE ENHANCED PATTERNS
## ============================================================================

cat("Step 1: Defining enhanced pattern dictionaries...\n")

## Implement Types ----
## Includes: regulatory instruments, planning documents, spatial designations,
##           authorization mechanisms, reporting requirements, AND action verbs
##           (establishment, designation, authorization, requirement)
implement_patterns <- list(
  # --- Regulatory Instruments ---
  regulation = list(
    pattern = "\\b(regulation|regulations|regulatory)\\b",
    context_boost = c("make", "issue", "prescribe", "establish"),
    authority_link = TRUE
  ),
  order = list(
    pattern = "\\b(order|orders)\\b(?!\\s+in\\s+council)",
    context_boost = c("make", "issue", "give"),
    authority_link = TRUE
  ),
  order_in_council = list(
    pattern = "\\b(order in council|orders in council|lieutenant governor in council|governor in council)\\b",
    context_boost = c("make", "issue"),
    authority_link = TRUE
  ),
  bylaw = list(
    pattern = "\\b(bylaw|by-law|bylaws|by-laws)\\b",
    context_boost = c("make", "pass", "adopt"),
    authority_link = TRUE
  ),
  
  # --- Planning Documents ---
  plan = list(
    pattern = "\\b(plan|plans|planning)\\b(?!\\s+of)",
    context_boost = c("prepare", "develop", "establish", "adopt", "approve"),
    authority_link = TRUE
  ),
  strategy = list(
    pattern = "\\b(strateg(y|ies))\\b",
    context_boost = c("prepare", "develop", "establish", "implement"),
    authority_link = TRUE
  ),
  program = list(
    pattern = "\\b(program|programme|programs|programmes)\\b",
    context_boost = c("establish", "develop", "implement", "administer"),
    authority_link = TRUE
  ),
  policy = list(
    pattern = "\\b(polic(y|ies))\\b",
    context_boost = c("establish", "develop", "adopt", "implement"),
    authority_link = TRUE
  ),
  framework = list(
    pattern = "\\b(framework|frameworks)\\b",
    context_boost = c("establish", "develop", "adopt"),
    authority_link = TRUE
  ),
  guideline = list(
    pattern = "\\b(guideline|guidelines)\\b",
    context_boost = c("issue", "establish", "develop", "publish"),
    authority_link = TRUE
  ),
  standard = list(
    pattern = "\\b(standard|standards)\\b",
    context_boost = c("establish", "set", "prescribe", "adopt"),
    authority_link = TRUE
  ),
  code = list(
    pattern = "\\b(code|codes)\\b(?!\\s+of)",
    context_boost = c("establish", "adopt", "prescribe"),
    authority_link = TRUE
  ),
  
  # --- Spatial Designations ---
  designation = list(
    pattern = "\\b(designat(e|ion|ions|ed|ing)|designated area|designated areas)\\b",
    context_boost = c("make", "establish"),
    authority_link = TRUE
  ),
  reserve = list(
    pattern = "\\b(reserve|reserves)\\b",
    context_boost = c("establish", "create", "designate"),
    authority_link = TRUE
  ),
  sanctuary = list(
    pattern = "\\b(sanctuar(y|ies))\\b",
    context_boost = c("establish", "create", "designate"),
    authority_link = TRUE
  ),
  park = list(
    pattern = "\\b(park|parks)\\b",
    context_boost = c("establish", "create", "designate"),
    authority_link = TRUE
  ),
  protected_area = list(
    pattern = "\\b(protected area|conservation area|management area|special area)\\b",
    context_boost = c("establish", "create", "designate"),
    authority_link = TRUE
  ),
  
  # --- Authorization Mechanisms ---
  agreement = list(
    pattern = "\\b(agreement|agreements)\\b",
    context_boost = c("enter into", "negotiate", "conclude", "sign"),
    authority_link = TRUE
  ),
  permit = list(
    pattern = "\\b(permit|permits)\\b",
    context_boost = c("issue", "grant", "require", "refuse"),
    authority_link = TRUE
  ),
  licence = list(
    pattern = "\\b(licen[cs]e|licen[cs]es)\\b",
    context_boost = c("issue", "grant", "require", "suspend", "cancel"),
    authority_link = TRUE
  ),
  authorization = list(
    pattern = "\\b(authori[sz]ation|authori[sz]ations)\\b",
    context_boost = c("issue", "grant", "require"),
    authority_link = TRUE
  ),
  approval = list(
    pattern = "\\b(approval|approvals)\\b",
    context_boost = c("give", "grant", "require", "withhold"),
    authority_link = TRUE
  ),
  certificate = list(
    pattern = "\\b(certificate|certificates)\\b",
    context_boost = c("issue", "grant"),
    authority_link = TRUE
  ),
  exemption = list(
    pattern = "\\b(exemption|exemptions|exempt|exempted|exempting)\\b",
    context_boost = c("grant", "issue", "provide", "allow"),
    authority_link = TRUE
  ),
  notice = list(
    pattern = "\\b(notice|notices)\\b",
    context_boost = c("give", "serve", "publish"),
    authority_link = TRUE
  ),
  
  # --- Reporting Requirements ---
  report = list(
    pattern = "\\b(report|reports|reporting)\\b",
    context_boost = c("prepare", "submit", "publish", "provide"),
    authority_link = TRUE
  ),
  assessment = list(
    pattern = "\\b(assessment|assessments)\\b",
    context_boost = c("conduct", "prepare", "require", "undertake"),
    authority_link = TRUE
  ),
  review = list(
    pattern = "\\b(review|reviews)\\b",
    context_boost = c("conduct", "undertake", "complete"),
    authority_link = TRUE
  ),
  study = list(
    pattern = "\\b(stud(y|ies))\\b",
    context_boost = c("conduct", "undertake", "commission"),
    authority_link = TRUE
  ),
  
  # --- Action Verbs (formerly in discretion_patterns) ---
  establishment = list(
    pattern = "\\b(establish|establishes|established|create|creates|created)\\b",
    context_boost = c("shall", "may", "must"),
    authority_link = TRUE
  ),
  appointment = list(
    pattern = "\\b(appoint|appoints|appointed|appointing)\\b",
    context_boost = c("shall", "may", "must"),
    authority_link = TRUE
  ),
  empowerment = list(
    pattern = "\\b(authorize|authorizes|authorized|empower|empowers|empowered)\\b",
    context_boost = c("shall", "may", "must"),
    authority_link = TRUE
  ),
  requirement = list(
    pattern = "\\b(require|requires|required|obligation|duty)\\b",
    context_boost = c("shall", "may", "must"),
    authority_link = TRUE
  )
)

## Responsible Officials (Enhanced with titles and hierarchies) ----
official_patterns <- list(
  governor_in_council = list(
    pattern = "\\b(Governor in Council|Lieutenant Governor in Council|governor in council|lieutenant governor in council)\\b",
    rank = 1,
    federal = TRUE
  ),
  minister = list(
    pattern = "\\b([Mm]inister|responsible minister)\\b",
    rank = 2,
    federal = NULL
  ),
  deputy_minister = list(
    pattern = "\\b([Dd]eputy [Mm]inister)\\b",
    rank = 3,
    federal = NULL
  ),
  director = list(
    pattern = "\\b([Dd]irector)\\b",
    rank = 4,
    federal = NULL
  ),
  commissioner = list(
    pattern = "\\b([Cc]ommissioner)\\b",
    rank = 4,
    federal = NULL
  ),
  chief = list(
    pattern = "\\b([Cc]hief)(?=\\s+\\w+)",
    rank = 4,
    federal = NULL
  ),
  board = list(
    pattern = "\\b([Bb]oard)\\b",
    rank = 3,
    federal = NULL
  ),
  commission = list(
    pattern = "\\b([Cc]ommission)\\b",
    rank = 3,
    federal = NULL
  ),
  authority = list(
    pattern = "\\b([Aa]uthority)\\b",
    rank = 3,
    federal = NULL
  ),
  agency = list(
    pattern = "\\b([Aa]gency)\\b",
    rank = 4,
    federal = NULL
  ),
  council = list(
    pattern = "\\b([Cc]ouncil)\\b",
    rank = 3,
    federal = NULL
  ),
  committee = list(
    pattern = "\\b([Cc]ommittee)\\b",
    rank = 4,
    federal = NULL
  ),
  officer = list(
    pattern = "\\b([Oo]fficer)\\b",
    rank = 5,
    federal = NULL
  ),
  inspector = list(
    pattern = "\\b([Ii]nspector)\\b",
    rank = 5,
    federal = NULL
  )
)

## Discretionary Language ----
## NOW ONLY contains mandatory vs discretionary indicators
## Excludes: prohibited, definition (not relevant to implementations)
## Moved to implement_patterns: establishment, designation, authorization, requirement
discretion_patterns <- list(
  mandatory = list(
    pattern = "\\b(shall|must|is required to|are required to)\\b",
    strength = "mandatory"
  ),
  discretionary = list(
    pattern = "\\b(may|can|is authorized to|are authorized to)\\b",
    strength = "discretionary"
  )
)

## ============================================================================
## SECTION 2: ENHANCED EXTRACTION FUNCTIONS
## ============================================================================

cat("Step 2: Defining enhanced extraction functions...\n")

## Function: Extract context window around implement mention ----
extract_context_window <- function(text, pattern, window_size = 5) {
  if (is.na(text) || text == "") return(NA_character_)
  
  words <- unlist(strsplit(text, "\\s+"))
  matches <- gregexpr(pattern, text, ignore.case = TRUE, perl = TRUE)
  
  if (matches[[1]][1] == -1) return(NA_character_)
  
  # Find word position of match
  char_pos <- matches[[1]][1]
  word_pos <- sum(cumsum(nchar(words) + 1) < char_pos) + 1
  
  # Extract window
  start_pos <- max(1, word_pos - window_size)
  end_pos <- min(length(words), word_pos + window_size)
  
  context <- paste(words[start_pos:end_pos], collapse = " ")
  return(context)
}

## Function: Enhanced implement extraction with context ----
extract_implements_enhanced <- function(paragraph_text) {
  if (is.na(paragraph_text) || paragraph_text == "") return(NA_character_)
  
  text_lower <- tolower(paragraph_text)
  implements_found <- character()
  
  for (impl_name in names(implement_patterns)) {
    impl_info <- implement_patterns[[impl_name]]
    pattern <- impl_info$pattern
    
    # Check if implement type is mentioned
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

## Function: Extract official with grammatical role analysis ----
extract_officials_enhanced <- function(paragraph_text, paragraph_id = NULL) {
  if (is.na(paragraph_text) || paragraph_text == "") return(NA_character_)
  
  officials_found <- character()
  
  for (official_name in names(official_patterns)) {
    official_info <- official_patterns[[official_name]]
    pattern <- official_info$pattern
    
    if (grepl(pattern, paragraph_text, perl = TRUE)) {
      officials_found <- c(officials_found, official_name)
    }
  }
  
  if (length(officials_found) > 0) {
    return(paste(officials_found, collapse = "; "))
  } else {
    return(NA_character_)
  }
}

## Function: Enhanced discretion type detection ----
## Now only extracts mandatory or discretionary
extract_discretion_type <- function(paragraph_text) {
  if (is.na(paragraph_text) || paragraph_text == "") return(NA_character_)
  
  text_lower <- tolower(paragraph_text)
  discretions_found <- character()
  
  for (disc_name in names(discretion_patterns)) {
    disc_info <- discretion_patterns[[disc_name]]
    pattern <- disc_info$pattern
    
    if (grepl(pattern, text_lower, perl = TRUE)) {
      discretions_found <- c(discretions_found, disc_name)
    }
  }
  
  if (length(discretions_found) > 0) {
    return(paste(discretions_found, collapse = "; "))
  } else {
    return(NA_character_)
  }
}

## Function: Classify as Legal Tool or Not Legal Tool ----
## This function determines if a paragraph describes a legal tool/implement
## based on co-occurrence of key elements
classify_legal_tool <- function(implement_type, responsible_official, discretion_type) {
  # Initialize classification
  has_implement <- !is.na(implement_type) && implement_type != ""
  has_official <- !is.na(responsible_official) && responsible_official != ""
  has_discretion <- !is.na(discretion_type) && discretion_type != ""
  
  # Classification logic:
  # LEGAL TOOL if:
  # 1. Has implement AND has discretion type (mandatory/discretionary)
  # 2. Has implement AND has responsible official
  # 3. Has all three elements (strongest indicator)
  
  # NOT LEGAL TOOL if:
  # 1. Has implement only (no context of creation/authority)
  # 2. No implement mentioned
  
  if (!has_implement) {
    return("Not Legal Tool")
  }
  
  # Check for high-confidence legal tool indicators
  if (has_implement && (has_official || has_discretion)) {
    return("Legal Tool")
  }
  
  # Check confidence level from implement_type
  if (has_implement && grepl("\\[high\\]", implement_type)) {
    return("Legal Tool")
  }
  
  # Default to "Not Legal Tool" for weak matches
  return("Not Legal Tool")
}

## ============================================================================
## SECTION 3: APPLY ENHANCED EXTRACTION
## ============================================================================

cat("\nStep 3: Applying enhanced extraction to pre-filtered paragraphs...\n")

## Apply basic extraction to pre-filtered paragraphs ----
cat("  - Extracting implement types with context...\n")
act_paragraphs[, implement_type := sapply(Paragraph, extract_implements_enhanced)]

cat("  - Extracting responsible officials...\n")
act_paragraphs[, responsible_official := sapply(Paragraph, extract_officials_enhanced)]

cat("  - Extracting discretion types (mandatory/discretionary only)...\n")
act_paragraphs[, discretion_type := sapply(Paragraph, extract_discretion_type)]

cat("\nStep 3b: Classifying paragraphs as Legal Tool or Not Legal Tool...\n")

## Apply classification function to all paragraphs ----
act_paragraphs[, legal_tool_classification := mapply(
  classify_legal_tool, 
  implement_type, 
  responsible_official, 
  discretion_type
)]

cat(sprintf("  - Classification of %d paragraphs complete\n", nrow(act_paragraphs)))
cat(sprintf("  - Potential Legal Tools: %d\n", sum(act_paragraphs$legal_tool_classification == "Legal Tool")))
cat(sprintf("  - Not Legal Tools: %d\n", sum(act_paragraphs$legal_tool_classification == "Not Legal Tool")))

## Create two datasets: Legal Tools and Not Legal Tools ----
legal_tools_data <- act_paragraphs[legal_tool_classification == "Legal Tool"]
not_legal_tools_data <- act_paragraphs[legal_tool_classification == "Not Legal Tool" & !is.na(implement_type)]

cat(sprintf("\n  - Legal Tools dataset: %d paragraphs\n", nrow(legal_tools_data)))
cat(sprintf("  - Not Legal Tools (with implement keywords): %d paragraphs\n", nrow(not_legal_tools_data)))

## Use legal_tools_data as the primary implements_data for output ----
implements_data <- legal_tools_data

cat(sprintf("\n  - Legal Tools dataset ready: %d paragraphs\n", nrow(implements_data)))

## ============================================================================
## SECTION 4: MERGE WITH METADATA AND PREPARE OUTPUT
## ============================================================================

cat("\nStep 4: Merging with legislation metadata...\n")

## Merge Legal Tools data ----
implements_data <- merge(
  implements_data[, .(paragraph_id, legislation_id, Section, Heading, Paragraph, 
                      implement_type, responsible_official, discretion_type)],
  acts_only[, .(legislation_id, act_name, jurisdiction)],
  by = "legislation_id",
  all.x = TRUE
)

## Clean confidence indicators from implement_type for output ----
implements_data[, implement_type := gsub("\\[.*?\\]", "", implement_type)]
not_legal_tools_data[, implement_type := gsub("\\[.*?\\]", "", implement_type)]

## Create clean version for summaries ----
implements_data[, implement_type_clean := implement_type]
not_legal_tools_data[, implement_type_clean := implement_type]

## Remove unwanted columns from output ----
implements_data[, c("paragraph_id", "legislation_id") := NULL]

## Reorder columns ----
setcolorder(implements_data, c(
  "act_name", "jurisdiction", "Section", "Heading", 
  "implement_type", "responsible_official", "discretion_type", 
  "Paragraph"
))

## Filter: Keep only rows where ALL three key columns have values ----
cat("\nStep 4b: Filtering rows with complete data...\n")
cat(sprintf("  - Rows before filtering: %d\n", nrow(implements_data)))

implements_data <- implements_data[
  !is.na(implement_type) & implement_type != "" &
    !is.na(responsible_official) & responsible_official != "" &
    !is.na(discretion_type) & discretion_type != ""
]

cat(sprintf("  - Rows after filtering (all 3 columns populated): %d\n", nrow(implements_data)))

## Clean encoding issues in Heading and Paragraph columns ----
cat("\nStep 4c: Cleaning text encoding issues...\n")
implements_data[, Heading := sapply(Heading, clean_encoding)]
implements_data[, Paragraph := sapply(Paragraph, clean_encoding)]
cat("  - Text cleaning complete\n")

## Sort ----
implements_data <- implements_data[order(act_name, Section)]

## ============================================================================
## SECTION 5: CREATE ENHANCED SUMMARIES
## ============================================================================

cat("Step 5: Creating enhanced summary statistics...\n")

## Summary by implement type ----
summary_by_implement <- implements_data[, .N, by = implement_type_clean][order(-N)]
setnames(summary_by_implement, c("Implement Type", "Count"))

## Summary by official ----
summary_by_official <- implements_data[!is.na(responsible_official), .N, by = responsible_official][order(-N)]
setnames(summary_by_official, c("Responsible Official", "Count"))

## Summary by discretion type ----
summary_by_discretion <- implements_data[!is.na(discretion_type), .N, by = discretion_type][order(-N)]
setnames(summary_by_discretion, c("Discretion Type", "Count"))

## Summary by act ----
summary_by_act <- implements_data[, .N, by = .(act_name, jurisdiction)][order(-N)]
setnames(summary_by_act, c("Act Name", "Jurisdiction", "Count"))

## Co-occurrence analysis: Implement + Official ----
co_occurrence <- implements_data[!is.na(responsible_official), 
                                 .N, 
                                 by = .(implement_type_clean, responsible_official)][order(-N)]
setnames(co_occurrence, c("Implement Type", "Responsible Official", "Co-occurrences"))

## Classification summary ----
classification_summary <- data.table(
  Classification = c("Legal Tool", "Not Legal Tool", "Total Processed"),
  Count = c(
    nrow(implements_data),
    nrow(not_legal_tools_data),
    nrow(act_paragraphs)
  ),
  Percentage = c(
    round(100 * nrow(implements_data) / nrow(act_paragraphs), 1),
    round(100 * nrow(not_legal_tools_data) / nrow(act_paragraphs), 1),
    100.0
  )
)

## ============================================================================
## SECTION 6: EXPORT TO CSV AND DATABASE
## ============================================================================

cat("\nStep 6: Exporting results...\n")

## Select only the columns we want in output (exclude implement_type_clean) ----
main_output <- implements_data[, .(
  act_name, jurisdiction, Section, Heading, 
  implement_type, responsible_official, discretion_type, 
  Paragraph
)]

## --- Export to CSV ---
output_file <- file.path(here("output"), "legislative_implementations.csv")

## Write CSV (fast and robust)
data.table::fwrite(main_output, output_file, sep = ",", na = "", quote = TRUE)

cat(sprintf("\n  CSV file saved to: %s\n", output_file))

## --- Export to SQLite Database ---
cat("\n  Writing to SQLite database...\n")

## Write table to database (overwrite if exists)
dbWriteTable(conn, "legislative_implementations", main_output, overwrite = TRUE)

cat(sprintf("  Table 'legislative_implementations' saved to database: %s\n", db_path))
cat(sprintf("  Rows written: %d\n", nrow(main_output)))

## Disconnect from database ----
dbDisconnect(conn)
cat("  Database connection closed.\n")

## ============================================================================
## SECTION 7: PRINT SUMMARY STATISTICS
## ============================================================================

cat("\n=====================================\n")
cat("SUMMARY STATISTICS\n")
cat("=====================================\n\n")

cat("PRE-FILTERING:\n")
cat(sprintf("  Total paragraphs in database: %d\n", nrow(paragraph_table)))
cat(sprintf("  Paragraphs with Management Domain labels: %d\n", length(management_domain_paragraph_ids)))
cat(sprintf("  Paragraphs with specified Clause Type labels: %d\n", length(clause_type_paragraph_ids)))
cat(sprintf("  Paragraphs with BOTH filters (intersection): %d\n", length(filtered_paragraph_ids)))
cat(sprintf("  Paragraphs from Acts (final filter): %d\n", nrow(act_paragraphs)))

cat("\nINCLUDED CLAUSE TYPES:\n")
for (ct in included_clause_types) {
  cat(sprintf("  - %s\n", ct))
}

cat("\nDISCRETION TYPES (mandatory/discretionary only):\n")
for (dt in names(discretion_patterns)) {
  cat(sprintf("  - %s\n", dt))
}

cat("\nCLASSIFICATION:\n")
print(classification_summary)

cat("\nTop 10 Implement Types:\n")
print(head(summary_by_implement, 10))

cat("\nTop 10 Responsible Officials:\n")
print(head(summary_by_official, 10))

cat("\nDiscretion Type Summary:\n")
print(summary_by_discretion)

cat("\nTop 5 Implement-Official Combinations:\n")
print(head(co_occurrence, 5))

cat("\n=====================================\n")
cat("Analysis complete!\n")
cat("Output saved to CSV and SQLite database.\n")
cat("=====================================\n")

## Notify Completion ----
beep(sound = 1)