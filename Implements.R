################################################################################
# Title: Legislative Implements Extraction - Simplified
# Authors: Joe Enns, Cory Lagasse (Modified by Claude)
# Date Created: 2025-01-XX
# Purpose / Description:
#   Extracts implements from first paragraphs, identifying implement type,
#   responsible official, and provision type (mandatory vs discretionary)
# Dependencies: DBI, RSQLite, data.table, here, openxlsx, stringr
# Outputs:
#   "Legislative_Implements_Final.xlsx" in the project root directory
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
cat("Legislative Implements Extraction\n")
cat("=====================================\n\n")

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

dbDisconnect(conn)

## Filter to Acts only ----
acts_only <- legislation_table[legislation_type == "Act"]
act_paragraphs <- paragraph_table[legislation_id %in% acts_only$legislation_id]

cat(sprintf("Analyzing %d paragraphs from %d Acts...\n\n", 
            nrow(act_paragraphs), 
            length(unique(act_paragraphs$legislation_id))))

## ============================================================================
## SECTION 1: IDENTIFY FIRST PARAGRAPHS
## ============================================================================

cat("Step 1: Identifying first paragraphs of sections and after headings...\n")

## Sort by legislation, section, and paragraph order
setorder(act_paragraphs, legislation_id, Section, paragraph_id)

## Mark first paragraph of each section
act_paragraphs[, is_first_in_section := FALSE]
act_paragraphs[, section_heading_group := paste(legislation_id, Section, 
                                                ifelse(is.na(Heading), "NO_HEADING", Heading), 
                                                sep = "___")]

## For each section-heading group, mark the first paragraph
act_paragraphs[, is_first_in_section := seq_len(.N) == 1, by = section_heading_group]

first_paragraphs <- act_paragraphs[is_first_in_section == TRUE]

cat(sprintf("  - Identified %d first paragraphs to analyze\n\n", nrow(first_paragraphs)))

## ============================================================================
## SECTION 2: DEFINE PATTERNS
## ============================================================================

cat("Step 2: Defining patterns...\n")

## Implement trigger verbs
implement_verbs <- c(
  "make", "establish", "create", "designate", "issue", "prepare", 
  "develop", "adopt", "prescribe", "set", "publish", "give",
  "enter into", "negotiate", "conclude", "sign", "grant", "approve",
  "included in", "implementing"
)

## Implement types (sorted longest first for best matching)
implement_types <- c(
  "order in council", "orders in council",
  "protected area", "conservation area", "management area", "special area",
  "action plan", "recovery plan", "management plan", "conservation plan",
  "sustainability plan", "stewardship plan", "operational plan",
  "recovery strategy", "management strategy", "conservation strategy",
  "by-law", "bylaw", "by-laws", "bylaws",
  "regulation", "regulations",
  "order", "orders",
  "plan", "plans",
  "strategy", "strategies",
  "program", "programme", "programs", "programmes",
  "policy", "policies",
  "framework", "frameworks",
  "guideline", "guidelines",
  "standard", "standards",
  "code", "codes",
  "designation", "designations",
  "reserve", "reserves",
  "sanctuary", "sanctuaries",
  "park", "parks",
  "agreement", "agreements",
  "permit", "permits",
  "licence", "license", "licences", "licenses",
  "authorization", "authorisation", "authorizations",
  "approval", "approvals",
  "certificate", "certificates",
  "notice", "notices",
  "report", "reports",
  "assessment", "assessments",
  "review", "reviews",
  "study", "studies"
)

## Responsible officials
official_patterns <- c(
  "Governor in Council", "Lieutenant Governor in Council",
  "Minister", "minister", "Deputy Minister", "director", "Director",
  "commissioner", "Commissioner", "Chief", "chief", "Board", "board",
  "Commission", "commission", "Authority", "authority", "Agency", "agency",
  "Council", "council", "Committee", "committee"
)

## Provision type patterns
provision_patterns <- list(
  mandatory = list(
    keywords = c("shall", "must", "is required to", "are required to", "has a duty to", "have a duty to"),
    pattern = "\\b(shall|must|is required to|are required to|has a duty to|have a duty to)\\b"
  ),
  discretionary = list(
    keywords = c("may", "can", "is authorized to", "are authorized to", "is permitted to", "are permitted to"),
    pattern = "\\b(may|can|is authorized to|are authorized to|is permitted to|are permitted to)\\b"
  ),
  prohibited = list(
    keywords = c("shall not", "must not", "may not", "prohibited", "forbidden"),
    pattern = "\\b(shall not|must not|may not|prohibited|forbidden)\\b"
  )
)

## ============================================================================
## SECTION 3: EXTRACTION FUNCTIONS
## ============================================================================

cat("Step 3: Defining extraction functions...\n")

## Function: Check if paragraph contains implement-creating language
contains_implement_language <- function(text) {
  if (is.na(text) || text == "") return(FALSE)
  
  text_lower <- tolower(text)
  
  # Check for implement verbs
  verb_pattern <- paste0("\\b(", paste(implement_verbs, collapse = "|"), ")\\b")
  has_verb <- grepl(verb_pattern, text_lower, perl = TRUE)
  
  # Check for implement types
  type_pattern <- paste0("\\b(", paste(implement_types, collapse = "|"), ")\\b")
  has_type <- grepl(type_pattern, text_lower, perl = TRUE)
  
  # Must have both verb and type
  return(has_verb && has_type)
}

## Function: Extract responsible official
extract_official <- function(text) {
  if (is.na(text) || text == "") return(NA_character_)
  
  officials_found <- character()
  
  for (pattern in official_patterns) {
    if (grepl(pattern, text, perl = TRUE)) {
      officials_found <- c(officials_found, pattern)
    }
  }
  
  if (length(officials_found) > 0) {
    # Return the first (usually most specific) official found
    return(officials_found[1])
  } else {
    return(NA_character_)
  }
}

## Function: Extract implement type
extract_implement_type <- function(text) {
  if (is.na(text) || text == "") return(NA_character_)
  
  text_lower <- tolower(text)
  types_found <- character()
  
  # Check longest patterns first
  for (impl_type in implement_types) {
    if (grepl(paste0("\\b", impl_type, "\\b"), text_lower, perl = TRUE)) {
      types_found <- c(types_found, impl_type)
    }
  }
  
  if (length(types_found) > 0) {
    return(paste(unique(types_found), collapse = "; "))
  } else {
    return(NA_character_)
  }
}

## Function: Extract provision type (mandatory, discretionary, prohibited)
extract_provision_type <- function(text) {
  if (is.na(text) || text == "") return(NA_character_)
  
  text_lower <- tolower(text)
  provisions_found <- character()
  
  # Check each provision type
  for (prov_name in names(provision_patterns)) {
    prov_info <- provision_patterns[[prov_name]]
    
    if (grepl(prov_info$pattern, text_lower, perl = TRUE)) {
      provisions_found <- c(provisions_found, prov_name)
    }
  }
  
  if (length(provisions_found) > 0) {
    # Return the most specific (prohibited > mandatory > discretionary)
    if ("prohibited" %in% provisions_found) {
      return("Prohibited")
    } else if ("mandatory" %in% provisions_found) {
      return("Mandatory")
    } else if ("discretionary" %in% provisions_found) {
      return("Discretionary")
    }
  }
  
  return(NA_character_)
}

## ============================================================================
## SECTION 4: APPLY EXTRACTION
## ============================================================================

cat("\nStep 4: Applying extraction to first paragraphs...\n")

## Filter to paragraphs with implement language
cat("  - Filtering to paragraphs with implement-creating language...\n")
first_paragraphs[, has_implement := sapply(Paragraph, contains_implement_language)]
implement_paragraphs <- first_paragraphs[has_implement == TRUE]

cat(sprintf("  - Found %d first paragraphs with implement language\n", nrow(implement_paragraphs)))

## Extract implement types
cat("  - Extracting implement types...\n")
implement_paragraphs[, implement_type := sapply(Paragraph, extract_implement_type)]

## Extract responsible officials
cat("  - Extracting responsible officials...\n")
implement_paragraphs[, responsible_official := sapply(Paragraph, extract_official)]

## Extract provision types
cat("  - Extracting provision types (mandatory/discretionary)...\n")
implement_paragraphs[, provision_type := sapply(Paragraph, extract_provision_type)]

cat("  - Extraction complete!\n\n")

## ============================================================================
## SECTION 5: MERGE WITH METADATA
## ============================================================================

cat("Step 5: Merging with legislation metadata...\n")

implements_data <- merge(
  implement_paragraphs[, .(paragraph_id, legislation_id, Section, Heading, Paragraph,
                           implement_type, responsible_official, provision_type)],
  acts_only[, .(legislation_id, act_name, jurisdiction)],
  by = "legislation_id",
  all.x = TRUE
)

## Reorder columns
setcolorder(implements_data, c(
  "act_name", "jurisdiction", "Section", "Heading",
  "implement_type", "responsible_official", "provision_type", "Paragraph"
))

## Sort
implements_data <- implements_data[order(act_name, Section)]

## ============================================================================
## SECTION 6: CREATE SUMMARIES
## ============================================================================

cat("Step 6: Creating summary statistics...\n")

## Summary by implement type
summary_by_type <- implements_data[!is.na(implement_type), .N, by = implement_type][order(-N)]
setnames(summary_by_type, c("Implement Type", "Count"))

## Summary by official
summary_by_official <- implements_data[!is.na(responsible_official), .N, by = responsible_official][order(-N)]
setnames(summary_by_official, c("Responsible Official", "Count"))

## Summary by provision type
summary_by_provision <- implements_data[!is.na(provision_type), .N, by = provision_type][order(-N)]
setnames(summary_by_provision, c("Provision Type", "Count"))

## Summary by act
summary_by_act <- implements_data[, .N, by = .(act_name, jurisdiction)][order(-N)]
setnames(summary_by_act, c("Act Name", "Jurisdiction", "Implement Count"))

## Cross-tabulation: Implement type by provision type
crosstab_type_provision <- implements_data[!is.na(implement_type) & !is.na(provision_type), 
                                           .N, 
                                           by = .(implement_type, provision_type)][order(implement_type, provision_type)]
setnames(crosstab_type_provision, c("Implement Type", "Provision Type", "Count"))

## Cross-tabulation: Official by provision type
crosstab_official_provision <- implements_data[!is.na(responsible_official) & !is.na(provision_type), 
                                               .N, 
                                               by = .(responsible_official, provision_type)][order(responsible_official, provision_type)]
setnames(crosstab_official_provision, c("Responsible Official", "Provision Type", "Count"))

## ============================================================================
## SECTION 7: EXPORT TO EXCEL
## ============================================================================

cat("\nStep 7: Creating Excel workbook...\n")

output_file <- file.path(here(), "Legislative_Implements.xlsx")

wb <- createWorkbook()

## Main data sheet
addWorksheet(wb, "Implements")
writeDataTable(wb, "Implements", implements_data)

## Summary sheets
addWorksheet(wb, "Summary by Type")
writeDataTable(wb, "Summary by Type", summary_by_type)

addWorksheet(wb, "Summary by Official")
writeDataTable(wb, "Summary by Official", summary_by_official)

addWorksheet(wb, "Summary by Provision Type")
writeDataTable(wb, "Summary by Provision Type", summary_by_provision)

addWorksheet(wb, "Summary by Act")
writeDataTable(wb, "Summary by Act", summary_by_act)

addWorksheet(wb, "Type x Provision")
writeDataTable(wb, "Type x Provision", crosstab_type_provision)

addWorksheet(wb, "Official x Provision")
writeDataTable(wb, "Official x Provision", crosstab_official_provision)

## Format columns
setColWidths(wb, "Implements", cols = 1:8, 
             widths = c(35, 12, 10, 30, 25, 25, 20, 70))
setColWidths(wb, "Summary by Type", cols = 1:2, widths = c(30, 10))
setColWidths(wb, "Summary by Official", cols = 1:2, widths = c(30, 10))
setColWidths(wb, "Summary by Provision Type", cols = 1:2, widths = c(20, 10))
setColWidths(wb, "Summary by Act", cols = 1:3, widths = c(40, 15, 10))
setColWidths(wb, "Type x Provision", cols = 1:3, widths = c(30, 20, 10))
setColWidths(wb, "Official x Provision", cols = 1:3, widths = c(30, 20, 10))

## Save workbook
saveWorkbook(wb, output_file, overwrite = TRUE)

cat(sprintf("\nâœ… Excel file saved to: %s\n", output_file))

## ============================================================================
## SECTION 8: PRINT SUMMARY
## ============================================================================

cat("\n=====================================\n")
cat("SUMMARY STATISTICS\n")
cat("=====================================\n\n")

cat(sprintf("Total first paragraphs analyzed: %d\n", nrow(first_paragraphs)))
cat(sprintf("Paragraphs with implement language: %d\n", nrow(implements_data)))
cat(sprintf("Unique Acts with implements: %d\n", length(unique(implements_data$act_name))))

cat("\nTop 10 Implement Types:\n")
print(head(summary_by_type, 10))

cat("\nTop 10 Responsible Officials:\n")
print(head(summary_by_official, 10))

cat("\nProvision Types (Mandatory vs Discretionary):\n")
print(summary_by_provision)

cat("\nTop 10 Acts by Implement Count:\n")
print(head(summary_by_act, 10))

cat("\nSample of Implements with Provision Types:\n")
sample_data <- implements_data[!is.na(provision_type)][1:min(10, .N)]
print(sample_data[, .(act_name, Section, implement_type, responsible_official, provision_type)])

cat("\n=====================================\n")
cat("Analysis complete!\n")
cat("=====================================\n")

## Notify Completion
beep(sound = 1)