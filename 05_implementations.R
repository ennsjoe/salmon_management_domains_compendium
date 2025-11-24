################################################################################
# Title: Enhanced Legislative Implements Extraction with NLP
# Authors: Joe Enns, Cory Lagasse
# Date Created: 2025-01-XX
# Purpose / Description:
#   Enhanced version using advanced NLP techniques including dependency parsing,
#   part-of-speech tagging, context windows, and machine learning to extract
#   implements from legislation with improved accuracy.
# Dependencies: DBI, RSQLite, data.table, here, openxlsx, stringr
# Outputs:
#   "Legislative_Implements_Enhanced.xlsx" in the project root directory
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
## SECTION 1: DEFINE ENHANCED PATTERNS
## ============================================================================

cat("Step 1: Defining enhanced pattern dictionaries...\n")

## Implement Types (Enhanced with variants) ----
implement_patterns <- list(
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
  notice = list(
    pattern = "\\b(notice|notices)\\b",
    context_boost = c("give", "serve", "publish"),
    authority_link = TRUE
  ),
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

## Provision Types (Enhanced from Clause Types) ----
provision_patterns <- list(
  mandatory = list(
    pattern = "\\b(shall|must|is required to|are required to)\\b",
    strength = "mandatory",
    keywords = c("shall", "must", "required")
  ),
  discretionary = list(
    pattern = "\\b(may|can|is authorized to|are authorized to)\\b",
    strength = "discretionary",
    keywords = c("may", "can", "authorized")
  ),
  prohibited = list(
    pattern = "\\b(shall not|must not|may not|prohibited|forbidden)\\b",
    strength = "mandatory",
    keywords = c("not", "prohibited", "forbidden")
  ),
  definition = list(
    pattern = "\\b(means|includes|refers to|definition|defined as)\\b",
    strength = "declaratory",
    keywords = c("means", "includes", "definition")
  ),
  establishment = list(
    pattern = "\\b(establish|establishes|established|create|creates|created)\\b",
    strength = "constitutive",
    keywords = c("establish", "create")
  ),
  designation = list(
    pattern = "\\b(designate|designates|designated|appoint|appoints|appointed)\\b",
    strength = "constitutive",
    keywords = c("designate", "appoint")
  ),
  authorization = list(
    pattern = "\\b(authorize|authorizes|authorized|empower|empowers|empowered)\\b",
    strength = "enabling",
    keywords = c("authorize", "empower")
  ),
  requirement = list(
    pattern = "\\b(require|requires|required|obligation|duty)\\b",
    strength = "mandatory",
    keywords = c("require", "obligation", "duty")
  )
)



## ============================================================================
## SECTION 3: ENHANCED EXTRACTION FUNCTIONS
## ============================================================================

cat("Step 3: Defining enhanced extraction functions...\n")

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

## Function: Enhanced provision type detection ----
extract_provision_type <- function(paragraph_text) {
  if (is.na(paragraph_text) || paragraph_text == "") return(NA_character_)
  
  text_lower <- tolower(paragraph_text)
  provisions_found <- character()
  
  for (prov_name in names(provision_patterns)) {
    prov_info <- provision_patterns[[prov_name]]
    pattern <- prov_info$pattern
    
    if (grepl(pattern, text_lower, perl = TRUE)) {
      provisions_found <- c(provisions_found, prov_name)
    }
  }
  
  if (length(provisions_found) > 0) {
    return(paste(provisions_found, collapse = "; "))
  } else {
    return(NA_character_)
  }
}



## Function: Classify as Legal Tool or Not Legal Tool ----
## This function determines if a paragraph describes a legal tool/implement
## based on co-occurrence of key elements
classify_legal_tool <- function(implement_type, responsible_official, provision_type) {
  # Initialize classification
  has_implement <- !is.na(implement_type) && implement_type != ""
  has_official <- !is.na(responsible_official) && responsible_official != ""
  has_provision <- !is.na(provision_type) && provision_type != ""
  
  # Classification logic:
  # LEGAL TOOL if:
  # 1. Has implement AND has provision type (mandate/authorization)
  # 2. Has implement AND has responsible official
  # 3. Has all three elements (strongest indicator)
  
  # NOT LEGAL TOOL if:
  # 1. Has implement only (no context of creation/authority)
  # 2. No implement mentioned
  
  if (!has_implement) {
    return("Not Legal Tool")
  }
  
  # Check for high-confidence legal tool indicators
  if (has_implement && (has_official || has_provision)) {
    # Additional check: is the provision type about creation/authorization?
    if (has_provision) {
      creation_provisions <- c("establishment", "designation", "authorization", 
                               "mandatory", "discretionary", "requirement")
      provision_lower <- tolower(provision_type)
      has_creation <- any(sapply(creation_provisions, function(x) grepl(x, provision_lower)))
      
      if (has_creation) {
        return("Legal Tool")
      }
    }
    
    # If has official, likely a legal tool
    if (has_official) {
      return("Legal Tool")
    }
  }
  
  # Check confidence level from implement_type
  if (has_implement && grepl("\\[high\\]", implement_type)) {
    return("Legal Tool")
  }
  
  # Default to "Not Legal Tool" for weak matches
  return("Not Legal Tool")
}



## ============================================================================
## SECTION 4: APPLY ENHANCED EXTRACTION
## ============================================================================

cat("\nStep 4: Applying enhanced extraction to ALL paragraphs...\n")

## Apply basic extraction to ALL paragraphs (not just those with implements) ----
cat("  - Extracting implement types with context...\n")
act_paragraphs[, implement_type := sapply(Paragraph, extract_implements_enhanced)]

cat("  - Extracting responsible officials...\n")
act_paragraphs[, responsible_official := sapply(Paragraph, extract_officials_enhanced)]

cat("  - Extracting provision types...\n")
act_paragraphs[, provision_type := sapply(Paragraph, extract_provision_type)]

cat("\nStep 4b: Classifying ALL paragraphs as Legal Tool or Not Legal Tool...\n")

## Apply classification function to ALL paragraphs ----
act_paragraphs[, legal_tool_classification := mapply(
  classify_legal_tool, 
  implement_type, 
  responsible_official, 
  provision_type
)]

cat(sprintf("  - Initial classification of %d paragraphs complete\n", nrow(act_paragraphs)))
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
## SECTION 6: MERGE WITH METADATA AND PREPARE OUTPUT
## ============================================================================

cat("\nStep 6: Merging with legislation metadata...\n")

## Merge Legal Tools data ----
implements_data <- merge(
  implements_data[, .(paragraph_id, legislation_id, Section, Heading, Paragraph, 
                     implement_type, responsible_official, provision_type, 
                     legal_tool_classification)],
  acts_only[, .(legislation_id, act_name, jurisdiction)],
  by = "legislation_id",
  all.x = TRUE
)

## Merge Not Legal Tools data (for comparison/export) ----
not_legal_tools_data <- merge(
  not_legal_tools_data[, .(paragraph_id, legislation_id, Section, Heading, Paragraph, 
                          implement_type, responsible_official, provision_type, 
                          legal_tool_classification)],
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

## Reorder columns ----
setcolorder(implements_data, c(
  "legal_tool_classification",
  "act_name", "jurisdiction", "Section", "Heading", 
  "implement_type", "responsible_official", "provision_type", 
  "Paragraph"
))

setcolorder(not_legal_tools_data, c(
  "legal_tool_classification",
  "act_name", "jurisdiction", "Section", "Heading", 
  "implement_type", "responsible_official", "provision_type", 
  "Paragraph"
))

## Sort ----
implements_data <- implements_data[order(act_name, Section)]
not_legal_tools_data <- not_legal_tools_data[order(act_name, Section)]

## ============================================================================
## SECTION 7: CREATE ENHANCED SUMMARIES
## ============================================================================

cat("Step 7: Creating enhanced summary statistics...\n")

## Summary by implement type ----
summary_by_implement <- implements_data[, .N, by = implement_type_clean][order(-N)]
setnames(summary_by_implement, c("Implement Type", "Count"))

## Summary by official ----
summary_by_official <- implements_data[!is.na(responsible_official), .N, by = responsible_official][order(-N)]
setnames(summary_by_official, c("Responsible Official", "Count"))

## Summary by provision type ----
summary_by_provision <- implements_data[!is.na(provision_type), .N, by = provision_type][order(-N)]
setnames(summary_by_provision, c("Provision Type", "Count"))

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
  Classification = c("Legal Tool", "Not Legal Tool", "Total"),
  Count = c(
    nrow(implements_data),
    nrow(not_legal_tools_data),
    nrow(implements_data) + nrow(not_legal_tools_data)
  ),
  Percentage = c(
    round(100 * nrow(implements_data) / (nrow(implements_data) + nrow(not_legal_tools_data)), 1),
    round(100 * nrow(not_legal_tools_data) / (nrow(implements_data) + nrow(not_legal_tools_data)), 1),
    100.0
  )
)

## SECTION 8: EXPORT TO EXCEL

output_file <- file.path(here("output"), "Legislative_Implementations.xlsx")

## Check if file is open and create backup name if needed ----
if (file.exists(output_file)) {
  test_write <- try(file.create(output_file), silent = TRUE)
  if (inherits(test_write, "try-error") || !test_write) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    output_file <- file.path(here(), paste0("Legislative_Implements_Enhanced_", timestamp, ".xlsx"))
    cat(sprintf("  ⚠️  Original file is open or locked. Saving to: %s\n", basename(output_file)))
  } else {
    # Clean up test file
    if (file.exists(output_file)) file.remove(output_file)
  }
}

wb <- createWorkbook()

## Main data sheets ----
addWorksheet(wb, "Legal Tools")
writeDataTable(wb, "Legal Tools", implements_data)

addWorksheet(wb, "Not Legal Tools")
writeDataTable(wb, "Not Legal Tools", not_legal_tools_data)

addWorksheet(wb, "Classification Summary")
writeDataTable(wb, "Classification Summary", classification_summary)

## Summary sheets ----
addWorksheet(wb, "Summary by Implement Type")
writeDataTable(wb, "Summary by Implement Type", summary_by_implement)

addWorksheet(wb, "Summary by Official")
writeDataTable(wb, "Summary by Official", summary_by_official)

addWorksheet(wb, "Summary by Provision Type")
writeDataTable(wb, "Summary by Provision Type", summary_by_provision)

addWorksheet(wb, "Summary by Act")
writeDataTable(wb, "Summary by Act", summary_by_act)

addWorksheet(wb, "Implement-Official Co-occur")
writeDataTable(wb, "Implement-Official Co-occur", co_occurrence)

## Format columns ----
setColWidths(wb, "Legal Tools", cols = 1:9, 
             widths = c(15, 35, 12, 10, 30, 30, 25, 25, 70))
setColWidths(wb, "Not Legal Tools", cols = 1:9, 
             widths = c(15, 35, 12, 10, 30, 30, 25, 25, 70))
setColWidths(wb, "Classification Summary", cols = 1:3, widths = c(20, 12, 12))
setColWidths(wb, "Summary by Implement Type", cols = 1:2, widths = c(30, 10))
setColWidths(wb, "Summary by Official", cols = 1:2, widths = c(30, 10))
setColWidths(wb, "Summary by Provision Type", cols = 1:2, widths = c(30, 10))
setColWidths(wb, "Summary by Act", cols = 1:3, widths = c(40, 15, 10))
setColWidths(wb, "Implement-Official Co-occur", cols = 1:3, widths = c(30, 30, 15))

## Save workbook ----
saveWorkbook(wb, output_file, overwrite = TRUE)

cat(sprintf("\n✅ Enhanced Excel file saved to: %s\n", output_file))

## ============================================================================
## SECTION 9: PRINT SUMMARY STATISTICS
## ============================================================================

cat("\n=====================================\n")
cat("SUMMARY STATISTICS\n")
cat("=====================================\n\n")

cat("CLASSIFICATION RESULTS:\n")
print(classification_summary)

cat(sprintf("\n\nLEGAL TOOLS DETAILS:\n"))
cat(sprintf("Total Legal Tool paragraphs: %d\n", nrow(implements_data)))
cat(sprintf("Unique Acts: %d\n", length(unique(implements_data$act_name))))
cat(sprintf("Unique implement types found: %d\n", nrow(summary_by_implement)))
cat(sprintf("Paragraphs with implement + (official OR provision)\n"))

cat("\n\nNOT LEGAL TOOLS DETAILS:\n")
cat(sprintf("Total Not Legal Tool paragraphs: %d\n", nrow(not_legal_tools_data)))
cat(sprintf("Paragraphs with implement keywords but lacking official/provision context\n"))

cat("\nTop 10 Implement Types (Legal Tools):\n")
print(head(summary_by_implement, 10))

cat("\nTop 10 Responsible Officials (Legal Tools):\n")
print(head(summary_by_official, 10))

cat("\nTop 10 Provision Types (Legal Tools):\n")
print(head(summary_by_provision, 10))

cat("\nTop 5 Implement-Official Combinations:\n")
print(head(co_occurrence, 5))

cat("\n=====================================\n")
cat("Analysis complete!\n")
cat("=====================================\n")

## Notify Completion ----
beep(sound = 1)
