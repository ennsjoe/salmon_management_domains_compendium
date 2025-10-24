################################################################################
# Title: Extract Legislative Implements
# Authors: Joe Enns, Cory Lagasse
# Date Created: 2025-01-XX
# Purpose / Description:
#   This script extracts "implements" from legislation - the products mandated
#   by acts such as orders, regulations, plans, strategies, land designations, etc.
#   It also identifies the responsible officials (ministers, governor in council, etc.)
# Dependencies: DBI, RSQLite, data.table, here, openxlsx, stringr
# Outputs:
#   An Excel file named "Legislative_Implements.xlsx" in the project root directory
################################################################################

## Load Libraries ----
library(here)
library(DBI)
library(RSQLite)
library(data.table)
library(openxlsx)
library(stringr)
library(beepr)

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

cat(sprintf("Analyzing %d paragraphs from %d Acts...\n", 
            nrow(act_paragraphs), 
            length(unique(act_paragraphs$legislation_id))))

## Define Implement Types ----
implement_patterns <- c(
  "regulation" = "\\b(regulation|regulations)\\b",
  "order" = "\\b(order|orders)\\b(?!\\s+in\\s+council)",
  "order in council" = "\\b(order in council|orders in council)\\b",
  "bylaw" = "\\b(bylaw|by-law|bylaws|by-laws)\\b",
  "plan" = "\\b(plan|plans)\\b(?!\\s+of)",
  "strategy" = "\\b(strategy|strategies)\\b",
  "program" = "\\b(program|programme|programs|programmes)\\b",
  "policy" = "\\b(policy|policies)\\b",
  "framework" = "\\b(framework|frameworks)\\b",
  "guideline" = "\\b(guideline|guidelines)\\b",
  "standard" = "\\b(standard|standards)\\b",
  "code" = "\\b(code|codes)\\b(?!\\s+of)",
  "designation" = "\\b(designation|designations|designated area|designated areas)\\b",
  "reserve" = "\\b(reserve|reserves)\\b",
  "sanctuary" = "\\b(sanctuary|sanctuaries)\\b",
  "park" = "\\b(park|parks)\\b",
  "area" = "\\b(protected area|conservation area|management area|special area)\\b",
  "agreement" = "\\b(agreement|agreements)\\b",
  "permit" = "\\b(permit|permits)\\b",
  "licence" = "\\b(licence|license|licences|licenses)\\b",
  "authorization" = "\\b(authorization|authorisation|authorizations|authorisations)\\b",
  "approval" = "\\b(approval|approvals)\\b",
  "certificate" = "\\b(certificate|certificates)\\b",
  "notice" = "\\b(notice|notices)\\b",
  "report" = "\\b(report|reports)\\b",
  "assessment" = "\\b(assessment|assessments)\\b",
  "review" = "\\b(review|reviews)\\b",
  "study" = "\\b(study|studies)\\b"
)

## Define Responsible Officials ----
official_patterns <- c(
  "Governor in Council" = "\\b(Governor in Council|Lieutenant Governor in Council)\\b",
  "Minister" = "\\b(Minister|minister)\\b",
  "Director" = "\\b(Director|director)\\b",
  "Commissioner" = "\\b(Commissioner|commissioner)\\b",
  "Chief" = "\\b(Chief|chief)(?=\\s+\\w+)",
  "Board" = "\\b(Board|board)\\b",
  "Commission" = "\\b(Commission|commission)\\b",
  "Authority" = "\\b(Authority|authority)\\b",
  "Agency" = "\\b(Agency|agency)\\b",
  "Council" = "\\b(Council|council)\\b",
  "Committee" = "\\b(Committee|committee)\\b",
  "Officer" = "\\b(Officer|officer)\\b",
  "Inspector" = "\\b(Inspector|inspector)\\b"
)

## Define Action Verbs (to identify mandates) ----
mandate_patterns <- c(
  "must" = "\\b(must|shall)\\b",
  "may" = "\\b(may|can)\\b",
  "establish" = "\\b(establish|establishes|established|establishing)\\b",
  "make" = "\\b(make|makes|made|making)\\b",
  "issue" = "\\b(issue|issues|issued|issuing)\\b",
  "create" = "\\b(create|creates|created|creating)\\b",
  "designate" = "\\b(designate|designates|designated|designating)\\b",
  "prepare" = "\\b(prepare|prepares|prepared|preparing)\\b",
  "develop" = "\\b(develop|develops|developed|developing)\\b",
  "approve" = "\\b(approve|approves|approved|approving)\\b",
  "authorize" = "\\b(authorize|authorizes|authorized|authorizing|authorise|authorises|authorised|authorising)\\b"
)

## Function to Extract Implements ----
extract_implements <- function(paragraph_text) {
  if (is.na(paragraph_text) || paragraph_text == "") return(NA_character_)
  
  # Convert to lowercase for matching
  text_lower <- tolower(paragraph_text)
  
  # Find implement types
  matches <- sapply(implement_patterns, function(pattern) {
    grepl(pattern, text_lower, ignore.case = TRUE, perl = TRUE)
  })
  
  if (any(matches)) {
    return(paste(names(implement_patterns)[matches], collapse = "; "))
  } else {
    return(NA_character_)
  }
}

## Function to Extract Officials ----
extract_officials <- function(paragraph_text) {
  if (is.na(paragraph_text) || paragraph_text == "") return(NA_character_)
  
  # Find official types
  matches <- sapply(official_patterns, function(pattern) {
    grepl(pattern, paragraph_text, ignore.case = TRUE, perl = TRUE)
  })
  
  if (any(matches)) {
    return(paste(names(official_patterns)[matches], collapse = "; "))
  } else {
    return(NA_character_)
  }
}

## Function to Extract Mandate Type ----
extract_mandate <- function(paragraph_text) {
  if (is.na(paragraph_text) || paragraph_text == "") return(NA_character_)
  
  text_lower <- tolower(paragraph_text)
  
  # Find mandate types
  matches <- sapply(mandate_patterns, function(pattern) {
    grepl(pattern, text_lower, ignore.case = TRUE, perl = TRUE)
  })
  
  if (any(matches)) {
    return(paste(names(mandate_patterns)[matches], collapse = "; "))
  } else {
    return(NA_character_)
  }
}

## Apply Extraction Functions ----
cat("Extracting implement types...\n")
act_paragraphs[, implement_type := sapply(Paragraph, extract_implements)]

cat("Extracting responsible officials...\n")
act_paragraphs[, responsible_official := sapply(Paragraph, extract_officials)]

cat("Extracting mandate types...\n")
act_paragraphs[, mandate_type := sapply(Paragraph, extract_mandate)]

## Filter to rows with implements ----
implements_data <- act_paragraphs[!is.na(implement_type)]

cat(sprintf("Found %d paragraphs containing implements.\n", nrow(implements_data)))

## Merge with Legislation Metadata ----
implements_data <- merge(
  implements_data[, .(paragraph_id, legislation_id, Section, Heading, Paragraph, 
                     implement_type, responsible_official, mandate_type)],
  acts_only[, .(legislation_id, act_name, jurisdiction)],
  by = "legislation_id",
  all.x = TRUE
)

## Reorder and Select Columns ----
setcolorder(implements_data, c(
  "act_name", "jurisdiction", "Section", "Heading", 
  "implement_type", "responsible_official", "mandate_type", "Paragraph"
))

## Sort by act_name and Section ----
implements_data <- implements_data[order(act_name, Section)]

## Create Summary Statistics ----
summary_by_implement <- implements_data[, .N, by = implement_type][order(-N)]
summary_by_official <- implements_data[!is.na(responsible_official), .N, by = responsible_official][order(-N)]
summary_by_act <- implements_data[, .N, by = .(act_name, jurisdiction)][order(-N)]

## Export to Excel ----
output_file <- file.path(here(), "Legislative_Implements.xlsx")

cat("Creating Excel workbook...\n")
wb <- createWorkbook()

# Main data sheet
addWorksheet(wb, "Implements")
writeDataTable(wb, "Implements", implements_data)

# Summary sheets
addWorksheet(wb, "Summary by Implement Type")
writeDataTable(wb, "Summary by Implement Type", summary_by_implement)

addWorksheet(wb, "Summary by Official")
writeDataTable(wb, "Summary by Official", summary_by_official)

addWorksheet(wb, "Summary by Act")
writeDataTable(wb, "Summary by Act", summary_by_act)

# Format columns
setColWidths(wb, "Implements", cols = 1:8, widths = c(30, 12, 10, 30, 25, 25, 20, 60))
setColWidths(wb, "Summary by Implement Type", cols = 1:2, widths = c(30, 10))
setColWidths(wb, "Summary by Official", cols = 1:2, widths = c(30, 10))
setColWidths(wb, "Summary by Act", cols = 1:3, widths = c(40, 15, 10))

# Save workbook
saveWorkbook(wb, output_file, overwrite = TRUE)

cat(sprintf("\n✅ Excel file saved to: %s\n", output_file))
cat(sprintf("\nSummary:\n"))
cat(sprintf("  - Total paragraphs with implements: %d\n", nrow(implements_data)))
cat(sprintf("  - Unique Acts: %d\n", length(unique(implements_data$act_name))))
cat(sprintf("  - Unique implement types found: %d\n", nrow(summary_by_implement)))
cat("\nTop 5 Implement Types:\n")
print(head(summary_by_implement, 5))

## Notify Completion ----
beep(sound = 1)
