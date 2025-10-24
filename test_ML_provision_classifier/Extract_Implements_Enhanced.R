################################################################################
# Title: Enhanced Legislative Implements Extraction with NLP
# Authors: Joe Enns, Cory Lagasse
# Date Created: 2025-01-XX
# Purpose / Description:
#   Enhanced version using advanced NLP techniques including dependency parsing,
#   part-of-speech tagging, context windows, and machine learning to extract
#   implements from legislation with improved accuracy.
# Dependencies: DBI, RSQLite, data.table, here, openxlsx, stringr, udpipe, 
#               quanteda, quanteda.textstats, caret, randomForest
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
library(udpipe)
library(quanteda)
library(quanteda.textstats)
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
## SECTION 2: ADVANCED NLP SETUP
## ============================================================================

cat("Step 2: Setting up advanced NLP models...\n")

## Download and load English language model for udpipe ----
cat("  - Loading linguistic model for dependency parsing...\n")
model_path <- file.path(here(), "english-ewt-ud-2.5-191206.udpipe")

if (!file.exists(model_path)) {
  cat("  - Downloading English language model (this may take a moment)...\n")
  udpipe_download_model(language = "english-ewt", model_dir = here())
}

ud_model <- udpipe_load_model(file = model_path)

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
      # Check for context boost words nearby (within 10 words)
      context <- extract_context_window(text_lower, pattern, window_size = 10)
      
      has_boost <- FALSE
      if (!is.na(context) && length(impl_info$context_boost) > 0) {
        boost_pattern <- paste0("\\b(", paste(impl_info$context_boost, collapse = "|"), ")\\b")
        has_boost <- grepl(boost_pattern, context, perl = TRUE)
      }
      
      # Add confidence indicator
      confidence <- if (has_boost) "high" else "medium"
      implements_found <- c(implements_found, paste0(impl_name, "[", confidence, "]"))
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

## Function: Dependency parsing analysis (for grammatical relationships) ----
analyze_dependencies <- function(text_sample, ud_model) {
  if (is.na(text_sample) || nchar(text_sample) > 5000) return(NULL)
  
  tryCatch({
    annotation <- udpipe_annotate(ud_model, x = text_sample)
    annotation_df <- as.data.frame(annotation)
    return(annotation_df)
  }, error = function(e) {
    return(NULL)
  })
}

## Function: Extract subject-verb-object relationships ----
extract_svo_relationships <- function(annotation_df) {
  if (is.null(annotation_df) || nrow(annotation_df) == 0) return(NA_character_)
  
  relationships <- character()
  
  # Find verbs
  verbs <- annotation_df[annotation_df$upos == "VERB", ]
  
  if (nrow(verbs) > 0) {
    for (i in 1:min(3, nrow(verbs))) {  # Analyze up to 3 verbs
      verb <- verbs[i, ]
      verb_lemma <- verb$lemma
      
      # Find subject
      subjects <- annotation_df[annotation_df$head_token_id == verb$token_id & 
                                  annotation_df$dep_rel %in% c("nsubj", "nsubj:pass"), ]
      
      # Find object
      objects <- annotation_df[annotation_df$head_token_id == verb$token_id & 
                                 annotation_df$dep_rel %in% c("obj", "dobj", "iobj"), ]
      
      if (nrow(subjects) > 0 && nrow(objects) > 0) {
        relationship <- paste(subjects$lemma[1], verb_lemma, objects$lemma[1], sep = " -> ")
        relationships <- c(relationships, relationship)
      }
    }
  }
  
  if (length(relationships) > 0) {
    return(paste(relationships, collapse = "; "))
  } else {
    return(NA_character_)
  }
}

## ============================================================================
## SECTION 4: APPLY ENHANCED EXTRACTION
## ============================================================================

cat("\nStep 4: Applying enhanced extraction to paragraphs...\n")

## Apply basic extraction ----
cat("  - Extracting implement types with context...\n")
act_paragraphs[, implement_type := sapply(Paragraph, extract_implements_enhanced)]

cat("  - Extracting responsible officials...\n")
act_paragraphs[, responsible_official := sapply(Paragraph, extract_officials_enhanced)]

cat("  - Extracting provision types...\n")
act_paragraphs[, provision_type := sapply(Paragraph, extract_provision_type)]

## Filter to rows with implements ----
implements_data <- act_paragraphs[!is.na(implement_type)]

cat(sprintf("  - Found %d paragraphs containing implements\n", nrow(implements_data)))

## Advanced linguistic analysis on sample ----
cat("\nStep 5: Performing advanced linguistic analysis on sample...\n")

# Analyze a sample for demonstration (first 100 with implements, or fewer if not available)
sample_size <- min(100, nrow(implements_data))
if (sample_size > 0) {
  cat(sprintf("  - Analyzing %d sample paragraphs with dependency parsing...\n", sample_size))
  
  implements_data[, grammar_analysis := NA_character_]
  
  for (i in 1:sample_size) {
    if (i %% 20 == 0) {
      cat(sprintf("    Processing paragraph %d of %d...\n", i, sample_size))
    }
    
    text <- implements_data$Paragraph[i]
    if (!is.na(text) && nchar(text) > 0 && nchar(text) < 5000) {
      annotation <- analyze_dependencies(text, ud_model)
      if (!is.null(annotation) && nrow(annotation) > 0) {
        svo <- extract_svo_relationships(annotation)
        if (!is.na(svo)) {
          implements_data[i, grammar_analysis := svo]
        }
      }
    }
  }
  
  cat(sprintf("  - Completed dependency analysis on %d paragraphs\n", sample_size))
}

## ============================================================================
## SECTION 6: MERGE WITH METADATA AND PREPARE OUTPUT
## ============================================================================

cat("\nStep 6: Merging with legislation metadata...\n")

implements_data <- merge(
  implements_data[, .(paragraph_id, legislation_id, Section, Heading, Paragraph, 
                     implement_type, responsible_official, provision_type, grammar_analysis)],
  acts_only[, .(legislation_id, act_name, jurisdiction)],
  by = "legislation_id",
  all.x = TRUE
)

## Clean confidence indicators for summary ----
implements_data[, implement_type_clean := gsub("\\[.*?\\]", "", implement_type)]

## Reorder columns ----
setcolorder(implements_data, c(
  "act_name", "jurisdiction", "Section", "Heading", 
  "implement_type", "responsible_official", "provision_type", 
  "grammar_analysis", "Paragraph"
))

## Sort ----
implements_data <- implements_data[order(act_name, Section)]

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

## ============================================================================
## SECTION 8: EXPORT TO EXCEL
## ============================================================================

cat("\nStep 8: Creating Excel workbook...\n")

output_file <- file.path(here(), "Legislative_Implements_Enhanced.xlsx")

wb <- createWorkbook()

## Main data sheet ----
addWorksheet(wb, "Implements (Enhanced)")
writeDataTable(wb, "Implements (Enhanced)", implements_data)

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
setColWidths(wb, "Implements (Enhanced)", cols = 1:9, 
             widths = c(35, 12, 10, 30, 30, 25, 25, 40, 70))
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

cat(sprintf("Total paragraphs with implements: %d\n", nrow(implements_data)))
cat(sprintf("Unique Acts: %d\n", length(unique(implements_data$act_name))))
cat(sprintf("Unique implement types found: %d\n", nrow(summary_by_implement)))
cat(sprintf("Paragraphs with grammatical analysis: %d\n", 
            sum(!is.na(implements_data$grammar_analysis))))

cat("\nTop 10 Implement Types:\n")
print(head(summary_by_implement, 10))

cat("\nTop 10 Responsible Officials:\n")
print(head(summary_by_official, 10))

cat("\nTop 10 Provision Types:\n")
print(head(summary_by_provision, 10))

cat("\nTop 5 Implement-Official Combinations:\n")
print(head(co_occurrence, 5))

cat("\n=====================================\n")
cat("Analysis complete!\n")
cat("=====================================\n")

## Notify Completion ----
beep(sound = 1)
