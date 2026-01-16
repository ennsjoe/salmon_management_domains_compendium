################################################################################
# Title: Compendium Checker Export (Expanded Version)
# Author: Copilot (Modified)
# Date: 2025-08-27
# Last Updated: 2025-01-XX
# Description:
#   Queries the legislation database and exports an Excel file containing
#   paragraph-level metadata, semantic labels, scope values, and matched keywords.
#   Modified to aggregate paragraphs by section. Management Domain/IUCN/Scope/Agency
#   values are concatenated with "; " when multiple values exist for a section.
#   Handles Excel's 32,767 character limit by chunking.
#   Governance-based management domains have blank IUCN values.
#   Separates keywords into management_domain_keywords and clause_type_keywords.
#
# ENHANCEMENTS (v2):
#   1. Includes ALL paragraphs, even those with no assigned management domain
#   2. Adds agency column (joined to act_name via agencies.csv) - multiple agencies concatenated with "; "
#   3. Adds URL column (joined to legislation_name via legislation_url.csv)
#   4. Adds actionable_type, responsible_official, and discretion_type columns
#      (extracted from Paragraph text using actionable clause keywords)
################################################################################

## Load Libraries ----
library(DBI)
library(RSQLite)
library(data.table)
library(here)
library(openxlsx)
library(beepr)
library(stringr)

## Connect to Database ----
db_path <- file.path(here("output"), "legislation.db")
conn <- dbConnect(SQLite(), dbname = db_path)

## Load Tables ----
tryCatch({
  legislation_table <- as.data.table(dbReadTable(conn, "LegislationMetadata"))
  paragraph_table <- as.data.table(dbReadTable(conn, "LegislationParagraphs"))
  paragraph_label_table <- as.data.table(dbReadTable(conn, "paragraph_label_table"))
  
  # Load management domain threat table if it exists
  if("management_domain_threat_table" %in% dbListTables(conn)) {
    management_domain_threat <- as.data.table(dbReadTable(conn, "management_domain_threat_table"))
    cat("Ã¢Å“â€¦ Loaded management_domain_threat_table from database\n")
  } else {
    management_domain_threat <- NULL
    cat("Ã¢Å¡Â Ã¯Â¸Â  management_domain_threat_table not found in database\n")
  }
  
  # Load actionable clause keywords
  if("actionable_clause_keywords" %in% dbListTables(conn)) {
    actionable_keywords <- as.data.table(dbReadTable(conn, "actionable_clause_keywords"))
    cat("Ã¢Å“â€¦ Loaded actionable_clause_keywords from database\n")
  } else {
    actionable_keywords <- fread(file.path(here(), "actionable_clause_keywords.csv"))
    cat("Ã¢Å“â€¦ Loaded actionable_clause_keywords from CSV\n")
  }
  
}, finally = {
  dbDisconnect(conn)
  cat("Ã¢Å“â€¦ Database connection closed.\n")
})

## Load Reference Tables from Database ----

# Reconnect to database to load reference tables
conn <- dbConnect(SQLite(), dbname = db_path)

# Load agencies table (join on act_name) - Many-to-many relationship allowed
if("agencies" %in% dbListTables(conn)) {
  agencies_table <- as.data.table(dbReadTable(conn, "agencies"))
  cat("=== AGENCIES TABLE (from database) ===\n")
  cat("Columns:", paste(names(agencies_table), collapse = ", "), "\n")
  cat("Total records:", nrow(agencies_table), "\n")
  if(nrow(agencies_table) > 0) {
    # Trim whitespace from join columns
    agencies_table[, act_name := trimws(act_name)]
    agencies_table[, agency := trimws(agency)]
    cat("First 5 act_names:\n")
    print(head(agencies_table$act_name, 5))
  }
} else {
  agencies_table <- data.table(act_name = character(), agency = character())
  cat("WARNING: agencies table not found in database\n")
}

# Load legislation_url table (join on legislation_name) - One-to-one relationship
if("legislation_url" %in% dbListTables(conn)) {
  legislation_url_table <- as.data.table(dbReadTable(conn, "legislation_url"))
  cat("\n=== LEGISLATION URL TABLE (from database) ===\n")
  cat("Columns:", paste(names(legislation_url_table), collapse = ", "), "\n")
  cat("Total records:", nrow(legislation_url_table), "\n")
  if(nrow(legislation_url_table) > 0) {
    # Trim whitespace from join columns
    legislation_url_table[, legislation_name := trimws(legislation_name)]
    legislation_url_table[, url := trimws(url)]
    cat("URLs with values:", sum(!is.na(legislation_url_table$url) & legislation_url_table$url != ""), "\n")
    cat("First 5 legislation_names:\n")
    print(head(legislation_url_table$legislation_name, 5))
  }
} else {
  legislation_url_table <- data.table(legislation_name = character(), url = character())
  cat("WARNING: legislation_url table not found in database\n")
}

dbDisconnect(conn)
cat("\nDatabase connection closed after loading reference tables.\n")


## ============================================================================
## ACTIONABLE CLAUSE EXTRACTION FUNCTIONS
## ============================================================================

## Define actionable patterns (same as 05_actionable_clauses.R) ----
actionable_patterns <- list(
  regulation = "\\b(regulation|regulations|regulatory)\\b",
  order = "\\b(order|orders)\\b(?!\\s+in\\s+council)",
  order_in_council = "\\b(order in council|orders in council|lieutenant governor in council|governor in council)\\b",
  bylaw = "\\b(bylaw|by-law|bylaws|by-laws)\\b",
  authorization = "\\b(authori[sz]ation|authori[sz]ations|authorize|authori[sz]ed|authori[sz]ing)\\b",
  plan = "\\b(plan|plans|planning)\\b(?!\\s+of)",
  strategy = "\\b(strateg(y|ies))\\b",
  program = "\\b(program|programme|programs|programmes)\\b",
  policy = "\\b(polic(y|ies))\\b",
  framework = "\\b(framework|frameworks)\\b",
  guideline = "\\b(guideline|guidelines)\\b",
  standard = "\\b(standard|standards)\\b",
  code = "\\b(code|codes)\\b(?!\\s+of)",
  designation = "\\b(designat(e|ion|ions|ed|ing)|designated area|designated areas)\\b",
  reserve = "\\b(reserve|reserves)\\b",
  sanctuary = "\\b(sanctuar(y|ies))\\b",
  park = "\\b(park|parks)\\b",
  protected_area = "\\b(protected area|conservation area|management area|special area)\\b",
  agreement = "\\b(agreement|agreements)\\b",
  permit = "\\b(permit|permits)\\b",
  licence = "\\b(licen[cs]e|licen[cs]es)\\b",
  approval = "\\b(approval|approvals)\\b",
  certificate = "\\b(certificate|certificates)\\b",
  exemption = "\\b(exemption|exemptions|exempt|exempted|exempting)\\b",
  notice = "\\b(notice|notices)\\b",
  report = "\\b(report|reports|reporting)\\b",
  assessment = "\\b(assessment|assessments)\\b",
  review = "\\b(review|reviews)\\b",
  study = "\\b(stud(y|ies))\\b"
)

## Function: Extract actionable types ----
extract_actionable_types <- function(paragraph_text) {
  if (is.na(paragraph_text) || paragraph_text == "") return(NA_character_)
  
  text_lower <- tolower(paragraph_text)
  actionables_found <- character()
  
  for (impl_name in names(actionable_patterns)) {
    pattern <- actionable_patterns[[impl_name]]
    if (grepl(pattern, text_lower, perl = TRUE)) {
      actionables_found <- c(actionables_found, impl_name)
    }
  }
  
  if (length(actionables_found) > 0) {
    return(paste(actionables_found, collapse = "; "))
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

## Prepare keyword lists for extraction ----
official_keywords <- actionable_keywords[
  actionable_clause_category == "responsible official",
  unique(keyword)
]

discretionary_keywords_dt <- actionable_keywords[
  actionable_clause_category == "discretionary language",
  .(keyword, type)
]

cat("Loaded", length(official_keywords), "official keywords and", 
    nrow(discretionary_keywords_dt), "discretionary language keywords\n")

## ============================================================================
## MAIN PROCESSING
## ============================================================================

## Identify Governance-Only Management Domains ----
if(!is.null(management_domain_threat)) {
  threat_based_domains <- unique(management_domain_threat$management_domain)
  all_mgmt_domains <- unique(paragraph_label_table[
    label_type == "Management Domain" & !is.na(label_value),
    label_value
  ])
  governance_only_domains <- setdiff(all_mgmt_domains, threat_based_domains)
  
  cat("Governance-only domains:", paste(governance_only_domains, collapse = ", "), "\n")
  cat("Threat-based domains:", paste(threat_based_domains, collapse = ", "), "\n")
} else {
  governance_only_domains <- character(0)
  cat("Ã¢Å¡Â Ã¯Â¸Â  Cannot identify governance domains without management_domain_threat_table\n")
}

## Merge Paragraphs with Legislation Metadata ----
paragraphs_with_legislation <- merge(
  paragraph_table[, .(paragraph_id, legislation_id, Section, Heading, Paragraph)],
  legislation_table[, .(legislation_id, jurisdiction, act_name, legislation_name)],
  by = "legislation_id",
  all.x = TRUE
)

# Trim whitespace from join columns to ensure clean matches
paragraphs_with_legislation[, act_name := trimws(act_name)]
paragraphs_with_legislation[, legislation_name := trimws(legislation_name)]

## Add Agency column (joined on act_name) ----
cat("\n=== AGENCY JOIN DIAGNOSTICS ===\n")
cat("Unique act_names in paragraphs:", length(unique(paragraphs_with_legislation$act_name)), "\n")
cat("Unique act_names in agencies:", length(unique(agencies_table$act_name)), "\n")

# Print first 5 from each for visual comparison
cat("\nFirst 5 act_names in PARAGRAPHS (with quotes to show exact values):\n")
for(i in 1:min(5, length(unique(paragraphs_with_legislation$act_name)))) {
  val <- unique(paragraphs_with_legislation$act_name)[i]
  cat(sprintf("  [%d] '%s' (nchar=%d)\n", i, val, nchar(val)))
}

cat("\nFirst 5 act_names in AGENCIES (with quotes to show exact values):\n")
for(i in 1:min(5, length(unique(agencies_table$act_name)))) {
  val <- unique(agencies_table$act_name)[i]
  cat(sprintf("  [%d] '%s' (nchar=%d)\n", i, val, nchar(val)))
}

# Check for matches
matching_acts <- intersect(unique(paragraphs_with_legislation$act_name), unique(agencies_table$act_name))
cat("\nMatching act_names between tables:", length(matching_acts), "\n")
if(length(matching_acts) > 0) {
  cat("First few matches:", paste(head(matching_acts, 3), collapse = ", "), "\n")
} else {
  cat("WARNING: No matches found! Check for encoding or formatting differences.\n")
  # Try to find near-matches
  para_acts <- unique(paragraphs_with_legislation$act_name)
  agency_acts <- unique(agencies_table$act_name)
  for(pa in head(para_acts, 3)) {
    for(aa in head(agency_acts, 3)) {
      if(tolower(trimws(pa)) == tolower(trimws(aa))) {
        cat(sprintf("  Potential case mismatch: '%s' vs '%s'\n", pa, aa))
      }
    }
  }
}

paragraphs_with_legislation <- merge(
  paragraphs_with_legislation,
  agencies_table[, .(act_name, agency)],
  by = "act_name",
  all.x = TRUE,
  allow.cartesian = TRUE  # One act can have multiple agencies
)

cat("Paragraphs with agency after join:", sum(!is.na(paragraphs_with_legislation$agency)), "\n")
cat("Total rows after agency join:", nrow(paragraphs_with_legislation), "\n")

## Add URL column (joined on legislation_name) ----
cat("\n=== URL JOIN DIAGNOSTICS ===\n")
cat("Unique legislation_names in paragraphs:", length(unique(paragraphs_with_legislation$legislation_name)), "\n")
cat("Unique legislation_names in URL table:", length(unique(legislation_url_table$legislation_name)), "\n")

# Check for matches
matching_leg <- intersect(unique(paragraphs_with_legislation$legislation_name), unique(legislation_url_table$legislation_name))
cat("Matching legislation_names:", length(matching_leg), "\n")
if(length(matching_leg) > 0) {
  cat("First few matches:", paste(head(matching_leg, 3), collapse = ", "), "\n")
} else {
  cat("WARNING: No URL matches found!\n")
  cat("First 3 legislation_names in paragraphs:\n")
  print(head(unique(paragraphs_with_legislation$legislation_name), 3))
  cat("First 3 legislation_names in URL table:\n")
  print(head(unique(legislation_url_table$legislation_name), 3))
}

paragraphs_with_legislation <- merge(
  paragraphs_with_legislation,
  legislation_url_table[, .(legislation_name, url)],
  by = "legislation_name",
  all.x = TRUE
)

cat("Paragraphs with URL after join:", sum(!is.na(paragraphs_with_legislation$url) & paragraphs_with_legislation$url != ""), "\n")
cat("Total paragraphs after joining with metadata:", nrow(paragraphs_with_legislation), "\n")

## Extract Actionable Clause Information at Paragraph Level ----
cat("Extracting actionable types from paragraphs...\n")

# Vectorized extraction - much faster than sapply
extract_actionable_types_vectorized <- function(texts) {
  results <- rep(NA_character_, length(texts))
  texts_lower <- tolower(texts)
  
  for (i in seq_along(texts)) {
    if (is.na(texts[i]) || texts[i] == "") next
    
    actionables_found <- character()
    for (impl_name in names(actionable_patterns)) {
      pattern <- actionable_patterns[[impl_name]]
      if (grepl(pattern, texts_lower[i], perl = TRUE)) {
        actionables_found <- c(actionables_found, impl_name)
      }
    }
    
    if (length(actionables_found) > 0) {
      results[i] <- paste(actionables_found, collapse = "; ")
    }
  }
  return(results)
}

# Process in batches to show progress
batch_size <- 10000
n_rows <- nrow(paragraphs_with_legislation)
n_batches <- ceiling(n_rows / batch_size)

paragraphs_with_legislation[, actionable_type := NA_character_]

for (b in seq_len(n_batches)) {
  start_idx <- (b - 1) * batch_size + 1
  end_idx <- min(b * batch_size, n_rows)
  
  paragraphs_with_legislation[start_idx:end_idx, 
                              actionable_type := extract_actionable_types_vectorized(Paragraph)]
  
  cat(sprintf("\r  Actionable types: batch %d/%d (rows %d-%d)", b, n_batches, start_idx, end_idx))
}
cat("\n")

cat("Extracting responsible officials from paragraphs...\n")

# Vectorized extraction for officials
extract_officials_vectorized <- function(texts, official_kws) {
  results <- rep(NA_character_, length(texts))
  
  for (i in seq_along(texts)) {
    if (is.na(texts[i]) || texts[i] == "") next
    
    officials_found <- character()
    for (kw in official_kws) {
      pattern <- paste0("\\b", kw, "\\b")
      if (grepl(pattern, texts[i], ignore.case = TRUE)) {
        officials_found <- c(officials_found, kw)
      }
    }
    
    if (length(officials_found) > 0) {
      results[i] <- paste(unique(officials_found), collapse = "; ")
    }
  }
  return(results)
}

paragraphs_with_legislation[, responsible_official := NA_character_]

for (b in seq_len(n_batches)) {
  start_idx <- (b - 1) * batch_size + 1
  end_idx <- min(b * batch_size, n_rows)
  
  paragraphs_with_legislation[start_idx:end_idx, 
                              responsible_official := extract_officials_vectorized(Paragraph, official_keywords)]
  
  cat(sprintf("\r  Responsible officials: batch %d/%d (rows %d-%d)", b, n_batches, start_idx, end_idx))
}
cat("\n")

cat("Extracting discretion types from paragraphs...\n")

# Vectorized extraction for discretion types
extract_discretion_vectorized <- function(texts, disc_keywords_dt) {
  results <- rep(NA_character_, length(texts))
  
  for (i in seq_along(texts)) {
    if (is.na(texts[i]) || texts[i] == "") next
    
    text_lower <- tolower(texts[i])
    discretions_found <- character()
    
    for (j in seq_len(nrow(disc_keywords_dt))) {
      kw <- disc_keywords_dt$keyword[j]
      dtype <- disc_keywords_dt$type[j]
      pattern <- paste0("\\b", tolower(kw), "\\b")
      
      if (grepl(pattern, text_lower, perl = TRUE)) {
        discretions_found <- c(discretions_found, dtype)
      }
    }
    
    if (length(discretions_found) > 0) {
      results[i] <- paste(unique(discretions_found), collapse = "; ")
    }
  }
  return(results)
}

paragraphs_with_legislation[, discretion_type := NA_character_]

for (b in seq_len(n_batches)) {
  start_idx <- (b - 1) * batch_size + 1
  end_idx <- min(b * batch_size, n_rows)
  
  paragraphs_with_legislation[start_idx:end_idx, 
                              discretion_type := extract_discretion_vectorized(Paragraph, discretionary_keywords_dt)]
  
  cat(sprintf("\r  Discretion types: batch %d/%d (rows %d-%d)", b, n_batches, start_idx, end_idx))
}
cat("\n")

## Merge with Labels (all.x = TRUE to keep ALL paragraphs) ----
paragraphs_with_labels <- merge(
  paragraphs_with_legislation,
  paragraph_label_table[
    label_type %in% c("Management Domain", "IUCN", "Clause Type") & !is.na(label_value),
    .(paragraph_id, label_type, label_value)
  ],
  by = "paragraph_id",
  all.x = TRUE,  # Keep ALL paragraphs, even those without labels
  allow.cartesian = TRUE
)

## Extract Scope (will be split into separate rows later) ----
scope_labels <- paragraph_label_table[
  !is.na(scope),
  .(paragraph_id, scope)
]

## Extract and Aggregate Keywords by Type ----
# Management Domain Keywords
mgmt_domain_keyword_labels <- paragraph_label_table[
  label_type == "Management Domain" & !is.na(keyword),
  .(paragraph_id, keyword)
]
mgmt_domain_keyword_labels <- mgmt_domain_keyword_labels[, 
                                                         .(management_domain_keywords = paste(unique(keyword), collapse = "; ")), 
                                                         by = paragraph_id
]

# Clause Type Keywords
clause_type_keyword_labels <- paragraph_label_table[
  label_type == "Clause Type" & !is.na(keyword),
  .(paragraph_id, keyword)
]
clause_type_keyword_labels <- clause_type_keyword_labels[, 
                                                         .(clause_type_keywords = paste(unique(keyword), collapse = "; ")), 
                                                         by = paragraph_id
]

## Merge keywords to paragraph data ----
paragraphs_with_meta <- merge(paragraphs_with_legislation, mgmt_domain_keyword_labels, by = "paragraph_id", all.x = TRUE)
paragraphs_with_meta <- merge(paragraphs_with_meta, clause_type_keyword_labels, by = "paragraph_id", all.x = TRUE)

## Aggregate keywords by section ----
meta_aggregated <- paragraphs_with_meta[, .(
  management_domain_keywords = paste(unique(na.omit(management_domain_keywords)), collapse = "; "),
  clause_type_keywords = paste(unique(na.omit(clause_type_keywords)), collapse = "; ")
), by = .(jurisdiction, act_name, legislation_name, Section, Heading)]

# Replace empty strings with NA
meta_aggregated[management_domain_keywords == "", management_domain_keywords := NA]
meta_aggregated[clause_type_keywords == "", clause_type_keywords := NA]

## Function to chunk paragraphs if they exceed Excel's limit ----
chunk_paragraphs <- function(paragraphs, max_chars = 30000) {
  if (sum(nchar(paragraphs)) + length(paragraphs) * 2 <= max_chars) {
    return(list(paste(paragraphs, collapse = "\n\n")))
  }
  
  chunks <- list()
  current_chunk <- character(0)
  current_length <- 0
  
  for (p in paragraphs) {
    p_length <- nchar(p)
    if (current_length + p_length + 2 > max_chars && length(current_chunk) > 0) {
      chunks[[length(chunks) + 1]] <- paste(current_chunk, collapse = "\n\n")
      current_chunk <- character(0)
      current_length <- 0
    }
    current_chunk <- c(current_chunk, p)
    current_length <- current_length + p_length + 2
  }
  
  if (length(current_chunk) > 0) {
    chunks[[length(chunks) + 1]] <- paste(current_chunk, collapse = "\n\n")
  }
  
  return(chunks)
}

## Step 1: Aggregate Paragraphs by Section with Chunking ----
# Get all unique labels per section (including scope)
section_labels <- unique(paragraphs_with_labels[, .(
  jurisdiction, act_name, legislation_name, Section, Heading, label_type, label_value
)])

# Get unique scope values per section - concatenate multiple scopes into one cell
section_scope <- unique(scope_labels[paragraph_id %in% paragraphs_with_legislation$paragraph_id, .(
  paragraph_id, scope
)])
section_scope <- merge(
  section_scope,
  paragraphs_with_legislation[, .(paragraph_id, jurisdiction, act_name, legislation_name, Section, Heading)],
  by = "paragraph_id"
)
section_scope <- section_scope[, 
                               .(scope = paste(unique(scope), collapse = "; ")),
                               by = .(jurisdiction, act_name, legislation_name, Section, Heading)
]

# Aggregate actionable clause info by section (take unique values)
# Helper function to get first non-NA value safely
first_non_na <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if(length(x) > 0) return(x[1]) else return(NA_character_)
}

# Aggregate actionable fields and URL (not agency - agency creates duplicate rows)
actionable_aggregated <- paragraphs_with_legislation[, .(
  actionable_type = paste(unique(na.omit(actionable_type)), collapse = "; "),
  responsible_official = paste(unique(na.omit(responsible_official)), collapse = "; "),
  discretion_type = paste(unique(na.omit(discretion_type)), collapse = "; "),
  url = first_non_na(url)
), by = .(jurisdiction, act_name, legislation_name, Section, Heading)]

# Aggregate agencies per section - concatenate multiple agencies into one cell
section_agencies <- paragraphs_with_legislation[
  !is.na(agency) & agency != "",
  .(agency = paste(unique(agency), collapse = "; ")),
  by = .(jurisdiction, act_name, legislation_name, Section, Heading)
]

# Debug: Check agency values
cat("Sections with agencies:", nrow(section_agencies), "\n")
cat("Sample agencies:", paste(head(unique(section_agencies$agency), 5), collapse = " | "), "\n")

# Replace empty strings with NA in actionable_aggregated
actionable_aggregated[actionable_type == "", actionable_type := NA]
actionable_aggregated[responsible_official == "", responsible_official := NA]
actionable_aggregated[discretion_type == "", discretion_type := NA]
actionable_aggregated[is.na(url) | url == "", url := NA_character_]

# Aggregate paragraphs with chunking
paragraphs_aggregated <- paragraphs_with_legislation[, {
  chunks <- chunk_paragraphs(unique(Paragraph))
  list(
    Paragraph = chunks,
    chunk_id = seq_along(chunks),
    total_chunks = length(chunks)
  )
}, by = .(jurisdiction, act_name, legislation_name, Section, Heading)]

# Convert list columns to regular columns
paragraphs_aggregated <- paragraphs_aggregated[, .(
  Paragraph = unlist(Paragraph),
  chunk_id = unlist(chunk_id),
  total_chunks = unlist(total_chunks)
), by = .(jurisdiction, act_name, legislation_name, Section, Heading)]

## Step 2: Merge Labels Back (using left join to keep ALL sections) ----
paragraphs_with_all_labels <- merge(
  paragraphs_aggregated,
  section_labels,
  by = c("jurisdiction", "act_name", "legislation_name", "Section", "Heading"),
  all.x = TRUE,  # Keep ALL paragraphs
  allow.cartesian = TRUE
)

## Step 3: Reshape Labels ----
# Separate Clause Type (will be aggregated)
clause_type_labels <- paragraphs_with_all_labels[
  label_type == "Clause Type" & !is.na(label_value),
  .(Clause_Type = paste(unique(label_value), collapse = "; ")),
  by = .(jurisdiction, act_name, legislation_name, Section, Heading, chunk_id)
]

# Keep Management Domain and IUCN separate (one row per combination)
mgmt_iucn_labels <- unique(paragraphs_with_all_labels[
  label_type %in% c("Management Domain", "IUCN") & !is.na(label_value),
  .(jurisdiction, act_name, legislation_name, Section, Heading, Paragraph, chunk_id, total_chunks, label_type, label_value)
])

# Reshape to wide format if we have Management Domain/IUCN labels
if(nrow(mgmt_iucn_labels) > 0) {
  mgmt_iucn_wide <- dcast(
    mgmt_iucn_labels,
    jurisdiction + act_name + legislation_name + Section + Heading + Paragraph + chunk_id + total_chunks ~ label_type,
    value.var = "label_value",
    fun.aggregate = function(x) if(length(x) > 0) x[1] else NA_character_
  )
  
  # CRITICAL: Set IUCN to NA for governance-only management domains
  if("Management Domain" %in% names(mgmt_iucn_wide) && "IUCN" %in% names(mgmt_iucn_wide) && length(governance_only_domains) > 0) {
    rows_to_clear <- mgmt_iucn_wide$`Management Domain` %in% governance_only_domains
    mgmt_iucn_wide[rows_to_clear, IUCN := NA_character_]
    cat("Ã¢Å“â€¦ Set IUCN to NA for", sum(rows_to_clear), "governance-only rows\n")
  }
  
  compendium_data <- copy(mgmt_iucn_wide)
} else {
  compendium_data <- unique(paragraphs_aggregated)
  compendium_data[, `Management Domain` := NA_character_]
  compendium_data[, IUCN := NA_character_]
}

# For paragraphs without any Management Domain/IUCN labels, add them back
# Get sections that exist in paragraphs_aggregated but not in compendium_data
all_sections <- unique(paragraphs_aggregated[, .(jurisdiction, act_name, legislation_name, Section, Heading, Paragraph, chunk_id, total_chunks)])
existing_sections <- unique(compendium_data[, .(jurisdiction, act_name, legislation_name, Section, Heading, chunk_id)])

# Find missing sections (those without Management Domain/IUCN labels)
missing_sections <- merge(
  all_sections,
  existing_sections,
  by = c("jurisdiction", "act_name", "legislation_name", "Section", "Heading", "chunk_id"),
  all.x = TRUE
)

# Add indicator for which rows are missing
setnames(missing_sections, old = names(missing_sections), new = names(missing_sections))
missing_only <- all_sections[!existing_sections, on = c("jurisdiction", "act_name", "legislation_name", "Section", "Heading", "chunk_id")]

if(nrow(missing_only) > 0) {
  missing_only[, `Management Domain` := NA_character_]
  missing_only[, IUCN := NA_character_]
  compendium_data <- rbindlist(list(compendium_data, missing_only), fill = TRUE)
  cat("Ã¢Å“â€¦ Added", nrow(missing_only), "sections without Management Domain/IUCN labels\n")
}

# Merge with Clause Type
compendium_data <- merge(
  compendium_data,
  clause_type_labels,
  by = c("jurisdiction", "act_name", "legislation_name", "Section", "Heading", "chunk_id"),
  all.x = TRUE
)

# Merge with scope (now concatenated into one cell per section)
compendium_data <- merge(
  compendium_data,
  section_scope,
  by = c("jurisdiction", "act_name", "legislation_name", "Section", "Heading"),
  all.x = TRUE
)

# Merge with aggregated keywords
compendium_data <- merge(
  compendium_data,
  meta_aggregated,
  by = c("jurisdiction", "act_name", "legislation_name", "Section", "Heading"),
  all.x = TRUE
)

# Merge with actionable clause info (url included, agency separate)
compendium_data <- merge(
  compendium_data,
  actionable_aggregated,
  by = c("jurisdiction", "act_name", "legislation_name", "Section", "Heading"),
  all.x = TRUE
)

# Merge with agencies - now one row per section with concatenated agencies
compendium_data <- merge(
  compendium_data,
  section_agencies,
  by = c("jurisdiction", "act_name", "legislation_name", "Section", "Heading"),
  all.x = TRUE
)

cat("Rows after agency join:", nrow(compendium_data), "\n")
cat("Rows with agency:", sum(!is.na(compendium_data$agency)), "\n")

# Update Section column in place (don't create new column)
compendium_data[, Section := ifelse(
  total_chunks > 1,
  paste0(Section, " (Part ", chunk_id, " of ", total_chunks, ")"),
  Section
)]

# Remove helper columns
compendium_data[, c("chunk_id", "total_chunks") := NULL]

# Rename columns to final desired names (lowercase with underscores)
setnames(compendium_data, old = c("Section", "Heading", "Paragraph", "Management Domain", "IUCN", "Clause_Type", "scope"),
         new = c("section", "heading", "aggregate_paragraph", "management_domain", "iucn_threat", "clause_type", "scope"),
         skip_absent = TRUE)

# Ensure all required columns exist
required_cols <- c("jurisdiction", "agency", "url", "management_domain", "iucn_threat", 
                   "clause_type", "scope", "management_domain_keywords", "clause_type_keywords",
                   "actionable_type", "responsible_official", "discretion_type")
for(col in required_cols) {
  if(!col %in% names(compendium_data)) {
    compendium_data[, (col) := NA_character_]
  }
}

# Reorder columns
desired_order <- c("jurisdiction", "agency", "act_name", "legislation_name", "url",
                   "section", "heading", "aggregate_paragraph",
                   "management_domain", "iucn_threat", "clause_type", "scope", 
                   "management_domain_keywords", "clause_type_keywords",
                   "actionable_type", "responsible_official", "discretion_type")
existing_order <- intersect(desired_order, names(compendium_data))
setcolorder(compendium_data, existing_order)

cat("Ã¢Å“â€¦ Final columns:", paste(names(compendium_data), collapse = ", "), "\n")

## Sort by jurisdiction, legislation and section ----
setorder(compendium_data, jurisdiction, act_name, legislation_name, section)

## Export to Excel in Output Directory ----
output_file <- file.path(here("output"), "LAPSE_full_compendium.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Compendium")

# Write data with default formatting
writeDataTable(wb, "Compendium", compendium_data)

# Auto-size columns for better readability (with max width)
setColWidths(wb, "Compendium", cols = 1:ncol(compendium_data), widths = "auto")

saveWorkbook(wb, output_file, overwrite = TRUE)
cat("Ã¢Å“â€¦ Excel file 'LAPSE_full_compendium.xlsx' has been saved to the output directory.\n")
cat(sprintf("   Total rows: %d\n", nrow(compendium_data)))

## ============================================================================
## EXPORT JSON VERSION OF THE COMPENDIUM
## ============================================================================

library(jsonlite)

json_output_file <- file.path(here("output"), "LAPSE_compendium.json")

# Convert data.table to a regular list for clean JSON structure
compendium_list <- as.list(compendium_data)

# Write JSON (pretty = TRUE for readability)
write_json(
  compendium_data,
  path = json_output_file,
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

cat("Ã¢Å“â€¦ JSON file 'LAPSE_compendium.json' has been saved to the output directory.\n")
################################################################################

# Summary statistics
cat("\n--- Summary Statistics ---\n")
cat(sprintf("   Paragraphs with Management Domain: %d\n", sum(!is.na(compendium_data$management_domain))))
cat(sprintf("   Paragraphs without Management Domain: %d\n", sum(is.na(compendium_data$management_domain))))
cat(sprintf("   Paragraphs with Actionable Type: %d\n", sum(!is.na(compendium_data$actionable_type))))
cat(sprintf("   Paragraphs with Responsible Official: %d\n", sum(!is.na(compendium_data$responsible_official))))
cat(sprintf("   Paragraphs with Discretion Type: %d\n", sum(!is.na(compendium_data$discretion_type))))
cat(sprintf("   Unique agencies: %d\n", length(unique(na.omit(compendium_data$agency)))))
cat(sprintf("   Paragraphs with URLs: %d\n", sum(!is.na(compendium_data$url) & compendium_data$url != "")))

# Check for any remaining character limit issues
char_counts <- nchar(compendium_data$aggregate_paragraph)
if(any(char_counts > 32767, na.rm = TRUE)) {
  cat("Ã¢Å¡Â Ã¯Â¸Â  Warning: Some cells still exceed Excel's limit. Consider further chunking.\n")
} else {
  cat("Ã¢Å“â€¦ All cells are within Excel's character limit.\n")
}

# Beep when done
beep(sound = 1)