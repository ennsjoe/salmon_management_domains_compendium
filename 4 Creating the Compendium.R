#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# PART 4 - BUILDING THE COMPENDIUM----------------------------------------------
# 
# Summary: this script filters the full legislation dataset based on keywords,
#
# Output: An Excel file containing the filtered legislation with assigned management 
# domains and clause types
#///////////////////////////////////////////////////////////////////////////////

library(data.table)
library(here)
library(writexl)

# Define file paths
full_legislation_path <- here("Full_legislation_parsed_DT.rds")
management_domain_path <- here("management_domain_selection.RData")  
mgmt_d_1_path <- here("mgmt_d_1.RData")  
keyword_selection_path <- here("keyword_selection.RData")
exclusion_keyword_selection_path <- here("exclusion_keyword_selection.RData")
clause_type_selection_path <- here("clause_type_selection.RData")

# Function to trim long strings while handling NA values
trim_string <- function(x, max_length = 100) {
  sapply(x, function(str) {
    if (is.na(str)) {
      return(NA)  # Preserve NA values
    } else if (nchar(str) > max_length) {
      return(substr(str, 1, max_length - 3))  # Trim string
    } else {
      return(str)
    }
  }, USE.NAMES = FALSE)
}

# Load datasets
load_object <- function(path, object_name) {
  if (file.exists(path)) {
    load(path, envir = .GlobalEnv)
    print(paste("Loaded:", object_name))
  } else {
    stop(paste("Error:", object_name, "not found."))
  }
}

# Load legislation dataset
if (file.exists(full_legislation_path)) {
  full_legislation_parsed_DT <- readRDS(full_legislation_path)
  print("Loaded: Full_legislation_parsed_DT.rds")
} else {
  stop("Error: Full_legislation_parsed_DT.rds not found.")
}

# Load all necessary objects
load_object(mgmt_d_1_path, "mgmt_d_1.RData")
load_object(keyword_selection_path, "keyword_selection.RData")
load_object(exclusion_keyword_selection_path, "exclusion_keyword_selection.RData")
load_object(clause_type_selection_path, "clause_type_selection.RData")

# Load management domain selection, ensuring md_selection_dt exists
if (file.exists(management_domain_path)) {
  load(management_domain_path)  
  if (!exists("md_selection_dt")) {
    print("Warning: md_selection_dt not found. Initializing empty selection data.")
    md_selection_dt <- data.table(Keyword = character(), L1 = character(), L2 = character(), `Management Domain` = character())
  } else {
    print("Loaded: management_domain_selection.RData")
  }
} else {
  print("Warning: management_domain_selection.RData not found. Initializing empty dataset.")
  md_selection_dt <- data.table(Keyword = character(), L1 = character(), L2 = character(), `Management Domain` = character())
}

# Ensure required datasets are loaded
required_objects <- c("full_legislation_parsed_DT", "included_keywords_dt", "exclusion_keywords_dt", "md_selection_dt", "clause_selection_dt", "mgmt_d_keywords")
missing_objects <- setdiff(required_objects, ls())

if (length(missing_objects) > 0) {
  stop(paste("Error: Required datasets not found:", paste(missing_objects, collapse=", ")))
}

# Convert Keyword and Paragraph columns to lowercase, then trim long strings safely
included_keywords_dt[, Keyword := trim_string(trimws(tolower(Keyword)))]
exclusion_keywords_dt[, Keyword := trim_string(trimws(tolower(Keyword)))]
full_legislation_parsed_DT[, Paragraph := trim_string(trimws(tolower(Paragraph)), max_length = 5000)]
md_selection_dt[, Keyword := trim_string(trimws(tolower(Keyword)))]
md_selection_dt[, `Management Domain` := trim_string(`Management Domain`, max_length = 100)]
md_selection_dt[, L1 := trim_string(L1, max_length = 100)]
md_selection_dt[, L2 := trim_string(L2, max_length = 100)]
mgmt_d_keywords[, Keyword := trim_string(trimws(tolower(Keyword)))]
clause_selection_dt[, Keyword := trim_string(trimws(tolower(Keyword)))]
clause_selection_dt[, Clause_Type := trim_string(Clause_Type, max_length = 100)]

### Step 1: Initialize `filtered_legislation_dt`
full_legislation_parsed_DT[, Specificity := NA_integer_]
filtered_legislation_dt <- copy(full_legislation_parsed_DT)

### Step 2: Filter rows using `included_keywords_dt`
setorder(included_keywords_dt, Specificity)  

for (spec_level in unique(included_keywords_dt$Specificity)) {
  keyword_subset <- included_keywords_dt[Specificity == spec_level, Keyword]
  keyword_pattern <- paste0("\\b(", paste(keyword_subset, collapse = "|"), ")\\b")
  
  filtered_legislation_dt[
    grepl(keyword_pattern, Paragraph, ignore.case = TRUE),
    Specificity := spec_level
  ]
}

filtered_legislation_dt <- filtered_legislation_dt[!is.na(Specificity)]  

### Step 3: Remove rows using `exclusion_keywords_dt`
exclusion_pattern <- paste0("\\b(", paste(exclusion_keywords_dt$Keyword, collapse = "|"), ")\\b")
filtered_legislation_dt <- filtered_legislation_dt[!grepl(exclusion_pattern, Paragraph, ignore.case = TRUE)]

### Step 4: Initialize columns
filtered_legislation_dt[, `:=`(L1 = NA_character_, L2 = NA_character_, `Management Domain` = NA_character_, Clause_Type = NA_character_)]

### Step 5: Assign values sequentially
for (spec_level in sort(unique(filtered_legislation_dt$Specificity), decreasing = FALSE)) {
  
  unassigned_rows <- filtered_legislation_dt[Specificity == spec_level]
  
  if (nrow(unassigned_rows) > 0) {
    unassigned_rows[, paragraph_words := lapply(Paragraph, function(text) {
      unlist(strsplit(tolower(text), "\\s+"))
    })]
    
    for (i in seq_len(nrow(unassigned_rows))) {
      paragraph_words <- unassigned_rows[i, paragraph_words][[1]]
      
      # Assign L1, L2, and Management Domain
      match_word <- paragraph_words[paragraph_words %in% md_selection_dt$Keyword][1]
      if (!is.na(match_word)) {
        first_domain_match <- md_selection_dt[Keyword == match_word, .(`L1`, `L2`, `Management Domain`)][1]
        
        filtered_legislation_dt[Paragraph == unassigned_rows[i, Paragraph] & is.na(L1), `:=`(
          L1 = trim_string(first_domain_match$L1), 
          L2 = trim_string(first_domain_match$L2)
        )]
        filtered_legislation_dt[Paragraph == unassigned_rows[i, Paragraph] & is.na(`Management Domain`), `:=`(
          `Management Domain` = trim_string(first_domain_match$`Management Domain`)
        )]
      }
      
      # Assign Clause Type
      match_word_clause <- paragraph_words[paragraph_words %in% clause_selection_dt$Keyword][1]
      if (!is.na(match_word_clause)) {
        first_clause_match <- clause_selection_dt[Keyword == match_word_clause, Clause_Type][1]
        
        filtered_legislation_dt[Paragraph == unassigned_rows[i, Paragraph] & is.na(Clause_Type), `:=`(
          Clause_Type = trim_string(first_clause_match)
        )]
      }
    }
  }
}

# Define file path for export
output_file <- "Pacific Salmon Management Domain Compendium.xlsx"

# Export filtered_legislation_dt
write_xlsx(filtered_legislation_dt, path = output_file)

print(paste("Export successful:", output_file))

