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
mgmt_d_1_path <- here("mgmt_d_1.RData")  # Reintroducing mgmt_d_1.RData
keyword_selection_path <- here("keyword_selection.RData")
exclusion_keyword_selection_path <- here("exclusion_keyword_selection.RData")
clause_type_selection_path <- here("clause_type_selection.RData")

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
load_object(management_domain_path, "management_domain_selection.RData")
load_object(mgmt_d_1_path, "mgmt_d_1.RData")  # Reintroduced dataset
load_object(keyword_selection_path, "keyword_selection.RData")
load_object(exclusion_keyword_selection_path, "exclusion_keyword_selection.RData")
load_object(clause_type_selection_path, "clause_type_selection.RData")

# Ensure required datasets are loaded
if (!exists("full_legislation_parsed_DT") || !exists("included_keywords_dt") || 
    !exists("exclusion_keywords_dt") || !exists("md_selection_dt") || !exists("clause_selection_dt") || !exists("mgmt_d_keywords")) {
  stop("Error: Required datasets not found. Ensure they are loaded.")
}

# Convert Keyword and Paragraph columns to lowercase for consistent matching
included_keywords_dt[, Keyword := tolower(trimws(Keyword))]
exclusion_keywords_dt[, Keyword := tolower(trimws(Keyword))]
full_legislation_parsed_DT[, Paragraph := tolower(trimws(Paragraph))]
md_selection_dt[, Keyword := tolower(trimws(Keyword))]  
mgmt_d_keywords[, Keyword := tolower(trimws(Keyword))]  
clause_selection_dt[, Keyword := tolower(trimws(Keyword))]

### Step 1: Initialize `filtered_legislation_dt` before filtering ###
full_legislation_parsed_DT[, Specificity := NA_integer_]
filtered_legislation_dt <- copy(full_legislation_parsed_DT)  # Ensure it exists

### Step 2: Filter rows using `included_keywords_dt`, prioritizing specificity ###
setorder(included_keywords_dt, Specificity) 

for (spec_level in unique(included_keywords_dt$Specificity)) {
  keyword_subset <- included_keywords_dt[Specificity == spec_level, Keyword]
  keyword_pattern <- paste0("\\b(", paste(keyword_subset, collapse = "|"), ")\\b")
  
  filtered_legislation_dt[
    grepl(keyword_pattern, Paragraph, ignore.case = TRUE),
    Specificity := spec_level
  ]
}

filtered_legislation_dt <- filtered_legislation_dt[!is.na(Specificity)]  # Remove unmatched rows

### Step 3: Remove rows using `exclusion_keywords_dt` ###
exclusion_pattern <- paste0("\\b(", paste(exclusion_keywords_dt$Keyword, collapse = "|"), ")\\b")
filtered_legislation_dt <- filtered_legislation_dt[!grepl(exclusion_pattern, Paragraph, ignore.case = TRUE)]

### Step 4: Initialize columns before assignment ###
filtered_legislation_dt[, `:=`(L1 = NA_character_, L2 = NA_character_, `Management Domain` = NA_character_, Clause_Type = NA_character_)]

### Step 5: Assign values sequentially by Specificity level ###
for (spec_level in sort(unique(filtered_legislation_dt$Specificity), decreasing = FALSE)) {
  
  # Get rows for the current specificity level that are **still unassigned**
  unassigned_rows <- filtered_legislation_dt[Specificity == spec_level]
  
  if (nrow(unassigned_rows) > 0) {
    # Initialize paragraph_words column
    unassigned_rows[, paragraph_words := lapply(Paragraph, function(text) {
      unlist(strsplit(tolower(text), "\\s+"))
    })]
    
    # Iterate through rows and assign values based on the first matched keyword
    for (i in seq_len(nrow(unassigned_rows))) {
      paragraph_words <- unassigned_rows[i, paragraph_words][[1]]
      
      # Find the first keyword that matches a word in `md_selection_dt`
      match_word <- paragraph_words[paragraph_words %in% md_selection_dt$Keyword][1]
      
      if (!is.na(match_word)) {
        first_domain_match <- md_selection_dt[Keyword == match_word, .(`L1`, `L2`, `Management Domain`)][1]
        
        filtered_legislation_dt[Paragraph == unassigned_rows[i, Paragraph] & is.na(L1), `:=`(
          L1 = first_domain_match$L1,
          L2 = first_domain_match$L2
        )]
        filtered_legislation_dt[Paragraph == unassigned_rows[i, Paragraph] & is.na(`Management Domain`), `:=`(
          `Management Domain` = first_domain_match$`Management Domain`
        )]
      }
      
      # Find the first keyword that matches a word in `clause_selection_dt`
      match_word_clause <- paragraph_words[paragraph_words %in% clause_selection_dt$Keyword][1]
      
      if (!is.na(match_word_clause)) {
        first_clause_match <- clause_selection_dt[Keyword == match_word_clause, Clause_Type][1]
        
        filtered_legislation_dt[Paragraph == unassigned_rows[i, Paragraph] & is.na(Clause_Type), `:=`(
          Clause_Type = first_clause_match
        )]
      }
    }
  }
}

# Remove temporary column safely
if ("paragraph_words" %in% names(filtered_legislation_dt)) {
  filtered_legislation_dt[, paragraph_words := NULL]
}

# Define file path for export
output_file <- "Pacific Salmon Management Domain Compendium.xlsx"

# Export filtered_legislation_dt to an xlsx file
write_xlsx(list(filtered_legislation_dt, included_keywords_dt, exclusion_keywords_dt,
             mgmt_d_keywords),
             path = output_file)

print(paste("Export successful:", output_file))

#make a full version of the legislation with joined compendium fields
full_compendium <- full_legislation_parsed_DT |>
  left_join(select(filtered_legislation_dt, legislation_name, heading, section, specificity:last_col()), 
            by = c("legislation_name", "heading", "section")) 

# Save the joined dataset
saveRDS(full_compendium, here("Full_legislation_parsed_compendium.rds"))

