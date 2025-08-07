################################################################################
# 3) ASSIGNING ATTRIBUTES AND GROUPING PARAGRAPHS
# Summary: 
# Inputs: 
# Outputs:
################################################################################

# 1) SETUP ---------------------------------------------------------------------
## Set Working Directory
library(here)

## Get the root directory of the project
here()

## Load Libraries ----
library(data.table)
library(xml2)
library(rvest)
library(stringi)
library(stringr)
library(writexl)
library(dplyr)

## Define file path using here()
rds_path <- here("Paragraphs_DT.rds")

## Check if the file exists before loading
if (file.exists(rds_path)) {
  Paragraphs_DT <- readRDS(rds_path)  # Load directly
  assign("Paragraphs_DT", Paragraphs_DT, envir = .GlobalEnv)  # Ensure global assignment
  message("File loaded successfully!")
} else {
  message("File does not exist: ", rds_path)
}

## Optional: Inspect loaded data structure
if (exists("Paragraphs_DT")) {
  str(Paragraphs_DT)
}

## Create the salmon_keywords data table
salmon_keywords <- data.table(
  keyword = c("salmon", "chinook", "sockeye", "coho", "chum", "salmonid"),
  scope = '1 - Salmon'
)

## Load management_domain keyword CSV file
md_threats_keywords <- fread(here("Management Domain Threats and Keywords.csv"))
file_path <- here("Clause Type Keywords.csv")

## Convert keywords into a lookup table for faster matching
keyword_lookup <- md_threats_keywords[, .(keyword, management_domain, iucn_l1, iucn_l2, scope)]

## Read CSV file while ensuring columns are characters
clause_type_keywords <- fread(file_path, colClasses = c("keyword" = "character", "clause_type" = "character"))

# Function to clean text by removing bracketed items and converting to lowercase
clean_text <- function(text) {
  text <- str_replace_all(text, "\\[.*?\\]|\\(.*?\\)", "")  # Remove items in brackets
  text <- tolower(trimws(text))  # Convert to lowercase and remove leading/trailing spaces
  return(text)
}

################################################################################
# 2) ASSIGNING MANAGEMENT DOMAIN AND IUCN THREATS-------------------------------
## Apply function to create Unique_ID using cleaned Legislation Name and Section
Paragraphs_DT[, Unique_ID := paste0(clean_text(`Legislation Name`), "_s", clean_text(Section))]

# Keep all original columns
Leg_Sec_ID_DT <- Paragraphs_DT[, .(
  Unique_ID,
  Jurisdiction,
  `Act Name`,
  `Legislation Name`,
  `Legislation Type`,
  Section,
  Heading,
  Paragraph,
  XPath
)]

################################################################################
library(data.table)
library(quanteda)
library(stringr)

# Step 1: Preprocess paragraph text
No_Punct_DT <- copy(Leg_Sec_ID_DT)
No_Punct_DT[, Paragraph := tolower(str_replace_all(Paragraph, "[[:punct:]]", " "))]
No_Punct_DT[, Paragraph := str_squish(Paragraph)]

# Step 2: Build phrase lists
md_phrases <- phrase(tolower(md_threats_keywords$keyword))
ct_valid_keywords <- clause_type_keywords[!is.na(keyword), keyword]
ct_phrases <- phrase(tolower(ct_valid_keywords))

# Step 3: Tokenize and compound both phrase sets
corpus_obj <- corpus(No_Punct_DT, text_field = "Paragraph")
tokens_obj <- tokens(corpus_obj, remove_punct = TRUE)

# Compound both threat and clause phrases
tokens_comp <- tokens_compound(tokens_obj, pattern = md_phrases)
tokens_comp <- tokens_compound(tokens_comp, pattern = ct_phrases)  # layer compounding

# Step 4: Convert tokens to list for matching
token_list <- lapply(as.list(tokens_comp), as.character)

# Step 5: Assign management domain attributes
assign_md_attributes <- function(token_vec) {
  matches <- which(tolower(md_threats_keywords$keyword) %in% gsub("_", " ", token_vec))
  if (length(matches) > 0) {
    return(md_threats_keywords[matches[1], .(management_domain, iucn_l1, iucn_l2, scope)])
  } else {
    return(data.table(management_domain = NA_character_,
                      iucn_l1 = NA_character_,
                      iucn_l2 = NA_character_,
                      scope = NA_character_))
  }
}

# Step 6: Assign clause type
assign_clause_type <- function(token_vec) {
  matches <- which(tolower(ct_valid_keywords) %in% gsub("_", " ", token_vec))
  if (length(matches) > 0) {
    return(clause_type_keywords[keyword == ct_valid_keywords[matches[1]], clause_type])
  } else {
    return(NA_character_)
  }
}

# Step 7: Apply both attribute assignments
md_attributes <- lapply(token_list, assign_md_attributes)
clause_types <- sapply(token_list, assign_clause_type)

# Step 8: Combine everything into MGMT_Attributes_DT
MGMT_Attributes_DT <- cbind(No_Punct_DT, rbindlist(md_attributes), clause_type = clause_types)


################################################################################
## Function to update scope based on matched salmon keywords
update_scope <- function(paragraph, current_scope) {
  words <- unlist(strsplit(paragraph, "\\s+"))  # Split paragraph into words
  matches <- which(salmon_keywords$keyword %in% words)  # Find matching keyword indices
  
  if (length(matches) > 0) {
    new_scope <- salmon_keywords[matches[1], scope]  # Extract scope for first match
    return(new_scope)  # Overwrite existing scope with new one
  } else {
    return(current_scope)  # Keep original value if no match
  }
}

## Apply function to update the scope column
MGMT_Attributes_DT[, scope := mapply(update_scope, Paragraph, scope)]


################################################################################

## Remove the Paragraph column ----
MGMT_Attributes_DT[, Paragraph := NULL]

# Remove the XPath column
MGMT_Attributes_DT[, XPath := NULL]

# Remove duplicate rows
MGMT_Attributes_DT <- unique(MGMT_Attributes_DT)

##########################################################################
# Create tables with unique values of domain, clause type and scope assignments by unique ID
# these are needed because you can have multiple assignments for each section
md_DT <- MGMT_Attributes_DT %>%
  select(Unique_ID, management_domain, iucn_l1, iucn_l2) %>%
  filter(!is.na(management_domain)) %>%   #remove NA rows
  distinct()  #remove duplicate rows

ct_DT <- MGMT_Attributes_DT %>%
  select(Unique_ID, clause_type) %>%
  filter(!is.na(clause_type)) %>%   #remove NA rows
  distinct()  #remove duplicate rows

scope_DT <- MGMT_Attributes_DT %>%
  select(Unique_ID, scope) %>%
  filter(!is.na(scope)) %>%   #remove NA rows
  distinct()  #remove duplicate rows

################################################################################
# 3) CONCATENATING PARAGRAPHS BY UNIQUE_ID--------------------------------------
## Aggregate Paragraphs by Unique_ID, maintaining row order and adding line breaks
Aggregated_Paragraphs_DT <- Leg_Sec_ID_DT[, .(
  Paragraph = paste(Paragraph[order(.I)], collapse = "\n")
), by = Unique_ID]

## Remove rows where Paragraph exceeds 30,000 characters
Aggregated_Paragraphs_DT <- Aggregated_Paragraphs_DT[nchar(Paragraph) <= 30000]

################################################################################
# 4) MERGE MANAGEMENT DOMAIN ATTRIBUTES WITH AGGREGATED PARAGRAPHS--------------

#function for cleaning up aggregated strings with NAs
concat_clean <- function(x) {
  # Split the string by semicolon and trim whitespace
  parts <- strsplit(x, ";")[[1]]
  parts <- trimws(parts)
  
  # Sort and remove duplicates
  unique_parts <- unique(parts)
  sorted_parts <- sort(unique_parts)
  # Remove NA; if present
  sorted_parts <- sorted_parts[sorted_parts != ";NA "]
  sorted_parts <- sorted_parts[sorted_parts != "NA"]
  
  # Concatenate back to a single string
  return(paste(sorted_parts, collapse = "; "))
}

MGMT_Attributes_DT <- MGMT_Attributes_DT[, .(
  Unique_ID,
  Jurisdiction,
  `Act Name`,
  `Legislation Name`,
  `Legislation Type`,
  Section,
  Heading)]

## Merge MGMT_Attributes_DT and Aggregated_Paragraphs_DT on Unique_ID
Merged_DT <- merge(MGMT_Attributes_DT, Aggregated_Paragraphs_DT, by = "Unique_ID", all.x = TRUE) |>
  distinct() 

# concatenate management domain and iucn threat assigments to one value per section, then merge with full DT
Collapsed_md <- md_DT[, .(management_domain = paste(management_domain, collapse = "; "), 
                           iucn_l1 = paste(iucn_l1, collapse = "; "),
                           iucn_l2 = paste(iucn_l2, collapse = "; ")), 
                       by = Unique_ID]

#remove duplicated character strings from collapsed_DT
Collapsed_md[, management_domain := sapply(management_domain, concat_clean)]
Collapsed_md[, iucn_l1 := sapply(iucn_l1, concat_clean)]
Collapsed_md[, iucn_l2 := sapply(iucn_l2, concat_clean)]

#repeat for clause type and scope attributes
Collapsed_ct <- ct_DT[, .(clause_type = paste(clause_type, collapse = "; ")), by = Unique_ID]
Collapsed_ct[, clause_type := sapply(clause_type, concat_clean)]

Collapsed_scope <- scope_DT[, .(scope = paste(scope, collapse = "; ")), by = Unique_ID]
Collapsed_scope[, scope := sapply(scope, concat_clean)]

Merged_DT <- merge(Merged_DT, Collapsed_md, by = "Unique_ID")
Merged_DT <- merge(Merged_DT, Collapsed_ct, by = "Unique_ID")
Merged_DT <- merge(Merged_DT, Collapsed_scope, by = "Unique_ID")

## Drop Unique_ID and XPath columns
#Merged_DT[, c("Unique_ID") := NULL]

################################################################################
# 6) FINALIZE AND SAVE THE DATA TABLE-------------------------------------------
## Create new formatted datatable
Full_legislation_parsed_DT <- Merged_DT[, .(
  Unique_ID,
  Jurisdiction,
  `Legislation Type`,
  `Act Name`,
  `Legislation Name`,
  Section,
  Heading,
  Paragraph,
  Scope = scope,
  `Management Domain` = management_domain,
  `IUCN Level 1` = iucn_l1,
  `IUCN Level 2` = iucn_l2,
  `Clause Type` = clause_type
)]

#arrange by numeric section value
Full_legislation_parsed_DT[, Section_num := as.numeric(gsub("s", "", Section))]  # Convert Section to numeric
Full_legislation_parsed_DT <- Full_legislation_parsed_DT[order(`Act Name`, `Legislation Name`, Section_num)]
Full_legislation_parsed_DT[, Section_num := NULL]  # Remove temporary numeric section column

# Ensure all objects are correctly passed to the list
saved_data <- list(
  Full_legislation_parsed_DT = Full_legislation_parsed_DT,
  salmon_keywords = salmon_keywords,
  md_threats_keywords = md_threats_keywords,
  clause_type_keywords = clause_type_keywords,
  md_table = md_DT,   #table with one row per management domain assignment for each section
  ct_table = ct_DT,
  scope_table = scope_DT
)

# Save as an R object
saveRDS(saved_data, "Full_legislation_compendium.rds")

file_pathxl <- here("Compendium_of_Legislation_(full).xlsx")

# Export to XLSX
write_xlsx(Full_legislation_parsed_DT, path = file_pathxl)

