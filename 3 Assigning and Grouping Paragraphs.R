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

## Function to assign attributes based on first matched keyword----
assign_attributes <- function(paragraph) {
  words <- unlist(strsplit(paragraph, "\\s+"))  # Split into individual words
  matches <- which(md_threats_keywords$keyword %in% words)  # Find matching keyword indices
  
  if (length(matches) > 0) {
    selected_row <- md_threats_keywords[matches[1], .(management_domain, iucn_l1, iucn_l2, scope)]  # Extract attributes
    setnames(selected_row, c("management_domain", "iucn_l1", "iucn_l2", "scope"))  # Ensure column names remain correct
    return(selected_row)
  } else {
    return(data.table(
      management_domain = NA_character_,
      iucn_l1 = NA_character_,
      iucn_l2 = NA_character_,
      scope = NA_character_
    ))
  }
}

## Create new data table storing keyword attributes ----
MGMT_Attributes_DT <- Leg_Sec_ID_DT[, {
  attributes <- assign_attributes(Paragraph)
  c(.SD, attributes)  # Preserve original columns while merging attributes correctly
}, by = Unique_ID]  # Ensure row consistency

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

## Remove the Paragraph column ----
MGMT_Attributes_DT[, Paragraph := NULL]

# Remove the XPath column
MGMT_Attributes_DT[, XPath := NULL]

# Remove duplicate rows
MGMT_Attributes_DT <- unique(MGMT_Attributes_DT)

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
## Merge MGMT_Attributes_DT and Aggregated_Paragraphs_DT on Unique_ID
Merged_DT <- merge(MGMT_Attributes_DT, Aggregated_Paragraphs_DT, by = "Unique_ID", all.x = TRUE)

## Drop Unique_ID and XPath columns
Merged_DT[, c("Unique_ID") := NULL]

################################################################################
# 5) ASSIGNING CLAUSE TYPE------------------------------------------------------

## Function to identify keywords and assign clause_type based on first match
assign_clause_type <- function(paragraph) {
  words <- unlist(strsplit(paragraph, "\\s+"))  # Split paragraph into individual words
  matches <- which(words %in% clause_type_keywords$keyword)  # Find keyword matches
  
  if (length(matches) > 0) {
    first_match <- words[matches[1]]  # Get the first matched keyword
    clause_type <- clause_type_keywords[keyword == first_match, clause_type]  # Assign clause_type
    return(clause_type)
  } else {
    return(NA_character_)  # Assign NA if no match found
  }
}

## Apply function to update Merged_DT with clause_type based on first matched keyword
Merged_DT[, clause_type := sapply(Paragraph, assign_clause_type)]

################################################################################
# 6) FINALIZE AND SAVE THE DATA TABLE-------------------------------------------
## Create new formatted datatable
Full_legislation_parsed_DT <- Merged_DT[, .(
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

# Ensure all objects are correctly passed to the list
saved_data <- list(
  Full_legislation_parsed_DT = Full_legislation_parsed_DT,
  salmon_keywords = salmon_keywords,
  md_threats_keywords = md_threats_keywords,
  clause_type_keywords = clause_type_keywords
)

# Save as an R object
saveRDS(saved_data, "Full_legislation_compendium.rds")

file_pathxl <- here("Compendium_of_Legislation_(full).xlsx")

# Export to XLSX
write_xlsx(Full_legislation_parsed_DT, path = file_pathxl)
