################################################################################
# 3) ASSIGNING ATTRIBUTES AND GROUPING PARAGRAPHS
################################################################################

## Set Working Directory ----
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

################################################################################
# 3) Assign Domains, etc and Group----------------------------------------------
## Function to assign attributes based on first matched keyword----
assign_attributes <- function(paragraph) {
  words <- unlist(strsplit(paragraph, "\\s+"))
  matches <- md_threats_keywords[Keyword %in% words]  
  
  if (nrow(matches) > 0) {
    # Find the first matching word in the paragraph
    first_match <- words[words %in% matches$Keyword][1]
    selected_row <- matches[Keyword == first_match][1]  # Retrieve attributes from first match
    return(selected_row[, .(`Management Domain`, L1, L2, Scope)])
  } else {
    return(data.table(
      `Management Domain` = NA_character_,
      L1 = NA_character_,
      L2 = NA_character_,
      Scope = NA_character_
    ))
  }
}

## Create a new datatable by copying Paragraphs_DT first
Management_DT <- copy(Paragraphs_DT)

## Assign attributes and add new columns without removing existing ones
Management_DT[, c("Management Domain", "L1", "L2", "Scope") := assign_attributes(Paragraph), by = Paragraph]

## Combine Paragraphs while keeping all original columns except XPath------------
Full_legislation_parsed_DT <- Management_DT[, .(
  Paragraph = paste(Paragraph, collapse = "\n\n")  # Add line breaks between paragraphs
), by = .(`Management Domain`, Section, Heading, `Legislation Name`, `Legislation Type`, `Act Name`, `Jurisdiction`, L1, L2, Scope)]  # Grouping in specified order

# Function to update Scope only for matching rows-------------------------------
update_scope_salmon <- function(paragraph, existing_scope, keywords_dt) {
  # Standardize text: Remove punctuation and convert to lowercase
  clean_paragraph <- tolower(gsub("[[:punct:]]", " ", paragraph))
  
  # Split paragraph into words
  words <- unlist(strsplit(clean_paragraph, "\\s+"))
  
  # Find matches in keyword list
  matches <- keywords_dt[Keyword %in% words]
  
  if (nrow(matches) > 0) {
    # Get first matched word
    first_match <- words[words %in% matches$Keyword][1]
    
    # Retrieve Scope corresponding to the first matched keyword
    scope_value <- matches[Keyword == first_match, Scope][1]
    
    return(scope_value)  # Update only for matched rows
  } else {
    return(existing_scope)  # Keep original value for non-matching rows
  }
}

## Apply function to update Scope only where a match is found
Full_legislation_parsed_DT[, Scope := mapply(update_scope_salmon, Paragraph, Scope, MoreArgs = list(keywords_dt = salmon_keywords))]

## Function to assign Clause_Type based on first matched word with improved matching----
assign_clause_type <- function(paragraph, keywords_dt) {
  # Standardize text: Remove punctuation (except word boundaries) and convert to lowercase
  clean_paragraph <- str_to_lower(gsub("[[:punct:]]", " ", paragraph))
  
  # Split paragraph into words
  words <- unlist(strsplit(clean_paragraph, "\\s+"))  
  
  # Find matches in keyword list (case-insensitive)
  matches <- keywords_dt[Keyword %in% words]
  
  if (nrow(matches) > 0) {
    # Get first matched word
    first_match <- words[words %in% matches$Keyword][1]
    
    # Retrieve Clause_Type corresponding to the first matched keyword
    clause_type <- matches[Keyword == first_match, Clause_Type][1]
    
    return(clause_type)
  } else {
    return(NA_character_)
  }
}

## Apply function to assign Clause_Type----
Full_legislation_parsed_DT[, Clause_Type := sapply(Paragraph, assign_clause_type, keywords_dt = clause_type_keywords)]

# 4) Export Results-------------------------------------------------------------
## Trim Paragraph column to a maximum of 5,000 characters-----------------------
Full_legislation_parsed_DT[, Paragraph := substr(Paragraph, 1, 5000)]

## Reorder the columns-----------------------------------------------------------
setcolorder(Full_legislation_parsed_DT, c(
  "Jurisdiction", "Legislation Type", "Act Name", "Legislation Name",
  "Heading", "Section", "Paragraph",
  "Management Domain", "L1", "L2", "Scope", "Clause_Type"
))

## Save datatables as an R object------------------------------------------------
saved_data<- list(
  Full_legislation_parsed_DT = Full_legislation_parsed_DT,
  salmon_keywords = salmon_keywords,
  md_threats_keywords = md_threats_keywords,
  clause_type_keywords = clause_type_keywords,
  Paragraphs_DT = Paragraphs_DT
)
saveRDS(saved_data, "Full_legislation_compendium.rds")

## Define file path using here()
file_pathxl <- here("Compendium_of_Legislation_(full).xlsx")

## Export data table to XLSX
write_xlsx(Full_legislation_parsed_DT, path = file_pathxl)