#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# PACIFIC SALMON MANAGEMENT DOMAINS
# A tool for compiling and assessing Canadian and British Columbian legislation relevant to Pacific Salmon management
#///////////////////////////////////////////////////////////////////////////////

library(data.table)
library(here)

# Define file paths for the R objects
full_legislation_path <- here("Full_legislation_parsed_DT.rds")
mgmt_d_1_path <- here("mgmt_d_1.RData")
keyword_selection_path <- here("keyword_selection.RData")
exclusion_keyword_selection_path <- here("exclusion_keyword_selection.RData")

# Load Full_legislation_parsed_DT.rds
if (file.exists(full_legislation_path)) {
  full_legislation_parsed_DT <- readRDS(full_legislation_path)
  print("Loaded: Full_legislation_parsed_DT.rds")
} else {
  stop("Error: Full_legislation_parsed_DT.rds not found.")
}

# Load mgmt_d_1.RData
if (file.exists(mgmt_d_1_path)) {
  load(mgmt_d_1_path)  # Objects will be loaded into the environment
  print("Loaded: mgmt_d_1.RData")
} else {
  stop("Error: mgmt_d_1.RData not found.")
}

# Load keyword_selection.RData
if (file.exists(keyword_selection_path)) {
  load(keyword_selection_path)  # Objects will be loaded into the environment
  print("Loaded: keyword_selection.RData")
} else {
  stop("Error: keyword_selection.RData not found.")
}

# Load exclusion_keyword_selection.RData
if (file.exists(exclusion_keyword_selection_path)) {
  load(exclusion_keyword_selection_path)  # Objects will be loaded into the environment
  print("Loaded: exclusion_keyword_selection.RData")
} else {
  stop("Error: exclusion_keyword_selection.RData not found.")
}

###############################################################

library(data.table)

# Ensure required datasets are loaded
if (!exists("full_legislation_parsed_DT") || !exists("mgmt_d_keywords")) {
  stop("Error: Required datasets not found. Ensure they are loaded.")
}

# Convert Keyword and Paragraph columns to lowercase for consistent matching
mgmt_d_keywords[, Keyword := tolower(Keyword)]
full_legislation_parsed_DT[, Paragraph := tolower(Paragraph)]

# Initialize Specificity column in full_legislation_parsed_DT
full_legislation_parsed_DT[, Specificity := NA_integer_]

# Iteratively assign Specificity starting with 1, then 2, etc.
for (spec_level in sort(unique(mgmt_d_keywords$Specificity), decreasing = FALSE)) {
  keyword_subset <- mgmt_d_keywords[Specificity == spec_level, Keyword]
  
  full_legislation_parsed_DT[
    grepl(paste(keyword_subset, collapse = "|"), Paragraph, ignore.case = TRUE),
    Specificity := spec_level
  ]
}

# Remove rows with NA Specificity
filtered_legislation_dt <- full_legislation_parsed_DT[!is.na(Specificity)]

###################################################

#** Next Exclusion words 

