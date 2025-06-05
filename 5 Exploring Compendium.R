#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# PART 5 - EXPLORING THE COMPENDIUM----------------------------------------------
# 
# Summary: 
#
# Output: 
#///////////////////////////////////////////////////////////////////////////////

library(data.table)
library(here)
library(readxl)
library(janitor)   #cleaning file names
library(dplyr)
library(explore)  #data exploration Shiny app and reports
library(summarytools) #functions to summarize and explore data

# Function for loading datasets
load_object <- function(path, object_name) {
  if (file.exists(path)) {
    load(path, envir = .GlobalEnv)
    print(paste("Loaded:", object_name))
  } else {
    stop(paste("Error:", object_name, "not found."))
  }
}


## Load compendium from excel
compend <- read_excel(here("Pacific Salmon Management Domain Compendium.xlsx")) |>
  clean_names() |>
  mutate(across(c("jurisdiction", "legislation_type", "specificity", "l1", "l2", "management_domain", "clause_type"), as.factor))

# Load other R data objects
# Define file paths
management_domain_path <- here("management_domain_selection.RData")  
mgmt_d_1_path <- here("mgmt_d_1.RData")  # Reintroducing mgmt_d_1.RData
keyword_selection_path <- here("keyword_selection.RData")
exclusion_keyword_selection_path <- here("exclusion_keyword_selection.RData")
clause_type_selection_path <- here("clause_type_selection.RData")

load_object(management_domain_path, "management_domain_selection.RData")
load_object(mgmt_d_1_path, "mgmt_d_1.RData")  # Reintroduced dataset
load_object(keyword_selection_path, "keyword_selection.RData")
load_object(exclusion_keyword_selection_path, "exclusion_keyword_selection.RData")
load_object(clause_type_selection_path, "clause_type_selection.RData")


full_compendium <- readRDS(here("Full_legislation_parsed_compendium.rds")) |>
  clean_names() |>
  mutate(across(c("jurisdiction", "legislation_type", "specificity", "l1", "l2", "management_domain", "clause_type"), as.factor))


# R shiny app for exploration
explore(compend)

explore(full_compendium)

# frequency and values summary table
dfSummary(compend)

#summary of a specific act
temp <- full_compendium |>
  filter(legislation_name == "Water Sustainability Act") |>
  select(jurisdiction, legislation_type, legislation_name, specificity, l1, l2, management_domain, clause_type) |>
  dfSummary()


#cross-tabulations
with(compend, ctable(legislation_type, management_domain))

#grouped version of cross-tabulations
gct1 <- with(compend, stby(data = list(x= legislation_type, y = management_domain), 
                  INDICES = jurisdiction, FUN = ctable))










