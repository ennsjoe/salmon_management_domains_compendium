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
library(janitor)
library(dplyr)
library(explore)  #data exploration Shiny app and reports
library(summarytools) #functions to summarize and explore data

compend <- read_excel(here("Pacific Salmon Management Domain Compendium.xlsx"))%>%
  clean_names() %>%
  mutate(across(c("jurisdiction", "legislation_type", "specificity", "l1", "l2", "management_domain", "clause_type"), as.factor))


# R shiny app for exploration
explore(compend)

# frequency and values summary table
dfSummary(compend)

#cross-tabulations
with(compend, ctable(legislation_type, management_domain))

#grouped version
gct1 <- with(compend, stby(data = list(x= legislation_type, y = management_domain), 
                  INDICES = jurisdiction, FUN = ctable))








