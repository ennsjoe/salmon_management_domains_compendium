################################################################################
# Title:  Plots of LAPSE outputs
# Authors: [Your Name]
# Date Created: 2025-01-19
# Last Modified: 2025-01-20
# Purpose / Description:
#   Exports CSV files from the legislation database for use in a TypeScript/Java/Vite
#   application. Outputs two files:
#   - legislation_output.csv: Legislation info (joinable by legislation_id)
#   - paragraph_output.csv: Paragraphs with labels and actionable clauses (one row per paragraph)
#
# Outputs:
#   - output/legislation_output.csv : Legislation metadata (id, jurisdiction, type, names, url, agencies)
#   - output/paragraph_output.csv : Paragraphs with all labels aggregated (one row per paragraph)
#
# Dependencies: DBI, RSQLite, data.table, here, stringi
################################################################################


## Load Libraries ----
library(DBI)
library(RSQLite)
library(data.table)
library(here)
library(stringi)
library(tidyverse)

here()
## Create output directory ----
output_dir <- file.path(here("output"))
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

paragraphs <- read.csv(file.path(output_dir, "paragraph_output.csv"))
legislation <- read.csv(file.path(output_dir, "legislation_output.csv"))

pardata <- paragraphs %>%
  left_join(select(legislation, legislation_id, jurisdiction, legislation_type, legislation_name, agencies), join_by(legislation_id))
  

#unique count of management domains within each act

dom_count <- pardata %>%
  group_by(legislation_name) %>%
  summarize( unique_doms = count(unique(management_domain)))
