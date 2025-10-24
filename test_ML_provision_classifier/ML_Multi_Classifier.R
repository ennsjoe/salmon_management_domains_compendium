################################################################################
# Title: Multi-Category ML Classifier for Legislation
# Authors: Joe Enns, Cory Lagasse
# Date Created: 2025-10-23
# Purpose / Description:
#   Trains separate ML models for three classification tasks:
#   1. Clause Types (7 categories)
#   2. Management Domains (17 categories)  
#   3. IUCN Threats (Level 1 and Level 2)
#   Uses existing labels from paragraph_label_table to train and predict.
# Dependencies: DBI, RSQLite, data.table, here, caret, randomForest, 
#               quanteda, quanteda.textstats, openxlsx
# Outputs:
#   ML_Multi_Classification_Results.xlsx with predictions for all categories
################################################################################

## Load Libraries ----
library(here)
library(DBI)
library(RSQLite)
library(data.table)
library(openxlsx)
library(stringr)
library(quanteda)
library(quanteda.textstats)
library(caret)
library(randomForest)
library(beepr)

cat("========================================\n")
cat("Multi-Category ML Classification System\n")
cat("========================================\n\n")

## Connect to Database ----
db_path <- file.path(here("output"), "legislation.db")
if (!file.exists(db_path)) {
  stop("Database file not found at: ", db_path)
}

conn <- dbConnect(SQLite(), dbname = db_path)

## Load Tables ----
cat("Step 1: Loading data from database...\n")
paragraph_table <- as.data.table(dbReadTable(conn, "LegislationParagraphs"))
legislation_table <- as.data.table(dbReadTable(conn, "LegislationMetadata"))
label_table <- as.data.table(dbReadTable(conn, "paragraph_label_table"))
clause_keywords <- as.data.table(dbReadTable(conn, "clause_type_keywords"))
governance_keywords <- as.data.table(dbReadTable(conn, "governance_keywords"))
threat_table <- as.data.table(dbReadTable(conn, "management_domain_threat_table"))

dbDisconnect(conn)

cat(sprintf("  - Loaded %d paragraphs\n", nrow(paragraph_table)))
cat(sprintf("  - Loaded %d labels\n", nrow(label_table)))

## ============================================================================
## SECTION 1: PREPARE DATA FOR EACH CLASSIFICATION TASK
## ============================================================================

cat("\nStep 2: Preparing training data for each task...\n")

## Task 1: Clause Types ----
cat("\n  Task 1: CLAUSE TYPES\n")
clause_labels <- label_table[label_type == "Clause Type" & !is.na(label_value)]
clause_labels <- merge(
  clause_labels[, .(paragraph_id, keyword)],
  clause_keywords[, .(keyword, clause_type)],
  by = "keyword",
  all.x = TRUE
)

# Aggregate multiple labels per paragraph
clause_labels_agg <- clause_labels[, .(
  clause_type = paste(unique(clause_type), collapse = "; ")
), by = paragraph_id]

# Filter single-label cases
clause_labels_single <- clause_labels_agg[!grepl(";", clause_type)]

# Merge with paragraph text
clause_training <- merge(
  clause_labels_single,
  paragraph_table[, .(paragraph_id, Paragraph, Section, legislation_id)],
  by = "paragraph_id",
  all.x = TRUE
)

clause_training <- merge(
  clause_training,
  legislation_table[, .(legislation_id, act_name, jurisdiction)],
  by = "legislation_id",
  all.x = TRUE
)

# Check class distribution
clause_dist <- clause_training[, .N, by = clause_type][order(-N)]
cat("    Clause type distribution:\n")
print(clause_dist)

# Filter to classes with sufficient examples
valid_clause_classes <- clause_dist[N >= 20, clause_type]
clause_training <- clause_training[clause_type %in% valid_clause_classes]

cat(sprintf("    - Training samples: %d in %d classes\n", 
            nrow(clause_training), length(valid_clause_classes)))

## Task 2: Management Domains ----
cat("\n  Task 2: MANAGEMENT DOMAINS\n")

# From governance keywords
governance_labels <- label_table[label_type == "Management Domain" & !is.na(label_value)]
governance_labels <- merge(
  governance_labels[, .(paragraph_id, keyword)],
  governance_keywords[, .(keyword, management_domain)],
  by = "keyword",
  all.x = TRUE
)

# Aggregate
domain_labels_agg <- governance_labels[, .(
  management_domain = paste(unique(management_domain), collapse = "; ")
), by = paragraph_id]

# Filter single-label
domain_labels_single <- domain_labels_agg[!grepl(";", management_domain)]

# Merge with paragraph text
domain_training <- merge(
  domain_labels_single,
  paragraph_table[, .(paragraph_id, Paragraph, Section, legislation_id)],
  by = "paragraph_id",
  all.x = TRUE
)

domain_training <- merge(
  domain_training,
  legislation_table[, .(legislation_id, act_name, jurisdiction)],
  by = "legislation_id",
  all.x = TRUE
)

# Check distribution
domain_dist <- domain_training[, .N, by = management_domain][order(-N)]
cat("    Management domain distribution:\n")
print(domain_dist)

# Filter to classes with sufficient examples
valid_domain_classes <- domain_dist[N >= 20, management_domain]
domain_training <- domain_training[management_domain %in% valid_domain_classes]

cat(sprintf("    - Training samples: %d in %d classes\n", 
            nrow(domain_training), length(valid_domain_classes)))

## Task 3: IUCN Threats ----
cat("\n  Task 3: IUCN THREATS\n")

iucn_labels <- label_table[label_type == "IUCN Threat" & !is.na(label_value)]

# Get IUCN levels from threat table
# First need to map keywords to IUCN
# Create a lookup from management_domain to IUCN
domain_to_iucn <- unique(threat_table[!is.na(iucn_l1), .(management_domain, iucn_l1, iucn_l2)])

# Join governance keywords with IUCN threats
governance_to_iucn <- merge(
  governance_keywords,
  domain_to_iucn,
  by = "management_domain",
  all.x = TRUE,
  allow.cartesian = TRUE
)

# Now join labels
iucn_training_data <- merge(
  iucn_labels[, .(paragraph_id, keyword)],
  governance_to_iucn[, .(keyword, management_domain, iucn_l1, iucn_l2)],
  by = "keyword",
  all.x = TRUE
)

# Aggregate by IUCN L1
iucn_l1_agg <- iucn_training_data[!is.na(iucn_l1), .(
  iucn_l1 = paste(unique(iucn_l1), collapse = "; ")
), by = paragraph_id]

iucn_l1_single <- iucn_l1_agg[!grepl(";", iucn_l1)]

# Merge with paragraphs
iucn_training <- merge(
  iucn_l1_single,
  paragraph_table[, .(paragraph_id, Paragraph, Section, legislation_id)],
  by = "paragraph_id",
  all.x = TRUE
)

iucn_training <- merge(
  iucn_training,
  legislation_table[, .(legislation_id, act_name, jurisdiction)],
  by = "legislation_id",
  all.x = TRUE
)

# Check distribution
iucn_dist <- iucn_training[, .N, by = iucn_l1][order(-N)]
cat("    IUCN L1 distribution:\n")
print(iucn_dist)

# Filter
valid_iucn_classes <- iucn_dist[N >= 20, iucn_l1]
iucn_training <- iucn_training[iucn_l1 %in% valid_iucn_classes]

cat(sprintf("    - Training samples: %d in %d classes\n", 
            nrow(iucn_training), length(valid_iucn_classes)))

## ============================================================================
## SECTION 2: FEATURE ENGINEERING FUNCTION
## ============================================================================

cat("\nStep 3: Preparing feature engineering pipeline...\n")

engineer_features <- function(data, target_col, corpus_name = "corpus") {
  cat(sprintf("  - Engineering features for %s...\n", corpus_name))
  
  # Create corpus
  corp <- corpus(data$Paragraph)
  
  # Tokenize
  toks <- tokens(corp, remove_punct = TRUE, remove_numbers = FALSE)
  toks <- tokens_tolower(toks)
  
  # Create DFM
  dfm_data <- dfm(toks)
  dfm_data <- dfm_trim(dfm_data, min_termfreq = 10, min_docfreq = 5)
  
  # Limit features
  if (ncol(dfm_data) > 500) {
    top_features <- topfeatures(dfm_data, n = 500)
    dfm_data <- dfm_select(dfm_data, names(top_features))
  }
  
  cat(sprintf("    Text features: %d\n", ncol(dfm_data)))
  
  # Convert to dataframe
  text_features <- as.data.frame(as.matrix(dfm_data))
  text_features$paragraph_id <- data$paragraph_id
  
  # Engineer additional features
  data[, `:=`(
    paragraph_length = nchar(Paragraph),
    word_count = str_count(Paragraph, "\\S+"),
    sentence_count = str_count(Paragraph, "[.!?]+"),
    avg_word_length = nchar(gsub("\\s", "", Paragraph)) / pmax(1, str_count(Paragraph, "\\S+")),
    has_subsection = grepl("\\([a-z]\\)", Paragraph),
    has_number = grepl("\\d", Paragraph),
    starts_with_number = grepl("^\\d", Paragraph),
    has_shall = grepl("\\bshall\\b", Paragraph, ignore.case = TRUE),
    has_must = grepl("\\bmust\\b", Paragraph, ignore.case = TRUE),
    has_may = grepl("\\bmay\\b", Paragraph, ignore.case = TRUE),
    has_means = grepl("\\bmeans\\b", Paragraph, ignore.case = TRUE),
    has_minister = grepl("\\bminister\\b", Paragraph, ignore.case = TRUE),
    has_governor = grepl("\\bgovernor\\b", Paragraph, ignore.case = TRUE),
    has_director = grepl("\\bdirector\\b", Paragraph, ignore.case = TRUE),
    has_prohibit = grepl("\\bprohibit|\\bforbid", Paragraph, ignore.case = TRUE),
    has_permit = grepl("\\bpermit|\\blicen[cs]e", Paragraph, ignore.case = TRUE),
    has_habitat = grepl("\\bhabitat|\\becosystem", Paragraph, ignore.case = TRUE),
    has_species = grepl("\\bspecies|\\bwildlife|\\bfish", Paragraph, ignore.case = TRUE),
    has_water = grepl("\\bwater|\\bstream|\\briver|\\bmarine", Paragraph, ignore.case = TRUE),
    section_number = suppressWarnings(as.numeric(Section))
  )]
  
  # Handle NAs
  numeric_cols <- c("paragraph_length", "word_count", "sentence_count", "avg_word_length", "section_number")
  for (col in numeric_cols) {
    data[is.na(get(col)) | is.infinite(get(col)), (col) := 0]
  }
  
  logical_cols <- c("has_subsection", "has_number", "starts_with_number", 
                    "has_shall", "has_must", "has_may", "has_means",
                    "has_minister", "has_governor", "has_director",
                    "has_prohibit", "has_permit", "has_habitat", "has_species", "has_water")
  for (col in logical_cols) {
    data[is.na(get(col)), (col) := FALSE]
  }
  
  engineered_features <- data[, .(
    paragraph_id, paragraph_length, word_count, sentence_count, avg_word_length,
    has_subsection, has_number, starts_with_number,
    has_shall, has_must, has_may, has_means,
    has_minister, has_governor, has_director,
    has_prohibit, has_permit, has_habitat, has_species, has_water,
    section_number
  )]
  
  # Add target variable
  engineered_features[, (target_col) := data[[target_col]]]
  
  # Merge
  ml_data <- merge(text_features, engineered_features, by = "paragraph_id", all.x = TRUE)
  ml_data <- as.data.frame(ml_data)
  ml_data <- ml_data[, !names(ml_data) %in% c("paragraph_id")]
  ml_data[is.na(ml_data)] <- 0
  ml_data[[target_col]] <- as.factor(ml_data[[target_col]])
  
  cat(sprintf("    Total features: %d\n", ncol(ml_data) - 1))
  
  return(ml_data)
}

## ============================================================================
## SECTION 3: TRAIN MODELS
## ============================================================================

cat("\nStep 4: Training machine learning models...\n")

set.seed(123)
train_control <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = "final",
  classProbs = TRUE,
  verboseIter = FALSE
)

models <- list()
performance <- list()

## Train Clause Type Model ----
if (nrow(clause_training) > 0) {
  cat("\n  Training CLAUSE TYPE classifier...\n")
  
  clause_ml_data <- engineer_features(clause_training, "clause_type", "Clause Types")
  
  # Split data
  train_idx <- createDataPartition(clause_ml_data$clause_type, p = 0.8, list = FALSE)
  train_set <- clause_ml_data[train_idx, ]
  test_set <- clause_ml_data[-train_idx, ]
  
  cat(sprintf("    Training: %d | Test: %d\n", nrow(train_set), nrow(test_set)))
  
  # Train model
  clause_model <- train(
    clause_type ~ .,
    data = train_set,
    method = "rf",
    trControl = train_control,
    ntree = 100
  )
  
  # Evaluate
  clause_pred <- predict(clause_model, newdata = test_set)
  clause_conf <- confusionMatrix(clause_pred, test_set$clause_type)
  
  models$clause_type <- clause_model
  performance$clause_type <- list(
    confusion = clause_conf,
    accuracy = clause_conf$overall["Accuracy"],
    kappa = clause_conf$overall["Kappa"]
  )
  
  cat(sprintf("    ✓ Accuracy: %.2f%%\n", clause_conf$overall["Accuracy"] * 100))
}

## Train Management Domain Model ----
if (nrow(domain_training) > 0) {
  cat("\n  Training MANAGEMENT DOMAIN classifier...\n")
  
  domain_ml_data <- engineer_features(domain_training, "management_domain", "Management Domains")
  
  train_idx <- createDataPartition(domain_ml_data$management_domain, p = 0.8, list = FALSE)
  train_set <- domain_ml_data[train_idx, ]
  test_set <- domain_ml_data[-train_idx, ]
  
  cat(sprintf("    Training: %d | Test: %d\n", nrow(train_set), nrow(test_set)))
  
  domain_model <- train(
    management_domain ~ .,
    data = train_set,
    method = "rf",
    trControl = train_control,
    ntree = 100
  )
  
  domain_pred <- predict(domain_model, newdata = test_set)
  domain_conf <- confusionMatrix(domain_pred, test_set$management_domain)
  
  models$management_domain <- domain_model
  performance$management_domain <- list(
    confusion = domain_conf,
    accuracy = domain_conf$overall["Accuracy"],
    kappa = domain_conf$overall["Kappa"]
  )
  
  cat(sprintf("    ✓ Accuracy: %.2f%%\n", domain_conf$overall["Accuracy"] * 100))
}

## Train IUCN Threat Model ----
if (nrow(iucn_training) > 0) {
  cat("\n  Training IUCN THREAT classifier...\n")
  
  iucn_ml_data <- engineer_features(iucn_training, "iucn_l1", "IUCN Threats")
  
  train_idx <- createDataPartition(iucn_ml_data$iucn_l1, p = 0.8, list = FALSE)
  train_set <- iucn_ml_data[train_idx, ]
  test_set <- iucn_ml_data[-train_idx, ]
  
  cat(sprintf("    Training: %d | Test: %d\n", nrow(train_set), nrow(test_set)))
  
  iucn_model <- train(
    iucn_l1 ~ .,
    data = train_set,
    method = "rf",
    trControl = train_control,
    ntree = 100
  )
  
  iucn_pred <- predict(iucn_model, newdata = test_set)
  iucn_conf <- confusionMatrix(iucn_pred, test_set$iucn_l1)
  
  models$iucn_threat <- iucn_model
  performance$iucn_threat <- list(
    confusion = iucn_conf,
    accuracy = iucn_conf$overall["Accuracy"],
    kappa = iucn_conf$overall["Kappa"]
  )
  
  cat(sprintf("    ✓ Accuracy: %.2f%%\n", iucn_conf$overall["Accuracy"] * 100))
}

## ============================================================================
## SECTION 4: EXPORT RESULTS
## ============================================================================

cat("\nStep 5: Creating comprehensive Excel output...\n")

output_file <- file.path(here(), "ML_Multi_Classification_Results.xlsx")
wb <- createWorkbook()

## Summary Sheet ----
addWorksheet(wb, "Model Summary")
summary_data <- data.frame(
  Model = character(),
  Training_Samples = integer(),
  Test_Samples = integer(),
  Classes = integer(),
  Accuracy = character(),
  Kappa = character(),
  stringsAsFactors = FALSE
)

if (!is.null(models$clause_type)) {
  summary_data <- rbind(summary_data, data.frame(
    Model = "Clause Types",
    Training_Samples = nrow(clause_training) * 0.8,
    Test_Samples = nrow(clause_training) * 0.2,
    Classes = length(valid_clause_classes),
    Accuracy = sprintf("%.2f%%", performance$clause_type$accuracy * 100),
    Kappa = sprintf("%.3f", performance$clause_type$kappa)
  ))
}

if (!is.null(models$management_domain)) {
  summary_data <- rbind(summary_data, data.frame(
    Model = "Management Domains",
    Training_Samples = nrow(domain_training) * 0.8,
    Test_Samples = nrow(domain_training) * 0.2,
    Classes = length(valid_domain_classes),
    Accuracy = sprintf("%.2f%%", performance$management_domain$accuracy * 100),
    Kappa = sprintf("%.3f", performance$management_domain$kappa)
  ))
}

if (!is.null(models$iucn_threat)) {
  summary_data <- rbind(summary_data, data.frame(
    Model = "IUCN Threats (L1)",
    Training_Samples = nrow(iucn_training) * 0.8,
    Test_Samples = nrow(iucn_training) * 0.2,
    Classes = length(valid_iucn_classes),
    Accuracy = sprintf("%.2f%%", performance$iucn_threat$accuracy * 100),
    Kappa = sprintf("%.3f", performance$iucn_threat$kappa)
  ))
}

writeDataTable(wb, "Model Summary", summary_data)

## Individual Performance Sheets ----
for (model_name in names(performance)) {
  perf <- performance[[model_name]]
  
  sheet_name <- gsub("_", " ", model_name)
  sheet_name <- paste0(toupper(substring(sheet_name, 1, 1)), substring(sheet_name, 2))
  sheet_name <- substr(sheet_name, 1, 31)  # Excel limit
  
  addWorksheet(wb, sheet_name)
  
  # Confusion matrix
  conf_table <- as.data.frame.matrix(perf$confusion$table)
  conf_table <- cbind(Actual = rownames(conf_table), conf_table)
  writeDataTable(wb, sheet_name, conf_table)
}

## Save workbook ----
saveWorkbook(wb, output_file, overwrite = TRUE)

cat(sprintf("\n✅ Results saved to: %s\n", output_file))

## Save models ----
model_file <- file.path(here(), "multi_classifier_models.rds")
saveRDS(models, model_file)
cat(sprintf("✅ Models saved to: %s\n", model_file))

## ============================================================================
## SUMMARY
## ============================================================================

cat("\n========================================\n")
cat("TRAINING COMPLETE - SUMMARY\n")
cat("========================================\n\n")

for (model_name in names(performance)) {
  perf <- performance[[model_name]]
  cat(sprintf("%s:\n", toupper(gsub("_", " ", model_name))))
  cat(sprintf("  Accuracy: %.2f%%\n", perf$accuracy * 100))
  cat(sprintf("  Kappa: %.3f\n\n", perf$kappa))
}

cat("========================================\n")
cat("Next steps:\n")
cat("1. Review model performance in Excel\n")
cat("2. Check confusion matrices\n")
cat("3. Use models to predict on new data\n")
cat("========================================\n")

beep(sound = 1)
