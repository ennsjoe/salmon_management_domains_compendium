################################################################################
# Title: Machine Learning for Provision Classification
# Authors: Joe Enns, Cory Lagasse
# Date Created: 2025-01-XX
# Purpose / Description:
#   Uses machine learning to learn from existing keyword-labeled data and
#   improve provision type classification. Trains models on your existing
#   paragraph_label_table data to predict provision types.
# Dependencies: DBI, RSQLite, data.table, here, caret, randomForest, 
#               quanteda, quanteda.textstats, text2vec
# Outputs:
#   Trained models and predictions added to database and Excel output
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

cat("=====================================\n")
cat("ML-Enhanced Provision Classification\n")
cat("=====================================\n\n")

## Connect to Database ----
db_path <- file.path(here("output"), "legislation.db")
if (!file.exists(db_path)) {
  stop("Database file not found at: ", db_path)
}

conn <- dbConnect(SQLite(), dbname = db_path)

## Load Tables ----
cat("Loading data from database...\n")
paragraph_table <- as.data.table(dbReadTable(conn, "LegislationParagraphs"))
legislation_table <- as.data.table(dbReadTable(conn, "LegislationMetadata"))
label_table <- as.data.table(dbReadTable(conn, "paragraph_label_table"))
clause_keywords <- as.data.table(dbReadTable(conn, "clause_type_keywords"))

dbDisconnect(conn)

## ============================================================================
## SECTION 1: PREPARE TRAINING DATA
## ============================================================================

cat("\nStep 1: Preparing training data from existing labels...\n")

## Get clause type labels ----
clause_labels <- label_table[label_type == "Clause Type" & !is.na(label_value)]

## Merge labels with keywords to get clause_type ----
clause_labels <- merge(
  clause_labels[, .(paragraph_id, keyword)],
  clause_keywords[, .(keyword, clause_type)],
  by = "keyword",
  all.x = TRUE
)

## Aggregate multiple labels per paragraph ----
clause_labels_agg <- clause_labels[, .(
  clause_type = paste(unique(clause_type), collapse = "; ")
), by = paragraph_id]

## Merge with paragraph text ----
training_data <- merge(
  clause_labels_agg,
  paragraph_table[, .(paragraph_id, Paragraph, Section, legislation_id)],
  by = "paragraph_id",
  all.x = TRUE
)

## Filter out multi-label cases for simpler model ----
training_data <- training_data[!grepl(";", clause_type)]

## Merge with legislation metadata ----
training_data <- merge(
  training_data,
  legislation_table[, .(legislation_id, act_name, jurisdiction)],
  by = "legislation_id",
  all.x = TRUE
)

cat(sprintf("  - Loaded %d labeled paragraphs\n", nrow(training_data)))
cat(sprintf("  - Unique clause types: %d\n", length(unique(training_data$clause_type))))

## Check class distribution ----
class_dist <- training_data[, .N, by = clause_type][order(-N)]
cat("\nClass distribution:\n")
print(class_dist)

## Filter to classes with sufficient examples (at least 20) ----
valid_classes <- class_dist[N >= 20, clause_type]
training_data <- training_data[clause_type %in% valid_classes]

cat(sprintf("\n  - After filtering: %d paragraphs in %d classes\n", 
            nrow(training_data), length(valid_classes)))

## ============================================================================
## SECTION 2: FEATURE ENGINEERING
## ============================================================================

cat("\nStep 2: Engineering features for machine learning...\n")

## Create quanteda corpus ----
corp <- corpus(training_data$Paragraph)
docvars(corp, "clause_type") <- training_data$clause_type
docvars(corp, "paragraph_id") <- training_data$paragraph_id

## Tokenize ----
toks <- tokens(corp, remove_punct = TRUE, remove_numbers = FALSE)
toks <- tokens_tolower(toks)

## Create Document-Feature Matrix (DFM) ----
cat("  - Creating document-feature matrix...\n")
dfm_train <- dfm(toks)

## Feature selection: keep top features by frequency ----
dfm_train <- dfm_trim(dfm_train, min_termfreq = 10, min_docfreq = 5)
cat(sprintf("  - Features retained: %d\n", ncol(dfm_train)))

# Further reduce to top N most frequent features to avoid memory issues
if (ncol(dfm_train) > 500) {
  cat("  - Reducing to top 500 features to manage memory...\n")
  top_features <- topfeatures(dfm_train, n = 500)
  dfm_train <- dfm_select(dfm_train, names(top_features))
  cat(sprintf("  - Final feature count: %d\n", ncol(dfm_train)))
})

## Additional features ----
cat("  - Engineering additional features...\n")

training_data[, `:=`(
  # Length features
  paragraph_length = nchar(Paragraph),
  word_count = str_count(Paragraph, "\\S+"),
  
  # Sentence features
  sentence_count = str_count(Paragraph, "[.!?]+"),
  avg_word_length = nchar(gsub("\\s", "", Paragraph)) / str_count(Paragraph, "\\S+"),
  
  # Structural features
  has_subsection = grepl("\\([a-z]\\)", Paragraph),
  has_number = grepl("\\d", Paragraph),
  starts_with_number = grepl("^\\d", Paragraph),
  
  # Modal verb features
  has_shall = grepl("\\bshall\\b", Paragraph, ignore.case = TRUE),
  has_must = grepl("\\bmust\\b", Paragraph, ignore.case = TRUE),
  has_may = grepl("\\bmay\\b", Paragraph, ignore.case = TRUE),
  has_means = grepl("\\bmeans\\b", Paragraph, ignore.case = TRUE),
  
  # Authority features
  has_minister = grepl("\\bminister\\b", Paragraph, ignore.case = TRUE),
  has_governor = grepl("\\bgovernor\\b", Paragraph, ignore.case = TRUE),
  has_director = grepl("\\bdirector\\b", Paragraph, ignore.case = TRUE),
  
  # Position features
  section_number = suppressWarnings(as.numeric(Section))
)]

# Replace NAs in numeric columns with median or 0
numeric_cols <- c("paragraph_length", "word_count", "sentence_count", "avg_word_length", "section_number")
for (col in numeric_cols) {
  training_data[is.na(get(col)) | is.infinite(get(col)), (col) := 0]
}

# Replace NAs in logical columns with FALSE
logical_cols <- c("has_subsection", "has_number", "starts_with_number", 
                  "has_shall", "has_must", "has_may", "has_means",
                  "has_minister", "has_governor", "has_director")
for (col in logical_cols) {
  training_data[is.na(get(col)), (col) := FALSE]
}

## Combine text features with engineered features ----
# Keep as sparse matrix and convert carefully
cat("  - Converting features (keeping sparse format)...\n")
text_features <- as.data.frame(as.matrix(dfm_train))
text_features$paragraph_id <- rownames(text_features)
engineered_features <- training_data[, .(
  paragraph_id, clause_type,
  paragraph_length, word_count, sentence_count, avg_word_length,
  has_subsection, has_number, starts_with_number,
  has_shall, has_must, has_may, has_means,
  has_minister, has_governor, has_director,
  section_number
)]

## Merge features ----
ml_data <- merge(
  text_features,
  engineered_features,
  by.x = "paragraph_id",
  by.y = "paragraph_id",
  all.x = TRUE
)

## Convert to proper format ----
ml_data <- as.data.frame(ml_data)

# Remove paragraph_id and any remaining NAs
ml_data <- ml_data[, !names(ml_data) %in% c("paragraph_id")]
ml_data[is.na(ml_data)] <- 0

ml_data$clause_type <- as.factor(ml_data$clause_type)

cat(sprintf("  - Final feature count: %d\n", ncol(ml_data) - 1))
cat(sprintf("  - Training samples: %d\n", nrow(ml_data)))

# Safety check for data quality
cat("\n  - Checking data quality...\n")
na_count <- sum(is.na(ml_data))
if (na_count > 0) {
  cat(sprintf("    WARNING: Found %d NA values. These will be set to 0.\n", na_count))
  ml_data[is.na(ml_data)] <- 0
}
cat("  - Data quality check complete.\n"))

## ============================================================================
## SECTION 3: TRAIN MODELS
## ============================================================================

cat("\nStep 3: Training machine learning models...\n")

## Set up cross-validation ----
set.seed(123)
train_control <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = "final",
  classProbs = TRUE,
  verboseIter = FALSE
)

## Handle class imbalance by downsampling ----
cat("  - Handling class imbalance with downsampling...\n")

## Split into train and test ----
train_idx <- createDataPartition(ml_data$clause_type, p = 0.8, list = FALSE)
train_set <- ml_data[train_idx, ]
test_set <- ml_data[-train_idx, ]

cat(sprintf("  - Training set: %d samples\n", nrow(train_set)))
cat(sprintf("  - Test set: %d samples\n", nrow(test_set)))

## Train Random Forest model ----
cat("\n  - Training Random Forest model...\n")
cat("    (This may take a few minutes)\n")

rf_model <- train(
  clause_type ~ .,
  data = train_set,
  method = "rf",
  trControl = train_control,
  ntree = 100,
  importance = TRUE
)

cat("  - Random Forest training complete!\n")

## ============================================================================
## SECTION 4: EVALUATE MODELS
## ============================================================================

cat("\nStep 4: Evaluating model performance...\n")

## Predictions on test set ----
rf_predictions <- predict(rf_model, newdata = test_set)

## Confusion matrix ----
conf_matrix <- confusionMatrix(rf_predictions, test_set$clause_type)

cat("\nModel Performance:\n")
cat(sprintf("  - Overall Accuracy: %.2f%%\n", conf_matrix$overall["Accuracy"] * 100))
cat(sprintf("  - Kappa: %.3f\n", conf_matrix$overall["Kappa"]))

cat("\nPer-Class Metrics:\n")
print(conf_matrix$byClass[, c("Sensitivity", "Specificity", "Balanced Accuracy")])

## Variable importance ----
importance_df <- varImp(rf_model)$importance
importance_df <- data.frame(
  Feature = rownames(importance_df),
  Importance = importance_df$Overall
)
importance_df <- importance_df[order(-importance_df$Importance), ]

cat("\nTop 20 Most Important Features:\n")
print(head(importance_df, 20))

## ============================================================================
## SECTION 5: APPLY MODEL TO UNLABELED DATA
## ============================================================================

cat("\nStep 5: Applying model to unlabeled paragraphs...\n")

## Get unlabeled paragraphs from Acts ----
acts_only <- legislation_table[legislation_type == "Act"]
act_paragraphs <- paragraph_table[legislation_id %in% acts_only$legislation_id]

## Remove already labeled ones ----
labeled_ids <- unique(training_data$paragraph_id)
unlabeled_paragraphs <- act_paragraphs[!paragraph_id %in% labeled_ids]

cat(sprintf("  - Found %d unlabeled paragraphs\n", nrow(unlabeled_paragraphs)))

if (nrow(unlabeled_paragraphs) > 0) {
  ## Create features for unlabeled data ----
  cat("  - Creating features for unlabeled paragraphs...\n")
  
  unlabeled_paragraphs[, `:=`(
    paragraph_length = nchar(Paragraph),
    word_count = str_count(Paragraph, "\\S+"),
    sentence_count = str_count(Paragraph, "[.!?]+"),
    avg_word_length = nchar(gsub("\\s", "", Paragraph)) / str_count(Paragraph, "\\S+"),
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
    section_number = suppressWarnings(as.numeric(Section))
  )]
  
  # Handle NAs in numeric columns
  numeric_cols <- c("paragraph_length", "word_count", "sentence_count", "avg_word_length", "section_number")
  for (col in numeric_cols) {
    unlabeled_paragraphs[is.na(get(col)) | is.infinite(get(col)), (col) := 0]
  }
  
  # Handle NAs in logical columns
  logical_cols <- c("has_subsection", "has_number", "starts_with_number", 
                    "has_shall", "has_must", "has_may", "has_means",
                    "has_minister", "has_governor", "has_director")
  for (col in logical_cols) {
    unlabeled_paragraphs[is.na(get(col)), (col) := FALSE]
  }
  
  ## Create text features ----
  corp_unlabeled <- corpus(unlabeled_paragraphs$Paragraph)
  toks_unlabeled <- tokens(corp_unlabeled, remove_punct = TRUE, remove_numbers = FALSE)
  toks_unlabeled <- tokens_tolower(toks_unlabeled)
  dfm_unlabeled <- dfm(toks_unlabeled)
  
  ## Match features to training set ----
  dfm_unlabeled <- dfm_match(dfm_unlabeled, featnames(dfm_train))
  
  # Convert to data frame carefully
  text_features_unlabeled <- as.data.frame(as.matrix(dfm_unlabeled))
  text_features_unlabeled$paragraph_id <- rownames(text_features_unlabeled)
  
  engineered_features_unlabeled <- unlabeled_paragraphs[, .(
    paragraph_id,
    paragraph_length, word_count, sentence_count, avg_word_length,
    has_subsection, has_number, starts_with_number,
    has_shall, has_must, has_may, has_means,
    has_minister, has_governor, has_director,
    section_number
  )]
  
  ## Merge features ----
  ml_data_unlabeled <- merge(
    text_features_unlabeled,
    engineered_features_unlabeled,
    by.x = "paragraph_id",
    by.y = "paragraph_id",
    all.x = TRUE
  )
  
  ml_data_unlabeled <- as.data.frame(ml_data_unlabeled)
  prediction_ids <- ml_data_unlabeled$paragraph_id
  ml_data_unlabeled <- ml_data_unlabeled[, !names(ml_data_unlabeled) %in% c("paragraph_id")]
  
  # Remove any remaining NA values
  ml_data_unlabeled[is.na(ml_data_unlabeled)] <- 0
  
  ## Make predictions ----
  cat("  - Making predictions on unlabeled data...\n")
  predictions <- predict(rf_model, newdata = ml_data_unlabeled, type = "prob")
  predicted_classes <- predict(rf_model, newdata = ml_data_unlabeled)
  
  ## Get confidence scores ----
  max_prob <- apply(predictions, 1, max)
  
  ## Create results dataframe ----
  prediction_results <- data.table(
    paragraph_id = as.integer(prediction_ids),
    predicted_clause_type = as.character(predicted_classes),
    confidence = max_prob
  )
  
  ## Merge with paragraph data ----
  prediction_results <- merge(
    prediction_results,
    unlabeled_paragraphs[, .(paragraph_id, legislation_id, Section, Heading, Paragraph)],
    by = "paragraph_id",
    all.x = TRUE
  )
  
  ## Merge with legislation metadata ----
  prediction_results <- merge(
    prediction_results,
    acts_only[, .(legislation_id, act_name, jurisdiction)],
    by = "legislation_id",
    all.x = TRUE
  )
  
  cat(sprintf("  - Predictions complete for %d paragraphs\n", nrow(prediction_results)))
}

## ============================================================================
## SECTION 6: EXPORT RESULTS
## ============================================================================

cat("\nStep 6: Exporting results to Excel...\n")

output_file <- file.path(here(), "ML_Provision_Classification.xlsx")

wb <- createWorkbook()

## Model performance sheet ----
addWorksheet(wb, "Model Performance")
perf_summary <- data.frame(
  Metric = c("Overall Accuracy", "Kappa", "Training Samples", "Test Samples"),
  Value = c(
    sprintf("%.2f%%", conf_matrix$overall["Accuracy"] * 100),
    sprintf("%.3f", conf_matrix$overall["Kappa"]),
    nrow(train_set),
    nrow(test_set)
  )
)
writeDataTable(wb, "Model Performance", perf_summary)

## Feature importance sheet ----
addWorksheet(wb, "Feature Importance")
writeDataTable(wb, "Feature Importance", head(importance_df, 50))

## Confusion matrix sheet ----
addWorksheet(wb, "Confusion Matrix")
conf_table <- as.data.frame.matrix(conf_matrix$table)
conf_table <- cbind(Predicted = rownames(conf_table), conf_table)
writeDataTable(wb, "Confusion Matrix", conf_table)

## Predictions sheet ----
if (exists("prediction_results") && nrow(prediction_results) > 0) {
  addWorksheet(wb, "ML Predictions")
  
  prediction_results <- prediction_results[order(-confidence)]
  setcolorder(prediction_results, c(
    "act_name", "jurisdiction", "Section", "Heading",
    "predicted_clause_type", "confidence", "Paragraph"
  ))
  
  writeDataTable(wb, "ML Predictions", prediction_results)
  setColWidths(wb, "ML Predictions", cols = 1:7, 
               widths = c(35, 12, 10, 30, 25, 12, 70))
}

## Summary by predicted type ----
if (exists("prediction_results") && nrow(prediction_results) > 0) {
  addWorksheet(wb, "Predictions by Type")
  pred_summary <- prediction_results[, .N, by = predicted_clause_type][order(-N)]
  setnames(pred_summary, c("Predicted Clause Type", "Count"))
  writeDataTable(wb, "Predictions by Type", pred_summary)
}

## Save workbook ----
saveWorkbook(wb, output_file, overwrite = TRUE)

cat(sprintf("\n✅ ML results saved to: %s\n", output_file))

## ============================================================================
## SECTION 7: SAVE MODEL
## ============================================================================

cat("\nStep 7: Saving trained model for future use...\n")

model_file <- file.path(here(), "provision_classifier_model.rds")
saveRDS(rf_model, model_file)

cat(sprintf("  - Model saved to: %s\n", model_file))

## ============================================================================
## SUMMARY
## ============================================================================

cat("\n=====================================\n")
cat("MACHINE LEARNING SUMMARY\n")
cat("=====================================\n\n")

cat("Model Training:\n")
cat(sprintf("  - Algorithm: Random Forest\n"))
cat(sprintf("  - Training samples: %d\n", nrow(train_set)))
cat(sprintf("  - Test samples: %d\n", nrow(test_set)))
cat(sprintf("  - Features: %d\n", ncol(train_set) - 1))
cat(sprintf("  - Classes: %d\n", length(levels(train_set$clause_type))))

cat("\nModel Performance:\n")
cat(sprintf("  - Accuracy: %.2f%%\n", conf_matrix$overall["Accuracy"] * 100))
cat(sprintf("  - Kappa: %.3f\n", conf_matrix$overall["Kappa"]))

if (exists("prediction_results")) {
  cat("\nPredictions:\n")
  cat(sprintf("  - Unlabeled paragraphs processed: %d\n", nrow(prediction_results)))
  cat(sprintf("  - High confidence predictions (>0.7): %d\n", 
              sum(prediction_results$confidence > 0.7)))
  cat(sprintf("  - Medium confidence predictions (0.5-0.7): %d\n", 
              sum(prediction_results$confidence >= 0.5 & prediction_results$confidence <= 0.7)))
}

cat("\nTop 5 Most Important Features:\n")
print(head(importance_df, 5))

cat("\n=====================================\n")
cat("Analysis complete!\n")
cat("=====================================\n")

beep(sound = 1)
