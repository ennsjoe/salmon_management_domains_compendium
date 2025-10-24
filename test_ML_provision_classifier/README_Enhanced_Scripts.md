# Enhanced Legislative Implements Extraction - User Guide

## Overview

This package includes three R scripts that use advanced NLP and machine learning to extract and classify legislative provisions:

1. **Extract_Implements_Enhanced.R** - Enhanced extraction with linguistic analysis
2. **ML_Provision_Classifier.R** - Machine learning classification
3. **Extract_Implements.R** - Original baseline version

## What's New - Advanced Features

### 1. Context-Aware Extraction
- Analyzes 10 words before/after implement mentions
- Identifies "context boost" words (e.g., "make", "establish", "issue")
- Assigns confidence scores: `[high]` or `[medium]`

### 2. Dependency Parsing
- Identifies grammatical relationships (subject-verb-object)
- Examples: "Minister -> establish -> regulation"
- Helps understand WHO does WHAT to WHICH implement

### 3. Enhanced Pattern Matching
- 27 implement types (regulations, plans, permits, etc.)
- 14 official types (Minister, Governor in Council, etc.)
- 8 provision types (mandatory, discretionary, prohibited, etc.)

### 4. Machine Learning
- Trains on your existing keyword-labeled data
- Learns to classify provision types automatically
- Provides confidence scores for predictions

## Installation

Install required packages (one-time setup):

```r
install.packages(c(
  "DBI", "RSQLite", "data.table", "here", "openxlsx", "stringr",
  "udpipe", "quanteda", "quanteda.textstats", 
  "caret", "randomForest", "beepr"
))
```

## Usage

### Option 1: Quick Start (Enhanced Extraction Only)

Run the enhanced extraction with linguistic analysis:

```r
source("Extract_Implements_Enhanced.R")
```

**First run**: Will download English language model (~20MB) for dependency parsing.

**Output**: `Legislative_Implements_Enhanced.xlsx` with these sheets:
- **Implements (Enhanced)** - Main data with all features
- **Summary by Implement Type** - Count by implement
- **Summary by Official** - Count by official
- **Summary by Provision Type** - Count by provision type
- **Summary by Act** - Count by act
- **Implement-Official Co-occurrence** - Which officials manage which implements

### Option 2: Machine Learning Workflow

Step 1: Run enhanced extraction (see above)

Step 2: Train machine learning model:

```r
source("ML_Provision_Classifier.R")
```

**Output**: `ML_Provision_Classification.xlsx` with:
- **Model Performance** - Accuracy, metrics
- **Feature Importance** - Which features matter most
- **Confusion Matrix** - Where model succeeds/fails
- **ML Predictions** - Predictions on unlabeled paragraphs
- **Predictions by Type** - Summary statistics

**Also saves**: `provision_classifier_model.rds` for reuse

### Option 3: Compare with Baseline

Run the original version to compare:

```r
source("Extract_Implements.R")
```

**Output**: `Legislative_Implements.xlsx` (simpler, faster)

## Understanding the Output

### Implement Types with Confidence

Enhanced extraction shows confidence:
```
regulation[high]; plan[medium]
```

- `[high]` = Found with context boost words nearby
- `[medium]` = Found but no strong context

### Grammar Analysis Column

Shows subject-verb-object relationships:
```
minister -> establish -> regulation; board -> approve -> plan
```

Helps understand:
- WHO has authority
- WHAT action they take  
- WHICH implement they create/manage

### ML Confidence Scores

Machine learning predictions include confidence (0-1):
- `>0.7` = High confidence, likely correct
- `0.5-0.7` = Medium confidence, review recommended
- `<0.5` = Low confidence, uncertain

## Key Improvements Over Baseline

| Feature | Baseline | Enhanced | ML Version |
|---------|----------|----------|------------|
| Implement Detection | ✓ | ✓ | ✓ |
| Official Detection | ✓ | ✓ | ✓ |
| Confidence Scores | ✗ | ✓ | ✓ |
| Context Analysis | ✗ | ✓ | ✗ |
| Grammar Parsing | ✗ | ✓ | ✗ |
| Text Features | ✗ | ✗ | ✓ |
| Learning from Data | ✗ | ✗ | ✓ |

## Technical Details

### Enhanced Extraction Features

**Context Windows**: 
- Examines 10 words before/after each implement mention
- Looks for action verbs: establish, make, issue, develop, etc.

**Dependency Parsing**: 
- Uses Universal Dependencies framework
- Identifies grammatical roles (subjects, verbs, objects)
- Analyzes up to 3 verb phrases per paragraph

**Provision Types**:
- **Mandatory**: shall, must, required
- **Discretionary**: may, can, authorized
- **Prohibited**: shall not, must not, forbidden
- **Definition**: means, includes, defined as
- **Establishment**: establish, create
- **Authorization**: authorize, empower

### Machine Learning Features

**Text Features**:
- Bag-of-words with frequency filtering
- Top features by occurrence (min 5 term freq, 3 doc freq)

**Engineered Features**:
- Length: character count, word count, sentence count
- Structure: has subsections, has numbers
- Modals: has "shall", "must", "may", "means"
- Authority: mentions minister, governor, director
- Position: section number

**Model**:
- Algorithm: Random Forest (100 trees)
- Cross-validation: 5-fold
- Class balancing: Downsampling
- Train/test split: 80/20

## Performance Expectations

### Enhanced Extraction
- **Speed**: ~1 minute per 100 paragraphs (with dependency parsing)
- **Coverage**: Analyzes all Act paragraphs
- **Accuracy**: High precision, some false positives

### Machine Learning
- **Training time**: 2-10 minutes (depends on data size)
- **Prediction time**: Fast (<1 second per 100 paragraphs)
- **Accuracy**: Typically 70-85% on test set
- **Minimum data**: Needs 20+ examples per class

## Troubleshooting

### Error: "udpipe model not found"
**Solution**: Script will auto-download on first run. Need internet connection.

### Error: "Not enough examples for class X"
**Solution**: ML script filters to classes with 20+ examples. Some rare classes excluded.

### Warning: "No features retained"
**Solution**: Check that paragraphs have sufficient text content. Very short paragraphs may fail.

### Slow performance on dependency parsing
**Solution**: Dependency parsing analyzes first 100 implements only. Increase/decrease sample_size variable to adjust.

## Customization

### Add New Implement Types

Edit `implement_patterns` list in Enhanced script:

```r
your_implement = list(
  pattern = "\\b(your|pattern|here)\\b",
  context_boost = c("verb1", "verb2"),
  authority_link = TRUE
)
```

### Add New Officials

Edit `official_patterns` list:

```r
your_official = list(
  pattern = "\\b(Your Official Title)\\b",
  rank = 3,  # hierarchy level
  federal = TRUE  # or FALSE for provincial
)
```

### Adjust ML Features

In ML script, add features to training_data section:

```r
training_data[, your_feature := grepl("pattern", Paragraph)]
```

## Best Practices

1. **Start with Enhanced Extraction** - See what patterns emerge
2. **Review High Confidence Results** - Validate the `[high]` confidence matches
3. **Check Grammar Analysis** - Verify subject-verb-object relationships make sense
4. **Train ML Model** - After validating patterns, use ML for automation
5. **Iterate** - Use ML predictions to find gaps, add keywords, retrain

## Citation

If using this work in publications:

```
Enns, J., Lagasse, C. (2025). Enhanced Legislative Implements Extraction. 
LAPSE (Legislation Applicable to Pacific Salmon and Ecosystems) Project.
```

## Support

Questions or issues? Contact:
- Joe Enns
- Cory Lagasse

## License

This code is part of the LAPSE project. Please cite appropriately if used in research or publications.
