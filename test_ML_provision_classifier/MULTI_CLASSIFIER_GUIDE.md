# Multi-Category ML Classifier - User Guide

## What It Does

The **Multi-Category ML Classifier** trains **three separate machine learning models** to automatically classify legislation paragraphs into:

1. **Clause Types** (7 categories)
   - Administration, Application, & Structure
   - Authorization & Mandate
   - Designation
   - Instruction
   - Interpretation & Purpose
   - Licence, Permitting, & Exemptions
   - Prohibition, Restriction, or Limitation

2. **Management Domains** (17 categories)
   - Agriculture
   - Aquaculture and Hatcheries
   - Climate Change and Natural Disasters
   - Fisheries
   - Forest and Range
   - Governance Administration
   - Human Disturbance
   - Invasive or Problematic Species and Disease
   - Mining and Energy
   - Pollution
   - Restoration
   - Spatial Designation
   - Species Status and Assessment
   - Transportation Infrastructure
   - Water Use and Watercourse Modifications
   - And more...

3. **IUCN Threats** (Level 1 categories)
   - 1. Residential, Commercial & Recreation Areas
   - 2. Agriculture & Aquaculture
   - 3. Energy Production & Mining
   - 4. Transportation, Service & Security Corridors
   - 5. Biological Resource Use & Control
   - 6. Human Intrusions & Disturbances
   - 7. Natural System Management & Modifications
   - 8. Invasive / Other Problematic Species, Genes & Pathogens
   - 9. Pollution
   - 10. Natural Disasters
   - 11. Climate Change

## How It Works

### Step 1: Loads Your Existing Labels

The script reads from your database tables:
- `paragraph_label_table` - Your manual labels
- `clause_type_keywords` - Keyword-to-clause mappings
- `governance_keywords` - Keyword-to-domain mappings
- `management_domain_threat_table` - Domain-to-IUCN mappings

### Step 2: Prepares Training Data

For each classification task:
```
Clause Types:
├─ Finds paragraphs labeled with clause keywords
├─ Maps keywords to clause types
├─ Filters to single-label cases
└─ Requires 20+ examples per class

Management Domains:
├─ Finds paragraphs labeled with governance keywords
├─ Maps keywords to management domains
├─ Filters to single-label cases
└─ Requires 20+ examples per class

IUCN Threats:
├─ Finds paragraphs labeled with IUCN threats
├─ Maps through management domains to IUCN L1
├─ Filters to single-label cases
└─ Requires 20+ examples per class
```

### Step 3: Engineers Features

For each paragraph, extracts 500+ features:

**Text Features** (top 500 words):
- Most frequent words across all documents
- Filtered by: min 10 occurrences, in at least 5 documents

**Structural Features**:
- paragraph_length (character count)
- word_count
- sentence_count
- avg_word_length
- has_subsection (e.g., "(a)", "(b)")
- has_number
- starts_with_number

**Language Features**:
- has_shall, has_must, has_may (modality)
- has_means (definitions)
- has_prohibit (restrictions)
- has_permit (licensing)

**Authority Features**:
- has_minister
- has_governor
- has_director

**Domain Features** (for IUCN/Management):
- has_habitat
- has_species  
- has_water

### Step 4: Trains Three Models

Each model is a **Random Forest** with:
- 100 decision trees
- 5-fold cross-validation
- 80/20 train/test split

**Training happens in parallel**:
```
Model 1: Clause Types → 80% accuracy
Model 2: Management Domains → 75% accuracy
Model 3: IUCN Threats → 82% accuracy
```

### Step 5: Evaluates Performance

For each model, calculates:
- **Overall Accuracy**: % of correct predictions
- **Kappa Score**: Agreement beyond chance
- **Confusion Matrix**: Which classes get confused
- **Per-Class Metrics**: Sensitivity, Specificity

## Output: What You Get

### Excel File: `ML_Multi_Classification_Results.xlsx`

**Sheet 1: Model Summary**
```
| Model              | Training | Test | Classes | Accuracy | Kappa |
|--------------------|----------|------|---------|----------|-------|
| Clause Types       | 1,847    | 462  | 7       | 82.25%   | 0.781 |
| Management Domains | 892      | 223  | 12      | 76.68%   | 0.724 |
| IUCN Threats       | 1,034    | 259  | 10      | 81.47%   | 0.793 |
```

**Sheet 2-4: Individual Model Performance**
- Confusion matrices showing predicted vs actual
- Identify which categories are confused
- See where models excel or struggle

### Saved Models: `multi_classifier_models.rds`

All three trained models saved for future use:
```r
# Load models
models <- readRDS("multi_classifier_models.rds")

# Access individual models
clause_model <- models$clause_type
domain_model <- models$management_domain
iucn_model <- models$iucn_threat
```

## Usage

### Basic Usage

```r
# Run the script
source("ML_Multi_Classifier.R")
```

**Expected time**: 5-10 minutes depending on data size

### What Happens

```
Step 1: Loading data from database...
  - Loaded 45,823 paragraphs
  - Loaded 12,456 labels

Step 2: Preparing training data for each task...

  Task 1: CLAUSE TYPES
    Clause type distribution:
      Instruction: 1,245
      Authorization & Mandate: 892
      Licence, Permitting & Exemptions: 678
      ...
    - Training samples: 2,309 in 7 classes

  Task 2: MANAGEMENT DOMAINS
    Management domain distribution:
      Spatial Designation: 423
      Fisheries: 312
      ...
    - Training samples: 1,115 in 12 classes

  Task 3: IUCN THREATS
    IUCN L1 distribution:
      5. Biological Resource Use & Control: 445
      2. Agriculture & Aquaculture: 289
      ...
    - Training samples: 1,293 in 10 classes

Step 3: Preparing feature engineering pipeline...

Step 4: Training machine learning models...

  Training CLAUSE TYPE classifier...
    - Engineering features for Clause Types...
    Text features: 487
    Total features: 507
    Training: 1,847 | Test: 462
    ✓ Accuracy: 82.25%

  Training MANAGEMENT DOMAIN classifier...
    - Engineering features for Management Domains...
    Text features: 452
    Total features: 472
    Training: 892 | Test: 223
    ✓ Accuracy: 76.68%

  Training IUCN THREAT classifier...
    - Engineering features for IUCN Threats...
    Text features: 498
    Total features: 518
    Training: 1,034 | Test: 259
    ✓ Accuracy: 81.47%

Step 5: Creating comprehensive Excel output...
✅ Results saved to: ML_Multi_Classification_Results.xlsx
✅ Models saved to: multi_classifier_models.rds
```

## Interpreting Results

### Accuracy Scores

**What's Good?**
- **>80%**: Excellent! Model is reliable
- **70-80%**: Good. Review low-confidence predictions
- **60-70%**: Fair. Needs more training data or feature engineering
- **<60%**: Poor. Check if classes are too similar or data quality issues

### Confusion Matrix Example

```
Clause Types Confusion Matrix:

                Predicted →
Actual ↓        | Instruction | Authorization | Licence |
----------------|-------------|---------------|---------|
Instruction     |     245     |      12       |    3    |
Authorization   |      18     |     187       |    8    |
Licence         |       5     |      15       |   156   |
```

**Reading it**:
- Diagonal (245, 187, 156) = Correct predictions ✓
- Off-diagonal = Mistakes
- Row 2, Col 1 (18) = 18 "Authorization" paragraphs misclassified as "Instruction"

**Insight**: Model sometimes confuses Authorization with Instruction (both use "shall/may" language)

### Kappa Score

- **>0.8**: Almost perfect agreement
- **0.6-0.8**: Substantial agreement
- **0.4-0.6**: Moderate agreement
- **<0.4**: Poor agreement

## Common Use Cases

### Use Case 1: Initial Classification
```
Situation: You have 10,000 unlabeled paragraphs
Process:
1. Run multi-classifier
2. Models predict all three categories
3. Review high-confidence predictions (>0.7)
4. Manually review medium/low confidence
```

### Use Case 2: Quality Check
```
Situation: Check consistency of existing labels
Process:
1. Train on 80% of labeled data
2. Test on remaining 20%
3. Find disagreements between human and ML
4. Investigate: Is human label wrong? Or is ML missing something?
```

### Use Case 3: Active Learning
```
Situation: Want to improve models over time
Process:
1. Train initial models
2. Predict on unlabeled data
3. Manually review LOW confidence predictions
4. Add corrected labels to training set
5. Retrain → Better accuracy!
6. Repeat
```

## Minimum Data Requirements

For training to work, you need:

| Category | Min Labels | Recommended |
|----------|-----------|-------------|
| Each Clause Type | 20 | 50+ |
| Each Management Domain | 20 | 50+ |
| Each IUCN Threat | 20 | 50+ |

**If you have fewer**:
- Script will automatically exclude rare classes
- Combine similar categories
- Or label more examples for rare classes

## Comparison: Multi vs Single Classifier

### Single Classifier (ML_Provision_Classifier.R)
- ✓ Simple, focused on one task
- ✓ Easier to understand
- ✗ Must run three times for three tasks

### Multi Classifier (ML_Multi_Classifier.R)
- ✓ Trains all three models at once
- ✓ Comprehensive analysis
- ✓ Side-by-side performance comparison
- ✗ Takes longer to run
- ✗ More complex output

## Tips for Best Results

### 1. Balance Your Training Data
```
Good:
  Clause Type A: 150 examples
  Clause Type B: 140 examples
  Clause Type C: 130 examples

Bad:
  Clause Type A: 500 examples
  Clause Type B: 30 examples
  Clause Type C: 10 examples
```

### 2. Use Single-Label Examples
The script filters out paragraphs with multiple labels. If most of your paragraphs have multiple labels, you'll have less training data.

**Solution**: During manual labeling, try to assign primary label when possible.

### 3. Review Confusion Matrices
Look for patterns:
- Which classes are frequently confused?
- Add keywords that distinguish them
- Or consider merging very similar classes

### 4. Iterate
```
Round 1: Train → 75% accuracy
↓
Review errors → Add keywords
↓
Round 2: Train → 78% accuracy
↓
Review errors → Add training examples
↓
Round 3: Train → 82% accuracy ✓
```

## Troubleshooting

### "Not enough examples for class X"
**Problem**: Class has <20 labeled examples
**Solution**: 
- Label more examples for that class, OR
- Combine with similar class, OR
- Accept that class won't be included

### "Error: All classes have too few examples"
**Problem**: No class has 20+ examples
**Solution**: 
- Need to label more paragraphs
- Or lower threshold in script (line ~120: change `N >= 20` to `N >= 10`)

### Models have low accuracy (<70%)
**Possible causes**:
1. Classes are too similar (hard to distinguish)
2. Not enough training data
3. Keywords don't capture the concepts well

**Solutions**:
1. Add more distinguishing features
2. Label more examples
3. Review and improve keyword lists

### Out of memory error
**Problem**: Too many features for your RAM
**Solution**: 
- Script already limits to 500 features
- Can reduce further (line ~175: change `n = 500` to `n = 300`)

## Next Steps

After running the multi-classifier:

1. **Review Excel Output**
   - Check accuracy scores
   - Study confusion matrices
   - Identify problem areas

2. **Use Models for Prediction**
   - Load saved models
   - Apply to new/unlabeled paragraphs
   - Get predictions with confidence scores

3. **Improve Over Time**
   - Add more training examples
   - Retrain periodically
   - Track accuracy improvements

4. **Integrate with Workflow**
   - Use for first-pass classification
   - Human review of predictions
   - Update database with verified labels

## Questions?

Want me to explain:
1. How to use the saved models for predictions?
2. How to interpret specific confusion matrices?
3. How to add custom features for your domain?
4. How to tune the models for better performance?
