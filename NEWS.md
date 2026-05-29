# LearnTidyML News

## LearnTidyML 0.3.1

Statistical correctness fixes.

### Bug fixes

* **Class rebalancing no longer leaks across the train/test split.** Over- and
  under-sampling were previously applied to the full dataset *before* splitting,
  so duplicated rows could appear in both the training and test sets, producing
  optimistic and invalid performance estimates. Rebalancing is now applied
  inside the modeling recipe via `themis` steps (`skip = TRUE`), so it affects
  only the analysis folds and the final training data - never the
  assessment/test data. Requires the `themis` package (added to Suggests).
* **Fixed inverted binary ROC-AUC.** The AUC was computed from the second
  class's probability while yardstick defaults to the first factor level as the
  event, which inverted the value (reporting `1 - AUC`) and made it inconsistent
  with accuracy/precision/recall. It now uses the first level consistently.
* **R-squared now uses the traditional definition** (`rsq_trad`, i.e.
  1 - SS_res/SS_tot) instead of the correlation-based `rsq`. This matches how
  R-squared is described to users ("proportion of variance explained"), is 0
  when predicting the mean, and can go negative for models worse than that
  baseline.
* **Model comparison selects the best model by cross-validation performance**
  rather than the test set, avoiding optimistic selection-on-the-test-set bias.
  Classification now ranks by ROC-AUC (falling back to accuracy) instead of
  accuracy alone.
* **Random Forest hyperparameters are now actually tuned.** Data-dependent
  tuning parameters (notably `mtry`) are finalized against the preprocessed
  predictors before `tune_grid()`, preventing the silent fallback to default
  (untuned) hyperparameters.

## LearnTidyML 0.3.0

Initial public release.

### Features

* **Data Import** - Support for CSV, TSV, Excel, RDS, SAS, Stata, and SPSS formats with 50MB file size limit
* **Data Profiling** - Comprehensive data quality assessment including column types, missing values, distributions, outliers, and correlations
* **Task Detection** - Automatic identification of ML problem type (binary classification, multi-class classification, regression)
* **Method Recommendations** - Algorithm suggestions based on data characteristics, with explanations of when to use each method
* **Data Cleaning** - Interactive tools for handling missing values, outliers, scaling, and encoding
* **Model Training** - Single model training and multi-model comparison with cross-validation
* **Plain-English Explanations** - Results explained without jargon for educational use
* **Model Comparison** - Train and compare multiple algorithms simultaneously
* **Feature Importance** - Visualize which features contribute most to predictions
* **Assumption Checking** - Statistical validation for regression models (residuals, normality, homoscedasticity)

### Supported Methods

**Classification:**
- Logistic Regression
- Decision Tree
- Random Forest
- Gradient Boosting (XGBoost)
- Support Vector Machine
- K-Nearest Neighbors
- Naive Bayes

**Regression:**
- Linear Regression
- Ridge/Lasso Regression
- Decision Tree
- Random Forest
- Gradient Boosting (XGBoost)
- Support Vector Regression
- K-Nearest Neighbors

### Security

* Local execution only - data never leaves your machine
* File size limits to prevent resource abuse
* Input validation for uploaded files
* Session isolation and automatic cleanup
