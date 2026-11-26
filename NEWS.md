# LearnTidyML News

## LearnTidyML (development version)

### CRAN preparation

* `check_regression_assumptions()` no longer assigns `has_warnings` in the
  global environment (it used `<<-`), which CRAN does not allow. This also
  fixes the summary saying no violations were found after it had flagged a
  non-normal or non-constant-variance warning.
* `LICENSE` now uses CRAN's two-line "MIT + file LICENSE" format; the full MIT
  text moved to `LICENSE.md`.
* DESCRIPTION: added `URL` and `BugReports`, removed `LazyData` (the package
  has no `data/` directory), quoted software names and spelled out "machine
  learning" in the Description.
* The `tidymodels` meta-package moved from Imports to Suggests. The package
  only uses functions from parsnip, recipes, workflows, tune, yardstick,
  rsample and dials, which remain in Imports. `library(tidymodels)` still
  appears in the example code the app generates for users.
* Every exported function now has a runnable example, and the documentation
  for `app_server()`, `app_ui()`, `launch_LearnTidyML()` and
  `generate_tidymodels_code()` now describes what they return.
* Added `cran-comments.md`, and excluded `.github/`, `.gitattributes`,
  `CODE_OF_CONDUCT.md` and `CONTRIBUTING.md` from the built package.

## LearnTidyML 0.3.2

* Feature importance no longer uses the vip package, which was removed from
  CRAN in July 2026 and stopped the automated checks from installing. It now
  comes from each model's own fit: random forest, gradient boosting
  (xgboost), decision tree, Ridge/Lasso and linear/logistic regression.
  SVM, k-nearest neighbors and naive Bayes models show no importance table.
* The README logo is trimmed so it displays at the same size as the other
  packages' logos.

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
