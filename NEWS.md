# LearnTidyML News

## LearnTidyML 0.3.0

Initial public release.

### Features

* **Data Import** - Support for CSV, TSV, Excel, RDS, SAS, Stata, and SPSS formats with 50MB file size limit
* **Data Profiling** - Comprehensive data quality assessment including column types, missing values, distributions, outliers, and correlations
* **Task Detection** - Automatic identification of ML problem type (binary classification, multi-class classification, regression)
* **Method Recommendations** - Intelligent algorithm suggestions with explanations of when to use each method
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
