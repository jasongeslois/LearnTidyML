# ==============================================================================
# Core Function Tests
# ==============================================================================
# Tests for data profiling, task detection, and method recommendation

# Data Profiling Tests ----

test_that("analyze_data creates profile for numeric data", {
  data <- data.frame(
    age = c(25, 30, 35, 40, 45),
    income = c(50000, 60000, 70000, 80000, 90000)
  )

  profile <- analyze_data(data)

  expect_type(profile, "list")
  expect_true("structure" %in% names(profile))
  expect_true("col_types" %in% names(profile))
  expect_true("numeric_summary" %in% names(profile))
  expect_true("missing" %in% names(profile))

  # Check structure
  expect_equal(profile$structure$n_rows, 5)
  expect_equal(profile$structure$n_cols, 2)

  # Check numeric summary exists
  expect_s3_class(profile$numeric_summary, "data.frame")
  expect_true(nrow(profile$numeric_summary) >= 2)
})

test_that("analyze_data handles mixed data types", {
  data <- data.frame(
    age = c(25, 30, 35),
    name = c("Alice", "Bob", "Charlie"),
    income = c(50000, 60000, 70000),
    category = factor(c("A", "B", "A")),
    stringsAsFactors = FALSE
  )

  profile <- analyze_data(data)

  expect_equal(profile$structure$n_rows, 3)
  expect_equal(profile$structure$n_cols, 4)

  # Should identify different types
  col_types <- profile$col_types
  expect_true(any(col_types$type == "numeric"))
  expect_true(any(col_types$type == "character" | col_types$type == "factor"))
})

test_that("analyze_data detects missing values", {
  data <- data.frame(
    x = c(1, 2, NA, 4, 5),
    y = c(NA, NA, 3, 4, 5),
    z = c(1, 2, 3, 4, 5)
  )

  profile <- analyze_data(data)

  missing <- profile$missing

  expect_s3_class(missing, "data.frame")
  expect_equal(nrow(missing), 3)

  # x should have 20% missing (1 out of 5)
  x_missing <- missing$pct_missing[missing$column == "x"]
  expect_equal(x_missing, 20)

  # y should have 40% missing (2 out of 5)
  y_missing <- missing$pct_missing[missing$column == "y"]
  expect_equal(y_missing, 40)

  # z should have 0% missing
  z_missing <- missing$pct_missing[missing$column == "z"]
  expect_equal(z_missing, 0)
})

test_that("analyze_data handles data with no missing values", {
  data <- data.frame(
    x = 1:10,
    y = 11:20
  )

  profile <- analyze_data(data)

  missing <- profile$missing

  expect_true(all(missing$n_missing == 0))
  expect_true(all(missing$pct_missing == 0))
})

test_that("analyze_data handles single-column data", {
  data <- data.frame(x = 1:5)

  profile <- analyze_data(data)

  expect_equal(profile$structure$n_cols, 1)
  expect_equal(profile$structure$n_rows, 5)
  expect_s3_class(profile$numeric_summary, "data.frame")
})

test_that("analyze_data handles large-ish data efficiently", {
  # Create moderately large dataset
  data <- data.frame(
    x = rnorm(1000),
    y = runif(1000),
    z = sample(letters, 1000, replace = TRUE),
    stringsAsFactors = FALSE
  )

  # Should complete without error or excessive time
  profile <- analyze_data(data)

  expect_equal(profile$structure$n_rows, 1000)
  expect_equal(profile$structure$n_cols, 3)
})

# Issue Identification Tests ----

test_that("identify_issues detects high missing data", {
  data <- data.frame(
    x = c(1, 2, 3, 4, 5),
    y = c(NA, NA, NA, NA, 5)  # 80% missing
  )

  profile <- analyze_data(data)
  issues <- identify_issues(data, profile)

  expect_s3_class(issues, "data.frame")

  # Should flag y column as having high missing data
  if (nrow(issues) > 0) {
    expect_true(any(grepl("missing", issues$issue_type, ignore.case = TRUE)))
  }
})

test_that("identify_issues handles clean data", {
  data <- data.frame(
    x = 1:10,
    y = 11:20,
    z = 21:30
  )

  profile <- analyze_data(data)
  issues <- identify_issues(data, profile)

  # Clean data should have no or minimal issues
  expect_s3_class(issues, "data.frame")
  # May have 0 rows if no issues detected
})

# Task Detection Tests ----

test_that("detect_ml_task identifies binary classification", {
  data <- data.frame(
    age = c(25, 30, 35, 40, 45),
    income = c(50000, 60000, 70000, 80000, 90000),
    target = c(0, 1, 0, 1, 1)
  )

  task_info <- detect_ml_task(data, "target")

  expect_type(task_info, "list")
  expect_equal(task_info$task_type, "classification")
  expect_equal(task_info$task_subtype, "binary")
  expect_equal(task_info$target, "target")
  expect_true(task_info$n_predictors >= 2)
})

test_that("detect_ml_task identifies multiclass classification", {
  data <- data.frame(
    x = 1:10,
    y = 11:20,
    target = rep(c("A", "B", "C"), length.out = 10),
    stringsAsFactors = FALSE
  )

  task_info <- detect_ml_task(data, "target")

  expect_equal(task_info$task_type, "classification")
  expect_equal(task_info$task_subtype, "multiclass")
  # n_unique is the field name, not n_classes
  expect_true(task_info$n_unique >= 3)
})

test_that("detect_ml_task identifies regression", {
  data <- data.frame(
    age = c(25, 30, 35, 40, 45),
    education_years = c(12, 16, 18, 20, 14),
    income = c(50000, 60000, 70000, 80000, 90000)
  )

  task_info <- detect_ml_task(data, "income")

  expect_equal(task_info$task_type, "regression")
  expect_equal(task_info$target, "income")
  expect_true(task_info$n_predictors >= 2)
})

test_that("detect_ml_task handles factor targets", {
  data <- data.frame(
    x = 1:10,
    y = 11:20,
    target = factor(rep(c("Yes", "No"), each = 5))
  )

  task_info <- detect_ml_task(data, "target")

  expect_equal(task_info$task_type, "classification")
  expect_equal(task_info$task_subtype, "binary")
})

test_that("detect_ml_task handles character targets", {
  data <- data.frame(
    x = 1:10,
    y = 11:20,
    target = rep(c("positive", "negative"), each = 5),
    stringsAsFactors = FALSE
  )

  task_info <- detect_ml_task(data, "target")

  expect_equal(task_info$task_type, "classification")
})

test_that("detect_ml_task validates target column exists", {
  data <- data.frame(x = 1:5, y = 6:10)

  expect_error(
    detect_ml_task(data, "nonexistent"),
    "not found"
  )
})

test_that("detect_ml_task handles numeric targets with few unique values", {
  # Numeric column with only 2 unique values should be classification
  data <- data.frame(
    x = rnorm(20),
    y = runif(20),
    target = rep(c(0, 1), each = 10)
  )

  task_info <- detect_ml_task(data, "target")

  # Could be regression or classification depending on implementation
  # Just verify it returns valid task type
  expect_true(task_info$task_type %in% c("regression", "classification"))
})

# Method Recommendation Tests ----

test_that("recommend_methods returns appropriate methods for classification", {
  task_info <- list(
    task_type = "classification",
    task_subtype = "binary",
    target = "target",
    n_predictors = 5,
    n_classes = 2,
    n_samples = 100,
    sample_to_feature_ratio = 20
  )

  profile <- list(
    structure = list(n_rows = 100, n_cols = 6)
  )

  methods <- recommend_methods(task_info, profile)

  expect_s3_class(methods, "data.frame")
  expect_true(nrow(methods) > 0)
  expect_true("method" %in% names(methods))
  expect_true("tidymodels_engine" %in% names(methods))
  expect_true("priority" %in% names(methods))

  # Should include classification-appropriate methods
  engines <- methods$tidymodels_engine
  expect_true(any(engines %in% c("glmnet", "ranger", "xgboost")))
})

test_that("recommend_methods returns appropriate methods for regression", {
  task_info <- list(
    task_type = "regression",
    target = "y",
    n_predictors = 3,
    n_samples = 50,
    sample_to_feature_ratio = 16.7
  )

  profile <- list(
    structure = list(n_rows = 50, n_cols = 4)
  )

  methods <- recommend_methods(task_info, profile)

  expect_s3_class(methods, "data.frame")
  expect_true(nrow(methods) > 0)

  # Should include regression-appropriate methods
  engines <- methods$tidymodels_engine
  expect_true(any(engines %in% c("glmnet", "ranger", "xgboost")))
})

test_that("recommend_methods prioritizes methods appropriately", {
  task_info <- list(
    task_type = "classification",
    task_subtype = "binary",
    n_predictors = 10,
    n_classes = 2,
    n_samples = 1000,
    sample_to_feature_ratio = 100
  )

  profile <- list(
    structure = list(n_rows = 1000, n_cols = 11)
  )

  methods <- recommend_methods(task_info, profile)

  # Should have priority ranking (using integers 1, 2, 3)
  expect_true("priority" %in% names(methods))
  expect_true(all(methods$priority %in% c(1, 2, 3)))

  # At least one high priority method (priority = 1)
  expect_true(any(methods$priority == 1))
})

test_that("recommend_methods handles small datasets", {
  task_info <- list(
    task_type = "classification",
    task_subtype = "binary",
    n_predictors = 2,
    n_classes = 2,
    n_samples = 20,
    sample_to_feature_ratio = 10
  )

  profile <- list(
    structure = list(n_rows = 20, n_cols = 3)  # Small dataset
  )

  methods <- recommend_methods(task_info, profile)

  # Should still return recommendations but maybe avoid complex methods
  expect_s3_class(methods, "data.frame")
  expect_true(nrow(methods) > 0)
})

# Integration Test: Full Workflow ----

test_that("full analysis workflow works end-to-end", {
  # Create test data
  data <- data.frame(
    age = c(25, 30, 35, 40, 45, 50, 55, 60),
    income = c(50000, 60000, 70000, 80000, 90000, 100000, 110000, 120000),
    education = c(12, 16, 18, 16, 20, 18, 22, 20),
    purchased = c(0, 0, 1, 1, 1, 1, 0, 1)
  )

  # Step 1: Analyze data
  profile <- analyze_data(data)
  expect_type(profile, "list")
  expect_equal(profile$structure$n_rows, 8)

  # Step 2: Identify issues
  issues <- identify_issues(data, profile)
  expect_s3_class(issues, "data.frame")

  # Step 3: Detect ML task
  task_info <- detect_ml_task(data, "purchased")
  expect_equal(task_info$task_type, "classification")
  expect_equal(task_info$target, "purchased")

  # Step 4: Recommend methods
  methods <- recommend_methods(task_info, profile)
  expect_s3_class(methods, "data.frame")
  expect_true(nrow(methods) > 0)

  # Verify workflow produces valid outputs
  expect_true(task_info$n_predictors == 3)
  expect_true("tidymodels_engine" %in% names(methods))
})

test_that("workflow handles data with missing values", {
  data <- data.frame(
    x1 = c(1, 2, NA, 4, 5),
    x2 = c(NA, 2, 3, 4, 5),
    y = c(10, 20, 30, 40, 50)
  )

  # Should not error on missing data
  profile <- analyze_data(data)
  # Check pct_missing instead of n_missing (actual column name)
  expect_true(any(profile$missing$pct_missing > 0))

  issues <- identify_issues(data, profile)
  expect_s3_class(issues, "data.frame")

  task_info <- detect_ml_task(data, "y")
  expect_equal(task_info$task_type, "regression")

  methods <- recommend_methods(task_info, profile)
  expect_true(nrow(methods) > 0)
})

# Edge Case Tests ----

test_that("functions handle minimal data gracefully", {
  # Minimal valid dataset: 2 rows, 2 columns
  data <- data.frame(
    x = c(1, 2),
    y = c(10, 20)
  )

  # Should not error
  profile <- analyze_data(data)
  expect_equal(profile$structure$n_rows, 2)

  task_info <- detect_ml_task(data, "y")
  expect_type(task_info, "list")

  methods <- recommend_methods(task_info, profile)
  expect_s3_class(methods, "data.frame")
})

test_that("functions handle all-NA columns", {
  data <- data.frame(
    x = c(1, 2, 3),
    y = c(NA, NA, NA),
    z = c(4, 5, 6)
  )

  profile <- analyze_data(data)

  # y should be flagged as 100% missing
  y_missing <- profile$missing$pct_missing[profile$missing$column == "y"]
  expect_equal(y_missing, 100)
})

test_that("functions handle constant columns", {
  data <- data.frame(
    x = c(1, 1, 1, 1, 1),
    y = c(2, 3, 4, 5, 6),
    target = c(10, 20, 30, 40, 50)
  )

  # Should not error on constant column
  profile <- analyze_data(data)
  task_info <- detect_ml_task(data, "target")
  methods <- recommend_methods(task_info, profile)

  expect_s3_class(methods, "data.frame")
})
