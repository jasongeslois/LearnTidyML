#' Detect Machine Learning Task Type
#'
#' @param data A data frame
#' @param target_col Name of the target variable
#' @return A list with task type and recommendations
#' @export
detect_ml_task <- function(data, target_col) {

  # Validate target column
  validate_target_column(data, target_col)

  target_var <- data[[target_col]]

  # Edge case 1: All NA target
  if (all(is.na(target_var))) {
    stop("Target variable is entirely missing (all NA). Cannot train a model.")
  }

  # Edge case 2: Constant target
  n_unique_non_na <- length(unique(na.omit(target_var)))
  if (n_unique_non_na < 2) {
    stop("Target variable has only one unique value - cannot train model. ",
         "A model needs variation in the target to learn patterns.")
  }

  # Edge case 3: Date/time target
  if (inherits(target_var, c("Date", "POSIXct", "POSIXt"))) {
    stop("Target is a date/time variable. Consider:\n",
         "  - Converting to numeric (days since a reference date)\n",
         "  - Extracting features (year, month, day, weekday)\n",
         "  - Using time series forecasting methods instead")
  }

  # Determine task based on target variable
  task_info <- list(
    target = target_col,
    target_type = class(target_var)[1],
    n_unique = n_unique_non_na,
    task_type = NULL,
    task_subtype = NULL,
    confidence = NULL,
    note = NULL,
    imbalance_warning = NULL
  )

  # Classification vs Regression logic - IMPROVED HEURISTICS
  if(is.numeric(target_var)) {

    # Check if values are actually discrete (integers only)
    is_discrete <- all(target_var == floor(target_var), na.rm = TRUE)

    # Ratio of unique values to observations
    n_obs <- length(na.omit(target_var))
    uniqueness_ratio <- n_unique_non_na / n_obs

    # Improved heuristic considering multiple factors
    if (n_unique_non_na <= 2) {
      # Binary case - clearly classification
      task_info$task_type <- "classification"
      task_info$task_subtype <- "binary"
      task_info$confidence <- "high"

    } else if (is_discrete && n_unique_non_na <= 20 && uniqueness_ratio < 0.05) {
      # Discrete with few unique values relative to sample size
      task_info$task_type <- "classification"
      task_info$task_subtype <- "multiclass"
      task_info$confidence <- "high"
      task_info$note <- paste0("Detected ", n_unique_non_na, " discrete levels (",
                               round(uniqueness_ratio * 100, 1), "% unique values)")

    } else if (is_discrete && n_unique_non_na <= 20 && uniqueness_ratio < 0.10) {
      # Borderline case - could be ordinal or discrete regression
      task_info$task_type <- "classification"
      task_info$task_subtype <- "multiclass"
      task_info$confidence <- "medium"
      task_info$note <- paste0("Detected ", n_unique_non_na, " discrete levels. ",
                               "If these represent ordered categories (e.g., ratings 1-5), ",
                               "classification treats them as unordered. Consider this when ",
                               "interpreting results.")
      warning("Target has ", n_unique_non_na, " unique values (",
              round(uniqueness_ratio * 100, 1), "% of data). ",
              "Treating as classification, but verify this is appropriate for your use case.")

    } else {
      # Many unique values or continuous - regression
      task_info$task_type <- "regression"
      task_info$task_subtype <- "continuous"
      task_info$confidence <- "high"
      if (is_discrete && n_unique_non_na < 50) {
        task_info$note <- paste0("Numeric target with ", n_unique_non_na,
                                 " discrete values - treating as regression")
      }
    }

  } else if(is.character(target_var) | is.factor(target_var)) {
    task_info$task_type <- "classification"
    task_info$task_subtype <- if(n_unique_non_na == 2) "binary" else "multiclass"
    task_info$confidence <- "high"
  } else {
    stop("Target variable must be numeric, factor, or character. ",
         "Detected type: ", class(target_var)[1])
  }

  # Check for class imbalance in classification tasks
  if (task_info$task_type == "classification") {
    class_counts <- table(na.omit(target_var))
    class_props <- prop.table(class_counts)
    min_prop <- min(class_props)

    if (min_prop < 0.05) {
      task_info$imbalance_warning <- paste0(
        "Severe class imbalance detected: smallest class represents only ",
        round(min_prop * 100, 1), "% of data. ",
        "Consider focusing on Precision/Recall/F1 instead of Accuracy, ",
        "and potentially using resampling techniques."
      )
      warning(task_info$imbalance_warning)
    } else if (min_prop < 0.20) {
      task_info$imbalance_warning <- paste0(
        "Moderate class imbalance detected: smallest class represents ",
        round(min_prop * 100, 1), "% of data. Monitor class-specific metrics."
      )
    }
  }

  # Calculate dataset characteristics for method selection
  n_rows <- nrow(data)
  n_predictors <- ncol(data) - 1

  task_info$n_samples <- n_rows
  task_info$n_predictors <- n_predictors
  task_info$sample_to_feature_ratio <- n_rows / n_predictors

  # Check for inadequate sample size
  min_recommended <- if (task_info$task_type == "classification") {
    max(n_predictors * 10, 100)
  } else {
    max(n_predictors * 20, 100)
  }

  if (n_rows < min_recommended) {
    task_info$sample_size_warning <- paste0(
      "Sample size may be inadequate. You have ", n_rows, " observations and ",
      n_predictors, " predictors. Recommended minimum: ~", min_recommended,
      " observations. Your model may overfit. Consider collecting more data, ",
      "reducing predictors, or using regularization."
    )
    warning(task_info$sample_size_warning)
  }

  task_info
}

#' Detect if Regularization is Needed
#' 
#' @param task_info Output from detect_ml_task()
#' @return Logical indicating if regularization recommended
#' @export
needs_regularization <- function(task_info) {
  # High dimensional data benefits from regularization
  task_info$sample_to_feature_ratio < 20 | task_info$n_predictors > 50
}