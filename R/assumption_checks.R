#' Check Statistical Assumptions for Linear Models
#'
#' @param model_results Output from train_model()
#' @return Character string with assumption check results (markdown format)
#' @export
check_regression_assumptions <- function(model_results) {

  if (!model_results$method %in% c("lm", "glmnet")) {
    return(NULL)  # Only applies to linear/regularized linear models
  }

  predictions <- model_results$predictions
  residuals <- predictions[[model_results$target]] - predictions$.pred
  fitted_values <- predictions$.pred

  checks <- list()
  has_warnings <- FALSE

  # 1. Normality of residuals (Shapiro-Wilk for small samples)
  if (length(residuals) < 5000 && length(residuals) >= 3) {
    shapiro_result <- tryCatch({
      shapiro.test(residuals)
    }, error = function(e) NULL)

    if (!is.null(shapiro_result)) {
      checks$normality <- list(
        test = "Shapiro-Wilk test for normality",
        p_value = shapiro_result$p.value,
        interpretation = if (shapiro_result$p.value < 0.05) {
          has_warnings <<- TRUE
          paste0("**Warning**: Residuals appear non-normal (p = ",
                 round(shapiro_result$p.value, 4), " < 0.05)")
        } else {
          paste0("Residuals appear normally distributed (p = ",
                 round(shapiro_result$p.value, 3), ")")
        }
      )
    }
  } else if (length(residuals) >= 5000) {
    checks$normality <- list(
      test = "Visual inspection recommended (sample too large for Shapiro-Wilk)",
      interpretation = "With large samples, examine Q-Q plot for normality. Small deviations are less concerning."
    )
  }

  # 2. Homoscedasticity (constant variance)
  # Split into low/high fitted value groups
  if (length(fitted_values) >= 10) {
    median_fit <- median(fitted_values, na.rm = TRUE)
    low_residuals <- residuals[fitted_values <= median_fit]
    high_residuals <- residuals[fitted_values > median_fit]

    if (length(low_residuals) >= 3 && length(high_residuals) >= 3) {
      var_low <- var(low_residuals, na.rm = TRUE)
      var_high <- var(high_residuals, na.rm = TRUE)

      # Avoid division by zero
      if (var_low > 1e-10 && var_high > 1e-10) {
        var_ratio <- var_high / var_low

        checks$homoscedasticity <- list(
          test = "Variance ratio between high and low fitted values",
          variance_ratio = var_ratio,
          interpretation = if (var_ratio > 2 || var_ratio < 0.5) {
            has_warnings <<- TRUE
            paste0("**Warning**: Variance appears non-constant (ratio = ",
                   round(var_ratio, 2), "). ",
                   "Check the residual plot below for funnel or cone shapes.")
          } else {
            paste0("Residual variance appears relatively constant (ratio = ",
                   round(var_ratio, 2), ")")
          }
        )
      }
    }
  }

  # 3. Generate report
  if (length(checks) == 0) {
    return(NULL)
  }

  report <- paste0(
    "\n\n## Regression Assumption Checks\n\n",
    "Linear regression assumes:\n",
    "1. **Linear relationships** between predictors and target\n",
    "2. **Independent observations** (no autocorrelation)\n",
    "3. **Homoscedasticity** (constant variance of residuals)\n",
    "4. **Normally distributed residuals**\n\n",

    "### Results:\n\n"
  )

  if (!is.null(checks$normality)) {
    report <- paste0(report,
      "**Normality of Residuals**\n\n",
      "- Test: ", checks$normality$test, "\n",
      "- ", checks$normality$interpretation, "\n"
    )

    if (grepl("Warning", checks$normality$interpretation)) {
      report <- paste0(report,
        "- Consider: log or square-root transformation of target, ",
        "or using tree-based models (no normality assumption)\n"
      )
    }
    report <- paste0(report, "\n")
  }

  if (!is.null(checks$homoscedasticity)) {
    report <- paste0(report,
      "**Homoscedasticity (Constant Variance)**\n\n",
      "- Test: ", checks$homoscedasticity$test, "\n",
      "- ", checks$homoscedasticity$interpretation, "\n"
    )

    if (grepl("Warning", checks$homoscedasticity$interpretation)) {
      report <- paste0(report,
        "- Consider: log transformation of target, ",
        "weighted regression, or robust regression methods\n"
      )
    }
    report <- paste0(report, "\n")
  }

  report <- paste0(report,
    "**What to do if assumptions are violated:**\n\n",
    "- **Mild violations**: Often okay with large sample sizes (Central Limit Theorem)\n",
    "- **Moderate violations**: Try transforming the target (log, sqrt, Box-Cox)\n",
    "- **Severe violations**: Use tree-based models (Random Forest, XGBoost) - no assumptions needed\n\n",

    if (has_warnings) {
      "**Note**: Assumption violations detected. Your predictions may still be useful, but:\n- Confidence intervals may be inaccurate\n- Feature importance may be misleading\n- Consider alternative models\n"
    } else {
      "**Result**: No major assumption violations detected!\n"
    }
  )

  report
}


#' Check for Overfitting
#'
#' @param model_results Output from train_model()
#' @return Character string with overfitting check (markdown format)
#' @export
check_overfitting <- function(model_results) {

  cv_metrics <- model_results$cv_metrics
  test_metrics <- model_results$test_metrics
  task_type <- model_results$task_type

  # Get comparable metric
  if (task_type == "regression") {
    cv_rmse <- cv_metrics %>%
      dplyr::filter(.metric == "rmse") %>%
      dplyr::pull(mean)
    test_rmse <- test_metrics %>%
      dplyr::filter(.metric == "rmse") %>%
      dplyr::pull(.estimate)

    if (length(cv_rmse) == 0 || length(test_rmse) == 0) {
      return(NULL)
    }

    degradation <- (test_rmse - cv_rmse) / cv_rmse

    report <- paste0(
      "\n\n## Overfitting Check\n\n",
      "**Cross-Validation RMSE**: ", round(cv_rmse, 4), "\n",
      "**Test RMSE**: ", round(test_rmse, 4), "\n",
      "**Degradation**: ", round(degradation * 100, 1), "%\n\n"
    )

    if (degradation > 0.15) {
      report <- paste0(report,
        "**Warning**: Test error is >15% worse than CV error. This suggests **overfitting**.\n\n",
        "**What this means**: Your model learned patterns specific to the training data that don't generalize.\n\n",
        "**What to do**:\n",
        "- Use regularization (Lasso/Ridge are already regularized)\n",
        "- Reduce model complexity (fewer features, shallower trees)\n",
        "- Collect more training data\n",
        "- Try ensemble methods\n"
      )
    } else if (degradation < -0.05) {
      report <- paste0(report,
        "**Unusual**: Test error is better than CV error by ", abs(round(degradation * 100, 1)), "%.\n\n",
        "This is unexpected and may indicate:\n",
        "- Lucky test set (not representative)\n",
        "- Error in CV procedure\n",
        "- Investigate further before trusting results\n"
      )
    } else {
      report <- paste0(report,
        "**Good**: Model generalizes well. CV and test performance are similar.\n\n",
        "This suggests your model has learned genuine patterns that transfer to new data.\n"
      )
    }

  } else {
    # Classification
    cv_acc <- cv_metrics %>%
      dplyr::filter(.metric == "accuracy") %>%
      dplyr::pull(mean)
    test_acc <- test_metrics %>%
      dplyr::filter(.metric == "accuracy") %>%
      dplyr::pull(.estimate)

    if (length(cv_acc) == 0 || length(test_acc) == 0) {
      return(NULL)
    }

    degradation <- (cv_acc - test_acc)  # Accuracy: higher is better

    report <- paste0(
      "\n\n## Overfitting Check\n\n",
      "**Cross-Validation Accuracy**: ", round(cv_acc, 4), "\n",
      "**Test Accuracy**: ", round(test_acc, 4), "\n",
      "**Difference**: ", round(degradation * 100, 1), " percentage points\n\n"
    )

    if (degradation > 0.05) {
      report <- paste0(report,
        "**Warning**: Test accuracy is >5% worse than CV accuracy. This suggests **overfitting**.\n\n",
        "**What this means**: Your model memorized the training data rather than learning general patterns.\n\n",
        "**What to do**:\n",
        "- Use regularization\n",
        "- Reduce model complexity\n",
        "- Collect more training data\n",
        "- Check for data leakage\n"
      )
    } else if (degradation < -0.05) {
      report <- paste0(report,
        "**Unusual**: Test accuracy is better than CV accuracy.\n\n",
        "This shouldn't happen. Possible causes:\n",
        "- Test set is easier/different from training set\n",
        "- Data leakage\n",
        "- Random chance with small sample\n"
      )
    } else {
      report <- paste0(report,
        "**Good**: Model generalizes well. CV and test performance are very similar.\n"
      )
    }
  }

  report
}


#' Calculate Baseline Performance
#'
#' @param predictions Predictions dataframe
#' @param target_col Target column name
#' @param task_type "regression" or "classification"
#' @return List with baseline metrics
#' @export
calculate_baseline <- function(predictions, target_col, task_type) {

  if (task_type == "regression") {
    # Baseline: predict mean
    baseline_pred <- mean(predictions[[target_col]], na.rm = TRUE)
    baseline_rmse <- sqrt(mean((predictions[[target_col]] - baseline_pred)^2, na.rm = TRUE))
    baseline_rsq <- 0  # Predicting mean gives R² = 0

    return(list(
      baseline_strategy = "Predict mean of target",
      baseline_rmse = baseline_rmse,
      baseline_rsq = baseline_rsq
    ))

  } else {
    # Baseline: predict mode (most common class)
    class_counts <- table(predictions[[target_col]])
    baseline_class <- names(which.max(class_counts))
    baseline_acc <- max(class_counts) / sum(class_counts)

    return(list(
      baseline_strategy = paste("Always predict most common class:", baseline_class),
      baseline_accuracy = baseline_acc,
      baseline_class = baseline_class
    ))
  }
}
