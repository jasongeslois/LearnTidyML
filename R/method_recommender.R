#' Recommend ML Methods
#' 
#' @param task_info Output from detect_ml_task()
#' @param data_profile Output from analyze_data()
#' @return A tibble of recommended methods with rationale
#' @export
recommend_methods <- function(task_info, data_profile) {
  
  recommendations <- tibble::tibble(
    method = character(),
    priority = integer(),
    rationale = character(),
    tidymodels_engine = character(),
    considerations = character()
  )
  
  if(task_info$task_type == "regression") {
    recommendations <- dplyr::bind_rows(
      recommendations,
      tibble::tibble(
        method = "Linear Regression (with Lasso)",
        priority = if(needs_regularization(task_info)) 1 else 2,
        rationale = "Good baseline; Lasso provides feature selection",
        tidymodels_engine = "glmnet",
        considerations = "Assumes linear relationships"
      ),
      tibble::tibble(
        method = "Random Forest",
        priority = 1,
        rationale = glue::glue("Handles non-linearity well; {task_info$n_samples} samples suitable"),
        tidymodels_engine = "ranger",
        considerations = "Can be slow with large datasets; less interpretable"
      ),
      tibble::tibble(
        method = "XGBoost",
        priority = if(task_info$n_samples > 1000) 1 else 2,
        rationale = "Often achieves strong performance; handles missing data",
        tidymodels_engine = "xgboost",
        considerations = "Requires careful tuning; less interpretable"
      ),
      tibble::tibble(
        method = "Support Vector Machine (SVM)",
        priority = if(task_info$n_samples < 5000 && task_info$n_predictors < 50) 1 else 3,
        rationale = "Effective in high-dimensional spaces; good for non-linear patterns",
        tidymodels_engine = "kernlab",
        considerations = "Slow with large datasets; requires feature scaling"
      ),
      tibble::tibble(
        method = "K-Nearest Neighbors (KNN)",
        priority = if(task_info$n_samples < 1000) 2 else 3,
        rationale = "Non-parametric approach; no training phase",
        tidymodels_engine = "kknn",
        considerations = "Sensitive to irrelevant features; slow predictions on large data"
      ),
      tibble::tibble(
        method = "Decision Tree",
        priority = 3,
        rationale = "Highly interpretable; captures non-linear patterns",
        tidymodels_engine = "rpart",
        considerations = "Prone to overfitting; consider using as baseline"
      )
    )
  } else if(task_info$task_type == "classification") {
    if(task_info$task_subtype == "binary") {
      recommendations <- dplyr::bind_rows(
        recommendations,
        tibble::tibble(
          method = "Logistic Regression (with Lasso)",
          priority = 1,
          rationale = "Interpretable baseline; Lasso for feature selection",
          tidymodels_engine = "glmnet",
          considerations = "Assumes linear decision boundary"
        ),
        tibble::tibble(
          method = "Random Forest",
          priority = 1,
          rationale = "Robust to outliers; handles interactions",
          tidymodels_engine = "ranger",
          considerations = "Less interpretable than logistic regression"
        ),
        tibble::tibble(
          method = "XGBoost",
          priority = if(task_info$n_samples > 1000) 1 else 2,
          rationale = "Often achieves strong performance",
          tidymodels_engine = "xgboost",
          considerations = "Needs tuning; prone to overfitting with small samples"
        ),
        tibble::tibble(
          method = "Support Vector Machine (SVM)",
          priority = if(task_info$n_samples < 5000) 1 else 3,
          rationale = "Effective for binary classification; handles non-linear boundaries",
          tidymodels_engine = "kernlab",
          considerations = "Requires scaled features; slow with large datasets"
        ),
        tibble::tibble(
          method = "Naive Bayes",
          priority = 2,
          rationale = "Fast, probabilistic; works well with many features",
          tidymodels_engine = "naivebayes",
          considerations = "Assumes feature independence; good for text data"
        ),
        tibble::tibble(
          method = "K-Nearest Neighbors (KNN)",
          priority = if(task_info$n_samples < 1000) 2 else 3,
          rationale = "Instance-based approach; no assumptions about data",
          tidymodels_engine = "kknn",
          considerations = "Slow with large data; sensitive to irrelevant features"
        ),
        tibble::tibble(
          method = "Decision Tree",
          priority = 3,
          rationale = "Highly interpretable decision rules",
          tidymodels_engine = "rpart",
          considerations = "Prone to overfitting; use as interpretable baseline"
        )
      )
    } else {
      # Multiclass
      recommendations <- dplyr::bind_rows(
        recommendations,
        tibble::tibble(
          method = "Multinomial Regression (with Lasso)",
          priority = 2,
          rationale = "Extension of logistic regression",
          tidymodels_engine = "glmnet",
          considerations = "May struggle with complex boundaries"
        ),
        tibble::tibble(
          method = "Random Forest",
          priority = 1,
          rationale = "Naturally handles multiclass problems",
          tidymodels_engine = "ranger",
          considerations = "Check class balance"
        ),
        tibble::tibble(
          method = "XGBoost",
          priority = 1,
          rationale = "Strong multiclass performance",
          tidymodels_engine = "xgboost",
          considerations = "Ensure adequate samples per class"
        ),
        tibble::tibble(
          method = "Naive Bayes",
          priority = 2,
          rationale = "Handles multiple classes naturally; fast training",
          tidymodels_engine = "naivebayes",
          considerations = "Good baseline; assumes feature independence"
        ),
        tibble::tibble(
          method = "K-Nearest Neighbors (KNN)",
          priority = if(task_info$n_samples < 1000) 2 else 3,
          rationale = "Effective multiclass classifier",
          tidymodels_engine = "kknn",
          considerations = "Requires balanced classes; slow with large data"
        ),
        tibble::tibble(
          method = "Decision Tree",
          priority = 3,
          rationale = "Interpretable multiclass rules",
          tidymodels_engine = "rpart",
          considerations = "Prone to overfitting; good for understanding patterns"
        )
      )
    }
  }
  
  # Filter to only compatible methods
  recommendations <- filter_compatible_methods(recommendations, task_info$task_type)

  recommendations %>%
    dplyr::arrange(priority)
}


#' Check Model Compatibility with Task Type
#'
#' Returns TRUE if the model engine is compatible with the task type
#' @param engine Tidymodels engine name
#' @param task_type "regression" or "classification"
#' @return Logical indicating compatibility
#' @export
is_model_compatible <- function(engine, task_type) {

  # Define compatibility matrix
  compatibility <- list(
    # Works for both regression and classification
    "glmnet" = c("regression", "classification"),
    "ranger" = c("regression", "classification"),
    "xgboost" = c("regression", "classification"),
    "kernlab" = c("regression", "classification"),
    "kknn" = c("regression", "classification"),
    "rpart" = c("regression", "classification"),

    # Classification ONLY
    "naivebayes" = c("classification"),

    # Regression ONLY (if we add any in future)
    "lm" = c("regression"),
    "glm" = c("classification")  # logistic regression
  )

  if (!engine %in% names(compatibility)) {
    # Unknown engine, assume compatible (defensive)
    warning("Unknown engine: ", engine, ". Assuming compatible.")
    return(TRUE)
  }

  task_type %in% compatibility[[engine]]
}


#' Filter Methods by Task Compatibility
#'
#' Removes methods that are not compatible with the task type
#' @param methods Tibble of recommended methods
#' @param task_type "regression" or "classification"
#' @return Filtered tibble of compatible methods
#' @export
filter_compatible_methods <- function(methods, task_type) {
  methods %>%
    dplyr::filter(purrr::map_lgl(tidymodels_engine, ~is_model_compatible(.x, task_type)))
}
