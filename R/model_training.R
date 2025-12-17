#' Model Training Functions for LearnTidyML
#'
#' Functions to train ML models and return results in a standardized format
#'
#' @name model_training
#' @import tidymodels
#' @import dplyr
#' @import parsnip
#' @import workflows
#' @import tune
#' @import yardstick
#' @import rsample
NULL

#' Train a Model with Cross-Validation
#'
#' @param data Cleaned/prepped data frame
#' @param target_col Target variable name
#' @param method_engine Tidymodels engine name (e.g., "ranger", "glmnet")
#' @param task_type "classification" or "regression"
#' @param recipe A recipes::recipe object for preprocessing
#' @param cv_folds Number of cross-validation folds (default: 5)
#' @param tune_grid Number of hyperparameter combinations to try (default: 10)
#' @return A list containing model, metrics, predictions, and importance
#' @export
train_model <- function(data, target_col, method_engine, task_type,
                        recipe = NULL, cv_folds = 5, tune_grid = 10) {

 # Validate inputs
 if (!target_col %in% names(data)) {
   stop("Target column not found in data")
 }

 # Validate model compatibility
 if (!is_model_compatible(method_engine, task_type)) {
   # Provide specific error message for common cases
   if (method_engine == "naivebayes" && task_type == "regression") {
     stop("Naive Bayes is only available for classification tasks, not regression. ",
          "Please select a different model such as 'glmnet', 'ranger', or 'xgboost'.")
   }
   stop(paste0("Model '", method_engine, "' is not compatible with task type '", task_type, "'. ",
               "Please select a different model."))
 }

 # Set seed for reproducibility
 set.seed(123)

 # Prepare target for classification
 if (task_type == "classification") {
   data[[target_col]] <- as.factor(data[[target_col]])
 }

 # Create data split with stratification tracking
 stratification_success <- FALSE
 stratification_message <- NULL

 data_split <- tryCatch({
   if (task_type == "classification") {
     split <- rsample::initial_split(data, prop = 0.75, strata = !!rlang::sym(target_col))
     stratification_success <- TRUE
     split
   } else {
     rsample::initial_split(data, prop = 0.75)
   }
 }, error = function(e) {
   # Fallback without stratification
   stratification_message <<- paste("Stratification failed:", e$message,
                                    "- using unstratified split. This may occur with very rare classes.")
   warning(stratification_message)
   rsample::initial_split(data, prop = 0.75)
 })

 train_data <- rsample::training(data_split)
 test_data <- rsample::testing(data_split)

 # Create recipe if not provided
 if (is.null(recipe)) {
   recipe <- create_default_recipe(train_data, target_col, task_type)
 }

 # Create model specification
 model_spec <- create_model_spec(method_engine, task_type)

 # Create workflow
 wf <- workflows::workflow() %>%
   workflows::add_recipe(recipe) %>%
   workflows::add_model(model_spec)

 # Create cross-validation folds with error handling
 set.seed(456)
 cv_stratification_success <- FALSE

 cv_folds_obj <- tryCatch({
   if (task_type == "classification") {
     folds <- rsample::vfold_cv(train_data, v = cv_folds, strata = target_col)
     cv_stratification_success <- TRUE
     folds
   } else {
     rsample::vfold_cv(train_data, v = cv_folds)
   }
 }, error = function(e) {
   # Fallback without stratification
   warning("CV stratification failed: ", e$message,
           " - using unstratified folds. This may affect reliability with imbalanced data.")
   rsample::vfold_cv(train_data, v = cv_folds)
 })

 # Check if model needs tuning
 needs_tuning <- has_tune_params(model_spec)

 # Suppress warnings during training but capture them
 warnings_list <- character()

 if (needs_tuning) {
   # Tune the model with better error handling
   tune_results <- tryCatch({
     withCallingHandlers({
       tune::tune_grid(
         wf,
         resamples = cv_folds_obj,
         grid = tune_grid,
         control = tune::control_grid(
           verbose = FALSE,
           save_pred = TRUE,
           allow_par = FALSE,  # Disable parallel to avoid some issues
           extract = NULL
         )
       )
     }, warning = function(w) {
       warnings_list <<- c(warnings_list, conditionMessage(w))
       invokeRestart("muffleWarning")
     })
   }, error = function(e) {
     message("Tuning failed, fitting with defaults: ", e$message)
     NULL
   })

   if (!is.null(tune_results)) {
     # Get best parameters
     metric_to_use <- if(task_type == "regression") "rmse" else "roc_auc"

     # Check if we have valid results
     metrics_available <- tryCatch({
       tune::collect_metrics(tune_results)
     }, error = function(e) NULL)

     if (!is.null(metrics_available) && nrow(metrics_available) > 0) {
       best_params <- tryCatch({
         tune::select_best(tune_results, metric = metric_to_use)
       }, error = function(e) {
         # Try with accuracy for classification
         if (task_type == "classification") {
           tune::select_best(tune_results, metric = "accuracy")
         } else {
           NULL
         }
       })

       if (!is.null(best_params)) {
         # Finalize workflow
         final_wf <- tune::finalize_workflow(wf, best_params)

         # Get CV metrics for best config
         cv_metrics <- metrics_available %>%
           dplyr::filter(.config == best_params$.config[1])
       } else {
         final_wf <- wf
         cv_metrics <- metrics_available %>% head(1)
       }
     } else {
       final_wf <- wf
       cv_metrics <- NULL
     }
   } else {
     final_wf <- wf
     cv_metrics <- NULL
   }
 } else {
   final_wf <- wf

   # Fit resamples for CV metrics
   cv_results <- tryCatch({
     withCallingHandlers({
       tune::fit_resamples(
         wf,
         resamples = cv_folds_obj,
         control = tune::control_resamples(save_pred = TRUE, allow_par = FALSE)
       )
     }, warning = function(w) {
       warnings_list <<- c(warnings_list, conditionMessage(w))
       invokeRestart("muffleWarning")
     })
   }, error = function(e) {
     message("CV fitting failed: ", e$message)
     NULL
   })

   cv_metrics <- if (!is.null(cv_results)) {
     tryCatch({
       tune::collect_metrics(cv_results)
     }, error = function(e) NULL)
   } else {
     NULL
   }
 }

 # Fit final model on full training data
 final_fit <- tryCatch({
   parsnip::fit(final_wf, data = train_data)
 }, error = function(e) {
   stop("Failed to fit final model: ", e$message)
 })

 # Make predictions on test data
 predictions <- tryCatch({
   predict(final_fit, test_data)
 }, error = function(e) {
   stop("Failed to generate predictions: ", e$message)
 })

 # Add probability predictions for classification
 if (task_type == "classification") {
   prob_preds <- tryCatch({
     predict(final_fit, test_data, type = "prob")
   }, error = function(e) NULL)

   if (!is.null(prob_preds)) {
     predictions <- dplyr::bind_cols(predictions, prob_preds)
   }
 }

 # Combine predictions with actual values
 predictions <- dplyr::bind_cols(
   predictions,
   test_data %>% dplyr::select(dplyr::all_of(target_col))
 )

 # Calculate test metrics
 test_metrics <- calculate_test_metrics(predictions, target_col, task_type)

 # Extract feature importance (if available)
 importance <- extract_importance(final_fit, method_engine)

 # Return results
 list(
   model = final_fit,
   train_data = train_data,
   test_data = test_data,
   predictions = predictions,
   cv_metrics = cv_metrics,
   test_metrics = test_metrics,
   importance = importance,
   method = method_engine,
   task_type = task_type,
   target = target_col,
   warnings = unique(warnings_list),
   n_train = nrow(train_data),
   n_test = nrow(test_data),
   stratification_used = stratification_success,
   cv_stratification_used = cv_stratification_success,
   stratification_message = stratification_message
 )
}


#' Create Default Recipe
#'
#' @param data Training data
#' @param target_col Target column name
#' @param task_type Task type
#' @return A recipe object
#' @keywords internal
create_default_recipe <- function(data, target_col, task_type) {

 # Create formula
 formula_obj <- stats::reformulate(".", response = target_col)

 rec <- recipes::recipe(formula_obj, data = data) %>%
   # Handle missing values FIRST
   recipes::step_impute_median(recipes::all_numeric_predictors()) %>%
   recipes::step_impute_mode(recipes::all_nominal_predictors()) %>%
   # Handle new levels and collapse rare categories
   recipes::step_novel(recipes::all_nominal_predictors()) %>%
   recipes::step_other(recipes::all_nominal_predictors(), threshold = 0.05, other = "OTHER") %>%
   # Remove unused factor levels (prevents "Dropped unused factor level" warnings)
   recipes::step_mutate_at(recipes::all_nominal_predictors(), fn = droplevels) %>%
   # Convert to dummy variables
   recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = FALSE) %>%
   # Remove zero variance BEFORE normalizing (prevents zero variance scaling warnings)
   recipes::step_zv(recipes::all_predictors()) %>%
   # NOW normalize (after removing zero variance columns)
   recipes::step_normalize(recipes::all_numeric_predictors()) %>%
   # Remove highly correlated predictors
   recipes::step_corr(recipes::all_numeric_predictors(), threshold = 0.9)

 rec
}


#' Create Model Specification
#'
#' @param engine Tidymodels engine name
#' @param task_type Task type
#' @return A parsnip model specification
#' @keywords internal
create_model_spec <- function(engine, task_type) {

 mode <- task_type

 spec <- switch(
   engine,

   # GLMNet (Lasso/Ridge) - simpler tuning
   "glmnet" = {
     if (task_type == "regression") {
       parsnip::linear_reg(penalty = tune::tune(), mixture = 1) %>%
         parsnip::set_engine("glmnet") %>%
         parsnip::set_mode("regression")
     } else {
       # Use multinom_reg for classification (handles both binary and multiclass)
       parsnip::multinom_reg(penalty = tune::tune(), mixture = 1) %>%
         parsnip::set_engine("glmnet") %>%
         parsnip::set_mode("classification")
     }
   },

   # Random Forest - reduced complexity
   "ranger" = {
     parsnip::rand_forest(
       mtry = tune::tune(),
       trees = 300,
       min_n = tune::tune()
     ) %>%
       parsnip::set_engine("ranger", importance = "impurity") %>%
       parsnip::set_mode(mode)
   },

   # XGBoost - simplified tuning with constraints for speed
   "xgboost" = {
     parsnip::boost_tree(
       trees = 100,  # Fixed to 100 trees for speed
       learn_rate = tune::tune(),
       tree_depth = tune::tune(),
       min_n = 5  # Minimum observations per node for faster training
     ) %>%
       parsnip::set_engine("xgboost",
                          nthread = 1,  # Use single thread for stability
                          verbosity = 0) %>%  # Suppress output
       parsnip::set_mode(mode)
   },

   # SVM - simplified
   "kernlab" = {
     parsnip::svm_rbf(
       cost = tune::tune()
     ) %>%
       parsnip::set_engine("kernlab") %>%
       parsnip::set_mode(mode)
   },

   # KNN
   "kknn" = {
     parsnip::nearest_neighbor(
       neighbors = tune::tune()
     ) %>%
       parsnip::set_engine("kknn") %>%
       parsnip::set_mode(mode)
   },

   # Decision Tree
   "rpart" = {
     parsnip::decision_tree(
       cost_complexity = tune::tune(),
       tree_depth = tune::tune()
     ) %>%
       parsnip::set_engine("rpart") %>%
       parsnip::set_mode(mode)
   },

   # Naive Bayes - no tuning needed
   "naivebayes" = {
     parsnip::naive_Bayes() %>%
       parsnip::set_engine("naivebayes") %>%
       parsnip::set_mode("classification")
   },

   # Default: simple linear/logistic
   {
     if (task_type == "regression") {
       parsnip::linear_reg() %>%
         parsnip::set_engine("lm") %>%
         parsnip::set_mode("regression")
     } else {
       parsnip::logistic_reg() %>%
         parsnip::set_engine("glm") %>%
         parsnip::set_mode("classification")
     }
   }
 )

 spec
}


#' Check if Model Has Tuning Parameters
#'
#' @param spec Model specification
#' @return Logical
#' @keywords internal
has_tune_params <- function(spec) {
 tryCatch({
   params <- tune::extract_parameter_set_dials(spec)
   nrow(params) > 0
 }, error = function(e) FALSE)
}


#' Calculate Test Metrics
#'
#' @param predictions Predictions with actual values
#' @param target_col Target column name
#' @param task_type Task type
#' @return A tibble of metrics
#' @keywords internal
calculate_test_metrics <- function(predictions, target_col, task_type) {

 truth_col <- target_col

 if (task_type == "regression") {

   result <- tryCatch({
     tibble::tibble(
       .metric = c("rmse", "rsq", "mae"),
       .estimator = "standard",
       .estimate = c(
         yardstick::rmse_vec(predictions[[truth_col]], predictions$.pred),
         yardstick::rsq_vec(predictions[[truth_col]], predictions$.pred),
         yardstick::mae_vec(predictions[[truth_col]], predictions$.pred)
       )
     )
   }, error = function(e) {
     tibble::tibble(.metric = "error", .estimator = "standard", .estimate = NA)
   })

 } else {
   # Classification
   predictions <- predictions %>%
     dplyr::mutate(
       !!truth_col := as.factor(!!rlang::sym(truth_col)),
       .pred_class = as.factor(.pred_class)
     )

   # Basic metrics
   result <- tryCatch({
     acc <- yardstick::accuracy_vec(predictions[[truth_col]], predictions$.pred_class)

     metrics_df <- tibble::tibble(
       .metric = "accuracy",
       .estimator = "standard",
       .estimate = acc
     )

     # Try to add precision, recall, f1
     prec <- tryCatch({
       yardstick::precision_vec(predictions[[truth_col]], predictions$.pred_class)
     }, error = function(e) NA)

     rec <- tryCatch({
       yardstick::recall_vec(predictions[[truth_col]], predictions$.pred_class)
     }, error = function(e) NA)

     if (!is.na(prec)) {
       metrics_df <- dplyr::bind_rows(metrics_df,
         tibble::tibble(.metric = "precision", .estimator = "standard", .estimate = prec))
     }
     if (!is.na(rec)) {
       metrics_df <- dplyr::bind_rows(metrics_df,
         tibble::tibble(.metric = "recall", .estimator = "standard", .estimate = rec))
     }
     if (!is.na(prec) && !is.na(rec) && (prec + rec) > 0) {
       f1 <- 2 * (prec * rec) / (prec + rec)
       metrics_df <- dplyr::bind_rows(metrics_df,
         tibble::tibble(.metric = "f_meas", .estimator = "standard", .estimate = f1))
     }

     # Try AUC if probability columns exist
     prob_cols <- names(predictions)[grepl("^\\.pred_", names(predictions)) & names(predictions) != ".pred_class"]
     if (length(prob_cols) >= 1) {
       auc <- tryCatch({
         if (length(prob_cols) == 2) {
           # Binary classification: use the second class probability
           yardstick::roc_auc_vec(predictions[[truth_col]], predictions[[prob_cols[2]]])
         } else if (length(prob_cols) > 2) {
           # Multiclass classification: use Hand-Till method for multiclass AUC
           # Create a matrix of probability columns for roc_auc calculation
           prob_matrix <- as.matrix(predictions[, prob_cols])
           # yardstick's roc_auc with estimator = "hand_till" for multiclass
           # We need to use the data frame version for multiclass
           pred_with_probs <- predictions %>%
             dplyr::select(dplyr::all_of(c(truth_col, prob_cols)))
           yardstick::roc_auc(pred_with_probs,
                             truth = !!rlang::sym(truth_col),
                             dplyr::all_of(prob_cols),
                             estimator = "hand_till")$.estimate
         } else {
           NA
         }
       }, error = function(e) {
         # If multiclass AUC fails, return NA silently
         NA
       })

       if (!is.na(auc)) {
         estimator_type <- if (length(prob_cols) > 2) "multiclass" else "binary"
         metrics_df <- dplyr::bind_rows(metrics_df,
           tibble::tibble(.metric = "roc_auc", .estimator = estimator_type, .estimate = auc))
       }
     }

     metrics_df

   }, error = function(e) {
     tibble::tibble(.metric = "accuracy", .estimator = "standard", .estimate = NA)
   })
 }

 result
}


#' Extract Feature Importance
#'
#' @param fitted_model Fitted workflow
#' @param engine Model engine
#' @return A tibble of feature importance or NULL
#' @keywords internal
extract_importance <- function(fitted_model, engine) {

 tryCatch({
   # Extract the fitted model from workflow
   model_fit <- workflows::extract_fit_parsnip(fitted_model)

   # Use vip package if available
   if (requireNamespace("vip", quietly = TRUE)) {
     imp <- tryCatch({
       vip::vi(model_fit)
     }, error = function(e) NULL)

     if (!is.null(imp) && nrow(imp) > 0) {
       return(imp %>%
                dplyr::arrange(dplyr::desc(Importance)) %>%
                head(20))
     }
   }

   # Fallback for specific engines
   if (engine == "ranger") {
     rf_fit <- model_fit$fit
     if (!is.null(rf_fit$variable.importance)) {
       imp <- tibble::tibble(
         Variable = names(rf_fit$variable.importance),
         Importance = as.numeric(rf_fit$variable.importance)
       ) %>%
         dplyr::arrange(dplyr::desc(Importance)) %>%
         head(20)
       return(imp)
     }
   }

   if (engine == "glmnet") {
     coefs <- tryCatch({
       coef(model_fit$fit)
     }, error = function(e) NULL)

     if (!is.null(coefs)) {
       coef_df <- as.matrix(coefs)
       imp <- tibble::tibble(
         Variable = rownames(coef_df),
         Importance = abs(coef_df[, ncol(coef_df)])
       ) %>%
         dplyr::filter(Variable != "(Intercept)", Importance > 0) %>%
         dplyr::arrange(dplyr::desc(Importance)) %>%
         head(20)
       return(imp)
     }
   }

   NULL

 }, error = function(e) {
   message("Could not extract importance: ", e$message)
   NULL
 })
}


#' Compare Multiple Models
#'
#' @param data Cleaned data
#' @param target_col Target column
#' @param task_type Task type
#' @param engines Vector of engine names to compare
#' @param recipe Recipe object (optional)
#' @param cv_folds Number of CV folds
#' @param tune_grid Tuning grid size
#' @param progress_callback Function to call with progress updates
#' @return A list with comparison results
#' @export
compare_models <- function(data, target_col, task_type,
                          engines = c("glmnet", "ranger", "xgboost"),
                          recipe = NULL,
                          cv_folds = 5,
                          tune_grid = 5,
                          progress_callback = NULL) {

 results <- list()
 n_engines <- length(engines)

 for (i in seq_along(engines)) {
   eng <- engines[i]

   if (!is.null(progress_callback)) {
     progress_callback(i / n_engines, paste("Training:", get_method_display_name(eng)))
   }

   message("Training: ", eng)

   result <- tryCatch({
     train_model(
       data = data,
       target_col = target_col,
       method_engine = eng,
       task_type = task_type,
       recipe = recipe,
       cv_folds = cv_folds,
       tune_grid = tune_grid
     )
   }, error = function(e) {
     message("Failed to train ", eng, ": ", e$message)
     NULL
   })

   if (!is.null(result)) {
     results[[eng]] <- result
   }
 }

 # Create comparison summary
 comparison <- purrr::map_df(results, function(r) {
   r$test_metrics %>%
     dplyr::mutate(model = r$method)
 }) %>%
   tidyr::pivot_wider(
     id_cols = model,
     names_from = .metric,
     values_from = .estimate
   )

 # Sort comparison by performance (best first)
 if (task_type == "regression" && "rsq" %in% names(comparison)) {
   comparison <- comparison %>%
     dplyr::arrange(dplyr::desc(rsq))
   best_model <- comparison$model[1]
 } else if (task_type == "classification" && "accuracy" %in% names(comparison)) {
   comparison <- comparison %>%
     dplyr::arrange(dplyr::desc(accuracy))
   best_model <- comparison$model[1]
 } else {
   # Fallback if metrics are missing
   best_model <- comparison$model[1]
 }

 list(
   models = results,
   comparison = comparison,
   best_model = best_model,
   task_type = task_type
 )
}


#' Get Method Display Name
#' @param engine Engine name
#' @return Human-readable name
#' @export
get_method_display_name <- function(engine) {
 switch(
   engine,
   "glmnet" = "Lasso/Ridge Regression",
   "ranger" = "Random Forest",
   "xgboost" = "XGBoost",
   "kernlab" = "Support Vector Machine",
   "kknn" = "K-Nearest Neighbors",
   "rpart" = "Decision Tree",
   "naivebayes" = "Naive Bayes",
   engine
 )
}
