#' Results Explanation Functions for LearnTidyML
#'
#' Functions to generate human-readable explanations of model results
#'
#' @name model_explain
#' @import dplyr
#' @import ggplot2
NULL

#' Generate Plain English Model Summary
#'
#' @param model_results Output from train_model()
#' @return A character string with plain English explanation
#' @export
explain_model_results <- function(model_results) {

 task_type <- model_results$task_type
 target <- model_results$target
 method <- model_results$method
 test_metrics <- model_results$test_metrics
 importance <- model_results$importance

 method_name <- get_method_display_name(method)

 explanation <- paste0(
   "## Model Summary\n\n",
   "**Model Type:** ", method_name, "\n\n",
   "**Target Variable:** ", target, "\n\n",
   "**Training Set:** ", model_results$n_train, " observations\n\n",
   "**Test Set:** ", model_results$n_test, " observations\n\n"
 )

 # Add task-specific explanation
 if (task_type == "regression") {
   explanation <- paste0(explanation, explain_regression_results(test_metrics, target))
 } else {
   explanation <- paste0(explanation, explain_classification_results(test_metrics, target))
 }

 # Add feature importance explanation
 if (!is.null(importance) && nrow(importance) > 0) {
   explanation <- paste0(explanation, "\n\n", explain_feature_importance(importance, target, method))
 }

 # Add baseline comparison
 baseline_info <- calculate_baseline(model_results$predictions, target, task_type)
 explanation <- paste0(explanation, "\n\n", explain_baseline_comparison(model_results, baseline_info))

 # Add overfitting check
 overfitting_check <- check_overfitting(model_results)
 if (!is.null(overfitting_check)) {
   explanation <- paste0(explanation, overfitting_check)
 }

 # Add statistical assumption checks (for linear models)
 assumption_check <- check_regression_assumptions(model_results)
 if (!is.null(assumption_check)) {
   explanation <- paste0(explanation, assumption_check)
 }

 # Add stratification info if relevant
 if (!is.null(model_results$stratification_message)) {
   explanation <- paste0(explanation,
     "\n\n##  Data Splitting Note\n\n",
     model_results$stratification_message, "\n"
   )
 }

 # Add recommendations
 explanation <- paste0(explanation, "\n\n", generate_recommendations(model_results))

 explanation
}


#' Explain Regression Results in Plain English
#'
#' @param metrics Test metrics tibble
#' @param target Target variable name
#' @return Character string explanation
#' @keywords internal
explain_regression_results <- function(metrics, target) {

 # Extract metrics
 rmse_val <- metrics %>% dplyr::filter(.metric == "rmse") %>% dplyr::pull(.estimate)
 rsq_val <- metrics %>% dplyr::filter(.metric == "rsq") %>% dplyr::pull(.estimate)
 mae_val <- metrics %>% dplyr::filter(.metric == "mae") %>% dplyr::pull(.estimate)

 # Handle missing values
 if (length(rmse_val) == 0) rmse_val <- NA
 if (length(rsq_val) == 0) rsq_val <- NA
 if (length(mae_val) == 0) mae_val <- NA

 # Interpret R-squared
 rsq_interpretation <- dplyr::case_when(
   is.na(rsq_val) ~ "unable to calculate",
   rsq_val >= 0.9 ~ "very strong",
   rsq_val >= 0.7 ~ "strong",
   rsq_val >= 0.5 ~ "moderate",
   rsq_val >= 0.3 ~ "weak",
   TRUE ~ "very weak"
 )

 rsq_pct <- if (!is.na(rsq_val)) round(rsq_val * 100, 1) else NA

 explanation <- paste0(
   "---\n\n",
   "## Understanding Your Results\n\n",

   "### R-squared (R-squared) = ", if(!is.na(rsq_val)) round(rsq_val, 3) else "N/A", "\n\n",

   "**What it measures:** How much of the variation in ", target, " the model explains.\n\n",

   "**Plain English:** ",
   if (!is.na(rsq_val)) {
     paste0("The model explains **", rsq_pct, "%** of why ", target, " values differ from each other. ",
            "The remaining ", round(100 - rsq_pct, 1), "% is due to factors not in the model or random variation.")
   } else {
     "Could not be calculated."
   }, "\n\n",

   "**Interpretation Guide:**\n",
   "- R-squared > 0.9 = Very Strong (model captures most patterns)\n",
   "- R-squared 0.7-0.9 = Strong (model is useful for predictions)\n",
   "- R-squared 0.5-0.7 = Moderate (model captures some patterns)\n",
   "- R-squared 0.3-0.5 = Weak (model has limited predictive power)\n",
   "- R-squared < 0.3 = Very weak (consider different approach)\n\n",

   "**Your model:** ", rsq_interpretation, "\n\n",

   "**Important caveats:**\n",
   "- R-squared always increases when you add more variables (even those with no predictive value)\n",
   "- A high R-squared doesn't guarantee good predictions on new data\n",
   "- Some outcomes are inherently hard to predict (R-squared of 0.5 may still represent strong performance for that domain.)\n\n",

   "---\n\n",

   "### Root Mean Square Error (RMSE) = ", if(!is.na(rmse_val)) round(rmse_val, 2) else "N/A", "\n\n",

   "**What it measures:** The typical size of prediction errors, in the same units as ", target, ".\n\n",

   "**Plain English:** ",
   if (!is.na(rmse_val)) {
     paste0("On average, the model's predictions are off by about **", round(rmse_val, 2), " units**. ",
            "This is like saying 'expect predictions to typically miss by this much.'")
   } else {
     "Could not be calculated."
   }, "\n\n",

   "**How to interpret:**\n",
   "- Compare RMSE to the range of ", target, " values\n",
   "- RMSE should be small relative to typical values\n",
   "- Lower RMSE = more accurate predictions\n\n",

   "**Key insight:** RMSE penalizes large errors more than small ones (due to squaring). If you have a few predictions that are way off, RMSE will be higher than MAE.\n\n",

   "---\n\n",

   "### Mean Absolute Error (MAE) = ", if(!is.na(mae_val)) round(mae_val, 2) else "N/A", "\n\n",

   "**What it measures:** The average absolute difference between predictions and actual values.\n\n",

   "**Plain English:** ",
   if (!is.na(mae_val)) {
     paste0("If you made 100 predictions, the average miss would be **", round(mae_val, 2), " units** ",
            "(ignoring whether predictions were too high or too low).")
   } else {
     "Could not be calculated."
   }, "\n\n",

   "**MAE vs RMSE:**\n",
   "- MAE is easier to interpret (simple average)\n",
   "- RMSE is more sensitive to outliers/large errors\n",
   "- If RMSE >> MAE, you have some predictions that are way off\n",
   "- If RMSE approx MAE, errors are fairly consistent\n\n",

   "---\n\n",

   "## What Do These Results Mean for You?\n\n"
 )

 # Add practical interpretation
 if (!is.na(rsq_val) && rsq_val >= 0.7) {
   explanation <- paste0(explanation,
     "### Strong Performance\n\n",
     "Your model captures most of the important patterns in the data. ",
     "The predictions should be reliable for most practical purposes.\n\n",
     "**Recommended next steps:**\n",
     "- Test on completely new data to confirm performance\n",
     "- Check if the important features make intuitive sense\n",
     "- Consider deploying or using for decision-making\n"
   )
 } else if (!is.na(rsq_val) && rsq_val >= 0.5) {
   explanation <- paste0(explanation,
     "### Moderate Performance\n\n",
     "The model captures some patterns but misses others. ",
     "It may be useful for general trends but not precise predictions.\n\n",
     "**Possible reasons:**\n",
     "- Important predictor variables might be missing from your data\n",
     "- The relationship might be more complex than the model can capture\n",
     "- ", target, " might be inherently difficult to predict\n\n",
     "**Recommended next steps:**\n",
     "- Consider adding more relevant variables if available\n",
     "- Try a more flexible model (Random Forest, XGBoost)\n",
     "- Examine residuals to look for patterns\n"
   )
 } else {
   explanation <- paste0(explanation,
     "### Limited Predictive Power\n\n",
     "The model struggles to accurately predict ", target, ". ",
     "This doesn't mean the model lacks predictive power, but predictions will have high uncertainty.\n\n",
     "**This could mean:**\n",
     "- Important predictor variables are missing\n",
     "- ", target, " is inherently random or unpredictable\n",
     "- The relationship is very complex and non-linear\n",
     "- The data might have quality issues\n\n",
     "**Recommended next steps:**\n",
     "- Review what variables might influence ", target, "\n",
     "- Check data quality (outliers, errors, missing values)\n",
     "- Try a different modeling approach\n",
     "- Consider if the prediction task is feasible\n"
   )
 }

 explanation
}


#' Explain Classification Results in Plain English
#'
#' @param metrics Test metrics tibble
#' @param target Target variable name
#' @return Character string explanation
#' @keywords internal
explain_classification_results <- function(metrics, target) {

 # Extract metrics safely
 get_metric <- function(name) {
   val <- metrics %>% dplyr::filter(.metric == name) %>% dplyr::pull(.estimate)
   if (length(val) == 0) NA else val
 }

 accuracy_val <- get_metric("accuracy")
 precision_val <- get_metric("precision")
 recall_val <- get_metric("recall")
 f1_val <- get_metric("f_meas")
 auc_val <- get_metric("roc_auc")

 accuracy_pct <- if (!is.na(accuracy_val)) round(accuracy_val * 100, 1) else NA

 # Interpret accuracy
 acc_interpretation <- dplyr::case_when(
   is.na(accuracy_val) ~ "unable to assess",
   accuracy_val >= 0.95 ~ "very strong",
   accuracy_val >= 0.85 ~ "good",
   accuracy_val >= 0.75 ~ "fair",
   accuracy_val >= 0.65 ~ "moderate",
   TRUE ~ "needs improvement"
 )

 explanation <- paste0(
   "---\n\n",
   "## Understanding Your Classification Results\n\n",

   "### Accuracy = ", if(!is.na(accuracy_val)) paste0(accuracy_pct, "%") else "N/A", "\n\n",

   "**What it measures:** The percentage of all predictions that were correct.\n\n",

   "**Plain English:** ",
   if (!is.na(accuracy_val)) {
     paste0("Out of every 100 observations, the model correctly classifies **", accuracy_pct, "** of them.")
   } else {
     "Could not be calculated."
   }, "\n\n",

   "**Interpretation Guide:**\n",
   "- > 95% = Very Strong\n",
   "- 85-95% = Good\n",
   "- 75-85% = Fair\n",
   "- 65-75% = Moderate\n",
   "- < 65% = Needs improvement\n\n",

   "**Important caveat:** Accuracy can be misleading with imbalanced data!\n",
   "Example: If 95% of emails are NOT spam, a model that always predicts 'not spam' would have 95% accuracy but would not be informative.\n\n",

   "---\n\n"
 )

 if (!is.na(precision_val)) {
   explanation <- paste0(explanation,
     "### Precision = ", round(precision_val * 100, 1), "%\n\n",

     "**What it measures:** Of all the positive predictions, how many were actually correct?\n\n",

     "**Plain English:** When the model predicts a positive case, it's correct ", round(precision_val * 100, 1), "% of the time.\n\n",

     "**Think of it as:** How much can you trust a positive prediction?\n\n",

     "**When precision matters most:**\n",
     "- Spam filters (you don't want to mark good emails as spam)\n",
     "- Medical tests where false positives lead to unnecessary treatment\n",
     "- Any situation where acting on a false positive is costly\n\n",

     "**Formula:** Precision = True Positives / (True Positives + False Positives)\n\n",

     "---\n\n"
   )
 }

 if (!is.na(recall_val)) {
   explanation <- paste0(explanation,
     "### Recall (Sensitivity) = ", round(recall_val * 100, 1), "%\n\n",

     "**What it measures:** Of all actual positive cases, how many did the model catch?\n\n",

     "**Plain English:** The model successfully detects ", round(recall_val * 100, 1), "% of all actual positive cases.\n\n",

     "**Think of it as:** How good is the model at finding all the positives?\n\n",

     "**When recall matters most:**\n",
     "- Disease screening (you don't want to miss sick patients)\n",
     "- Fraud detection (missing fraud is very costly)\n",
     "- Any situation where missing a positive case is dangerous\n\n",

     "**Formula:** Recall = True Positives / (True Positives + False Negatives)\n\n",

     "---\n\n"
   )
 }

 if (!is.na(precision_val) && !is.na(recall_val)) {
   explanation <- paste0(explanation,
     "### The Precision-Recall Trade-off\n\n",

     "**Key insight:** You usually can't maximize both!\n\n",

     "- Making the model more cautious (fewer positive predictions) -> Higher precision, lower recall\n",
     "- Making the model more aggressive (more positive predictions) -> Higher recall, lower precision\n\n",

     "**Your model's balance:**\n",
     if (precision_val > recall_val + 0.1) {
       "Your model is **conservative** - it's cautious about predicting positives, so when it does predict positive, it's usually right. But it misses some actual positives.\n\n"
     } else if (recall_val > precision_val + 0.1) {
       "Your model is **aggressive** - it catches most of the actual positives, but also has more false alarms.\n\n"
     } else {
       "Your model is **balanced** - it strikes a reasonable balance between catching positives and avoiding false alarms.\n\n"
     },

     "---\n\n"
   )
 }

 if (!is.na(f1_val)) {
   explanation <- paste0(explanation,
     "### F1 Score = ", round(f1_val, 3), "\n\n",

     "**What it measures:** The harmonic mean of precision and recall - a single number that balances both.\n\n",

     "**Plain English:** A combined score that only gets high if BOTH precision AND recall are good. ",
     "Ranges from 0 (worst) to 1 (perfect).\n\n",

     "**Why harmonic mean?** It penalizes extreme imbalances. You can't get a high F1 by strong performance in one metric and weak in the other.\n\n",

     "**Interpretation:**\n",
     "- F1 > 0.9 = Very Strong\n",
     "- F1 0.8-0.9 = Good\n",
     "- F1 0.7-0.8 = Fair\n",
     "- F1 < 0.7 = Needs improvement\n\n",

     "---\n\n"
   )
 }

 if (!is.na(auc_val)) {
   auc_interpretation <- dplyr::case_when(
     auc_val >= 0.9 ~ "very strong discrimination",
     auc_val >= 0.8 ~ "good discrimination",
     auc_val >= 0.7 ~ "fair discrimination",
     auc_val >= 0.6 ~ "poor discrimination",
     TRUE ~ "no better than random guessing"
   )

   explanation <- paste0(explanation,
     "### AUC (Area Under ROC Curve) = ", round(auc_val, 3), "\n\n",

     "**What it measures:** How well the model distinguishes between classes, regardless of the threshold you choose.\n\n",

     "**Plain English:** If you randomly pick one positive and one negative example, there's a ",
     round(auc_val * 100, 1), "% chance the model ranks the positive higher.\n\n",

     "**Interpretation Guide:**\n",
     "- AUC = 1.0: Perfect (never happens in practice)\n",
     "- AUC > 0.9: Very Strong discrimination\n",
     "- AUC 0.8-0.9: Good discrimination\n",
     "- AUC 0.7-0.8: Fair discrimination\n",
     "- AUC 0.6-0.7: Poor discrimination\n",
     "- AUC = 0.5: Random guessing (model provides no discrimination)\n",
     "- AUC < 0.5: Worse than random (something is wrong!)\n\n",

     "**Your model:** ", auc_interpretation, "\n\n",

     "**Why AUC is useful:** It's not affected by class imbalance and gives you the full picture of model performance across all possible decision thresholds.\n\n",

     "---\n\n"
   )
 }

 # Add practical interpretation
 explanation <- paste0(explanation,
   "## What Do These Results Mean for You?\n\n"
 )

 if (!is.na(accuracy_val) && accuracy_val >= 0.85) {
   explanation <- paste0(explanation,
     "### Good Performance\n\n",
     "Your model reliably classifies ", target, ". ",
     "It should work well for most practical applications.\n\n",
     "**Recommended next steps:**\n",
     "- Validate on completely new data\n",
     "- Check which types of cases the model gets wrong\n",
     "- Consider the business impact of false positives vs. false negatives\n"
   )
 } else if (!is.na(accuracy_val) && accuracy_val >= 0.7) {
   explanation <- paste0(explanation,
     "### Decent Performance\n\n",
     "The model does better than random, but there's room for improvement.\n\n",
     "**Possible improvements:**\n",
     "- Add more distinguishing features\n",
     "- Try a more powerful model (XGBoost, Random Forest)\n",
     "- Check if classes are very imbalanced\n",
     "- Look at which cases are being misclassified\n"
   )
 } else {
   explanation <- paste0(explanation,
     "### Limited Performance\n\n",
     "The model struggles to accurately classify ", target, ".\n\n",
     "**This could mean:**\n",
     "- The classes are hard to distinguish with available features\n",
     "- Important distinguishing features are missing\n",
     "- The classes might overlap significantly\n\n",
     "**Recommended next steps:**\n",
     "- Review what actually differentiates the classes\n",
     "- Consider adding domain-specific features\n",
     "- Check data quality and labeling accuracy\n"
   )
 }

 explanation
}


#' Explain Feature Importance in Plain English
#'
#' @param importance Importance tibble from extract_importance()
#' @param target Target variable name
#' @param method Model method/engine name
#' @return Character string explanation
#' @keywords internal
explain_feature_importance <- function(importance, target, method = NULL) {

 if (is.null(importance) || nrow(importance) == 0) {
   return("")
 }

 # Get top features
 top_n <- min(5, nrow(importance))
 top_features <- importance %>% head(top_n)

 # Calculate relative importance
 total_imp <- sum(importance$Importance, na.rm = TRUE)
 top_features <- top_features %>%
   dplyr::mutate(
     Relative = round(Importance / total_imp * 100, 1)
   )

 explanation <- paste0(
   "---\n\n",
   "## Feature Importance: What Drives Predictions?\n\n",

   "**What this shows:** Which variables have the most influence on the model's predictions.\n\n",

   "### Top ", top_n, " Most Important Features:\n\n"
 )

 for (i in 1:nrow(top_features)) {
   feat <- top_features[i, ]
   explanation <- paste0(explanation,
     i, ". **", feat$Variable, "** - ", feat$Relative, "% of total importance\n"
   )
 }

 explanation <- paste0(explanation,
   "\n### How to Interpret This:\n\n",

   "**Variables at the top:**\n",
   "- Have the strongest influence on predictions\n",
   "- Changes in these variables create bigger changes in predictions\n",
   "- Should be your focus for data quality and collection\n\n",

   "**Variables not shown (or with low importance):**\n",
   "- Have minimal impact on predictions\n",
   "- Could potentially be removed to simplify the model\n",
   "- Might still be important - they could be correlated with other features\n\n"
 )

 # Add method-specific warnings
 if (!is.null(method)) {
   if (method == "ranger") {
     explanation <- paste0(explanation,
       "**Random Forest Importance Note:**\n",
       "- This uses 'impurity' importance, which can be biased toward high-cardinality variables\n",
       "- Variables with many categories may appear more important than they truly are\n",
       "- For more reliable importance, consider permutation importance methods\n\n"
     )
   } else if (method == "glmnet") {
     explanation <- paste0(explanation,
       "**LASSO Coefficient Note:**\n",
       "- Importance based on absolute coefficient values\n",
       "- Features were standardized before fitting, so magnitudes are comparable\n",
       "- Coefficients can be unstable if predictors are highly correlated\n",
       "- LASSO automatically sets some coefficients to exactly zero (feature selection)\n\n"
     )
   } else if (method == "xgboost") {
     explanation <- paste0(explanation,
       "**XGBoost Importance Note:**\n",
       "- Based on how often features are used for splits (gain importance)\n",
       "- More reliable than Random Forest impurity importance\n",
       "- Can still be affected by correlated features\n\n"
     )
   }
 }

 explanation <- paste0(explanation,
   "**Important caveats:**\n",
   "- Importance != causation (the variable might just be correlated with something causal)\n",
   "- Different models calculate importance differently\n",
   "- Correlated features can 'share' importance, making each look less important\n\n",

   "**Practical takeaway:** If you want to influence ", target, ", focus on the top variables.\n"
 )

 explanation
}


#' Explain Baseline Comparison
#'
#' @param model_results Output from train_model()
#' @param baseline_info Output from calculate_baseline()
#' @return Character string explanation
#' @keywords internal
explain_baseline_comparison <- function(model_results, baseline_info) {

  task_type <- model_results$task_type
  test_metrics <- model_results$test_metrics

  if (task_type == "regression") {
    model_rmse <- test_metrics %>%
      dplyr::filter(.metric == "rmse") %>%
      dplyr::pull(.estimate)

    if (length(model_rmse) == 0) return("")

    improvement <- (baseline_info$baseline_rmse - model_rmse) / baseline_info$baseline_rmse * 100

    explanation <- paste0(
      "---\n\n",
      "## Baseline Comparison\n\n",
      "**Baseline strategy:** ", baseline_info$baseline_strategy, "\n",
      "**Baseline RMSE:** ", round(baseline_info$baseline_rmse, 4), "\n",
      "**Your model RMSE:** ", round(model_rmse, 4), "\n",
      "**Improvement:** ", round(improvement, 1), "%\n\n",

      if (improvement > 50) {
        paste0("**Strong Result:** Your model is ", round(improvement, 0),
               "% better than just predicting the mean. This is a substantial improvement.")
      } else if (improvement > 20) {
        paste0("**Positive Result:** Your model beats the baseline by ", round(improvement, 0),
               "%. This shows the features have predictive power.")
      } else if (improvement > 5) {
        paste0("**Modest improvement.** Your model is only ", round(improvement, 0),
               "% better than the baseline. Consider:\n",
               "- Adding more informative features\n",
               "- Trying different model types\n",
               "- Checking if there's genuinely a signal in the data")
      } else if (improvement > 0) {
        paste0("[X] **Weak model.** Your model barely beats predicting the mean. ",
               "The features may not contain useful information for predicting ", model_results$target, ".")
      } else {
        paste0("[X] **Model worse than baseline!** This shouldn't happen. Possible issues:\n",
               "- Extreme overfitting\n",
               "- Target leakage fixed incorrectly\n",
               "- Error in model fitting")
      }, "\n"
    )

  } else {
    # Classification
    model_acc <- test_metrics %>%
      dplyr::filter(.metric == "accuracy") %>%
      dplyr::pull(.estimate)

    if (length(model_acc) == 0) return("")

    lift <- model_acc - baseline_info$baseline_accuracy

    explanation <- paste0(
      "---\n\n",
      "## Baseline Comparison\n\n",
      "**Baseline strategy:** ", baseline_info$baseline_strategy, "\n",
      "**Baseline accuracy:** ", round(baseline_info$baseline_accuracy * 100, 1), "%\n",
      "**Your model accuracy:** ", round(model_acc * 100, 1), "%\n",
      "**Lift:** +", round(lift * 100, 1), " percentage points\n\n",

      if (lift > 0.25) {
        paste0("**Strong Result:** Your model is ", round(lift * 100, 1),
               " percentage points better than always predicting '", baseline_info$baseline_class,
               "'. This is strong performance.")
      } else if (lift > 0.10) {
        paste0("**Positive Result:** Your model improves on the baseline by ", round(lift * 100, 1),
               " percentage points. Your features are informative.")
      } else if (lift > 0.03) {
        paste0("**Modest lift.** Only ", round(lift * 100, 1),
               " percentage points better than baseline. ",
               "In imbalanced datasets, this can still be valuable, but verify if the model adds real value.")
      } else if (lift > 0) {
        paste0("**Very weak.** Barely better than always predicting the most common class. ",
               "The model may not be learning useful patterns.")
      } else {
        paste0("**Worse than baseline!** This is concerning. Check for:\n",
               "- Overfitting\n",
               "- Data leakage issues\n",
               "- Errors in model training")
      }, "\n"
    )
  }

  explanation
}


#' Generate Recommendations Based on Results
#'
#' @param model_results Output from train_model()
#' @return Character string with recommendations
#' @keywords internal
generate_recommendations <- function(model_results) {

 task_type <- model_results$task_type
 metrics <- model_results$test_metrics
 importance <- model_results$importance

 recommendations <- "---\n\n## Next Steps & Recommendations\n\n"

 # Performance-based recommendations
 if (task_type == "regression") {
   rsq <- metrics %>% dplyr::filter(.metric == "rsq") %>% dplyr::pull(.estimate)
   if (length(rsq) > 0 && !is.na(rsq) && rsq < 0.5) {
     recommendations <- paste0(recommendations,
       "### To Improve Model Performance:\n\n",
       "1. **Add more predictors:** Are there other variables that might influence ", model_results$target, "?\n",
       "2. **Feature engineering:** Create new features from existing ones (ratios, interactions, etc.)\n",
       "3. **Try non-linear models:** Random Forest and XGBoost can capture complex patterns\n",
       "4. **Check for outliers:** Extreme values can hurt model performance\n",
       "5. **Collect more data:** More observations often help, especially for complex patterns\n\n"
     )
   }
 } else {
   acc <- metrics %>% dplyr::filter(.metric == "accuracy") %>% dplyr::pull(.estimate)
   if (length(acc) > 0 && !is.na(acc) && acc < 0.8) {
     recommendations <- paste0(recommendations,
       "### To Improve Classification:\n\n",
       "1. **Check class balance:** If one class is rare, consider resampling techniques\n",
       "2. **Add distinguishing features:** What characteristics separate the classes?\n",
       "3. **Try ensemble methods:** XGBoost and Random Forest often perform better\n",
       "4. **Adjust the decision threshold:** You can trade precision for recall\n",
       "5. **Examine misclassified cases:** Look for patterns in what the model gets wrong\n\n"
     )
   }
 }

 # Feature importance recommendations
 if (!is.null(importance) && nrow(importance) > 0) {
   top_feat <- importance$Variable[1]
   recommendations <- paste0(recommendations,
     "### Key Finding:\n\n",
     "**", top_feat, "** is the most important predictor. Focus your data quality efforts here.\n\n"
   )
 }

 # General recommendations
 recommendations <- paste0(recommendations,
   "### Before Using This Model:\n\n",
   "1. **Validate on new data:** The test results are estimates - confirm on truly new data\n",
   "2. **Check for data leakage:** Make sure no future information leaked into training\n",
   "3. **Consider the cost of errors:** Are false positives or false negatives worse?\n",
   "4. **Document assumptions:** What conditions must hold for predictions to be valid?\n",
   "5. **Plan for monitoring:** Models can degrade over time as patterns change\n"
 )

 recommendations
}


#' Create Feature Importance Plot
#'
#' @param importance Importance tibble
#' @param top_n Number of features to show (default: 15)
#' @return A ggplot object
#' @export
plot_importance <- function(importance, top_n = 15) {

 if (is.null(importance) || nrow(importance) == 0) {
   return(NULL)
 }

 plot_data <- importance %>%
   head(top_n) %>%
   dplyr::mutate(
     Variable = factor(Variable, levels = rev(Variable))
   )

 ggplot2::ggplot(plot_data, ggplot2::aes(x = Importance, y = Variable)) +
   ggplot2::geom_col(fill = "#3498db", alpha = 0.8) +
   ggplot2::labs(
     title = "Feature Importance",
     subtitle = "Variables with the most influence on predictions",
     x = "Importance Score",
     y = NULL
   ) +
   ggplot2::theme_minimal() +
   ggplot2::theme(
     plot.title = ggplot2::element_text(face = "bold", size = 14),
     plot.subtitle = ggplot2::element_text(color = "gray50"),
     axis.text.y = ggplot2::element_text(size = 10)
   )
}


#' Create Predictions vs Actual Plot (Regression)
#'
#' @param predictions Predictions tibble with .pred and actual columns
#' @param target_col Target column name
#' @return A ggplot object
#' @export
plot_predictions_regression <- function(predictions, target_col) {

 ggplot2::ggplot(predictions, ggplot2::aes(x = !!rlang::sym(target_col), y = .pred)) +
   ggplot2::geom_point(alpha = 0.5, color = "#3498db") +
   ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 1) +
   ggplot2::labs(
     title = "Predicted vs Actual Values",
     subtitle = "Points on the red dashed line = perfect predictions",
     x = paste("Actual", target_col),
     y = paste("Predicted", target_col),
     caption = "Closer to the line = better predictions"
   ) +
   ggplot2::theme_minimal() +
   ggplot2::theme(
     plot.title = ggplot2::element_text(face = "bold", size = 14),
     plot.subtitle = ggplot2::element_text(color = "gray50")
   )
}


#' Create Residual Plot (Regression)
#'
#' @param predictions Predictions tibble
#' @param target_col Target column name
#' @return A ggplot object
#' @export
plot_residuals <- function(predictions, target_col) {

 plot_data <- predictions %>%
   dplyr::mutate(
     residual = !!rlang::sym(target_col) - .pred
   )

 ggplot2::ggplot(plot_data, ggplot2::aes(x = .pred, y = residual)) +
   ggplot2::geom_point(alpha = 0.5, color = "#e74c3c") +
   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
   ggplot2::geom_smooth(method = "loess", se = TRUE, color = "#3498db", fill = "#3498db", alpha = 0.2) +
   ggplot2::labs(
     title = "Residual Plot (Error Analysis)",
     subtitle = "Residuals should be randomly scattered around zero",
     x = "Predicted Value",
     y = "Residual (Actual - Predicted)",
     caption = "Patterns in residuals suggest the model is missing something"
   ) +
   ggplot2::theme_minimal() +
   ggplot2::theme(
     plot.title = ggplot2::element_text(face = "bold", size = 14),
     plot.subtitle = ggplot2::element_text(color = "gray50")
   )
}


#' Create Confusion Matrix Visualization
#'
#' @param predictions Predictions tibble with .pred_class and actual
#' @param target_col Target column name
#' @return A ggplot object
#' @export
plot_confusion_matrix <- function(predictions, target_col) {

 # Calculate confusion matrix
 conf_mat <- predictions %>%
   dplyr::mutate(
     actual = as.factor(!!rlang::sym(target_col)),
     predicted = as.factor(.pred_class)
   ) %>%
   dplyr::count(actual, predicted) %>%
   dplyr::group_by(actual) %>%
   dplyr::mutate(
     pct = n / sum(n) * 100,
     label = paste0(n, "\n(", round(pct, 1), "%)")
   ) %>%
   dplyr::ungroup()

 ggplot2::ggplot(conf_mat, ggplot2::aes(x = predicted, y = actual, fill = n)) +
   ggplot2::geom_tile(color = "white", linewidth = 1) +
   ggplot2::geom_text(ggplot2::aes(label = label), color = "white", fontface = "bold", size = 4) +
   ggplot2::scale_fill_gradient(low = "#3498db", high = "#2c3e50") +
   ggplot2::labs(
     title = "Confusion Matrix",
     subtitle = "Diagonal cells = correct predictions",
     x = "Predicted Class",
     y = "Actual Class",
     fill = "Count",
     caption = "Higher numbers on diagonal = better performance"
   ) +
   ggplot2::theme_minimal() +
   ggplot2::theme(
     plot.title = ggplot2::element_text(face = "bold", size = 14),
     plot.subtitle = ggplot2::element_text(color = "gray50"),
     panel.grid = ggplot2::element_blank()
   )
}


#' Create Metrics Summary Card Data
#'
#' @param model_results Output from train_model()
#' @return A list with formatted metrics for display
#' @export
create_metrics_summary <- function(model_results) {

 metrics <- model_results$test_metrics
 task_type <- model_results$task_type

 summary_list <- list()

 get_metric <- function(name) {
   val <- metrics %>% dplyr::filter(.metric == name) %>% dplyr::pull(.estimate)
   if (length(val) == 0 || is.na(val)) NA else val
 }

 if (task_type == "regression") {
   rsq <- get_metric("rsq")
   summary_list$primary <- list(
     name = "R-squared",
     value = if(!is.na(rsq)) round(rsq, 3) else "N/A",
     description = "Variance explained by model",
     color = get_metric_color(rsq, "rsq"),
     explanation = "Higher is better. 1.0 = perfect, 0 = no predictive power."
   )

   rmse <- get_metric("rmse")
   mae <- get_metric("mae")

   summary_list$secondary <- list(
     list(
       name = "RMSE",
       value = if(!is.na(rmse)) round(rmse, 2) else "N/A",
       description = "Root mean square error",
       explanation = "Average prediction error (penalizes large errors more)"
     ),
     list(
       name = "MAE",
       value = if(!is.na(mae)) round(mae, 2) else "N/A",
       description = "Mean absolute error",
       explanation = "Average absolute prediction error"
     )
   )
 } else {
   acc <- get_metric("accuracy")
   summary_list$primary <- list(
     name = "Accuracy",
     value = if(!is.na(acc)) paste0(round(acc * 100, 1), "%") else "N/A",
     description = "Overall correct predictions",
     color = get_metric_color(acc, "accuracy"),
     explanation = "Percentage of all predictions that were correct"
   )

   secondary <- list()

   prec <- get_metric("precision")
   if (!is.na(prec)) {
     secondary[[length(secondary) + 1]] <- list(
       name = "Precision",
       value = paste0(round(prec * 100, 1), "%"),
       description = "Positive prediction accuracy",
       explanation = "When predicting positive, how often correct?"
     )
   }

   rec <- get_metric("recall")
   if (!is.na(rec)) {
     secondary[[length(secondary) + 1]] <- list(
       name = "Recall",
       value = paste0(round(rec * 100, 1), "%"),
       description = "True positive detection rate",
       explanation = "What % of actual positives were found?"
     )
   }

   auc <- get_metric("roc_auc")
   if (!is.na(auc)) {
     secondary[[length(secondary) + 1]] <- list(
       name = "AUC",
       value = round(auc, 3),
       description = "Discrimination ability",
       explanation = "How well model separates classes (0.5 = random, 1.0 = perfect)"
     )
   }

   summary_list$secondary <- secondary
 }

 summary_list
}


#' Get Color for Metric Value
#'
#' @param value Metric value
#' @param metric_type Type of metric
#' @return Color string
#' @keywords internal
get_metric_color <- function(value, metric_type) {

 if (is.na(value) || length(value) == 0) return("gray")

 thresholds <- switch(
   metric_type,
   "rsq" = c(0.3, 0.5, 0.7),
   "accuracy" = c(0.65, 0.75, 0.85),
   c(0.3, 0.5, 0.7)
 )

 if (value >= thresholds[3]) return("green")
 if (value >= thresholds[2]) return("yellow")
 if (value >= thresholds[1]) return("orange")
 return("red")
}


#' Generate Model Comparison Summary
#'
#' @param comparison_results Output from compare_models()
#' @return Character string with comparison explanation
#' @export
explain_model_comparison <- function(comparison_results) {

 comparison <- comparison_results$comparison
 best <- comparison_results$best_model
 task_type <- comparison_results$task_type

 explanation <- paste0(
   "## Model Comparison Results\n\n",
   "### Best Performing Model: **", get_method_display_name(best), "**\n\n"
 )

 if (task_type == "regression") {
   explanation <- paste0(explanation,
     "Models were ranked by R-squared (higher = better).\n\n",
     "**What this means:** ", get_method_display_name(best),
     " explained the most variance in your target variable.\n\n"
   )
 } else {
   explanation <- paste0(explanation,
     "Models were ranked by Accuracy (higher = better).\n\n",
     "**What this means:** ", get_method_display_name(best),
     " correctly classified the most observations.\n\n"
   )
 }

 explanation <- paste0(explanation,
   "### Model Rankings:\n\n"
 )

 for (i in 1:nrow(comparison)) {
   model_name <- get_method_display_name(comparison$model[i])

   if (task_type == "regression" && "rsq" %in% names(comparison)) {
     metric_val <- round(comparison$rsq[i], 3)
     metric_name <- "R-squared"
   } else if ("accuracy" %in% names(comparison)) {
     metric_val <- paste0(round(comparison$accuracy[i] * 100, 1), "%")
     metric_name <- "Accuracy"
   } else {
     metric_val <- "N/A"
     metric_name <- ""
   }

   rank_marker <- paste0(i, ".")

   explanation <- paste0(explanation,
     rank_marker, " **", model_name, "**: ", metric_name, " = ", metric_val, "\n"
   )
 }

 explanation <- paste0(explanation,
   "\n### Interpretation Tips:\n\n",
   "- **Small differences** (< 2-3%) between models may not be meaningful\n",
   "- **Simpler models** (Lasso, Decision Tree) are easier to interpret\n",
   "- **Complex models** (XGBoost, Random Forest) often perform better but are 'black boxes'\n",
   "- Consider **your use case**: Do you need maximum accuracy or interpretability?\n"
 )

 explanation
}
