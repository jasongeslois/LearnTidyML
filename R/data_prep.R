#' Generate Data Preparation Recipe
#'
#' @param data A data frame
#' @param target_col Target variable name
#' @param issues Output from identify_issues()
#' @return A tidymodels recipe object
#' @export
create_prep_recipe <- function(data, target_col, issues) {

  # Validate inputs
  validate_target_column(data, target_col)

  # Use reformulate for safe formula construction
  formula_obj <- reformulate(".", response = target_col)

  # Start with basic recipe
  rec <- recipes::recipe(formula_obj, data = data)
  
  # Handle missing data
  if(any(issues$issue_type == "high_missing")) {
    high_miss_cols <- issues %>%
      dplyr::filter(issue_type == "high_missing") %>%
      dplyr::pull(column)
    
    # Could remove or impute
    rec <- rec %>%
      recipes::step_rm(dplyr::all_of(high_miss_cols))
  }
  
  # Impute remaining missing
  rec <- rec %>%
    recipes::step_impute_median(recipes::all_numeric_predictors()) %>%
    recipes::step_impute_mode(recipes::all_nominal_predictors())
  
  # Handle categorical variables
  rec <- rec %>%
    recipes::step_novel(recipes::all_nominal_predictors()) %>%
    recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = FALSE)
  
  # Normalize numeric predictors
  rec <- rec %>%
    recipes::step_normalize(recipes::all_numeric_predictors())
  
  # Remove near-zero variance
  rec <- rec %>%
    recipes::step_nzv(recipes::all_predictors())
  
  rec
}

#' Generate Human-Readable Prep Steps
#' 
#' @param issues Output from identify_issues()
#' @return A character vector of preparation steps
#' @export
generate_prep_steps <- function(issues) {
  
  steps <- character()
  
  if(nrow(issues) == 0) {
    return("No major data preparation needed - your data looks good!")
  }
  
  issue_summary <- issues %>%
    dplyr::group_by(issue_type) %>%
    dplyr::summarise(
      n = dplyr::n(),
      columns = paste(column, collapse = ", "),
      .groups = "drop"
    )
  
  for(i in 1:nrow(issue_summary)) {
    issue <- issue_summary[i, ]
    
    step <- switch(
      issue$issue_type,
      "high_missing" = glue::glue("Consider removing or imputing: {issue$columns}"),
      "high_cardinality" = glue::glue("Encode categorical variables: {issue$columns}"),
      "near_zero_variance" = glue::glue("Consider removing low-variance: {issue$columns}"),
      glue::glue("Review {issue$issue_type} in: {issue$columns}")
    )
    
    steps <- c(steps, step)
  }
  
  steps
}