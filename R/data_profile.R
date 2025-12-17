#' Analyze Dataset Characteristics
#'
#' @param data A data frame to analyze
#' @param max_unique_check Maximum unique values to check for categorical columns (default: 10000)
#' @return A list containing data profile information
#' @export
analyze_data <- function(data, max_unique_check = 10000) {

  # Validate input
  if(!is.data.frame(data)) {
    stop("Input must be a data frame")
  }

  if(nrow(data) == 0) {
    stop("Data frame is empty")
  }

  # Check for problematic column types
  problematic_cols <- data %>%
    purrr::map_lgl(~is.list(.x) && !inherits(.x, "factor"))

  if(any(problematic_cols)) {
    warning(paste("List columns detected (will be ignored):",
                  paste(names(data)[problematic_cols], collapse = ", ")))
    # Remove list columns
    data <- data[, !problematic_cols, drop = FALSE]
  }

  # Basic structure
  structure_info <- list(
    n_rows = nrow(data),
    n_cols = ncol(data),
    col_names = names(data)
  )

  # Column type analysis
  col_types <- data %>%
    purrr::map_chr(~class(.x)[1]) %>%
    tibble::enframe(name = "column", value = "type")

  # Missing data analysis
  missing_info <- data %>%
    purrr::map_dbl(~sum(is.na(.x)) / length(.x) * 100) %>%
    tibble::enframe(name = "column", value = "pct_missing")

  # Numeric column statistics
  numeric_cols <- data %>%
    dplyr::select(where(is.numeric)) %>%
    names()

  numeric_summary <- if(length(numeric_cols) > 0) {
    data %>%
      dplyr::select(all_of(numeric_cols)) %>%
      purrr::map_df(~{
        # Check for all NA
        if(all(is.na(.x))) {
          return(tibble::tibble(
            mean = NA_real_,
            median = NA_real_,
            sd = NA_real_,
            min = NA_real_,
            max = NA_real_,
            n_unique = 0,
            n_infinite = 0
          ))
        }

        # Check for infinite values
        inf_count <- sum(is.infinite(.x), na.rm = TRUE)

        # Calculate statistics (handle infinite values)
        tibble::tibble(
          mean = if(inf_count > 0) NA_real_ else mean(.x, na.rm = TRUE),
          median = median(.x[is.finite(.x)], na.rm = TRUE),
          sd = if(inf_count > 0) NA_real_ else sd(.x, na.rm = TRUE),
          min = if(inf_count > 0) NA_real_ else min(.x, na.rm = TRUE),
          max = if(inf_count > 0) NA_real_ else max(.x, na.rm = TRUE),
          n_unique = length(unique(.x[is.finite(.x) & !is.na(.x)])),
          n_infinite = inf_count
        )
      }, .id = "column")
  } else {
    tibble::tibble()
  }

  # Character/Factor column analysis
  categorical_cols <- data %>%
    dplyr::select(where(~is.character(.x) | is.factor(.x))) %>%
    names()

  categorical_summary <- if(length(categorical_cols) > 0) {
    data %>%
      dplyr::select(all_of(categorical_cols)) %>%
      purrr::map_df(~{
        # Get unique values efficiently
        unique_vals <- unique(.x)
        n_unique <- length(unique_vals)

        # Only calculate top values if cardinality is reasonable
        top_values <- if(n_unique <= max_unique_check && n_unique > 0) {
          tryCatch({
            list(head(sort(table(.x), decreasing = TRUE), 5))
          }, error = function(e) {
            list("Unable to calculate")
          })
        } else if(n_unique > max_unique_check) {
          list(paste("Too many unique values (", n_unique, ") to display"))
        } else {
          list("No values")
        }

        tibble::tibble(
          n_unique = n_unique,
          top_values = top_values
        )
      }, .id = "column")
  } else {
    tibble::tibble()
  }

  list(
    structure = structure_info,
    col_types = col_types,
    missing = missing_info,
    numeric_summary = numeric_summary,
    categorical_summary = categorical_summary
  )
}

#' Identify Data Quality Issues
#' 
#' @param data A data frame
#' @param profile_data Output from analyze_data()
#' @return A tibble of identified issues
#' @export
identify_issues <- function(data, profile_data) {
  
  issues <- tibble::tibble(
    column = character(),
    issue_type = character(),
    description = character(),
    severity = character()
  )
  
  # High missing data
  high_missing <- profile_data$missing %>%
    dplyr::filter(pct_missing > 50) %>%
    dplyr::mutate(
      issue_type = "high_missing",
      description = glue::glue("{round(pct_missing, 1)}% missing values"),
      severity = "high"
    )
  
  # Character columns that might need encoding
  # Handle empty categorical_summary (no categorical columns in data)
  char_encoding <- if (nrow(profile_data$categorical_summary) > 0) {
    profile_data$categorical_summary %>%
      dplyr::filter(n_unique > 10) %>%
      dplyr::mutate(
        issue_type = "high_cardinality",
        description = glue::glue("{n_unique} unique values - may need encoding"),
        severity = "medium"
      ) %>%
      dplyr::select(column, issue_type, description, severity)
  } else {
    tibble::tibble(column = character(), issue_type = character(),
                   description = character(), severity = character())
  }
  
  # Near-zero variance using coefficient of variation (scale-invariant)
  # Handle empty numeric_summary (no numeric columns in data)
  nzv <- if (nrow(profile_data$numeric_summary) > 0) {
    profile_data$numeric_summary %>%
      dplyr::filter(!is.na(sd), !is.na(mean), abs(mean) > 1e-10) %>%
      dplyr::mutate(cv = abs(sd / mean)) %>%
      dplyr::filter(cv < 0.01) %>%
      dplyr::mutate(
        issue_type = "near_zero_variance",
        description = glue::glue("Coefficient of variation = {round(cv, 4)} - very low relative variance"),
        severity = "low"
      ) %>%
      dplyr::select(column, issue_type, description, severity)
  } else {
    tibble::tibble(column = character(), issue_type = character(),
                   description = character(), severity = character())
  }
  
  dplyr::bind_rows(
    high_missing %>% dplyr::select(column, issue_type, description, severity),
    char_encoding,
    nzv
  )
}