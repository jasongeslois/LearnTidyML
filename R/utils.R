#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

# Security and Validation Utilities ----

#' Validate imported File
#'
#' Performs comprehensive security validation on imported files
#'
#' @param file_info Shiny fileInput object
#' @param max_size_mb Maximum file size in megabytes (default: 50)
#' @return TRUE if validation passes, throws error otherwise
#' @keywords internal
validate_imported_file <- function(file_info, max_size_mb = 50) {
  # Check file exists
  if(is.null(file_info) || !file.exists(file_info$datapath)) {
    stop("Invalid file import")
  }

  # Validate file size
  max_size <- max_size_mb * 1024 * 1024  # Convert to bytes
  file_size <- file.info(file_info$datapath)$size
  if(file_size > max_size) {
    stop(paste0("File size exceeds ", max_size_mb, "MB limit"))
  }

  # Validate file extension server-side
  allowed_extensions <- c("csv", "tsv", "txt", "xlsx", "xls", "rds", "rdata", "rda", "sas7bdat", "xpt", "dta", "sav")
  ext <- tolower(tools::file_ext(file_info$name))
  if(!ext %in% allowed_extensions) {
    stop("File format not supported. Allowed: CSV, TSV, TXT, Excel (.xlsx/.xls), R data (.rds/.RData/.rda), SAS (.sas7bdat/.xpt), Stata (.dta), SPSS (.sav)")
  }

  # Check for binary file signatures (basic magic number check)
  con <- file(file_info$datapath, "rb")
  magic_bytes <- readBin(con, "raw", n = 4)
  close(con)

  # Check for common binary file formats
  # Note: xlsx/xls files are actually ZIP archives internally, so we must
  # allow ZIP signatures for Excel files
  # Note: RDS/RData files are gzip-compressed by default, so allow gzip for R data formats
  is_excel_file <- ext %in% c("xlsx", "xls")
  is_r_data_file <- ext %in% c("rds", "rdata", "rda")

  if(length(magic_bytes) >= 2) {
    # Gzip files start with 1f 8b
    # But RDS/RData files are gzip-compressed by default, so allow them
    if(magic_bytes[1] == as.raw(0x1f) && magic_bytes[2] == as.raw(0x8b) && !is_r_data_file) {
      stop("Compressed files are not allowed")
    }
    # ZIP files start with PK (50 4b)
    # But xlsx files ARE zip files, so allow them
    if(magic_bytes[1] == as.raw(0x50) && magic_bytes[2] == as.raw(0x4b) && !is_excel_file) {
      stop("Archive files are not allowed")
    }
  }

  TRUE
}

#' Validate Column Names
#'
#' Ensures column names are safe and valid R identifiers.
#' SECURITY: Stricter validation to prevent formula injection attacks.
#'
#' @param data A data frame
#' @return TRUE if validation passes, throws error otherwise
#' @keywords internal
validate_column_names <- function(data) {
  col_names <- names(data)

  # SECURITY FIX (CRITICAL-2): Stricter pattern - NO dots allowed

  # Dots can be exploited for formula injection (e.g., I(malicious.code()))
  # Pattern: Start with letter, then only letters, numbers, and underscores
  valid_pattern <- "^[a-zA-Z][a-zA-Z0-9_]*$"

  invalid_cols <- col_names[!grepl(valid_pattern, col_names)]

  if(length(invalid_cols) > 0) {
    stop(paste("Invalid column names detected:",
               paste(head(invalid_cols, 5), collapse = ", "),
               "\nColumn names must start with a letter and contain only letters, numbers, and underscores (no dots or special characters)."))
  }

  # SECURITY: Check for suspicious patterns that could indicate injection attempts
  suspicious_patterns <- c(
    "system", "eval", "parse", "exec", "shell", "cmd",
    "library", "require", "source", "load"
  )
  for (col in col_names) {
    col_lower <- tolower(col)
    for (pattern in suspicious_patterns) {
      if (grepl(pattern, col_lower, fixed = TRUE)) {
        stop(paste("Column name contains suspicious pattern '", pattern,
                   "' in column: ", col, ". Please rename this column.", sep = ""))
      }
    }
  }

  # Check for reserved R keywords
  reserved <- c("if", "else", "repeat", "while", "function", "for", "in",
                "next", "break", "TRUE", "FALSE", "NULL", "Inf", "NaN",
                "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_")

  reserved_used <- col_names[col_names %in% reserved]
  if(length(reserved_used) > 0) {
    stop(paste("Reserved R keywords used as column names:",
               paste(reserved_used, collapse = ", ")))
  }

  # Check column name length
  max_length <- 100
  long_cols <- col_names[nchar(col_names) > max_length]
  if(length(long_cols) > 0) {
    stop("Column names exceed maximum length of 100 characters")
  }

  TRUE
}

#' Validate Target Column
#'
#' Validates that a target column exists and is safe to use
#'
#' @param data A data frame
#' @param target_col Target column name
#' @return TRUE if validation passes, throws error otherwise
#' @keywords internal
validate_target_column <- function(data, target_col) {
  if(is.null(target_col) || length(target_col) != 1) {
    stop("Invalid target column: must be a single column name")
  }

  if(!target_col %in% names(data)) {
    stop(paste("Target column", target_col, "not found in dataset"))
  }

  if(!is.character(target_col)) {
    stop("Target column name must be a character string")
  }

  # SECURITY FIX (CRITICAL-2): Stricter validation - NO dots allowed
  # Dots can be exploited for formula injection attacks
  if(!grepl("^[a-zA-Z][a-zA-Z0-9_]*$", target_col)) {
    stop("Invalid target column name format. Use only letters, numbers, and underscores (no dots).")
  }

  # Check for suspicious patterns
  target_lower <- tolower(target_col)
  suspicious_patterns <- c("system", "eval", "parse", "exec", "shell", "cmd")
  for (pattern in suspicious_patterns) {
    if (grepl(pattern, target_lower, fixed = TRUE)) {
      stop(paste("Target column name contains suspicious pattern '", pattern, "'. Please rename.", sep = ""))
    }
  }

  TRUE
}

#' Validate Data Dimensions
#'
#' Ensures data doesn't exceed resource limits
#'
#' @param data A data frame
#' @param max_rows Maximum number of rows (default: 100000)
#' @param max_cols Maximum number of columns (default: 1000)
#' @param max_cells Maximum total cells (default: 10000000)
#' @return TRUE if validation passes, throws error otherwise
#' @keywords internal
validate_data_dimensions <- function(data, max_rows = 100000, max_cols = 1000, max_cells = 10000000) {

  # Check row limit
  if(nrow(data) > max_rows) {
    stop(paste("Dataset too large. Maximum", format(max_rows, big.mark = ","), "rows allowed."))
  }

  # Check column limit
  if(ncol(data) > max_cols) {
    stop(paste("Too many columns. Maximum", max_cols, "columns allowed."))
  }

  # Check total cells
  total_cells <- nrow(data) * ncol(data)
  if(total_cells > max_cells) {
    stop(paste("Dataset too large. Maximum", format(max_cells, big.mark = ","), "cells allowed."))
  }

  TRUE
}

#' Safe Error Message Handler
#'
#' Sanitizes error messages to prevent information disclosure
#'
#' @param e Error condition
#' @param user_message Generic user-facing message
#' @return Character string with sanitized message
#' @keywords internal
safe_error_message <- function(e, user_message = "An error occurred") {
  # Log full error for debugging (server-side only)
  message(paste("Error:", conditionMessage(e)))
  if(!is.null(e$call)) {
    message(paste("Call:", paste(deparse(e$call), collapse = " ")))
  }

  # Return sanitized message to user
  return(user_message)
}

#' Execute with Timeout
#'
#' Wraps an expression with a timeout to prevent resource exhaustion
#'
#' @param expr Expression to evaluate
#' @param timeout Timeout in seconds (default: 30)
#' @return Result of expression if successful
#' @keywords internal
with_timeout <- function(expr, timeout = 30) {
  setTimeLimit(cpu = timeout, elapsed = timeout, transient = TRUE)
  on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE))

  tryCatch(
    expr,
    error = function(e) {
      if(grepl("reached CPU time limit|reached elapsed time limit", conditionMessage(e))) {
        stop("Operation timed out. Dataset may be too large or complex.")
      }
      stop(e)
    }
  )
}

#' Safe Column Name for Code Generation
#'
#' Escapes column names for safe use in generated R code
#'
#' @param name Column name
#' @return Escaped column name
#' @keywords internal
safe_r_name <- function(name) {
  # List of reserved keywords
  reserved <- c("if", "else", "repeat", "while", "function", "for", "in",
                "next", "break", "TRUE", "FALSE", "NULL", "Inf", "NaN",
                "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_")

  # If name is a simple valid identifier and not reserved, use as-is
  if(grepl("^[a-zA-Z][a-zA-Z0-9._]*$", name) && !name %in% reserved) {
    return(name)
  } else {
    # Otherwise, backtick-quote it (escape any backticks in the name)
    return(paste0("`", gsub("`", "``", name), "`"))
  }
}

#' Read Data File with Format Detection
#'
#' Reads data from various file formats (CSV, Excel, R data, SAS, etc.)
#' Includes security validation to prevent path traversal attacks.
#'
#' @param file_path Path to the file
#' @param file_name Original filename (optional, for fallback)
#' @return Data frame
#' @export
read_data_file <- function(file_path, file_name = NULL) {
  # SECURITY FIX (HIGH-1): Validate file path to prevent path traversal attacks

  # Check file exists

  if (!file.exists(file_path)) {
    stop("File not found")
  }

  # Normalize path to resolve any .. or symlinks
  real_path <- tryCatch(
    normalizePath(file_path, mustWork = TRUE),
    error = function(e) stop("Invalid file path")
  )

  # Security check: Ensure file is in an allowed location
  # For Shiny apps, files should be in temp directory (from fileInput)
  # or in a known safe location
  temp_dir <- normalizePath(tempdir(), mustWork = FALSE)

  # Check if path is within temp directory (standard for Shiny imports)
  # On Windows, use case-insensitive comparison
  is_in_temp <- if (.Platform$OS.type == "windows") {
    startsWith(tolower(real_path), tolower(temp_dir))
  } else {
    startsWith(real_path, temp_dir)
  }

  # Also allow current working directory for non-Shiny usage
  current_dir <- normalizePath(getwd(), mustWork = FALSE)
  is_in_cwd <- if (.Platform$OS.type == "windows") {
    startsWith(tolower(real_path), tolower(current_dir))
  } else {
    startsWith(real_path, current_dir)
  }

  if (!is_in_temp && !is_in_cwd) {
    # Log the attempt for security monitoring
    message(paste("Security: Blocked file access attempt outside allowed directories:", real_path))
    stop("File access denied: file must be in an allowed directory")
  }

  # Try to get extension from file_name first, fallback to file_path
  if (!is.null(file_name) && file_name != "") {
    ext <- tolower(tools::file_ext(file_name))
  } else {
    ext <- tolower(tools::file_ext(file_path))
  }

  # If extension still empty, try from file_path
  if (ext == "" || is.na(ext)) {
    ext <- tolower(tools::file_ext(file_path))
  }

  # Track if this is a haven format (requires special cleanup)
  is_haven_format <- ext %in% c("sas7bdat", "xpt", "dta", "sav")

  data <- switch(
    ext,

    # CSV formats
    "csv" = readr::read_csv(file_path, show_col_types = FALSE),
    "tsv" = readr::read_tsv(file_path, show_col_types = FALSE),
    "txt" = {
      # Try to detect delimiter by reading multiple lines for better detection
      sample_lines <- tryCatch({
        readLines(file_path, n = 5, warn = FALSE)
      }, error = function(e) character(0))

      if (length(sample_lines) == 0) {
        stop("Unable to read .txt file - file may be empty or corrupted")
      }

      # Count delimiter occurrences across sample lines to detect most likely delimiter
      combined_sample <- paste(sample_lines, collapse = "\n")
      tab_count <- length(gregexpr("\t", combined_sample, fixed = TRUE)[[1]])
      comma_count <- length(gregexpr(",", combined_sample, fixed = TRUE)[[1]])
      semicolon_count <- length(gregexpr(";", combined_sample, fixed = TRUE)[[1]])
      pipe_count <- length(gregexpr("|", combined_sample, fixed = TRUE)[[1]])

      # Fix for gregexpr returning -1 when no match
      if (tab_count == 1 && gregexpr("\t", combined_sample, fixed = TRUE)[[1]][1] == -1) tab_count <- 0
      if (comma_count == 1 && gregexpr(",", combined_sample, fixed = TRUE)[[1]][1] == -1) comma_count <- 0
      if (semicolon_count == 1 && gregexpr(";", combined_sample, fixed = TRUE)[[1]][1] == -1) semicolon_count <- 0
      if (pipe_count == 1 && gregexpr("|", combined_sample, fixed = TRUE)[[1]][1] == -1) pipe_count <- 0

      # Choose the most common delimiter
      max_delim <- max(tab_count, comma_count, semicolon_count, pipe_count)

      if (max_delim == 0) {
        # No common delimiters found - try read_table for whitespace-separated
        readr::read_table(file_path, show_col_types = FALSE)
      } else if (tab_count == max_delim) {
        readr::read_tsv(file_path, show_col_types = FALSE)
      } else if (comma_count == max_delim) {
        readr::read_csv(file_path, show_col_types = FALSE)
      } else if (semicolon_count == max_delim) {
        readr::read_delim(file_path, delim = ";", show_col_types = FALSE)
      } else if (pipe_count == max_delim) {
        readr::read_delim(file_path, delim = "|", show_col_types = FALSE)
      } else {
        # Fallback to whitespace-separated
        readr::read_table(file_path, show_col_types = FALSE)
      }
    },

    # Excel formats
    "xlsx" = {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Package 'readxl' is required to read Excel files. Install it with: install.packages('readxl')")
      }
      readxl::read_excel(file_path)
    },
    "xls" = {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Package 'readxl' is required to read Excel files. Install it with: install.packages('readxl')")
      }
      readxl::read_excel(file_path)
    },

    # R data formats
    "rds" = {
      data_obj <- readRDS(file_path)
      if (!is.data.frame(data_obj)) {
        stop(".rds file does not contain a data frame")
      }
      data_obj
    },
    "rdata" = ,
    "rda" = {
      env <- new.env()
      load(file_path, envir = env)
      # Get the first data frame object
      objs <- ls(env)
      df_objs <- objs[sapply(objs, function(x) is.data.frame(env[[x]]))]
      if (length(df_objs) == 0) {
        stop("No data frame found in .RData file")
      }
      if (length(df_objs) > 1) {
        warning(paste("Multiple data frames found. Using:", df_objs[1]))
      }
      env[[df_objs[1]]]
    },

    # SAS formats
    "sas7bdat" = {
      if (!requireNamespace("haven", quietly = TRUE)) {
        stop("Package 'haven' is required to read SAS files. Install it with: install.packages('haven')")
      }
      haven::read_sas(file_path)
    },
    "xpt" = {
      if (!requireNamespace("haven", quietly = TRUE)) {
        stop("Package 'haven' is required to read SAS transport files. Install it with: install.packages('haven')")
      }
      haven::read_xpt(file_path)
    },

    # Stata format
    "dta" = {
      if (!requireNamespace("haven", quietly = TRUE)) {
        stop("Package 'haven' is required to read Stata files. Install it with: install.packages('haven')")
      }
      haven::read_dta(file_path)
    },

    # SPSS format
    "sav" = {
      if (!requireNamespace("haven", quietly = TRUE)) {
        stop("Package 'haven' is required to read SPSS files. Install it with: install.packages('haven')")
      }
      haven::read_sav(file_path)
    },

    # Fallback - provide helpful error message
    {
      if (ext == "" || is.na(ext)) {
        stop("Unable to determine file format: no file extension found. ",
             "Please rename the file with a valid extension (e.g., .csv, .xlsx, .rds)")
      } else {
        stop(paste0("Unsupported file format: '.", ext, "'. ",
                    "Supported formats: CSV, TSV, TXT, Excel (.xlsx/.xls), ",
                    "R data (.rds/.RData/.rda), SAS (.sas7bdat/.xpt), ",
                    "Stata (.dta), SPSS (.sav)"))
      }
    }
  )

  # Clean up haven formats (SAS, Stata, SPSS)
  # These formats have special column classes that can cause display issues
  if (is_haven_format && requireNamespace("haven", quietly = TRUE)) {
    # Remove labels (converts haven_labelled to base R types)
    data <- haven::zap_labels(data)
    # Remove format attributes (format.stata, format.spss, etc.)
    data <- haven::zap_formats(data)
    # Remove width attributes
    data <- haven::zap_widths(data)
  }

  # Convert to standard data frame
  data <- as.data.frame(data)

  # Final cleanup: ensure no haven_labelled columns remain
  # This catches edge cases where haven cleanup might have missed something
  for (col in names(data)) {
    if (inherits(data[[col]], "haven_labelled")) {
      # Convert to appropriate base R type
      if (is.numeric(data[[col]])) {
        data[[col]] <- as.numeric(data[[col]])
      } else {
        data[[col]] <- as.character(data[[col]])
      }
    }
    # Remove any remaining haven-specific attributes
    attr(data[[col]], "label") <- NULL
    attr(data[[col]], "format.stata") <- NULL
    attr(data[[col]], "format.spss") <- NULL
    attr(data[[col]], "display_width") <- NULL
    attr(data[[col]], "labels") <- NULL
  }

  data
}

#' Sanitize CSV Data for Safe Export
#'
#' Prevents CSV formula injection attacks by sanitizing cell values that start
#' with potentially dangerous characters (=, +, -, @, tab, carriage return).
#' These characters can trigger formula execution in Excel and other spreadsheet applications.
#'
#' @param data A data frame to sanitize
#' @return A sanitized data frame safe for CSV export
#' @keywords internal
sanitize_csv_data <- function(data) {
  # Characters that can trigger formula injection
  dangerous_chars <- c("=", "+", "-", "@", "\t", "\r")

  # Function to sanitize a single value
  sanitize_value <- function(x) {
    if (is.character(x) && length(x) > 0 && !is.na(x)) {
      # Check if string starts with any dangerous character
      first_char <- substr(x, 1, 1)
      if (first_char %in% dangerous_chars) {
        # Prepend single quote to prevent formula interpretation
        # Excel treats 'x as literal text, not a formula
        return(paste0("'", x))
      }
    }
    return(x)
  }

  # Apply sanitization to all character columns
  for (col in names(data)) {
    if (is.character(data[[col]])) {
      data[[col]] <- sapply(data[[col]], sanitize_value, USE.NAMES = FALSE)
    }
  }

  data
}

#' HTML Encode String for Safe Output
#'
#' Encodes special HTML characters to prevent XSS attacks when
#' generating HTML reports or displaying user-controlled content.
#'
#' @param text Character string to encode
#' @return HTML-encoded string safe for insertion into HTML documents
#' @keywords internal
html_encode <- function(text) {
  if (is.null(text) || length(text) == 0) return("")
  text <- as.character(text)
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub('"', "&quot;", text, fixed = TRUE)
  text <- gsub("'", "&#39;", text, fixed = TRUE)
  text
}

#' Hash Session Token for Logging
#'
#' Creates a one-way hash of session tokens to enable log correlation
#' without exposing actual session identifiers.
#'
#' @param token Session token string
#' @return First 8 characters of SHA256 hash
#' @keywords internal
hash_session_token <- function(token) {
  if (is.null(token) || nchar(token) == 0) {
    return("unknown")
  }

  # Use digest package for secure hashing if available
  if (requireNamespace("digest", quietly = TRUE)) {
    hash_full <- digest::digest(token, algo = "sha256", serialize = FALSE)
    return(substr(hash_full, 1, 8))
  }

  # Fallback: simple hash using base R (less secure but doesn't expose token)
  # Convert to integer hash and format as hex
  hash_val <- sum(utf8ToInt(token) * seq_along(utf8ToInt(token))) %% 2^32
  return(sprintf("%08x", hash_val))
}
