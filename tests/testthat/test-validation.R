# ==============================================================================
# Validation Function Tests
# ==============================================================================
# Tests for file import validation, target column validation,
# data dimension validation, and column name validation

# File import Validation Tests ----

test_that("validate_imported_file accepts valid CSV files", {
  # Create valid small CSV
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file), add = TRUE)

  writeLines("col1,col2\n1,2\n3,4", temp_file)

  file_info <- list(
    name = "test.csv",
    datapath = temp_file
  )

  expect_true(validate_imported_file(file_info, max_size_mb = 50))
})

test_that("validate_imported_file rejects oversized files", {
  # Create large file
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file), add = TRUE)

  # Write ~5KB of data to ensure it exceeds 0.001 MB (1024 bytes) on all platforms
  large_data <- paste(rep("a,b,c,d,e,f,g,h,i,j\n", 500), collapse = "")
  writeLines(large_data, temp_file)

  file_info <- list(
    name = "large.csv",
    datapath = temp_file
  )

  # Set very small limit to trigger error (0.001 MB = ~1KB)
  expect_error(
    validate_imported_file(file_info, max_size_mb = 0.001),
    "File size exceeds"
  )
})

test_that("validate_imported_file rejects unsupported extensions", {
  # Create file with unsupported extension
  temp_file <- tempfile(fileext = ".exe")
  on.exit(unlink(temp_file), add = TRUE)

  writeLines("data", temp_file)

  file_info <- list(
    name = "malicious.exe",
    datapath = temp_file
  )

  expect_error(
    validate_imported_file(file_info, max_size_mb = 50),
    "File format not supported"
  )
})

test_that("validate_imported_file accepts all allowed extensions", {
  allowed_exts <- c("csv", "tsv", "txt", "xlsx", "xls",
                    "rds", "rdata", "rda",
                    "sas7bdat", "xpt", "dta", "sav")

  for (ext in allowed_exts) {
    temp_file <- tempfile(fileext = paste0(".", ext))

    # Write minimal valid content
    if (ext == "rds") {
      saveRDS(data.frame(x = 1), temp_file)
    } else {
      writeLines("data", temp_file)
    }

    file_info <- list(
      name = paste0("test.", ext),
      datapath = temp_file
    )

    # Should not error
    result <- tryCatch({
      validate_imported_file(file_info, max_size_mb = 50)
      TRUE
    }, error = function(e) {
      FALSE
    })

    unlink(temp_file)

    expect_true(result, info = paste("Extension", ext, "should be accepted"))
  }
})

test_that("validate_imported_file handles missing file", {
  file_info <- list(
    name = "missing.csv",
    datapath = "/nonexistent/path/file.csv"
  )

  expect_error(
    validate_imported_file(file_info),
    "Invalid file import"
  )
})

test_that("validate_imported_file handles NULL file_info", {
  expect_error(
    validate_imported_file(NULL),
    "Invalid file import"
  )
})

# Target Column Validation Tests ----

test_that("validate_target_column accepts valid target column", {
  data <- data.frame(
    age = c(25, 30, 35),
    income = c(50000, 60000, 70000),
    target = c(1, 0, 1)
  )

  expect_true(validate_target_column(data, "target"))
  expect_true(validate_target_column(data, "age"))
})

test_that("validate_target_column rejects missing column", {
  data <- data.frame(
    age = c(25, 30, 35),
    income = c(50000, 60000, 70000)
  )

  expect_error(
    validate_target_column(data, "nonexistent"),
    "not found in dataset"
  )
})

test_that("validate_target_column rejects NULL target", {
  data <- data.frame(x = 1:3)

  expect_error(
    validate_target_column(data, NULL),
    "Invalid target column"
  )
})

test_that("validate_target_column rejects multiple columns", {
  data <- data.frame(x = 1:3, y = 4:6)

  expect_error(
    validate_target_column(data, c("x", "y")),
    "must be a single column name"
  )
})

test_that("validate_target_column rejects non-character target names", {
  data <- data.frame(x = 1:3)

  # Note: The function checks if column exists before checking type,

  # so a numeric target name will fail with "not found" error
  expect_error(
    validate_target_column(data, 123),
    "not found"
  )
})

test_that("validate_target_column rejects invalid column name formats", {
  data <- data.frame(x = 1:3)

  # Column names with special characters that aren't in data
  # Note: function checks if column exists first, then validates format
  expect_error(
    validate_target_column(data, "123invalid"),
    "not found"
  )

  expect_error(
    validate_target_column(data, "invalid-name"),
    "not found"
  )

  expect_error(
    validate_target_column(data, "invalid name"),
    "not found"
  )
})

# Data Dimensions Validation Tests ----

test_that("validate_data_dimensions accepts normal-sized data", {
  data <- data.frame(
    x = 1:100,
    y = 101:200,
    z = 201:300
  )

  expect_true(validate_data_dimensions(data,
                                       max_rows = 1000,
                                       max_cols = 100,
                                       max_cells = 100000))
})

test_that("validate_data_dimensions rejects too many rows", {
  data <- data.frame(x = 1:1000)

  expect_error(
    validate_data_dimensions(data, max_rows = 100, max_cols = 100),
    "Dataset too large.*rows"
  )
})

test_that("validate_data_dimensions rejects too many columns", {
  # Create data frame with many columns
  data <- as.data.frame(matrix(1, nrow = 10, ncol = 150))

  expect_error(
    validate_data_dimensions(data, max_rows = 1000, max_cols = 100),
    "Too many columns"
  )
})

test_that("validate_data_dimensions rejects too many total cells", {
  # 200 rows x 200 cols = 40,000 cells
  data <- as.data.frame(matrix(1, nrow = 200, ncol = 200))

  expect_error(
    validate_data_dimensions(data, max_rows = 10000, max_cols = 1000, max_cells = 10000),
    "Dataset too large.*cells"
  )
})

test_that("validate_data_dimensions handles edge cases at limits", {
  # Exactly at row limit
  data1 <- data.frame(x = 1:100)
  expect_true(validate_data_dimensions(data1, max_rows = 100, max_cols = 10))

  # Exactly at column limit
  data2 <- as.data.frame(matrix(1, nrow = 10, ncol = 10))
  expect_true(validate_data_dimensions(data2, max_rows = 100, max_cols = 10))

  # Exactly at cell limit
  data3 <- as.data.frame(matrix(1, nrow = 10, ncol = 10))  # 100 cells
  expect_true(validate_data_dimensions(data3, max_rows = 100, max_cols = 100, max_cells = 100))
})

test_that("validate_data_dimensions handles very small data", {
  data <- data.frame(x = 1)

  expect_true(validate_data_dimensions(data,
                                       max_rows = 100,
                                       max_cols = 10,
                                       max_cells = 1000))
})

# Column Name Validation Tests ----

test_that("validate_column_names accepts valid R identifiers", {
  # Note: dots are now rejected for security reasons (formula injection prevention)
  data <- data.frame(
    age = 1:3,
    income = 4:6,
    target = 7:9,
    var_1 = 10:12,
    var_2 = 13:15  # underscore instead of dot
  )

  expect_true(validate_column_names(data))
})

test_that("validate_column_names rejects columns starting with numbers", {
  data <- data.frame(x = 1:3)
  names(data) <- "123invalid"

  expect_error(
    validate_column_names(data),
    "Invalid column names detected"
  )
})

test_that("validate_column_names rejects columns with spaces", {
  data <- data.frame(x = 1:3)
  names(data) <- "invalid name"

  expect_error(
    validate_column_names(data),
    "Invalid column names detected"
  )
})

test_that("validate_column_names rejects columns with hyphens", {
  data <- data.frame(x = 1:3)
  names(data) <- "invalid-name"

  expect_error(
    validate_column_names(data),
    "Invalid column names detected"
  )
})

test_that("validate_column_names rejects reserved R keywords", {
  reserved_keywords <- c("if", "else", "repeat", "while", "function",
                         "for", "in", "next", "break", "TRUE", "FALSE",
                         "NULL", "Inf", "NaN", "NA")

  for (keyword in reserved_keywords) {
    data <- data.frame(x = 1:3)
    names(data) <- keyword

    expect_error(
      validate_column_names(data),
      "Reserved R keywords",
      info = paste("Keyword", keyword, "should be rejected")
    )
  }
})

test_that("validate_column_names rejects excessively long column names", {
  data <- data.frame(x = 1:3)
  # Create name with 101 characters
  names(data) <- paste0(rep("a", 101), collapse = "")

  expect_error(
    validate_column_names(data),
    "exceed maximum length"
  )
})

test_that("validate_column_names accepts column names at length limit", {
  data <- data.frame(x = 1:3)
  # Create name with exactly 100 characters
  names(data) <- paste0(rep("a", 100), collapse = "")

  expect_true(validate_column_names(data))
})

test_that("validate_column_names handles multiple invalid columns", {
  data <- data.frame(x = 1:3, y = 4:6, z = 7:9)
  names(data) <- c("123invalid", "456bad", "789wrong")

  expect_error(
    validate_column_names(data),
    "Invalid column names detected"
  )
})

# Integration Tests: Multi-Layer Validation ----

test_that("validation pipeline catches all issues", {
  # Create problematic data
  data <- data.frame(x = 1:10000)  # Too many rows
  names(data) <- "123invalid"       # Invalid name

  # Dimension validation should catch row count
  expect_error(
    validate_data_dimensions(data, max_rows = 100),
    "Dataset too large"
  )

  # Column name validation should catch invalid name
  expect_error(
    validate_column_names(data),
    "Invalid column names"
  )
})

test_that("validation accepts valid multi-column data", {
  data <- data.frame(
    age = c(25, 30, 35, 40, 45),
    income = c(50000, 60000, 70000, 80000, 90000),
    education = c("HS", "BS", "MS", "PhD", "BS"),
    target = c(0, 1, 1, 0, 1),
    stringsAsFactors = FALSE
  )

  # All validations should pass
  expect_true(validate_data_dimensions(data, max_rows = 1000, max_cols = 100))
  expect_true(validate_column_names(data))
  expect_true(validate_target_column(data, "target"))
})

# Safe Error Message Tests ----

test_that("safe_error_message sanitizes error messages", {
  # Create error with potentially sensitive info
  err <- tryCatch(
    stop("Database connection failed: credential='testvalue_xyz' at line 42"),
    error = function(e) e
  )

  # Safe message should not expose details
  safe_msg <- safe_error_message(err, "Operation failed")

  expect_equal(safe_msg, "Operation failed")
  expect_false(grepl("credential", safe_msg))
  expect_false(grepl("testvalue_xyz", safe_msg))
})

test_that("safe_error_message uses default message", {
  err <- simpleError("Some error")

  safe_msg <- safe_error_message(err)

  expect_equal(safe_msg, "An error occurred")
})
