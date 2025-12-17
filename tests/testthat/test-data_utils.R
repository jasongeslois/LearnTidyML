# ==============================================================================
# Data Utility Function Tests
# ==============================================================================
# Tests for file reading, format detection, and data cleaning utilities

# CSV File Reading Tests ----

test_that("read_data_file reads CSV files correctly", {
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file), add = TRUE)

  # Write test CSV
  test_data <- data.frame(
    name = c("Alice", "Bob", "Charlie"),
    age = c(25, 30, 35),
    stringsAsFactors = FALSE
  )
  readr::write_csv(test_data, temp_file)

  # Read back
  result <- read_data_file(temp_file, "test.csv")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 2)
  expect_equal(result$name, c("Alice", "Bob", "Charlie"))
  expect_equal(result$age, c(25, 30, 35))
})

test_that("read_data_file reads TSV files correctly", {
  temp_file <- tempfile(fileext = ".tsv")
  on.exit(unlink(temp_file), add = TRUE)

  # Write test TSV
  writeLines("name\tage\nAlice\t25\nBob\t30", temp_file)

  result <- read_data_file(temp_file, "test.tsv")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(names(result), c("name", "age"))
})

test_that("read_data_file reads RDS files correctly", {
  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file), add = TRUE)

  # Write test RDS
  test_data <- data.frame(
    x = 1:5,
    y = 6:10
  )
  saveRDS(test_data, temp_file)

  result <- read_data_file(temp_file, "test.rds")

  expect_s3_class(result, "data.frame")
  expect_equal(result$x, 1:5)
  expect_equal(result$y, 6:10)
})

test_that("read_data_file rejects non-data.frame RDS files", {
  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file), add = TRUE)

  # Save a list instead of data.frame
  saveRDS(list(a = 1, b = 2), temp_file)

  expect_error(
    read_data_file(temp_file, "test.rds"),
    "does not contain a data frame"
  )
})

test_that("read_data_file handles TXT files with auto-detection", {
  # Test comma-delimited
  temp_csv <- tempfile(fileext = ".txt")
  on.exit(unlink(temp_csv), add = TRUE)
  writeLines("a,b,c\n1,2,3", temp_csv)

  result_csv <- read_data_file(temp_csv, "test.txt")
  expect_equal(ncol(result_csv), 3)

  # Test tab-delimited
  temp_tab <- tempfile(fileext = ".txt")
  on.exit(unlink(temp_tab), add = TRUE)
  writeLines("a\tb\tc\n1\t2\t3", temp_tab)

  result_tab <- read_data_file(temp_tab, "test2.txt")
  expect_equal(ncol(result_tab), 3)

  # Test semicolon-delimited
  temp_semi <- tempfile(fileext = ".txt")
  on.exit(unlink(temp_semi), add = TRUE)
  writeLines("a;b;c\n1;2;3", temp_semi)

  result_semi <- read_data_file(temp_semi, "test3.txt")
  expect_equal(ncol(result_semi), 3)
})

test_that("read_data_file converts to standard data.frame", {
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file), add = TRUE)

  test_data <- data.frame(x = 1:3, y = 4:6)
  readr::write_csv(test_data, temp_file)

  result <- read_data_file(temp_file, "test.csv")

  # Should be a regular data.frame, not tibble
  expect_s3_class(result, "data.frame")
  expect_true(is.data.frame(result))
})

test_that("read_data_file handles files with special characters in data", {
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file), add = TRUE)

  test_data <- data.frame(
    text = c("normal", "with,comma", "with\"quote", "with\nnewline"),
    stringsAsFactors = FALSE
  )
  readr::write_csv(test_data, temp_file)

  result <- read_data_file(temp_file, "test.csv")

  expect_equal(nrow(result), 4)
  expect_equal(result$text[2], "with,comma")
})

# Haven Format Cleanup Tests ----

test_that("read_data_file cleans haven attributes", {
  skip_if_not_installed("haven")

  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file), add = TRUE)

  # Create data with haven-like attributes
  test_data <- data.frame(
    x = 1:3,
    y = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )

  # Add haven-like attributes
  attr(test_data$x, "label") <- "Test Label"
  attr(test_data$x, "format.stata") <- "%9.0g"
  attr(test_data$y, "labels") <- c(A = 1, B = 2, C = 3)

  saveRDS(test_data, temp_file)

  # Note: This tests RDS, but the cleanup logic applies to haven formats
  result <- read_data_file(temp_file, "test.rds")

  # Attributes should be stripped
  expect_null(attr(result$x, "label"))
  expect_null(attr(result$x, "format.stata"))
  expect_null(attr(result$y, "labels"))
})

# Safe R Name Generation Tests ----

test_that("safe_r_name handles simple valid names", {
  expect_equal(safe_r_name("age"), "age")
  expect_equal(safe_r_name("income_2023"), "income_2023")
  expect_equal(safe_r_name("var.1"), "var.1")
})

test_that("safe_r_name quotes reserved keywords", {
  expect_equal(safe_r_name("if"), "`if`")
  expect_equal(safe_r_name("function"), "`function`")
  expect_equal(safe_r_name("TRUE"), "`TRUE`")
  expect_equal(safe_r_name("NULL"), "`NULL`")
})

test_that("safe_r_name quotes names with spaces", {
  expect_equal(safe_r_name("my variable"), "`my variable`")
  expect_equal(safe_r_name("Income 2023"), "`Income 2023`")
})

test_that("safe_r_name quotes names with special characters", {
  expect_equal(safe_r_name("income-2023"), "`income-2023`")
  expect_equal(safe_r_name("my/variable"), "`my/variable`")
  expect_equal(safe_r_name("var@special"), "`var@special`")
})

test_that("safe_r_name escapes backticks in names", {
  # Names containing backticks should have them escaped
  result <- safe_r_name("var`with`backticks")
  expect_true(grepl("``", result))  # Double backticks for escaping
})

test_that("safe_r_name handles empty string", {
  # Empty strings are not valid R names
  result <- safe_r_name("")
  expect_true(grepl("`", result))  # Should be quoted
})

# Timeout Wrapper Tests ----

test_that("with_timeout allows fast operations", {
  result <- with_timeout({
    sum(1:100)
  }, timeout = 5)

  expect_equal(result, 5050)
})

test_that("with_timeout stops long operations", {
  skip_on_cran()  # This test involves timeouts

  expect_error(
    with_timeout({
      Sys.sleep(10)  # Sleep for 10 seconds
    }, timeout = 1),  # But timeout after 1 second
    "timed out|time limit"
  )
})

test_that("with_timeout passes through errors", {
  expect_error(
    with_timeout({
      stop("Test error")
    }, timeout = 5),
    "Test error"
  )
})

# Data Profiling Integration Tests ----

test_that("file reading + profiling pipeline works end-to-end", {
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file), add = TRUE)

  # Create test data
  test_data <- data.frame(
    age = c(25, 30, 35, NA, 40),
    income = c(50000, 60000, 70000, 80000, 90000),
    category = c("A", "B", "A", "B", NA),
    stringsAsFactors = FALSE
  )
  readr::write_csv(test_data, temp_file)

  # Read file
  result <- read_data_file(temp_file, "test.csv")

  # Verify structure
  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 3)

  # Verify NA handling
  expect_true(is.na(result$age[4]))
  expect_true(is.na(result$category[5]))

  # Verify data types
  expect_true(is.numeric(result$age))
  expect_true(is.numeric(result$income))
  expect_true(is.character(result$category))
})

test_that("file reading handles missing values across formats", {
  # CSV with NA
  temp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_csv), add = TRUE)
  writeLines("x,y\n1,2\nNA,4\n3,NA", temp_csv)

  result_csv <- read_data_file(temp_csv, "test.csv")
  expect_true(any(is.na(result_csv$x)))
  expect_true(any(is.na(result_csv$y)))

  # RDS with NA
  temp_rds <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_rds), add = TRUE)
  test_data <- data.frame(x = c(1, NA, 3), y = c(2, 4, NA))
  saveRDS(test_data, temp_rds)

  result_rds <- read_data_file(temp_rds, "test.rds")
  expect_true(any(is.na(result_rds$x)))
  expect_true(any(is.na(result_rds$y)))
})

# Edge Case Tests ----

test_that("read_data_file handles single-row data", {
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file), add = TRUE)

  writeLines("x,y,z\n1,2,3", temp_file)

  result <- read_data_file(temp_file, "test.csv")

  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 3)
})

test_that("read_data_file handles single-column data", {
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file), add = TRUE)

  writeLines("x\n1\n2\n3", temp_file)

  result <- read_data_file(temp_file, "test.csv")

  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 1)
})

test_that("read_data_file handles wide data", {
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file), add = TRUE)

  # Create 50 column data frame
  wide_data <- as.data.frame(matrix(1:200, nrow = 4, ncol = 50))
  readr::write_csv(wide_data, temp_file)

  result <- read_data_file(temp_file, "test.csv")

  expect_equal(ncol(result), 50)
  expect_equal(nrow(result), 4)
})

test_that("read_data_file handles tall data", {
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file), add = TRUE)

  # Create 1000 row data frame
  tall_data <- data.frame(
    x = 1:1000,
    y = 1001:2000
  )
  readr::write_csv(tall_data, temp_file)

  result <- read_data_file(temp_file, "test.csv")

  expect_equal(nrow(result), 1000)
  expect_equal(ncol(result), 2)
})

test_that("read_data_file errors on unsupported formats", {
  temp_file <- tempfile(fileext = ".unknown")
  on.exit(unlink(temp_file), add = TRUE)

  writeLines("data", temp_file)

  expect_error(
    read_data_file(temp_file, "test.unknown"),
    "Unsupported file format"
  )
})

# File Extension Detection Tests ----

test_that("read_data_file uses file_name extension over file_path", {
  # Create CSV file but name it .txt
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file), add = TRUE)

  writeLines("a,b,c\n1,2,3", temp_file)

  # Should read as TXT (and auto-detect CSV from content)
  result <- read_data_file(temp_file, "myfile.txt")

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 3)
})

test_that("read_data_file handles case-insensitive extensions", {
  temp_file <- tempfile(fileext = ".CSV")
  on.exit(unlink(temp_file), add = TRUE)

  writeLines("x,y\n1,2", temp_file)

  # Should work with uppercase extension
  result <- read_data_file(temp_file, "test.CSV")

  expect_s3_class(result, "data.frame")
})
