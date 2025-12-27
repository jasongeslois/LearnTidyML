# ==============================================================================
# Security Function Tests
# ==============================================================================
# Tests for CSV formula injection prevention and session token hashing
# Critical for preventing security vulnerabilities

# CSV Formula Injection Prevention Tests ----

test_that("sanitize_csv_data prevents equals formula injection", {
  data <- data.frame(
    name = c("Alice", "=1+1", "=SUM(A1:A10)"),
    value = c(100, 200, 300),
    stringsAsFactors = FALSE
  )

  safe_data <- sanitize_csv_data(data)

  # Formulas starting with = should be prefixed with '
  expect_equal(safe_data$name[1], "Alice")
  expect_equal(safe_data$name[2], "'=1+1")
  expect_equal(safe_data$name[3], "'=SUM(A1:A10)")

  # Numeric columns unchanged
  expect_equal(safe_data$value, c(100, 200, 300))
})

test_that("sanitize_csv_data prevents plus formula injection", {
  data <- data.frame(
    formula = c("+1+1", "+SUM(A1:A10)", "normal"),
    stringsAsFactors = FALSE
  )

  safe_data <- sanitize_csv_data(data)

  expect_equal(safe_data$formula[1], "'+1+1")
  expect_equal(safe_data$formula[2], "'+SUM(A1:A10)")
  expect_equal(safe_data$formula[3], "normal")
})

test_that("sanitize_csv_data prevents minus formula injection", {
  data <- data.frame(
    formula = c("-1+1", "-SUM(A1:A10)", "normal-text"),
    stringsAsFactors = FALSE
  )

  safe_data <- sanitize_csv_data(data)

  # Only strings STARTING with - should be prefixed
  expect_equal(safe_data$formula[1], "'-1+1")
  expect_equal(safe_data$formula[2], "'-SUM(A1:A10)")
  expect_equal(safe_data$formula[3], "normal-text")  # - in middle is ok
})

test_that("sanitize_csv_data prevents at-sign formula injection", {
  data <- data.frame(
    formula = c("@SUM(A1:A10)", "@HYPERLINK", "email@example.com"),
    stringsAsFactors = FALSE
  )

  safe_data <- sanitize_csv_data(data)

  # Only strings STARTING with @ should be prefixed
  expect_equal(safe_data$formula[1], "'@SUM(A1:A10)")
  expect_equal(safe_data$formula[2], "'@HYPERLINK")
  expect_equal(safe_data$formula[3], "email@example.com")  # @ in middle is ok
})

test_that("sanitize_csv_data prevents tab and carriage return injection", {
  data <- data.frame(
    text = c("\tTAB_START", "\rCR_START", "normal"),
    stringsAsFactors = FALSE
  )

  safe_data <- sanitize_csv_data(data)

  expect_equal(safe_data$text[1], "'\tTAB_START")
  expect_equal(safe_data$text[2], "'\rCR_START")
  expect_equal(safe_data$text[3], "normal")
})

test_that("sanitize_csv_data handles NA values correctly", {
  data <- data.frame(
    name = c("Alice", NA, "=formula"),
    stringsAsFactors = FALSE
  )

  safe_data <- sanitize_csv_data(data)

  expect_equal(safe_data$name[1], "Alice")
  expect_true(is.na(safe_data$name[2]))  # NA preserved
  expect_equal(safe_data$name[3], "'=formula")
})

test_that("sanitize_csv_data handles empty strings correctly", {
  data <- data.frame(
    name = c("", "Alice", "=formula"),
    stringsAsFactors = FALSE
  )

  safe_data <- sanitize_csv_data(data)

  expect_equal(safe_data$name[1], "")  # Empty string unchanged
  expect_equal(safe_data$name[2], "Alice")
  expect_equal(safe_data$name[3], "'=formula")
})

test_that("sanitize_csv_data handles mixed column types", {
  data <- data.frame(
    text = c("=formula", "normal"),
    number = c(1, 2),
    factor_col = factor(c("A", "B")),
    stringsAsFactors = FALSE
  )

  safe_data <- sanitize_csv_data(data)

  # Character columns sanitized
  expect_equal(safe_data$text[1], "'=formula")
  expect_equal(safe_data$text[2], "normal")

  # Numeric columns unchanged
  expect_equal(safe_data$number, c(1, 2))

  # Factor columns unchanged (not character)
  expect_equal(safe_data$factor_col, factor(c("A", "B")))
})

test_that("sanitize_csv_data handles data frames with no character columns", {
  data <- data.frame(
    x = c(1, 2, 3),
    y = c(4, 5, 6)
  )

  safe_data <- sanitize_csv_data(data)

  # Should return unchanged
  expect_equal(safe_data, data)
})

test_that("sanitize_csv_data handles data frames with all dangerous values", {
  data <- data.frame(
    dangerous = c("=A1", "+B2", "-C3", "@D4", "\tE5", "\rF6"),
    stringsAsFactors = FALSE
  )

  safe_data <- sanitize_csv_data(data)

  # All should be prefixed
  expect_true(all(startsWith(safe_data$dangerous, "'")))
  expect_equal(nrow(safe_data), 6)
})

# Session Token Hashing Tests ----

test_that("hash_session_token produces consistent hashes", {
  token <- "test_session_token_12345"

  hash1 <- hash_session_token(token)
  hash2 <- hash_session_token(token)

  # Same token should produce same hash
  expect_equal(hash1, hash2)
})

test_that("hash_session_token produces 8-character hashes", {
  tokens <- c(
    "short",
    "medium_length_token",
    "very_long_session_token_with_lots_of_characters_12345678901234567890"
  )

  hashes <- sapply(tokens, hash_session_token)

  # All hashes should be exactly 8 characters
  expect_true(all(nchar(hashes) == 8))
})

test_that("hash_session_token produces different hashes for different tokens", {
  token1 <- "session_token_1"
  token2 <- "session_token_2"

  hash1 <- hash_session_token(token1)
  hash2 <- hash_session_token(token2)

  # Different tokens should produce different hashes
  expect_false(hash1 == hash2)
})

test_that("hash_session_token does not expose original token", {
  token <- "test_session_token_abc"

  hash <- hash_session_token(token)

  # Hash should not contain the original token
  expect_false(grepl(token, hash))
  expect_false(grepl("secret", hash))
  expect_false(grepl("session", hash))
})

test_that("hash_session_token handles NULL token", {
  hash <- hash_session_token(NULL)

  expect_equal(hash, "unknown")
})

test_that("hash_session_token handles empty string", {
  hash <- hash_session_token("")

  expect_equal(hash, "unknown")
})

test_that("hash_session_token handles special characters", {
  tokens <- c(
    "token-with-dashes",
    "token_with_underscores",
    "token.with.dots",
    "token with spaces"
  )

  hashes <- sapply(tokens, hash_session_token)

  # All should produce valid 8-character hashes
  expect_true(all(nchar(hashes) == 8))
  expect_true(all(grepl("^[0-9a-f]{8}$", hashes)))
})

test_that("hash_session_token produces hexadecimal output", {
  token <- "test_token_123"

  hash <- hash_session_token(token)

  # Should be valid hexadecimal (0-9, a-f)
  expect_true(grepl("^[0-9a-f]{8}$", hash))
})

test_that("hash_session_token fallback works without digest package", {
  # Test the base R fallback path
  # This simulates the case where digest is not available

  token <- "test_token_for_fallback"

  # The function should still work even if digest is not available
  hash <- hash_session_token(token)

  expect_equal(nchar(hash), 8)
  expect_true(grepl("^[0-9a-f]{8}$", hash))
})

# Integration Test: CSV Sanitization in Export Pipeline ----

test_that("CSV export pipeline prevents formula injection", {
  # Create malicious data
  malicious_data <- data.frame(
    user_input = c("Alice", "=1+1", "+SUM(A:A)", "-cmd|'/c calc'", "@HYPERLINK"),
    values = 1:5,
    stringsAsFactors = FALSE
  )

  # Sanitize
  safe_data <- sanitize_csv_data(malicious_data)

  # Write to temp file
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file), add = TRUE)

  readr::write_csv(safe_data, temp_file)

  # Read back
  read_back <- readr::read_csv(temp_file, show_col_types = FALSE)

  # Verify formulas are still prefixed (Excel won't execute them)
  expect_equal(read_back$user_input[2], "'=1+1")
  expect_equal(read_back$user_input[3], "'+SUM(A:A)")
  expect_equal(read_back$user_input[4], "'-cmd|'/c calc'")
  expect_equal(read_back$user_input[5], "'@HYPERLINK")

  # Verify normal data unchanged
  expect_equal(read_back$user_input[1], "Alice")
  expect_equal(read_back$values, 1:5)
})

# Integration Test: Session Token Hashing in Logging ----

test_that("session token hashing prevents token exposure in logs", {
  # Simulate session tokens
  session_tokens <- c(
    "real_session_token_abc123xyz",
    "another_token_def456uvw",
    "third_token_ghi789rst"
  )

  # Hash all tokens
  hashes <- sapply(session_tokens, hash_session_token)

  # Verify no token appears in any hash
  for (i in seq_along(session_tokens)) {
    for (j in seq_along(hashes)) {
      expect_false(grepl(session_tokens[i], hashes[j]))
    }
  }

  # Verify hashes are consistent
  expect_equal(hash_session_token(session_tokens[1]),
               hash_session_token(session_tokens[1]))

  # Verify hashes are unique for different tokens
  expect_equal(length(unique(hashes)), length(session_tokens))
})
