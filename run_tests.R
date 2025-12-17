# ==============================================================================
# Test Runner Script for LearnTidyML
# ==============================================================================
# Run this script in RStudio or R console to execute all tests
#
# Usage:
#   source("run_tests.R")
#
# Or from command line:
#   Rscript run_tests.R

cat("\n")
cat("====================================================================\n")
cat("              LearnTidyML Test Suite Runner                        \n")
cat("====================================================================\n")
cat("\n")

# Load required packages
if (!require("devtools", quietly = TRUE)) {
  stop("devtools package is required. Install with: install.packages('devtools')")
}

if (!require("testthat", quietly = TRUE)) {
  stop("testthat package is required. Install with: install.packages('testthat')")
}

# Set working directory to package root
if (basename(getwd()) != "LearnTidyML") {
  if (file.exists("LearnTidyML")) {
    setwd("LearnTidyML")
  }
}

cat("Working directory:", getwd(), "\n\n")

# Run tests
cat("Running tests...\n")
cat("====================================================================\n")

test_results <- devtools::test()

cat("\n====================================================================\n")
cat("Test Summary\n")
cat("====================================================================\n")

print(test_results)

# Check if all tests passed
if (any(test_results$failed > 0)) {
  cat("\n❌ SOME TESTS FAILED\n")
  cat("Review the output above for details.\n")
} else {
  cat("\n✅ ALL TESTS PASSED\n")
}

cat("\n====================================================================\n")
cat("Coverage Analysis\n")
cat("====================================================================\n")

# Measure coverage if covr is available
if (requireNamespace("covr", quietly = TRUE)) {
  cat("\nCalculating test coverage...\n\n")

  coverage <- covr::package_coverage()

  cat("Coverage Summary:\n")
  print(coverage)

  # Get coverage percentage
  coverage_pct <- covr::percent_coverage(coverage)
  cat("\nTotal Coverage:", round(coverage_pct, 2), "%\n")

  if (coverage_pct >= 80) {
    cat("✅ Coverage target met (≥80%)\n")
  } else if (coverage_pct >= 60) {
    cat("⚠️  Coverage acceptable (60-80%)\n")
  } else {
    cat("❌ Coverage below target (<60%)\n")
  }

  cat("\nTo view detailed coverage report, run:\n")
  cat("  covr::report()\n")

} else {
  cat("\nℹ️  Install 'covr' package to measure test coverage:\n")
  cat("  install.packages('covr')\n")
  cat("  Then re-run this script.\n")
}

cat("\n====================================================================\n")
cat("Done!\n")
cat("====================================================================\n")
cat("\n")
