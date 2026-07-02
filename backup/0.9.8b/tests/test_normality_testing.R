# tests/test_normality_testing.R
source("global_0.9.8b.R")

cat("=== Starting Normality Testing Unit Tests ===\n")

# Mock data generation
set.seed(42)

# Case 1: n < 3 (insufficient data)
data_small <- c(1.5, 2.3)

# Case 2: 3 <= n < 50, normal data
data_small_normal <- rnorm(30, mean = 10, sd = 2)

# Case 3: 3 <= n < 50, non-normal data (exponential)
data_small_nonnormal <- rexp(30, rate = 0.5)

# Case 4: n >= 50, normal data
data_large_normal <- rnorm(100, mean = 50, sd = 10)

# Case 5: n >= 50, non-normal data (skewed chi-squared)
data_large_nonnormal <- rchisq(100, df = 2)


# Test runner function
run_test_case <- function(name, data, expected_status, expected_method) {
  cat("Running test case:", name, "... ")
  
  res <- tryCatch({
    compute_normality(data)
  }, error = function(e) {
    cat("FAILED with error:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(res)) {
    return(FALSE)
  }
  
  # Check status
  if (res$status != expected_status) {
    cat("FAILED. Expected status:", expected_status, "Got:", res$status, "\n")
    return(FALSE)
  }
  
  # Check test method used
  if (!grepl(expected_method, res$method, ignore.case = TRUE)) {
    cat("FAILED. Expected method like:", expected_method, "Got:", res$method, "\n")
    return(FALSE)
  }
  
  # Check sample size reporting
  expected_n <- sum(!is.na(data))
  if (res$n != expected_n) {
    cat("FAILED. Expected n:", expected_n, "Got:", res$n, "\n")
    return(FALSE)
  }
  
  cat("PASSED\n")
  return(TRUE)
}

# Run all test cases
success <- TRUE
success <- success && run_test_case("Small Sample Size (n < 3)", data_small, "insufficient", "none")
success <- success && run_test_case("Shapiro-Wilk Normal (n = 30)", data_small_normal, "normal", "Shapiro-Wilk")
success <- success && run_test_case("Shapiro-Wilk Non-Normal (n = 30)", data_small_nonnormal, "not_normal", "Shapiro-Wilk")
success <- success && run_test_case("Lilliefors Normal (n = 100)", data_large_normal, "normal", "Lilliefors")
success <- success && run_test_case("Lilliefors Non-Normal (n = 100)", data_large_nonnormal, "not_normal", "Lilliefors")

if (success) {
  cat("\n=== All Normality Tests PASSED ===\n")
  quit(status = 0)
} else {
  cat("\n=== Some Normality Tests FAILED ===\n")
  quit(status = 1)
}
