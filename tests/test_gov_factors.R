# tests/test_gov_factors.R
source("spatial_helpers_0.9.8b.R")

# Check if randomForest and DALEX are available
if (!requireNamespace("randomForest", quietly = TRUE) || !requireNamespace("DALEX", quietly = TRUE)) {
  cat("Required packages (randomForest, DALEX) are not available.\n")
  quit(status = 0)
}

# Create mock data with overlapping variable names: "x" and "x.1"
set.seed(42)
n <- 100
df_mock <- data.frame(
  target = rnorm(n),
  x = rnorm(n),
  x.1 = rnorm(n)
)

# Test 1: Reproducibility
cat("=== Test 1: Governing Factors Reproducibility ===\n")
res1 <- compute_governing_factors(df_mock, "target", c("x", "x.1"))
res2 <- compute_governing_factors(df_mock, "target", c("x", "x.1"))

# Check if feature importances (dropout_loss) are identical
imp1 <- res1$importance
imp2 <- res2$importance

identical_imp <- identical(imp1, imp2)
cat("Are successive runs identical? ", identical_imp, "\n")

if (identical_imp) {
  cat("Reproducibility check: PASSED\n")
} else {
  cat("Reproducibility check: FAILED (Runs are not reproducible yet)\n")
}
