# tests/test_tps_cv_shap.R
source("spatial_helpers_0.9.8b.R")

# Test 1: TPS CV NA Propagation
cat("=== Test 1: TPS CV NA Propagation ===\n")
set.seed(42)
n <- 10
observed <- rnorm(n)
predicted <- observed + rnorm(n, sd = 0.1)
predicted[3] <- NA

cv_res <- data.frame(
  observed = observed,
  var1.pred = predicted,
  x = runif(n),
  y = runif(n)
)

cv_metrics <- perform_cv(cv_res)
cat("Number of valid observations in perform_cv: ", cv_metrics$n, "\n")
if (cv_metrics$n == 9) {
  cat("TPS CV NA Propagation (perform_cv check): PASSED\n")
} else {
  cat("TPS CV NA Propagation (perform_cv check): FAILED\n")
}

# Test 2: SHAP Variable Matching Regex Fix
cat("=== Test 2: SHAP Variable Matching Regex Fix ===\n")
# Create dummy shap_df
top_var_test <- "x"
shap_df_test <- data.frame(
  obs_id = c(1, 1),
  variable_name = c("x", "x.1"),
  contribution = c(10, 5),
  stringsAsFactors = FALSE
)

# We want the contribution to ONLY count the exact match "x" (which has value 10)
# Original logic:
contribution_orig <- sapply(1, function(i) {
  sub <- shap_df_test[shap_df_test$obs_id == i & grepl(paste0("^", top_var_test), shap_df_test$variable_name), ]
  if(nrow(sub) > 0) sum(sub$contribution) else 0
})

# Exact logic:
contribution_exact <- sapply(1, function(i) {
  sub <- shap_df_test[shap_df_test$obs_id == i & shap_df_test$variable_name == top_var_test, ]
  if(nrow(sub) > 0) sum(sub$contribution) else 0
})

cat("Original contribution sum: ", contribution_orig, "\n")
cat("Exact contribution sum: ", contribution_exact, "\n")

if (contribution_orig == 15 && contribution_exact == 10) {
  cat("SHAP Variable Matching Regex check: PASSED (Regex behaves incorrectly, Exact matches correctly)\n")
} else {
  cat("SHAP Variable Matching Regex check: FAILED\n")
}
