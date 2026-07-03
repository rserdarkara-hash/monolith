# test-rfk-ntree-consistency.R — tests for rf_ntree parameter consistency
# between the main random forest model and LOOCV fold models in apply_RFK.

test_that("apply_RFK uses consistent rf_ntree for main model and LOOCV folds", {
  # 1. Setup mock/spy to capture ntree arguments
  captured_calls <- list()
  orig_rf <- randomForest::randomForest
  
  mock_rf <- function(...) {
    args <- list(...)
    captured_calls <<- c(captured_calls, list(args))
    do.call(orig_rf, args)
  }
  
  # 2. Prepare synthetic spatial data
  # n = 12 ensures that we run the LOOCV path (3 <= n <= 50)
  pts <- make_test_points(n = 12)
  grid <- make_test_grid_safe(pts, res = 200)
  lags <- calc_scientific_lags(pts)
  
  # Use a distinct, non-default ntree value (e.g. 37)
  target_ntree <- 37
  method_params <- list(rf_ntree = target_ntree)
  
  # 3. Run apply_RFK under the mocked randomForest binding
  testthat::with_mocked_bindings(
    {
      res <- suppressWarnings(
        apply_RFK(
          data = pts,
          target_var = "v",
          grid_p = grid,
          lags = lags,
          method_params = method_params,
          aux_vars = c("aux1", "aux2")
        )
      )
    },
    randomForest = mock_rf,
    .package = "randomForest"
  )
  
  # 4. Verify that we captured calls
  # We expect 1 main model fit + 12 LOOCV fold fits = 13 calls
  expect_gt(length(captured_calls), 1)
  
  # 5. Assert that every captured call's ntree matches target_ntree (37)
  for (i in seq_along(captured_calls)) {
    call_args <- captured_calls[[i]]
    expect_true("ntree" %in% names(call_args), info = paste("Call", i, "does not specify 'ntree'"))
    expect_equal(call_args$ntree, target_ntree, info = paste("Call", i, "ntree parameter does not match target_ntree"))
  }
})
