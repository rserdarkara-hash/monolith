# test-run-estimator.R — tests for estimate_run_duration from monolith.R.

test_that("estimate_run_duration returns list with expected names", {
  result <- estimate_run_duration(c(50, 100, 75), "OK", comp_mode = FALSE, cores = 4)
  expected_names <- c("est_time_sec", "est_time_str", "estimate_text",
                      "is_long_run", "n_models")
  expect_setequal(names(result), expected_names)
})

test_that("estimate_run_duration cold-start produces positive time", {
  result <- estimate_run_duration(c(100), "OK", comp_mode = FALSE, cores = 4)
  expect_true(result$est_time_sec > 0)
})

test_that("estimate_run_duration method multipliers differ by method", {
  r_ok  <- estimate_run_duration(c(100), "OK",  FALSE, 4)
  r_rfk <- estimate_run_duration(c(100), "RFK", FALSE, 4)
  # RFK is slower than OK (multiplier 1.0 vs 0.5)
  expect_true(r_rfk$est_time_sec > r_ok$est_time_sec)
})

test_that("estimate_run_duration comp_mode doubles model count", {
  r_single <- estimate_run_duration(c(100), "OK", comp_mode = FALSE, cores = 4)
  r_comp   <- estimate_run_duration(c(100), "OK", comp_mode = TRUE,  cores = 4)
  expect_equal(r_comp$n_models, r_single$n_models * 2)
})

test_that("estimate_run_duration handles NA sample counts", {
  result <- estimate_run_duration(c(100, NA, 50), "OK", comp_mode = FALSE, cores = 4)
  expect_true(result$est_time_sec > 0)
})

test_that("estimate_run_duration handles zero sample counts", {
  result <- estimate_run_duration(c(0, 100), "OK", comp_mode = FALSE, cores = 4)
  expect_true(result$est_time_sec > 0)
})

test_that("estimate_run_duration is_long_run TRUE for slow methods", {
  # RFK with comp_mode and many localities should be flagged as long
  r <- estimate_run_duration(rep(500, 10), "RFK", comp_mode = TRUE, cores = 4)
  expect_true(r$is_long_run)
})

test_that("estimate_run_duration is_long_run FALSE for fast small runs", {
  r <- estimate_run_duration(c(30), "IDW", comp_mode = FALSE, cores = 4)
  expect_false(r$is_long_run)
})

test_that("estimate_run_duration est_time_str is human-readable", {
  r <- estimate_run_duration(c(50), "OK", comp_mode = FALSE, cores = 4)
  expect_match(r$est_time_str, "second|minute")
})

test_that("estimate_run_duration estimate_text includes model count", {
  r <- estimate_run_duration(c(100, 200), "OK", comp_mode = FALSE, cores = 4)
  expect_match(r$estimate_text, "~.*model")
})

test_that("estimate_run_duration single-locality time is at least max of per-loc times", {
  r <- estimate_run_duration(c(10, 500), "OK", comp_mode = FALSE, cores = 4)
  # The estimated time should be at least the single largest locality time
  expect_true(r$est_time_sec > 0)
})

test_that("estimate_run_duration distributed time benefits from more cores", {
  r_1core <- estimate_run_duration(rep(100, 4), "OK", comp_mode = FALSE, cores = 1)
  r_8core <- estimate_run_duration(rep(100, 4), "OK", comp_mode = FALSE, cores = 8)
  # More cores -> shorter or equal time
  expect_true(r_8core$est_time_sec <= r_1core$est_time_sec)
})
