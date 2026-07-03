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

test_that("estimate_run_duration CK multiplier is calibrated and has expected relationships", {
  r_ck  <- estimate_run_duration(c(100), "CK",  FALSE, 4)
  r_rfk <- estimate_run_duration(c(100), "RFK", FALSE, 4)
  r_rk  <- estimate_run_duration(c(100), "RK",  FALSE, 4)
  r_ok  <- estimate_run_duration(c(100), "OK",  FALSE, 4)
  r_idw <- estimate_run_duration(c(100), "IDW", FALSE, 4)
  r_tps <- estimate_run_duration(c(100), "TPS", FALSE, 4)
  
  # CK should be in the same order of magnitude as RFK/RK (e.g. within factor of 2)
  expect_true(r_ck$est_time_sec / r_rfk$est_time_sec >= 0.5)
  expect_true(r_ck$est_time_sec / r_rfk$est_time_sec <= 2.0)
  expect_true(r_ck$est_time_sec / r_rk$est_time_sec >= 0.5)
  expect_true(r_ck$est_time_sec / r_rk$est_time_sec <= 2.0)
  
  # CK should be slower than OK, IDW, and TPS
  expect_true(r_ck$est_time_sec > r_ok$est_time_sec)
  expect_true(r_ck$est_time_sec > r_idw$est_time_sec)
  expect_true(r_ck$est_time_sec > r_tps$est_time_sec)
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

test_that("estimate_run_duration prefers history with matching cores when enough records exist", {
  old_wd <- getwd()
  tmp_dir <- tempfile("run_estimator_cores_")
  dir.create(tmp_dir)
  setwd(tmp_dir)
  on.exit({
    setwd(old_wd)
    unlink(tmp_dir, recursive = TRUE)
  }, add = TRUE)

  dir.create("run_history")
  history_file <- file.path("run_history", "run_history.csv")

  # Create mock run history data:
  # - method = "OK"
  # - comp_mode = FALSE
  # - 5 records with cores_used = 2, per_locality_share_sec = 10
  # - 5 records with cores_used = 4, per_locality_share_sec = 100
  mock_history <- data.frame(
    timestamp = rep("2026-07-03 12:00:00", 10),
    method = rep("OK", 10),
    comp_mode = rep(FALSE, 10),
    n_locs_in_batch = rep(1, 10),
    n_samples = rep(100, 10),
    cores_used = c(rep(2, 5), rep(4, 5)),
    batch_elapsed_sec = c(rep(10, 5), rep(100, 5)),
    per_locality_share_sec = c(rep(10, 5), rep(100, 5)),
    stringsAsFactors = FALSE
  )
  write.csv(mock_history, history_file, row.names = FALSE)

  # Scenario 1: cores = 4. Since there are 5 records with cores_used == 4, it should filter to those.
  # Predicted per_locality_share_sec for 100 samples is 100.
  # max_single_loc_time = 100, distributed_time = 33.33, fudge_mult = 1.25 -> 100 * 1.25 = 125
  res_cores_4 <- estimate_run_duration(c(100), "OK", comp_mode = FALSE, cores = 4)

  # Scenario 2: cores = 2. Since there are 5 records with cores_used == 2, it should filter to those.
  # Predicted per_locality_share_sec for 100 samples is 10.
  # max_single_loc_time = 10, distributed_time = 6.67, fudge_mult = 1.25 -> 10 * 1.25 = 12.5
  res_cores_2 <- estimate_run_duration(c(100), "OK", comp_mode = FALSE, cores = 2)

  # Scenario 3: cores = 8. Since there are 0 records (< 5) with cores_used == 8, it should fall back.
  # The fallback is all comp_mode = FALSE records (mixture of 10s and 100s, mean = 55).
  # Predicted per_locality_share_sec for 100 samples is 55.
  # max_single_loc_time = 55, distributed_time = 9.17, fudge_mult = 1.25 -> 55 * 1.25 = 68.75
  res_cores_8 <- estimate_run_duration(c(100), "OK", comp_mode = FALSE, cores = 8)

  # Verify that:
  # 1. cores-matching subset is preferred: res_cores_4 uses the 100s, res_cores_2 uses the 10s.
  # 2. res_cores_8 falls back to the full dataset (mixture).
  expect_equal(res_cores_4$est_time_sec, 125)
  expect_equal(res_cores_2$est_time_sec, 12.5)
  expect_equal(res_cores_8$est_time_sec, 68.75)
})

