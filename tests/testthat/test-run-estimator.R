# estimate_run_duration() and its run-history file (global_utils.R).

test_that("monolith_history_file resolves outside the working directory", {
  # The ETA log resolves under the user's per-application data directory, never
  # relative to the working directory (unwritable on a read-only deployment,
  # shared between concurrent sessions). setup.R points the override at a temp
  # dir for the suite, so clear it to see the default.
  default_path <- withr::with_options(
    list(monolith_history_dir = NULL),
    monolith_history_file()
  )
  norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)

  expect_equal(basename(default_path), "run_history.csv")
  expect_equal(norm(dirname(default_path)),
               norm(tools::R_user_dir("monolith", which = "data")))
  expect_false(identical(norm(dirname(default_path)),
                         norm(file.path(getwd(), "run_history"))))

  # The override is what a locked-down deployment (and this suite) relies on.
  expect_equal(
    withr::with_options(list(monolith_history_dir = file.path(tempdir(), "hist_opt")),
                        monolith_history_file()),
    file.path(tempdir(), "hist_opt", "run_history.csv")
  )
})

test_that("a cold-start estimate is labelled rough and a history-fitted one is not", {
  # Below five matching records the number comes from a cold-start formula
  # calibrated on two hardware measurements, with unverified multipliers for
  # OK/IDW/TPS, so it is labelled rough.
  tmp_dir <- tempfile("run_estimator_rough_")
  dir.create(tmp_dir)
  old_hist <- getOption("monolith_history_dir")
  options(monolith_history_dir = tmp_dir)
  on.exit({
    options(monolith_history_dir = old_hist)
    unlink(tmp_dir, recursive = TRUE)
  }, add = TRUE)

  cold <- estimate_run_duration(c(100), "OK", comp_mode = FALSE, cores = 4)
  expect_match(cold$est_time_str, "(rough estimate)", fixed = TRUE)

  write.csv(
    data.frame(
      timestamp = rep("2026-08-13 12:00:00", 6),
      method = rep("OK", 6),
      comp_mode = rep(FALSE, 6),
      n_locs_in_batch = rep(1, 6),
      n_samples = rep(100, 6),
      cores_used = rep(4, 6),
      batch_elapsed_sec = rep(40, 6),
      per_locality_share_sec = rep(40, 6),
      stringsAsFactors = FALSE
    ),
    monolith_history_file(), row.names = FALSE
  )

  warm <- estimate_run_duration(c(100), "OK", comp_mode = FALSE, cores = 4)
  expect_false(grepl("rough estimate", warm$est_time_str, fixed = TRUE))
})

test_that("estimate_run_duration returns list with expected names", {
  result <- estimate_run_duration(c(50, 100, 75), "OK", comp_mode = FALSE, cores = 4)
  expected_names <- c("est_time_sec", "est_time_str", "estimate_text",
                      "is_long_run", "n_models")
  expect_setequal(names(result), expected_names)
})

test_that("degenerate sample counts stand in as 50, and the estimate covers the biggest locality", {
  est <- function(n, ...) estimate_run_duration(n, "OK", comp_mode = FALSE, cores = 4, ...)

  # An unknown (NA) or empty (0) locality count is replaced by 50 rather than
  # dropped, so the estimate is the one for a 50-sample locality - not merely
  # "greater than zero", which every implementation satisfies.
  expect_identical(est(c(100, NA, 50)), est(c(100, 50, 50)))
  expect_identical(est(c(0, 100)), est(c(50, 100)))
  expect_identical(est(c(NA_real_, 0)), est(c(50, 50)))

  for (n in list(c(100), c(100, NA, 50), c(0, 100), c(NA_real_, 0))) {
    r <- est(n)
    expect_true(is.finite(r$est_time_sec))
    expect_gt(r$est_time_sec, 0)
  }

  # A batch is never estimated below its single largest locality, however many
  # cores share the work.
  expect_gte(est(c(10, 500))$est_time_sec, est(c(500))$est_time_sec)
  expect_gte(estimate_run_duration(rep(100, 8), "OK", FALSE, 64)$est_time_sec,
             estimate_run_duration(c(100), "OK", FALSE, 64)$est_time_sec)
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

test_that("estimate_run_duration distributed time benefits from more cores", {
  r_1core <- estimate_run_duration(rep(100, 4), "OK", comp_mode = FALSE, cores = 1)
  r_8core <- estimate_run_duration(rep(100, 4), "OK", comp_mode = FALSE, cores = 8)
  # More cores -> shorter or equal time
  expect_true(r_8core$est_time_sec <= r_1core$est_time_sec)
})

test_that("estimate_run_duration prefers history with matching cores when enough records exist", {
  # Redirect the history option, so the test never writes into the repository
  # or the user's real data dir.
  tmp_dir <- tempfile("run_estimator_cores_")
  dir.create(tmp_dir)
  old_hist <- getOption("monolith_history_dir")
  options(monolith_history_dir = tmp_dir)
  on.exit({
    options(monolith_history_dir = old_hist)
    unlink(tmp_dir, recursive = TRUE)
  }, add = TRUE)

  history_file <- monolith_history_file()

  # Mock history: OK, comp_mode FALSE, 5 records at 2 cores (10 s per locality)
  # and 5 at 4 cores (100 s).
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

  # Expected = max(single-locality time, total / (0.75 * cores)) * 1.25, the
  # history-based margin.
  # cores 4: its 5 matching records give 100 s -> 100 * 1.25 = 125.
  res_cores_4 <- estimate_run_duration(c(100), "OK", comp_mode = FALSE, cores = 4)

  # cores 2: its 5 matching records give 10 s -> 10 * 1.25 = 12.5.
  res_cores_2 <- estimate_run_duration(c(100), "OK", comp_mode = FALSE, cores = 2)

  # cores 8: fewer than 5 matching records, so all 10 are used (mean 55 s)
  # -> 55 * 1.25 = 68.75.
  res_cores_8 <- estimate_run_duration(c(100), "OK", comp_mode = FALSE, cores = 8)

  expect_equal(res_cores_4$est_time_sec, 125)
  expect_equal(res_cores_2$est_time_sec, 12.5)
  expect_equal(res_cores_8$est_time_sec, 68.75)
})


test_that("a malformed run history falls back to the cold-start formula, not a half-filtered frame", {
  # Any failure while reading the history must yield the cold-start path, never
  # a partially filtered frame reaching the ETA lm.
  tmp_dir <- tempfile("run_estimator_broken_")
  dir.create(tmp_dir)
  old_hist <- getOption("monolith_history_dir")
  options(monolith_history_dir = tmp_dir)
  on.exit({
    options(monolith_history_dir = old_hist)
    unlink(tmp_dir, recursive = TRUE)
  }, add = TRUE)

  history_file <- monolith_history_file()

  cold <- estimate_run_duration(c(100), "OK", comp_mode = FALSE, cores = 4)

  # A history file with the per-locality column MISSING: the method/comp_mode
  # filters succeed, so a stale partial frame would survive and reach the lm.
  broken <- data.frame(
    method = rep("OK", 10),
    comp_mode = rep(FALSE, 10),
    cores_used = rep(4, 10),
    n_samples = seq(10, 100, length.out = 10),
    stringsAsFactors = FALSE
  )
  write.csv(broken, history_file, row.names = FALSE)

  res <- estimate_run_duration(c(100), "OK", comp_mode = FALSE, cores = 4)
  expect_true(is.finite(res$est_time_sec))
  expect_gt(res$est_time_sec, 0)
  # Falls back to the cold-start estimate rather than an lm on a bad frame.
  expect_equal(res$est_time_sec, cold$est_time_sec)
})
