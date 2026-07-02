# test-progress-files.R — tests for update_progress_file and write_warning_file.

test_that("update_progress_file creates file in temp directory", {
  tmp <- tempfile("progress_test_")
  dir.create(tmp)
  old_progress_dir <- getOption("monolith_progress_dir")
  old_session_id  <- getOption("monolith_session_id")
  options(monolith_progress_dir = tmp, monolith_session_id = "test_session")
  on.exit({
    options(monolith_progress_dir = old_progress_dir,
            monolith_session_id  = old_session_id)
    unlink(tmp, recursive = TRUE)
  }, add = TRUE)

  update_progress_file("loc_A", "act", 50, 100)

  expected_file <- file.path(tmp, "progress_test_session_loc_A_act.txt")
  expect_true(file.exists(expected_file))
  pct <- as.numeric(readLines(expected_file))
  expect_equal(pct, 50)
})

test_that("update_progress_file sanitizes locality name", {
  tmp <- tempfile("progress_test_")
  dir.create(tmp)
  old_progress_dir <- getOption("monolith_progress_dir")
  old_session_id  <- getOption("monolith_session_id")
  options(monolith_progress_dir = tmp, monolith_session_id = "test")
  on.exit({
    options(monolith_progress_dir = old_progress_dir,
            monolith_session_id  = old_session_id)
    unlink(tmp, recursive = TRUE)
  }, add = TRUE)

  # Locality name with special characters
  update_progress_file("Region A (North)", "act", 75, 100)

  files <- list.files(tmp, pattern = "progress_test_Region_A__North__act")
  expect_true(length(files) > 0)
})

test_that("update_progress_file creates directory if it doesn't exist", {
  tmp <- file.path(tempdir(), paste0("nonexistent_", as.integer(Sys.time())))
  # Ensure it doesn't exist
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE)
  old_progress_dir <- getOption("monolith_progress_dir")
  old_session_id  <- getOption("monolith_session_id")
  options(monolith_progress_dir = tmp, monolith_session_id = "test")
  on.exit({
    options(monolith_progress_dir = old_progress_dir,
            monolith_session_id  = old_session_id)
    unlink(tmp, recursive = TRUE)
  }, add = TRUE)

  update_progress_file("loc", "pre", 90, 100)
  expect_true(dir.exists(tmp))
})

test_that("write_warning_file creates warning file", {
  tmp <- tempfile("warn_test_")
  dir.create(tmp)
  old_progress_dir <- getOption("monolith_progress_dir")
  old_session_id  <- getOption("monolith_session_id")
  options(monolith_progress_dir = tmp, monolith_session_id = "test_warn")
  on.exit({
    options(monolith_progress_dir = old_progress_dir,
            monolith_session_id  = old_session_id)
    unlink(tmp, recursive = TRUE)
  }, add = TRUE)

  write_warning_file("loc_B", "act", "Test warning message")

  expected_file <- file.path(tmp, "warn_test_warn_loc_B_act.txt")
  expect_true(file.exists(expected_file))
  msg <- readLines(expected_file)
  expect_equal(msg, "Test warning message")
})
