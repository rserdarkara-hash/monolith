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

# ── run phase strip ────────────────────────────────────────────────────────
#
# The reading end of the same protocol. The workers write per-locality
# checkpoints with update_progress_file above; the run observer turns their
# mean into a band (2 = fitting, 3 = predicting, 4 = cross-validating) and
# hands it to update_premium_progress, which drives the four-entry strip on
# the Map Viewer overlay. The strip is what tells a reader which phase a run
# is in, so what gets emitted for a given `step` is asserted rather than
# eyeballed. These tests capture the emitted instruction; they do not execute
# it in a browser.

capture_progress_js <- function(...) {
  emitted <- character(0)
  local_mocked_bindings(
    runjs = function(code, ...) { emitted <<- c(emitted, code); invisible(NULL) },
    html  = function(...) invisible(NULL),
    .package = "shinyjs"
  )
  update_premium_progress(...)
  emitted
}

test_that("update_premium_progress leaves the phase strip alone by default", {
  js <- capture_progress_js(45)
  # Every progress tick moves the bar; only the ones that carry a step may
  # touch the strip, or a mid-phase tick would reset the entries beside it.
  expect_length(js, 1L)
  expect_match(js, "map_progress_bar_inner", fixed = TRUE)
  expect_false(any(grepl("map_step_", js, fixed = TRUE)))
})

test_that("update_premium_progress formats the bar width for both pct forms", {
  expect_match(capture_progress_js(45)[1], "width = '45%'", fixed = TRUE)
  expect_match(capture_progress_js(45.4)[1], "width = '45%'", fixed = TRUE)
  expect_match(capture_progress_js("70%")[1], "width = '70%'", fixed = TRUE)
  expect_match(capture_progress_js("70")[1], "width = '70%'", fixed = TRUE)
})

test_that("a step marks strictly earlier entries done and the named one on", {
  strip <- grep("map_step_", capture_progress_js(50, step = 2L), value = TRUE)
  expect_length(strip, 1L)
  # Strictly earlier: `i < n` finished, `i === n` running. An `i <= n` here
  # would light the phase in progress as already complete.
  expect_match(strip, "(i<n?' done':(i===n?' on':''))", fixed = TRUE)
  expect_match(strip, "})(2);", fixed = TRUE)
})

test_that("step 5 is passed through as the whole-strip-finished sentinel", {
  strip <- grep("map_step_", capture_progress_js(100, step = 5), value = TRUE)
  # The strip has four entries, so the completion call has to sit past the
  # last one: clamping it to 4 would leave Cross-validate showing as running
  # on a run that had finished.
  expect_match(strip, "})(5);", fixed = TRUE)
  expect_match(strip, "for(var i=1;i<=4;i++)", fixed = TRUE)
})

# ── status_file_parts: one parser for both readers ─────────────────────────
# The live progress poller and the completion handler that persists warnings
# into the run log read the same file names. Two copies of this regex is how a
# warning ends up visible in one place and lost in the other.

test_that("status_file_parts maps a file name to its locality and surface", {
  a <- status_file_parts("warn_sess1_Kale_act.txt", "sess1")
  expect_identical(a$locality, "Kale")
  expect_identical(a$target, "act")
  expect_identical(a$label, "Kale (Actual)")

  p <- status_file_parts("/tmp/warn_sess1_Kale_pre.txt", "sess1")
  expect_identical(p$target, "pre")
  expect_identical(p$label, "Kale (Predicted)")

  # The locality is sanitised to [A-Za-z0-9_], so the name itself can contain
  # underscores: only the trailing _act / _pre separates the two fields.
  u <- status_file_parts("warn_sess1_West_Field_act.txt", "sess1")
  expect_identical(u$locality, "West_Field")
  expect_identical(u$target, "act")

  # Progress files share the naming scheme.
  g <- status_file_parts("progress_sess1_West_Field_pre.txt", "sess1", kind = "progress")
  expect_identical(g$locality, "West_Field")
  expect_identical(g$target, "pre")

  # No recognised surface suffix: locality only, no invented label.
  n <- status_file_parts("warn_sess1_Kale.txt", "sess1")
  expect_true(is.na(n$target))
  expect_identical(n$suffix, "")
  expect_identical(n$label, "Kale")
})

test_that("status_file_parts round-trips the names write_warning_file writes", {
  tmp <- tempfile("warn_parts_")
  dir.create(tmp)
  old_dir <- getOption("monolith_progress_dir")
  old_sid <- getOption("monolith_session_id")
  options(monolith_progress_dir = tmp, monolith_session_id = "sid42")
  on.exit({
    options(monolith_progress_dir = old_dir, monolith_session_id = old_sid)
    unlink(tmp, recursive = TRUE)
  }, add = TRUE)

  write_warning_file("West Field", "act", "constant target")
  f <- list.files(tmp, pattern = "^warn_", full.names = TRUE)
  expect_length(f, 1L)
  parts <- status_file_parts(f, "sid42")
  # The writer sanitises, so a space arrives as an underscore; what matters is
  # that the surface suffix is still separated correctly.
  expect_identical(parts$locality, "West_Field")
  expect_identical(parts$target, "act")
  expect_identical(readLines(f, warn = FALSE), "constant target")
})
