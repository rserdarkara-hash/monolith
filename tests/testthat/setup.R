# setup.R — sourced by testthat before the first test file (but after helper.R).
#
# Use this file for:
#   - Setting R options that should apply to all tests.
#   - Environment variables.
#   - Creating temporary directories shared across test files.
#
# Keep it lightweight — heavy fixtures belong in helper.R.

options(
  stringsAsFactors = FALSE,
  warnPartialMatchArgs = TRUE
)

# Suppress the auto-showtext side effect that global.R normally triggers.
if (requireNamespace("showtext", quietly = TRUE)) {
  tryCatch(showtext::showtext_auto(FALSE), error = function(e) NULL)
}

# Use sequential processing in tests (no parallel workers).
if (requireNamespace("future", quietly = TRUE)) {
  tryCatch(future::plan(future::sequential), error = function(e) NULL)
}

# estimate_run_duration() resolves its ETA log through monolith_history_file(),
# which defaults to the user's real per-application data directory. Point it at
# an empty temp dir for the whole suite so the estimator tests always see a cold
# start (a developer's accumulated timings would otherwise decide their result)
# and no test can append to the real history.
options(monolith_history_dir = file.path(tempdir(), "monolith_test_run_history"))
