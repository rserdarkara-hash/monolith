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
