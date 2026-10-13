# Suite-wide options; testthat sources this after helper.R.

options(
  stringsAsFactors = FALSE,
  warnPartialMatchArgs = TRUE
)

# Undo global.R's showtext_auto().
if (requireNamespace("showtext", quietly = TRUE)) {
  tryCatch(showtext::showtext_auto(FALSE), error = function(e) NULL)
}

# No parallel workers in tests.
if (requireNamespace("future", quietly = TRUE)) {
  tryCatch(future::plan(future::sequential), error = function(e) NULL)
}

# Send the run-duration ETA log to an empty temp dir: the estimator tests see a
# cold start, and no test appends to the user's real history.
options(monolith_history_dir = file.path(tempdir(), "monolith_test_run_history"))
