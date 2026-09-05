# make_baselines.R — record the values that can only be measured, not derived.
#
# Run from the project root, after building a fixture with make_golden.R:
#
#   "C:/Program Files/R/R-4.5.2/bin/Rscript.exe" tests/testthat/fixtures/make_baselines.R
#
# Most of the suite needs nothing from this script: those tests recompute their
# reference from whatever data they are handed, so they are correct for any
# golden set. A few quantities have no closed form to check against - Jenks
# breaks, the VIF pruning order, the fixture's own identity, the end-to-end
# surface digest - and those are recorded here, for THIS golden set.
#
# THE GATE. A recorded value is only meaningful if the code that produced it was
# already known good, so this script runs the full suite first, with the
# baseline-dependent tests bypassed, and REFUSES to record anything unless that
# run is clean. Pinning a number from a broken tree would enshrine the breakage
# as the expected answer, which is the one way a golden-file test can do harm.
#
# Re-run it when, and only when, you deliberately change the golden fixture.
# Never run it to make a failing test pass: a moved baseline is something to
# explain first.

if (!file.exists("global.R")) {
  stop("run make_baselines.R from the project root (the directory holding global.R)")
}

# Everything runs from the project root: testthat::test_path() resolves
# "tests/testthat" from here, which is what helper.R and golden_dir() rely on.
message("Loading the application and fixture helpers ...")
source(file.path("tests", "testthat", "helper.R"))

target_file <- file.path(golden_dir(), "golden_baselines.rds")

# ── The gate ────────────────────────────────────────────────────────────────
message("Running the suite with baselines bypassed ...")
options(monolith_golden_baselines_bypass = TRUE)
res <- testthat::test_dir("tests/testthat", stop_on_failure = FALSE,
                          reporter = "summary")
options(monolith_golden_baselines_bypass = FALSE)

df <- as.data.frame(res)
n_fail <- sum(df$failed) + sum(df$error)
n_skip <- sum(df$skipped)
message(sprintf("Suite: %d passed, %d failed, %d skipped.",
                sum(df$passed), n_fail, n_skip))

if (n_fail > 0) {
  bad <- df$file[df$failed > 0 | df$error > 0]
  stop("REFUSING to record baselines: ", n_fail, " failure(s) in ",
       paste(unique(bad), collapse = ", "),
       ".\nFix the failures first. A baseline recorded from a failing tree ",
       "pins the failure as the expected answer.")
}

# ── The values ──────────────────────────────────────────────────────────────
message("Recording baselines ...")
gs <- golden_soil("full")
meta <- golden_meta()
cov <- meta$columns$covariates
target <- meta$columns$target

baselines <- list()

# The fixture's own identity: what this golden set happens to be. The
# REQUIREMENTS every golden set must meet (projected metres, no missing values,
# a severely collinear pair, a rare class) stay hardcoded in
# test-golden-fixture.R, because those are what the rest of the suite relies on.
baselines$identity <- list(
  n_rows = nrow(gs),
  n_cols = ncol(gs),
  locality_counts = table(gs$locality),
  class_counts = table(gs[[meta$columns$categorical]]),
  x_range = range(gs$x),
  y_range = range(gs$y),
  varlist_dim = if (is.null(golden_varlist())) NULL else dim(golden_varlist()),
  scopes = meta$scopes,
  n_core = nrow(golden_soil("core")),
  n_tiny = nrow(golden_soil("tiny"))
)

# Iterative VIF pruning has no closed form for WHICH covariates go; the test
# proves the ranking against an lm-based VIF at every step, and this records
# the answer that ranking produces on this data.
baselines$vif_drop_order <-
  detect_multicollinearity_engine(gs[cov], vif_threshold = 10)$dropped_vif

# Jenks (Fisher) breaks have no closed form at all.
baselines$jenks_target_5 <- calc_class_breaks(gs[[target]], 5, "jenks")

# End-to-end: does the whole regional driver still assemble the same surface?
baselines$ok_surface_digest <- run_surface_digest(golden_sf("tiny"))

saveRDS(baselines, target_file, version = 3)

cat("\nWritten:", target_file, "\n\n")
cat("identity        : ", baselines$identity$n_rows, "x",
    baselines$identity$n_cols, "rows/cols, core",
    baselines$identity$n_core, ", tiny", baselines$identity$n_tiny, "\n")
cat("vif_drop_order  : ", paste(baselines$vif_drop_order, collapse = ", "), "\n")
cat("jenks_target_5  : ", paste(signif(baselines$jenks_target_5, 12), collapse = ", "), "\n")
cat("ok_surface_digest:\n")
print(baselines$ok_surface_digest)
cat("\nNEXT: re-run the full suite. It should now be green with 0 skips.\n")
