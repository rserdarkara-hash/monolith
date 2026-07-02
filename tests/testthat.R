library(testthat)

# Locate the project root by walking up the directory tree until we find
# global.R.  This works regardless of whether the runner is invoked from the
# project root, tests/, or tests/testthat/ (RStudio, CLI, etc.).
proj_root <- getwd()
while (!file.exists(file.path(proj_root, "global.R"))) {
  parent <- dirname(proj_root)
  if (parent == proj_root) {
    stop("Cannot locate project root: global.R not found in any parent directory")
  }
  proj_root <- parent
}
proj_root <- normalizePath(proj_root, winslash = "/")

# helper.R handles all application sourcing (with shinyApp no-oping, setwd()
# management, and idempotency).  setup.R sets the sequential future plan.
# There is no need to source anything here — testthat will run helper.R and
# setup.R automatically before the test files.
testthat::test_dir(
  file.path(proj_root, "tests", "testthat"),
  env = testthat::test_env()
)
