library(testthat)

# Walk up to the project root (the directory holding global.R), so the runner
# works from the root, tests/ or tests/testthat/.
proj_root <- getwd()
while (!file.exists(file.path(proj_root, "global.R"))) {
  parent <- dirname(proj_root)
  if (parent == proj_root) {
    stop("Cannot locate project root: global.R not found in any parent directory")
  }
  proj_root <- parent
}
proj_root <- normalizePath(proj_root, winslash = "/")

# testthat sources helper.R (loads the app) and setup.R (suite options) itself.
testthat::test_dir(
  file.path(proj_root, "tests", "testthat"),
  env = testthat::test_env()
)
