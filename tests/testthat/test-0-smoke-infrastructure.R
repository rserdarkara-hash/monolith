# Smoke test — verifies the testthat infrastructure is wired up correctly.
# If this file doesn't run, nothing else will.

test_that("testthat infrastructure loads application functions", {
  # helper.R should have sourced global.R + all helpers + monolith.R
  expect_true(exists("calc_ccc"),              label = "calc_ccc from spatial_helpers.R")
  expect_true(exists("perform_cv"),            label = "perform_cv from spatial_helpers.R")
  expect_true(exists("detect_multicollinearity_engine"), label = "multicollinearity engine")
  expect_true(exists("fuzzy_match_column"),    label = "fuzzy_match_column from ui_helpers.R")
  expect_true(exists("generate_core_plot"),    label = "generate_core_plot from ui_helpers.R")
  expect_true(exists("discretize_numeric_var"),label = "discretize_numeric_var from ui_helpers.R")
  expect_true(exists("compute_normality"),     label = "compute_normality from desc_exploratory_module.R")
  expect_true(exists("monolith_theme_css"),    label = "monolith_theme_css from theme_helpers.R")
  expect_true(exists("compute_governing_factors"), label = "compute_governing_factors from spatial_helpers.R")
  expect_true(exists("get_nut_key"),           label = "get_nut_key from ui_helpers.R")
  expect_true(exists("validate_crs"),          label = "validate_crs from monolith.R")
  expect_true(exists("estimate_run_duration"), label = "estimate_run_duration from monolith.R")
})

test_that("application sourcing honors sequential test startup without changing the app plan", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
  blocks <- Filter(
    function(expr) {
      is.call(expr) &&
        identical(expr[[1]], as.name("if")) &&
        grepl(
          "future::plan",
          paste(deparse(expr), collapse = " "),
          fixed = TRUE
        )
    },
    as.list(parse(file = file.path(root, "global.R")))
  )
  expect_length(blocks, 1L)

  calls <- list()
  current <- future::sequential
  testthat::local_mocked_bindings(
    plan = function(strategy = NULL, ...) {
      if (is.null(strategy)) {
        return(current)
      }
      calls[[length(calls) + 1L]] <<- strategy
      current <<- strategy
    },
    .package = "future"
  )

  # Exercise the real startup branch without opening sockets. A parallel
  # worker attempt during test sourcing must fail even on a healthy runner.
  withr::with_options(list(monolith_test_sequential = TRUE), {
    eval(blocks[[1]])
    expect_identical(calls, list(future::sequential))
  })

  calls <- list()
  current <- future::sequential
  withr::with_options(list(monolith_test_sequential = NULL), {
    eval(blocks[[1]])
    expect_identical(calls, list(future::multisession))
    eval(blocks[[1]])
    expect_length(calls, 1L) # Re-sourcing keeps the existing app pool.
  })

  # Re-load the actual helper to cover both sources and option restoration.
  calls <- list()
  current <- future::sequential
  old_option <- getOption("monolith_test_sequential")
  helper_env <- new.env(parent = globalenv())
  helper_env$.monolith_sourced <- FALSE
  withr::defer(showtext::showtext_auto(FALSE))  # Keep setup.R plotting isolation.
  source(file.path(root, "tests", "testthat", "helper.R"), local = helper_env)
  expect_identical(calls, list(future::sequential, future::sequential))
  expect_identical(getOption("monolith_test_sequential"), old_option)
})

test_that("fixture factories produce valid objects", {
  pts <- make_test_points(20)
  expect_s3_class(pts, "sf")
  expect_true(nrow(pts) == 20)
  expect_true(all(c("v", "pv", "aux1", "aux2") %in% colnames(pts)))

  df <- make_test_df(50)
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) == 50)

  grid <- make_test_grid_safe(pts, res = 50)
  expect_s3_class(grid, "sf")
  expect_true(nrow(grid) > 0)
  expect_true(all(c("x", "y") %in% colnames(grid)))
})

test_that("DESCRIPTION file exists and is parseable", {
  desc_path <- file.path(testthat::test_path(), "..", "..", "DESCRIPTION")
  expect_true(file.exists(desc_path))
  desc <- read.dcf(desc_path)
  expect_true("monolith" %in% desc[, "Package"])
})

test_that("Shiny's upload cap admits every per-file limit the handlers enforce", {
  # Shiny refuses an upload above shiny.maxRequestSize before any handler runs,
  # so a handler limit above the cap is dead code and its message never shows.
  src <- readLines(file.path(testthat::test_path(), "..", "..", "server_data_setup.R"),
                   warn = FALSE)
  mb <- as.numeric(regmatches(src, regexpr("(?<=fsize > )[0-9]+(?= \\* 1024 \\* 1024)",
                                           src, perl = TRUE)))
  expect_gt(length(mb), 0)
  expect_gte(getOption("shiny.maxRequestSize"), max(mb) * 1024^2)
})

test_that("run triggers are counters and draw nothing from the session RNG", {
  expect_identical(next_trigger(NULL), 1L)
  expect_identical(next_trigger(1L), 2L)
  expect_identical(next_trigger(41), 42L)
  set.seed(9); ref <- runif(1)
  set.seed(9); invisible(next_trigger(NULL)); expect_identical(runif(1), ref)
  # No server chunk fires a trigger by drawing a random number: a draw there
  # moves the RNG state every later unseeded call in the session starts from.
  root <- file.path(testthat::test_path(), "..", "..")
  for (f in list.files(root, pattern = "^server_.*[.]R$", full.names = TRUE)) {
    src <- readLines(f, warn = FALSE)
    expect_false(any(grepl("<- *runif[(]", src)), info = basename(f))
  }
})
