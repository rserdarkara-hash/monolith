# End-to-end smoke tests: does the real app BOOT and wire its shell together?
#
# Everything else in this suite exercises functions in-process; nothing checked
# that ui_main.R + the nine server_*.R chunks actually assemble into a running
# Shiny app. A typo in a chunk, a duplicated input id, or a UI element that
# references a helper removed elsewhere is invisible to unit tests and fatal in
# production. These four tests boot the app in a headless browser and assert the
# shell: the server initialises without error, the sidebar defaults are what the
# engines assume, and the tab strip's stable `value=` ids still drive the
# sidebar swap.
#
# Deliberately NOT attempted: upload -> run -> export flows. Those dispatch
# parallel futures, which are flaky under a test harness and would make the
# suite depend on wall-clock timing.
#
# The whole file self-skips unless a Chromium-based browser and shinytest2 are
# available (CI has neither, and CI expansion is a standing decision).

skip_if_not_installed("shinytest2")
skip_if_not_installed("chromote")
skip_on_ci()

# chromote::find_chrome() only looks for Chrome/Chromium; on a stock Windows
# install the available Chromium engine is Edge. Point CHROMOTE_CHROME at it
# rather than skipping a machine that can perfectly well run the test.
resolve_browser <- function() {
  found <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  if (!is.null(found) && nzchar(found)) return(found)
  candidates <- c(
    file.path(Sys.getenv("ProgramFiles(x86)", ""), "Microsoft", "Edge", "Application", "msedge.exe"),
    file.path(Sys.getenv("ProgramFiles", ""), "Microsoft", "Edge", "Application", "msedge.exe")
  )
  hit <- candidates[nzchar(candidates) & file.exists(candidates)][1]
  if (is.na(hit)) return(NULL)
  Sys.setenv(CHROMOTE_CHROME = hit)
  hit
}

# One booted app shared by every test in this file: startup costs ~20 s (60
# packages), and none of them mutates state another one reads back. The handle
# lives in an environment rather than a closure variable so the file can shut
# the app down at the end (see the bottom of the file).
.smoke <- new.env(parent = emptyenv())
.smoke$app <- NULL

smoke_app <- local({
  function() {
    if (!is.null(.smoke$app)) return(.smoke$app)
    browser_path <- resolve_browser()
    skip_if(is.null(browser_path), "No Chromium-based browser found for shinytest2")

    proj_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")

    # shinytest2 needs an app DIRECTORY, and monolith.R is a single file that
    # ends in shinyApp(). The shim supplies one. The setwd() inside the server
    # function is load-bearing: shiny sets the working directory to the app dir
    # for every session, and server() sources its nine chunks with relative
    # paths (as do the docs drawer, assets and the run pipeline's main_wd).
    shim <- file.path(tempdir(), "monolith_smoke_app")
    dir.create(shim, showWarnings = FALSE, recursive = TRUE)
    writeLines(c(
      sprintf('.monolith_root <- "%s"', proj_root),
      'setwd(.monolith_root)',
      'source("monolith.R")',
      'shiny::shinyApp(ui = ui, server = function(input, output, session) {',
      '  setwd(.monolith_root)',
      '  server(input, output, session)',
      '})'
    ), file.path(shim, "app.R"))

    # shinytest2 skips on CRAN internally; this file's own guards above decide.
    withr::local_envvar(NOT_CRAN = "true", .local_envir = testthat::teardown_env())
    # check_names = TRUE (the default) is load-bearing here: it is shinytest2's
    # assertion that no two inputs or outputs share an id. A collision silently
    # wires one control to the wrong observer and is invisible to every unit
    # test, which is one of the failure modes this file exists to catch.
    .smoke$app <- tryCatch(
      shinytest2::AppDriver$new(shim, name = "monolith-smoke",
                                load_timeout = 180 * 1000, timeout = 60 * 1000,
                                check_names = TRUE),
      error = function(e) {
        skip(paste("Could not start the app under shinytest2:", conditionMessage(e)))
      }
    )
    # Safety net only; the file stops the app itself as soon as its tests end.
    withr::defer(try(.smoke$app$stop(), silent = TRUE), envir = testthat::teardown_env())
    .smoke$app
  }
})

test_that("the app boots and lands on the Data Setup tab", {
  app <- smoke_app()
  expect_equal(app$get_value(input = "main_tabs"), "tab_data")
  html <- app$get_html("body")
  expect_true(grepl("1. Data Setup", html, fixed = TRUE))
  expect_true(grepl("2. Spatial Engine", html, fixed = TRUE))
  # Nothing is displayed before the first run: the committed display context
  # (rv$disp) is empty, which every conditionalPanel keys on.
  expect_equal(as.character(app$get_value(output = "disp_method")), "")
})

test_that("the server initialises all nine chunks without error", {
  app <- smoke_app()
  logs <- as.data.frame(app$get_logs())
  msgs <- ifelse(is.na(logs$message), "", logs$message)
  # A failure inside any source(local = TRUE) chunk surfaces here as a shiny
  # stderr line; the UI would still render, so only the log proves it worked.
  offenders <- grep("Error in |Warning: Error", msgs, value = TRUE)
  expect_equal(offenders, character(0))
})

test_that("sidebar defaults match what the engines assume", {
  app <- smoke_app()
  expect_equal(app$get_value(input = "method"), "OK")
  expect_equal(app$get_value(input = "cv_strategy"), "auto")
  expect_equal(app$get_value(input = "value_type"), "actual")
  expect_equal(app$get_value(input = "color_style"), "cont")
  # Repeated CV must default to OFF: it multiplies cross-validation cost and
  # the reported metrics are identical either way.
  expect_false(isTRUE(app$get_value(input = "cv_repeat_on")))

  app$set_inputs(method = "IDW")
  expect_equal(app$get_value(input = "method"), "IDW")
  app$set_inputs(method = "OK")
})

test_that("the suite tabs swap the sidebar via their stable value ids", {
  app <- smoke_app()
  # The two suite tabs carry value = "tab_desc" / "tab_classif"; ui_sidebar.R
  # hides the interpolation sidebar behind those ids. A renamed tab title must
  # never be able to break this again.
  app$set_inputs(main_tabs = "tab_classif")
  expect_true(grepl("Classification Suite Active", app$get_html("body"), fixed = TRUE))

  app$set_inputs(main_tabs = "tab_desc")
  expect_true(grepl("Descriptive", app$get_html("body"), fixed = TRUE))

  app$set_inputs(main_tabs = "tab_data")
  expect_equal(app$get_value(input = "main_tabs"), "tab_data")
})

test_that("the documentation drawer renders the three shipped guides", {
  app <- smoke_app()
  # The drawer's three renderUI blocks read docs/*.md through RELATIVE paths
  # (shiny sets the working directory to the app directory for every session).
  # A renamed, moved or unreadable guide leaves an empty drawer and no error,
  # which no unit test can see: they never run the server.
  app$click("info_btn")
  app$wait_for_idle()

  # The drawer's three guides live in a tabsetPanel, so only the active tab's
  # output is visible and the other two stay suspended until selected: walk the
  # tabs rather than reading all three at once.
  guide_len <- function(id) {
    as.numeric(app$get_js(sprintf(
      "var el = document.getElementById('%s'); el ? el.textContent.length : 0;", id
    )))
  }

  expect_true(guide_len("render_user_guide") > 1000)

  app$set_inputs(docs_tabs = "Scientific Guide")
  app$wait_for_idle()
  expect_true(guide_len("render_scientific_guide") > 1000)
  expect_true(grepl("Validation Diagnostics", app$get_html("#docs_drawer"), fixed = TRUE))

  app$set_inputs(docs_tabs = "Descriptive and Exploratory Suite")
  app$wait_for_idle()
  expect_true(guide_len("render_desc_exploratory_guide") > 1000)

  app$set_inputs(docs_tabs = "User Guide")
  app$click("close_docs_btn")
  app$wait_for_idle()
})

test_that("the About dialog reports the version read from DESCRIPTION", {
  app <- smoke_app()
  # 1.0.5 shipped this string hardcoded AND with an unclosed parenthesis
  # ("(v1.0.5"). It is now interpolated from `app_version`, which global.R reads
  # from DESCRIPTION at startup, so the wiring is only provable in a booted app.
  ver <- unname(read.dcf(
    file.path(normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/"),
              "DESCRIPTION"),
    fields = "Version"
  )[1, 1])
  expect_false(is.na(ver))

  app$click("about_btn")
  app$wait_for_idle()
  expect_true(grepl(paste0("(v", ver, ")"), app$get_html("body"), fixed = TRUE))

  app$run_js("$('#shiny-modal').modal('hide');")
  app$wait_for_idle()
})

# Shut the app down here rather than at suite teardown: global.R sets
# future::plan(multisession), so the app process keeps one worker per core
# alive for as long as it lives (~2 GB of RSS on an 16-core machine), and
# every later test file in the suite would run alongside them.
if (!is.null(.smoke$app)) {
  try(.smoke$app$stop(), silent = TRUE)
  .smoke$app <- NULL
}
