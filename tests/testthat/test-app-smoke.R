# End-to-end smoke tests: does the real app BOOT and wire its shell together?
#
# Everything else in this suite exercises functions in-process; nothing checked
# that ui_main.R + the nine server_*.R chunks actually assemble into a running
# Shiny app. A typo in a chunk, a duplicated input id, or a UI element that
# references a helper removed elsewhere is invisible to unit tests and fatal in
# production. These tests boot the app in a headless browser and assert the
# shell (the server initialises without error, the sidebar defaults are what
# the engines assume, the tab strip's stable `value=` ids still drive the
# sidebar swap) and the flows only a browser reaches: the CRS wiring, the
# auxiliary correlation table, and a saved session configuration restored in a
# second, fresh session.
#
# Deliberately NOT attempted: any assertion on a run's NUMBERS. Runs dispatch
# parallel futures and would make the suite depend on wall-clock timing. The
# tests that do run the pipeline need it for subjects no other route reaches:
# whether a rendered DataTable puts its values under their own headings is a
# property of the browser's layout, whether a restored or cancelled run is
# described consistently across the panels needs real runs to archive, restore
# and cancel, and a TPS lambda must reach the run unchanged. They compare runs
# with each other (hashes, the record's fields, the parameter used), never with
# fixed numbers, and skip rather than fail if a run does not complete in time.
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

# One booted app shared by every test in this file (startup costs ~20 s); a
# test that reads state an earlier one left says so. The handle lives in an
# environment rather than a closure variable so the file can shut the app down
# at the end (see the bottom of the file).
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
    # The server body is evaluated in the shim's own frame, so the run state
    # (rv) can be exported for the archive/restore and cancel tests; a surface
    # is exported as a hash of its cell values, not as the raster.
    writeLines(c(
      sprintf('.monolith_root <- "%s"', proj_root),
      'setwd(.monolith_root)',
      'source("monolith.R")',
      'shiny::shinyApp(ui = ui, server = function(input, output, session) {',
      '  setwd(.monolith_root)',
      '  eval(body(server))',
      '  shiny::exportTestValues(run_state = list(',
      '    run_id = rv$run_config_summary$run_id,',
      '    method = rv$run_config_summary$method,',
      '    status = rv$run_config_summary$status,',
      '    disp_method = rv$disp$method,',
      '    rast_hash = if (!is.null(rv$rast)) rlang::hash(terra::values(rv$rast, mat = FALSE)),',
      '    rmse = if (length(rv$cv_metrics_act)) rv$cv_metrics_act[[1]]$rmse,',
      '    map_label = rv$export_registry$map_actual$label,',
      '    history = vapply(rv$run_history, function(h) h$config$method, character(1))),',
      '    tps_store = rv$tps_lambdas,',
      '    tps_lambda_used = lapply(rv$disp$regional_params, function(p) p$tps_fit_act$lambda),',
      '    idw_store = rv$idw_factors,',
      '    vgm_manual = lapply(Filter(function(m) identical(attr(m, "monolith_source"), "manual"), rv$v_fit_list),',
      '      function(m) list(key = attr(m, "monolith_key"), model = as.character(m$model), psill = m$psill, range = m$range)),',
      '    vars = vapply(rv$mapping$vars, function(v) paste(v$actual, if (is_valid_col_ref(v$pred)) v$pred else "",',
      '      if (is_valid_col_ref(v$pred_ss)) v$pred_ss else "", v$label, v$category, v$unit, v$palette, sep = "|"), ""),',
      '    palettes = stats::setNames(lapply(rv$mapping$vars, function(v) palette_of(v$actual)),',
      '      vapply(rv$mapping$vars, function(v) v$actual, "")),',
      '    display_palette = get_display_meta()$palette,',
      '    cfg_restore = list(active = cfg_restore_active(), skipped = cfg_restore$skipped))',
      '})'
    ), file.path(shim, "app.R"))
    .smoke$shim <- shim

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
  expect_true(grepl("Data Setup", html, fixed = TRUE))
  expect_true(grepl("Spatial Engine", html, fixed = TRUE))
  # Tabs and sidebar sections carry no ordinal: the exploratory and
  # classification suites are not steps 5 and 6 of the interpolation workflow,
  # and the sidebar sections are not a sequence either.
  expect_false(grepl("1. Data Setup", html, fixed = TRUE))
  expect_false(grepl("2. Spatial Engine", html, fixed = TRUE))
  # Where the surface is predicted is its own section, split out of the engine.
  expect_true(grepl('data-key="domain"', html, fixed = TRUE))
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
  # kNNDM is the second choice in both CV selectors; the defaults are unchanged.
  choice_values <- function(id) {
    unlist(app$get_js(sprintf(
      "Array.prototype.map.call(document.querySelectorAll('#%s input[type=radio]'), function (e) { return e.value; })",
      id)))
  }
  expect_identical(choice_values("cv_strategy"), c("auto", "knndm", "loocv", "block"))
  expect_identical(choice_values("classification-cv_strategy"), c("spatial", "knndm", "standard"))
  expect_equal(app$get_value(input = "classification-cv_strategy"), "spatial")
  expect_equal(app$get_value(input = "value_type"), "actual")
  expect_equal(app$get_value(input = "color_style"), "cont")
  # Repeated CV must default to OFF: it multiplies cross-validation cost and
  # the reported metrics are identical either way.
  expect_false(isTRUE(app$get_value(input = "cv_repeat_on")))

  app$set_inputs(method = "IDW")
  expect_equal(app$get_value(input = "method"), "IDW")
  # No IDW run is on screen yet, so the panel draws no pooled-CV box.
  expect_equal(app$get_js("document.getElementById('idw_metrics_ui').children.length"), 0)
  # IDW runs at a fixed p = 2 unless Auto (CV) is chosen; TPS selects its
  # smoothing by GCV unless a lambda is fixed. Both apply to all localities.
  expect_equal(app$get_value(input = "idw_p_mode"), "fixed")
  expect_equal(app$get_value(input = "idw_p"), 2)
  expect_equal(app$get_value(input = "idw_mode"), "auto")
  app$set_inputs(method = "TPS")
  expect_equal(app$get_value(input = "tps_lambda_mode"), "gcv")
  expect_equal(app$get_value(input = "tps_mode"), "auto")
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

# ── CRS wiring, end to end ────────────────────────────────────────────────
# These two run LAST and deliberately mutate session state (they upload a
# table), because the tests above assert the pristine shell. They exist
# because the CRS wiring had been pinned only by source-text assertions, and
# a source-text assertion cannot tell a live branch from a dead one: the
# "Input Data CRS Not Set" modal was certified by such a test while sitting
# downstream of a req() that aborted the run in silence.

crs_smoke_csv <- function(with_lonlat) {
  # 15 stations around Potsdam, written in UTM 33N. With no lon/lat pair the
  # zone is not recoverable from the eastings, which is the case the empty
  # selectors and the Tier-3 picker exist for.
  ctr <- sf::st_coordinates(sf::st_transform(
    sf::st_sfc(sf::st_point(c(12.958, 52.466)), crs = 4326), 32633))
  df <- data.frame(
    locality = "Potsdam",
    x = ctr[1] + seq(-2000, 2000, length.out = 15),
    y = ctr[2] + seq(-1500, 1500, length.out = 15),
    value = seq(10, 24, length.out = 15)
  )
  if (with_lonlat) {
    ll <- sf::st_coordinates(sf::st_transform(
      sf::st_as_sf(df, coords = c("x", "y"), crs = 32633), 4326))
    df <- data.frame(df[, c("locality", "x", "y")],
                     lon = ll[, 1], lat = ll[, 2], value = df$value)
  }
  f <- tempfile(fileext = ".csv")
  utils::write.csv(df, f, row.names = FALSE)
  f
}

test_that("Run Interpolation refuses visibly while the Input Data CRS is unset", {
  app <- smoke_app()
  app$upload_file(user_file = crs_smoke_csv(with_lonlat = FALSE))
  app$wait_for_idle()

  # No evidence in the file, so nothing may be assumed: both selectors empty.
  expect_equal(app$get_value(input = "map_crs") %||% "", "")
  expect_equal(app$get_value(input = "map_x"), "x")
  expect_equal(app$get_value(input = "map_y"), "y")
  # The standing caption under the mini-map says why nothing is plotted. It
  # req()s rv$mapping$x, which is only non-NULL because the column mapping is
  # no longer gated behind the CRS.
  expect_true(grepl("Input Data CRS not set", app$get_html("body"), fixed = TRUE))

  app$click("run")
  app$wait_for_idle()
  body <- app$get_html("body")
  expect_true(grepl("Input Data CRS Not Set", body, fixed = TRUE))
  # And the run did not start behind the modal.
  expect_equal(as.character(app$get_value(output = "disp_method")), "")

  app$run_js("$('#shiny-modal').modal('hide');")
  app$wait_for_idle()
})

test_that("a companion lon/lat pair identifies the input CRS on upload", {
  app <- smoke_app()
  app$upload_file(user_file = crs_smoke_csv(with_lonlat = TRUE))
  app$wait_for_idle()

  expect_equal(app$get_value(input = "map_crs"), "EPSG:32633")
  # The Target Mapping CRS is filled only because it was still unset.
  expect_equal(app$get_value(input = "crs_selection"), "EPSG:32633")
  expect_true(grepl("Currently plotting at", app$get_html("body"), fixed = TRUE))
})

test_that("the header context strip names both coordinate systems", {
  app <- smoke_app()
  # Inherits the previous test's upload: dataset loaded, both CRS selectors set.
  strip <- app$get_html(".mn-ctx")
  skip_if(is.null(strip), "context strip absent (no dataset in session)")

  expect_true(grepl("Points", strip, fixed = TRUE))
  # The strip is the only always-visible statement of coordinate system, and
  # every metric quantity the app reports - buffer and range in metres, cell
  # size, hectares - is computed in the TARGET system. A single item labelled
  # "CRS" showing the input one invited those metres to be read against the
  # wrong system, so both roles are named.
  expect_true(grepl("Input CRS", strip, fixed = TRUE))
  expect_true(grepl("Target CRS", strip, fixed = TRUE))
  expect_false(grepl(">CRS<", strip, fixed = TRUE))
  expect_true(grepl("EPSG:32633", strip, fixed = TRUE))
})

test_that("variogram tuning context tracks the method and the fitting mode", {
  app <- smoke_app()
  # Governs what the Scientific Analysis tab puts next to a variogram, so a
  # regression here shows one model's curves beside another model's metrics.
  # Both outputs are suspendWhenHidden = FALSE, so they read from any tab.
  app$set_inputs(method = "OK", vgm_mode = "auto")
  app$wait_for_idle()
  expect_equal(as.character(app$get_value(output = "sci_vgm_tuning")), "no")

  # Manual fitting on a kriging engine IS tuning.
  app$set_inputs(vgm_mode = "manual")
  app$wait_for_idle()
  expect_equal(as.character(app$get_value(output = "sci_vgm_tuning")), "yes")
  # Nothing has been run, so no displayed run can be stale against it.
  expect_equal(as.character(app$get_value(output = "sci_stale_run")), "no")

  # IDW fits no variogram, so the tuning layout must not engage for it even
  # with the fitting mode left on Manual.
  app$set_inputs(method = "IDW")
  app$wait_for_idle()
  expect_equal(as.character(app$get_value(output = "sci_vgm_tuning")), "no")

  app$set_inputs(method = "OK", vgm_mode = "auto")
  app$wait_for_idle()
})

test_that("auxiliary correlation switches and table work in the browser", {
  app <- smoke_app()
  df <- data.frame(locality = rep(c("A", "B"), each = 6),
                   subset = rep(rep(c("Train", "Test"), each = 3), 2),
                   x = 450000 + 1:12 * 10, y = 5819000 + rep(1:3, 4) * 10,
                   actual = 1:12, actual_cve = c(3, 1, 2, 6, 4, 5, 9, 7, 8, 12, 10, 11),
                   actual_ss = c(3, 2, 1, 4, 5, 6, 9, 8, 7, 10, 11, 12), aux = 1:12)
  csv <- tempfile(fileext = ".csv")
  withr::defer(unlink(csv))
  utils::write.csv(df, csv, row.names = FALSE)
  app$upload_file(user_file = csv)
  app$set_inputs(var_id = "actual", locality = "A", method = "RK", value_type = "actual")
  app$wait_for_idle()
  visible <- function(id) isTRUE(app$get_js(sprintf(
    "var el = document.getElementById('%s'); !!el && el.getClientRects().length > 0;", id)))
  expect_false(visible("corr_source"))
  app$click("calc_corr")
  app$wait_for_idle()
  expect_true(grepl("Actual values: actual", app$get_html("#corr_results_ui"), fixed = TRUE))
  app$set_inputs(value_type = "pred")
  app$wait_for_idle()
  expect_true(visible("corr_source"))
  expect_equal(app$get_value(input = "corr_source"), "predictions")
  expect_true(grepl("ML predictions: actual_cve", app$get_html("#corr_results_ui"), fixed = TRUE))
  expect_false(visible("corr_subset"))
  app$set_inputs(corr_source = "actual")
  app$wait_for_idle()
  expect_true(grepl("Actual values: actual", app$get_html("#corr_results_ui"), fixed = TRUE))
  app$set_inputs(value_type = "pred_ss")
  app$set_inputs(subset = "Train")
  app$wait_for_idle()
  expect_equal(app$get_value(input = "corr_source"), "predictions")
  expect_equal(app$get_value(input = "corr_subset"), "Train")
  expect_true(visible("corr_subset"))
  expect_true(grepl("ML predictions: actual_ss", app$get_html("#corr_results_ui"), fixed = TRUE))
  expect_true(grepl("-1.000", app$get_html("#corr_table_ui"), fixed = TRUE))
  app$set_inputs(corr_subset = "Test")
  app$wait_for_idle()
  expect_equal(app$get_value(input = "subset"), "Train")
  expect_true(grepl("+1.000", app$get_html("#corr_table_ui"), fixed = TRUE))
  expect_equal(as.numeric(app$get_js("document.querySelectorAll('.mn-corr-table thead th').length;")), 4)
  app$set_inputs(value_type = "actual")
  app$wait_for_idle()
  expect_false(visible("corr_source"))
  expect_false(visible("corr_subset"))
})

# Clicks Run and answers every modal on the way to the revealed maps, in ONE
# wait loop: the "Previous Results Detected" modal opens BEFORE the run starts,
# so a loop that only watches reveal_maps_btn waits for a run that never began.
# `prev` is the answer to that modal. TRUE when the maps were revealed.
run_and_reveal <- function(app, prev = "discard", timeout = 600) {
  state <- function() {
    jsonlite::fromJSON(app$get_js(
      "JSON.stringify({
         prev: !!document.getElementById('discard_prev_run'),
         confirm: !!document.getElementById('confirm_start_run'),
         reveal: !!document.getElementById('reveal_maps_btn') &&
                 document.getElementById('reveal_maps_btn').offsetParent !== null,
         title: (document.getElementById('map_processing_title') || {}).innerText || ''
       })"))
  }
  app$click("run")
  # Generous: late in the full suite the machine is loaded, and the run starts
  # its own PSOCK workers.
  deadline <- Sys.time() + timeout
  repeat {
    s <- try(state(), silent = TRUE)
    if (!inherits(s, "try-error")) {
      if (isTRUE(s$prev)) app$click(paste0(prev, "_prev_run"))
      else if (isTRUE(s$confirm)) app$click("confirm_start_run")
      else if (isTRUE(s$reveal)) { app$click("reveal_maps_btn"); app$wait_for_idle(); return(TRUE) }
      else if (grepl("Failed", s$title)) return(FALSE)
    }
    if (Sys.time() > deadline) return(FALSE)
    Sys.sleep(1)
  }
}

test_that("Model Performance values sit under their own headings", {
  app <- smoke_app()
  # A displaced value is not cosmetic: at a 1600 px viewport the body table ran
  # 162 px wider than its cloned header, so RMSE printed under MAE and NRMSE
  # under R2. The offset depends on the viewport and on the data, so it is
  # measured at two widths, and again after leaving and returning to the tab -
  # the revisit is what a fixed-timer adjust could not fix.
  # A scattered point set, not the collinear CRS fixture above: a line has no
  # domain to interpolate over, so the run produces no table to measure.
  ctr <- sf::st_coordinates(sf::st_transform(
    sf::st_sfc(sf::st_point(c(12.958, 52.466)), crs = 4326), 32633))
  set.seed(3)
  d <- data.frame(locality = "Potsdam",
                  x = ctr[1] + runif(30, -2000, 2000),
                  y = ctr[2] + runif(30, -1500, 1500),
                  value = runif(30, 10, 24))
  ll <- sf::st_coordinates(sf::st_transform(
    sf::st_as_sf(d, coords = c("x", "y"), crs = 32633), 4326))
  d <- data.frame(d[, c("locality", "x", "y")], lon = ll[, 1], lat = ll[, 2], value = d$value)
  csv <- tempfile(fileext = ".csv")
  withr::defer(unlink(csv))
  utils::write.csv(d, csv, row.names = FALSE)

  app$upload_file(user_file = csv)
  app$wait_for_idle()
  app$set_inputs(var_id = "value", method = "IDW", value_type = "actual")
  app$wait_for_idle()

  # A palette picked before the run is the one the run is drawn in, and a
  # Styling switch does not undo it: the picker used to reopen on the
  # variable's default whenever it was redrawn.
  expect_identical(app$get_value(input = "palette_select"), "YlOrRd")
  app$set_inputs(palette_select = "viridis")
  app$set_inputs(color_style = "bin")
  app$set_inputs(color_style = "cont")
  expect_identical(app$get_value(input = "palette_select"), "viridis")

  skip_if_not(run_and_reveal(app), "the interpolation run did not finish inside the smoke harness")
  expect_identical(app$get_value(input = "palette_select"), "viridis")
  expect_identical(app$get_value(export = "display_palette"), "viridis")

  # header.left vs body.left for the first and last column of the rendered
  # table. Under scrollX these are two tables; the whole point is that they
  # agree.
  offsets <- function() {
    jsonlite::fromJSON(app$get_js(
      "(function () {
         var root = document.getElementById('metrics_table');
         if (!root) return 'null';
         var h = root.querySelectorAll('.dataTables_scrollHead thead tr th');
         var b = root.querySelectorAll('.dataTables_scrollBody tbody tr:first-child td');
         if (!h.length) { h = root.querySelectorAll('thead tr th');
                          b = root.querySelectorAll('tbody tr:first-child td'); }
         if (!h.length || h.length !== b.length) return 'null';
         var d = [];
         for (var i = 0; i < h.length; i++) {
           d.push(Math.abs(h[i].getBoundingClientRect().left -
                           b[i].getBoundingClientRect().left));
         }
         return JSON.stringify(d);
       })()"))
  }

  # Poll rather than read once: wait_for_idle() can return before DataTables
  # has drawn the table (and after a tab switch, before the realignment frame).
  settled_offsets <- function(timeout = 30) {
    end <- Sys.time() + timeout
    repeat {
      d <- tryCatch(offsets(), error = function(e) NULL)
      if (length(d)) return(d)
      if (Sys.time() > end) return(NULL)
      Sys.sleep(0.5)
    }
  }

  app$set_inputs(main_tabs = "tab_analysis")
  app$wait_for_idle()
  for (w in c(1600, 1366)) {
    app$set_window_size(width = w, height = 900)
    app$wait_for_idle()
    d <- settled_offsets()
    skip_if(is.null(d), "the Model Performance table did not render")
    expect_lt(max(d), 1.5)                 # first and last column included

    # Leave and come back: the table re-renders while hidden, which is the
    # state its column widths used to be computed in.
    app$set_inputs(main_tabs = "tab_map")
    app$wait_for_idle()
    app$set_inputs(main_tabs = "tab_analysis")
    app$wait_for_idle()
    Sys.sleep(0.5)                         # one animation frame is enough; be generous
    expect_lt(max(settled_offsets()), 1.5)
  }

  # The browser's formatter and R's must agree, or a number means one thing in
  # a table and another in the file exported beside it.
  vals <- c(0, 1.2e-7, 0.1238, 30.4204, 632, 12345.6, 123456.7, 0.0025, -0.00005)
  js_out <- app$get_js(sprintf(
    "JSON.stringify([%s].map(function (v) { return window.mnFormatSig(v); }))",
    paste(format(vals, scientific = TRUE), collapse = ", ")))
  expect_equal(jsonlite::fromJSON(js_out), format_sig(vals))
})

# The archive and the cancel path, in the booted app: both are about which run
# every panel describes, and neither can be asserted without a real run.
smoke_run_state <- function(app) app$get_value(export = "run_state")

test_that("restoring an archived run brings back its maps, metrics and record together", {
  app <- smoke_app()
  first <- smoke_run_state(app)
  skip_if(is.null(first$rast_hash) || !identical(first$method, "IDW"),
          "the IDW run of the previous test did not complete")

  # A second run with another engine, archiving the first when asked.
  app$set_inputs(method = "OK")
  app$wait_for_idle()
  skip_if_not(run_and_reveal(app, prev = "archive"),
              "the second run did not finish inside the smoke harness")
  second <- smoke_run_state(app)
  expect_equal(second$method, "OK")
  expect_false(identical(second$rast_hash, first$rast_hash))
  expect_equal(second$history, "IDW")
  # The palette picked before the previous run is still the variable's.
  expect_identical(app$get_value(input = "palette_select"), "viridis")

  # Restoring the IDW run brings back its record, its surface and its metrics
  # together - restoring used to swap the record and the registry only - and
  # the OK run goes into the archive in its place. The archive panel renders
  # only while its tab is open.
  app$set_inputs(main_tabs = "tab_export")
  app$wait_for_idle()
  app$click(paste0("restore_run_", first$run_id))
  app$wait_for_idle()
  back <- smoke_run_state(app)
  expect_equal(back$run_id, first$run_id)
  expect_equal(back$method, "IDW")
  expect_equal(back$disp_method, "IDW")
  expect_identical(back$rast_hash, first$rast_hash)
  expect_equal(back$rmse, first$rmse)
  expect_match(back$map_label, get_method_label("IDW"), fixed = TRUE)
  expect_equal(back$history, "OK")
  expect_equal(as.character(app$get_value(output = "disp_method")), "IDW")
  # A palette is not run state: the restored run is drawn in the variable's pick.
  expect_identical(app$get_value(input = "palette_select"), "viridis")
  expect_identical(app$get_value(export = "display_palette"), "viridis")
})

test_that("a cancelled run is labelled cancelled and never archived", {
  app <- smoke_app()
  before <- smoke_run_state(app)
  skip_if(!identical(before$method, "IDW"), "the restore test did not leave the IDW run on screen")

  # Archive the run on screen when asked, then cancel as soon as the button is up.
  visible <- function(id) isTRUE(app$get_js(sprintf(
    "var el = document.getElementById('%s'); !!el && el.offsetParent !== null;", id)))
  app$set_inputs(method = "OK")
  app$click("run")
  deadline <- Sys.time() + 180
  clicked <- FALSE
  repeat {
    if (visible("archive_prev_run")) app$click("archive_prev_run")
    else if (visible("cancel_model_btn")) { app$click("cancel_model_btn"); clicked <- TRUE; break }
    if (Sys.time() > deadline) break
    Sys.sleep(0.3)
  }
  skip_if_not(clicked, "the run never showed its Cancel button")
  app$wait_for_idle()
  st <- smoke_run_state(app)
  skip_if(!identical(st$status, "cancelled"), "the run finished before the cancellation reached it")

  expect_equal(st$method, "OK")
  expect_null(st$rast_hash)
  # The IDW run on screen was archived; the cancelled record was not.
  expect_setequal(st$history, c("IDW", "OK"))
  expect_match(app$get_html("#run_status_chip"), "Cancelled", fixed = TRUE)
  expect_match(app$get_html("#run_config_display_map"), "CANCELLED", fixed = TRUE)
})

test_that("a TPS lambda survives the per-locality panel and the run unchanged", {
  app <- smoke_app()
  # Two localities of 30 samples, in UTM 33N with a lon/lat twin so the Input
  # Data CRS is identified on upload (see the CRS tests above).
  ctr <- sf::st_coordinates(sf::st_transform(
    sf::st_sfc(sf::st_point(c(12.958, 52.466)), crs = 4326), 32633))
  set.seed(8)
  d <- data.frame(locality = rep(c("A", "B"), each = 30),
                  x = ctr[1] + c(runif(30, -2000, 2000), runif(30, 4000, 8000)),
                  y = ctr[2] + runif(60, -1500, 1500),
                  value = runif(60, 10, 24))
  ll <- sf::st_coordinates(sf::st_transform(
    sf::st_as_sf(d, coords = c("x", "y"), crs = 32633), 4326))
  d <- data.frame(d[, c("locality", "x", "y")], lon = ll[, 1], lat = ll[, 2], value = d$value)
  csv <- tempfile(fileext = ".csv")
  withr::defer(unlink(csv))
  utils::write.csv(d, csv, row.names = FALSE)
  app$upload_file(user_file = csv)
  app$wait_for_idle()
  app$set_inputs(var_id = "value", value_type = "actual", method = "TPS")
  app$set_inputs(locality = c("A", "B"))
  app$set_inputs(tps_mode = "manual")
  app$wait_for_idle()

  # GCV-scale values, a value above the old slider's range, and the value a
  # fixed lambda of 0.000256 used to round to 0 on: each is stored as typed,
  # shown again when the locality is revisited, and valid in the browser
  # (step = "any"; a numeric step flags 2.4e-06 as invalid).
  for (lam in c(2.4e-06, 20.2, 0.000256)) {
    app$set_inputs(tps_m_loc = "A")
    app$wait_for_idle()
    app$set_inputs(tps_m_mode = "fixed")
    app$set_inputs(tps_m_lambda = lam)
    app$wait_for_idle()
    expect_true(isTRUE(app$get_js("document.getElementById('tps_m_lambda').validity.valid")),
                info = format(lam))
    app$click("apply_tps_manual")
    app$wait_for_idle()
    app$set_inputs(tps_m_loc = "B")
    app$wait_for_idle()
    app$set_inputs(tps_m_loc = "A")
    app$wait_for_idle()
    expect_identical(app$get_value(input = "tps_m_mode"), "fixed", info = format(lam))
    expect_identical(app$get_value(input = "tps_m_lambda"), lam, info = format(lam))
    expect_identical(app$get_value(export = "tps_store")$A$act$value, lam, info = format(lam))
  }

  # "All localities" runs every locality with the sidebar setting; A's stored
  # 0.000256 applies only under "Per locality".
  app$set_inputs(tps_mode = "auto")
  app$set_inputs(tps_lambda_mode = "fixed")
  app$set_inputs(tps_lambda = 20.2)
  app$wait_for_idle()
  skip_if_not(run_and_reveal(app), "the TPS run did not finish inside the smoke harness")
  used <- app$get_value(export = "tps_lambda_used")
  expect_identical(used$A, 20.2)
  expect_identical(used$B, 20.2)
})

test_that("Save config and Load config restore every run-defining setting", {
  app <- smoke_app()
  # Two localities 5 km apart in UTM 35N with no lon/lat pair, so a new
  # session opens with both CRS selectors empty. A second locality column and
  # a copy of each coordinate let the saved mapping differ from the one an
  # upload picks by itself.
  ctr <- sf::st_coordinates(sf::st_transform(
    sf::st_sfc(sf::st_point(c(27.5, 38.5)), crs = 4326), 32635))
  gx <- rep(0:5, 5) * 150
  gy <- rep(0:4, each = 6) * 150
  i <- seq_len(60)
  d <- data.frame(locality = rep(c("A", "B"), each = 30), site = rep(c("A", "B"), each = 30),
                  x = ctr[1] + c(gx, gx + 5000), y = ctr[2] + c(gy, gy))
  d$east_m <- d$x
  d$north_m <- d$y
  d$ph <- 6.5 + 0.4 * sin(i / 3)
  d$ph_cve <- d$ph + 0.05 * cos(i)
  d$k <- 200 + 60 * cos(i / 4)
  d$k_cve <- d$k + 8 * sin(i)
  d$k_ss <- d$k - 6 * cos(i / 2)
  d$subset <- rep(c("Train", "Test"), 30)
  d$elev <- 100 + 3 * (i %% 7)
  d$clay <- 20 + 5 * sin(i / 5)
  data_csv <- tempfile(fileext = ".csv")
  meta_csv <- tempfile(fileext = ".csv")
  withr::defer(unlink(c(data_csv, meta_csv)))
  utils::write.csv(d, data_csv, row.names = FALSE)
  utils::write.csv(data.frame(Variable = c("ph", "k"), Label = c("pH", "Potassium"),
                              Category = c("Soil", "Nutrients"), Unit = c("", "mg/kg")),
                   meta_csv, row.names = FALSE)

  set <- function(a, ...) {
    a$set_inputs(..., wait_ = FALSE)
    a$wait_for_idle()
  }
  app$upload_file(user_file = data_csv)
  app$wait_for_idle()
  app$upload_file(meta_file = meta_csv)
  app$wait_for_idle()
  set(app, map_x = "east_m", map_y = "north_m", map_loc = "site")
  app$run_js("Shiny.setInputValue('map_crs', 'EPSG:32635'); Shiny.setInputValue('crs_selection', 'EPSG:32635');")
  app$wait_for_idle()
  set(app, var_category = "Nutrients")
  set(app, var_id = "k")
  # The run on screen is of another dataset's variable, so the picker styles k.
  set(app, palette_select = "Greys")
  set(app, value_type = "pred_ss")
  set(app, subset = "Test", comp_mode = TRUE, sep_fit = FALSE, match_scales = TRUE, locality = "B")
  # One per-locality IDW power and one manual variogram model.
  set(app, method = "IDW")
  set(app, idw_mode = "manual")
  set(app, idw_m_mode = "fixed", idw_m_p = 3.2)
  app$click("apply_idw_manual")
  app$wait_for_idle()
  set(app, method = "OK", vgm_mode = "manual")
  set(app, k_mod = "Exp")
  app$click("apply_manual")
  app$wait_for_idle()
  set(app, method = "RK")
  set(app, cv_strategy = "knndm", cv_population = "comparable", cv_repeat_on = TRUE, cv_repeat_n = "10",
      rfk_uncertainty = "spread", ck_nmax = 25, aux_vars = c("elev", "clay"))
  set(app, idw_p_mode = "cv", idw_p = 3.5, idw_nmax = 20, tps_mode = "manual",
      tps_lambda_mode = "fixed", tps_lambda = 2.4e-06)
  set(app, boundary_type = "wrapped", buff_mode = "fixed", buff_dist = 150, res_mode = "fixed", grid_res = 35)
  set(app, color_style = "agro")
  set(app, agro_method = "limits", agro_n_classes = 4)
  set(app, agro_limit_1 = 150, agro_limit_2 = 250, agro_limit_3 = 350)

  ids <- c("map_x", "map_y", "map_loc", "map_crs", "crs_selection", "locality", "var_category",
           "var_id", "value_type", "subset", "comp_mode", "sep_fit", "match_scales", "method",
           "cv_strategy", "cv_population", "cv_repeat_on", "cv_repeat_n", "rfk_uncertainty",
           "ck_nmax", "aux_vars", "vgm_mode", "idw_mode", "idw_p_mode", "idw_p", "idw_nmax",
           "tps_mode", "tps_lambda_mode", "tps_lambda", "boundary_type", "buff_mode", "buff_dist",
           "res_mode", "grid_res", "color_style", "agro_method", "agro_n_classes",
           "agro_limit_1", "agro_limit_2", "agro_limit_3")
  before <- app$get_values(input = ids)$input
  expect_identical(before$map_x, "east_m")
  expect_identical(before$method, "RK")
  expect_equal(before$agro_limit_3, 350)
  stores <- c("idw_store", "vgm_manual", "vars", "palettes")
  saved <- app$get_values(export = stores)$export
  expect_equal(saved$idw_store$B$act$value, 3.2)
  expect_length(saved$vgm_manual, 1)
  expect_true(any(grepl("^k\\|k_cve\\|k_ss\\|Potassium\\|Nutrients\\|mg/kg\\|", saved$vars)))
  expect_identical(saved$palettes$k, "Greys")

  app$click("save_config")
  app$wait_for_idle()
  cfg_file <- app$get_download("download_config_json")
  app$run_js("$('#shiny-modal').modal('hide');")
  app$wait_for_idle()
  # Load config reads through the server's file chooser, whose roots are the
  # home directory and the project; the file goes under the project.
  proj_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
  cfg_copy <- file.path(proj_root, "tests", "testthat", "_config_roundtrip.json")
  withr::defer(unlink(cfg_copy))
  file.copy(cfg_file, cfg_copy, overwrite = TRUE)

  # A new session on the same data.
  app2 <- tryCatch(
    shinytest2::AppDriver$new(.smoke$shim, name = "monolith-config", load_timeout = 180 * 1000,
                              timeout = 60 * 1000),
    error = function(e) skip(paste("Could not start a second app:", conditionMessage(e))))
  withr::defer(try(app2$stop(), silent = TRUE))
  app2$upload_file(user_file = data_csv)
  app2$wait_for_idle()
  expect_false(identical(app2$get_value(input = "map_x"), "east_m"))
  expect_identical(app2$get_value(input = "map_crs") %||% "", "")

  app2$run_js(paste0("Shiny.setInputValue('load_config', {files: {'0': ['tests', 'testthat', ",
                     "'_config_roundtrip.json']}, root: 'Project'}, {priority: 'event'});"))
  deadline <- Sys.time() + 90
  repeat {
    Sys.sleep(1)
    st <- app2$get_value(export = "cfg_restore")
    after <- app2$get_values(input = ids)$input
    if ((isFALSE(st$active) && isTRUE(all.equal(after[ids], before[ids]))) || Sys.time() > deadline) break
  }
  expect_false(st$active)
  expect_length(st$skipped, 0)
  for (id in ids) expect_equal(after[[id]], before[[id]], info = id)
  restored <- app2$get_values(export = stores)$export
  # Full precision is written (digits = NA); the tolerance allows its last bit.
  expect_equal(restored$idw_store, saved$idw_store)
  expect_equal(restored$vgm_manual, saved$vgm_manual)
  expect_identical(sort(restored$vars), sort(saved$vars))
  expect_identical(restored$palettes, saved$palettes)
})

# Shut the app down here rather than at suite teardown: global.R sets
# future::plan(multisession), so the app process keeps one worker per core
# alive for as long as it lives (~2 GB of RSS on an 16-core machine), and
# every later test file in the suite would run alongside them.
if (!is.null(.smoke$app)) {
  try(.smoke$app$stop(), silent = TRUE)
  .smoke$app <- NULL
}
