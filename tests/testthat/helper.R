# helper.R — sourced automatically by testthat before each test file.
#
# This file:
#   1. Sources all application R files so every function is available.
#   2. Provides shared factory functions for synthetic test fixtures.
#   3. Sets up test-local options (timezone, seed policy, etc.).
#
# Individual test files should NOT re-source the app; helper.R handles it
# once per session so repeated `source()` calls are avoided.

# ── Source application code (idempotent guard) ──────────────────────────────
if (!exists(".monolith_sourced") || !isTRUE(.monolith_sourced)) {
  proj_root <- normalizePath(
    file.path(testthat::test_path(), "..", ".."),
    winslash = "/"
  )

  # global.R calls addResourcePath("assets", ...) which uses getwd().
  # It also calls source() for helpers using relative paths.  Both require
  # the working directory to be the project root.
  old_wd <- getwd()
  setwd(proj_root)
  on.exit(setwd(old_wd), add = TRUE)

  # Helpers load before setup.R, so request sequential startup for both app
  # sources. Restore the option afterward so normal Shiny startup is unchanged.
  withr::with_options(list(monolith_test_sequential = TRUE), {
    suppressPackageStartupMessages({
      suppressMessages({
        source(file.path(proj_root, "global.R"))
      })
    })

    # monolith.R defines validate_crs, estimate_run_duration, and the Shiny
    # app.  Source it with shinyApp temporarily no-opped so it doesn't launch.
    if (requireNamespace("shiny", quietly = TRUE)) {
      .real_shinyApp <- shiny::shinyApp
      utils::assignInNamespace("shinyApp", function(ui, server, ...) {}, "shiny")
      on.exit(utils::assignInNamespace("shinyApp", .real_shinyApp, "shiny"),
              add = TRUE)
    }
    suppressMessages({
      source(file.path(proj_root, "monolith.R"))
    })
  })

  setwd(old_wd)
  on.exit()  # clear the on.exit handlers now that we've restored state

  .monolith_sourced <- TRUE
}

# ── Third-party partial-match notices ──────────────────────────────────────
#
# setup.R sets warnPartialMatchArgs = TRUE so that a partial argument match in
# Monolith's own code is an alarm. Two dependencies trip it from inside their
# own source and cannot be fixed here: randomForest calls seq(along = ...) and
# mgcv hands contrasts = to model.matrix. Left alone they are the large
# majority of the suite's warning output, which buries a genuine new one.
#
# Muffle those two notices BY MESSAGE at the call sites that raise them, rather
# than wrapping the call in suppressWarnings(), so every other warning the
# expression raises still reaches the reporter - a partial match in Monolith's
# own code included.
without_partial_match_notices <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl("'along' to 'along.with'|'contrasts' to 'contrasts.arg'", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

# The common cross-validation schema every kriging engine returns
# (run_kriging_folds). IDW and TPS keep their own engines' column sets.
KRIGING_CV_SCHEMA <- c("row_id", "fold", "observed", "var1.pred", "var1.var",
                       "residual", "geometry")

# ── Synthetic data factories (no external file dependencies) ────────────────

#' Create a small sf POINT dataframe for spatial tests.
#'
#' @param n Number of points (min 3).
#' @param target_mean Mean of the target variable `v`.
#' @param seed RNG seed for reproducibility.
#' @return An sf object with columns `x`, `y`, `v`, `pv`, `aux1`, `aux2`.
make_test_points <- function(n = 20, target_mean = 50, seed = 42) {
  set.seed(seed)
  coords <- data.frame(
    x = runif(n, 450000, 451000),
    y = runif(n, 5800000, 5801000)
  )
  pts <- cbind(coords, data.frame(
    v    = rnorm(n, mean = target_mean, sd = 10),
    pv   = rnorm(n, mean = target_mean, sd = 10),
    aux1 = runif(n, 0, 100),
    aux2 = runif(n, 0, 50)
  ))
  sf::st_as_sf(pts, coords = c("x", "y"), crs = 32633)
}

#' Create a spatially-structured sf POINT dataframe for classification tests:
#' a 3-class target (`soil`) driven by a smooth gradient, two numeric covariates
#' (`elev`, `slope`) and one categorical covariate (`parent`).
#'
#' @param n Number of points.
#' @param seed RNG seed.
#' @return sf object (EPSG:32633) with columns soil, elev, slope, parent.
make_classif_points <- function(n = 60, seed = 42) {
  set.seed(seed)
  x <- runif(n, 450000, 452000)
  y <- runif(n, 5800000, 5802000)
  score <- (x - 450000) / 2000 + (y - 5800000) / 2000 + rnorm(n, 0, 0.3)
  cls <- cut(score, breaks = stats::quantile(score, c(0, .34, .67, 1)),
             labels = c("Low", "Med", "High"), include.lowest = TRUE)
  df <- data.frame(
    x = x, y = y,
    soil   = factor(cls),
    elev   = score * 10 + rnorm(n, 0, 2),
    slope  = runif(n, 0, 15),
    parent = factor(sample(c("Granite", "Shale"), n, replace = TRUE))
  )
  sf::st_as_sf(df, coords = c("x", "y"), crs = 32633)
}

#' Plain data.frame (projected coords as columns, EPSG:32633) with TWO spatially
#' separated localities ("A" around x = 450500, "B" around x = 458500, ~7 km
#' apart) for classification scope tests. Coordinates are already metric so
#' tests can pass 32633 as both source and projected CRS.
#'
#' @return data.frame with columns x, y, loc, soil, elev, slope.
make_classif_scope_df <- function(nA = 40, nB = 30, seed = 7) {
  set.seed(seed)
  n <- nA + nB
  x <- c(runif(nA, 450000, 451000), runif(nB, 458000, 459000))
  y <- runif(n, 5800000, 5801000)
  score <- (y - 5800000) / 1000 + rnorm(n, 0, 0.3)
  data.frame(
    x = x, y = y,
    loc = c(rep("A", nA), rep("B", nB)),
    soil = factor(cut(score, breaks = stats::quantile(score, c(0, .5, 1)),
                      labels = c("Low", "High"), include.lowest = TRUE)),
    elev = score * 10 + rnorm(n, 0, 2),
    slope = runif(n, 0, 15)
  )
}

#' Create a small regular prediction grid.
#'
#' @param pts_sf sf point object used to derive the bounding box.
#' @param res Cell resolution in metres.
#' @return An sf POINT grid with columns `x`, `y`.
make_test_grid_safe <- function(pts_sf, res = 50) {
  bbox <- sf::st_bbox(pts_sf)
  r <- terra::rast(terra::ext(bbox), resolution = res, crs = sf::st_crs(pts_sf)$wkt)
  grid_pts <- terra::as.points(r, values = FALSE)
  grid_sf  <- sf::st_as_sf(grid_pts)
  coords   <- sf::st_coordinates(grid_sf)
  grid_sf$x <- coords[, 1]
  grid_sf$y <- coords[, 2]
  grid_sf
}

#' Create a small variogram-like data.frame for testing variogram helpers.
make_mock_vgm <- function(model = "Sph", psill = 0.5, range = 200, nugget = 0.1) {
  gstat::vgm(psill = psill, model = model, range = range, nugget = nugget)
}

#' Create a numeric data.frame suitable for PCA / correlation / multicollinearity
#' tests.
#'
#' @param n Number of rows.
#' @param seed RNG seed.
#' @return data.frame with columns `a`, `b`, `c`, `d`, `e`.
make_test_df <- function(n = 50, seed = 123) {
  set.seed(seed)
  data.frame(
    a = rnorm(n, 10, 3),
    b = rnorm(n, 20, 5),
    c = rnorm(n, 15, 2),
    d = rnorm(n, 30, 7),
    e = rnorm(n, 8,  1),
    cat1 = factor(sample(c("Low", "Med", "High"), n, replace = TRUE)),
    cat2 = factor(sample(c("A", "B"), n, replace = TRUE))
  )
}

#' Create a highly collinear data.frame for multicollinearity tests.
make_collinear_df <- function(n = 50, seed = 456) {
  set.seed(seed)
  x <- rnorm(n, 10, 2)
  data.frame(
    v1 = x,
    v2 = x + rnorm(n, 0, 0.01),      # near-perfect correlation with v1
    v3 = rnorm(n, 20, 5),             # independent
    v4 = rnorm(n, 15, 3)              # independent
  )
}

#' Create a point table with two SPATIALLY co-structured variables plus an
#' unstructured one, for the spatial cross-correlogram tests. Coordinates are
#' plain projected metres (columns `x`/`y`); `a` and `b` share a smooth field, so
#' their cross-correlation must decay with lag distance, while `c` is white noise.
make_xcorr_df <- function(n = 200, seed = 7) {
  set.seed(seed)
  x <- runif(n, 0, 1000)
  y <- runif(n, 0, 1000)
  field <- sin(x / 250) + cos(y / 250)
  data.frame(
    x = x, y = y,
    a = field + rnorm(n, 0, 0.2),
    b = field + rnorm(n, 0, 0.2),
    c = rnorm(n)
  )
}

#' Create a known-answer pair for Lin's CCC.
make_ccc_known <- function() {
  list(
    observed  = c(10, 20, 30, 40, 50),
    predicted = c(12, 19, 31, 38, 52),
    # CCC computed externally with DescTools::CCC(obs, pre)$rho.c$est
    # (re-derived 2026-09-01 to full precision: the previous 0.9937 was wrong in
    # the 4th decimal and the 0.01 tolerance it was asserted under could not
    # tell Lin's population-moment definition from the sample-moment variant.)
    expected  = 0.9929789368
  )
}

#' Create a known-answer pair for the WHOLE error-metric dictionary.
#' The pair is make_ccc_known()'s, so one fixture anchors every statistic
#' perform_cv() reports. Every value below was derived from the definition
#' (2026-09-03), not read off this implementation, and RMSE/RPD/RPIQ were
#' additionally cross-checked against yardstick, which agrees on those three
#' conventions:
#'   RMSE      = sqrt(mean(r^2))                    r = observed - predicted
#'   MAE       = mean(|r|)          ME  = mean(r)   (perform_cv's direction)
#'   R2(corr)  = cor(observed, predicted)^2
#'   NSE       = 1 - SSE/SST, SST about mean(observed)
#'   NRMSE     = RMSE / |mean(observed)| * 100
#'   RPD       = sd(observed) / RMSE     (sample sd; Chang et al. 2001)
#'   RPIQ      = IQR(observed) / RMSE
#'   sMAPE     = mean(2|r| / (|observed| + |predicted|)) * 100
#' A perfect pair alone (the old fixture) cannot separate sqrt(mean(r^2)) from
#' sqrt(sum(r^2)/(n-1)), nor NSE from SSE/SST - both are trivially right at zero
#' residual - so it is kept only as the degenerate companion. If a formula ever
#' changes, re-derive these externally; never adjust them to make a test pass.
make_metrics_known <- function() {
  list(
    observed  = c(10, 20, 30, 40, 50),
    predicted = c(12, 19, 31, 38, 52),
    rmse      = 1.6733200531,
    mae       = 1.6,
    me        = -0.4,
    r2        = 0.9868103101,
    nse       = 0.986,
    nrmse     = 5.5777335102,
    rpd       = 9.4491118252,
    rpiq      = 11.9522860933,
    smape     = 7.1276971181,
    # Perfect-prediction companion, for the degenerate branches.
    perfect   = list(observed = c(1, 2, 4, 5), predicted = c(1, 2, 4, 5),
                     nse = 1.0, rmse = 0.0)
  )
}

#' Create an empirical variogram + data vector whose candidate fits reliably
#' trigger gstat non-convergence/singular screening warnings under
#' robust_vgm_fit's 4-model x 4-range grid (seeds verified 2026-07-04:
#' seed 1 = all 16 candidates flawed; seed 12 = clean Gau winner).
make_hostile_vgm_input <- function(n = 9, seed = 1) {
  set.seed(seed)
  df <- data.frame(
    x = runif(n, 450000, 451000),
    y = runif(n, 5800000, 5801000),
    v = rnorm(n, 50, 10)
  )
  pts <- sf::st_as_sf(df, coords = c("x", "y"), crs = 32633)
  lags <- calc_scientific_lags(pts)
  v_emp <- gstat::variogram(v ~ 1, pts, width = lags$width, cutoff = lags$cutoff)
  list(v_emp = v_emp, v_data = df$v)
}

#' A field carrying a strong linear trend: its empirical variogram keeps rising
#' to the cutoff, so every converged candidate reaches its sill far beyond the
#' observed lag window and every in-window candidate is non-converged. Seed 3
#' verified 2026-09-19: 16 candidates, 0 converged and range-resolved, 4
#' converged and range-unresolved, 12 non-converged - and NONE of the
#' non-converged ones is range-resolved either, so the old eligibility rule
#' found nothing eligible at all and returned the heuristic.
make_trend_vgm_input <- function(n = 60, seed = 3) {
  set.seed(seed)
  df <- data.frame(x = runif(n, 0, 1000), y = runif(n, 0, 1000))
  df$v <- 0.01 * df$x + 0.005 * df$y + rnorm(n, 0, 0.3)
  pts <- sf::st_as_sf(df, coords = c("x", "y"), crs = 32633)
  lags <- calc_scientific_lags(pts)
  v_emp <- gstat::variogram(v ~ 1, pts, width = lags$width, cutoff = lags$cutoff)
  list(v_emp = v_emp, v_data = df$v, pts = pts)
}

#' A periodic structure plus a weak trend, on which the screen yields BOTH a
#' converged candidate whose range the lags resolve and a converged one whose
#' range they do not - and the unresolved one fits better. Seed 4 verified
#' 2026-09-19: best resolved Gau SSErr 1.064e-4, best unresolved Mat 6.53e-5
#' (ratio 1.63). A hard resolved-first tier picks the poorer Gau here.
make_mixed_vgm_input <- function(n = 60, seed = 4) {
  set.seed(seed)
  df <- data.frame(x = runif(n, 0, 1000), y = runif(n, 0, 1000))
  df$v <- sin(df$x / 150) + 0.003 * df$y + rnorm(n, 0, 0.3)
  pts <- sf::st_as_sf(df, coords = c("x", "y"), crs = 32633)
  lags <- calc_scientific_lags(pts)
  v_emp <- gstat::variogram(v ~ 1, pts, width = lags$width, cutoff = lags$cutoff)
  list(v_emp = v_emp, v_data = df$v, pts = pts)
}

#' Every candidate robust_vgm_fit's screen produces for `h`, classified the way
#' the screen classifies it, re-derived independently so a selection test does
#' not read its expectation off the function under test.
screen_vgm_candidates <- function(h) {
  max_dist <- max(h$v_emp$dist, na.rm = TRUE)
  init_sill <- var(h$v_data)
  n0 <- min(h$v_emp$gamma)
  if (n0 == 0) n0 <- max(init_sill * 1e-6, 1e-6)
  if (n0 > init_sill) n0 <- init_sill * 0.9
  ps0 <- max(init_sill - n0, init_sill * 0.1)
  out <- list()
  for (m in c("Sph", "Exp", "Gau", "Mat")) {
    for (r in max_dist / c(10, 5, 4, 2)) {
      flawed <- FALSE
      f <- tryCatch(withCallingHandlers(
        gstat::fit.variogram(h$v_emp, gstat::vgm(ps0, m, r, n0, kappa = if (m == "Mat") 1.5 else 0.5)),
        warning = function(w) {
          if (grepl("No convergence after|singular model|singular covariance", conditionMessage(w))) {
            flawed <<- TRUE; invokeRestart("muffleWarning")
          }
        }), error = function(e) NULL)
      if (is.null(f)) next
      sse <- attr(f, "SSErr")
      prange <- f$range[2] * .vgm_practical_range_factor(f$model[2], f$kappa[2])
      out[[length(out) + 1]] <- list(
        fit = f, sse = sse,
        flawed = flawed || isTRUE(attr(f, "singular")),
        valid = length(sse) == 1L && is.finite(sse) &&
                is.finite(f$psill[2]) && f$psill[2] > 0 &&
                is.finite(prange) && prange > 0 &&
                is.finite(f$psill[1]) && f$psill[1] >= 0 &&
                !isTRUE(vgm_smooth_nugget_share(f) <= 1e-8),
        resolved = prange > (max_dist / 100) && prange < max_dist * 2)
    }
  }
  out
}

# ── Golden fixture (real survey data, frozen) ───────────────────────────────
#
# tests/testthat/fixtures/ holds a frozen extract of the repository's own sample
# data (see GOLDEN_MANIFEST.md for provenance, licence, the column roles and the
# properties it is relied on for). Use it wherever a test needs REAL spatial
# structure, real multicollinearity or real class imbalance; the synthetic
# make_*() factories above stay the right tool for degenerate and edge inputs,
# which real data does not contain.
#
# Nothing here uses RNG. The reduced scopes are every-k-th-row systematic
# samples of a coordinate-sorted table, so they are stable across R versions
# and spatially spread by construction.
#
# A different golden set can be substituted without touching a single test:
# build one with fixtures/make_golden.R, point `monolith_golden_dir` at it, and
# regenerate its baselines with fixtures/make_baselines.R. Only the handful of
# tests that pin recorded values need those baselines; every other test
# recomputes its reference from whatever data it is given and is therefore
# fixture-agnostic by construction.

.golden_cache <- new.env(parent = emptyenv())

#' Directory the golden fixture is read from.
#'
#' Defaults to the shipped `fixtures/`. Override with
#' `options(monolith_golden_dir = "path")` or the `MONOLITH_GOLDEN_DIR`
#' environment variable to run the suite against your own golden set.
golden_dir <- function() {
  d <- getOption("monolith_golden_dir", Sys.getenv("MONOLITH_GOLDEN_DIR", ""))
  if (nzchar(d)) return(normalizePath(d, winslash = "/", mustWork = TRUE))
  testthat::test_path("fixtures")
}

.golden_read <- function(file, required = TRUE) {
  dir <- golden_dir()
  if (!identical(.golden_cache$dir, dir)) {
    rm(list = ls(.golden_cache), envir = .golden_cache)
    .golden_cache$dir <- dir
  }
  if (!is.null(.golden_cache[[file]])) return(.golden_cache[[file]])
  path <- file.path(dir, file)
  if (!file.exists(path)) {
    if (required) stop("golden fixture file not found: ", path)
    return(NULL)
  }
  .golden_cache[[file]] <- readRDS(path)
  .golden_cache[[file]]
}

#' The fixture's own description: CRS, column roles, scope definitions, source
#' provenance. Written by make_golden.R.
golden_meta <- function() .golden_read("golden_meta.rds")

#' The golden soil table.
#'
#' @param scope "full" (every row), "core" (the three largest localities,
#'   thinned) or "tiny" (one compact locality, thinned). The reduced scopes are
#'   defined by the fixture itself, not by this file.
#' @return A plain data.frame with the fixture's canonical columns.
golden_soil <- function(scope = c("core", "full", "tiny")) {
  scope <- match.arg(scope)
  full <- .golden_read("golden_soil.rds")
  if (scope == "full") return(full)

  spec <- golden_meta()$scopes[[scope]]
  if (is.null(spec)) stop("the golden fixture defines no '", scope, "' scope")
  out <- do.call(rbind, lapply(names(spec), function(l) {
    d <- full[full$locality == l, , drop = FALSE]
    d[seq(1L, nrow(d), by = spec[[l]]), , drop = FALSE]
  }))
  rownames(out) <- NULL
  out
}

#' The golden soil table as sf POINTs.
#'
#' Column names are the fixture's canonical ones (`ph`, `som`, `v82`, ...), so
#' tests pass them straight to the engines as `target_var` / `aux_vars`.
#'
#' @param scope Passed to golden_soil().
#' @param localities Optional character vector to filter to.
#' @param crs Target CRS; the fixture's own is the default.
golden_sf <- function(scope = c("core", "full", "tiny"),
                      localities = NULL, crs = NULL) {
  df <- golden_soil(scope)
  if (!is.null(localities)) df <- df[df$locality %in% localities, , drop = FALSE]
  native <- golden_meta()$crs
  pts <- sf::st_as_sf(df, coords = c("x", "y"), crs = native, remove = FALSE)
  if (!is.null(crs) && !identical(crs, native)) pts <- sf::st_transform(pts, crs)
  pts
}

#' The frozen variable dictionary (id, label, category), or NULL if the fixture
#' ships none.
golden_varlist <- function() .golden_read("golden_varlist.rds", required = FALSE)

#' A value recorded for THIS golden set by make_baselines.R.
#'
#' Returns NULL when no baseline has been recorded (a freshly built fixture, or
#' the deliberate bypass make_baselines.R sets while it runs the suite to decide
#' whether recording is safe). Tests that pin a recorded value must skip on
#' NULL rather than fail: a golden set with no baselines is an unfinished
#' fixture, not a broken code base.
golden_baseline <- function(key) {
  if (isTRUE(getOption("monolith_golden_baselines_bypass", FALSE))) return(NULL)
  b <- .golden_read("golden_baselines.rds", required = FALSE)
  if (is.null(b)) return(NULL)
  b[[key]]
}

# ── The environment a baseline was recorded in ───────────────────────────────
#
# A recorded baseline means "same code + same data + same packages -> same
# numbers". The data is frozen in this fixture and the code is in git; the
# package versions were the one leg nothing recorded, which made a moved value
# indistinguishable from an upstream change and left the reader bisecting.
#
# These are the packages that can actually move one of the four recorded values:
# gstat (the empirical variogram and the kriging solve), sf and terra
# (projection, grid construction, rasterisation, the ellipsoidal areas),
# Ckmeans.1d.dp (the exact Jenks natural breaks). Moran diagnostics are not
# recorded. Extend the list only when a NEW baseline brings a new package in -
# an entry that cannot move a value would report drift that means nothing.
GOLDEN_BASELINE_PKGS <- c("gstat", "sf", "terra", "Ckmeans.1d.dp")

#' TRUE when this session is deliberately running on unpinned package versions.
#'
#' Set by the upstream-drift CI job (.github/workflows/upstream.yaml), which runs
#' the suite against the latest CRAN on purpose. The two environment assertions
#' below skip there: a version difference is that job's PREMISE, so asserting
#' against it would make the job permanently red and useless as an alarm. What it
#' still asserts is the thing it exists for - whether a recorded VALUE moved.
monolith_unpinned_run <- function() {
  isTRUE(as.logical(Sys.getenv("MONOLITH_ALLOW_LATEST", "false")))
}

#' This session's environment, in the shape make_baselines.R records.
#'
#' `sys_libs` is the GDAL/GEOS/PROJ that sf and terra are linked against. Like
#' `platform` it is carried for the message only, never compared: Windows
#' binaries bundle their own libraries while Linux links Ubuntu's, so the two
#' legs differ by construction.
golden_env_snapshot <- function(pkgs = GOLDEN_BASELINE_PKGS) {
  vs <- vapply(pkgs, function(p) {
    tryCatch(as.character(utils::packageVersion(p)),
             error = function(e) NA_character_)
  }, character(1))
  libs <- tryCatch({
    s <- sf::sf_extSoftVersion()
    t <- unlist(terra::gdal(lib = "all"))
    c(sf = sprintf("GDAL %s, GEOS %s, PROJ %s", s[["GDAL"]], s[["GEOS"]], s[["PROJ"]]),
      terra = sprintf("GDAL %s, GEOS %s, PROJ %s", t[["gdal"]], t[["geos"]], t[["proj"]]))
  }, error = function(e) NULL)
  list(r_version = as.character(getRversion()),
       platform = R.version$platform,
       recorded_at = format(Sys.Date(), "%Y-%m-%d"),
       packages = vs,
       sys_libs = libs)
}

#' The environment `golden_baselines.rds` was recorded in, or NULL when the
#' fixture ships no baselines (or carries none recorded before this was added).
golden_provenance <- function() {
  if (isTRUE(getOption("monolith_golden_baselines_bypass", FALSE))) return(NULL)
  b <- .golden_read("golden_baselines.rds", required = FALSE)
  if (is.null(b)) return(NULL)
  attr(b, "provenance", exact = TRUE)
}

#' Every difference between the recording environment and this one, as
#' "pkg: recorded X, here Y" lines; `character(0)` when they agree, NULL when
#' nothing was recorded.
#'
#' PLATFORM IS DELIBERATELY NOT COMPARED. Baselines are recorded on Windows and
#' CI runs Linux, so comparing it would be permanently red there; it is carried
#' for the message only - and the platform difference is real enough to matter
#' (it moved `ok_surface_digest` once), which is exactly why the reader needs to
#' see it rather than have the suite shout about it on every run.
#'
#' Versions are compared as `package_version()`, never as strings: gstat reports
#' "2.1-5" where `packageVersion()` normalises it to 2.1.5, and a string test
#' would call every dashed version a drift.
golden_provenance_drift <- function(prov = golden_provenance()) {
  if (is.null(prov)) return(NULL)
  here <- golden_env_snapshot(names(prov$packages))
  out <- character(0)
  if (!identical(prov$r_version, here$r_version)) {
    out <- c(out, sprintf("R: recorded %s, here %s", prov$r_version, here$r_version))
  }
  for (p in names(prov$packages)) {
    a <- prov$packages[[p]]
    b <- here$packages[[p]]
    same <- !is.na(a) && !is.na(b) && isTRUE(package_version(a) == package_version(b))
    if (!same) out <- c(out, sprintf("%s: recorded %s, here %s", p, a, b))
  }
  out
}

#' The provenance note a baseline assertion passes as `info =`, so a moved value
#' arrives with the versions it was recorded under instead of sending the reader
#' to bisect. NULL when nothing was recorded, in which case the expectation
#' simply carries no note.
golden_baseline_info <- function() {
  prov <- golden_provenance()
  if (is.null(prov)) return(NULL)
  base <- sprintf("baseline recorded %s on %s under R %s (%s)",
                  prov$recorded_at %||% "?", prov$platform %||% "?", prov$r_version,
                  paste(names(prov$packages), prov$packages, collapse = ", "))
  if (length(prov$sys_libs)) {
    here_libs <- golden_env_snapshot(character(0))$sys_libs
    base <- paste0(base, sprintf("; libraries recorded [%s], here [%s]",
                                 paste(names(prov$sys_libs), prov$sys_libs, collapse = "; "),
                                 paste(names(here_libs), here_libs, collapse = "; ")))
  }
  drift <- golden_provenance_drift(prov)
  if (length(drift)) {
    paste0(base, "; THIS SESSION DIFFERS - ", paste(drift, collapse = "; "))
  } else {
    paste0(base, "; this session matches it")
  }
}

#' Every declared package whose installed version differs from `renv.lock`, as
#' "pkg: lock X, session Y" lines; `character(0)` when they agree, NULL when the
#' lockfile cannot be read.
#'
#' Read-only: nothing is installed, activated or written. The lockfile stays the
#' authority and this only reports when the session has stopped matching it,
#' which is the cheap half of what an activated renv would guarantee. Its caller
#' reports a difference as a SKIP, not a failure - a deliberate local upgrade is
#' not a defect, and `golden_provenance_drift()` is what speaks up when a
#' version actually moves a recorded number.
renv_lock_drift <- function(pkgs = NULL) {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = FALSE)
  lf <- file.path(root, "renv.lock")
  if (!file.exists(lf)) return(NULL)
  lock <- tryCatch(jsonlite::fromJSON(lf, simplifyVector = FALSE)$Packages,
                   error = function(e) NULL)
  if (is.null(lock)) return(NULL)
  if (is.null(pkgs)) {
    pkgs <- if (exists("required_packages", inherits = TRUE)) required_packages else names(lock)
  }
  out <- character(0)
  # A declared package the lockfile has never heard of is the same class of
  # problem as a version difference, and the one the intersect() below would
  # otherwise hide: renv.lock has not been regenerated since it was added, so
  # `global.R` and the lockfile no longer describe the same dependency set.
  absent <- setdiff(pkgs, names(lock))
  if (length(absent)) {
    out <- c(out, sprintf("%s: declared in global.R, absent from renv.lock", absent))
  }
  for (p in intersect(pkgs, names(lock))) {
    lv <- lock[[p]]$Version
    iv <- tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
    ok <- !is.null(lv) && !is.na(iv) && isTRUE(package_version(lv) == package_version(iv))
    if (!ok) out <- c(out, sprintf("%s: lock %s, session %s", p, lv %||% "?", iv))
  }
  out
}

#' The variogram the surface digest is pinned to.
#'
#' The digest exists to lock the DRIVER - grid construction, the kriging solve,
#' the CV loop, raster assembly - so it must not also depend on which of
#' robust_vgm_fit()'s 16 screened candidates happens to win. On the `tiny` scope
#' all 16 fail to converge and exactly one clears the sanity window, so the
#' winner is an artefact of the platform's floating-point path: Windows lands on
#' Sph(range 312.9), Linux on Sph(range 274.7), and the whole digest moves with
#' it. robust_vgm_fit() is covered on its own in test-robust-vgm-fit.R; pinning
#' here removes that ambiguity without losing the coverage.
#'
#' The pinned model is read off the golden data, not invented:
#'   - total sill 0.0617 = var(ph) on the `tiny` scope, the usual sill anchor
#'     for a second-order stationary field;
#'   - nugget 0.035 = the lowest empirical bin (min gamma 0.0351 at 746 m),
#'     i.e. gamma extrapolated back to the origin;
#'   - partial sill 0.0267 = the remainder;
#'   - range 1200 m: above the 197 m mean nearest-neighbour spacing, so the
#'     surface interpolates instead of reverting to the mean; inside the
#'     trustworthy half of the 3233 m cutoff; and consistent with the 1335 m
#'     the better-sampled `core` scope converges to cleanly.
#'
#' Nugget:sill is 57%, moderate spatial dependence on the Cambardella et al.
#' (1994) scale, which is what this variable actually shows. It is a fixed test
#' input, not a claim that this is the best attainable fit.
golden_pin_vgm <- function() {
  gstat::vgm(psill = 0.0267, model = "Sph", range = 1200, nugget = 0.035)
}

#' The boundary the surface digest is pinned to.
#'
#' Same problem as the variogram, one step further down the driver. The
#' point-derived boundary (`concaveman` hull, then a 300 m buffer) puts cell
#' centres 1.2 m and 2.0 m from its edge on a 300 m grid, so a sub-metre
#' difference in either library decides whether those cells are inside. Measured
#' locally: moving the buffer 0.5 m changes the mask by one cell and moves
#' pred_min 1.6e-2, pred_max 2.0e-3, pred_sd 3.5e-4 - the same five entries, in
#' the same direction and magnitude, that Linux CI reported while `cells`,
#' `vgm_*` and `cv_*` stayed bit-identical.
#'
#' So the digest supplies its own boundary: the union of the `res`-metre lattice
#' cells whose centre lies within `reach` of a sample. Every edge falls on a
#' cell boundary, which puts EVERY candidate cell centre exactly res/2 = 150 m
#' from the nearest edge - there is no near-tie left for a GEOS or concaveman
#' build to decide differently. It still clips (the study area follows the
#' sampling pattern rather than covering the bounding box), so the mask stays
#' under test; what it no longer covers is hull construction, which
#' `run_regional_interpolation`'s own boundary tests exercise.
#'
#' Returned with a `Locality` column so the driver's shapefile branch matches it
#' by name, which is the same path a user-supplied boundary takes.
golden_pin_boundary <- function(pts, res = 300, reach = 600) {
  bb <- sf::st_bbox(pts)
  lo <- function(v) floor((v - res) / res) * res
  hi <- function(v) ceiling((v + res) / res) * res
  g <- terra::rast(terra::ext(lo(bb[["xmin"]]), hi(bb[["xmax"]]),
                              lo(bb[["ymin"]]), hi(bb[["ymax"]])),
                   resolution = res, crs = sf::st_crs(pts)$wkt)
  ctr <- terra::xyFromCell(g, seq_len(terra::ncell(g)))
  co <- sf::st_coordinates(pts)
  d2 <- outer(ctr[, 1], co[, 1], "-")^2 + outer(ctr[, 2], co[, 2], "-")^2
  terra::values(g) <- ifelse(apply(d2, 1, min) <= reach^2, 1L, NA_integer_)
  p <- sf::st_as_sf(terra::as.polygons(g, dissolve = TRUE))
  sf::st_sf(Locality = "golden", geometry = sf::st_union(sf::st_geometry(p)))
}

#' Summary digest of a full regional interpolation run.
#'
#' The end-to-end alarm: it says "this pipeline no longer produces the surface
#' it produced before", not "the surface is wrong". Its standing rests on the
#' component tests above, which establish that each engine computes what its
#' method defines; this only pins that the whole driver still assembles them the
#' same way. Ordinary Kriging on purpose - it is fully seed-sandboxed, whereas
#' RFK's forest is unseeded and would not reproduce.
#'
#' The variogram is pinned by golden_pin_vgm() rather than fitted, so the digest
#' measures the driver and not the screening tie-break; the vgm_* entries are
#' therefore constants that confirm the pin reached the engine.
#'
#' `b_type` / `b_dist` are inert while `boundary` is supplied: the driver takes
#' its shapefile branch and never reaches the hull switch. Pass `boundary = NULL`
#' to exercise the point-derived path instead.
#'
#' Runs sequentially, so it does NOT cover the future/PSOCK dispatch layer.
#'
#' @return A named numeric vector, ready to paste into a baseline.
run_surface_digest <- function(pts, target = "ph", method = "OK",
                               grid_res = 300, b_type = "wrapped",
                               b_dist = 300, crs = NULL,
                               pre_fit = golden_pin_vgm(),
                               boundary = golden_pin_boundary(pts)) {
  crs <- crs %||% golden_meta()$crs
  co <- sf::st_coordinates(pts)
  pts_data <- data.frame(x = co[, 1], y = co[, 2],
                         v = pts[[target]], pv = pts[[target]],
                         Locality = "golden")
  item <- list(l = "golden", pts_data = pts_data,
               m_params = list(idw_p_act = 2, idw_p_pre = 2, idw_nmax = 12,
                               tps_lambda_act = -1, tps_lambda_pre = -1,
                               pre_fit_act = pre_fit, pre_fit_pre = NULL,
                               cv_strategy = "auto", rfk_uncertainty = "jackknife"))
  res <- suppressWarnings(run_regional_interpolation(
    item, method, crs, character(0), boundary, b_type, "fixed", b_dist,
    "fixed", grid_res, paste0("EPSG:", crs), FALSE, "actual"))

  r <- terra::unwrap(res$r_a)
  pred <- terra::values(r[["var1.pred"]])
  vv <- if ("var1.var" %in% names(r)) terra::values(r[["var1.var"]]) else NA_real_
  fit <- res$v_fit_act
  cv <- res$cv_act

  c(cells      = sum(!is.na(pred)),
    pred_min   = min(pred, na.rm = TRUE),
    pred_mean  = mean(pred, na.rm = TRUE),
    pred_max   = max(pred, na.rm = TRUE),
    pred_sd    = stats::sd(pred, na.rm = TRUE),
    var_mean   = mean(vv, na.rm = TRUE),
    vgm_nugget = if (is.null(fit)) NA_real_ else fit$psill[1],
    vgm_sill   = if (is.null(fit)) NA_real_ else sum(fit$psill),
    vgm_range  = if (is.null(fit)) NA_real_ else fit$range[nrow(fit)],
    cv_rmse    = if (is.null(cv)) NA_real_ else cv$rmse,
    cv_r2      = if (is.null(cv)) NA_real_ else cv$r2)
}

# ── Known-answer confusion matrix ──────────────────────────────────────────

#' A 3x3 confusion matrix small enough to check by hand: rows = truth,
#' columns = prediction, n = 20, trace = 15 (accuracy 0.75). The classes are
#' deliberately unbalanced so macro and weighted-macro averages differ.
make_cm_known <- function() {
  matrix(c(5, 1, 0,
           2, 4, 1,
           0, 1, 6),
         nrow = 3, byrow = TRUE,
         dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
}

#' Expand a confusion matrix into the predictions data.frame the classification
#' metric helpers consume: the truth column (named `soil`) plus `.pred_class`,
#' both factors on the same levels. No probability columns, so probability
#' metrics are skipped and only the class metrics are computed.
make_cm_pred_df <- function(cm = make_cm_known(), target = "soil") {
  levs <- colnames(cm)
  idx <- which(cm > 0, arr.ind = TRUE)
  out <- do.call(rbind, lapply(seq_len(nrow(idx)), function(r) {
    i <- idx[r, 1]; j <- idx[r, 2]
    data.frame(truth = rep(levs[i], cm[i, j]),
               .pred_class = rep(levs[j], cm[i, j]),
               stringsAsFactors = FALSE)
  }))
  names(out)[1] <- target
  out[[target]] <- factor(out[[target]], levels = levs)
  out$.pred_class <- factor(out$.pred_class, levels = levs)
  out
}
