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

  setwd(old_wd)
  on.exit()  # clear the on.exit handlers now that we've restored state

  .monolith_sourced <- TRUE
}

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
