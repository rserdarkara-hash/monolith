# perform_kriging_loocv(): the RK/RFK fold loop (trend, covariate kriging and
# residual variogram refitted per fold), on small synthetic sets.

test_that("perform_kriging_loocv returns NULL for fewer than 3 points", {
  pts <- make_test_points(2)
  result <- perform_kriging_loocv(pts, "v", "aux1", model_type = "lm",
                                  folds = kriging_cv_folds(pts, "v", "aux1"))
  expect_null(result)
})

test_that("lm and rf kriging CV return a complete common schema", {
  pts <- make_test_points(12)
  for (engine in c("lm", "rf")) {
    cv <- suppressWarnings(perform_kriging_loocv(pts, "v", "aux1", model_type = engine, rf_ntree = 50,
                                                 folds = kriging_cv_folds(pts, "v", "aux1")))
    expect_s3_class(cv, "sf")
    expect_identical(names(cv), c("row_id", "fold", "observed", "var1.pred", "var1.var", "residual", "geometry"))
    expect_equal(nrow(cv), nrow(pts))
    expect_equal(cv$observed, pts$v)
    expect_true(all(is.finite(cv$var1.pred)))
    expect_true(all(is.na(cv$var1.var)))
  }
})

test_that("every RK and RFK fold reports the state of its residual variogram", {
  # The residual variogram is refitted per fold by the same auto-fit OK uses, so
  # a fold can land in a state the map's fit did not; the run log can only name
  # it if the fold reports it.
  pts <- make_test_points(12)
  for (engine in c("lm", "rf")) {
    cv <- suppressWarnings(perform_kriging_loocv(pts, "v", "aux1", model_type = engine, rf_ntree = 50,
                                                 folds = kriging_cv_folds(pts, "v", "aux1")))
    st <- .cv_fold_statuses(cv)
    expect_length(st, length(unique(cv$fold)))
    expect_true(all(st %in% VGM_FIT_STATUSES))
    tb <- .cv_fold_status_table(cv)
    expect_equal(sum(tb$n), length(st))
  }
})

test_that("an RK or RFK fold whose covariate kriging failed is named in the run log", {
  # Each fold kriges its held-out covariates from its training rows and falls
  # back to IDW where that fails (krige_covariates), as the map does for its
  # grid; the fold records it and the run log counts the folds per covariate.
  pts <- make_test_points(12)
  cv0 <- suppressWarnings(perform_kriging_loocv(pts, "v", "aux1", model_type = "lm",
                                                folds = kriging_cv_folds(pts, "v", "aux1")))
  expect_true(all(vapply(attr(cv0, "cv_fold_meta"), function(m) !length(m$cov_fallback), logical(1))))
  expect_false(grepl("came from IDW", safe_run_cv(init_interpolation_res(), cv0, "RK")$log_msg,
                     fixed = TRUE))

  # Covariate kriging that returns no prediction in every fold; the residual
  # kriging is left alone.
  withr::defer(if (exists("krige", envir = globalenv(), inherits = FALSE)) {
    rm("krige", envir = globalenv())
  })
  assign("krige", function(formula, ...) {
    r <- gstat::krige(formula, ...)
    if (identical(all.vars(formula)[1], ".mn_cov")) r$var1.pred[] <- NA_real_
    r
  }, envir = globalenv())
  for (engine in c("lm", "rf")) {
    cv <- suppressWarnings(perform_kriging_loocv(pts, "v", "aux1", model_type = engine, rf_ntree = 50,
                                                 folds = kriging_cv_folds(pts, "v", "aux1")))
    meta <- attr(cv, "cv_fold_meta")
    expect_length(meta, length(unique(cv$fold)))
    expect_true(all(vapply(meta, function(m) identical(m$cov_fallback, "aux1"), logical(1))))
    expect_true(all(is.finite(cv$var1.pred)))
    expect_match(safe_run_cv(init_interpolation_res(), cv, "RK")$log_msg,
                 sprintf("[RK CV] covariate kriging failed, so the held-out values came from IDW: aux1 in %d of %d folds.",
                         length(meta), length(meta)), fixed = TRUE)
  }
  rm("krige", envir = globalenv())
})

test_that("an RK or RFK fold whose covariate was kriged flat is named in the run log", {
  # A covariate whose fold variogram is a pure nugget keeps its flat kriged
  # surface (krige_covariates); the fold records it like a fallback, and the
  # run log counts the folds per covariate.
  pts <- make_test_points(12)
  original <- krige_covariates
  withr::defer(assign("krige_covariates", original, envir = globalenv()))
  assign("krige_covariates", function(...) {
    kc <- original(...)
    kc$flat <- "aux1"
    kc
  }, envir = globalenv())
  for (engine in c("lm", "rf")) {
    cv <- suppressWarnings(perform_kriging_loocv(pts, "v", "aux1", model_type = engine, rf_ntree = 50,
                                                 folds = kriging_cv_folds(pts, "v", "aux1")))
    meta <- attr(cv, "cv_fold_meta")
    expect_true(all(vapply(meta, function(m) identical(m$cov_flat, "aux1"), logical(1))))
    expect_match(safe_run_cv(init_interpolation_res(), cv, "RK")$log_msg,
                 sprintf(paste("[RK CV] covariate variogram was a pure nugget, so the held-out values were",
                               "the mean of the fold's covariate locations: aux1 in %d of %d folds."),
                         length(meta), length(meta)),
                 fixed = TRUE)
  }
  # Unmocked, aux1 is independent noise, so some folds' training rows fit it a
  # pure nugget and others do not, and the note counts those folds only.
  assign("krige_covariates", original, envir = globalenv())
  cv0 <- suppressWarnings(perform_kriging_loocv(pts, "v", "aux1", model_type = "lm",
                                                folds = kriging_cv_folds(pts, "v", "aux1")))
  meta0 <- attr(cv0, "cv_fold_meta")
  n_flat <- sum(vapply(meta0, function(m) identical(m$cov_flat, "aux1"), logical(1)))
  expect_gt(n_flat, 0L)                                                        # the premises
  expect_lt(n_flat, length(meta0))
  expect_match(safe_run_cv(init_interpolation_res(), cv0, "RK")$log_msg,
               sprintf("the mean of the fold's covariate locations: aux1 in %d of %d folds.", n_flat, length(meta0)),
               fixed = TRUE)
})

test_that("perform_kriging_loocv refuses a fold vector that does not match its rows", {
  pts <- make_test_points(12)
  expect_error(perform_kriging_loocv(pts, "v", "aux1", model_type = "lm", folds = seq_len(11)),
               "fold vector has 11 entries for 12 complete-case samples", fixed = TRUE)
})

test_that("perform_kriging_loocv refuses a rank-deficient trend instead of returning NA metrics", {
  # With fewer training rows than the lm has coefficients, the fold fit aliases
  # coefficients, predict() returns NA, the NAs propagate through the residual
  # kriging and the reported metrics degrade to NA with nothing saying why.
  pts <- make_test_points(8)
  for (i in 1:6) pts[[paste0("cv", i)]] <- rnorm(8)
  aux <- paste0("cv", 1:6)   # 6 covariates + intercept = 7 coefficients

  # LOOCV leaves 7 training rows for 7 coefficients: no residual df.
  expect_error(
    perform_kriging_loocv(pts, "v", aux, model_type = "lm", folds = kriging_cv_folds(pts, "v", aux, "loocv")),
    "regression coefficients")

  # randomForest has no rank requirement, so the guard must not fire for RFK.
  # (suppressWarnings: randomForest's own seq(along=) partial-match notice.)
  rf_res <- suppressWarnings(tryCatch(
    perform_kriging_loocv(pts, "v", aux, model_type = "rf", folds = kriging_cv_folds(pts, "v", aux, "loocv")),
    error = function(e) e))
  expect_false(inherits(rf_res, "error") &&
                 grepl("regression coefficients", conditionMessage(rf_res)))
})

test_that("a comfortably-specified lm passes the rank guard", {
  pts <- make_test_points(30)
  res <- tryCatch(
    perform_kriging_loocv(pts, "v", c("aux1", "aux2"), model_type = "lm",
                          folds = kriging_cv_folds(pts, "v", c("aux1", "aux2"), "loocv")),
    error = function(e) e)
  expect_false(inherits(res, "error") &&
                 grepl("regression coefficients", conditionMessage(res)))
})

test_that("RK and RFK holdout predictions do not use the held-out covariate", {
  set.seed(42)
  n <- 16L
  aux <- rnorm(n)
  pts <- sf::st_as_sf(data.frame(
    x = runif(n, 450000, 451000), y = runif(n, 5800000, 5801000),
    aux1 = aux, v = 5 * aux + rnorm(n, 0, 0.3)
  ), coords = c("x", "y"), crs = 32633)
  changed <- pts
  changed$aux1[1] <- changed$aux1[1] + 100

  for (type in c("lm", "rf")) {
    cv <- suppressWarnings(perform_kriging_loocv(
      pts, "v", "aux1", model_type = type, folds = kriging_cv_folds(pts, "v", "aux1", "loocv"), rf_ntree = 50))
    cv_changed <- suppressWarnings(perform_kriging_loocv(
      changed, "v", "aux1", model_type = type, folds = kriging_cv_folds(changed, "v", "aux1", "loocv"), rf_ntree = 50))
    expect_true(is.finite(cv$var1.pred[1]))
    expect_true(is.finite(cv_changed$var1.pred[1]))
    expect_equal(cv_changed$var1.pred[1], cv$var1.pred[1], tolerance = 1e-8)
  }
})

# ── The covariate screen inside each fold ─────────────────────────────────

test_that("the per-fold covariate screen raises no pruning warning", {
  set.seed(11)
  df <- data.frame(a = rnorm(30))
  df$b <- df$a + rnorm(30, 0, 1e-6)
  # Run once per fold, the screen would otherwise add one "VIF Iterative
  # Pruning" warning per fold to the run log, repeating the covariate-gate line.
  expect_silent(res <- screen_covariates(df, c("a", "b")))
  expect_length(res$kept, 1)
  expect_identical(res$dropped_vif, setdiff(c("a", "b"), res$kept))
  # Fewer than two candidates have nothing to compare, so they pass through.
  expect_identical(screen_covariates(df, "a")$kept, "a")
  expect_identical(screen_covariates(df, character(0))$kept, character(0))
})

test_that("the RK fold screen reads only its own training rows", {
  # aux2 follows aux1 closely in every row but the last. Version A keeps that
  # pattern at the last row too, so the FULL-data screen sees VIF 38 and drops
  # one; version B puts an outlier there, so the full-data screen sees VIF 2.4
  # and keeps both. The last row's own fold trains on the SAME rows in both
  # versions (VIF 38 either way), so a screen run inside the fold must hand it
  # the same prediction, which a screen run once on all the data cannot.
  set.seed(7)
  n <- 20L
  a1 <- rnorm(n)
  base <- sf::st_as_sf(data.frame(
    x = runif(n, 450000, 451000), y = runif(n, 5800000, 5801000),
    aux1 = a1, aux2 = a1 + rnorm(n, 0, 0.25),
    v = 3 * a1 + rnorm(n, 0, 0.5)
  ), coords = c("x", "y"), crs = 32633)
  a <- base
  b <- base; b$aux2[n] <- b$aux2[n] + 5

  expect_length(screen_covariates(a, c("aux1", "aux2"))$kept, 1)
  expect_length(screen_covariates(b, c("aux1", "aux2"))$kept, 2)

  grid <- make_test_grid_safe(base, res = 400)
  lags <- calc_scientific_lags(base)
  run <- function(p) suppressWarnings(apply_RK(p, "v", grid, lags,
    list(cv_strategy = "loocv"), c("aux1", "aux2")))
  res_a <- run(a); res_b <- run(b)
  expect_false(grepl("Falling back to OK", res_a$log_msg, fixed = TRUE))
  expect_false(grepl("Falling back to OK", res_b$log_msg, fixed = TRUE))
  expect_true(is.finite(res_a$cv_obj$var1.pred[n]))
  expect_equal(res_b$cv_obj$var1.pred[n], res_a$cv_obj$var1.pred[n], tolerance = 1e-8)
})

test_that("a fold whose screen keeps no covariate reports NA, not another model", {
  # Both covariates are zero in every row but the last, so the last row's own
  # fold trains on two constants: its screen keeps nothing and the fold has no
  # trend model. Its prediction must be NA and counted as missing coverage,
  # never filled in from another engine.
  set.seed(3)
  n <- 12L
  pts <- sf::st_as_sf(data.frame(
    x = runif(n, 450000, 451000), y = runif(n, 5800000, 5801000),
    aux1 = c(rep(0, n - 1L), 5), aux2 = c(rep(0, n - 1L), 3),
    v = rnorm(n, 10, 2)
  ), coords = c("x", "y"), crs = 32633)
  grid <- make_test_grid_safe(pts, res = 400)
  res <- suppressWarnings(apply_RK(pts, "v", grid, calc_scientific_lags(pts),
    list(cv_strategy = "loocv"), c("aux1", "aux2")))

  expect_false(grepl("Falling back to OK", res$log_msg, fixed = TRUE))
  expect_s3_class(res$res_sf, "sf")          # the map still runs
  expect_true(is.na(res$cv_obj$var1.pred[n]))
  expect_equal(sum(!is.na(res$cv_obj$var1.pred)), n - 1L)
  m <- perform_cv(res$cv_obj)
  expect_equal(m$n, n - 1L)
  expect_equal(m$n_expected, n)
  expect_match(res$log_msg, paste0("fold ", n), fixed = TRUE)
})

test_that("each RFK fold draws its forest from its own seed", {
  # A forest's number of RNG draws depends on the data it is grown on, so a
  # single stream over the whole fold loop lets EARLIER folds (which train on
  # the last row) decide where the last row's own forest starts. Raising only
  # the last row's target must not move its own out-of-fold prediction.
  set.seed(42)
  n <- 30L
  a1 <- rnorm(n); a2 <- rnorm(n)
  pts <- sf::st_as_sf(data.frame(
    x = runif(n, 450000, 452000), y = runif(n, 5800000, 5802000),
    aux1 = a1, aux2 = a2, v = 2 * a1 - a2 + rnorm(n, 0, 0.5)
  ), coords = c("x", "y"), crs = 32633)
  raised <- pts
  raised$v[n] <- raised$v[n] + 10 * sd(pts$v)

  cv_of <- function(p) suppressWarnings(perform_kriging_loocv(
    p, "v", c("aux1", "aux2"), model_type = "rf", rf_ntree = 100,
    folds = kriging_cv_folds(p, "v", c("aux1", "aux2"), "loocv")))
  base <- cv_of(pts); moved <- cv_of(raised)
  expect_true(is.finite(base$var1.pred[n]))
  expect_equal(moved$var1.pred[n], base$var1.pred[n], tolerance = 1e-8)
})

# ── The covariate support of a fold ───────────────────────────────────────
# The map kriges each covariate from every covariate-complete location, with
# or without a target value. A fold must do the same, less its held-out
# locations: scored on the training rows alone it would estimate the error of a
# map built from sparser covariates than the one drawn.

# A covariate at `n_cov` locations and the target at `n_t` of them, as the run
# builds its point sets: `cov` (the map's covariate support, every location)
# and `t` (the target rows), both carrying LOC_ID_COL.
make_covariate_dense <- function(n_cov = 200, n_t = 40, seed = 5) {
  d <- with_seed(seed, {
    x <- 500000 + runif(n_cov, 0, 2000); y <- 4000000 + runif(n_cov, 0, 2000)
    z <- sin(x / 300) + cos(y / 400)
    data.frame(x = x, y = y, ec = z + rnorm(n_cov, 0, 0.1),
               v = ifelse(seq_len(n_cov) %in% sample(n_cov, n_t), 10 + 2 * z + rnorm(n_cov, 0, 0.2), NA_real_),
               pv = NA_real_)
  })
  pp <- .locality_points("L", d, 32635, "RK", "ec", list())
  list(d = d, cov = pp$pts, t = dedup_valid_points(pp$pts_projected, "v"))
}

test_that("a fold kriges its held-out covariates from the map's covariate locations less the held-out ones", {
  s <- make_covariate_dense()
  expect_equal(c(nrow(s$cov), nrow(s$t)), c(200L, 40L))                        # the premise
  folds <- rep(1:5, 8)
  calls <- list()
  original <- krige_covariates
  withr::defer(assign("krige_covariates", original, envir = globalenv()))
  assign("krige_covariates", function(data, grid_p, aux_vars, lags, method_params, ...) {
    out <- original(data, grid_p, aux_vars, lags, method_params, ...)
    calls[[length(calls) + 1L]] <<- list(ids = data[[LOC_ID_COL]], lags = lags,
                                         xy = sf::st_coordinates(grid_p), ec = out$grid_aux$ec)
    out
  }, envir = globalenv())

  for (type in c("lm", "rf")) {
    calls <- list()
    cv <- suppressWarnings(perform_kriging_loocv(s$t, "v", "ec", model_type = type, rf_ntree = 50,
                                                 folds = folds, cov_data = s$cov))
    expect_length(calls, 5L)
    expect_true(all(is.finite(cv$var1.pred)))
    expect_identical(attr(cv, "cov_support"), c(n = 200L, extra = 160L))
    for (k in 1:5) {
      held <- s$t[folds == k, ]
      support <- s$cov[!s$cov[[LOC_ID_COL]] %in% held[[LOC_ID_COL]], ]
      # Every covariate location but the eight held out, target or not.
      expect_setequal(calls[[k]]$ids, support[[LOC_ID_COL]])
      expect_length(calls[[k]]$ids, 192L)
      expect_equal(unname(calls[[k]]$xy), unname(sf::st_coordinates(held)))
      # The values are a kriging of the covariate from that support, with the
      # support's own lags and variogram, computed here on its own.
      lags <- calc_scientific_lags(support)
      expect_equal(calls[[k]]$lags, lags)
      v_emp <- gstat::variogram(ec ~ 1, support, width = lags$width, cutoff = lags$cutoff)
      direct <- gstat::krige(ec ~ 1, support, sf::st_sf(geometry = sf::st_geometry(held)),
                             model = robust_vgm_fit(v_emp, support$ec), debug.level = 0)
      expect_equal(calls[[k]]$ec, direct$var1.pred)
    }
  }

  # Without a covariate support (a direct call) the training rows are it.
  calls <- list()
  cv0 <- suppressWarnings(perform_kriging_loocv(s$t, "v", "ec", model_type = "lm", folds = folds))
  expect_true(all(lengths(lapply(calls, `[[`, "ids")) == 32L))
  expect_null(attr(cv0, "cov_support"))
})

test_that("the covariate support hands a fold nothing about the values it is scored on", {
  s <- make_covariate_dense(n_cov = 120, n_t = 30)
  loo <- function(t, cov, type = "lm") suppressWarnings(perform_kriging_loocv(
    t, "v", "ec", model_type = type, rf_ntree = 50, folds = kriging_cv_folds(t, "v", "ec", "loocv"),
    cov_data = cov))
  for (type in c("lm", "rf")) {
    base <- loo(s$t, s$cov, type)
    # The held-out location's own covariate, wherever it is stored.
    i <- 3L
    at <- s$cov[[LOC_ID_COL]] == s$t[[LOC_ID_COL]][i]
    expect_equal(sum(at), 1L)
    t2 <- s$t; cov2 <- s$cov
    t2$ec[i] <- t2$ec[i] + 100
    cov2$ec[at] <- cov2$ec[at] + 100
    expect_equal(loo(t2, cov2, type)$var1.pred[i], base$var1.pred[i], tolerance = 1e-8)
    # The covariate-only locations carry no target value a fold reads.
    cov3 <- s$cov
    cov3$v[is.na(cov3$v)] <- 1e6
    expect_identical(loo(s$t, cov3, type)$var1.pred, base$var1.pred)
  }
  # Where every covariate location carries the target, the support of a fold is
  # its training rows, value for value.
  all_t <- s$cov[!is.na(s$cov$v), ]
  for (type in c("lm", "rf")) {
    expect_identical(loo(s$t, all_t, type)$var1.pred,
                     suppressWarnings(perform_kriging_loocv(s$t, "v", "ec", model_type = type, rf_ntree = 50,
                                                            folds = kriging_cv_folds(s$t, "v", "ec", "loocv")))$var1.pred)
  }
})

test_that("a held-out location leaves the covariate support whole, co-located samples included", {
  # A second sample at a target location, measuring the covariate and not the
  # target: the display set holds one location with the mean covariate, and in
  # the fold that holds the location out neither sample may stay.
  s0 <- make_covariate_dense(n_cov = 80, n_t = 25)
  first <- which(!is.na(s0$d$v))[1]
  twin <- s0$d[first, ]
  twin$v <- NA_real_
  twin$ec <- twin$ec + 3
  d <- rbind(s0$d, twin)
  pp <- .locality_points("L", d, 32635, "RK", "ec", list())
  t <- dedup_valid_points(pp$pts_projected, "v")
  expect_equal(c(nrow(pp$pts), nrow(t)), c(80L, 25L))
  i <- which(abs(t$ec - s0$d$ec[first]) < 1e-12)
  expect_length(i, 1L)
  at <- pp$pts[[LOC_ID_COL]] == t[[LOC_ID_COL]][i]
  # One location in both sets: the target row keeps the covariate of the sample
  # that measured the target, the support the mean of both samples.
  expect_equal(pp$pts$ec[at], s0$d$ec[first] + 1.5)

  seen <- list()
  original <- krige_covariates
  withr::defer(assign("krige_covariates", original, envir = globalenv()))
  assign("krige_covariates", function(data, grid_p, ...) {
    seen[[length(seen) + 1L]] <<- list(ids = data[[LOC_ID_COL]],
                                       d_min = min(sf::st_distance(data, grid_p)))
    original(data, grid_p, ...)
  }, envir = globalenv())
  cv <- suppressWarnings(perform_kriging_loocv(t, "v", "ec", model_type = "lm",
                                               folds = kriging_cv_folds(t, "v", "ec", "loocv"), cov_data = pp$pts))
  expect_length(seen, 25L)
  expect_false(t[[LOC_ID_COL]][i] %in% seen[[i]]$ids)
  expect_length(seen[[i]]$ids, 79L)
  # No support row sits on the held-out point in any fold.
  expect_true(all(vapply(seen, function(f) as.numeric(f$d_min) > 0.01, logical(1))))
  expect_true(all(is.finite(cv$var1.pred)))
})

test_that("a covariate support that cannot be matched to the CV rows is refused", {
  s <- make_covariate_dense(n_cov = 60, n_t = 20)
  bare <- s$cov; bare[[LOC_ID_COL]] <- NULL
  other <- s$cov; other[[LOC_ID_COL]] <- other[[LOC_ID_COL]] + 1000L
  for (cov in list(bare, other)) {
    expect_error(perform_kriging_loocv(s$t, "v", "ec", model_type = "lm", cov_data = cov),
                 "does not name the locations of the CV population", fixed = TRUE)
  }
})

test_that("an RK or RFK run scores its folds on the map's covariate support and says so", {
  s <- make_covariate_dense()
  item <- list(l = "L", pts_data = s$d,
               m_params = list(cv_strategy = "auto", idw_p_act = 2, idw_nmax = 12, rf_ntree = 50))
  seen <- integer(0)
  original <- krige_covariates
  withr::defer(assign("krige_covariates", original, envir = globalenv()))
  assign("krige_covariates", function(data, ...) { seen <<- c(seen, nrow(data)); original(data, ...) },
         envir = globalenv())
  for (method in c("RK", "RFK")) {
    seen <- integer(0)
    res <- suppressWarnings(run_regional_interpolation(
      item, method, 32635, "ec", NULL, "convex", "fixed", 200, "fixed", 100, "EPSG:32635", FALSE, "actual"))
    expect_false(grepl("Falling back to OK", res$log_msg, fixed = TRUE))
    # The map reads all 200 covariate locations; each of the 40 leave-one-out
    # folds reads the other 199.
    expect_equal(seen[1], 200L)
    expect_true(all(seen[-1] == 199L))
    expect_length(seen, 41L)
    expect_match(res$log_msg, sprintf(paste0("[%s CV] each fold kriged its held-out covariates from the map's 200 ",
                                             "covariate locations less the held-out ones; 160 of them carry no target value."),
                                      method), fixed = TRUE)
    expect_equal(res$cv_act$n, 40L)
    expect_equal(res$cv_act$coverage, 1)
    # The location index is the pipeline's own: it does not reach the points
    # the interface lists columns of.
    expect_false(LOC_ID_COL %in% names(res$pts))
    expect_equal(nrow(res$pts), 200L)
  }
})
