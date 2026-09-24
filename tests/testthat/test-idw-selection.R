# test-idw-selection.R — IDW power selection, Auto (CV).
#
# The power is selected on an exact nearest-neighbour IDW kernel
# (idw_kernel_predict) because nesting the selection inside cross-validation
# multiplies the number of IDW cross-validations by the grid size and the inner
# fold count, which gstat::krige.cv cannot afford. Every reported prediction
# still comes from gstat::idw(). So the kernel is pinned against krige.cv, the
# selection against a brute-force krige.cv search, and the nesting against an
# independent recomputation from each fold's own training rows. The candidate
# powers span the whole IDW family: equal weights (p = 0, gstat's idp = 0) to
# the nearest-neighbour limit (p = Inf, gstat's IDW over nmax = 1).

idw_fixture <- function(scope = "tiny", locality = NULL) {
  pts <- golden_sf(scope, localities = locality)
  list(pts = pts, xy = sf::st_coordinates(pts), v = pts$ph)
}

# gstat's cross-validation of one IDW family member: the nearest-neighbour
# limit is gstat's IDW over the single nearest sample.
gstat_idw_cv <- function(pts, folds, p, nmax) {
  if (is.infinite(p)) {
    return(gstat::krige.cv(ph ~ 1, pts, nmax = 1, nfold = folds, debug.level = 0))
  }
  gstat::krige.cv(ph ~ 1, pts, nmax = nmax, set = list(idp = p), nfold = folds, debug.level = 0)
}

test_that("the kernel reproduces gstat's IDW cross-validation", {
  # Leave-one-out on golden tiny for every power and neighbourhood; on the 198
  # Yorga samples leave-one-out once (krige.cv costs one gstat call per fold)
  # and the random and block 10-fold designs for every combination. The two
  # ends of the family are among the powers.
  tiny <- idw_fixture("tiny")
  yorga <- idw_fixture("core", "Yorga")
  powers <- c(0, 0.5, 2, 4.75, IDW_MAX_FINITE_POWER, Inf)
  check <- function(f, folds, p, nmax, label) {
    ref <- gstat_idw_cv(f$pts, folds, p, nmax)
    got <- idw_cv_predictions(f$xy, f$v, folds, p, nmax)[, 1]
    expect_equal(got, ref$var1.pred, tolerance = 1e-10, info = label)
  }
  for (p in powers) for (nmax in c(4, 12)) {
    check(tiny, seq_len(nrow(tiny$xy)), p, nmax, sprintf("tiny LOO p=%s nmax=%s", p, nmax))
    for (strategy in c("auto", "block")) {
      folds <- make_cv_folds(yorga$xy, strategy, nrow(yorga$xy), CV_FOLD_SEED)
      check(yorga, folds, p, nmax, sprintf("Yorga %s p=%s nmax=%s", strategy, p, nmax))
    }
  }
  check(yorga, seq_len(nrow(yorga$xy)), 2, 12, "Yorga LOO p=2 nmax=12")

  # A test location on a sample takes that sample's value, as gstat does.
  hit <- idw_kernel_predict(tiny$xy[-1, ], tiny$v[-1], tiny$xy[2, , drop = FALSE], c(1, 3), 12)
  expect_equal(unname(hit[1, ]), rep(tiny$v[2], 2))
})

test_that("the selected power is the brute-force krige.cv optimum on the same folds", {
  tiny <- idw_fixture("tiny")
  n <- nrow(tiny$xy)
  brute <- list()
  for (strategy in c("auto", "loocv", "block")) {
    folds <- make_cv_folds(tiny$xy, strategy, n, CV_FOLD_SEED)
    # At n = 40 "auto" resolves to leave-one-out, the same partition as
    # "loocv"; the brute-force search is run once per distinct fold vector.
    key <- paste(folds, collapse = ",")
    if (is.null(brute[[key]])) {
      brute[[key]] <- vapply(IDW_POWER_GRID, function(p) gstat_idw_cv(tiny$pts, folds, p, 12)$residual,
                             numeric(n))
    }
    e2 <- brute[[key]]^2
    rmse <- sqrt(colMeans(e2))
    sel <- select_idw_power(tiny$xy, tiny$v, strategy, 12)
    expect_identical(sel$p, IDW_POWER_GRID[which.min(rmse)], info = strategy)
    expect_equal(sel$profile$rmse, rmse, tolerance = 1e-10, info = strategy)
    expect_null(sel$skipped)
    # Within one standard error of the best: the mean squared-error excess
    # over the best is at most one standard error of the per-sample
    # differences, computed here from gstat's own residuals.
    d <- e2 - e2[, which.min(rmse)]
    expect_identical(sel$profile$within_se, unname(colMeans(d) <= apply(d, 2, sd) / sqrt(n)), info = strategy)
    expect_true(sel$profile$within_se[sel$profile$p == sel$p])
  }
})

test_that("the three strategies resolve to genuinely different partitions", {
  # The premise the brute-force comparison rests on: at n = 60, auto resolves
  # to random 10-fold, loocv to 60 folds and block to 10 spatial clusters. If
  # these collapsed onto one partition, the strategy loop would still pass
  # while proving nothing about the fold authority.
  pts <- make_test_points(60, seed = 11)
  coords <- sf::st_coordinates(pts)
  expect_length(unique(make_cv_folds(coords, "auto", 60, CV_FOLD_SEED)), 10)
  expect_length(unique(make_cv_folds(coords, "loocv", 60, CV_FOLD_SEED)), 60)
  expect_length(unique(make_cv_folds(coords, "block", 60, CV_FOLD_SEED)), 10)
  expect_false(identical(make_cv_folds(coords, "auto", 60, CV_FOLD_SEED),
                         make_cv_folds(coords, "block", 60, CV_FOLD_SEED)))
})

test_that("the power selection leaves the caller's RNG stream untouched", {
  f <- idw_fixture("tiny")
  set.seed(999)
  before <- .Random.seed
  invisible(select_idw_power(f$xy, f$v, "block", 12))
  expect_identical(.Random.seed, before)
})

test_that("Auto (CV) re-selects the power in every fold from its own training rows", {
  f <- idw_fixture("core", "Yorga")
  pts <- f$pts
  grid <- make_test_grid_safe(pts, res = 1500)
  mp <- list(idw_p = -1, idw_nmax = 12, cv_strategy = "block")
  res <- suppressWarnings(apply_IDW(pts, "ph", grid, mp, "Yorga", "act"))

  # The CV object keeps krige.cv's shape, so every consumer reads it the same way.
  expect_s3_class(res$cv_obj, "sf")
  expect_identical(setdiff(names(res$cv_obj), attr(res$cv_obj, "sf_column")),
                   c("var1.pred", "var1.var", "observed", "residual", "zscore", "fold"))
  expect_equal(res$cv_obj$residual, res$cv_obj$observed - res$cv_obj$var1.pred)

  # Independent recomputation of each fold's power, and of its predictions with
  # gstat at that power.
  folds <- make_cv_folds(f$xy, "block", nrow(f$xy), CV_FOLD_SEED)
  ids <- sort(unique(folds))
  expect_identical(res$cv_obj$fold, folds)
  fold_p <- vapply(ids, function(i) {
    tr <- folds != i
    select_idw_power(f$xy[tr, ], f$v[tr], "block", 12)$p
  }, numeric(1))
  expect_equal(unname(attr(res$cv_obj, "fold_power")), fold_p)
  expect_equal(unname(res$idw_fit$fold_p), fold_p)
  # gstat at a family member: the nearest-neighbour limit is its IDW over the
  # single nearest sample.
  ref_idw <- function(train, test, p) {
    if (is.infinite(p)) return(gstat::idw(ph ~ 1, train, test, nmax = 1, debug.level = 0))
    gstat::idw(ph ~ 1, train, test, nmax = 12, idp = p, debug.level = 0)
  }
  for (j in seq_along(ids)) {
    te <- folds == ids[j]
    expect_equal(res$cv_obj$var1.pred[te], ref_idw(pts[!te, ], pts[te, ], fold_p[j])$var1.pred)
  }

  # The map is gstat::idw at the power selected on all rows.
  expect_identical(res$idw_fit$mode, "cv")
  expect_identical(res$idw_fit$select_source, "own")
  expect_identical(res$idw_fit$p, select_idw_power(f$xy, f$v, "block", 12)$p)
  expect_equal(res$res_sf$var1.pred, ref_idw(pts, grid, res$idw_fit$p)$var1.pred)
  expect_match(res$log_msg, paste0("[IDW] Yorga (Actual): Auto (CV) selected ", idw_power_text(res$idw_fit$p),
                                   " on all rows"), fixed = TRUE)
  # The log says how well the data separate the powers.
  prof <- res$idw_fit$profile
  expect_true(prof$within_se[prof$p == res$idw_fit$p])
  expect_match(res$log_msg, paste0("\\[IDW\\] Yorga \\(Actual\\): Auto \\(CV\\): (None of the other 36 powers|",
                                   "[0-9]+ of 37 powers) (is|are) within one standard error"))

  # Leakage check: a held-out value cannot move the power its own fold selects.
  i_out <- which(folds == ids[1])[1]
  bumped <- pts
  bumped$ph[i_out] <- bumped$ph[i_out] * 10
  res_b <- suppressWarnings(apply_IDW(bumped, "ph", grid, mp, "Yorga", "act"))
  expect_equal(unname(attr(res_b$cv_obj, "fold_power"))[1], fold_p[1])
})

test_that("repeated CV re-runs the nested selection on every fold realization", {
  pts <- make_test_points(70, seed = 13)
  grid <- make_test_grid_safe(pts, res = 250)
  mp <- list(idw_p = -1, idw_nmax = 12, cv_strategy = "auto")
  one <- suppressWarnings(apply_IDW(pts, "v", grid, mp))
  rep3 <- suppressWarnings(apply_IDW(pts, "v", grid, c(mp, list(cv_repeats = 3))))
  # Realization 1 is the single-realization run; the map does not move.
  expect_length(rep3$cv_obj_reps, 3)
  expect_equal(rep3$cv_metrics, one$cv_metrics)
  expect_equal(rep3$res_sf$var1.pred, one$res_sf$var1.pred)
  expect_equal(rep3$idw_fit$fold_p, one$idw_fit$fold_p)
  expect_equal(perform_cv(rep3$cv_obj_reps[[1]], moran = FALSE)$rmse, one$cv_metrics$rmse)
  # A later realization folds differently, so its predictions differ.
  expect_false(isTRUE(all.equal(rep3$cv_obj_reps[[2]]$var1.pred, rep3$cv_obj_reps[[1]]$var1.pred)))
})

test_that("Fixed p keeps gstat's own cross-validation", {
  f <- idw_fixture("tiny")
  grid <- make_test_grid_safe(f$pts, res = 500)
  res <- apply_IDW(f$pts, "ph", grid, list(idw_p = 2, idw_nmax = 12, cv_strategy = "auto"))
  folds <- make_cv_folds(f$xy, "auto", nrow(f$xy), CV_FOLD_SEED)
  ref <- gstat::krige.cv(ph ~ 1, f$pts, nmax = 12, set = list(idp = 2), nfold = folds, debug.level = 0)
  expect_equal(sf::st_drop_geometry(res$cv_obj), sf::st_drop_geometry(ref))
  expect_identical(res$idw_fit$mode, "fixed")
  expect_identical(res$idw_fit$p, 2)
  expect_null(res$idw_fit$select_source)
  expect_null(attr(res$cv_obj, "fold_power"))
})

test_that("the search spans the whole family, so an end is the family's own end", {
  expect_identical(IDW_POWER_GRID[1], 0)
  expect_identical(utils::tail(IDW_POWER_GRID, 1), Inf)
  expect_identical(max(IDW_POWER_GRID[is.finite(IDW_POWER_GRID)]), IDW_MAX_FINITE_POWER)
  expect_false(is.unsorted(IDW_POWER_GRID))

  # Nearest-neighbour end. Pairs 1 m apart on a line, each pair 1.5 m from the
  # next and carrying one value: a held-out sample's twin predicts it exactly,
  # and every finite power still gives the next pair's sample, (1/1.5)^p of
  # the weight, some error. Only the nearest-neighbour limit is exact.
  pos <- as.vector(rbind(seq(0, by = 2.5, length.out = 20), seq(0, by = 2.5, length.out = 20) + 1))
  xy <- cbind(pos, 0)
  v <- rep(c(3, 7, 4, 9, 1, 6, 8, 2, 5, 10), each = 2, times = 2)
  sel <- select_idw_power(xy, v, "loocv", 12)
  expect_identical(sel$p, Inf)
  expect_identical(sel$limit, "nearest_neighbour")
  expect_equal(utils::tail(sel$profile$rmse, 1), 0)
  expect_true(all(utils::head(sel$profile$rmse, -1) > 0))

  # Samples on a line (y = 4e6) and a two-row grid along it.
  line_sf <- function(x, v) sf::st_as_sf(data.frame(x = x, y = 4e6, v = v), coords = c("x", "y"), crs = 32635)
  line_grid <- function(x) sf::st_as_sf(expand.grid(x = x, y = 4e6 + c(-1, 1)), coords = c("x", "y"), crs = 32635)
  pts <- line_sf(pos, v)
  grid <- line_grid(seq(0, 48, by = 2))
  dir <- withr::local_tempdir("idw_end_")
  withr::local_options(monolith_progress_dir = dir, monolith_session_id = "end")
  res <- suppressWarnings(apply_IDW(pts, "v", grid, list(idw_p = -1, idw_nmax = 12, cv_strategy = "loocv"),
                                    "Step", "act"))
  expect_true(is.infinite(res$idw_fit$p))
  # The map is gstat's IDW over the single nearest sample.
  expect_equal(res$res_sf$var1.pred, gstat::idw(v ~ 1, pts, grid, nmax = 1, debug.level = 0)$var1.pred)
  expect_match(res$log_msg, "[WARN] Step (Actual): Auto (CV) selected the nearest-neighbour limit (p → ∞)",
               fixed = TRUE)
  expect_match(paste(readLines(file.path(dir, "warn_end_Step_act.txt")), collapse = " "),
               "stepped (Thiessen) surface", fixed = TRUE)

  # Equal-weights end. Alternating values on a line: a sample's nearest
  # neighbours carry the other value and the next ones its own, so any
  # distance weighting leans toward the wrong value and equal weights (p = 0)
  # predict best.
  line <- cbind(seq(0, 390, by = 10), 0)
  alt <- rep(c(1, -1), 20)
  eq <- select_idw_power(line, alt, "loocv", 4)
  expect_identical(eq$p, 0)
  expect_identical(eq$limit, "equal_weights")
  expect_true(all(eq$profile$rmse[-1] > eq$profile$rmse[1]))
  pts_eq <- line_sf(line[, 1], alt)
  grid_eq <- line_grid(seq(0, 390, by = 15))
  res_eq <- suppressWarnings(apply_IDW(pts_eq, "v", grid_eq, list(idw_p = -1, idw_nmax = 4, cv_strategy = "loocv"),
                                       "Alt", "act"))
  expect_identical(res_eq$idw_fit$p, 0)
  # The map is the plain mean of the four nearest samples (gstat's idp = 0).
  nn4 <- FNN::get.knnx(sf::st_coordinates(pts_eq), sf::st_coordinates(grid_eq), k = 4)$nn.index
  expect_equal(res_eq$res_sf$var1.pred, rowMeans(matrix(alt[nn4], ncol = 4)))
  expect_match(res_eq$log_msg, "the map is the mean of the 4 nearest samples", fixed = TRUE)
  expect_match(res_eq$log_msg, "Max Neighbors now sets the smoothing", fixed = TRUE)
  expect_no_match(res_eq$log_msg, "[WARN]", fixed = TRUE)
  # Max Neighbors reaching every sample: the equal-weights map is one value,
  # and the note says so.
  res_flat <- suppressWarnings(apply_IDW(line_sf(line[1:8, 1], alt[1:8]), "v", grid_eq,
                                         list(idw_p = -1, idw_nmax = 12, cv_strategy = "loocv"), "Few", "act"))
  expect_identical(res_flat$idw_fit$p, 0)
  expect_identical(unique(res_flat$res_sf$var1.pred), mean(alt[1:8]))
  expect_match(res_flat$log_msg, "Max Neighbors reaches all 8 samples, so the map is one value, their mean",
               fixed = TRUE)

  small <- select_idw_power(xy[1:4, ], v[1:4], "loocv", 12)
  expect_identical(small$p, 2)
  expect_match(small$skipped, "fewer than 5", fixed = TRUE)
  flat <- select_idw_power(xy, rep(3, nrow(xy)), "loocv", 12)
  expect_identical(flat$p, 2)
  expect_match(flat$skipped, "no usable variance", fixed = TRUE)
  res_small <- suppressWarnings(apply_IDW(pts[1:4, ], "v", line_grid(c(0, 2, 4)),
                                          list(idw_p = -1, idw_nmax = 12, cv_strategy = "loocv"), "Few", "act"))
  expect_identical(res_small$idw_fit$p, 2)
  expect_match(res_small$log_msg, "not searched", fixed = TRUE)
})

test_that("a steep power is read like the nearest-neighbour limit, and the log says how well the data separate the powers", {
  dir <- withr::local_tempdir("idw_steep_")
  withr::local_options(monolith_progress_dir = dir, monolith_session_id = "steep")
  # From IDW_STEEP_POWER up, a sample 10% farther than the nearest keeps under a
  # ninth of the nearest's weight; at the grid power below it, more.
  expect_lt((1 / 1.1)^IDW_STEEP_POWER, 1 / 9)
  expect_gt((1 / 1.1)^max(IDW_POWER_GRID[IDW_POWER_GRID < IDW_STEEP_POWER]), 1 / 9)
  expect_true(idw_stepped(IDW_STEEP_POWER))
  expect_true(idw_stepped(Inf))
  expect_false(idw_stepped(20))
  expect_false(idw_stepped(NA_real_))

  prof <- data.frame(p = IDW_POWER_GRID, rmse = 1, within_se = IDW_POWER_GRID >= 1.5)
  fit <- list(mode = "cv", p = 32, limit = NULL, skipped = NULL, fold_p = c(`1` = 24, `2` = 48),
              nmax = 12, n_samples = 60, select_source = "own", profile = prof)
  out <- .log_idw_selection(list(log_msg = "", idw_fit = fit), "Steep", "act")
  expect_match(out$log_msg, paste0("[WARN] Steep (Actual): Auto (CV) selected p = 32: a sample 10% farther than ",
                                   "the nearest keeps under a ninth of the nearest's weight"), fixed = TRUE)
  expect_match(paste(readLines(file.path(dir, "warn_steep_Steep_act.txt")), collapse = " "),
               "practically the stepped nearest-neighbour (Thiessen) surface", fixed = TRUE)
  expect_match(out$log_msg, sprintf(paste0("[IDW] Steep (Actual): Auto (CV): %d of 37 powers are within one standard ",
                                           "error of the best, p = 2 among them: on these folds the data do not ",
                                           "distinguish p = 2 from p = 32."), sum(IDW_POWER_GRID >= 1.5)), fixed = TRUE)

  # Below the steep powers nothing is raised, and a power the data separate
  # from every other says so.
  mid <- modifyList(fit, list(p = 20))
  mid$profile$within_se <- IDW_POWER_GRID == 20
  out_mid <- .log_idw_selection(list(log_msg = "", idw_fit = mid), "Mid", "act")
  expect_no_match(out_mid$log_msg, "[WARN]", fixed = TRUE)
  expect_false(file.exists(file.path(dir, "warn_steep_Mid_act.txt")))
  expect_match(out_mid$log_msg, "None of the other 36 powers is within one standard error of the selected one.",
               fixed = TRUE)
  # p = 2 outside the band, and a selection of p = 2 itself.
  far <- modifyList(fit, list(p = 6))
  far$profile$within_se <- IDW_POWER_GRID >= 5
  expect_match(idw_flatness_note(far$profile, far$p), "; p = 2 is not among them.", fixed = TRUE)
  expect_identical(idw_flatness_note(prof, 2), sprintf("%d of 37 powers are within one standard error of the best.",
                                                        sum(IDW_POWER_GRID >= 1.5)))
  expect_null(idw_flatness_note(data.frame(p = c(1, 2), rmse = c(1, 2)), 1))
})

test_that("an unseparated Predicted surface takes the power the measured values select", {
  f <- idw_fixture("core", "Yorga")
  pts <- f$pts
  pts$pv <- pts$ph + with_seed(3, rnorm(nrow(pts), 0, 0.2))
  grid <- make_test_grid_safe(pts, res = 1500)
  mp <- list(idw_p = -1, idw_nmax = 12, cv_strategy = "block", idw_select_col = "ph")
  res <- suppressWarnings(apply_IDW(pts, "pv", grid, mp, "Yorga", "pre"))

  expect_identical(res$idw_fit$p, select_idw_power(f$xy, f$v, "block", 12)$p)
  folds <- make_cv_folds(f$xy, "block", nrow(f$xy), CV_FOLD_SEED)
  fold_p <- vapply(sort(unique(folds)), function(i) {
    tr <- folds != i
    select_idw_power(f$xy[tr, ], f$v[tr], "block", 12)$p
  }, numeric(1))
  expect_equal(unname(attr(res$cv_obj, "fold_power")), fold_p)
  # Scored on its own column.
  expect_equal(res$cv_obj$observed, pts$pv)
  # The record, the log and the panel say whose values selected the power.
  expect_identical(res$idw_fit$select_source, "measured")
  expect_match(res$log_msg, paste0("[IDW] Yorga (Predicted): Auto (CV) selected ", idw_power_text(res$idw_fit$p),
                                   " on the measured values of all rows"), fixed = TRUE)
})

test_that("the Per locality note names each locality's own value and what the others run with", {
  # Kale and Yorga hold values applied for ph; Tavas's was applied for another
  # variable, so for ph it has none and runs with the setting for all localities.
  store <- list(
    Kale = list(act = list(value = -1, key = "ph"), pre = list(value = 3, key = "ph_cve")),
    Yorga = list(act = list(value = 4, key = "ph")),
    Tavas = list(act = list(value = 2.5, key = "k")))
  locs <- c("Kale", "Tavas", "Yorga")
  note <- per_locality_note("IDW", store, "act", locs, "ph", "Fixed p = 3.5", "Tavas")
  expect_identical(note, c(
    "Tavas has no value of its own: it runs with the setting for all localities, Fixed p = 3.5, which the controls below start from.",
    "Own values: Kale: Auto (CV); Yorga: Fixed p = 4. Tavas runs with the setting for all localities, Fixed p = 3.5."))
  expect_identical(per_locality_note("IDW", store, "act", locs, "ph", "Auto (CV)", "Yorga")[1],
                   "Yorga runs with its own value, Fixed p = 4.")
  # The Predicted slot is read for the Predicted target only.
  expect_match(per_locality_note("IDW", store, "pre", locs, "ph_cve", "Auto (CV)")[1],
               "Own values: Kale: Fixed p = 3. Tavas, Yorga run with", fixed = TRUE)
  expect_identical(per_locality_note("IDW", list(), "act", locs, "ph", "Fixed p = 2"),
                   "No locality has a value of its own yet; all run with the setting for all localities, Fixed p = 2.")
  tps <- list(A = list(act = list(value = 0, key = "k")), B = list(act = list(value = 2.4e-06, key = "k")))
  expect_identical(per_locality_note("TPS", tps, "act", c("A", "B"), "k", "Auto (GCV)"),
                   "Every locality has a value of its own (A: Exact (λ = 0); B: Fixed λ = 2.400e-06).")
  expect_identical(param_setting_text("TPS", -1), "Auto (GCV)")
  expect_identical(param_setting_text("IDW", 2), "Fixed p = 2")
})

test_that("the sidebar IDW panel reads only a displayed IDW run", {
  m <- list(A = list(n = 10, rmse = 2, me = 0.5), B = list(n = 30, rmse = 1, me = -0.1),
            C = list(n = 5, rmse = NA, me = NA))
  expect_null(idw_panel_metrics("OK", m))
  expect_null(idw_panel_metrics(NULL, m))
  out <- idw_panel_metrics("IDW", m)
  # Pooled RMSE: every residual weighted once, i.e. sqrt(sum(n * rmse^2) / sum(n))
  # over the localities that have one.
  expect_equal(out$Value[1], sqrt((10 * 2^2 + 30 * 1^2) / 40))
  expect_equal(out$Value[2], (10 * 0.5 + 30 * -0.1) / 40)
})
