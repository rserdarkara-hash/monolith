# test-knndm.R — kNNDM cross-validation (k-fold nearest-neighbour distance
# matching; Linnenbrink, Mila, Ludwig & Meyer 2024) and the CV Distance Match
# record. Every reference below is recomputed from the data the test hands in:
# a definition, a hand computation, a brute-force search or an independent
# implementation written here. The layouts are built without RNG: a regular
# lattice (random folds already match its map), six tight sub-grids far apart
# (they do not), and fifty tight sub-grids for the cell path.

lattice_xy <- function() unname(as.matrix(expand.grid(x = 0:14 * 100, y = 0:9 * 100)))

subgrids_xy <- function(nx = 3, ny = 2, gap = c(4000, 3000), side = c(5, 5), step = 20) {
  cen <- expand.grid(cx = seq_len(nx) - 1, cy = seq_len(ny) - 1)
  sub <- expand.grid(dx = (seq_len(side[1]) - 1) * step, dy = (seq_len(side[2]) - 1) * step)
  unname(do.call(rbind, lapply(seq_len(nrow(cen)), function(i) {
    cbind(cen$cx[i] * gap[1] + sub$dx, cen$cy[i] * gap[2] + sub$dy)
  })))
}

as_pts <- function(xy, crs = 32633, ...) {
  sf::st_as_sf(data.frame(x = xy[, 1], y = xy[, 2], ...), coords = c("x", "y"), crs = crs, remove = FALSE)
}

rect_sf <- function(xmin, ymin, xmax, ymax, crs = 32633) {
  sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
    c(xmin, ymin), c(xmax, ymin), c(xmax, ymax), c(xmin, ymax), c(xmin, ymin)))), crs = crs))
}

bbox_domain <- function(xy) {
  knndm_domain_points(rect_sf(min(xy[, 1]), min(xy[, 2]), max(xy[, 1]), max(xy[, 2])))
}

# The app's seeded random k-fold by its definition, not through cv_random_folds.
random_folds_by_definition <- function(n, k = 10L, seed = CV_FOLD_SEED) {
  withr::with_seed(seed, sample(rep(seq_len(k), length.out = n)),
                   .rng_kind = "Mersenne-Twister", .rng_normal_kind = "Inversion",
                   .rng_sample_kind = "Rejection")
}

nn_dist <- function(xy) FNN::get.knn(xy, k = 1)$nn.dist[, 1]
map_dist <- function(xy, D) FNN::get.knnx(xy, D, k = 1)$nn.dist[, 1]

# An independent implementation of the candidate search, written from the
# method's definition rather than calling .knndm_merge: Ward tree, q clusters
# ordered along the sign-fixed first principal axis, clusters of at least n/k
# samples keep their own fold, the rest dealt in turn over the remaining folds,
# partitions with a fold above maxp * n discarded. Returns every candidate
# cut's W (NA where discarded).
independent_candidates <- function(xy, Gij, k = 10, maxp = KNNDM_MAXP) {
  n <- nrow(xy)
  hc <- stats::hclust(stats::dist(xy), method = "ward.D2")
  pc <- stats::prcomp(xy, center = TRUE, scale. = FALSE)
  axis <- pc$rotation[, 1]
  if (axis[which.max(abs(axis))] < 0) axis <- -axis
  qs <- unique(as.integer(round(exp(seq(log(k), log(n - 2), length.out = 100)))))
  Ws <- vapply(qs, function(q) {
    cl <- stats::cutree(hc, k = q)
    size <- as.vector(table(factor(cl, levels = seq_len(q))))
    pc1 <- vapply(seq_len(q), function(j) {
      sum((colMeans(xy[cl == j, , drop = FALSE]) - pc$center) * axis)
    }, numeric(1))
    ord <- order(pc1)
    fold_of <- integer(q)
    used <- 0L
    for (j in ord) if (size[j] >= n / k) { used <- used + 1L; fold_of[j] <- used }
    small <- ord[size[ord] < n / k]
    if (length(small)) {
      rest <- setdiff(seq_len(k), seq_len(used))
      if (!length(rest)) return(NA_real_)
      fold_of[small] <- rep(rest, length.out = length(small))
    }
    folds <- fold_of[cl]
    if (any(table(folds) / n > maxp)) return(NA_real_)
    cv_wasserstein1(cv_heldout_nnd(xy, folds), Gij)
  }, numeric(1))
  list(qs = qs, Ws = Ws)
}

# Far-apart sub-grids split the residual Moran neighbour graph into one
# component per sub-grid, which spdep reports; that is the layout, not a fault.
# Every other warning still reaches the reporter.
without_subgraph_notice <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("sub-graphs", conditionMessage(w), fixed = TRUE)) invokeRestart("muffleWarning")
  })
}

# ── T1 ───────────────────────────────────────────────────────────────────────
test_that("cv_wasserstein1 is the Wasserstein-1 distance between the empirical distributions", {
  # By hand: F(0,1) - F(0.5) is 1/2 on [0, 0.5) and -1/2 on [0.5, 1).
  expect_equal(cv_wasserstein1(c(0, 1), 0.5), 0.5)
  a <- c(0.3, 1.7, 2.2, 5, 8.1)
  b <- c(1, 1, 4, 4.5, 9)
  # Equal sizes: the mean gap between the sorted samples (the quantile form).
  expect_equal(cv_wasserstein1(a, b), mean(abs(sort(a) - sort(b))))
  expect_equal(cv_wasserstein1(a, b), cv_wasserstein1(b, a))
  u <- c(2, 7, 7, 11)
  expect_equal(cv_wasserstein1(a, u), cv_wasserstein1(u, a))
  # A shift moves the whole distribution by its size.
  expect_equal(cv_wasserstein1(a, a + 2.5), 2.5)
  expect_equal(cv_wasserstein1(a, a - 0.4), 0.4)
  expect_equal(cv_wasserstein1(a, rev(a)), 0)
})

# ── T2 ───────────────────────────────────────────────────────────────────────
test_that("cv_heldout_nnd is each sample's distance to the nearest sample outside its fold", {
  xy <- sf::st_coordinates(golden_sf("core"))[, 1:2]
  n <- nrow(xy)
  folds <- rep_len(1:10, n)
  d <- as.matrix(stats::dist(xy))
  brute <- vapply(seq_len(n), function(i) min(d[i, folds != folds[i]]), numeric(1))
  expect_equal(cv_heldout_nnd(xy, folds), brute)
  # Leave-one-out: the nearest other sample.
  loo <- vapply(seq_len(n), function(i) min(d[i, -i]), numeric(1))
  expect_equal(cv_heldout_nnd(xy, seq_len(n)), loo)
  expect_equal(cv_heldout_nnd(xy, seq_len(n)), FNN::get.knn(xy, k = 1)$nn.dist[, 1])
})

# ── T3 ───────────────────────────────────────────────────────────────────────
test_that("the map's locations are a lattice inside the boundary, independent of the grid", {
  hexagon <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
    c(0, 500), c(400, 0), c(1300, 0), c(1700, 500), c(1300, 1000), c(400, 1000), c(0, 500)))),
    crs = 32633))
  D <- knndm_domain_points(hexagon)
  inside <- lengths(sf::st_intersects(sf::st_as_sf(as.data.frame(D), coords = 1:2, crs = 32633),
                                      hexagon)) > 0
  expect_true(all(inside))
  expect_gte(nrow(D), KNNDM_DOMAIN_N / 2)
  expect_lte(nrow(D), 2 * KNNDM_DOMAIN_N)
  expect_identical(knndm_domain_points(hexagon), D)
  # Square lattice: every coordinate step is the same spacing, sqrt(area / 1000).
  s <- sqrt(as.numeric(sf::st_area(hexagon)) / KNNDM_DOMAIN_N)
  expect_equal(min(diff(sort(unique(round(D[, 1], 6))))), s, tolerance = 1e-6)

  # The grid is read only without a usable boundary: two resolutions, one lattice.
  g1 <- as.matrix(expand.grid(seq(5, 1695, by = 10), seq(5, 995, by = 10)))
  g2 <- as.matrix(expand.grid(seq(20, 1680, by = 40), seq(20, 980, by = 40)))
  expect_identical(knndm_domain_points(hexagon, grid_xy = g1), D)
  expect_identical(knndm_domain_points(hexagon, grid_xy = g2), D)

  # A strip narrower than the spacing keeps its midline.
  strip <- rect_sf(0, 0, 1, 1000)
  Ds <- knndm_domain_points(strip)
  expect_gt(nrow(Ds), 0)
  expect_true(all(Ds[, 1] > 0 & Ds[, 1] < 1))

  expect_null(knndm_domain_points())
  expect_null(knndm_domain_points(NULL, NULL))

  # Without a boundary, a regular thinning of the grid that keeps both ends.
  thin <- knndm_domain_points(grid_xy = g1)
  expect_lte(nrow(thin), KNNDM_DOMAIN_N)
  expect_equal(thin[1, ], unname(g1[1, ]))
  expect_equal(thin[nrow(thin), ], unname(g1[nrow(g1), ]))
})

# ── T4 ───────────────────────────────────────────────────────────────────────
test_that("where random folds already match the map, kNNDM returns Auto's random folds", {
  xy <- lattice_xy()
  n <- nrow(xy)
  hull <- sf::st_sf(geometry = sf::st_convex_hull(sf::st_union(sf::st_geometry(as_pts(xy)))))
  D <- knndm_domain_points(hull)
  p <- suppressWarnings(stats::ks.test(nn_dist(xy), map_dist(xy, D), alternative = "greater")$p.value)
  expect_gte(p, KNNDM_KS_ALPHA)
  f <- knndm_folds(xy, D)
  info <- attr(f, "knndm")
  expect_identical(info$branch, "random")
  expect_equal(info$ks_p, p)
  expect_identical(as.integer(f), make_cv_folds(xy, "auto", n))
  expect_identical(as.integer(f), random_folds_by_definition(n))
  expect_identical(as.integer(make_cv_folds(xy, "knndm", n, domain_xy = D)), as.integer(f))
})

# ── T5 / T6 ──────────────────────────────────────────────────────────────────
test_that("clustered samples under a wide map get spatial folds at the minimum W", {
  xy <- subgrids_xy()
  n <- nrow(xy)
  D <- bbox_domain(xy)
  Gij <- map_dist(xy, D)
  p <- suppressWarnings(stats::ks.test(nn_dist(xy), Gij, alternative = "greater")$p.value)
  expect_lt(p, KNNDM_KS_ALPHA)

  f <- knndm_folds(xy, D)
  info <- attr(f, "knndm")
  expect_identical(info$branch, "spatial")
  expect_true(all(f %in% seq_len(10)))
  expect_lte(max(tabulate(f)), KNNDM_MAXP * n)
  W <- cv_wasserstein1(cv_heldout_nnd(xy, f), Gij)
  W_random <- cv_wasserstein1(cv_heldout_nnd(xy, random_folds_by_definition(n)), Gij)
  expect_equal(info$W, W)
  expect_equal(info$W_random, W_random)
  expect_lte(W, W_random)

  # T6: the independent search scores every candidate cut; kNNDM keeps the
  # first minimum, which beats the random partition here.
  cand <- independent_candidates(xy, Gij)
  expect_lt(min(cand$Ws, na.rm = TRUE), W_random)
  expect_equal(info$W, min(cand$Ws, na.rm = TRUE))
  expect_identical(info$q, cand$qs[which.min(cand$Ws)])
})

test_that("where no cluster partition beats random folds, kNNDM keeps the random folds", {
  # The compact locality under the buffered boundary an Ordinary Kriging run draws (the
  # concave hull widened by the dynamic buffer, 3 x half the mean
  # nearest-neighbour spacing): the gate only just rejects, and every cut of
  # the published candidate set matches the map worse than random folds do.
  pts <- golden_sf("full", localities = golden_case("knndm_random")$locality)
  xy <- sf::st_coordinates(pts)[, 1:2]
  n <- nrow(xy)
  hull <- sf::st_geometry(concaveman::concaveman(pts))
  wrapped <- sf::st_sf(geometry = sf::st_buffer(hull, 1.5 * mean(nn_dist(xy))))
  D <- knndm_domain_points(wrapped)
  Gij <- map_dist(xy, D)
  p <- suppressWarnings(stats::ks.test(nn_dist(xy), Gij, alternative = "greater")$p.value)
  expect_lt(p, KNNDM_KS_ALPHA)
  W_random <- cv_wasserstein1(cv_heldout_nnd(xy, random_folds_by_definition(n)), Gij)
  cand <- independent_candidates(xy, Gij)
  expect_gt(min(cand$Ws, na.rm = TRUE), W_random)

  f <- knndm_folds(xy, D)
  info <- attr(f, "knndm")
  expect_identical(info$branch, "random")
  expect_identical(as.integer(f), random_folds_by_definition(n))
  expect_equal(info$W, W_random)
  expect_equal(info$ks_p, p)
  expect_match(knndm_log_line(info, "Kale", "Actual", n, "m"),
               "kNNDM chose random folds; they matched the map's distances better than any spatial partition")
})

# ── T7 ───────────────────────────────────────────────────────────────────────
test_that("kNNDM is deterministic and leaves the caller's RNG alone", {
  xy <- subgrids_xy()
  D <- bbox_domain(xy)
  withr::local_seed(99)
  before <- .Random.seed
  a <- knndm_folds(xy, D)
  expect_identical(.Random.seed, before)
  expect_identical(knndm_folds(xy, D), a)
  # Spatial folds draw no random numbers: the seed moves nothing but the
  # random reference.
  expect_identical(as.integer(knndm_folds(xy, D, seed = 1L)), as.integer(knndm_folds(xy, D, seed = 2L)))
})

# ── T8 ───────────────────────────────────────────────────────────────────────
test_that("kNNDM falls back to LOOCV below 30 samples and to random folds without a partition", {
  xy25 <- subgrids_xy(nx = 1, ny = 1)
  expect_equal(nrow(xy25), 25L)
  expect_identical(make_cv_folds(xy25, "knndm", 25, domain_xy = bbox_domain(xy25)), seq_len(25))
  plan <- resolve_cv_plan("knndm", 25)
  expect_identical(plan$type, "loocv")
  expect_identical(plan$label, "LOOCV [kNNDM needs n ≥ 30]")
  expect_identical(resolve_cv_plan("knndm", 30)$type, "knndm")

  xy <- subgrids_xy()
  n <- nrow(xy)
  none <- knndm_folds(xy, NULL)
  expect_identical(attr(none, "knndm")$branch, "none")
  expect_identical(as.integer(none), random_folds_by_definition(n))

  # Every partition into 10 folds has a fold of at least 10% of the samples,
  # so a 5% cap admits none: the branch is reachable through its parameters
  # only (at the default cap the finest cut is always balanced).
  fb <- knndm_folds(xy, bbox_domain(xy), maxp = 0.05)
  expect_identical(attr(fb, "knndm")$branch, "fallback")
  expect_identical(as.integer(fb), random_folds_by_definition(n))
})

# ── T9 ───────────────────────────────────────────────────────────────────────
test_that("the first principal axis has a fixed sign", {
  xy <- sf::st_coordinates(golden_sf("core"))[, 1:2]
  ax <- .knndm_pc_axis(xy)$axis
  ref <- unname(stats::prcomp(xy, center = TRUE, scale. = FALSE)$rotation[, 1])
  expect_true(isTRUE(all.equal(ax, ref)) || isTRUE(all.equal(ax, -ref)))
  expect_gt(ax[which.max(abs(ax))], 0)
  # A reflected layout has the same axis, so the same fold order.
  expect_equal(.knndm_pc_axis(-xy)$axis, ax)
})

# ── T10 ──────────────────────────────────────────────────────────────────────
test_that("above the exact limit the tree is built on at most KNNDM_MAX_CELLS cells", {
  xy <- subgrids_xy(nx = 10, ny = 5, gap = c(1000, 1000), side = c(10, 5), step = 5)
  n <- nrow(xy)
  expect_equal(n, 2500L)
  D <- bbox_domain(xy)
  f <- knndm_folds(xy, D)
  info <- attr(f, "knndm")
  expect_false(info$exact)
  expect_lte(info$units, KNNDM_MAX_CELLS)
  expect_length(f, n)
  expect_true(all(f %in% seq_len(10)))
  expect_lte(max(tabulate(f)), KNNDM_MAXP * n)
  Gij <- map_dist(xy, D)
  expect_lte(cv_wasserstein1(cv_heldout_nnd(xy, f), Gij),
             cv_wasserstein1(cv_heldout_nnd(xy, random_folds_by_definition(n)), Gij))

  small <- subgrids_xy()
  forced <- attr(knndm_folds(small, bbox_domain(small), exact_max = 10L), "knndm")
  expect_false(forced$exact)

  # Every sample in one cell: no cut to score, so the random partition stands
  # alone (the tree is not built).
  same <- matrix(5, 2100, 2)
  one_cell <- knndm_folds(same, bbox_domain(rbind(same, c(15, 15))), exact_max = 2000L)
  expect_false(attr(one_cell, "knndm")$exact)
  expect_identical(attr(one_cell, "knndm")$units, 1L)
  expect_identical(attr(one_cell, "knndm")$branch, "random")
  expect_identical(as.integer(one_cell), random_folds_by_definition(2100))
})

# ── T11 ──────────────────────────────────────────────────────────────────────
test_that("a size-weighted tree on group centroids reproduces Ward's merges above the groups", {
  # hclust's ward.D2 carries d^2(A, B) = 2 nA nB / (nA + nB) |cA - cB|^2
  # (Lance & Williams 1967), so feeding those distances with the group sizes
  # as members must give back every merge above the group level.
  xy <- sf::st_coordinates(golden_sf("core"))[, 1:2]
  hc <- stats::hclust(stats::dist(xy), method = "ward.D2")
  groups <- stats::cutree(hc, k = 30)
  cnt <- tabulate(groups)
  cen <- rowsum(xy, groups) / cnt
  D <- as.matrix(stats::dist(cen)) * sqrt(2 * outer(cnt, cnt) / outer(cnt, cnt, "+"))
  hc2 <- stats::hclust(stats::as.dist(D), method = "ward.D2", members = cnt)
  canon <- function(p) match(p, unique(p))
  for (q in 2:29) {
    expect_identical(canon(stats::cutree(hc2, k = q)[groups]), canon(stats::cutree(hc, k = q)), info = q)
  }
  expect_equal(hc2$height, utils::tail(hc$height, 29), tolerance = 1e-9)
  # Plain centroid distances are not the same tree.
  hc3 <- stats::hclust(stats::dist(cen), method = "ward.D2", members = cnt)
  expect_false(isTRUE(all.equal(hc3$height, utils::tail(hc$height, 29))))
  # The app builds the same dissimilarities in the distance object itself,
  # without the U x U matrices: the identical tree.
  hc4 <- .knndm_cell_tree(cen, cnt)
  expect_identical(hc4$merge, hc2$merge)
  expect_identical(hc4$height, hc2$height)
})

# ── T12 ──────────────────────────────────────────────────────────────────────
test_that("build_cv_plan keeps one realization for spatial folds and the Auto repeats otherwise", {
  xy <- subgrids_xy()
  D <- bbox_domain(xy)
  plan <- build_cv_plan(as_pts(xy), "knndm", repeats = 5, domain_xy = D)
  expect_length(plan$folds, 1)
  expect_identical(attr(plan$folds[[1]], "knndm")$branch, "spatial")
  expect_equal(plan$design$W_cv, attr(plan$folds[[1]], "knndm")$W)
  expect_identical(plan$design$n, nrow(xy))

  lat <- lattice_xy()
  hull <- sf::st_sf(geometry = sf::st_convex_hull(sf::st_union(sf::st_geometry(as_pts(lat)))))
  Dl <- knndm_domain_points(hull)
  kn <- build_cv_plan(as_pts(lat), "knndm", repeats = 5, domain_xy = Dl)
  auto <- build_cv_plan(as_pts(lat), "auto", repeats = 5)
  expect_length(kn$folds, 5)
  for (r in 1:5) expect_identical(as.integer(kn$folds[[r]]), auto$folds[[r]], info = r)
  expect_false(is.null(kn$design))
  # No domain, no distance record; the folds are unchanged.
  expect_null(auto$design)
})

# ── T13 ──────────────────────────────────────────────────────────────────────
test_that("a kNNDM row is labelled by the design it chose, and spatial folds are read as contiguous", {
  pl <- function(branch) applied_cv_plan(100, "knndm", list(knndm_branch = branch))
  expect_identical(pl("random")[c("type", "label")], list(type = "knndm_random", label = "kNNDM CV [random folds]"))
  expect_identical(pl("spatial")[c("type", "label")], list(type = "knndm_spatial", label = "kNNDM CV [spatial folds]"))
  expect_identical(pl("fallback")$type, "random_kfold")
  expect_identical(pl("fallback")$label, "Random 10-fold CV [kNNDM: no valid spatial partition]")
  expect_identical(pl("none")$type, "random_kfold")
  expect_identical(pl("none")$label, "Random 10-fold CV [kNNDM: no prediction domain]")
  expect_identical(applied_cv_plan(100, "knndm", NULL)$type, "knndm")
  expect_identical(applied_cv_plan(20, "knndm", list(knndm_branch = "spatial"))$label,
                   "LOOCV [kNNDM needs n ≥ 30]")

  sp <- moran_reading("knndm_spatial")
  expect_true(sp$block)
  expect_false(sp$report_p)
  expect_identical(sp$label, "Spatial-fold residual clustering")
  expect_identical(sp$design, "kNNDM spatial folds")
  expect_match(sp$context, "p not reported")
  rnd <- moran_reading("knndm_random")
  expect_false(rnd$block)
  expect_true(rnd$report_p)
  mixed <- moran_reading(c("knndm_spatial", "knndm_random"))
  expect_true(mixed$mixed)
  expect_false(mixed$block)
  expect_true(mixed$report_p)
  blk <- moran_reading("block")
  expect_identical(blk$label, "Block-CV residual clustering")
  expect_identical(blk$design, "Spatial Block CV")
  expect_false(blk$report_p)
  # perform_cv carries the branch the folds recorded, NA without one.
  expect_true(is.na(perform_cv(NULL)$knndm_branch))
})

# ── T14 ──────────────────────────────────────────────────────────────────────
test_that("repeated CV keeps one realization for kNNDM spatial folds and repeats random ones", {
  xy <- lattice_xy()[1:60, ]
  cv <- sf::st_as_sf(data.frame(x = xy[, 1], y = xy[, 2], observed = xy[, 1] / 100,
                                var1.pred = xy[, 1] / 100 + 0.1),
                     coords = c("x", "y"), crs = 32633)
  mp <- list(cv_repeats = 5, cv_strategy = "knndm")
  res <- init_interpolation_res()
  res$cv_obj <- cv
  attr(res$cv_obj, "knndm") <- list(branch = "spatial")
  out <- add_cv_repeats(res, function(seed) stop("a deterministic partition must not be repeated"),
                        mp, 60, "OK", "Loc", "act")
  expect_null(out$cv_obj_reps)
  expect_match(out$log_msg, "OK, Loc \\(Actual\\): kNNDM spatial folds are deterministic; one realization\\.")

  attr(res$cv_obj, "knndm") <- list(branch = "random")
  seen <- integer(0)
  out2 <- add_cv_repeats(res, function(seed) { seen <<- c(seen, seed); cv }, mp, 60, "OK", "Loc", "act")
  expect_length(out2$cv_obj_reps, 5)
  expect_identical(seen, CV_FOLD_SEED + 1:4)
})

# ── T15 ──────────────────────────────────────────────────────────────────────
test_that("every engine folds under kNNDM, IDW's nested selection included", {
  xy <- subgrids_xy()
  n <- nrow(xy)
  v <- 5 + sin(xy[, 1] / 1500) + cos(xy[, 2] / 900) + 0.002 * (xy[, 1] %% 97)
  pts <- as_pts(xy, v = v)
  D <- bbox_domain(xy)
  grid <- make_test_grid_safe(pts, res = 500)
  ref <- make_cv_folds(sf::st_coordinates(pts), "knndm", n, CV_FOLD_SEED, D)

  idw <- without_subgraph_notice(apply_IDW(
    pts, "v", grid, list(cv_strategy = "knndm", cv_domain_xy = D, idw_p = -1, idw_nmax = 12), "Loc", "act"))
  expect_identical(idw$cv_metrics$knndm_branch, "spatial")
  expect_identical(as.integer(idw$cv_obj$fold), as.integer(ref))
  fold_p <- idw$idw_fit$fold_p
  co <- sf::st_coordinates(pts)
  # The map's power is the selection on all rows (whose folds are the
  # reference CV's).
  expect_identical(idw$idw_fit$p, select_idw_power(co, v, "knndm", 12, domain_xy = D)$p)
  for (j in names(fold_p)) {
    train <- which(ref != as.integer(j))
    expect_identical(unname(fold_p[[j]]),
                     select_idw_power(co[train, , drop = FALSE], v[train], "knndm", 12, domain_xy = D)$p,
                     info = j)
  }
  expect_equal(idw$cv_design$W_cv, attr(ref, "knndm")$W)
  expect_identical(idw$cv_design$units, "m")

  lags <- calc_scientific_lags(pts)
  ok <- suppressWarnings(apply_OK(pts, "v", grid, lags, list(cv_strategy = "knndm", cv_domain_xy = D), "Loc", "act"))
  expect_identical(ok$cv_metrics$knndm_branch, "spatial")
  expect_equal(ok$cv_design$W_cv, attr(ref, "knndm")$W)
  tps <- suppressWarnings(apply_TPS(pts, "v", grid, list(cv_strategy = "knndm", cv_domain_xy = D), "Loc", "act"))
  expect_false(is.null(tps$tps_fit))
  expect_identical(tps$cv_metrics$knndm_branch, "spatial")
  expect_equal(tps$cv_design$W_cv, attr(ref, "knndm")$W)

  # The dispatcher supplies the map's locations when the caller gives none: a
  # thinning of the prediction grid.
  disp <- without_subgraph_notice(apply_interpolation(
    pts, "v", "IDW", grid, character(0), NULL, list(cv_strategy = "knndm", idw_p = 2, idw_nmax = 12),
    "Loc", "act"))
  expect_false(is.na(disp$cv_metrics$knndm_branch))
  expect_identical(disp$cv_design$n_domain, nrow(knndm_domain_points(grid_xy = sf::st_coordinates(grid))))
})

test_that("the regional driver runs kNNDM against its boundary and logs the design", {
  pts <- golden_sf("core", localities = golden_locality("core", "largest", min_n = 30L))
  co <- sf::st_coordinates(pts)
  pts_data <- data.frame(x = co[, 1], y = co[, 2], v = pts$ph, pv = NA_real_, Locality = "golden")
  item <- list(l = "golden", pts_data = pts_data,
               m_params = list(idw_p_act = 2, idw_p_pre = 2, idw_nmax = 12,
                               tps_lambda_act = -1, tps_lambda_pre = -1,
                               pre_fit_act = golden_pin_vgm(), pre_fit_pre = NULL,
                               cv_strategy = "knndm", rfk_uncertainty = "jackknife"))
  crs <- golden_meta()$crs
  res <- suppressWarnings(run_regional_interpolation(
    item, "OK", crs, character(0), golden_pin_boundary(pts), "wrapped", "fixed", 300,
    "fixed", 300, paste0("EPSG:", crs), FALSE, "actual"))
  expect_false(is.null(res$cv_design_act))
  expect_identical(res$cv_design_act$n, nrow(pts))
  expect_true(res$cv_act$knndm_branch %in% c("random", "spatial"))
  expect_match(res$log_msg, "\\[CV\\] golden \\(Actual\\): kNNDM chose (random|spatial) folds")
  # The lattice comes from the boundary, whatever the cell size.
  res2 <- suppressWarnings(run_regional_interpolation(
    item, "OK", crs, character(0), golden_pin_boundary(pts), "wrapped", "fixed", 300,
    "fixed", 150, paste0("EPSG:", crs), FALSE, "actual"))
  expect_identical(res2$cv_design_act$map, res$cv_design_act$map)
  expect_identical(res2$cv_act$knndm_branch, res$cv_act$knndm_branch)
})

test_that("the CV Distance Match panel draws every design and states its absence", {
  xy <- subgrids_xy()
  D <- bbox_domain(xy)
  f <- knndm_folds(xy, D)
  des <- cv_distance_summary(xy, f, D, units = "m")
  expect_named(des, c("probs", "sample", "map", "cv", "W_cv", "W_random", "n", "n_domain", "k",
                      "reference", "units"))
  expect_equal(des$cv[51], unname(stats::quantile(cv_heldout_nnd(xy, f), 0.5)))
  expect_equal(des$W_cv, cv_wasserstein1(cv_heldout_nnd(xy, f), map_dist(xy, D)))
  expect_equal(des$W_random, cv_wasserstein1(cv_heldout_nnd(xy, random_folds_by_definition(nrow(xy))),
                                             map_dist(xy, D)))
  expect_identical(des$reference, "random 10-fold")
  expect_null(cv_distance_summary(xy, f, NULL))
  expect_null(cv_distance_summary(xy[1:2, ], f[1:2], D))
  # A supplied reference partition replaces the seeded random one, and is named.
  alt <- rep_len(1:5, nrow(xy))
  des5 <- cv_distance_summary(xy, f, D, units = "m", random_folds = alt, reference = "every fifth sample")
  expect_equal(des5$W_random, cv_wasserstein1(cv_heldout_nnd(xy, alt), map_dist(xy, D)))
  expect_identical(des5$k, 5L)
  expect_identical(des5$reference, "every fifth sample")
  expect_match(build_cv_distance_plot(des5)$labels$subtitle, "W, every fifth sample: ")
  expect_true("W, every fifth sample (m)" %in% names(cv_distance_export_df(des5)))

  single <- build_cv_distance_plot(des, title = "t")
  expect_s3_class(single, "ggplot")
  expect_match(single$labels$subtitle, "W, this CV: .* · W, random 10-fold: .* \\(m\\); n = 150 samples")
  both <- build_cv_distance_plot(list(Actual = des, Predicted = des))
  expect_s3_class(both$facet, "FacetWrap")
  expect_s3_class(build_cv_distance_plot(NULL), "ggplot")

  df <- cv_distance_export_df(des)
  expect_equal(nrow(df), 101)
  expect_equal(df[["W, this CV (m)"]][1], des$W_cv)
  expect_equal(df[["W, random 10-fold (m)"]][1], des$W_random)
  expect_null(cv_distance_export_df(NULL))
})

test_that("a supplied random partition is kNNDM's candidate and its random outcome", {
  # Clustered samples under a wide map: spatial folds beat any random
  # partition, which the record still scores.
  xy <- subgrids_xy()
  n <- nrow(xy)
  D <- bbox_domain(xy)
  Gij <- map_dist(xy, D)
  alt <- rep_len(c(3L, 1L, 4L, 2L, 5L, 9L, 6L, 10L, 8L, 7L), n)
  sp <- attr(knndm_folds(xy, D, random_folds = alt), "knndm")
  expect_identical(sp$branch, "spatial")
  expect_equal(sp$W_random, cv_wasserstein1(cv_heldout_nnd(xy, alt), Gij))
  # A lattice under its hull: the gate passes, and the folds are the supplied
  # partition itself, scored as such.
  lat <- lattice_xy()
  hull <- sf::st_sf(geometry = sf::st_convex_hull(sf::st_union(sf::st_geometry(as_pts(lat)))))
  Dl <- knndm_domain_points(hull)
  alt_l <- rep_len(10:1, nrow(lat))
  f <- knndm_folds(lat, Dl, random_folds = alt_l)
  expect_identical(attr(f, "knndm")$branch, "random")
  expect_identical(as.integer(f), alt_l)
  expect_equal(attr(f, "knndm")$W, cv_wasserstein1(cv_heldout_nnd(lat, alt_l), map_dist(lat, Dl)))
  # No domain: the supplied partition too.
  expect_identical(as.integer(knndm_folds(lat, NULL, random_folds = alt_l)), alt_l)
  expect_error(knndm_folds(lat, Dl, random_folds = alt_l[-1]), "fold ids for")
})

test_that("a kNNDM run record carries the settings its folds were built with", {
  cfg <- list(method = "OK", cv_strategy = "knndm", cv_repeats = 1L, cv_population = "Native")
  rec <- run_record_payload(cfg, list(), "test", list())
  expect_identical(names(rec$config)[match("cv_repeats", names(rec$config)) + 1L], "cv_knndm")
  expect_identical(rec$config$cv_knndm[c("k", "maxp", "min_n", "domain_points", "exact_max", "max_cells")],
                   list(k = CV_FOLD_K, maxp = KNNDM_MAXP, min_n = CV_KNNDM_MIN_N,
                        domain_points = KNNDM_DOMAIN_N, exact_max = KNNDM_EXACT_MAX,
                        max_cells = KNNDM_MAX_CELLS))
  expect_null(run_record_payload(modifyList(cfg, list(cv_strategy = "auto")), list(), "test", list())$config$cv_knndm)
  expect_match(run_record_json(rec), "\"cv_knndm\"")
})
