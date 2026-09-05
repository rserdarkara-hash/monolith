# The golden fixture's contract.
#
# Two kinds of assertion live here, and the difference matters.
#
# REQUIREMENTS are hardcoded. They are the properties the rest of the suite
# depends on: projected metres, no missing values, a severely collinear
# covariate pair, a rare class, enough points per scope. Any golden set must
# meet them, including one you substitute for the shipped survey. If your data
# fails here, the tests that need those properties would not have been testing
# anything.
#
# IDENTITY comes from the fixture's own baseline file. Row counts, locality
# sizes, coordinate ranges: facts about this particular survey, recorded by
# make_baselines.R. They fail loudly if the fixture is refreshed, which is the
# point - a refresh should be explained, not absorbed.

# ── Requirements every golden set must meet ────────────────────────────────

test_that("the golden table has the canonical columns and no missing values", {
  gs <- golden_soil("full")
  meta <- golden_meta()
  expect_s3_class(gs, "data.frame")

  cols <- meta$columns
  expect_true(all(c(cols$keys, cols$coords, cols$soil, cols$pred,
                    cols$covariates) %in% names(gs)))
  expect_true(cols$target %in% names(gs))
  expect_true(cols$categorical %in% names(gs))
  expect_equal(sum(is.na(gs)), 0L)

  # `na` is a real column name in the survey (sodium). It is kept precisely
  # because a column named "na" is a hazard worth having in a fixture.
  expect_true("na" %in% names(gs))
  expect_true(is.numeric(gs$na))
})

test_that("the coordinates are projected metres", {
  pts <- golden_sf("full")
  crs <- sf::st_crs(pts)
  # Everything downstream - variogram ranges, buffers, grid resolution, nearest
  # neighbour spacing - is metres. A geographic golden set would silently make
  # every distance a degree.
  expect_false(is.na(crs))
  expect_match(crs$units_gdal, "metre")
  expect_true(all(is.finite(sf::st_coordinates(pts))))
})

test_that("the reduced scopes are deterministic subsets of the full table", {
  full <- golden_soil("full")
  core <- golden_soil("core")
  tiny <- golden_soil("tiny")
  scopes <- golden_meta()$scopes

  expect_setequal(unique(core$locality), names(scopes$core))
  expect_setequal(unique(tiny$locality), names(scopes$tiny))
  # The kriging, CV and classification tests need enough points to be
  # meaningful at all.
  expect_gte(nrow(core), 100L)
  expect_gte(nrow(tiny), 30L)
  expect_gte(length(unique(core$locality)), 2L)

  # Subsets, not resamples: every row is a row of the full table.
  expect_true(all(core$sample_no %in% full$sample_no))
  expect_true(all(tiny$sample_no %in% full$sample_no))
  expect_false(any(duplicated(core$sample_no)))

  # Deterministic: no RNG anywhere in the derivation.
  expect_identical(golden_soil("core"), core)
  set.seed(1); a <- golden_soil("core")
  set.seed(99); b <- golden_soil("core")
  expect_identical(a, b)
})

test_that("systematic thinning keeps the full spatial extent", {
  full <- golden_soil("full")
  core <- golden_soil("core")
  for (l in unique(core$locality)) {
    fx <- diff(range(full$x[full$locality == l]))
    cx <- diff(range(core$x[core$locality == l]))
    # A head()-style subset would collapse the extent; every-k-th does not.
    expect_gt(cx / fx, 0.85)
  }
})

test_that("golden_sf carries the fixture CRS and its coordinates round-trip", {
  pts <- golden_sf("tiny")
  expect_s3_class(pts, "sf")
  expect_equal(sf::st_crs(pts), sf::st_crs(golden_meta()$crs))
  co <- sf::st_coordinates(pts)
  expect_equal(unname(co[, "X"]), pts$x)
  expect_equal(unname(co[, "Y"]), pts$y)

  # Transforming to geographic gives plausible lon/lat.
  ll <- sf::st_coordinates(golden_sf("tiny", crs = 4326))
  expect_true(all(ll[, "X"] > -180 & ll[, "X"] < 180))
  expect_true(all(ll[, "Y"] > -90 & ll[, "Y"] < 90))
})

test_that("golden_sf filters by locality", {
  l <- names(golden_meta()$scopes$core)[1]
  pts <- golden_sf("core", localities = l)
  expect_setequal(unique(pts$locality), l)
  expect_lt(nrow(pts), nrow(golden_soil("core")))
})

test_that("the fixture carries the severe collinearity the VIF tests need", {
  gs <- golden_soil("full")
  cov <- golden_meta()$columns$covariates
  cm <- cor(gs[cov])
  diag(cm) <- 0
  # Without a genuinely collinear pair the VIF pruning tests would pass
  # vacuously: nothing would ever be dropped and the ranking would go unchecked.
  expect_gt(max(abs(cm)), 0.99)
  diag(cm) <- 1
  expect_gt(max(diag(solve(cm))), 100)
})

test_that("the fixture carries the class imbalance the adequacy test needs", {
  gs <- golden_soil("full")
  tt <- table(gs[[golden_meta()$columns$categorical]])
  expect_gte(length(tt), 3L)
  # A class below the adequacy floor, and a well-sampled one to contrast it.
  expect_lt(min(tt), 3L)
  expect_gt(max(tt), 100L)
})

test_that("the variable dictionary labels every measured column", {
  vl <- golden_varlist()
  skip_if(is.null(vl), "this golden set ships no variable dictionary")
  expect_named(vl, c("vn", "vid", "cat"))

  gs <- golden_soil("full")
  cols <- golden_meta()$columns
  measured <- c(cols$soil, cols$covariates)
  # Every measured column is labelled. Sodium was the one gap - the shipped
  # dictionary held 100 covariates + 16 soil variables against the data's 17 -
  # and it was closed in samp_var_list.xlsx on 2026-09-05. Asserted as
  # "nothing is unlabelled" rather than as a count, so a covariate added to the
  # data without a dictionary row fails here too.
  expect_equal(setdiff(measured, vl$vn), character(0))
})

# ── This fixture's recorded identity ───────────────────────────────────────

test_that("the fixture matches its recorded identity", {
  id <- golden_baseline("identity")
  skip_if(is.null(id), "no baselines recorded for this golden set")

  gs <- golden_soil("full")
  expect_equal(nrow(gs), id$n_rows)
  expect_equal(ncol(gs), id$n_cols)
  expect_equal(table(gs$locality), id$locality_counts)
  expect_equal(table(gs[[golden_meta()$columns$categorical]]), id$class_counts)
  expect_equal(range(gs$x), id$x_range)
  expect_equal(range(gs$y), id$y_range)
  expect_equal(nrow(golden_soil("core")), id$n_core)
  expect_equal(nrow(golden_soil("tiny")), id$n_tiny)
  expect_equal(golden_meta()$scopes, id$scopes)
  if (!is.null(id$varlist_dim)) expect_equal(dim(golden_varlist()), id$varlist_dim)
})
