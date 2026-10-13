# The golden fixture's contract.
#
# REQUIREMENTS are hardcoded: the properties the rest of the suite depends on
# (projected metres, no missing values, a severely collinear covariate pair, a
# rare class, enough points per scope). Any golden set, including one substituted
# for the shipped survey, must meet them, or the tests needing them test nothing.
#
# IDENTITY comes from the fixture's own baselines (row counts, locality sizes,
# coordinate ranges), recorded by make_baselines.R. It fails loudly when the
# fixture is refreshed, so a refresh is explained rather than absorbed.

# ── Requirements every golden set must meet ────────────────────────────────

test_that("property-based golden selectors are invariant to locality names", {
  for (scope in c("full", "core", "tiny")) {
    d <- golden_soil(scope)
    names_map <- setNames(paste0("Renamed site ", seq_along(unique(d$locality))), unique(d$locality))
    renamed <- d
    renamed$locality <- unname(names_map[d$locality])
    for (property in c("smallest", "largest", "compact")) {
      original <- golden_locality(scope, property, data = d)
      changed <- golden_locality(scope, property, data = renamed)
      expect_identical(changed, unname(names_map[original]))
      expect_identical(d$sample_no[d$locality == original], renamed$sample_no[renamed$locality == changed])
    }
  }
  expect_error(golden_locality("tiny", min_n = nrow(golden_soil("full")) + 1L), "at least")
  compact <- golden_locality("full", "compact")
  expect_false(identical(golden_locality("full", "compact", exclude = compact), compact))
})

test_that("the fixture generator maps ordered role vectors and derives named scopes", {
  generator <- new.env(parent = globalenv())
  source(file.path(proj_root, "tests", "testthat", "fixtures", "make_golden.R"), local = generator)
  withr::local_dir(proj_root)
  d <- golden_soil("full")
  sites <- unique(d$locality)
  d$locality <- paste0("Site", match(d$locality, sites))
  names(d)[names(d) == "ph"] <- "Laboratory pH"
  names(d)[names(d) == "som"] <- "Carbon content"
  roles <- generator$.golden_default_roles()
  roles$soil[roles$soil == "ph"] <- "Laboratory pH"
  roles$soil[roles$soil == "som"] <- "Carbon content"
  roles$crs <- golden_meta()$crs
  tmp <- withr::local_tempdir()
  input <- file.path(tmp, "source.xlsx")
  openxlsx::write.xlsx(d, input)
  out <- file.path(tmp, "fixture")
  generator$make_golden(input, src_meta = NULL, out_dir = out, roles = roles)
  mapped <- readRDS(file.path(out, "golden_soil.rds"))
  idx <- match(mapped$sample_no, d$sample_no)
  expect_equal(mapped$ph, d[["Laboratory pH"]][idx], tolerance = 1e-12)
  expect_equal(mapped$som, d[["Carbon content"]][idx], tolerance = 1e-12)
  meta <- readRDS(file.path(out, "golden_meta.rds"))
  expect_true(all(names(meta$scopes$core) %in% d$locality))
  expect_true(all(names(meta$scopes$tiny) %in% d$locality))
  expect_identical(meta$scopes, generator$.golden_default_scopes(mapped))
  expect_error(generator$make_golden(input, out_dir = out, roles = list(target = "Laboratory pH")), "Unknown roles")
})

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
  # Asserted as "nothing is unlabelled" rather than as a count, so a measured
  # column added without a dictionary row fails here.
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

# ── The environment the baselines were recorded in ──────────────────────────

test_that("the baselines record the environment they were taken in", {
  prov <- golden_provenance()
  skip_if(is.null(prov), "no baseline provenance recorded for this golden set")
  skip_if(monolith_unpinned_run(), "deliberately unpinned run (upstream drift job)")

  expect_true(all(c("r_version", "platform", "packages") %in% names(prov)))
  # Every package that can move one of the four recorded values must be named,
  # or the record is incomplete in exactly the way that sends a reader bisecting.
  expect_true(all(GOLDEN_BASELINE_PKGS %in% names(prov$packages)))
  expect_false(any(is.na(prov$packages)))

  # A failure, not a note: with gstat/sf/terra/Ckmeans.1d.dp differing from the
  # recording session, a moved value cannot be told from an upstream change.
  # Restore the recorded versions, or re-record after confirming the values did
  # not move. make_baselines.R sets the bypass while it runs the gate, so this
  # test skips there instead of deadlocking it.
  expect_equal(golden_provenance_drift(prov), character(0),
               info = golden_baseline_info())
})

test_that("this session's packages are the versions renv.lock pins", {
  drift <- renv_lock_drift()
  skip_if(is.null(drift), "renv.lock is not readable from here")
  skip_if(monolith_unpinned_run(), "deliberately unpinned run (upstream drift job)")

  # A SKIP, not a failure: renv is not activated by design, so a deliberate local
  # upgrade is a choice, not a defect. "FAIL 0 | SKIP 0" then also means "this is
  # the environment renv.lock describes"; the test above is the one that fails
  # when a baseline package differs from the recording session.
  if (length(drift)) {
    skip(paste0("session differs from renv.lock (renv::restore() realigns it): ",
                paste(drift, collapse = "; ")))
  }
  expect_equal(drift, character(0))
})

test_that("a missing package never installs without an explicit yes", {
  # install.packages() fetches whatever CRAN publishes today, the one action that
  # can move a recorded value with nothing in the repository changed. Refusing to
  # install would push users into source builds of superseded locked versions, so
  # global.R's block owes three properties instead:
  #   (a) it never installs without an explicit yes,
  #   (b) it never prompts non-interactively (helper.R sources global.R, and a
  #       prompt would hang the suite),
  #   (c) it names both routes, so the choice is informed.
  # Asserted by EVALUATING the block against a package that cannot exist; a
  # source-text match would also pass on an unreachable branch.
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  src <- readLines(file.path(root, "global.R"), warn = FALSE)
  i <- grep("if (length(missing_packages) > 0) {", src, fixed = TRUE)
  expect_length(i, 1)
  j <- i + which(src[(i + 1):length(src)] == "}")[1]
  blk <- parse(text = paste(src[i:j], collapse = "\n"))

  # `interactive()` and `utils::menu()` are stubbed through the evaluation
  # environment. menu() is reached as `utils::menu`, so the `::` operator itself
  # is the binding to intercept; everything else it is asked for is forwarded.
  mk_env <- function(interactive_val, menu_answer = 1L) {
    e <- new.env(parent = globalenv())
    e$missing_packages <- "definitely.not.a.real.package"
    e$install.packages <- function(...) stop("<<installed>>", call. = FALSE)
    e$interactive <- function() interactive_val
    e$`::` <- function(pkg, name) {
      p <- as.character(substitute(pkg)); n <- as.character(substitute(name))
      if (p == "utils" && n == "menu") return(function(...) menu_answer)
      get(n, envir = asNamespace(p))
    }
    e
  }

  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  # MONOLITH_ALLOW_LATEST is forced off here because the upstream-drift job sets
  # it globally.
  withr::with_envvar(c(MONOLITH_ALLOW_LATEST = "false"), {
    # (b) non-interactive: stops, never prompts, never installs.
    msg <- tryCatch(eval(blk, mk_env(FALSE)), error = conditionMessage)
    expect_false(grepl("<<installed>>", msg, fixed = TRUE))
    # (c) renv.lock is present at the root, so both routes are named.
    expect_match(msg, "install.packages(c(", fixed = TRUE)
    expect_match(msg, "renv::restore()", fixed = TRUE)

    # (a) interactive and declined: still nothing installed.
    expect_error(eval(blk, mk_env(TRUE, menu_answer = 2L)), "Cancelled", fixed = TRUE)
    # interactive and accepted: the install proceeds.
    expect_error(eval(blk, mk_env(TRUE, menu_answer = 1L)), "<<installed>>", fixed = TRUE)
  })

  # MONOLITH_ALLOW_LATEST is the upstream-drift CI job's declared opt-in: it
  # installs with no question asked, because that job exists to run on newer
  # versions on a machine no one is sitting at.
  withr::with_envvar(c(MONOLITH_ALLOW_LATEST = "true"), {
    expect_error(eval(blk, mk_env(FALSE)), "<<installed>>", fixed = TRUE)
  })
})
