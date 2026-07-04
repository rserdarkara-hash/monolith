# robust_vgm_fit Convergence Handling Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `robust_vgm_fit` detect non-converged/singular candidate variogram fits, prefer clean fits in selection, muffle the expected screening warnings (301 of the suite's 314), and surface flawed winners in the map banner.

**Architecture:** Harden `robust_vgm_fit` (monolith.R) in place: classify each of the 16 candidate fits as clean/flawed via `withCallingHandlers` + `attr(f, "singular")`, select clean-first, and attach a diagnostics attribute. A new pure helper `build_vgm_warning_html()` (ui_helpers.R) renders the red/amber banner HTML so it is unit-testable; `draw_map` calls it.

**Tech Stack:** R 4.5.2, gstat, sf, testthat (suite in `tests/testthat`, harness sources the whole app via `helper.R`).

**Spec:** `docs/superpowers/specs/2026-07-04-robust-vgm-fit-convergence-design.md`

## Global Constraints

- **No git commands** (CLAUDE.md): no `git add`/`commit` steps anywhere. Each task ends with a checkpoint that reports changed files; the user commits manually.
- **Worktree isolation** (CLAUDE.md Phase 2): before the first edit, create/enter an isolated worktree with the native `EnterWorktree` tool (per superpowers:using-git-worktrees). Do not use raw `git worktree` commands.
- **R is not on PATH.** Always use `"C:\Program Files\R\R-4.5.2\bin\Rscript.exe"`.
- **Single-file test loop:** `"C:\Program Files\R\R-4.5.2\bin\Rscript.exe" -e 'testthat::test_dir("tests/testthat", filter = "robust-vgm-fit")'` — never `test_file()` directly. Every filtered run re-sources the whole app (47 packages): 1–2 minutes is normal, not a hang.
- **Full suite:** `"C:\Program Files\R\R-4.5.2\bin\Rscript.exe" tests/testthat.R` (allow ≥ 5 minutes). Current baseline: `[ FAIL 0 | WARN 314 | SKIP 0 | PASS 638 ]`.
- **Screening-warning regex (exact):** `"No convergence after|singular model|singular covariance"` — all three patterns observed empirically from `gstat::fit.variogram`.
- **Behavior change is sanctioned ONLY in Task 2** (selection policy + tagging). Task 1 must be selection-neutral.
- **Fixtures must run inside the test harness.** `gstat::fit.variogram` segfaulted R in a bare session on degenerate inputs during discovery; all fixture code goes through `helper.R`/test files, never standalone scripts.
- **No snapshot risk:** `tests/testthat/_snaps/` is empty (verified 2026-07-04).
- All pinned values below were captured on this machine on 2026-07-04 with the app-context discovery scripts (scratchpad `discover_vgm_fixtures.R`, `pin_seed12.R`). `expect_no_warning` and `expect_no_match` are available in the installed testthat.

## File Structure

- `monolith.R:199-257` — `robust_vgm_fit` (modified in Tasks 1–2)
- `monolith.R:4316-4323` — banner block inside `draw_map` (modified in Task 3)
- `ui_helpers.R` — add `build_vgm_warning_html()` after `get_buffer_multiplier` (~line 118) (Task 3)
- `tests/testthat/helper.R` — add `make_hostile_vgm_input()` fixture factory (Task 1)
- `tests/testthat/test-robust-vgm-fit.R` — new test file (Tasks 1–3)

## Pinned fixture facts (discovery, 2026-07-04)

| Fixture | Today (pure SSErr) | After Task 2 (clean-first) |
|---|---|---|
| `make_hostile_vgm_input(seed = 1)` — 9 pts, 10-row v_emp | 17 warnings; 16/16 candidates flawed; 5 eligible; winner Sph (flawed) | same Sph winner, `flawed_winner = TRUE`, nugget≈0.041309, psill≈123.891310, range≈58.043038 |
| `make_hostile_vgm_input(seed = 12)` — 10-row v_emp | winner Gau (clean), nugget≈21.239390, psill≈84.136984, range≈379.252147 | unchanged (invariance pin) |
| `make_test_points(30)` → v_emp (15 rows) | winner Gau (**flawed**, 8/16 flawed), psill≈112.6896 | winner **Mat** (clean), nugget = 0, psill≈114.613972, range≈32.241131 |

---

### Task 1: Classify candidates, muffle screening warnings, attach diagnostics (selection-neutral)

**Files:**
- Modify: `monolith.R:199-257` (`robust_vgm_fit`)
- Modify: `tests/testthat/helper.R` (append fixture factory)
- Create: `tests/testthat/test-robust-vgm-fit.R`

**Interfaces:**
- Consumes: `gstat::fit.variogram` (returns fit with `attr "singular"` and `attr "SSErr"`; non-convergence surfaces ONLY as a warning), `calc_scientific_lags(pts)` (app function), `clean_gstat_env(vgm)` (monolith.R:185 — strips only formula env / call attr).
- Produces: `robust_vgm_fit(v_emp, v_data)` — signature unchanged; every returned fit now carries `attr(fit, "vgm_diagnostics")` = `list(n_tried = <int>, n_flawed = <int>, flawed_winner = <lgl>)` (`flawed_winner` fixed `FALSE` until Task 2). Helper `make_hostile_vgm_input(n = 9, seed = 1)` → `list(v_emp = <gstatVariogram>, v_data = <numeric>)`.

- [ ] **Step 1: Append the fixture factory to `tests/testthat/helper.R`**

```r
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
```

- [ ] **Step 2: Create `tests/testthat/test-robust-vgm-fit.R` with the Task-1 tests**

```r
# test-robust-vgm-fit.R — candidate screening, diagnostics, and selection
# policy for robust_vgm_fit.
# Spec: docs/superpowers/specs/2026-07-04-robust-vgm-fit-convergence-design.md
# Pinned values captured 2026-07-04 via app-context discovery (see plan).

test_that("candidate screening emits no warnings on hostile data", {
  h <- make_hostile_vgm_input(seed = 1) # emits 17 warnings before this change
  expect_no_warning(robust_vgm_fit(h$v_emp, h$v_data))
})

test_that("returned fit carries the vgm_diagnostics contract", {
  h <- make_hostile_vgm_input(seed = 1)
  fit <- suppressWarnings(robust_vgm_fit(h$v_emp, h$v_data))
  d <- attr(fit, "vgm_diagnostics")
  expect_type(d, "list")
  expect_named(d, c("n_tried", "n_flawed", "flawed_winner"), ignore.order = TRUE)
  expect_identical(d$n_tried, 16L)
  expect_identical(d$n_flawed, 16L)
  expect_true(is.logical(d$flawed_winner))
})

test_that("diagnostics present on the tiny-variogram early return", {
  fit <- robust_vgm_fit(NULL, rnorm(10))
  d <- attr(fit, "vgm_diagnostics")
  expect_identical(d$n_tried, 0L)
  expect_identical(d$n_flawed, 0L)
})

test_that("diagnostics survive clean_gstat_env (worker serialization path)", {
  h <- make_hostile_vgm_input(seed = 1)
  fit <- suppressWarnings(robust_vgm_fit(h$v_emp, h$v_data))
  cleaned <- clean_gstat_env(fit)
  expect_false(is.null(attr(cleaned, "vgm_diagnostics")))
})

test_that("muffling does not change selection on a clean-winner fixture", {
  # Invariance guard (green before AND after this target): seed 12's
  # lowest-SSErr candidate is already clean, so neither Task 1 (neutral)
  # nor Task 2 (clean preference) may alter this selection.
  h <- make_hostile_vgm_input(seed = 12)
  fit <- suppressWarnings(robust_vgm_fit(h$v_emp, h$v_data))
  expect_identical(as.character(fit$model[2]), "Gau")
  expect_equal(fit$psill[1], 21.239390, tolerance = 1e-3)
  expect_equal(fit$psill[2], 84.136984, tolerance = 1e-3)
  expect_equal(fit$range[2], 379.252147, tolerance = 1e-3)
  expect_type(attr(fit, "vgm_diagnostics"), "list") # contract holds on the clean-winner path too
})
```

- [ ] **Step 3: Run the file — verify the red state**

Run: `"C:\Program Files\R\R-4.5.2\bin\Rscript.exe" -e 'testthat::test_dir("tests/testthat", filter = "robust-vgm-fit")'`
Expected: all 5 tests FAIL — the no-warning test fails (17 warnings emitted), the three diagnostics tests fail (`attr` is NULL), and the seed-12 invariance test fails ONLY at its final `expect_type(attr(...))` expectation (its model/nugget/psill/range expectations must pass first). If any of the seed-12 model/parameter expectations fail, STOP: the pinned values are machine-stale — re-derive them with the discovery procedure before continuing.

- [ ] **Step 4: Replace `robust_vgm_fit` in `monolith.R` (lines 199–257) with the hardened, selection-neutral version**

The old body iterates `fits <- lapply(models, ...)` with a nested range loop and picks best-per-model then best-overall — mathematically identical to a global lowest-SSErr over in-window candidates, which is what the new flattened loop does (same iteration order, same tie-breaking). Full replacement:

```r
robust_vgm_fit <- function(v_emp, v_data) {
  initial_sill <- var(v_data, na.rm=TRUE)
  if (is.na(initial_sill) || initial_sill == 0) initial_sill <- 1

  max_dist <- if (!is.null(v_emp) && nrow(v_emp) > 0) max(v_emp$dist, na.rm = TRUE) else 1.0
  if (is.na(max_dist) || is.infinite(max_dist) || max_dist <= 0) {
    max_dist <- 1.0 # Safe default positive distance fallback
  }

  vgm_diag <- function(n_tried, n_flawed, flawed_winner) {
    list(n_tried = n_tried, n_flawed = n_flawed, flawed_winner = flawed_winner)
  }

  if (is.null(v_emp) || nrow(v_emp) < 5) {
    # Skip fitting to prevent gstat::fit.variogram from crashing R on very small empirical variograms
    fallback <- gstat::vgm(psill = initial_sill * 0.8, "Sph", range = max_dist/2, nugget = initial_sill * 0.2)
    attr(fallback, "vgm_diagnostics") <- vgm_diag(0L, 0L, FALSE)
    return(fallback)
  }

  initial_nugget <- min(v_emp$gamma)
  if (initial_nugget == 0) initial_nugget <- max(initial_sill * 1e-6, 1e-6)

  if (initial_nugget > initial_sill) initial_nugget <- initial_sill * 0.9
  initial_psill <- max(initial_sill - initial_nugget, initial_sill * 0.1)

  ranges <- c(max_dist / 10, max_dist / 5, max_dist / 4, max_dist / 2)
  models <- c("Sph", "Exp", "Gau", "Mat") # Added Matern

  # gstat reports singular fits via attr(, "singular") but non-convergence
  # only as a C-level warning, so the warning itself is the detection signal.
  # These are expected while screening candidates and are muffled; anything
  # unrecognized still propagates.
  screening_warning <- "No convergence after|singular model|singular covariance"

  candidates <- list()
  for (m in models) {
    for (r in ranges) {
      start_kappa <- if (m == "Mat") 1.5 else 0.5
      flawed <- FALSE
      f <- tryCatch({
        withCallingHandlers(
          gstat::fit.variogram(v_emp, gstat::vgm(psill = initial_psill, model = m, range = r, nugget = initial_nugget, kappa = start_kappa)),
          warning = function(w) {
            if (grepl(screening_warning, conditionMessage(w))) {
              flawed <<- TRUE
              invokeRestart("muffleWarning")
            }
          }
        )
      }, error = function(e) NULL)
      if (is.null(f)) next
      flawed <- flawed || isTRUE(attr(f, "singular"))
      sse <- attr(f, "SSErr")
      in_window <- !is.null(sse) && f$range[2] > (max_dist/100) && f$range[2] < max_dist * 2 && f$psill[2] > 0
      candidates[[length(candidates) + 1]] <- list(fit = f, sse = sse, flawed = flawed, in_window = in_window)
    }
  }

  n_tried <- length(candidates)
  n_flawed <- sum(vapply(candidates, function(x) x$flawed, logical(1)))
  eligible <- Filter(function(x) x$in_window, candidates)

  best_fit <- NULL
  if (length(eligible) > 0) {
    best_fit <- eligible[[which.min(vapply(eligible, function(x) x$sse, numeric(1)))]]$fit
  }

  if (is.null(best_fit)) {
    if (initial_nugget > initial_sill * 0.8) {
      best_fit <- gstat::vgm(psill = initial_sill * 0.05, "Sph", range = max_dist/10, nugget = initial_sill * 0.95)
    } else {
      best_fit <- gstat::vgm(psill = initial_sill * 0.8, "Sph", range = max_dist/2, nugget = initial_sill * 0.2)
    }
    attr(best_fit, "is_fallback") <- TRUE
  }
  attr(best_fit, "vgm_diagnostics") <- vgm_diag(n_tried, n_flawed, FALSE) # flawed_winner semantics land in Task 2
  return(best_fit)
}
```

- [ ] **Step 5: Run the file — verify green**

Run: `"C:\Program Files\R\R-4.5.2\bin\Rscript.exe" -e 'testthat::test_dir("tests/testthat", filter = "robust-vgm-fit")'`
Expected: FAIL 0, WARN 0, all tests pass.

- [ ] **Step 6: Confirm the warning flood is gone at its loudest call site**

Run: `"C:\Program Files\R\R-4.5.2\bin\Rscript.exe" -e 'testthat::test_dir("tests/testthat", filter = "kriging-loocv")'`
Expected: FAIL 0 and **WARN 0** (baseline was 301 warnings from this file).

- [ ] **Step 7: Checkpoint (no git)**

Report changed files (`monolith.R`, `tests/testthat/helper.R`, `tests/testthat/test-robust-vgm-fit.R`) and test results for manual commit by the user.

---

### Task 2: Clean-preference selection, flawed_winner tag, tiny-variogram is_fallback tag (sanctioned behavior change)

**Files:**
- Modify: `monolith.R` (`robust_vgm_fit`, as written by Task 1)
- Modify: `tests/testthat/test-robust-vgm-fit.R` (append tests)

**Interfaces:**
- Consumes: Task 1's `robust_vgm_fit` internals (`candidates` records with `fit/sse/flawed/in_window`; `vgm_diag()` helper).
- Produces: returned fit additionally carries `attr(fit, "flawed_winner") = TRUE` when no clean candidate existed; tiny-variogram early return now carries `attr(fit, "is_fallback") = TRUE`; `vgm_diagnostics$flawed_winner` mirrors the top-level attribute. Task 3's banner builder relies on exactly these two top-level attribute names.

- [ ] **Step 1: Append the Task-2 tests to `tests/testthat/test-robust-vgm-fit.R`**

```r
test_that("a clean candidate is preferred over a lower-SSErr flawed one", {
  # Discovery (2026-07-04): pure-SSErr selection picks a non-converged Gau
  # for this fixture; clean preference must select the converged Mat.
  pts <- make_test_points(30)
  lags <- calc_scientific_lags(pts)
  v_emp <- gstat::variogram(v ~ 1, pts, width = lags$width, cutoff = lags$cutoff)
  fit <- robust_vgm_fit(v_emp, pts$v)
  expect_identical(as.character(fit$model[2]), "Mat")
  expect_equal(fit$psill[1], 0, tolerance = 1e-6)
  expect_equal(fit$psill[2], 114.613972, tolerance = 1e-3)
  expect_equal(fit$range[2], 32.241131, tolerance = 1e-3)
  expect_false(isTRUE(attr(fit, "flawed_winner")))
  expect_gt(attr(fit, "vgm_diagnostics")$n_flawed, 0)
})

test_that("flawed winner is tagged when no clean candidate exists", {
  h <- make_hostile_vgm_input(seed = 1) # 16/16 candidates flawed, 5 in-window
  fit <- robust_vgm_fit(h$v_emp, h$v_data)
  expect_true(isTRUE(attr(fit, "flawed_winner")))
  expect_true(attr(fit, "vgm_diagnostics")$flawed_winner)
  expect_identical(as.character(fit$model[2]), "Sph")
  expect_equal(fit$psill[2], 123.891310, tolerance = 1e-3)
  expect_false(isTRUE(attr(fit, "is_fallback")))
})

test_that("tiny empirical variogram fallback is tagged is_fallback", {
  fit_null <- robust_vgm_fit(NULL, rnorm(10))
  expect_true(isTRUE(attr(fit_null, "is_fallback")))
  h <- make_hostile_vgm_input(seed = 1)
  fit_tiny <- robust_vgm_fit(h$v_emp[1:3, ], h$v_data)
  expect_true(isTRUE(attr(fit_tiny, "is_fallback")))
})
```

- [ ] **Step 2: Run the file — verify the red state**

Run: `"C:\Program Files\R\R-4.5.2\bin\Rscript.exe" -e 'testthat::test_dir("tests/testthat", filter = "robust-vgm-fit")'`
Expected: the three new tests FAIL (Gau selected instead of Mat; `flawed_winner` attr absent; `is_fallback` absent on tiny path). All Task-1 tests still PASS.

- [ ] **Step 3: Apply the three edits to `robust_vgm_fit` in `monolith.R`**

Edit A — tag the tiny-variogram early return. Old:

```r
  if (is.null(v_emp) || nrow(v_emp) < 5) {
    # Skip fitting to prevent gstat::fit.variogram from crashing R on very small empirical variograms
    fallback <- gstat::vgm(psill = initial_sill * 0.8, "Sph", range = max_dist/2, nugget = initial_sill * 0.2)
    attr(fallback, "vgm_diagnostics") <- vgm_diag(0L, 0L, FALSE)
    return(fallback)
  }
```

New:

```r
  if (is.null(v_emp) || nrow(v_emp) < 5) {
    # Skip fitting to prevent gstat::fit.variogram from crashing R on very small empirical variograms
    fallback <- gstat::vgm(psill = initial_sill * 0.8, "Sph", range = max_dist/2, nugget = initial_sill * 0.2)
    attr(fallback, "is_fallback") <- TRUE
    attr(fallback, "vgm_diagnostics") <- vgm_diag(0L, 0L, FALSE)
    return(fallback)
  }
```

Edit B — clean-first selection. Old:

```r
  n_tried <- length(candidates)
  n_flawed <- sum(vapply(candidates, function(x) x$flawed, logical(1)))
  eligible <- Filter(function(x) x$in_window, candidates)

  best_fit <- NULL
  if (length(eligible) > 0) {
    best_fit <- eligible[[which.min(vapply(eligible, function(x) x$sse, numeric(1)))]]$fit
  }
```

New:

```r
  n_tried <- length(candidates)
  n_flawed <- sum(vapply(candidates, function(x) x$flawed, logical(1)))
  eligible <- Filter(function(x) x$in_window, candidates)
  clean_pool <- Filter(function(x) !x$flawed, eligible)
  flawed_pool <- Filter(function(x) x$flawed, eligible)

  pick_best <- function(pool) pool[[which.min(vapply(pool, function(x) x$sse, numeric(1)))]]$fit

  best_fit <- NULL
  flawed_winner <- FALSE
  if (length(clean_pool) > 0) {
    best_fit <- pick_best(clean_pool)
  } else if (length(flawed_pool) > 0) {
    # No clean candidate anywhere: still better than the heuristic fallback, but flagged.
    best_fit <- pick_best(flawed_pool)
    flawed_winner <- TRUE
    attr(best_fit, "flawed_winner") <- TRUE
  }
```

Edit C — final diagnostics line. Old:

```r
  attr(best_fit, "vgm_diagnostics") <- vgm_diag(n_tried, n_flawed, FALSE) # flawed_winner semantics land in Task 2
```

New:

```r
  attr(best_fit, "vgm_diagnostics") <- vgm_diag(n_tried, n_flawed, flawed_winner)
```

- [ ] **Step 4: Run the file — verify green**

Run: `"C:\Program Files\R\R-4.5.2\bin\Rscript.exe" -e 'testthat::test_dir("tests/testthat", filter = "robust-vgm-fit")'`
Expected: FAIL 0, WARN 0 — including the seed-12 invariance pin from Task 1 (its winner was already clean).

- [ ] **Step 5: Run the neighboring scientific-core test files**

Run: `"C:\Program Files\R\R-4.5.2\bin\Rscript.exe" -e 'testthat::test_dir("tests/testthat", filter = "kriging-loocv|interpolation-pipeline|cv-metrics|rfk-ntree")'`
Expected: FAIL 0. These exercise `robust_vgm_fit` through the real pipelines; failures here mean the selection change broke an integration assumption — STOP and investigate rather than adjusting fixtures (known-answer tests must never be re-derived to fit the code).

- [ ] **Step 6: Checkpoint (no git)**

Report changed files and results for manual commit.

---

### Task 3: Banner builder + draw_map integration

**Files:**
- Modify: `ui_helpers.R` (insert `build_vgm_warning_html` directly after `get_buffer_multiplier`, ~line 118)
- Modify: `monolith.R:4316-4323` (banner block inside `draw_map`)
- Modify: `tests/testthat/test-robust-vgm-fit.R` (append builder tests)

**Interfaces:**
- Consumes: top-level fit attributes `is_fallback` and `flawed_winner` (Task 2), `make_mock_vgm()` from helper.R.
- Produces: `build_vgm_warning_html(v_fit_list)` → single HTML string, or `NULL` when nothing is flagged. `draw_map` becomes a thin caller.

- [ ] **Step 1: Append the builder tests to `tests/testthat/test-robust-vgm-fit.R`**

```r
test_that("build_vgm_warning_html returns NULL when nothing is flagged", {
  expect_null(build_vgm_warning_html(list(A_act = make_mock_vgm())))
  expect_null(build_vgm_warning_html(list()))
})

test_that("build_vgm_warning_html renders red fallback and amber flawed sections", {
  f_fb <- make_mock_vgm(); attr(f_fb, "is_fallback") <- TRUE
  f_fw <- make_mock_vgm(); attr(f_fw, "flawed_winner") <- TRUE
  html <- build_vgm_warning_html(list(LocA_act = f_fb, LocB_pre = f_fw, LocC_act = make_mock_vgm()))
  expect_match(html, "LocA_act", fixed = TRUE)
  expect_match(html, "LocB_pre", fixed = TRUE)
  expect_match(html, "fallback spherical model", fixed = TRUE)
  expect_match(html, "non-converged or singular", fixed = TRUE)
  expect_no_match(html, "LocC_act", fixed = TRUE)
})

test_that("amber-only banner omits the red section", {
  f_fw <- make_mock_vgm(); attr(f_fw, "flawed_winner") <- TRUE
  html <- build_vgm_warning_html(list(L1_act = f_fw))
  expect_match(html, "non-converged or singular", fixed = TRUE)
  expect_no_match(html, "Variogram fit failed", fixed = TRUE)
})
```

- [ ] **Step 2: Run the file — verify the red state**

Run: `"C:\Program Files\R\R-4.5.2\bin\Rscript.exe" -e 'testthat::test_dir("tests/testthat", filter = "robust-vgm-fit")'`
Expected: the three new tests ERROR with `could not find function "build_vgm_warning_html"`. All earlier tests PASS.

- [ ] **Step 3: Add `build_vgm_warning_html` to `ui_helpers.R`** (insert after the closing brace of `get_buffer_multiplier`, ~line 118)

```r
# Builds the Map Viewer variogram-quality banner from the per-locality fit
# list. Red = heuristic fallback (fit failed entirely); amber = auto-fit had
# to select a non-converged/singular candidate. Returns NULL when clean.
build_vgm_warning_html <- function(v_fit_list) {
  fallback_keys <- character(0)
  flawed_keys <- character(0)
  for (n in names(v_fit_list)) {
    if (isTRUE(attr(v_fit_list[[n]], "is_fallback"))) {
      fallback_keys <- c(fallback_keys, n)
    } else if (isTRUE(attr(v_fit_list[[n]], "flawed_winner"))) {
      flawed_keys <- c(flawed_keys, n)
    }
  }
  if (length(fallback_keys) == 0 && length(flawed_keys) == 0) return(NULL)

  red_part <- if (length(fallback_keys) > 0) {
    paste0("<span style='color:red;'>Note: Variogram fit failed for some localities (",
           paste(fallback_keys, collapse = ", "),
           ").<br>A fallback spherical model was applied to prevent application failure. Interpret interpolations with caution.</span>")
  } else ""
  amber_part <- if (length(flawed_keys) > 0) {
    paste0("<span style='color:#e67700;'>Auto-fit selected a non-converged or singular variogram for: ",
           paste(flawed_keys, collapse = ", "),
           ".<br>Interpret interpolations with caution.</span>")
  } else ""
  sep <- if (nzchar(red_part) && nzchar(amber_part)) "<br>" else ""

  paste0("<div id='vgm_fallback_warn' style='font-weight:bold; background:white; padding:5px 25px 5px 5px; border-radius:4px; position:relative;'>",
         "<button onclick='document.getElementById(\"vgm_fallback_warn\").style.display=\"none\";' style='position:absolute; top:2px; right:2px; background:none; border:none; color:red; font-size:16px; font-weight:bold; cursor:pointer;'>&times;</button>",
         red_part, sep, amber_part,
         "</div>")
}
```

- [ ] **Step 4: Run the file — verify green**

Run: `"C:\Program Files\R\R-4.5.2\bin\Rscript.exe" -e 'testthat::test_dir("tests/testthat", filter = "robust-vgm-fit")'`
Expected: FAIL 0.

- [ ] **Step 5: Replace the banner block inside `draw_map` (monolith.R:4316-4323)**

Old code (verbatim, including the long inline HTML):

```r
      if (length(r_list) > 0) {
        fallback_locs <- c()
        for(n in names(rv$v_fit_list)) {
          if(isTRUE(attr(rv$v_fit_list[[n]], "is_fallback"))) fallback_locs <- c(fallback_locs, n)
        }
        if(length(fallback_locs) > 0) {
          m <- m %>% addControl(html = paste0("<div id='vgm_fallback_warn' style='color:red; font-weight:bold; background:white; padding:5px 25px 5px 5px; border-radius:4px; position:relative;'><button onclick='document.getElementById(\"vgm_fallback_warn\").style.display=\"none\";' style='position:absolute; top:2px; right:2px; background:none; border:none; color:red; font-size:16px; font-weight:bold; cursor:pointer;'>&times;</button>Note: Variogram fit failed for some localities (", paste(fallback_locs, collapse=", "), ").<br>A fallback spherical model was applied to prevent application failure. Interpret interpolations with caution.</div>"), position = "bottomleft")
        }
```

New code:

```r
      if (length(r_list) > 0) {
        vgm_warn_html <- build_vgm_warning_html(rv$v_fit_list)
        if (!is.null(vgm_warn_html)) {
          m <- m %>% addControl(html = vgm_warn_html, position = "bottomleft")
        }
```

(The red wording is preserved inside the builder; the only visual difference is that `color:red` moved from the `<div>` to the red `<span>` so the amber span can carry its own color.)

- [ ] **Step 6: Confirm the app still sources cleanly**

Run: `"C:\Program Files\R\R-4.5.2\bin\Rscript.exe" -e 'testthat::test_dir("tests/testthat", filter = "0-smoke-infrastructure")'`
Expected: FAIL 0 (the smoke file re-sources the whole app, catching syntax/scoping mistakes in `draw_map`).

- [ ] **Step 7: Checkpoint (no git)**

Report changed files and results for manual commit.

---

### Task 4: Full-suite acceptance and handoff

**Files:** none modified (verification only).

**Interfaces:**
- Consumes: everything above.
- Produces: evidence for Phase 4 (`/simplify`) and Phase 5 (`/code-review` + scientific-accuracy-reviewer) handoff.

- [ ] **Step 1: Run the full suite**

Run: `"C:\Program Files\R\R-4.5.2\bin\Rscript.exe" tests/testthat.R` (timeout ≥ 10 minutes)
Expected: `FAIL 0 | SKIP 0`, PASS > 638 (baseline expectations plus the new file's), WARN ≤ 13 (baseline 314; the 301 robust_vgm_fit-origin warnings gone).

- [ ] **Step 2: Verify the remaining warnings' provenance**

Run: `"C:\Program Files\R\R-4.5.2\bin\Rscript.exe" tests/testthat.R 2>&1 | grep "WARNING: '" | sort | uniq -c | sort -rn`
Expected: remaining warnings only from `test-governing-factors.R`, `test-stat-tests.R`, and possibly `test-interpolation-pipeline.R` — all separate backlog targets. If any warning still originates in `robust_vgm_fit`/`perform_kriging_loocv`: inspect the message; add it to `screening_warning` ONLY if it is clearly a fit.variogram candidate-screening message, otherwise leave it propagating (it is a real signal) and note it.

- [ ] **Step 3: Confirm diff scope**

Changed files must be exactly: `monolith.R`, `ui_helpers.R`, `tests/testthat/helper.R`, `tests/testthat/test-robust-vgm-fit.R` (plus the spec/plan docs). Anything else is scope creep — investigate.

- [ ] **Step 4: Report for manual commit and phase handoff**

Summarize: warning counts before/after, the sanctioned selection-policy change and where it shows up (fixtures where Gau→Mat), and readiness for Phase 4 (`/simplify` on the diff) and Phase 5 (`/code-review`, plus the scientific-accuracy-reviewer agent since model code changed). Remind: commits are manual; update the spatial-model-conventions skill doc if the reviewer confirms the new selection policy is a convention worth recording.
