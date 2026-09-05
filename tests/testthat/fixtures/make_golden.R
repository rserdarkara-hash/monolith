# make_golden.R — build a golden fixture from a source dataset.
#
# Run from the project root. With no arguments it rebuilds the shipped fixture
# from sample_data/ byte-for-byte:
#
#   "C:/Program Files/R/R-4.5.2/bin/Rscript.exe" tests/testthat/fixtures/make_golden.R
#
# To build a fixture from your OWN data instead, map your columns onto the
# fixture's canonical names and write to your own directory:
#
#   source("tests/testthat/fixtures/make_golden.R")
#   make_golden(
#     src_data = "my_survey.xlsx",
#     out_dir  = "tests/testthat/fixtures_mine",
#     roles = list(
#       locality = "site", x = "easting", y = "northing", crs = 25832,
#       target = "pH_lab", target2 = "carbon", categorical = "usda_class",
#       covariates = c("dem", "slope", "twi", "ndvi", "temp", "precip",
#                      "tpi", "tri", "ndvi_s2", "temp_warm")
#     ))
#
# then point the suite at it and regenerate its baselines:
#
#   options(monolith_golden_dir = "tests/testthat/fixtures_mine")
#   "…/Rscript.exe" tests/testthat/fixtures/make_baselines.R
#
# The canonical column names are the shipped survey's own (`ph`, `v82`, …), so
# the tests can read them literally and stay readable. `roles` is what makes
# them portable: your `dem` is written out as `v82`. GOLDEN_MANIFEST.md carries
# the full role table.
#
# The fixture is FROZEN on purpose. Its source is hand-edited; if the tests
# derived their reference values from it at run time, an edit there would
# silently move every golden number. Refresh deliberately, re-run the suite, and
# treat any moved value as something to explain, not to accept.

suppressPackageStartupMessages({
  library(readxl)
  library(tools)
})

# Canonical fixture columns. Everything downstream reads these names.
.GOLDEN_SOIL <- c("ph", "ec", "caco3", "som", "sand", "silt", "clay", "tn",
                  "p", "k", "ca", "mg", "na", "fe", "cu", "zn", "mn")
.GOLDEN_PRED <- c("tn_cve", "tn_ss", "p_cve", "p_ss", "k_cve", "k_ss")
# Ten covariates spanning the source survey's four covariate families, chosen so
# the set carries REAL multicollinearity (v1/v10 temperature, v43/v61 NDVI)
# rather than a synthetic correlation matrix.
.GOLDEN_COVAR <- c("v1", "v10", "v12", "v43", "v61",
                   "v82", "v83", "v85", "v86", "v87")
.GOLDEN_KEYS <- c("sample_no", "locality", "subset", "data_from", "texture")

.golden_default_roles <- function() {
  list(
    sample_no = "sample_no", locality = "locality", subset = "subset",
    data_from = "data_from", categorical = "texture",
    x = "x", y = "y", crs = 32635,
    target = "ph", target2 = "som",
    soil = .GOLDEN_SOIL, pred = .GOLDEN_PRED, covariates = .GOLDEN_COVAR
  )
}

# Scope definitions: locality -> keep every k-th row of the coordinate-sorted
# table. No RNG anywhere, so the reduced scopes are stable across R versions and
# keep the full spatial extent by construction.
#
# The explicit default reproduces the shipped fixture exactly. Data whose
# localities differ falls through to the automatic rule.
.golden_default_scopes <- function(gs) {
  want <- list(core = list(Altinova = 6L, Karacasu = 5L, Yorga = 5L),
               tiny = list(Kale = 2L))
  have <- unique(gs$locality)
  if (all(c(names(want$core), names(want$tiny)) %in% have)) return(want)

  tab <- sort(table(gs$locality), decreasing = TRUE)
  if (length(tab) == 0) stop("no localities in the source data")

  # core: up to the three largest localities, each thinned toward ~48 points.
  big <- names(tab)[seq_len(min(3L, length(tab)))]
  core <- lapply(big, function(l) max(1L, as.integer(round(tab[[l]] / 48))))
  names(core) <- big

  # tiny: the most compact locality that still yields ~40 points, so the
  # kriging tests run on a small, spatially tight point set.
  cand <- names(tab)[tab >= 40]
  if (length(cand) == 0) cand <- names(tab)[1]
  span <- vapply(cand, function(l) {
    d <- gs[gs$locality == l, ]
    max(diff(range(d$x)), diff(range(d$y)))
  }, numeric(1))
  pick <- cand[which.min(span)]
  tiny <- list(max(1L, as.integer(round(tab[[pick]] / 40))))
  names(tiny) <- pick

  list(core = core, tiny = tiny)
}

#' Build a golden fixture.
#'
#' @param src_data Source table (.xlsx) holding the points.
#' @param src_meta Optional variable dictionary (.xlsx, three columns:
#'   id, label, category). NULL writes no dictionary.
#' @param out_dir Directory to write the fixture into.
#' @param roles Column mapping; see .golden_default_roles(). Any entry omitted
#'   falls back to the default (i.e. the source column is already named
#'   canonically).
#' @param scopes Optional explicit scope spec, list(core = list(<loc> = k),
#'   tiny = list(<loc> = k)). NULL derives it.
make_golden <- function(src_data = "sample_data/samp_data_1.xlsx",
                        src_meta = "sample_data/samp_var_list.xlsx",
                        out_dir = "tests/testthat/fixtures",
                        roles = list(),
                        scopes = NULL) {
  if (!file.exists("global.R")) {
    stop("run make_golden.R from the project root (the directory holding global.R)")
  }
  r <- utils::modifyList(.golden_default_roles(), roles)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  raw <- as.data.frame(read_excel(src_data, sheet = 1))

  # Map source columns onto the canonical names.
  src_cols <- c(r$sample_no, r$locality, r$subset, r$data_from, r$categorical,
                r$x, r$y, r$soil, r$pred, r$covariates)
  dst_cols <- c(.GOLDEN_KEYS, "x", "y", .GOLDEN_SOIL, .GOLDEN_PRED, .GOLDEN_COVAR)
  if (length(src_cols) != length(dst_cols)) {
    stop("roles must name ", length(.GOLDEN_SOIL), " soil, ",
         length(.GOLDEN_PRED), " prediction and ", length(.GOLDEN_COVAR),
         " covariate columns")
  }
  missing <- setdiff(src_cols, names(raw))
  if (length(missing)) {
    stop("source data has no column(s): ", paste(missing, collapse = ", "))
  }
  gs <- raw[, src_cols]
  names(gs) <- dst_cols

  for (k in .GOLDEN_KEYS) gs[[k]] <- as.character(gs[[k]])
  for (n in setdiff(dst_cols, .GOLDEN_KEYS)) gs[[n]] <- as.numeric(gs[[n]])

  # Deterministic row order: sorting on the coordinates makes the systematic
  # thinning below spatially spread by construction.
  gs <- gs[order(gs$locality, gs$x, gs$y, gs$sample_no), ]
  rownames(gs) <- NULL

  if (is.null(scopes)) scopes <- .golden_default_scopes(gs)

  meta <- list(
    crs = r$crs,
    roles = r,
    scopes = scopes,
    columns = list(keys = .GOLDEN_KEYS, coords = c("x", "y"),
                   soil = .GOLDEN_SOIL, pred = .GOLDEN_PRED,
                   covariates = .GOLDEN_COVAR,
                   target = "ph", target2 = "som",
                   covariate_main = "v82", categorical = "texture"),
    source = list(data = src_data, meta = src_meta,
                  md5_data = unname(md5sum(src_data)),
                  md5_meta = if (is.null(src_meta)) NA_character_ else unname(md5sum(src_meta))),
    built_at = format(Sys.time(), "%Y-%m-%d")
  )

  saveRDS(gs, file.path(out_dir, "golden_soil.rds"), version = 3)
  saveRDS(meta, file.path(out_dir, "golden_meta.rds"), version = 3)
  # CSV is for eyeballing and diffing only; the .rds is authoritative (the CSV
  # round-trip is not bit-exact for doubles).
  write.csv(gs, file.path(out_dir, "golden_soil.csv"), row.names = FALSE, na = "")

  if (!is.null(src_meta)) {
    vl <- as.data.frame(read_excel(src_meta, sheet = 1))
    names(vl) <- c("vn", "vid", "cat")
    for (n in names(vl)) vl[[n]] <- as.character(vl[[n]])
    saveRDS(vl, file.path(out_dir, "golden_varlist.rds"), version = 3)
  }

  cat("fixture written to", out_dir, "\n")
  cat("rows:", nrow(gs), " cols:", ncol(gs), "\n")
  cat("crs:", meta$crs, "\n")
  cat("scopes: core =", paste(names(scopes$core), unlist(scopes$core), sep = "/", collapse = ", "),
      "| tiny =", paste(names(scopes$tiny), unlist(scopes$tiny), sep = "/", collapse = ", "), "\n")
  cat("source md5:", meta$source$md5_data, "/", meta$source$md5_meta, "\n")
  cat("localities:\n"); print(table(gs$locality))
  cat("\nNEXT: regenerate this fixture's baselines with\n",
      "  Rscript tests/testthat/fixtures/make_baselines.R\n", sep = "")
  invisible(meta)
}

# Direct invocation rebuilds the shipped fixture.
if (!interactive() && identical(environmentName(parent.frame()), "R_GlobalEnv") &&
    length(sys.calls()) == 0) {
  make_golden()
}
