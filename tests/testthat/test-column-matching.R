# Column matching: the rules a variable-list row is matched to a column by
# (match_metadata_columns()), detect_pred_column() and the coordinate and
# column-reference guards.

# ── variable-list rows to columns ─────────────────────────────────────────
# One row per name in `rows`, labelled "L:<name>", so a label says which row
# claimed a column.
match_rows <- function(rows, cols) {
  res <- match_metadata_columns(
    data.frame(Variable = rows, Label = paste0("L:", rows), stringsAsFactors = FALSE), cols)
  list(actual = vapply(res$vars, function(v) v$actual, character(1)),
       label = vapply(res$vars, function(v) v$label, character(1)),
       unmatched = res$unmatched)
}

test_that("a row takes the column of the same name", {
  m <- match_rows(c("Nitrogen", "Clay"), c("pH", "Nitrogen", "Organic_Carbon", "Clay"))
  expect_identical(m$actual, c("Nitrogen", "Clay"))
  expect_identical(m$unmatched, character(0))
})

test_that("case, then spaces and punctuation, may differ", {
  m <- match_rows(c("nitrogen", "ph", "Organic Carbon", "pH H2O", "Ca CO3"),
                  c("pH", "NITROGEN", "Organic_Carbon", "pH-H2O", "CaCO3"))
  expect_identical(m$actual, c("NITROGEN", "pH", "Organic_Carbon", "pH-H2O", "CaCO3"))
  expect_identical(m$unmatched, character(0))
})

test_that("a name is never matched by similarity", {
  # One or two characters from a column of the table is another variable.
  m <- match_rows(c("Nitrogem", "Ca", "zzz_nonexistent_zzz"), c("Nitrogen", "Phosphorus", "K", "Calcium"))
  expect_length(m$actual, 0L)
  expect_identical(m$unmatched, c("Nitrogem", "Ca", "zzz_nonexistent_zzz"))
})

test_that("a list row for an absent column leaves its near-namesake to the row that names it", {
  # A list kept for several datasets names columns this table lacks. Whichever
  # order the rows come in, each column takes the label of the row naming it.
  cols <- c("ndvi_2021", "clay_60", "x", "y")
  rows <- c("ndvi_2020", "ndvi_2021", "clay_30", "clay_60")
  for (ord in list(seq_along(rows), rev(seq_along(rows)))) {
    m <- match_rows(rows[ord], cols)
    expect_setequal(m$actual, c("ndvi_2021", "clay_60"))
    expect_identical(m$label, paste0("L:", m$actual))
    expect_setequal(m$unmatched, c("ndvi_2020", "clay_30"))
  }
})

test_that("an exact name outranks a looser match whatever the row order", {
  # "PH" would take the column "pH" ignoring case; the row naming it exactly
  # keeps it, and "PH" is reported as having no column of its own.
  for (rows in list(c("PH", "pH"), c("pH", "PH"))) {
    m <- match_rows(rows, c("pH", "EC"))
    expect_identical(m$actual, "pH")
    expect_identical(m$label, "L:pH")
    expect_identical(m$unmatched, "PH")
  }
  # Two columns differing in case each go to their own row.
  m <- match_rows(c("ph", "pH"), c("pH", "ph"))
  expect_identical(m$actual, c("ph", "pH"))
  expect_identical(m$label, c("L:ph", "L:pH"))
})

test_that("a row repeating a mapped name is a duplicate, not a missing column", {
  m <- match_rows(c("pH", "pH"), c("pH", "EC"))
  expect_identical(m$actual, "pH")
  expect_identical(m$unmatched, character(0))
})

test_that("letters of every script count, so names in another alphabet do not collide", {
  micro <- "pH_µ"; mu <- "pH_μ"                     # micro sign, Greek mu
  azot <- "Азот"; fosfor <- "Фосфор"
  cols <- c(micro, mu, azot, fosfor)
  # The micro sign and the Greek mu are different letters: each row its column.
  m <- match_rows(c("pH μ", "pH µ"), cols)
  expect_identical(m$actual, c(mu, micro))
  # "pH" alone names neither, and a Cyrillic name is not reduced to nothing
  # (which would hand it the first column that also reduces to nothing).
  kaliy <- "Калий"
  m <- match_rows(c("pH", paste0(fosfor, " "), kaliy), cols)
  expect_identical(m$actual, fosfor)
  expect_identical(m$unmatched, c("pH", kaliy))
  # A name of punctuation only has no letters to match on.
  expect_identical(match_rows("--", c("__", "pH"))$unmatched, "--")
})

test_that("columns reducing to the same letters go to the rows in order", {
  m <- match_rows(c("pH H2O", "pH.H2O"), c("pH_H2O", "pH-H2O", "Clay"))
  expect_identical(m$actual, c("pH_H2O", "pH-H2O"))
  long <- strrep("a", 200)
  expect_identical(match_rows(long, c(long, "short"))$actual, long)
})

# ── detect_pred_column ────────────────────────────────────────────────────

test_that("detects _cve suffix for CVE type", {
  candidates <- c("pH", "pH_cve", "pH_pred", "Clay_cve")
  result <- detect_pred_column("pH", candidates, type = "cve")
  expect_equal(result, "pH_cve")
})

test_that("detects _pred suffix when _cve is absent", {
  candidates <- c("pH", "pH_pred", "Clay_pred")
  result <- detect_pred_column("pH", candidates, type = "cve")
  expect_equal(result, "pH_pred")
})

test_that("detects _predicted suffix", {
  candidates <- c("pH", "pH_predicted", "Clay")
  result <- detect_pred_column("pH", candidates, type = "cve")
  expect_equal(result, "pH_predicted")
})

test_that("detects CamelCase Pred/Predicted variants", {
  candidates <- c("pH", "pHPred", "Clay")
  result <- detect_pred_column("pH", candidates, type = "cve")
  expect_equal(result, "pHPred")
})

test_that("detects pred_ prefix pattern", {
  candidates <- c("pH", "pred_pH", "pred_Clay")
  result <- detect_pred_column("pH", candidates, type = "cve")
  expect_equal(result, "pred_pH")
})

test_that("detects _ss / _split / _test suffixes for SS type", {
  candidates <- c("pH", "pH_ss", "pH_test", "Clay")
  result <- detect_pred_column("pH", candidates, type = "ss")
  expect_equal(result, "pH_ss")

  candidates2 <- c("pH", "pH_split", "Clay")
  result2 <- detect_pred_column("pH", candidates2, type = "ss")
  expect_equal(result2, "pH_split")
})

test_that("detects CamelCase Split/Test for SS type", {
  candidates <- c("pH", "pHSplit", "ClayTest")
  result <- detect_pred_column("pH", candidates, type = "ss")
  expect_equal(result, "pHSplit")
})

test_that("returns NA when no pattern matches", {
  candidates <- c("foo", "bar", "baz")
  result <- detect_pred_column("pH", candidates, type = "cve")
  expect_true(is.na(result))
})

test_that("handles NULL target gracefully", {
  candidates <- c("pH_cve", "Clay_cve")
  result <- detect_pred_column(NULL, candidates, type = "cve")
  expect_true(is.na(result))
})

test_that("handles empty candidates gracefully", {
  result <- detect_pred_column("pH", character(0), type = "cve")
  expect_true(is.na(result))
})

test_that("a column name is compared as plain text, whatever characters it holds", {
  # A header with brackets, a plus sign or a dot is a name, not a pattern.
  expect_identical(detect_pred_column("Fe (mg/kg)", c("Fe (mg/kg)", "Fe (mg/kg)_cve")), "Fe (mg/kg)_cve")
  expect_identical(detect_pred_column("NH4+", c("NH4+", "NH4+_cve", "NH4_cve")), "NH4+_cve")
  expect_identical(detect_pred_column("NH4+", c("NH4+", "nh4+_SS"), type = "ss"), "nh4+_SS")
  # The dot is a dot: another character in its place is another column.
  expect_true(is.na(detect_pred_column("pH.H2O", c("pH.H2O", "pHxH2O_cve"))))
  expect_identical(detect_pred_column("pH.H2O", c("pHxH2O_cve", "pH.H2O_cve")), "pH.H2O_cve")
  # An unbalanced bracket is a character like any other, never an invalid
  # pattern (an error here is raised inside the upload observer).
  for (target in c("Depth [cm", "Yield (t/ha", "a\\b", "50%*")) {
    expect_true(is.na(detect_pred_column(target, c(target, "other_cve"))))
    expect_identical(detect_pred_column(target, c(target, paste0(target, "_pred"))), paste0(target, "_pred"))
    expect_identical(detect_pred_column(target, c(paste0("pred_", target), target)), paste0("pred_", target))
  }
})

test_that("the naming forms are tried in their stated order", {
  cand <- c("predicted_pH", "pred_pH", "pHPredicted", "pHPred", "pH_predicted", "pH_pred", "pH_cve")
  for (k in rev(seq_along(cand))) {
    expect_identical(detect_pred_column("pH", cand[seq_len(k)]), cand[k])
  }
  cand_ss <- c("pHTest", "pHSplit", "pH_test", "pH_split", "pH_ss")
  for (k in rev(seq_along(cand_ss))) {
    expect_identical(detect_pred_column("pH", cand_ss[seq_len(k)], type = "ss"), cand_ss[k])
  }
})

# ── is_valid_col_ref ──────────────────────────────────────────────────────
# Guards the "uploaded predictions" panels: detect_pred_column() returns NA (not
# NULL) when no prediction column exists, which an is.null() check would read
# as "predictions present".

test_that("is_valid_col_ref accepts a real column name", {
  expect_true(is_valid_col_ref("pH_cve"))
})

test_that("is_valid_col_ref rejects NULL, NA, empty and multi-length inputs", {
  expect_false(is_valid_col_ref(NULL))
  expect_false(is_valid_col_ref(NA))
  expect_false(is_valid_col_ref(NA_character_))
  expect_false(is_valid_col_ref(""))
  expect_false(is_valid_col_ref(c("a", "b")))
})

test_that("is_valid_col_ref is FALSE for detect_pred_column's no-match result", {
  no_match <- detect_pred_column("pH", c("foo", "bar"), type = "cve")
  expect_false(is_valid_col_ref(no_match))
})

# ── is_coord_col ──────────────────────────────────────────────────────────

test_that("is_coord_col matches only whole-name coordinate tokens", {
  expect_true(all(is_coord_col(c("x", "Y", "lon", "LAT", "Longitude",
                                 "latitude", " easting ", "Northing", "lng"))))
  # Names that merely contain a coordinate token are variables.
  expect_false(any(is_coord_col(c("Precipitation_cumulative", "correlation_index",
                                  "latent_heat", "longevity", "along_slope",
                                  "Platinum", "x_offset", "pH"))))
})

# ── pick_coord_column ─────────────────────────────────────────────────────

test_that("pick_coord_column pre-selects only whole-name coordinate columns", {
  # Whole-name matching, as is_coord_col(): a variable like Longevity_index is
  # never pre-selected as the X coordinate.
  cols <- c("Longevity_index", "Lateral_flow", "pH", "Longitude", "Latitude")
  expect_identical(pick_coord_column(cols, "x"), "Longitude")
  expect_identical(pick_coord_column(cols, "y"), "Latitude")

  # No coordinate column: NULL, which leaves the input's first column selected.
  expect_null(pick_coord_column(c("Longevity_index", "Lateral_flow", "pH"), "x"))
  expect_null(pick_coord_column(c("Longevity_index", "Lateral_flow", "pH"), "y"))
  expect_null(pick_coord_column(character(0), "x"))

  # Case and surrounding whitespace are ignored; the first hit wins.
  expect_identical(pick_coord_column(c("pH", " EASTING ", "x"), "x"), " EASTING ")
  expect_identical(pick_coord_column(c("pH", "NORTHING", "y"), "y"), "NORTHING")
})

test_that("match_metadata_columns tolerates empty and blank metadata rows", {
  # A headers-only sheet and a sheet with no column at all yield an empty
  # mapping; NA and blank names are skipped and are not reported as unmatched.
  none <- list(vars = list(), unmatched = character(0))
  empty <- data.frame(Variable = character(0), Label = character(0))
  expect_identical(match_metadata_columns(empty, c("pH", "EC")), none)
  expect_identical(match_metadata_columns(data.frame(), c("pH", "EC")), none)

  m_df <- data.frame(Variable = c("pH", NA, ""), Label = c("Soil pH", "x", "y"))
  res <- match_metadata_columns(m_df, c("pH", "EC"))
  expect_length(res$vars, 1)
  expect_identical(res$vars[[1]]$actual, "pH")
  expect_identical(res$vars[[1]]$label, "Soil pH")
  expect_identical(res$unmatched, character(0))
})

test_that("an empty label or category cell takes the mapping editor's own default", {
  m_df <- data.frame(Variable = c("pH", "EC", "K"), Label = c(NA, "  ", "Potassium"),
                     Category = c("Soil", NA, ""), stringsAsFactors = FALSE)
  res <- match_metadata_columns(m_df, c("pH", "EC", "K"))$vars
  expect_identical(vapply(res, function(v) v$label, character(1)), c("pH", "EC", "Potassium"))
  expect_identical(vapply(res, function(v) v$category, character(1)),
                   c("Soil", "Uploaded Data", "Uploaded Data"))
})
