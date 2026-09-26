# test-metadata-matching.R — tests for match_metadata_columns, desc_var_labels,
# get_var_label, and get_var_labels.

# ── get_var_label / get_var_labels ─────────────────────────────────────────

test_that("get_var_label returns label from metadata when available", {
  metadata <- list(
    list(actual = "pH", label = "Soil pH", category = "Soil"),
    list(actual = "Clay", label = "Clay Content", category = "Soil")
  )
  expect_equal(get_var_label("pH", metadata), "Soil pH")
  expect_equal(get_var_label("Clay", metadata), "Clay Content")
})

test_that("get_var_label returns original name when not in metadata", {
  metadata <- list(list(actual = "pH", label = "Soil pH", category = "Soil"))
  expect_equal(get_var_label("Sand", metadata), "Sand")
})

test_that("get_var_label handles NULL metadata", {
  expect_equal(get_var_label("pH", NULL), "pH")
})

test_that("get_var_label handles NA and empty input", {
  metadata <- list(list(actual = "pH", label = "Soil pH", category = "Soil"))
  expect_equal(get_var_label(NA_character_, metadata), NA_character_)
  expect_equal(get_var_label("", metadata), "")
})

test_that("get_var_label fuzzy-matches when exact match fails", {
  # The label must differ from the query, or the fuzzy branch and the miss
  # branch (which returns the query itself) return the same string and the
  # assertion cannot tell them apart.
  metadata <- list(
    list(actual = "Organic_Carbon", label = "Soil Organic Carbon", category = "Soil")
  )
  expect_equal(get_var_label("Organic Carbon", metadata), "Soil Organic Carbon")
})

test_that("get_var_labels vectorizes correctly", {
  metadata <- list(
    list(actual = "pH", label = "Acidity", category = "Soil"),
    list(actual = "N", label = "Nitrogen", category = "Soil")
  )
  result <- get_var_labels(c("pH", "N", "Unknown"), metadata)
  expect_equal(as.character(result), c("Acidity", "Nitrogen", "Unknown"))
})

test_that("descriptive display labels are unique without renaming source data", {
  metadata <- list(list(actual = "a", label = "Value"), list(actual = "b", label = "Value"),
                   list(actual = "c", label = "Value [a]"))
  labels <- desc_var_labels(c("a", "b", "c", "other"), metadata)
  expect_identical(names(labels), c("a", "b", "c", "other"))
  expect_equal(anyDuplicated(labels), 0L)
  expect_match(labels[["a"]], "a", fixed = TRUE)
  expect_equal(labels[["other"]], "other")
  expect_equal(display_var_labels(c("a", "missing"), labels), c(labels[["a"]], "missing"))
  expect_equal(desc_var_labels(c("a", "b")), c(a = "a", b = "b"))
})

# ── match_metadata_columns ────────────────────────────────────────────────

test_that("match_metadata_columns returns list of mapped variables", {
  m_df <- data.frame(
    actual = c("pH", "Clay"),
    label  = c("Soil pH", "Clay Content"),
    cat    = c("Soil", "Soil"),
    stringsAsFactors = FALSE
  )
  user_cols <- c("pH", "Clay", "Sand", "x", "y")
  result <- match_metadata_columns(m_df, user_cols)
  expect_type(result, "list")
  # Which column was mapped and to what, not just that something was.
  expect_length(result, 2L)
  expect_equal(vapply(result, `[[`, character(1), "actual"), c("pH", "Clay"))
  expect_equal(vapply(result, `[[`, character(1), "label"), c("Soil pH", "Clay Content"))
  expect_equal(vapply(result, `[[`, character(1), "category"), c("Soil", "Soil"))
})

test_that("match_metadata_columns assigns palettes", {
  m_df <- data.frame(
    actual = c("TN"),
    label  = c("Total Nitrogen"),
    cat    = c("Soil"),
    stringsAsFactors = FALSE
  )
  user_cols <- c("TN", "x", "y")
  result <- match_metadata_columns(m_df, user_cols)
  # No `if` guard: an empty mapping must fail here, not silently pass with no
  # expectation at all.
  expect_length(result, 1L)
  expect_true("palette" %in% names(result[[1]]))
  expect_equal(result[[1]]$palette, get_default_palette("TN", "Soil", "Total Nitrogen"))
})

test_that("match_metadata_columns reads a Unit column into each variable", {
  m_df <- data.frame(actual = c("pH", "K", "TN"), label = c("Soil pH", "Potassium", "Total N"),
                     cat = "Soil", Unit = c(NA, "mg/kg", " % "), stringsAsFactors = FALSE)
  res <- match_metadata_columns(m_df, c("pH", "K", "TN", "x", "y"))
  expect_equal(vapply(res, `[[`, character(1), "unit"), c("", "mg/kg", "%"))
  # "Units" and a header naming the unit in words are unit columns too.
  names(m_df)[4] <- "Measurement units"
  expect_equal(match_metadata_columns(m_df, c("pH", "K", "TN"))[[2]]$unit, "mg/kg")
})

test_that("a variable list without a Unit column gives every variable an empty unit", {
  # The shipped list's identifier headers must not be read as a unit column,
  # nor a header that merely contains the letters "unit".
  # Headers as the shipped samp_var_list.xlsx has them.
  m_df <- data.frame(`Variable Number (VN)` = c("pH", "K"),
                     `Variable ID (VID)` = c("Soil pH", "K (mg/kg)"),
                     `Variable Category` = "Soil", Community = c("a", "b"),
                     check.names = FALSE, stringsAsFactors = FALSE)
  res <- match_metadata_columns(m_df, c("pH", "K"))
  expect_length(res, 2L)
  expect_equal(vapply(res, `[[`, character(1), "label"), c("Soil pH", "K (mg/kg)"))
  expect_equal(vapply(res, `[[`, character(1), "unit"), c("", ""))
})

test_that("map legend titles carry the unit once", {
  expect_equal(map_legend_title("K", "mg/kg", "var"), "Variance: K (mg/kg)^2")
  expect_equal(map_legend_title("K (mg/kg)", "mg/kg"), "K (mg/kg)")
  expect_equal(map_legend_title("K (MG/KG)", "mg/kg", "se"), "SE: K (MG/KG)")
  expect_equal(map_legend_title("K", "mg/kg"), "K mg/kg")
  # The variance layer always states the squared unit.
  expect_equal(map_legend_title("K (mg/kg)", "mg/kg", "var"), "Variance: K (mg/kg) (mg/kg)^2")
  # A short unit inside a longer word is not a unit the label names.
  expect_equal(map_legend_title("Organic matter", "g"), "Organic matter g")
  expect_equal(map_legend_title("Temperature (°C)", "°C"), "Temperature (°C)")
})

# ── find_subset_column ──────────────────────────────────────────────────────

test_that("find_subset_column detects the partition column case-insensitively", {
  expect_equal(find_subset_column(c("x", "y", "subset")), "subset")
  expect_equal(find_subset_column(c("x", "Subset", "z")), "Subset")
  expect_equal(find_subset_column("SUBSET"), "SUBSET")
  expect_equal(find_subset_column(c("subset", "Subset")), "subset")
})

test_that("find_subset_column returns NA when no partition column exists", {
  expect_true(is.na(find_subset_column(c("x", "y", "value"))))
  expect_true(is.na(find_subset_column(c("subset_id", "my_subset"))))
  expect_true(is.na(find_subset_column(character(0))))
})

# ── default_var_pick ────────────────────────────────────────────────────────

test_that("the Context panel opens on a variable that has predictions", {
  v <- function(actual, category, pred = NA) list(actual = actual, category = category, pred = pred, pred_ss = NA)
  vars <- list(v("elev", "Covariates"), v("clay", "Covariates"),
               v("ph", "Soil", pred = "ph_cve"), v("oc", "Soil"), v("n", "Other", pred = "n_cve"))
  # The first category holding a variable with a prediction column, and in it
  # that variable - not the list's first category, which here is covariates.
  expect_equal(default_var_pick(vars), list(category = "Soil", var = "ph"))
  # Inside a category chosen by the user, the first variable with predictions.
  expect_equal(default_var_pick(vars, "Other")$var, "n")
  # No prediction anywhere: the first category and its first variable.
  plain <- list(v("elev", "Covariates"), v("ph", "Soil"))
  expect_equal(default_var_pick(plain), list(category = "Covariates", var = "elev"))
  expect_equal(default_var_pick(plain, "Soil")$var, "ph")
})
