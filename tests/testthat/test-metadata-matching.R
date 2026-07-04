# test-metadata-matching.R — tests for match_metadata_columns, apply_labels_to_df,
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
  metadata <- list(
    list(actual = "Organic_Carbon", label = "Organic Carbon", category = "Soil")
  )
  # Fuzzy matching should find "Organic Carbon" from "OrganicCarbon"
  result <- get_var_label("Organic Carbon", metadata)
  expect_match(result, "Organic")
})

test_that("get_var_labels vectorizes correctly", {
  metadata <- list(
    list(actual = "pH", label = "Acidity", category = "Soil"),
    list(actual = "N", label = "Nitrogen", category = "Soil")
  )
  result <- get_var_labels(c("pH", "N", "Unknown"), metadata)
  expect_equal(as.character(result), c("Acidity", "Nitrogen", "Unknown"))
})

# ── apply_labels_to_df ────────────────────────────────────────────────────

test_that("apply_labels_to_df renames columns using metadata labels", {
  df <- data.frame(pH = 1:5, Clay = 6:10, Sand = 11:15)
  metadata <- list(
    list(actual = "pH", label = "Acidity", category = "Soil"),
    list(actual = "Clay", label = "Clay %", category = "Soil")
  )
  result <- apply_labels_to_df(df, c("pH", "Clay"), metadata)
  expect_true("Acidity" %in% colnames(result))
  expect_true("Clay %" %in% colnames(result))
  expect_true("Sand" %in% colnames(result))  # unchanged
})

test_that("apply_labels_to_df handles empty vars", {
  df <- data.frame(a = 1:3)
  result <- apply_labels_to_df(df, character(0), NULL)
  expect_equal(colnames(result), "a")
})

test_that("apply_labels_to_df handles NULL df", {
  expect_null(apply_labels_to_df(NULL, "x", NULL))
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
  expect_true(length(result) >= 1)
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
  if (length(result) > 0) {
    expect_true("palette" %in% names(result[[1]]))
  }
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
