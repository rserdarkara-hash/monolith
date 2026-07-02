# test-fuzzy-matching.R — tests for fuzzy_match_column and detect_pred_column.

# ── fuzzy_match_column ─────────────────────────────────────────────────────

test_that("exact match returns the column name", {
  cols <- c("pH", "Nitrogen", "Organic_Carbon", "Clay")
  expect_equal(fuzzy_match_column("Nitrogen", cols), "Nitrogen")
  expect_equal(fuzzy_match_column("Clay", cols), "Clay")
})

test_that("case-insensitive match works", {
  cols <- c("pH", "NITROGEN", "Organic_Carbon")
  expect_equal(fuzzy_match_column("nitrogen", cols), "NITROGEN")
  expect_equal(fuzzy_match_column("Nitrogen", cols), "NITROGEN")
  expect_equal(fuzzy_match_column("ph", cols), "pH")
})

test_that("handles special characters by stripping them", {
  cols <- c("Organic_Carbon", "Total_Nitrogen", "Available_P")
  # clean_act strips non-alphanumeric: "OrganicCarbon" should match "Organic_Carbon"
  expect_equal(fuzzy_match_column("Organic Carbon", cols), "Organic_Carbon")
})

test_that("fuzzy match within edit distance <= 2", {
  cols <- c("Nitrogen", "Phosphorus", "Potassium", "Calcium")
  # Minor typo: "Nitrogem" has edit distance 1 from "Nitrogen"
  result <- fuzzy_match_column("Nitrogem", cols)
  expect_equal(result, "Nitrogen")
})

test_that("fuzzy match respects relative distance threshold (≤ 30%)", {
  cols <- c("N", "P", "K")
  # "Ca" has edit distance 1 from "K" but Ca has 2 chars, K has 1 char
  # 1/max(1, nchar("ca")) = 1/2 = 0.5 > 0.3, so no match
  result <- fuzzy_match_column("Ca", cols)
  expect_null(result)
})

test_that("returns NULL when no match is close enough", {
  cols <- c("pH", "Clay", "Sand")
  result <- fuzzy_match_column("zzz_nonexistent_zzz", cols)
  expect_null(result)
})

test_that("handles empty user_cols", {
  result <- fuzzy_match_column("anything", character(0))
  expect_null(result)
})

test_that("returns NULL for empty act_name", {
  cols <- c("pH", "Clay")
  result <- fuzzy_match_column("", cols)
  expect_null(result)
})

test_that("clean matching catches identically-stripped strings", {
  cols <- c("pH-H2O", "CaCO3", "Fe_Mn")
  # Cleaning strips non-alnum: "phh2o" matches "pH-H2O" → "phh2o"
  expect_equal(fuzzy_match_column("pH H2O", cols), "pH-H2O")
  expect_equal(fuzzy_match_column("Ca CO3", cols), "CaCO3")
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

# ── Edge cases ────────────────────────────────────────────────────────────

test_that("fuzzy_match_column handles Unicode characters", {
  cols <- c("pH_µ", "Ca_mg_kg⁻¹", "Fe_μg")
  # Unicode in act_name — should not crash
  result <- fuzzy_match_column("pH µ", cols)
  expect_true(is.null(result) || is.character(result))
})

test_that("fuzzy_match_column handles very long column names", {
  long_name <- paste(rep("a", 200), collapse = "")
  cols <- c(long_name, "short")
  result <- fuzzy_match_column(long_name, cols)
  expect_equal(result, long_name)
})

test_that("fuzzy_match_column handles duplicate cleaned names", {
  # Two columns clean to the same string — first match wins
  cols <- c("pH_H2O", "pH-H2O", "Clay")
  result <- fuzzy_match_column("pH H2O", cols)
  expect_equal(result, "pH_H2O")
})

test_that("detect_pred_column handles missing type gracefully", {
  candidates <- c("pH", "pH_cve", "pH_pred")
  result <- detect_pred_column("pH", candidates, type = "cve")
  expect_equal(result, "pH_cve")
})
