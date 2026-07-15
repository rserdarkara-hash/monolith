# resolve_selected_localities (ui_helpers.R): single source of truth for
# turning the sidebar locality selection into the locality set an analysis
# runs on. Pins the ALL / empty / NULL / missing-column edge cases that were
# previously handled inconsistently across ~13 inline snippets in monolith.R.

test_that("explicit selections pass through verbatim", {
  df <- data.frame(loc = c("A", "B", "C", "A"))
  expect_identical(resolve_selected_localities(c("B", "C"), df, "loc"), c("B", "C"))
  expect_identical(resolve_selected_localities("A", df, "loc"), "A")
})

test_that("'ALL' resolves to every locality in the data", {
  df <- data.frame(loc = c("A", "B", "C", "A"))
  expect_identical(resolve_selected_localities("ALL", df, "loc"), c("A", "B", "C"))
  # ALL wins even when combined with explicit picks
  expect_identical(resolve_selected_localities(c("ALL", "B"), df, "loc"), c("A", "B", "C"))
})

test_that("empty and NULL selections resolve to every locality", {
  df <- data.frame(loc = c("A", "B", "A"))
  expect_identical(resolve_selected_localities(character(0), df, "loc"), c("A", "B"))
  expect_identical(resolve_selected_localities(NULL, df, "loc"), c("A", "B"))
})

test_that("NA localities are never returned for ALL-type selections", {
  df <- data.frame(loc = c("A", NA, "B"))
  expect_identical(resolve_selected_localities("ALL", df, "loc"), c("A", "B"))
  expect_identical(resolve_selected_localities(NULL, df, "loc"), c("A", "B"))
})

test_that("missing data or locality column yields character(0), not an error", {
  df <- data.frame(loc = c("A", "B"))
  expect_identical(resolve_selected_localities("ALL", NULL, "loc"), character(0))
  expect_identical(resolve_selected_localities("ALL", df, NULL), character(0))
  expect_identical(resolve_selected_localities("ALL", df, "not_a_column"), character(0))
  expect_identical(resolve_selected_localities(NULL, NULL, NULL), character(0))
})

test_that("factor locality columns keep their values", {
  df <- data.frame(loc = factor(c("A", "B", "A")))
  res <- resolve_selected_localities("ALL", df, "loc")
  expect_setequal(as.character(res), c("A", "B"))
})
