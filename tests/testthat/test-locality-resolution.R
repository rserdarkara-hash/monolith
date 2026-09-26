# resolve_selected_localities (ui_helpers.R): single source of truth for
# turning the sidebar locality selection into the locality set an analysis
# runs on. Pins the ALL / empty / NULL / missing-column edge cases that were
# previously handled inconsistently across ~13 inline snippets in monolith.R.

test_that("effective_subset applies only to an available Single-split partition", {
  expect_identical(effective_subset("pred_ss", "Test", c("v", "Subset")), "Test")
  for (view in c("actual", "pred", "resid")) {
    expect_identical(effective_subset(view, "Test", "subset"), "all")
  }
  expect_identical(effective_subset("pred_ss", "Test", "v"), "all")
  expect_identical(effective_subset("pred_ss", "all", "subset"), "all")
  expect_identical(effective_subset("pred_ss", NULL, "subset"), "all")
})

test_that("run_locality_rows filters locality and subset without introducing NA rows", {
  df <- data.frame(loc = c("A", "A", NA, "B", "A"),
                   Subset = c("Test", NA, "Test", "Test", "Train"), id = 1:5)
  expect_identical(run_locality_rows(df, "loc", "A", "Test")$id, 1L)
  expect_identical(run_locality_rows(df, "loc", "A", "all")$id, c(1L, 2L, 5L))
  full <- golden_soil("full")
  locality <- golden_locality("full", "compact", min_n = 30L)
  for (subset in c("Test", "all")) {
    expected <- full[which(full$locality == locality &
      (subset == "all" | full$subset == subset)), , drop = FALSE]
    expect_identical(run_locality_rows(full, "locality", locality, subset), expected)
  }
})

test_that("tuning_key identifies the column and the effective subset", {
  expect_identical(tuning_key("tn", "all"), "tn")
  expect_identical(tuning_key("tn_ss", "Test"), "tn_ss [subset Test]")
  expect_identical(tuning_key("tn", "Test"), "tn [subset Test]")
  for (col in list(NULL, NA_character_, "", character(0), c("tn", "ph"))) {
    expect_identical(tuning_key(col, "all"), NA_character_)
  }
})

test_that("descriptive run vectors respect the committed subset on both surfaces", {
  df <- data.frame(loc = c("A", "A", "B", "A"), subset = c("Test", "Train", "Test", NA),
                   tn = c(2, 100, 200, 300), tn_ss = c(3, 110, 210, 310))
  meta <- list(actual = "tn", pred_ss = "tn_ss", value_type = "pred_ss", subset = "Test")
  expect_identical(stats_table_vectors(df, meta, "loc", "A"), list(act = 2, pre = 3))
})

test_that("analysis locality filter offers a combined view only for multiple localities", {
  local_mocked_bindings(shinyApp = .real_shinyApp, .package = "shiny")
  withr::local_dir(proj_root)
  shiny::testServer(function(input, output, session) {
    rv <- shiny::reactiveValues(loc_names = "A", v_fit_list = list(),
                                user_data = data.frame(loc = c("A", "B", "C")),
                                mapping = list(loc = "loc"))
    # Names the chunk reads from chunks C and F, stubbed to their contract:
    # whether variogram tuning is active, and the tuning store filtered to the
    # current variable and localities.
    sci_vgm_tuning <- shiny::reactiveVal(FALSE)
    tuning_vgm_entries <- function(store) store
    source(file.path(proj_root, "server_run_config.R"), local = TRUE)
  }, {
    html <- output$locality_selector_ui$html
    expect_false(grepl("Total (Combined)", html, fixed = TRUE))
    expect_true(grepl('value="A" selected', html, fixed = TRUE))
    expect_identical(output$sci_multiple_localities, "no")
    rv$loc_names <- c("A", "B")
    session$flushReact()
    expect_true(grepl("Total (Combined)", output$locality_selector_ui$html, fixed = TRUE))
    expect_identical(output$sci_multiple_localities, "yes")

    # A variogram tuning session adds the locality it tuned and leaves the
    # displayed run's list, and with it the Total entry, alone.
    sci_vgm_tuning(TRUE)
    rv$v_fit_list <- list(C_act = "fit")
    session$flushReact()
    html <- output$locality_selector_ui$html
    expect_true(grepl("Total (Combined)", html, fixed = TRUE))
    expect_true(grepl('value="C"', html, fixed = TRUE))
    expect_identical(rv$loc_names, c("A", "B"))
    expect_identical(output$sci_multiple_localities, "yes")
  })
})

test_that("correlation controls follow Context defaults and refresh a calculated screen", {
  local_mocked_bindings(shinyApp = .real_shinyApp, .package = "shiny")
  withr::local_dir(proj_root)
  shiny::testServer(function(input, output, session) {
    rv <- shiny::reactiveValues(user_data = data.frame(
      loc = rep("A", 6), subset = rep(c("Train", "Test"), each = 3),
      actual = 1:6, custom_cve = c(3, 1, 2, 6, 4, 5), custom_ss = c(3, 2, 1, 4, 5, 6), aux = 1:6),
      mapping = list(loc = "loc", vars = list(list(actual = "actual", pred = "custom_cve",
                                                  pred_ss = "custom_ss", category = "Soil", label = "Target"))))
    source(file.path(proj_root, "server_run_config.R"), local = TRUE)
  }, {
    session$setInputs(var_id = "actual", value_type = "actual", locality = "A", subset = "Train")
    session$setInputs(calc_corr = 1)
    expect_identical(corr_ranks()$target, "actual")
    session$setInputs(value_type = "pred")
    expect_identical(corr_ranks()$target, "custom_cve")
    session$setInputs(corr_source = "actual")
    expect_identical(corr_ranks()$target, "actual")
    session$setInputs(value_type = "pred_ss")
    expect_identical(corr_ranks()$target, "custom_ss")
    expect_identical(corr_ranks()$subset, "Train")
    expect_equal(corr_ranks()$results$Corr[corr_ranks()$results$Variable == "aux"], -1)
    session$setInputs(corr_subset = "Test")
    expect_identical(corr_ranks()$subset, "Test")
    expect_equal(corr_ranks()$results$Corr[corr_ranks()$results$Variable == "aux"], 1)
    session$setInputs(subset = "Test")
    expect_identical(corr_subset_value(), "Test")
    session$setInputs(value_type = "actual")
    expect_identical(corr_ranks()$target, "actual")
    expect_equal(corr_ranks()$n, 6L)
    session$setInputs(locality = "B")
    expect_equal(corr_ranks()$n, 0L)
    expect_equal(nrow(corr_ranks()$results), 0L)
    expect_true(grepl("No predictors", output$corr_table_ui$html, fixed = TRUE))
  })
})

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

test_that("the analysis filter adds tuned localities without rewriting the run's list", {
  # No run on screen, two localities tuned: their names only. The combined
  # tables belong to a run, so no Total (Combined) entry.
  expect_identical(sci_locality_choices(NULL, c("Kale", "Tavas")), c("Kale", "Tavas"))
  # A 3-locality run keeps its list and its Total; a tuned locality the run did
  # not cover is appended, one it covered is not repeated.
  run <- c("Altinova", "Kale", "Yorga")
  expect_identical(sci_locality_choices(run, "Tavas"),
                   c("Total (Combined)", run, "Tavas"))
  expect_identical(sci_locality_choices(run, "Kale"), c("Total (Combined)", run))
  # A 1-locality run with nothing tuned: that locality alone.
  expect_identical(sci_locality_choices("Kale", character(0)), "Kale")
  expect_identical(sci_locality_choices(NULL, NULL), character(0))
})
