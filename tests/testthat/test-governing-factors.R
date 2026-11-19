# compute_governing_factors() (spatial_pipeline.R) and the Governing Factors
# summary table.

test_that("compute_governing_factors returns NULL for fewer than 10 rows", {
  df <- make_test_df(9)
  result <- compute_governing_factors(df, "a", c("b", "c", "d"))
  expect_null(result)
})

test_that("the returned list is the shape the Governing Factors panel reads", {
  # One fit, read every way the panel reads it.
  df <- make_test_df(30)
  predictors <- c("b", "c", "d")
  result <- compute_governing_factors(df, "a", predictors)

  expect_type(result, "list")
  expect_setequal(names(result),
                  c("model", "explainer", "importance", "top_var",
                    "ale", "pdp", "shap", "n_used", "n_total"))
  expect_s3_class(result$model, "randomForest")
  expect_s3_class(result$explainer, "explainer")

  expect_s3_class(result$importance, "data.frame")
  expect_gte(nrow(result$importance), 1L)
  expect_true(all(c("variable", "mse_increase") %in% colnames(result$importance)))
  expect_setequal(as.character(result$importance$variable), predictors)

  expect_true(result$top_var %in% predictors)
  expect_s3_class(result$ale, "data.frame")
  expect_s3_class(result$pdp, "data.frame")
  expect_s3_class(result$shap, "data.frame")
  expect_true(all(c("feature_value", "contribution") %in% colnames(result$shap)))
})

test_that("compute_governing_factors reports the complete-case sample it fitted", {
  df <- make_test_df(40)
  df$b[1:6] <- NA         # missing in a predictor
  df$a[7:9] <- NA         # missing in the target
  result <- compute_governing_factors(df, "a", c("b", "c", "d"))
  expect_equal(result$n_total, 40)
  # The forest is fitted on complete cases across target + predictors, and the
  # panel prints that sample, so the two must agree by construction.
  expect_equal(result$n_used, sum(stats::complete.cases(df[, c("a", "b", "c", "d")])))
  expect_lt(result$n_used, result$n_total)
})

test_that("governing-factors importance is the forest's out-of-bag permutation importance", {
  df <- make_test_df(30)
  result <- compute_governing_factors(df, "a", c("b", "c"), rf_ntree = 30)

  # Breiman (2001): per tree, the increase in out-of-bag MSE when a predictor
  # is permuted, averaged over the trees and NOT divided by its standard error.
  # The reference forest is grown here, independently, with the same seed.
  ref_fit <- with_seed(12345, randomForest::randomForest(
    x = df[, c("b", "c")], y = df$a, ntree = 30, importance = TRUE))
  ref <- randomForest::importance(ref_fit, type = 1, scale = FALSE)
  expect_equal(result$importance$variable, rownames(ref))
  expect_equal(result$importance$mse_increase, unname(ref[, 1]))
  # It is the unscaled column, not randomForest's default scaled one, which is
  # reported beside it: the same increase divided by its standard error.
  scaled <- unname(randomForest::importance(ref_fit, type = 1)[, 1])
  expect_false(isTRUE(all.equal(result$importance$mse_increase, scaled)))
  expect_equal(result$importance$mse_increase_scaled, scaled)
  expect_equal(result$importance$mse_increase_scaled,
               unname(ref[, 1] / ref_fit$importanceSD[rownames(ref)]))
})

test_that("out-of-bag importance gives a pure-noise predictor no credit", {
  # Permuting on the rows the forest was grown on credits a noise predictor
  # with what the forest memorised; the out-of-bag rows do not.
  set.seed(7)
  n <- 200
  d <- data.frame(x1 = runif(n), x2 = runif(n))
  d$y <- 3 * d$x1 + rnorm(n, 0, 0.3)
  res <- compute_governing_factors(d, "y", c("x1", "x2"), rf_ntree = 200, shap_sample_size = 20)
  imp <- stats::setNames(res$importance$mse_increase, res$importance$variable)
  expect_identical(res$top_var, "x1")
  expect_lt(imp[["x2"]], 0.02 * imp[["x1"]])
})

test_that("compute_governing_factors handles a single predictor", {
  df <- make_test_df(30)
  result <- compute_governing_factors(df, "a", "b")
  expect_equal(result$top_var, "b")
  expect_setequal(as.character(result$importance$variable), "b")
})

test_that("compute_governing_factors removes rows with NAs in target/predictors", {
  df <- make_test_df(30)
  df$a[1:5] <- NA
  result <- compute_governing_factors(df, "a", c("b", "c"))
  # The five NA-target rows are dropped from the fit and counted out of n_used,
  # while n_total still reports the sample the user supplied.
  expect_equal(result$n_total, 30)
  expect_equal(result$n_used, 25)
  # And the forest really was grown on those 25 rows, not on 30 with NAs.
  expect_length(result$model$predicted, 25)
})

test_that("compute_governing_factors SHAP values are reproducible across calls", {
  df <- make_test_df(30)
  r1 <- compute_governing_factors(df, "a", c("b", "c"))
  r2 <- compute_governing_factors(df, "a", c("b", "c"))
  expect_equal(r1$shap, r2$shap)
})

test_that("SHAP dependence contributions have per-observation magnitude (T14)", {
  df <- make_test_df(30)
  result <- compute_governing_factors(df, "a", "b")
  # With a single predictor every SHAP permutation attributes the full
  # deviation to that variable, so contribution(i) = f(x_i) - mean(f(X))
  # exactly; adding predict_parts' aggregate row to its B permutation rows
  # would inflate it B + 1 times.
  preds <- predict(result$model, newdata = df)
  set.seed(12345)
  sample_idx <- sample(seq_len(nrow(df)), min(100, nrow(df)))
  expected <- preds[sample_idx] - mean(preds)
  expect_equal(result$shap$contribution, unname(expected), tolerance = 1e-6)
})

test_that("compute_governing_factors does not perturb the caller's RNG (T19)", {
  df <- make_test_df(30)
  set.seed(123); expected_draw <- runif(1)
  set.seed(123)
  invisible(compute_governing_factors(df, "a", c("b", "c")))
  actual_draw <- runif(1)
  expect_equal(actual_draw, expected_draw)
})

# ── Cooperative cancellation ────────────────────────────────────────────────

test_that("a failed SHAP cluster start restores mc.cores", {
  withr::local_options(mc.cores = 1L)
  testthat::with_mocked_bindings({
    expect_error(
      compute_governing_factors(make_test_df(60), "a", c("b", "c", "d"),
                                rf_ntree = 10,
                                shap_sample_size = 50, cores_hint = 3),
      "forced cluster startup failure")
    expect_identical(getOption("mc.cores"), 1L)
  }, makeClusterPSOCK = function(...) stop("forced cluster startup failure"),
  .package = "parallelly")
})

test_that("with_nested_cluster runs on its own cluster and hands the worker back", {
  withr::local_options(mc.cores = 1L)
  expect_true(inherits(future::plan(), "sequential"))
  # Below two workers neither the plan nor mc.cores is touched.
  expect_equal(with_nested_cluster(1L, future::nbrOfWorkers()), 1)
  expect_identical(with_nested_cluster(0L, getOption("mc.cores")), 1L)
  # Two workers: the expression runs on them with mc.cores set, and the plan
  # and mc.cores are back afterwards, after an error too.
  seen <- with_nested_cluster(2L, list(workers = future::nbrOfWorkers(), mc = getOption("mc.cores")))
  expect_equal(seen$workers, 2)
  expect_identical(seen$mc, 2L)
  expect_identical(getOption("mc.cores"), 1L)
  expect_true(inherits(future::plan(), "sequential"))
  expect_error(with_nested_cluster(2L, stop("inside the map")), "inside the map")
  expect_identical(getOption("mc.cores"), 1L)
  expect_true(inherits(future::plan(), "sequential"))
})

test_that("compute_governing_factors aborts when the cancel flag is set", {
  df <- make_test_df(60)
  preds <- c("b", "c", "d")
  cancel_file <- tempfile(fileext = ".txt")
  file.create(cancel_file)
  on.exit(unlink(cancel_file), add = TRUE)

  # The first checkpoint runs before the random forest is fitted, so a flag
  # that is already set must abort essentially immediately.
  expect_error(
    compute_governing_factors(df, "a", preds,
                              rf_ntree = 10, shap_sample_size = 10,
                              cancel_file = cancel_file),
    "cancelled by user")
})

test_that("compute_governing_factors is unchanged when no cancel file is given", {
  df <- make_test_df(60)
  preds <- c("b", "c", "d")
  missing_flag <- file.path(tempdir(), "gov_cancel_never_created.txt")
  unlink(missing_flag)

  a <- compute_governing_factors(df, "a", preds,
                                 rf_ntree = 20, shap_sample_size = 10)
  b <- compute_governing_factors(df, "a", preds,
                                 rf_ntree = 20, shap_sample_size = 10,
                                 cancel_file = missing_flag)
  expect_equal(a$importance, b$importance)
  expect_equal(a$shap, b$shap)
  expect_identical(a$top_var, b$top_var)
})

# ── Numeric contract: what the explanation plots actually show ─────────────

test_that("the PDP is the mean prediction with the feature held fixed", {
  d <- sf::st_drop_geometry(golden_sf("core", localities = golden_locality("core", "smallest", min_n = 30L)))[
    , c("ph", "v82", "v87", "v43")]
  res <- compute_governing_factors(d, "ph", c("v82", "v87", "v43"),
                                   rf_ntree = 60,
                                   shap_sample_size = 40)
  expect_equal(res$n_used, nrow(d))

  # Friedman (2001): the partial dependence at v is the average prediction with
  # the feature set to v across the whole sample. Recomputed here from the
  # returned forest, on all rows - DALEX samples at most N = 100 rows and this
  # fixture has 40, so the two see the same data.
  xs <- res$pdp[["_x_"]]
  ref <- vapply(xs, function(v) {
    dd <- d[, c("v82", "v87", "v43")]
    dd[[res$top_var]] <- v
    mean(predict(res$model, dd))
  }, numeric(1))

  expect_gt(length(xs), 5L)
  expect_equal(res$pdp[["_yhat_"]], ref, tolerance = 1e-8)
})

test_that("the SHAP sample is the documented deterministic draw", {
  d <- sf::st_drop_geometry(golden_sf("core", localities = golden_locality("core", "smallest", min_n = 30L)))[
    , c("ph", "v82", "v87", "v43")]
  res <- compute_governing_factors(d, "ph", c("v82", "v87", "v43"),
                                   rf_ntree = 60,
                                   shap_sample_size = 25)

  # The sampled rows come from a fixed seed, so the plotted feature values are
  # reproducible and are the sampled rows' own values - not a re-sort, not a
  # different subset.
  idx <- with_seed(12345, sample(seq_len(nrow(d)), 25))
  expect_equal(res$shap$feature_value, d[[res$top_var]][idx])
  expect_equal(nrow(res$shap), 25L)
})

test_that("governing factors accept column names that are not syntactic", {
  # randomForest's formula method re-reads names through make.names(); a
  # header with units or a space ("Total N (%)", "soil moisture") must give
  # the results of its syntactic twin, under its own name.
  d <- make_test_df(40)
  plain <- compute_governing_factors(d, "a", c("b", "c"), rf_ntree = 30, shap_sample_size = 15)
  names(d)[names(d) == "a"] <- "Total N (%)"
  names(d)[names(d) == "b"] <- "soil moisture"
  odd <- compute_governing_factors(d, "Total N (%)", c("soil moisture", "c"),
                                   rf_ntree = 30, shap_sample_size = 15)
  expect_equal(odd$importance$mse_increase, plain$importance$mse_increase)
  expect_equal(odd$importance$variable, c("soil moisture", "c"))
  expect_equal(odd$shap, plain$shap)
  expect_equal(odd$pdp[["_yhat_"]], plain$pdp[["_yhat_"]])
  expect_identical(odd$top_var, c(b = "soil moisture", c = "c")[[plain$top_var]])
})


# ── The Tabular Data Metrics table ─────────────────────────────────────────

test_that("the governing-factors table keeps full precision and names its units", {
  res <- list(
    importance = data.frame(variable = c("som", "clay", "sand"),
                            mse_increase = c(2.001495588850122, 0.5, 1.25),
                            mse_increase_scaled = c(14.2, 3.1, 8.7)),
    model = list(rsq = c(0.10, 0.4213)))
  df <- gov_summary_df(res, NULL)

  expect_equal(names(df), c("Governing Factor / Metric", "Value", "Unit", GOV_SCALED_COL))
  # The scaled column travels with its factor; the model-quality row has none.
  expect_equal(df[[GOV_SCALED_COL]], c(NA, 14.2, 8.7, 3.1))
  # The model-quality row comes first, and each row states its own unit.
  expect_match(df[[1]][1], "OOB variance explained", fixed = TRUE)
  expect_equal(df$Value[1], 42.13)
  expect_equal(df$Unit[1], "% of variance (out-of-bag)")
  expect_true(all(df$Unit[-1] == GOV_IMPORTANCE_UNIT))
  expect_equal(length(unique(df$Unit)), 2)

  # Importances are the model's values, in decreasing order, at full precision;
  # formatting is the display's job.
  expect_equal(df$Value[-1], sort(res$importance$mse_increase, decreasing = TRUE))
  expect_equal(df[[1]][-1], c("som", "sand", "clay"))
  expect_equal(format_sig(df$Value[2]), "2.001")

  # Without a usable OOB figure the table is the importance rows alone.
  bare <- gov_summary_df(list(importance = res$importance, model = list(rsq = NA_real_)), NULL)
  expect_equal(nrow(bare), 3)
  expect_null(gov_summary_df(NULL))

  # The module keeps only the OOB figure and drops the forest and the DALEX
  # explainer once a run lands, so the table has to read either shape.
  kept <- gov_summary_df(list(importance = res$importance, oob_rsq = 0.4213), NULL)
  expect_equal(kept$Value[1], 42.13)
  expect_equal(kept[[1]], df[[1]])

  # Factors are named with the suite's display labels; a label two columns
  # share carries its column name, as on the other analytics tabs.
  labs <- desc_var_labels(c("som", "clay", "sand"), list(
    list(actual = "som", label = "Organic matter"),
    list(actual = "clay", label = "Texture"),
    list(actual = "sand", label = "Texture")))
  named <- gov_summary_df(res, labs)
  expect_equal(named[[1]][-1], c("Organic matter", "Texture [sand]", "Texture [clay]"))
})


# ── The Governing Factors tab ──────────────────────────────────────────────

test_that("Governing Factors is fitted on the active groups and offers every numeric column", {
  d <- golden_soil("core")
  first <- golden_locality("core", "smallest")
  active <- c(first, golden_locality("core", "largest", exclude = first))
  seen <- new.env()
  # The tab's module, replaced by a recorder of what the Analytics Engine
  # hands it.
  rlang::local_bindings(
    gov_factors_server = function(id, data_reactive, rows_reactive, vars_metadata_reactive) {
      seen$data <- data_reactive
      seen$rows <- rows_reactive
    },
    .env = environment(desc_exploratory_server))
  shiny::testServer(desc_exploratory_server, args = list(
    data_reactive = shiny::reactive(d), vars_metadata_reactive = shiny::reactive(NULL)
  ), {
    session$setInputs(analytics_group_vars = "locality", grp_type_1 = "categorical",
                      analytics_active_group = active)
    rows <- seen$rows()
    expect_lt(nrow(rows), nrow(d))
    expect_equal(nrow(rows), sum(d$locality %in% active))
    expect_setequal(unique(as.character(rows$locality)), active)
    # The choices are the table's own columns, whatever the grouping.
    expect_identical(seen$data(), d)
  })
})

test_that("a run counts the active groups' rows against the 50-row minimum", {
  # 60 complete rows in the table, 40 in the active groups: the run is refused.
  d <- make_test_df(60)
  shiny::testServer(gov_factors_server, args = list(
    data_reactive = shiny::reactive(d), rows_reactive = shiny::reactive(d[1:40, ]),
    vars_metadata_reactive = shiny::reactive(NULL)
  ), {
    session$setInputs(gov_target = "a", gov_predictors = c("b", "c"), gov_run_btn = 1)
    expect_identical(gov_rv$ready, "no")
    expect_null(gov_rv$res)
  })
})

test_that("the target and factor picks survive a Variable naming switch", {
  d <- make_test_df(30)
  meta <- shiny::reactiveVal(NULL)
  shiny::testServer(gov_factors_server, args = list(
    data_reactive = shiny::reactive(d), rows_reactive = shiny::reactive(d),
    vars_metadata_reactive = meta
  ), {
    session$setInputs(gov_target = "b", gov_predictors = c("c", "d"))
    expect_match(output$gov_target_ui$html, "<option value=\"b\" selected>b</option>", fixed = TRUE)
    meta(list(list(actual = "b", label = "Bee"), list(actual = "c", label = "Sea")))
    session$flushReact()
    target <- output$gov_target_ui$html
    factors <- output$gov_predictors_ui$html
    expect_match(target, "<option value=\"b\" selected>Bee</option>", fixed = TRUE)
    expect_match(factors, "<option value=\"c\" selected>Sea</option>", fixed = TRUE)
    expect_match(factors, "<option value=\"d\" selected>d</option>", fixed = TRUE)
    expect_no_match(factors, "<option value=\"b\" selected>", fixed = TRUE)
  })
})

test_that("the importance chart keeps its ticks, notes and labels apart on a narrow card", {
  # Values whose round ticks fall at the axis ends (0.15 and 50).
  res <- list(importance = data.frame(variable = c("som", "v1", "v12"),
                                      mse_increase = c(0.135, 0.08, 0.06),
                                      mse_increase_scaled = c(42, 20, 12)),
              n_used = 980L, n_total = 1035L)
  labels <- desc_var_labels(c("som", "v1", "v12"),
                            list(list(actual = "v1", label = "Annual Mean Temperature (°C)")))
  p <- build_gov_importance_plot(res, labels)
  b <- ggplot2::ggplot_build(p)

  # Per panel: zero and at most three other ticks, none in the last 10% of
  # the axis, where its label would run into the next panel or off the plot.
  at_end <- function(br, rng) br > rng[1] + 0.9 * diff(rng) & br <= rng[2]
  for (pp in b$layout$panel_params) {
    br <- pp$x$get_breaks()
    br <- br[!is.na(br)]
    rng <- pp$x$continuous_range
    # Premise: the default ticks of this axis put one there.
    expect_true(any(at_end(scales::breaks_extended(n = 4)(rng), rng)))
    expect_true(0 %in% br)
    expect_lte(length(br), 4)
    expect_false(any(at_end(br[br != 0], rng)))
  }
  expect_equal(b$layout$panel_params[[1]]$x$get_labels()[1:3], c("0", "0.05", "0.1"))

  # The factors in increasing importance, under their display labels, with
  # no axis titles beside them.
  expect_equal(b$layout$panel_params[[1]]$y$get_labels(),
               c("v12", "Annual Mean Temperature (°C)", "som"))
  expect_null(ggplot2::get_labs(p)$y)
  expect_null(ggplot2::get_labs(p)$x)

  # Notes wrapped to the card, starting with the title at the plot's left edge.
  notes <- ggplot2::get_labs(p)[c("subtitle", "caption")]
  for (note in notes) expect_true(all(nchar(strsplit(note, "\n")[[1]]) < IMPORTANCE_NOTE_WIDTH))
  expect_equal(gsub("\n", " ", notes$subtitle), complete_case_note(980L, 1035L))
  expect_identical(p$theme$plot.title.position, "plot")
  expect_identical(p$theme$plot.caption.position, "plot")
  expect_equal(p$theme$plot.caption$hjust, 0)

  # Without the scaled measure the chart has one panel and no note about it.
  bare <- build_gov_importance_plot(list(importance = res$importance[c("variable", "mse_increase")]))
  expect_equal(nrow(ggplot2::ggplot_build(bare)$layout$layout), 1L)
  expect_null(ggplot2::get_labs(bare)$caption)
  expect_null(ggplot2::get_labs(bare)$subtitle)
})
