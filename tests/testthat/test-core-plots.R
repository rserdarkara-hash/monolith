# test-core-plots.R — tests for generate_core_plot, generate_ghosted_plot and
# generate_advanced_plot, plus the RF-importance, variogram and sci_dt builders.

df <- make_test_df(30)

# ── The three plot builders ────────────────────────────────────────────────
#
# expect_s3_class(p, "ggplot") does not build the plot. ggplot() is lazy, so an
# aesthetic naming a column that does not exist survives the class check and
# fails only when the panel is drawn. ggplot_build() is what turns
# "constructed" into "renders", so these sweep the plot types through it.

build_ok <- function(p) expect_no_error(suppressWarnings(ggplot2::ggplot_build(p)))
placeholder_label <- function(p) ggplot2::ggplot_build(p)$data[[1]]$label

test_that("every core plot type renders", {
  for (pt in c("histogram", "density", "boxplot", "violin", "scatter", "ecdf")) {
    p <- generate_core_plot(df, "a", group_col = "cat1", plot_type = pt)
    expect_s3_class(p, "ggplot")
    build_ok(p)
  }

  # No group column at all: the builder substitutes a single "All" level
  # rather than leaving an aesthetic pointing at a column that is not there.
  p <- generate_core_plot(df, "a", plot_type = "histogram")
  build_ok(p)
  expect_identical(levels(ggplot2::ggplot_build(p)$plot$data$group_id), "All")

  # A second variable sends boxplot and violin through the pivot_longer branch,
  # which draws one facet per variable.
  for (pt in c("boxplot", "violin")) {
    p <- generate_core_plot(df, "a", y_var = "b", group_col = "cat1", plot_type = pt)
    build_ok(p)
    expect_equal(nrow(ggplot2::ggplot_build(p)$layout$layout), 2L)
  }
})

test_that("every scatter fit renders, with and without a y variable", {
  # This path used to wrap the call in tryCatch and convert any error into
  # skip("Known .data pronoun bug in scatter+fit path"). The aes() defers
  # .data[[x_col]] and resolves x_col from the enclosing frame, so the path
  # does not error and the skip absorbed nothing - which under a SKIP 0 policy
  # means a future regression here would have been reported as a skip.
  for (fit in c("none", "linear", "loess", "polynomial", "gam")) {
    build_ok(generate_core_plot(df, "a", y_var = "b", plot_type = "scatter",
                                scatter_fit = fit))
    # Without y_var the fit is taken against the row index instead, which is
    # the branch that resolves x_col to "index_seq".
    build_ok(generate_core_plot(df, "a", plot_type = "scatter", scatter_fit = fit))
  }

  # A fit really was added, and "none" really adds nothing.
  n_layers <- function(fit) {
    length(generate_core_plot(df, "a", y_var = "b", plot_type = "scatter",
                              scatter_fit = fit)$layers)
  }
  expect_equal(n_layers("linear"), n_layers("none") + 1L)
})

test_that("every ghosted plot type renders over its global backdrop", {
  df_local <- df[df$cat1 == "Low", ]
  for (pt in c("histogram", "density", "boxplot", "violin", "scatter", "ecdf")) {
    p <- generate_ghosted_plot(df, df_local, "a", group_col = "cat1", plot_type = pt)
    expect_s3_class(p, "ggplot")
    build_ok(p)
    # The ghost IS the point: a grey global layer under a coloured local one.
    expect_gte(length(p$layers), 2L)
  }
  build_ok(generate_ghosted_plot(df, df_local, "a", y_var = "b",
                                 group_col = "cat1", plot_type = "scatter"))
  # No group column: both frames get the same substituted level.
  build_ok(generate_ghosted_plot(df, df_local, "a", plot_type = "histogram"))
})

test_that("every advanced plot type renders", {
  cases <- list(
    list(plot_type = "qq",              vars = "a"),
    list(plot_type = "sinaplot",        vars = "a"),
    list(plot_type = "sinaplot",        vars = c("a", "b")),
    list(plot_type = "ridge",           vars = "a"),
    list(plot_type = "density_heatmap", vars = c("a", "b")),
    list(plot_type = "parallel",        vars = c("a", "b", "c", "d")),
    list(plot_type = "radar",           vars = c("a", "b", "c"))
  )
  for (cs in cases) {
    p <- generate_advanced_plot(df, vars = cs$vars, group_col = "cat1",
                                plot_type = cs$plot_type)
    expect_s3_class(p, "ggplot")
    build_ok(p)
  }

  # Each surface fit is a separate model call whose prediction feeds the tiles.
  for (fit in c("linear", "loess", "polynomial", "gam", "tps")) {
    # mgcv's own contrasts = / contrasts.arg notice; see helper.R.
    p <- without_partial_match_notices(
      generate_advanced_plot(df, vars = c("a", "b", "c"),
                             plot_type = "xyz_surface", xyz_fit = fit))
    build_ok(p)
    # One filled tile per prediction-grid cell. A fit that failed draws the
    # single-row "Model fitting failed" notice instead, so the count is what
    # separates a surface from an excuse.
    expect_equal(nrow(ggplot2::ggplot_build(p)$data[[1]]), 50L * 50L, info = fit)
  }
})

test_that("a selection too small for the plot draws its own explanation", {
  # These names used to promise a requirement the bodies never checked: the
  # radar test passed two variables and asserted success, and the density
  # heatmap test passed exactly two and asserted nothing about the rule.
  expect_match(placeholder_label(generate_advanced_plot(df, vars = c("a", "b"),
                                                        plot_type = "radar")),
               "Radar requires at least 3 variables with observed values", fixed = TRUE)
  expect_match(placeholder_label(generate_advanced_plot(df, vars = "a",
                                                        plot_type = "density_heatmap")),
               "requires two numeric variables", fixed = TRUE)
  expect_match(placeholder_label(generate_advanced_plot(df, vars = "a",
                                                        plot_type = "parallel")),
               "requires at least 2 variables with observed values", fixed = TRUE)
  expect_match(placeholder_label(generate_advanced_plot(df, vars = c("a", "b"),
                                                        plot_type = "xyz_surface")),
               "requires 3 numeric variables", fixed = TRUE)

  # And one more variable draws the plot itself, not the notice.
  expect_gt(length(generate_advanced_plot(df, vars = c("a", "b", "c"),
                                          plot_type = "radar")$layers), 1L)
  expect_gt(length(generate_advanced_plot(df, vars = c("a", "b"),
                                          plot_type = "density_heatmap")$layers), 0L)
})

# ── Scientific Analysis naming radio: RF importance + CK id relabeling ──────

test_that("build_rf_importance_plot maps covariate names through metadata", {
  set.seed(42)
  df_rf <- data.frame(a = rnorm(30), b = rnorm(30))
  df_rf$y <- df_rf$a + rnorm(30, sd = 0.1)
  # randomForest's own seq(along = ) notice; see helper.R.
  rf <- without_partial_match_notices(
    randomForest::randomForest(y ~ a + b, data = df_rf, ntree = 25))
  meta <- list(list(actual = "a", label = "Alpha"), list(actual = "b", label = "Beta"))

  p_lab <- build_rf_importance_plot(rf, "T", meta)
  expect_s3_class(p_lab, "ggplot")
  b_lab <- ggplot_build(p_lab)
  y_labels <- unlist(lapply(b_lab$layout$panel_params, function(pp) pp$y$get_labels()))
  expect_true(all(c("Alpha", "Beta") %in% y_labels))

  p_raw <- build_rf_importance_plot(rf, "T", NULL)
  b_raw <- ggplot_build(p_raw)
  y_raw <- unlist(lapply(b_raw$layout$panel_params, function(pp) pp$y$get_labels()))
  expect_true(all(c("a", "b") %in% y_raw))
  expect_false(any(c("Alpha", "Beta") %in% y_raw))

  # A forest grown with importance = TRUE gets one panel per measure, the
  # unscaled increase first, and the strongest covariate at the top.
  rf_imp <- without_partial_match_notices(
    randomForest::randomForest(y ~ a + b, data = df_rf, ntree = 25, importance = TRUE))
  b_imp <- ggplot_build(build_rf_importance_plot(rf_imp, "T", NULL))
  expect_equal(levels(b_imp$layout$layout$Measure), unname(RF_IMPORTANCE_LABELS))
  expect_equal(tail(b_imp$layout$panel_params[[1]]$y$get_labels(), 1), "a")
})

test_that("relabel_ck_variogram renames direct and cross ids consistently", {
  vm <- data.frame(id = factor(c("v", "cov1", "v.cov1"),
                               levels = c("v", "cov1", "v.cov1")))
  model <- setNames(list("m1", "m2", "m3"), c("v", "cov1", "v.cov1"))
  out <- relabel_ck_variogram(vm, model, c(v = "Zinc", cov1 = "Clay Content"))
  expect_equal(levels(out$vm$id), c("Zinc", "Clay Content", "Zinc × Clay Content"))
  expect_equal(names(out$model), c("Zinc", "Clay Content", "Zinc × Clay Content"))
})

test_that("relabel_ck_variogram matches dotted ids exactly and keeps unknowns", {
  # An id containing a dot must not be mis-split into fake components.
  vm <- data.frame(id = factor(c("Ca.Mg", "other"), levels = c("Ca.Mg", "other")))
  out <- relabel_ck_variogram(vm, NULL, c("Ca.Mg" = "Ca/Mg Ratio"))
  expect_equal(levels(out$vm$id), c("Ca/Mg Ratio", "other"))

  # Duplicate labels stay unique so lattice panels cannot collapse.
  vm2 <- data.frame(id = factor(c("a", "b"), levels = c("a", "b")))
  out2 <- relabel_ck_variogram(vm2, NULL, c(a = "Same", b = "Same"))
  expect_equal(anyDuplicated(levels(out2$vm$id)), 0L)
})

# ── ggplot variogram builders (lattice replacements; same numbers) ──────────

test_that("build_variogram_ggplot draws empirical points and the fitted variogramLine", {
  v_emp <- data.frame(np = c(20, 40, 60), dist = c(50, 150, 300),
                      gamma = c(0.2, 0.6, 0.9))
  v_fit <- gstat::vgm(psill = 0.7, model = "Sph", range = 250, nugget = 0.2)

  p <- build_variogram_ggplot(v_emp, v_fit, title = "T", subtitle = "S")
  expect_s3_class(p, "ggplot")

  line_layers <- Filter(function(l) inherits(l$geom, "GeomLine"), p$layers)
  expect_equal(length(line_layers), 1)
  expected <- gstat::variogramLine(v_fit, maxdist = max(v_emp$dist))
  expect_equal(line_layers[[1]]$data$gamma, expected$gamma)

  # manual overlay adds a second (dashed) line with variogramLine numbers
  v_man <- gstat::vgm(psill = 0.5, model = "Exp", range = 100, nugget = 0.1)
  p_man <- build_variogram_ggplot(v_emp, v_fit, title = "T", manual_model = v_man)
  man_layers <- Filter(function(l) inherits(l$geom, "GeomLine"), p_man$layers)
  expect_equal(length(man_layers), 2)
  expected_man <- gstat::variogramLine(v_man, maxdist = max(v_emp$dist))
  expect_equal(man_layers[[2]]$data$gamma, expected_man$gamma)

  # no fit: points only, and a NULL/empty empirical variogram returns NULL
  p_nofit <- build_variogram_ggplot(v_emp, NULL, title = "T")
  expect_equal(length(Filter(function(l) inherits(l$geom, "GeomLine"), p_nofit$layers)), 0)
  expect_null(build_variogram_ggplot(NULL))
})

test_that("build_ck_variogram_ggplot facets by id and matches models by name", {
  vm <- data.frame(id = factor(rep(c("v", "cov1", "v.cov1"), each = 3),
                               levels = c("v", "cov1", "v.cov1")),
                   np = rep(c(10, 20, 30), 3),
                   dist = rep(c(50, 150, 300), 3),
                   gamma = c(0.2, 0.5, 0.8, 0.1, 0.3, 0.4, 0.05, 0.15, 0.2))
  model <- list(v = gstat::vgm(0.7, "Sph", 250, 0.1),
                cov1 = gstat::vgm(0.4, "Sph", 250, 0.05))

  p <- build_ck_variogram_ggplot(vm, model, title = "CK")
  expect_s3_class(p, "ggplot")
  expect_true("id" %in% names(ggplot2::ggplot_build(p)$layout$facet$params$facets))

  line_layers <- Filter(function(l) inherits(l$geom, "GeomLine"), p$layers)
  expect_equal(length(line_layers), 1)
  # lines exist only for ids present in the model list (no v.cov1 line)
  expect_setequal(as.character(unique(line_layers[[1]]$data$id)), c("v", "cov1"))
  expect_equal(levels(line_layers[[1]]$data$id), levels(vm$id))
})

test_that("sci_dt header tooltips attach title attributes to matching headers only", {
  df <- data.frame(RMSE = 1.2, Foo = "x", check.names = FALSE)
  dt <- sci_dt(df, header_tooltips = c(RMSE = "Root mean square error."))
  html <- as.character(dt$x$container)
  expect_true(grepl('title="Root mean square error."', html, fixed = TRUE))
  expect_true(grepl(">Foo</th>", html, fixed = TRUE))
  expect_false(grepl('Foo</th>.*title=', html))

  # every Model Performance column except the escaped Moran spans has a tooltip
  tips <- sci_metric_tooltips()
  perf_cols <- c("Source", "RMSE", "R² (Corr)", "R² (NSE/Trad)", "Bias (ME)",
                 "RPD (Prec)", "SMAPE (%)", "Moran's I", "Moran p")
  expect_true(all(perf_cols %in% names(tips)))
})

test_that("sci_dt(NULL) renders an empty state and never a NULL payload", {
  # DT's binding reads `data.lazyRender` BEFORE its own `data === null` branch,
  # so a NULL payload arriving at a hidden table throws a TypeError inside
  # Shiny's async message dispatch and every remaining output in that batch is
  # dropped. The Scientific Analysis tables render eagerly into a hidden tab,
  # which is exactly that situation, so sci_dt must never hand DT a NULL.
  dt <- sci_dt(NULL)
  expect_s3_class(dt, "datatables")
  expect_equal(nrow(dt$x$data), 1)
  expect_equal(unname(as.character(dt$x$data[[1]])), "No data for this selection.")

  # Header tooltips are dropped with the data: the custom container is built
  # from the caller's column names, which the empty state does not have.
  dt_tips <- sci_dt(NULL, header_tooltips = c(RMSE = "Root mean square error."))
  expect_s3_class(dt_tips, "datatables")
  expect_equal(nrow(dt_tips$x$data), 1)
})
test_that("normalised plots preserve missing values and name unavailable dimensions", {
  d <- data.frame(a = 1:6, b = c(2, NA, 4, 7, 3, 1), constant = c(4, NA, 4, 4, 4, 4),
                  absent = rep(NA_real_, 6), group_id = factor(rep(c("A", "B"), each = 3)))
  for (type in c("parallel", "radar")) {
    p <- generate_advanced_plot(d, c("a", "b", "constant", "absent"), "group_id", type)
    expect_true(all(is.na(p$data$value[p$data$variable == "absent"])))
    expect_match(p$labels$caption, "absent")
    expect_true(all(na.omit(p$data$value[p$data$variable == "constant"]) == 0))
    if (type == "parallel") expect_true(is.na(p$data$value[p$data$variable == "constant" & p$data$id == 2]))
  }
  for (type in c("parallel", "radar")) {
    p <- generate_advanced_plot(d, c("a", "absent"), "group_id", type)
    expect_match(p$layers[[1]]$aes_params$label, "observed")
  }
})

test_that("descriptive plot labels and final themes reflect the selected context", {
  d <- make_test_df(30)
  labels <- c(a = "Measurement A", b = "Measurement B", c = "Measurement C")
  for (type in c("qq", "parallel", "radar", "xyz_surface")) {
    p <- generate_advanced_plot(d, c("a", "b", "c"), "cat1", type,
                                labels = labels, group_label = "Treatment")
    built <- ggplot2::ggplot_build(p)
    expect_s3_class(built$plot$theme$panel.background, "element_blank")
    if (type != "xyz_surface") expect_equal(ggplot2::get_labs(p)$colour, "Treatment")
    if (type %in% c("parallel", "radar")) {
      expect_equal(built$layout$panel_scales_x[[1]]$get_labels(), unname(labels))
    }
  }
  p <- generate_core_plot(d, "a", group_col = "cat1", plot_type = "boxplot", labels = labels, group_label = "Treatment")
  expect_equal(ggplot2::get_labs(p)$x, "Treatment")
  expect_equal(ggplot2::get_labs(p)$y, "Measurement A")
})

test_that("expanded interactive parallel plots keep their Cartesian coordinates", {
  d <- make_test_df(30)
  d$absent <- NA_real_
  shiny::testServer(desc_exploratory_server, args = list(
    data_reactive = shiny::reactive(d), vars_metadata_reactive = shiny::reactive(NULL)
  ), {
    session$setInputs(desc_vars_multi = c("a", "b", "c"), desc_plot_type = "parallel",
                      desc_expand_mode = "interactive", desc_expand_plot_btn = 1)
    widget <- jsonlite::fromJSON(output$desc_main_plot_expanded_plotly, simplifyVector = FALSE)
    types <- vapply(widget$x$data, function(x) x$type %||% "", "")
    expect_false(any(types == "scatterpolar"))
    session$setInputs(desc_plot_type = "radar", desc_vars_multi = c("a", "b", "c", "absent"))
    widget <- jsonlite::fromJSON(output$desc_main_plot_expanded_plotly, simplifyVector = FALSE)
    for (trace in widget$x$data) {
      expect_identical(trace$mode, "markers")
      # Plotly omits default-valued properties when serializing a widget;
      # scatterpolar's default fill is none.
      expect_identical(trace$fill %||% "none", "none")
    }
  })
})
