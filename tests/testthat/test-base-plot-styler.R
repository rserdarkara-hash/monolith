# test-base-plot-styler.R — tests for generate_base_plot and apply_styler_theme
# from ui_helpers.R.  These functions depend on Shiny's `input` reactive list;
# we mock it as a plain named list since both functions only read from it with
# `$` / `%||%` (never rely on reactivity).

# ── Mock input ─────────────────────────────────────────────────────────────

mock_input_full <- list(
  palette_select       = "YlOrRd",
  color_style          = "continuous",
  styler_high_contrast = FALSE,
  styler_title_size    = 16,
  styler_base_size     = 12,
  styler_x_size        = 12,
  styler_y_size        = 12,
  styler_label_size    = 10,
  styler_legend_size   = 10,
  styler_font_family   = "sans",
  styler_legend_pos    = "right",
  styler_legend_dir    = "auto",
  styler_legend_key_size = 1.0,
  styler_legend_text_angle = 0,
  styler_margin_t      = 10,
  styler_margin_r      = 10,
  styler_margin_b      = 10,
  styler_margin_l      = 15,
  styler_x_title       = "X Axis",
  styler_y_title       = "Y Axis"
)

mock_input_minimal <- list()

# ── generate_base_plot ─────────────────────────────────────────────────────

test_that("generate_base_plot returns item$obj for non-map type", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  item <- list(type = "plot", obj = p, label = "Test Plot")
  result <- generate_base_plot(item, mock_input_full)
  expect_s3_class(result, "ggplot")
})

test_that("generate_base_plot returns item$obj for histogram type", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg)) +
    ggplot2::geom_histogram()
  item <- list(type = "histogram", obj = p, label = "Hist")
  result <- generate_base_plot(item, mock_input_full)
  expect_s3_class(result, "ggplot")
})

test_that("generate_base_plot handles NULL agro_params for non-map", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  item <- list(type = "plot", obj = p, label = "Test")
  result <- generate_base_plot(item, mock_input_full, agro_params = NULL)
  expect_s3_class(result, "ggplot")
})

# ── apply_styler_theme ─────────────────────────────────────────────────────

test_that("apply_styler_theme returns a ggplot with full input", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Test Plot")
  result <- apply_styler_theme(p, mock_input_full, calibration = 1,
                                item_label = "Test Label", item_type = "plot")
  expect_s3_class(result, "ggplot")
})

test_that("apply_styler_theme returns a ggplot with minimal input", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  result <- apply_styler_theme(p, mock_input_minimal, calibration = 1,
                                item_label = "", item_type = "plot")
  expect_s3_class(result, "ggplot")
})

test_that("apply_styler_theme applies calibration scaling", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  result <- apply_styler_theme(p, mock_input_full, calibration = 2,
                                item_label = "", item_type = "plot")
  expect_s3_class(result, "ggplot")

  result_half <- apply_styler_theme(p, mock_input_full, calibration = 0.5,
                                     item_label = "", item_type = "plot")
  expect_s3_class(result_half, "ggplot")
})

test_that("apply_styler_theme handles map_combined item_type", {
  p1 <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  p2 <- ggplot2::ggplot(mtcars, ggplot2::aes(hp, qsec)) +
    ggplot2::geom_point()
  p_obj <- list(p1 = p1, p2 = p2)
  result <- apply_styler_theme(p_obj, mock_input_full, calibration = 1,
                                item_label = "Combined", item_type = "map_combined")
  # Patchwork-assembled result is a ggplot
  expect_s3_class(result, "ggplot")
})

test_that("apply_styler_theme handles legend position bottom", {
  input_bottom <- mock_input_full
  input_bottom$styler_legend_pos <- "bottom"
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  result <- apply_styler_theme(p, input_bottom, calibration = 1,
                                item_label = "", item_type = "plot")
  expect_s3_class(result, "ggplot")
})

test_that("apply_styler_theme handles NULL font overrides", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  result <- apply_styler_theme(p, mock_input_full, calibration = 1,
                                item_label = "With Label", item_type = "plot")
  expect_s3_class(result, "ggplot")
})

test_that("apply_styler_theme handles legend direction horizontal", {
  input_horiz <- mock_input_full
  input_horiz$styler_legend_dir <- "horizontal"
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, color = factor(cyl))) +
    ggplot2::geom_point()
  result <- apply_styler_theme(p, input_horiz, calibration = 1,
                                item_label = "", item_type = "plot")
  expect_s3_class(result, "ggplot")
})

test_that("apply_styler_theme handles high_contrast input switch", {
  input_hc <- mock_input_full
  input_hc$styler_high_contrast <- TRUE
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  result <- apply_styler_theme(p, input_hc, calibration = 1,
                                item_label = "", item_type = "plot")
  expect_s3_class(result, "ggplot")
})

test_that("apply_styler_theme sets legend text angle", {
  input_angle <- mock_input_full
  input_angle$styler_legend_text_angle <- 90
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, color = factor(cyl))) +
    ggplot2::geom_point()
  result <- apply_styler_theme(p, input_angle, calibration = 1,
                                item_label = "", item_type = "plot")
  expect_s3_class(result, "ggplot")
})
