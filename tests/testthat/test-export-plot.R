# test-export-plot.R — tests for export_plot_to_file from theme_helpers.R.

# ── Mock Shiny input object ────────────────────────────────────────────────
mock_input <- list(
  styler_width  = 8,
  styler_height = 6,
  styler_dpi    = 150
)

# ── ggplot export ──────────────────────────────────────────────────────────

test_that("export_plot_to_file creates a PNG file for ggplot", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  export_plot_to_file(p, tmp, "png", mock_input)
  expect_true(file.exists(tmp))
  expect_true(file.info(tmp)$size > 0)
})

test_that("export_plot_to_file creates a PDF file for ggplot", {
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  export_plot_to_file(p, tmp, "pdf", mock_input)
  expect_true(file.exists(tmp))
  expect_true(file.info(tmp)$size > 0)
})

test_that("export_plot_to_file creates a JPEG file for ggplot", {
  tmp <- tempfile(fileext = ".jpg")
  on.exit(unlink(tmp), add = TRUE)

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  export_plot_to_file(p, tmp, "jpg", mock_input)
  expect_true(file.exists(tmp))
  expect_true(file.info(tmp)$size > 0)
})

test_that("export_plot_to_file creates a TIFF file for ggplot", {
  tmp <- tempfile(fileext = ".tiff")
  on.exit(unlink(tmp), add = TRUE)

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  export_plot_to_file(p, tmp, "tiff", mock_input)
  expect_true(file.exists(tmp))
  expect_true(file.info(tmp)$size > 0)
})

# ── trellis export ─────────────────────────────────────────────────────────

test_that("export_plot_to_file creates a PNG file for trellis/lattice plot", {
  skip_if_not_installed("latticeExtra")
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  p <- lattice::xyplot(mpg ~ wt, data = mtcars)

  export_plot_to_file(p, tmp, "png", mock_input)
  expect_true(file.exists(tmp))
  expect_true(file.info(tmp)$size > 0)
})

test_that("export_plot_to_file creates a PDF for trellis plot", {
  skip_if_not_installed("latticeExtra")
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  p <- lattice::xyplot(mpg ~ wt, data = mtcars)
  export_plot_to_file(p, tmp, "pdf", mock_input)
  expect_true(file.exists(tmp))
  expect_true(file.info(tmp)$size > 0)
})

# ── Edge cases ─────────────────────────────────────────────────────────────

test_that("export_plot_to_file uses default dimensions when input fields missing", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  # input with no styler fields — should fall back to defaults via %||%
  empty_input <- list()
  export_plot_to_file(p, tmp, "png", empty_input)
  expect_true(file.exists(tmp))
  expect_true(file.info(tmp)$size > 0)
})
