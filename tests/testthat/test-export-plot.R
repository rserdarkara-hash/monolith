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

# ── downloadHandler contract ───────────────────────────────────────────────

test_that("no downloadHandler content function returns without writing its file", {
  # A downloadHandler cannot decline a request: by the time `content` runs the
  # browser has already opened the download URL. Returning early without
  # writing `file` therefore leaves the user on a blank page at
  # /session/<id>/download/<id>?w= with the app behind them, and a
  # showNotification fired from there arrives on a page they have just left.
  # The supported way to refuse is stop(safeError(msg)), which shiny turns into
  # a readable text response. Guard the whole app: scan each downloadHandler(
  # call's parenthesis-balanced body for a bare `return(NULL)`.
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."),
                        winslash = "/")
  files <- list.files(root, pattern = "\\.R$", full.names = TRUE)

  hits <- unlist(lapply(files, function(f) {
    lines <- readLines(f, warn = FALSE)
    starts <- grep("downloadHandler(", lines, fixed = TRUE)
    unlist(lapply(starts, function(s) {
      depth <- 0
      for (i in seq(s, length(lines))) {
        depth <- depth +
          lengths(regmatches(lines[i], gregexpr("(", lines[i], fixed = TRUE))) -
          lengths(regmatches(lines[i], gregexpr(")", lines[i], fixed = TRUE)))
        if (grepl("^[[:space:]]*return\\(NULL\\)[[:space:]]*$", lines[i])) {
          return(sprintf("%s:%d", basename(f), i))
        }
        if (depth <= 0 && i > s) break
      }
      NULL
    }))
  }))
  expect_equal(as.character(hits), character(0))
})

# ── showtext / device resolution ───────────────────────────────────────────

test_that("with_showtext_dpi applies the setting and restores it", {
  skip_if_not_installed("showtext")
  before <- showtext::showtext_opts()$dpi
  inside <- with_showtext_dpi(456, showtext::showtext_opts()$dpi)
  expect_equal(inside, 456)
  expect_equal(showtext::showtext_opts()$dpi, before)
})

test_that("exported text is sized by the theme, not by the export DPI", {
  skip_if_not_installed("png")
  skip_if_not_installed("showtext")
  # setup.R disables showtext for the suite; the app runs with it on
  # (global.R), and that is the only state in which this defect exists.
  showtext::showtext_auto(TRUE)
  on.exit(showtext::showtext_auto(FALSE), add = TRUE)

  # theme_minimal (what every builder in the app uses) is the state showtext
  # actually intercepts; theme_void is drawn by the device itself and would
  # pass this test even unfixed.
  p <- ggplot2::ggplot() +
    ggplot2::labs(title = "Title Width Probe") +
    ggplot2::theme_minimal(base_size = 12)

  # The title is the topmost ink, and a line of text occupies a contiguous run
  # of rows, so the first such run is the title wherever the panel starts.
  title_width <- function(dpi) {
    f <- tempfile(fileext = ".png")
    on.exit(unlink(f), add = TRUE)
    export_plot_to_file(p, f, "png", list(), width = 6, height = 4, dpi = dpi)
    dark <- png::readPNG(f)[, , 1] < 0.5
    rows <- which(rowSums(dark) > 0)
    if (length(rows) == 0) return(0)
    last <- rows[1]
    while ((last + 1L) %in% rows) last <- last + 1L
    cols <- which(colSums(dark[rows[1]:last, , drop = FALSE]) > 0)
    max(cols) - min(cols)
  }

  w150 <- title_width(150)
  w300 <- title_width(300)
  expect_gt(w150, 0)
  # A 12 pt title is 12 pt on the page whatever the resolution, so doubling the
  # DPI must double its pixel width. showtext sizes text against its own dpi
  # option unless told the device's: unfixed, this ratio came out near 3.1 and
  # raising the DPI silently shrank the type.
  expect_equal(w300 / w150, 2, tolerance = 0.05)
})
