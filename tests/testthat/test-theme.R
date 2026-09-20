# test-theme.R — the single Monolith theme: tokens, stylesheet, variant toggle.

# ── helpers ───────────────────────────────────────────────────────────────

# WCAG 2.x relative luminance / contrast ratio, used to hold the palette to
# the 4.5:1 body-text floor in BOTH variants. These are the checks the old
# per-theme palettes kept failing by hand (a body colour dark enough to vanish
# on a dark content background, and so on); with one token set they are cheap
# to assert once and for all.
rel_luminance <- function(hex) {
  ch <- grDevices::col2rgb(hex)[, 1] / 255
  lin <- ifelse(ch <= 0.03928, ch / 12.92, ((ch + 0.055) / 1.055)^2.4)
  sum(c(0.2126, 0.7152, 0.0722) * lin)
}

contrast_ratio <- function(a, b) {
  la <- rel_luminance(a)
  lb <- rel_luminance(b)
  (max(la, lb) + 0.05) / (min(la, lb) + 0.05)
}

colour_tokens <- function(tokens) {
  tokens[!names(tokens) %in% c("shadow", "scheme")]
}

# ── tokens ────────────────────────────────────────────────────────────────

test_that("monolith_tokens defines both variants over the same role names", {
  tk <- monolith_tokens()

  expect_named(tk, c("light", "dark"))
  expect_identical(names(tk$light), names(tk$dark))
  expect_true(all(nzchar(tk$light)))
  expect_true(all(nzchar(tk$dark)))
})

test_that("every colour role is a literal hex in both variants", {
  tk <- monolith_tokens()

  for (variant in names(tk)) {
    cols <- colour_tokens(tk[[variant]])
    expect_true(
      all(grepl("^#[0-9A-Fa-f]{6}$", cols)),
      info = paste(variant, "has a non-hex colour:",
                   paste(names(cols)[!grepl("^#[0-9A-Fa-f]{6}$", cols)], collapse = ", "))
    )
  }
})

test_that("the two variants are actually different palettes", {
  tk <- monolith_tokens()
  cols_light <- colour_tokens(tk$light)
  cols_dark <- colour_tokens(tk$dark)

  # Nothing may be shared: a value that survived a variant switch would be one
  # that stops responding to the toggle.
  expect_equal(sum(cols_light == cols_dark), 0)
  expect_lt(rel_luminance(tk$dark[["surface"]]), rel_luminance(tk$light[["surface"]]))
})

# ── contrast ──────────────────────────────────────────────────────────────

test_that("text roles clear 4.5:1 against the surfaces they are used on", {
  tk <- monolith_tokens()

  for (variant in names(tk)) {
    v <- tk[[variant]]
    for (surface in c("surface", "surface-2")) {
      for (role in c("text", "text-2", "text-3")) {
        expect_gte(
          contrast_ratio(v[[role]], v[[surface]]), 4.5,
          label = paste0(variant, ": ", role, " on ", surface)
        )
      }
    }
  }
})

test_that("on-accent text clears 4.5:1 against the accent it sits on", {
  tk <- monolith_tokens()

  for (variant in names(tk)) {
    v <- tk[[variant]]
    expect_gte(contrast_ratio(v[["on-accent"]], v[["accent"]]), 4.5,
               label = paste(variant, "on-accent vs accent"))
    # The accent also carries link text and the active tab label directly on
    # the surface, so it has to survive as a foreground colour too.
    expect_gte(contrast_ratio(v[["accent"]], v[["surface"]]), 4.5,
               label = paste(variant, "accent vs surface"))
  }
})

test_that("state colours stay distinguishable from the accent", {
  tk <- monolith_tokens()

  # Petrol replaced an indigo accent that sat beside a near-identical "live"
  # blue; the live role was dropped for exactly that reason. What separates a
  # state colour from the accent is HUE, not luminance — green and petrol can
  # share a luminance and still be unmistakable — so contrast ratio is the
  # wrong instrument here and hue distance is the right one.
  for (variant in names(tk)) {
    v <- tk[[variant]]
    acc_h <- grDevices::rgb2hsv(grDevices::col2rgb(v[["accent"]]))["h", 1]
    for (role in c("ok", "warn", "danger")) {
      role_h <- grDevices::rgb2hsv(grDevices::col2rgb(v[[role]]))["h", 1]
      gap <- abs(acc_h - role_h)
      gap <- min(gap, 1 - gap) * 360           # circular distance in degrees
      expect_gte(gap, 30,
                 label = paste(variant, role, "hue distance from accent (deg)"))
    }
  }
})

# ── stylesheet ────────────────────────────────────────────────────────────

test_that("monolith_theme_css emits both variant blocks and the webfonts", {
  css <- monolith_theme_css()

  expect_type(css, "character")
  expect_length(css, 1)
  expect_match(css, "fonts.googleapis.com", fixed = TRUE)
  expect_match(css, "IBM Plex Sans", fixed = TRUE)
  expect_match(css, "IBM Plex Mono", fixed = TRUE)
  expect_match(css, ":root {", fixed = TRUE)
  expect_match(css, ":root[data-theme='dark']", fixed = TRUE)
})

test_that("every token is declared in the stylesheet, in both variants", {
  css <- monolith_theme_css()
  tk <- monolith_tokens()

  for (role in names(tk$light)) {
    expect_match(css, paste0("--mn-", role, ": ", tk$light[[role]], ";"), fixed = TRUE,
                 info = paste("light", role))
    expect_match(css, paste0("--mn-", role, ": ", tk$dark[[role]], ";"), fixed = TRUE,
                 info = paste("dark", role))
  }
})

test_that("the stylesheet references no token it does not define", {
  css <- monolith_theme_css()

  used <- unique(regmatches(css, gregexpr("var\\(--mn-[a-z0-9-]+\\)", css))[[1]])
  used <- sub("^var\\(", "", sub("\\)$", "", used))
  declared <- unique(regmatches(css, gregexpr("--mn-[a-z0-9-]+(?=:)", css, perl = TRUE))[[1]])

  expect_true(length(used) > 0)
  expect_equal(setdiff(used, declared), character(0))
})

test_that("filled button rules outrank Bootstrap's own btn-default", {
  css <- monolith_theme_css()

  # shiny::actionButton() emits `btn btn-default action-button` and appends the
  # caller's class AFTER it, so a "primary" button carries btn-default as well.
  # The .btn-default rule is declared later in the sheet, so an unqualified
  # .btn-primary loses on source order at equal specificity and every filled
  # button renders as an outline. Qualifying with .btn is what makes them win.
  for (variant in c("btn-primary", "btn-danger", "btn-light")) {
    expect_match(css, paste0(".btn.", variant), fixed = TRUE, info = variant)
    expect_false(grepl(paste0("\n\\.", variant, "[ ,:{]"), css),
                 info = paste("unqualified rule for", variant, "loses to .btn-default"))
  }
  expect_match(css, ".btn.mn-iconbtn", fixed = TRUE)
  expect_false(grepl("\n\\.mn-iconbtn[ ,:{]", css))
})

test_that("every heading level is sized", {
  css <- monolith_theme_css()

  # Bootstrap's h1/h2 defaults are far out of scale for this interface; a
  # heading left unsized renders at 36px next to 13px body text.
  for (h in c("h1", "h2", "h3", "h4", "h5", "h6")) {
    expect_match(css, paste0(h, " { font-size:"), fixed = TRUE, info = h)
  }
})

test_that("readonly and disabled fields resolve to theme surfaces", {
  css <- monolith_theme_css()

  # Bootstrap paints them #eee. In the dark variant that is a near-white field
  # carrying near-white text, which made the file input's filename box (it is
  # readonly) unreadable as soon as a file was chosen.
  expect_match(css, ".form-control[readonly]", fixed = TRUE)
  expect_match(css, ".form-control[disabled]", fixed = TRUE)
  block <- regmatches(
    css, regexpr("\\.form-control\\[readonly\\][^{]*\\{[^}]*\\}", css)
  )
  expect_length(block, 1)
  expect_match(block, "background-color: var(--mn-surface-2)", fixed = TRUE)
  expect_match(block, "color: var(--mn-text)", fixed = TRUE)
  expect_match(block, "opacity: 1", fixed = TRUE)
})

test_that("one chevron serves every closed control", {
  css <- monolith_theme_css()

  # Three libraries ship three different disclosure marks: an SVG for the
  # native select, a CSS border triangle for selectize (inset 15px against a
  # 9px text inset, and swapped for a second triangle while open), and
  # Bootstrap's caret for bootstrap-select, which flips to point UP whenever
  # the menu would open upwards. All three draw --mn-chevron instead.
  expect_match(css, "--mn-chevron:", fixed = TRUE)
  expect_equal(
    length(regmatches(css, gregexpr("--mn-chevron:", css, fixed = TRUE))[[1]]), 2
  )
  for (sel in c("select, select.form-control",
                ".selectize-control.single .selectize-input::after",
                ".bootstrap-select > .dropdown-toggle .caret")) {
    expect_match(css, sel, fixed = TRUE, info = sel)
  }
  expect_equal(
    length(regmatches(css, gregexpr("var(--mn-chevron)", css, fixed = TRUE))[[1]]), 3
  )
  # Every chevron sits the same distance from its right edge as the text does
  # from the left, and the open state rotates the one mark rather than
  # substituting another. Both library sheets arrive as Shiny dependencies and
  # position their own mark at equal specificity, so the geometry is forced.
  expect_equal(
    length(regmatches(css, gregexpr("right: 9px !important;", css, fixed = TRUE))[[1]]), 2
  )
  expect_match(css, "background-position: right 9px center", fixed = TRUE)
  expect_match(css, ".selectize-input.dropdown-active::after { transform: rotate(180deg); }",
               fixed = TRUE)
  expect_match(css, ".bootstrap-select.open > .dropdown-toggle .caret { transform: rotate(180deg); }",
               fixed = TRUE)
})

test_that("the file-input progress bar is tall enough for its own label", {
  css <- monolith_theme_css()

  # Shiny writes the upload state INTO the bar ("Upload complete", or the error
  # text on failure). The 6px .progress used everywhere else let that label
  # overflow and print itself across the controls underneath.
  block <- regmatches(
    css, regexpr("\\.progress\\.shiny-file-input-progress \\{[^}]*\\}", css)
  )
  expect_length(block, 1)
  expect_match(block, "height: 18px", fixed = TRUE)
  expect_match(block, "margin: 7px 0 0 0", fixed = TRUE)
  expect_match(css, ".progress.shiny-file-input-progress .progress-bar.bar-danger",
               fixed = TRUE)
})

test_that("controls centre their own contents", {
  css <- monolith_theme_css()

  # actionButton() emits an .action-label span even for an empty label, and
  # .btn puts a 7px gap between flex children, so an icon-only button laid its
  # glyph out left of centre. The theme toggle, which has no label span, was
  # the only one that looked right.
  expect_match(css, ".btn.mn-iconbtn { gap: 0 !important;", fixed = TRUE)
  expect_match(css, ".btn.mn-iconbtn > .action-label { display: none !important; }",
               fixed = TRUE)
  expect_match(css, ".btn.mn-iconbtn > .action-icon", fixed = TRUE)

  # bootstrap-select's label box stretches to the toggle's full height and
  # leaves its text on the first line of it, so the selected value sat above
  # the centre of a control whose chevron was centred.
  expect_match(css, ".bootstrap-select > .dropdown-toggle .filter-option", fixed = TRUE)
  block <- regmatches(
    css,
    regexpr("\\.bootstrap-select > \\.dropdown-toggle \\.filter-option \\{[^}]*\\}", css)
  )
  expect_length(block, 1)
  expect_match(block, "display: flex !important", fixed = TRUE)
  expect_match(block, "align-items: center !important", fixed = TRUE)
})

test_that("the sticky run dock reaches the bottom of the sidebar card", {
  css <- monolith_theme_css()

  # A sticky child resolves bottom: 0 against its containing block's CONTENT
  # box, so the well's bottom padding parked the dock 15px short of the card
  # edge and the controls it was scrolled past showed through the strip
  # underneath. The dock supplies that padding itself now.
  # Scoped to the sidebar: sidebarPanel() is the only .well that is its own
  # scroll container. Applied to every wellPanel it turned the analytics
  # grouping panel into a clipping box that cut off its own select menus.
  well <- regmatches(css, regexpr("\\.well\\[role='complementary'\\] \\{[^}]*\\}", css))
  expect_length(well, 1)
  expect_match(well, "padding: 14px 14px 0 14px !important", fixed = TRUE)
  expect_match(well, "overflow-y: auto", fixed = TRUE)
  expect_match(well, "position: sticky", fixed = TRUE)

  # The unscoped rule keeps the card's look and nothing else.
  plain <- regmatches(css, regexpr("\\.well \\{[^}]*\\}", css))
  expect_length(plain, 1)
  expect_false(grepl("overflow-y", plain, fixed = TRUE))
  expect_false(grepl("max-height", plain, fixed = TRUE))

  dock <- regmatches(css, regexpr("\\.sidebar-run-sticky \\{[^}]*\\}", css))
  expect_length(dock, 1)
  expect_match(dock, "margin: 10px -14px 0 -14px", fixed = TRUE)
  expect_match(dock, "padding: 12px 14px 14px 14px", fixed = TRUE)

  # Whatever ends the sidebar when the dock is not on screen still needs it.
  expect_match(css, ".mn-sidebar-tail { margin-bottom: 14px; }", fixed = TRUE)
  sidebar <- readLines(
    file.path(normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/"),
              "ui_sidebar.R"), warn = FALSE
  )
  expect_equal(length(grep("mn-sidebar-tail", sidebar, fixed = TRUE)), 2)
})

test_that("the Fixed resolution slider keeps its frame after CRS selection", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
  frames <- list()
  # Inspect calls, not text: comments and unrelated sliders must not count.
  collect_frames <- function(expr) {
    if (missing(expr)) return(invisible(NULL))
    if (!is.call(expr) && !is.expression(expr)) return(invisible(NULL))
    if (is.call(expr) && as.character(expr[[1]])[1] %in%
        c("sliderInput", "updateSliderInput")) {
      args <- as.list(expr)[-1]
      if ("grid_res" %in% args && !is.null(args$min)) {
        frames[[length(frames) + 1L]] <<- args[c("min", "max", "step")]
      }
    }
    for (child in as.list(expr)[if (is.call(expr)) -1 else TRUE]) collect_frames(child)
  }
  collect_frames(parse(file.path(root, "ui_sidebar.R")))
  collect_frames(parse(file.path(root, "server_data_setup.R")))
  expect_length(frames, 3L) # declaration, Fixed CRS update, Auto CRS update
  for (frame in frames) expect_equal(frame, list(min = 1, max = 500, step = 1))
})

test_that("button focus is shown to keyboard users only", {
  css <- monolith_theme_css()

  # Bootstrap rings every CLICKED button with its own outline, and the
  # .btn-default:focus rule additionally repainted the border, so the header's
  # icon buttons kept a box around them after opening their panel.
  expect_match(css, ".btn:focus, .btn:active:focus, .btn.active:focus { outline: none; }",
               fixed = TRUE)
  expect_match(css, ".btn:focus-visible { outline: 2px solid var(--mn-accent);",
               fixed = TRUE)
  expect_false(grepl(".btn-default:focus,", css, fixed = TRUE))
  expect_match(css, ".btn-default:focus-visible", fixed = TRUE)
})

test_that("no fresh/AdminLTE remnant survives in the stylesheet", {
  css <- monolith_theme_css()

  expect_false(grepl("adminlte", css, ignore.case = TRUE))
  expect_false(grepl("light_blue", css, fixed = TRUE))
})

test_that("no UI file paints a colour outside the token set", {
  # A literal colour in an inline style is a one-variant colour: it survives the
  # theme toggle and takes whatever token-coloured text sits on it down with it.
  # This is what left the Export tab's asset list as pale grey on white, the
  # RK trend chips as near-black on the dark surface, and the variogram
  # fallback warning unreadable once the dark variant was on.
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
  files <- setdiff(
    list.files(root, pattern = "\\.R$"),
    # The stylesheet is where the literals belong; ui_colors.R holds DATA
    # palettes (map ramps, group colours), which are not interface chrome.
    c("theme_helpers.R", "ui_colors.R")
  )
  pattern <- paste0(
    "(background|background-color|color|border-color)\\s*:\\s*",
    "(#[0-9A-Fa-f]{3,8}|white|black|red|silver|gray|grey|whitesmoke)\\b"
  )

  # NOT an exemption list entry, but deliberately outside this scan:
  # server_export.R's `fgFill = "#EFEFEF"` is the exported worksheet's header
  # fill. Excel has no access to the interface tokens, so a literal is correct
  # there. The pattern does not match `fgFill =`; if it is ever widened to
  # catch hex values in any argument, exempt that line rather than "fixing" it.
  # The north arrow is drawn ON basemap tiles by leaflet's addControl(), not on
  # an app surface, so it is white-over-shadow in both variants by design.
  exempt <- "text-shadow: 1px 1px 2px black"

  offenders <- unlist(lapply(files, function(f) {
    lines <- readLines(file.path(root, f), warn = FALSE)
    hits <- grep(pattern, lines, perl = TRUE)
    hits <- hits[!grepl(exempt, lines[hits], fixed = TRUE)]
    if (!length(hits)) return(NULL)
    paste0(f, ":", hits)
  }))

  expect_equal(offenders, NULL)
})

test_that("every variogram banner band is painted with tokens", {
  # The banner is drawn over the map in both variants; a literal colour here is
  # what left the fallback warning unreadable once the dark variant was on.
  # One fit per band, so no band can be added without a token.
  fb <- gstat::vgm(0.5, "Sph", 200, 0.1); attr(fb, "is_fallback") <- TRUE
  fw <- gstat::vgm(0.5, "Sph", 200, 0.1); attr(fw, "flawed_winner") <- TRUE
  ru <- gstat::vgm(2, "Exp", 300, 0.5)
  attr(ru, "vgm_diagnostics") <- list(status = "range_unresolved", practical_range = 900,
                                      max_lag = 700, sill_resolved = FALSE,
                                      range_side = "beyond", trend_suspected = TRUE,
                                      target_degenerate = FALSE)
  sm <- manual_vgm(1, "Gau", 300, 0.02)

  html <- build_vgm_warning_html(list(A_act = fb, B_act = fw, C_act = ru, D_act = sm))
  expect_match(html, "Variogram fit failed", fixed = TRUE)
  expect_match(html, "non-converged or singular", fixed = TRUE)
  expect_match(html, "Variogram range not resolved", fixed = TRUE)
  expect_match(html, "Sill not observed", fixed = TRUE)
  expect_match(html, "nugget below 5% of the sill", fixed = TRUE)

  colours <- regmatches(html, gregexpr("color[[:space:]]*:[[:space:]]*[^;']+", html))[[1]]
  expect_gt(length(colours), 0)
  expect_true(all(grepl("var(--mn-", colours, fixed = TRUE)),
              info = paste(colours[!grepl("var(--mn-", colours, fixed = TRUE)], collapse = " | "))
})

# ── variant switching ─────────────────────────────────────────────────────

test_that("the boot script stamps a variant before first paint", {
  html <- as.character(monolith_theme_boot_js())

  expect_match(html, "data-theme", fixed = TRUE)
  expect_match(html, "monolith_theme", fixed = TRUE)
  expect_match(html, "prefers-color-scheme: dark", fixed = TRUE)
})

test_that("theme_switcher_ui renders a namespaced client-side toggle", {
  html <- as.character(theme_switcher_ui("theme_mod"))

  expect_match(html, "theme_mod-toggle", fixed = TRUE)
  expect_match(html, "mn-theme-toggle", fixed = TRUE)
  expect_match(html, "aria-label", fixed = TRUE)
  # Flips the attribute and remembers the choice; never touches Shiny.
  expect_match(html, "setAttribute('data-theme', next)", fixed = TRUE)
  expect_match(html, "localStorage.setItem('monolith_theme', next)", fixed = TRUE)
  expect_false(grepl("Shiny.setInputValue", html, fixed = TRUE))
})

# ── export writer (unchanged behaviour, kept covered) ──────────────────────

test_that("export_plot_to_file writes at the requested size and format", {
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) + ggplot2::geom_point()
  path <- file.path(tempdir(), "monolith-theme-test.png")
  on.exit(unlink(path), add = TRUE)

  export_plot_to_file(p, path, "png", input = list(), width = 4, height = 3, dpi = 72)

  expect_true(file.exists(path))
  expect_gt(file.info(path)$size, 0)
})

# ── copy a result table to the clipboard ──────────────────────────────────
# The button is the table's counterpart to a figure card's PNG download. The
# script is shipped as a string from ui_components.R so its contract is
# testable without a browser.

test_that("copy_table_btn is a plain button, not a Shiny input", {
  html <- as.character(copy_table_btn("metrics_table", "Model Performance"))

  expect_match(html, "mnCopyTable(&#39;metrics_table&#39;, this);", fixed = TRUE)
  expect_match(html, "mn-copy-btn", fixed = TRUE)
  expect_match(html, 'aria-label="Copy the Model Performance table to the clipboard"',
               fixed = TRUE)
  # a copy needs no server round-trip, so it must not bind as an action button
  expect_false(grepl("action-button", html, fixed = TRUE))
  # a non-character title (an HTML tag) must not leak into the aria-label
  expect_match(as.character(copy_table_btn("t", shiny::tags$span("x"))),
               'aria-label="Copy the table to the clipboard"', fixed = TRUE)
})

test_that("sci_table pairs a table output with its copy button", {
  html <- as.character(sci_table("metrics_table", "Model Performance",
                                 shiny::uiOutput("cv_strategy_badge")))

  expect_match(html, 'id="metrics_table"', fixed = TRUE)
  expect_match(html, "sci-table-head", fixed = TRUE)
  expect_match(html, "table-container", fixed = TRUE)
  expect_match(html, "mnCopyTable(&#39;metrics_table&#39;, this);", fixed = TRUE)
  # extra content sits between the title strip and the table
  expect_match(html, "cv_strategy_badge", fixed = TRUE)
  expect_lt(regexpr("cv_strategy_badge", html, fixed = TRUE),
            regexpr("table-container", html, fixed = TRUE))

  # a renderUI that emits its own <table> supplies its own content
  lift <- as.character(sci_table("lift_ui", content = shiny::uiOutput("lift_ui")))
  expect_match(lift, "mnCopyTable(&#39;lift_ui&#39;, this);", fixed = TRUE)
  expect_false(grepl("table-container", lift, fixed = TRUE))
})

test_that("the copy script writes both clipboard flavours and handles scrollX", {
  js <- copy_table_js()

  # both flavours, or the paste lands as a single cell in Excel
  expect_match(js, "'text/html'", fixed = TRUE)
  expect_match(js, "'text/plain'", fixed = TRUE)
  # DataTables splits the header into a cloned table under scrollX
  expect_match(js, "dataTables_scrollHead thead", fixed = TRUE)
  expect_match(js, "dataTables_scrollBody tbody", fixed = TRUE)
  # async API first, execCommand fallback for an insecure origin
  expect_match(js, "navigator.clipboard.write", fixed = TRUE)
  expect_match(js, "document.execCommand('copy')", fixed = TRUE)
  # tab-separated rows, CRLF-terminated, is what a spreadsheet parses
  expect_match(js, "r.join('\\t')", fixed = TRUE)
  expect_match(js, "lines.join('\\r\\n')", fixed = TRUE)
  expect_match(js, "mn_copy_live", fixed = TRUE)
})

test_that("the copy script reads a paginated DataTable through its API", {
  js <- copy_table_js()
  # the tbody of a paged DataTable holds the current page only
  expect_match(js, "jq.fn.dataTable.isDataTable(bodyTable)", fixed = TRUE)
  expect_match(js, "search: 'applied'", fixed = TRUE)
  expect_match(js, "render('display')", fixed = TRUE)
  # server-side paging leaves rows unreachable: report it, never claim the whole
  expect_match(js, "recordsDisplay", fixed = TRUE)
  expect_match(js, "Copied only ", fixed = TRUE)
  # status messages are reported, not copied as a 1 x 1 table
  expect_match(js, "table.mn-status-table", fixed = TRUE)
  # focus returns to the button after the execCommand fallback
  expect_match(js, "btn.focus()", fixed = TRUE)
})

test_that("sci_dt marks a status message so the copy button reports it", {
  cls <- function(w) w$x$container
  expect_match(cls(sci_dt(NULL)), "mn-status-table", fixed = TRUE)
  expect_match(cls(sci_dt(data.frame(Status = "Awaiting classification"))),
               "mn-status-table", fixed = TRUE)
  expect_false(grepl("mn-status-table", cls(sci_dt(data.frame(a = 1, b = 2))), fixed = TRUE))
  expect_false(grepl("mn-status-table",
                     cls(sci_dt(data.frame(Status = c("x", "y")))), fixed = TRUE))
})

test_that("the clipboard payload carries no colour of its own", {
  # It is a document bound for Word or Excel, which supply their own palette;
  # a literal here would also be a one-variant colour in a shipped .R file.
  js <- copy_table_js()
  expect_false(grepl("(background|background-color|border-color)\\s*:", js, perl = TRUE))
  expect_false(grepl("#[0-9A-Fa-f]{3,8}", js, perl = TRUE))
  # borders and the bold header come from the receiving application instead
  expect_match(js, 'border="1"', fixed = TRUE)
  expect_match(js, "<th style=", fixed = TRUE)
})

# ── DataTables column alignment (a metric must never sit under another's
# heading) ─────────────────────────────────────────────────────────────────
# Under scrollX DataTables splits a table in two and keeps the halves aligned
# with the pixel widths it writes on both. A `width: 100% !important` beats
# those inline widths, and with nowrap cells each half then resolves 100%
# against its own min-content - the header against the labels, the body
# against the data - which is what moved values under the wrong column.

test_that("the stylesheet leaves a scrolling DataTable its computed widths", {
  css <- monolith_theme_css()
  # The rule that broke it: a forced width on any table inside the container.
  expect_false(grepl("\\.table-container table,\\s*\\n\\s*\\.table-container \\.dataTables_wrapper \\{[^}]*width:\\s*100%\\s*!important",
                     css, perl = TRUE))
  # What replaces it: the wrapper, and a table that is its DIRECT child (which
  # is the shape a NON-scrolling DataTable has). The two scroll containers
  # match neither, so their tables keep the widths DataTables computed.
  expect_match(css, ".table-container .dataTables_wrapper { width: 100% !important; }", fixed = TRUE)
  expect_match(css, ".table-container .dataTables_wrapper > table.dataTable { width: 100% !important; }",
               fixed = TRUE)
  # No RULE may set a width on either scroll container (comments stripped
  # first: the block above names them in prose, which is not a rule).
  rules <- gsub("(?s)/\\*.*?\\*/", "", css, perl = TRUE)
  expect_false(grepl("dataTables_scroll[^{}]*\\{[^}]*width", rules, perl = TRUE))
  # The colour and radius keep their !important - they fight DT's own sheet.
  expect_match(css, "background-color: var(--mn-surface) !important", fixed = TRUE)
})

test_that("the shipped head script realigns columns on every reveal and redraw", {
  head_html <- paste(as.character(htmltools::renderTags(ui)$head), collapse = "")

  # A tab reveal, an output Shiny makes visible, and the table's own redraw.
  expect_match(head_html, "shown.bs.tab", fixed = TRUE)
  expect_match(head_html, "shiny:visualchange", fixed = TRUE)
  expect_match(head_html, "draw.dt", fixed = TRUE)
  # shiny:visualchange fires on the OUTPUT element, so the handler is
  # delegated from the output container and resolves the table inside.
  expect_match(head_html, ".shiny-datatable-output", fixed = TRUE)
  # After the layout settles, not on a fixed timer, and once per frame.
  expect_match(head_html, "requestAnimationFrame", fixed = TRUE)
  expect_match(head_html, "columns.adjust()", fixed = TRUE)
  expect_match(head_html, "_mnAdjustPending", fixed = TRUE)
  # NEVER .draw() from a draw handler: that is a redraw loop.
  expect_false(grepl("\\.draw\\(\\)", head_html))
  expect_false(grepl("setTimeout(function () { if ($.fn.dataTable)", head_html, fixed = TRUE))
})

test_that("a non-scrolling table still copies one header row", {
  # The copy script special-cases scrollX's cloned header; without scrollX
  # there is no clone, so it must fall through to the single table.
  js <- copy_table_js()
  expect_match(js, "if (!thead || !tbody) {", fixed = TRUE)
  expect_match(js, "var t = root.querySelector('table');", fixed = TRUE)

  df <- data.frame(Param = c("Model", "Nugget"), Actual = c("Sph", "0.035"))
  expect_false(sci_dt(df, scroll_x = FALSE)$x$options$scrollX)
})

test_that("the copy script is mounted in the shipped UI", {
  # htmltools hoists tags$head content out of the body, so the script has to be
  # read from renderTags()$head rather than from as.character(ui).
  rendered <- htmltools::renderTags(ui)
  head_html <- paste(as.character(rendered$head), collapse = "")
  body_html <- as.character(rendered$html)

  expect_match(head_html, "window.mnCopyTable", fixed = TRUE)
  expect_match(head_html, "window.mnTablePayload", fixed = TRUE)
  # the live region and the copy buttons live in the body
  expect_match(body_html, 'id="mn_copy_live"', fixed = TRUE)
  expect_match(body_html, "mn-copy-btn", fixed = TRUE)
})
