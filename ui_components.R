# ui_components.R - shiny tag / widget generators (cards, tooltips, docs
# drawer, modals, DT wrappers). register_expanded_modal registers observers on
# the session passed to it; nothing here creates top-level reactives.
# Sourced via ui_helpers.R.


tuning_ui <- function(id, label, 
                      global_slider_id, manual_slider_id, 
                      global_slider_args, manual_slider_args, 
                      optimize_btn_label = paste("OPTIMIZE", label),
                      manual_btn_label = paste("Apply Manual", label),
                      top_extra_ui = NULL,
                      extra_ui = NULL) {
  
  content <- tagList(
    radioButtons(paste0(id, "_mode"), "Fitting Mode", 
                 choices = c("Auto-Fit" = "auto", "Manual" = "manual"), inline = TRUE),
    
    top_extra_ui,
    
    conditionalPanel(
      condition = sprintf("input.%s_mode == 'auto'", id),
      actionButton(paste0("opt_", id), optimize_btn_label, class = "btn-default btn-block"),
      uiOutput(paste0(id, "_opt_panel")),
      do.call(sliderInput, c(list(inputId = global_slider_id), global_slider_args))
    ),
    
    conditionalPanel(
      condition = sprintf("input.%s_mode == 'manual'", id),
      div(class = "mn-subsection",
          h5("Manual Tuning"),
          selectInput(paste0(id, "_m_loc"), "Locality to Tune", choices = NULL),
          conditionalPanel(
              condition = "input.comp_mode == true",
              radioButtons(paste0(id, "_m_target"), "Target", 
                           choices = c("Actual" = "act", "Predicted" = "pre"), inline = TRUE)
          ),
          do.call(sliderInput, c(list(inputId = manual_slider_id), manual_slider_args)),
          actionButton(paste0("apply_", id, "_manual"), manual_btn_label, class = "btn-default btn-block")
      )
    ),
    
    extra_ui
  )
  
  div(class = "mn-subsection", content)
}

#' A button that reveals a panel of related controls.
#'
#' Built on <details> deliberately: the panel's contents stay in the DOM while
#' it is closed, so every Shiny input and output inside binds and updates
#' exactly as it would in an always-visible toolbar. A widget that mounted its
#' content lazily would leave those outputs suspended until first opened.
mn_popover <- function(label, ..., icon_tag = NULL, align = c("left", "right"),
                       width = "300px") {
  align <- match.arg(align)
  tags$details(
    class = paste("mn-popover", if (align == "right") "mn-popover-right"),
    tags$summary(class = "btn btn-default btn-sm", icon_tag, label),
    div(class = "mn-popover-panel", style = paste0("width: ", width, ";"), ...)
  )
}

# North-arrow markup for leaflet addControl(). A constant string, so it belongs
# with the UI builders: both the Map Viewer overlay observer (where it is
# toggled by show_north) and the Data Setup mini-map (where it is permanent)
# draw the same arrow.
map_north_arrow_html <- function() {
  "<div style='text-align: center; color: white; font-family: Arial, sans-serif; pointer-events: none;'><div style='font-size: 16px; font-weight: bold; line-height: 1; margin-bottom: 4px; text-shadow: 1px 1px 2px black;'>N</div><svg width='30' height='30' viewBox='0 0 24 24' style='filter: drop-shadow(1px 1px 2px black);'><polygon points='12,2 7,22 12,17 17,22' fill='#e74c3c' stroke='white' stroke-width='1.5'/><polygon points='12,2 7,22 12,17' fill='#c0392b' stroke='white' stroke-width='1.5'/></svg></div>"
}

# Wraps a leaflet legend title so the stylesheet can hide it. addLegend() drops
# the title into the control as raw HTML, which is what makes the span reach the
# DOM; the Overlays "Variable Label in Legend" checkbox then stamps
# body.mn-show-legend-title to reveal it. Done in CSS rather than by rebuilding
# the legend so the toggle costs nothing on maps that carry a raster image per
# locality, and so every legend on screen answers it in the same frame.
legend_var_title <- function(txt) {
  htmltools::HTML(paste0(
    "<span class=\"mn-legend-title\">", htmltools::htmlEscape(txt), "</span>"
  ))
}

# Stylesheet for the leaflet measure control (the Map Viewer ruler), injected
# once in ui_main.R's head. Kept here as a function rather than inline so the
# exact shipped rules can be loaded into a test page and measured.
#
# Every selector carries an element qualifier (`a.leaflet-control-measure-...`)
# or an extra class so it outranks the plugin's own stylesheet on SPECIFICITY
# rather than on order: that stylesheet arrives as an htmlwidget dependency and
# is therefore appended to <head> AFTER this block, so an equal-specificity
# rule here would lose.
#
# Two things are corrected. (1) Geometry: the plugin ships a 36px button (44px
# once Leaflet flags the container `leaflet-touch`, which it does on any
# touch-capable machine), while every other button on the map (the drawing
# toolbar) is 26/30px inside leaflet's standard `leaflet-bar` chrome. The
# control is restyled to that same chrome so the map has one button size
# whatever corner a control sits in. (2) Footprint: the
# expanded panel is trimmed to what the map cannot already tell the user. The
# heading repeats the button's own tooltip, and the last-point latitude and
# longitude readout answers a question the ruler is not being asked; both are
# hidden, and Cancel / Finish keep their icons without the label text. The
# plugin's own images are used, so the icons stay the x and the tick it draws
# elsewhere. Their accessible names are set in add_map_ruler(), because hidden
# text is not an accessible name.
map_ruler_css <- function() {
  paste(
    ".leaflet-control-measure { border-radius: 4px; box-shadow: 0 1px 5px rgba(0,0,0,0.65); }",
    ".leaflet-touch .leaflet-control-measure { border: 2px solid rgba(0,0,0,0.2); background-clip: padding-box; }",
    ".leaflet-control-measure a.leaflet-control-measure-toggle,",
    ".leaflet-control-measure a.leaflet-control-measure-toggle:hover { width: 26px; height: 26px; border-radius: 2px; }",
    ".leaflet-touch .leaflet-control-measure a.leaflet-control-measure-toggle,",
    ".leaflet-touch .leaflet-control-measure a.leaflet-control-measure-toggle:hover { width: 30px; height: 30px; }",
    ".leaflet-control-measure h3 { display: none; }",
    # The coordinate readout is the first .group the results template emits
    # (it opens with <p class='lastpoint heading'>); the distance and area
    # groups follow it, so they are untouched.
    ".leaflet-control-measure .js-results .group:first-child { display: none; }",
    ".leaflet-control-measure .js-measuretasks { margin-top: 8px; padding-top: 8px; }",
    ".leaflet-control-measure .js-measuretasks a.cancel,",
    ".leaflet-control-measure .js-measuretasks a.finish { display: inline-block; width: 0; height: 14px; padding-left: 18px; overflow: hidden; text-indent: 100%; white-space: nowrap; vertical-align: middle; }",
    sep = "\n"
  )
}

# Contents of a measurement's own popup, built from measure_path_metrics()
# (global_utils.R), which recomputes the drawn shape in R. It REPLACES the text
# the measure plugin writes: the plugin computes on a sphere (radius 6371000 m)
# and knows nothing about the Target Mapping CRS, so leaving its text beside
# this one would put two answers to the same question on the screen. Attaching
# the authoritative numbers to the shape itself is also what keeps them true
# when an older measurement is clicked again - a single box in the corner can
# only ever describe the most recent one.
#
# The markup deliberately reuses the plugin's own classes (h3, p, ul.tasks,
# a.zoomto, a.deletemarkup): its stylesheet is already loaded, so the popup
# keeps the familiar heading rule, spacing and task icons for free. The
# mono-ruler-* classes are the handles add_map_ruler() wires the two links to.
# NULL for a shape with fewer than two vertices - there is nothing to report,
# and the plugin's single-point coordinate popup stands.
map_ruler_popup_html <- function(res) {
  if (is.null(res) || !is.list(res) || (res$n_points %||% 0) < 2) return(NULL)

  closed <- isTRUE(res$closed)
  crossed <- closed && isTRUE(res$self_intersecting)
  row <- function(label, value) {
    sprintf("<p><span style='color: var(--mn-text-3);'>%s:</span> <b>%s</b></p>", label, value)
  }
  proj_lab <- paste0("projected (", res$crs_label, ")")
  # Three vertices or more is a ring, so the figure is a perimeter and says so.
  len_lab <- if (closed) "Perimeter" else "Length"
  lines <- row(paste0(len_lab, ", ground (WGS84)"),
               format_measure_length(res$length_geodesic))
  if (!is.null(res$length_projected) && is.finite(res$length_projected)) {
    lines <- paste0(lines, row(paste0(len_lab, ", ", proj_lab),
                               format_measure_length(res$length_projected)))
  }
  # Area is shown on both bases for the same reason lengths are: terra's
  # ellipsoidal area (the app's own convention, shared with the class-zone
  # export) and the planimetric area in the analysis CRS differ by the
  # projection's area distortion, and reporting only one invites the reader to
  # assume a GIS would re-measure the same figure.
  if (!is.null(res$area_geodesic) && is.finite(res$area_geodesic)) {
    lines <- paste0(lines, row("Area, ground (WGS84)", format_measure_area(res$area_geodesic)))
    if (!is.null(res$area_projected) && is.finite(res$area_projected)) {
      lines <- paste0(lines, row(paste0("Area, ", proj_lab),
                                 format_measure_area(res$area_projected)))
    }
  }
  # measure_path_metrics() withholds the area of a ring that crosses itself:
  # both engines integrate around the ring in traversal order, so a figure
  # eight's oppositely-traversed lobes return their DIFFERENCE rather than the
  # area drawn. Say so, or the missing rows read as a failure of the tool.
  if (crossed) {
    lines <- paste0(lines,
      "<p style='color:var(--mn-warn);'>Area not reported: the path crosses itself. ",
      "The perimeter above is exact.</p>")
  }

  paste0(
    "<div class='monolith-ruler-popup'>",
    "<h3>", if (crossed) "Closed path" else if (closed) "Area measurement" else "Linear measurement",
    " <span style='color: var(--mn-text-3);font-size:0.85em;'>(", res$n_points, " points)</span></h3>",
    lines,
    "<ul class='tasks'>",
    "<li><a href='#' class='mono-ruler-zoom zoomto'>Center on this ",
    if (crossed) "shape" else if (closed) "area" else "line", "</a></li>",
    "<li><a href='#' class='mono-ruler-delete deletemarkup'>Delete</a></li>",
    "</ul></div>"
  )
}

# Header row (title + PNG download + expand-to-modal buttons) above a plot
# output. Server side pairs with register_sci_plot() in monolith.R, which
# wires <id>_expand (modal) and <id>_dl (300-dpi PNG) to the same builder
# closure that feeds the in-page cached plot.
#
# Callers inside a Shiny module pass ids that are ALREADY namespaced, so
# `expand_id` can be given explicitly instead of being derived from `id`
# (the Classification Suite keeps its historical button ids that way).
# `download = FALSE` drops the PNG button for panels with no export handler,
# `info` takes an icon/tooltip tag shown next to the title, `click_id` wires
# the plot's click input, and `head_min_height` reserves a constant header
# height so a title that wraps cannot push its plot out of line with the
# card beside it.
sci_plot_card <- function(id, title, height = "350px",
                          expand_id = paste0(id, "_expand"),
                          download = TRUE, info = NULL, click_id = NULL,
                          head_min_height = NULL) {
  div(class = "sci-plot-card",
      div(class = "sci-plot-card-head",
          style = if (!is.null(head_min_height)) paste0("min-height: ", head_min_height, ";"),
          h4(title, info, style = "margin: 0; font-size: 17px;"),
          div(class = "sci-plot-card-tools",
              # Icon-only controls: the title tooltip is a sighted-user
              # affordance, aria-label is what assistive tech reads.
              if (isTRUE(download)) {
                downloadButton(paste0(id, "_dl"), label = "", icon = icon("download"),
                               class = "btn-xs btn-light", title = "Download PNG (300 dpi)",
                               "aria-label" = paste0("Download ", title, " as PNG"))
              },
              if (!is.null(expand_id)) {
                actionButton(expand_id, label = NULL, icon = icon("expand"),
                             class = "btn-xs btn-light", title = "Expand (static / interactive)",
                             "aria-label" = paste0("Expand ", title))
              }
          )
      ),
      plotOutput(id, height = height, click = click_id)
  )
}

# Unified results-card container: one plain surface with a hairline border.
# Cards used to carry a coloured left bar to tell them apart; the title does
# that, and a bar of colour per card competed with the class-break and
# residual palettes the cards contain.
sci_card <- function(title, subtitle, ...) {
  div(class = "sci-card",
      div(class = "sci-card-head",
          h4(title, style = "margin: 0 0 2px 0;"),
          if (!is.null(subtitle)) p(class = "sci-card-sub", subtitle)
      ),
      ...
  )
}

# Plain-language definitions shown as hover tooltips on metric table headers
# (matched by column name; columns without an entry render normally).
sci_metric_tooltips <- function() {
  c(
    "Source" = "Model and cross-validation design that produced this row's metrics.",
    "RMSE" = "Root Mean Square Error of the cross-validation residuals, in the variable's units. Lower is better.",
    "NRMSE (%)" = "RMSE expressed as a percentage of the observed mean. Scale-free, so it compares across variables; undefined (NA) when the observed mean is zero.",
    "MAE" = "Mean Absolute Error of the cross-validation residuals, in the variable's units. Less sensitive to single large errors than RMSE.",
    "R² (Corr)" = "Squared Pearson correlation between observed and CV-predicted values. Measures association only; insensitive to systematic bias.",
    "R² (NSE/Trad)" = "Nash-Sutcliffe efficiency (traditional R²): 1 - SSE/SStot against the observed mean. 1 = perfect, 0 = no better than predicting the mean, negative = worse than the mean.",
    "Bias (ME)" = "Mean Error, mean(observed - predicted): positive = model underpredicts on average, negative = overpredicts.",
    "Lin's CCC (Agree)" = "Lin's Concordance Correlation Coefficient: agreement with the 1:1 line, combining precision (correlation) and accuracy (bias/scale shift). 1 = perfect agreement. NA when either vector is constant.",
    "RPD (Prec)" = "Ratio of Performance to Deviation: SD(observed) / RMSE. Chemometrics convention: > 2 good, 1.4-2 fair, < 1.4 poor.",
    "RPIQ" = "Ratio of Performance to Interquartile distance: IQR(observed) / RMSE. The RPD analogue for skewed distributions, where the SD is a poor spread measure. Higher is better.",
    "SMAPE (%)" = "Symmetric Mean Absolute Percentage Error: scale-free accuracy; 0% is perfect.",
    "Moran's I" = "Spatial autocorrelation of the CV residuals (symmetric 8-nearest-neighbour weights). Read it against its null expectation E[I] = -1/(n-1) (shown per row on hover), not against 0: values near E[I] mean spatially unstructured errors, clearly higher values signal unmodelled spatial pattern. NA* = the statistic could not be computed for this point set.",
    "Moran p" = "Two-sided significance of Moran's I under the normality assumption (spdep::moran.test). Small p = the residual autocorrelation is unlikely under the no-structure null. NA* where no sampling distribution is available (the all-pairs fallback weighting) or where Moran's I itself could not be computed."
  )
}
build_rk_trend_ui <- function(lm_sum, dt_id, raw_id) {
  stats <- rk_fit_stats(lm_sum)
  if (is.null(stats)) return(NULL)
  chip <- function(lab, val) {
    div(style = "background-color: var(--mn-surface-2); border: 1px solid var(--mn-line); border-radius: 6px; padding: 6px 12px; text-align: center; color: var(--mn-text);",
        div(lab, style = "font-size: 0.7em; text-transform: uppercase; letter-spacing: 0.4px; opacity: 0.7;"),
        div(val, style = "font-weight: 600; font-size: 0.95em;"))
  }
  f_lab <- if (is.na(stats$f_value)) "NA" else {
    sprintf("%.2f (%d, %d)", stats$f_value, round(stats$f_df1), round(stats$f_df2))
  }
  tagList(
    div(style = "display: flex; flex-wrap: wrap; gap: 8px; margin-bottom: 12px;",
        chip("R²", sprintf("%.3f", stats$r2)),
        chip("Adj. R²", sprintf("%.3f", stats$adj_r2)),
        chip("Residual SE", sprintf("%.4g (df = %d)", stats$sigma, stats$df_res)),
        chip("F statistic", f_lab),
        chip("Model p", format_p_value(stats$f_p)),
        chip("n", as.character(stats$n))
    ),
    div(class = "table-container", DT::dataTableOutput(dt_id)),
    tags$p(style = "font-size: 0.72em; opacity: 0.65; margin-top: 6px;",
           "Signif. codes: *** p ≤ 0.001, ** p ≤ 0.01, * p ≤ 0.05, . p ≤ 0.1. CI = 95% confidence interval (t-based)."),
    # The trend is fitted by OLS and its residuals are then kriged BECAUSE they
    # are spatially autocorrelated - precisely the condition under which OLS
    # standard errors are biased low. Reporting the coefficients without this
    # note invites over-declaring covariate significance. The estimates
    # themselves are unbiased; only their uncertainty is understated. The fix
    # is the caveat, not a different estimator: a GLS refit under the fitted
    # residual variogram would be Universal Kriging, a different method.
    tags$p(style = "font-size: 0.72em; opacity: 0.65; margin-top: 2px;",
           tags$b("Read the p-values with care: "),
           "these standard errors, t statistics, confidence intervals and the F test ",
           "assume independent residuals. Regression Kriging kriges these residuals ",
           "precisely because they are spatially autocorrelated, which lowers the ",
           "effective sample size and biases the standard errors downward, so ",
           "significance is overstated. Check the residual Moran's I in the Model ",
           "Performance table: the further it sits above its expectation ",
           "E[I] = -1/(n-1), the more optimistic this table is. The coefficient ",
           "estimates themselves remain unbiased."),
    tags$details(style = "margin-top: 4px;",
      tags$summary("Raw R model summary", style = "cursor: pointer; font-size: 0.8em; opacity: 0.7;"),
      verbatimTextOutput(raw_id)
    )
  )
}

register_expanded_modal <- function(input, output, session, btn_id, mode_id, ui_id, plot_static_id, plot_plotly_id, title_text, build_fn, radar_special = FALSE, pca_3d_special = FALSE) {
  ns <- session$ns
  
  is_pca_3d <- function() {
    if (is.function(pca_3d_special)) {
      pca_3d_special()
    } else if (shiny::is.reactive(pca_3d_special)) {
      pca_3d_special()
    } else {
      isTRUE(pca_3d_special)
    }
  }
  
  shiny::observeEvent(input[[btn_id]], {
    mode_selector <- if (is_pca_3d()) {
      NULL
    } else {
      shiny::radioButtons(ns(mode_id), "View Mode:", choices = c("Static (High-Res)" = "static", "Interactive (Hover/Zoom)" = "interactive"), inline = TRUE)
    }
    
    shiny::showModal(shiny::modalDialog(
      title = paste0("Expanded View: ", title_text), size = "l", easyClose = TRUE,
      mode_selector,
      shiny::uiOutput(ns(ui_id)),
      footer = shiny::modalButton("Close")
    ))
  })
  
  output[[ui_id]] <- shiny::renderUI({
    if (is_pca_3d()) {
      plotly::plotlyOutput(ns(paste0(plot_plotly_id, "_3d")), height = "700px")
    } else {
      if (!is.null(input[[mode_id]]) && input[[mode_id]] == "interactive") {
        plotly::plotlyOutput(ns(plot_plotly_id), height = "700px")
      } else {
        shiny::plotOutput(ns(plot_static_id), height = "700px")
      }
    }
  })
  
  output[[plot_static_id]] <- shiny::renderPlot({
    p <- build_fn()
    shiny::req(p)
    p
  })
  
  output[[plot_plotly_id]] <- plotly::renderPlotly({
    p <- build_fn()
    shiny::req(p)
    if (radar_special && inherits(p, "ggplot") && nrow(p$data) > 0 && "variable" %in% colnames(p$data)) {
      d <- p$data
      fig <- plotly::plot_ly(type = 'scatterpolar', mode = 'lines+markers')
      for(g in unique(d$group)) {
          dg <- d[d$group == g, ]
          dg <- rbind(dg, dg[1, ])
          fig <- plotly::add_trace(fig, r = dg$value, theta = dg$variable, name = g, fill = 'toself')
      }
      fig <- plotly::layout(fig, 
                            polar = list(radialaxis = list(visible = TRUE, range = c(0, max(d$value, na.rm=TRUE)))), 
                            showlegend = TRUE, 
                            title = list(text = "Radar Chart (Normalized Means)<br><sup>Note: Native plotly style used for interactive mode</sup>", x = 0.5))
      return(fig)
    }
    if (inherits(p, "ggplot")) ggplotly_smart(p) else p
  })
  
  output[[paste0(plot_plotly_id, "_3d")]] <- plotly::renderPlotly({
    p <- build_fn()
    shiny::req(p)
    if (inherits(p, "plotly")) return(p)
  })
}

render_docs_drawer <- function() {
  div(
    id = "docs_drawer",
    class = "docs-drawer",
    # The drawer itself does not scroll: its scrollbar landed flush against the
    # page's own and the two read as one doubled bar. The content scrolls in an
    # inner element instead, which keeps its bar clear of the window edge.
    div(
      id = "docs_drawer_body",
      class = "docs-drawer-body",
      div(style = "display: flex; justify-content: space-between; align-items: center; border-bottom: 1px solid #eee; padding-bottom: 10px; margin-bottom: 15px;",
          h3("Documentation", style = "margin: 0;"),
          actionButton("close_docs_btn", icon("times"), class = "btn-light btn-sm",
                       "aria-label" = "Close documentation",
                       style = "border: none; background: transparent; font-size: 20px;")
      ),
      tabsetPanel(
        id = "docs_tabs",
        tabPanel("User Guide",
                 uiOutput("render_user_guide")
        ),
        tabPanel("Scientific Guide",
                 uiOutput("render_scientific_guide")
        ),
        tabPanel("Descriptive and Exploratory Suite",
                 uiOutput("render_desc_exploratory_guide")
        )
      )
    ),
    # Floating navigation over the open drawer: jump to top/end or step
    # between sections (headings of the active guide tab). Plain buttons on
    # purpose - all behaviour is client-side, no server round-trip.
    div(class = "docs-nav-fab",
        tags$button(type = "button", id = "docs_nav_top", class = "btn",
                    title = "Back to top", "aria-label" = "Back to top", icon("angle-double-up")),
        tags$button(type = "button", id = "docs_nav_prev", class = "btn",
                    title = "Previous section", "aria-label" = "Previous section", icon("angle-up")),
        tags$button(type = "button", id = "docs_nav_next", class = "btn",
                    title = "Next section", "aria-label" = "Next section", icon("angle-down")),
        tags$button(type = "button", id = "docs_nav_bottom", class = "btn",
                    title = "Jump to end", "aria-label" = "Jump to end", icon("angle-double-down"))
    ),
    tags$script(HTML("
      (function() {
        var drawer = document.getElementById('docs_drawer');
        if (!drawer) return;
        // Scrolling lives on the inner body; the drawer is only the frame.
        var scroller = document.getElementById('docs_drawer_body') || drawer;
        // Click outside the open drawer closes it. The opener button is
        // excluded (it manages its own state), as are Bootstrap layers that
        // legitimately sit on top of the drawer (modals, popovers).
        document.addEventListener('click', function(e) {
          if (!drawer.classList.contains('open')) return;
          var t = e.target;
          if (!t || !t.closest) return;
          if (drawer.contains(t)) return;
          if (t.closest('#info_btn')) return;
          if (t.closest('.modal, .modal-backdrop, .popover')) return;
          drawer.classList.remove('open');
        });
        function headings() {
          var pane = scroller.querySelector('.tab-pane.active');
          return pane ? Array.prototype.slice.call(pane.querySelectorAll('h1, h2, h3')) : [];
        }
        function offsetIn(el) {
          return el.getBoundingClientRect().top - scroller.getBoundingClientRect().top + scroller.scrollTop;
        }
        function go(y) { scroller.scrollTo({ top: y, behavior: 'smooth' }); }
        function bind(id, fn) {
          var el = document.getElementById(id);
          if (el) el.addEventListener('click', fn);
        }
        bind('docs_nav_top', function() { go(0); });
        bind('docs_nav_bottom', function() { go(scroller.scrollHeight); });
        bind('docs_nav_next', function() {
          var hs = headings(), cur = scroller.scrollTop;
          for (var i = 0; i < hs.length; i++) {
            var y = offsetIn(hs[i]) - 12;
            if (y > cur + 5) { go(y); return; }
          }
          go(scroller.scrollHeight);
        });
        bind('docs_nav_prev', function() {
          var hs = headings(), cur = scroller.scrollTop, target = 0;
          for (var i = 0; i < hs.length; i++) {
            var y = offsetIn(hs[i]) - 12;
            if (y < cur - 5) { target = y; } else { break; }
          }
          go(target);
        });
      })();
    "))
  )
}

info_tooltip <- function(id, text) {
  content_html <- paste0(text, "<br><br><div style='text-align: right;'><button type='button' class='btn btn-xs btn-outline-secondary' onclick='$(this).closest(\".popover\").popover(\"hide\");'>Close &times;</button></div>")
  
  tags$span(
    id = paste0(id, "_info_icon"),
    class = "info-icon",
    style = "cursor: pointer; color: var(--mn-text-3); margin-left: 5px;",
    tabindex = "0",
    `data-toggle` = "popover",
    `data-placement` = "auto",
    `data-trigger` = "focus",
    # Attached to <body>, not to the icon's own parent. Left where Bootstrap
    # puts it by default, the panel inherits its parent's clipping: every
    # tooltip in the sidebar (a scroll container) and in the map/plot cards was
    # cut off at the container edge.
    `data-container` = "body",
    `data-content` = content_html,
    `data-html` = "true",
    `data-bs-toggle` = "popover",
    `data-bs-placement` = "auto",
    `data-bs-trigger` = "focus",
    `data-bs-container` = "body",
    `data-bs-content` = content_html,
    `data-bs-html` = "true",
    onclick = "event.stopPropagation(); event.preventDefault(); if (typeof bootstrap !== 'undefined' && bootstrap.Popover) { new bootstrap.Popover(this, {container: 'body'}).show(); }",
    icon("info-circle")
  )
}


# One-line statement of the sample a matrix-valued panel was estimated on.
# Correlation matrices, partial correlations, PCA and the collinearity screen
# all use the rows complete across EVERY selected variable, so their n is not
# the row count of the active selection and has to be said out loud.
complete_case_note <- function(n_used, n_total) {
  dropped <- max(0, n_total - n_used)
  sprintf("Complete cases: n = %d of %d rows%s.", n_used, n_total,
          if (dropped > 0) sprintf(" (%d dropped for missing values)", dropped) else "")
}

# Shared DT wrapper for the compact summary tables on the Scientific Analysis
# tab, matching the Classification Suite look (dom = 't', scrollX). Paging is
# disabled because dom = 't' hides the paging controls: with the default
# pageLength, rows beyond the first page would be silently unreachable in
# variable-length tables (e.g. per-locality variogram parameters).
sci_dt <- function(df, escape = TRUE, header_tooltips = NULL) {
  # Never return NULL: DT's htmlwidgets binding reads `data.lazyRender` BEFORE
  # its own `data === null` branch, so a NULL payload arriving at a table that
  # is currently hidden (this tab renders eagerly, suspendWhenHidden = FALSE)
  # throws a TypeError inside Shiny's async message dispatch and the remaining
  # outputs in that batch are never applied. An explicit empty state is also a
  # better answer for the reader than a blank slot.
  if (is.null(df)) {
    df <- data.frame(Status = "No data for this selection.")
    header_tooltips <- NULL
    escape <- TRUE
  }
  opts <- list(dom = 't', paging = FALSE, scrollX = TRUE)
  if (!is.null(header_tooltips)) {
    ths <- lapply(names(df), function(nm) {
      if (nm %in% names(header_tooltips)) {
        htmltools::tags$th(nm, title = unname(header_tooltips[[nm]]),
                           style = "cursor: help; text-decoration: underline dotted 1px;")
      } else {
        htmltools::tags$th(nm)
      }
    })
    container <- htmltools::tags$table(class = "display",
                                       htmltools::tags$thead(do.call(htmltools::tags$tr, ths)))
    return(DT::datatable(df, options = opts, rownames = FALSE, escape = escape,
                         container = container))
  }
  DT::datatable(df, options = opts, rownames = FALSE, escape = escape)
}

# `step` is the 1-4 index of the phase strip entry currently running; earlier
# entries are marked finished. Pass 5 to mark the whole strip finished, or
# leave it NULL to move the bar without touching the strip.
update_premium_progress <- function(pct, message = NULL, step = NULL) {
  width_val <- if (is.numeric(pct)) {
    sprintf("%d%%", round(pct))
  } else if (grepl("%$", pct)) {
    pct
  } else {
    paste0(pct, "%")
  }
  
  shinyjs::runjs(sprintf("document.getElementById('map_progress_bar_inner').style.width = '%s';", width_val))
  
  if (!is.null(message)) {
    shinyjs::html("map_progress_text", message)
  }

  if (!is.null(step)) {
    shinyjs::runjs(sprintf(
      "(function(n){for(var i=1;i<=4;i++){var e=document.getElementById('map_step_'+i);if(!e)continue;e.className='mn-run-step'+(i<n?' done':(i===n?' on':''));}})(%d);",
      as.integer(step)))
  }
}

render_locality_pan_input <- function(loc_names) {
  choices <- c("Global View" = "global")
  if (length(loc_names) > 0) {
    choices <- c(choices, loc_names)
  }
  selectInput("locality_pan", NULL,
              choices = choices,
              selected = "global", width = "160px", selectize = FALSE)
}
