# ui_components.R - shiny tag / widget generators (cards, tooltips, docs
# drawer, modals, DT wrappers). register_expanded_modal registers observers on
# the session passed to it; nothing here creates top-level reactives.
# Sourced via ui_helpers.R.


tuning_ui <- function(id, label, 
                      global_slider_id, manual_slider_id, 
                      global_slider_args, manual_slider_args, 
                      optimize_btn_label = paste("OPTIMIZE", label),
                      manual_btn_label = paste("Apply Manual", label),
                      outer_style = NULL,
                      manual_style = "background-color: #fff9db; padding: 10px; border: 1px solid #fab005; border-radius: 4px; margin-bottom: 10px;",
                      top_extra_ui = NULL,
                      extra_ui = NULL) {
  
  content <- tagList(
    radioButtons(paste0(id, "_mode"), "Fitting Mode", 
                 choices = c("Auto-Fit" = "auto", "Manual" = "manual"), inline = TRUE),
    
    top_extra_ui,
    
    conditionalPanel(
      condition = sprintf("input.%s_mode == 'auto'", id),
      actionButton(paste0("opt_", id), optimize_btn_label, class = "btn-info btn-block"),
      uiOutput(paste0(id, "_opt_panel")),
      do.call(sliderInput, c(list(inputId = global_slider_id), global_slider_args))
    ),
    
    conditionalPanel(
      condition = sprintf("input.%s_mode == 'manual'", id),
      div(style = manual_style,
          h5("Manual Tuning"),
          selectInput(paste0(id, "_m_loc"), "Locality to Tune", choices = NULL),
          conditionalPanel(
              condition = "input.comp_mode == true",
              radioButtons(paste0(id, "_m_target"), "Target", 
                           choices = c("Actual" = "act", "Predicted" = "pre"), inline = TRUE)
          ),
          do.call(sliderInput, c(list(inputId = manual_slider_id), manual_slider_args)),
          actionButton(paste0("apply_", id, "_manual"), manual_btn_label, class = "btn-warning btn-block")
      )
    ),
    
    extra_ui
  )
  
  if (!is.null(outer_style)) {
    div(style = outer_style, content)
  } else {
    content
  }
}

# Header row (title + PNG download + expand-to-modal buttons) above a plot
# output. Server side pairs with register_sci_plot() in monolith.R, which
# wires <id>_expand (modal) and <id>_dl (300-dpi PNG) to the same builder
# closure that feeds the in-page cached plot.
sci_plot_card <- function(id, title, height = "350px") {
  div(class = "sci-plot-card",
      div(class = "sci-plot-card-head",
          h4(title, style = "margin: 0; font-size: 17px;"),
          div(class = "sci-plot-card-tools",
              downloadButton(paste0(id, "_dl"), label = "", icon = icon("download"),
                             class = "btn-xs btn-light", title = "Download PNG (300 dpi)"),
              actionButton(paste0(id, "_expand"), label = NULL, icon = icon("expand"),
                           class = "btn-xs btn-light", title = "Expand (static / interactive)")
          )
      ),
      plotOutput(id, height = height)
  )
}

# Unified results-card container (generalizes the RK fit-chip styling): a
# neutral card with a coloured left accent instead of the former solid-colour
# info boxes. Accent keeps each card's identity; content stays readable in
# both themes because the body is neutral.
sci_card <- function(title, subtitle, ..., accent = "#fab005") {
  div(class = "sci-card", style = paste0("border-left: 4px solid ", accent, ";"),
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
    "R2 (Corr)" = "Squared Pearson correlation between observed and CV-predicted values. Measures association only; insensitive to systematic bias.",
    "R2 (NSE/Trad)" = "Nash-Sutcliffe efficiency (traditional R2): 1 - SSE/SStot against the observed mean. 1 = perfect, 0 = no better than predicting the mean, negative = worse than the mean.",
    "Bias (ME)" = "Mean Error, mean(observed - predicted): positive = model underpredicts on average, negative = overpredicts.",
    "RPD (Prec)" = "Ratio of Performance to Deviation: SD(observed) / RMSE. Chemometrics convention: > 2 good, 1.4-2 fair, < 1.4 poor.",
    "SMAPE (%)" = "Symmetric Mean Absolute Percentage Error: scale-free accuracy; 0% is perfect.",
    "Moran's I" = "Spatial autocorrelation of the CV residuals (symmetric 8-nearest-neighbour weights). Near 0 = errors are spatially unstructured; clearly positive values signal unmodelled spatial pattern."
  )
}
build_rk_trend_ui <- function(lm_sum, dt_id, raw_id) {
  stats <- rk_fit_stats(lm_sum)
  if (is.null(stats)) return(NULL)
  chip <- function(lab, val) {
    div(style = "background-color: #f1f3f5; border: 1px solid #dee2e6; border-radius: 6px; padding: 6px 12px; text-align: center; color: #343a40;",
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
    div(style = "display: flex; justify-content: space-between; align-items: center; border-bottom: 1px solid #eee; padding-bottom: 10px; margin-bottom: 15px;",
        h3("Documentation", style = "margin: 0;"),
        actionButton("close_docs_btn", icon("times"), class = "btn-light btn-sm", style = "border: none; background: transparent; font-size: 20px;")
    ),
    tabsetPanel(
      id = "docs_tabs",
      tabPanel("User Guide",
               uiOutput("render_user_guide")
      ),
      tabPanel("Scientific Guide",
               uiOutput("render_scientific_guide")
      ),
      tabPanel("Descriptive & Exploratory Suite",
               uiOutput("render_desc_exploratory_guide")
      )
    ),
    # Floating navigation over the open drawer: jump to top/end or step
    # between sections (headings of the active guide tab). Plain buttons on
    # purpose - all behaviour is client-side, no server round-trip.
    div(class = "docs-nav-fab",
        tags$button(type = "button", id = "docs_nav_top", class = "btn",
                    title = "Back to top", icon("angle-double-up")),
        tags$button(type = "button", id = "docs_nav_prev", class = "btn",
                    title = "Previous section", icon("angle-up")),
        tags$button(type = "button", id = "docs_nav_next", class = "btn",
                    title = "Next section", icon("angle-down")),
        tags$button(type = "button", id = "docs_nav_bottom", class = "btn",
                    title = "Jump to end", icon("angle-double-down"))
    ),
    tags$script(HTML("
      (function() {
        var drawer = document.getElementById('docs_drawer');
        if (!drawer) return;
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
          var pane = drawer.querySelector('.tab-pane.active');
          return pane ? Array.prototype.slice.call(pane.querySelectorAll('h1, h2, h3')) : [];
        }
        function offsetIn(el) {
          return el.getBoundingClientRect().top - drawer.getBoundingClientRect().top + drawer.scrollTop;
        }
        function go(y) { drawer.scrollTo({ top: y, behavior: 'smooth' }); }
        function bind(id, fn) {
          var el = document.getElementById(id);
          if (el) el.addEventListener('click', fn);
        }
        bind('docs_nav_top', function() { go(0); });
        bind('docs_nav_bottom', function() { go(drawer.scrollHeight); });
        bind('docs_nav_next', function() {
          var hs = headings(), cur = drawer.scrollTop;
          for (var i = 0; i < hs.length; i++) {
            var y = offsetIn(hs[i]) - 12;
            if (y > cur + 5) { go(y); return; }
          }
          go(drawer.scrollHeight);
        });
        bind('docs_nav_prev', function() {
          var hs = headings(), cur = drawer.scrollTop, target = 0;
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
    style = "cursor: pointer; color: #17a2b8; margin-left: 5px;",
    tabindex = "0",
    `data-toggle` = "popover",
    `data-placement` = "auto",
    `data-trigger` = "focus",
    `data-content` = content_html,
    `data-html` = "true",
    `data-bs-toggle` = "popover",
    `data-bs-placement` = "auto",
    `data-bs-trigger` = "focus",
    `data-bs-content` = content_html,
    `data-bs-html` = "true",
    onclick = "event.stopPropagation(); event.preventDefault(); if (typeof bootstrap !== 'undefined' && bootstrap.Popover) { new bootstrap.Popover(this).show(); }",
    icon("info-circle")
  )
}


# Shared DT wrapper for the compact summary tables on the Scientific Analysis
# tab, matching the Classification Suite look (dom = 't', scrollX). Paging is
# disabled because dom = 't' hides the paging controls: with the default
# pageLength, rows beyond the first page would be silently unreachable in
# variable-length tables (e.g. per-locality variogram parameters).
sci_dt <- function(df, escape = TRUE, header_tooltips = NULL) {
  if (is.null(df)) return(NULL)
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

update_premium_progress <- function(pct, message = NULL) {
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
