# ui_components.R - shiny tag / widget generators (cards, tooltips, docs
# drawer, modals, DT wrappers). register_expanded_modal registers observers on
# the session passed to it; nothing here creates top-level reactives.
# Sourced via ui_helpers.R.


#' Sidebar panel of an engine parameter (IDW power, TPS smoothing) with a scope
#' switch `<id>_mode`: "auto" (one setting for all localities, `global_ui`) or
#' "manual" (a value per locality and target, `manual_ui` plus Apply). Under
#' Per locality, `<id>_m_note` says which localities have their own value and
#' what the others run with.
tuning_ui <- function(id, label, global_ui, manual_ui, manual_btn_label,
                      top_extra_ui = NULL, extra_ui = NULL) {
  content <- tagList(
    top_extra_ui,
    radioButtons(paste0(id, "_mode"), paste(label, "applies to"),
                 choices = c("All localities" = "auto", "Per locality" = "manual"), inline = TRUE),
    conditionalPanel(condition = sprintf("input.%s_mode == 'auto'", id), global_ui),
    conditionalPanel(
      condition = sprintf("input.%s_mode == 'manual'", id),
      div(class = "mn-subsection",
          selectInput(paste0(id, "_m_loc"), "Locality", choices = NULL),
          # Offered only when the Predicted surface gets its own parameter
          # ("Fit Actual/Predicted separately"); otherwise it reuses the Actual one.
          conditionalPanel(
              condition = "(input.comp_mode == true || ['pred', 'pred_ss', 'resid'].includes(input.value_type)) && input.sep_fit == true",
              radioButtons(paste0(id, "_m_target"), "Target",
                           choices = c("Actual" = "act", "Predicted" = "pre"), inline = TRUE)
          ),
          uiOutput(paste0(id, "_m_note")),
          manual_ui,
          actionButton(paste0("apply_", id, "_manual"), manual_btn_label, class = "btn-default btn-block")
      )
    ),
    extra_ui
  )

  div(class = "mn-subsection", content)
}

#' A button that reveals a panel of related controls. Built on <details> so the
#' contents stay in the DOM while closed and every input and output inside
#' binds and updates as in a visible toolbar.
mn_popover <- function(label, ..., icon_tag = NULL, align = c("left", "right"),
                       width = "300px") {
  align <- match.arg(align)
  tags$details(
    class = paste("mn-popover", if (align == "right") "mn-popover-right"),
    tags$summary(class = "btn btn-default btn-sm", icon_tag, label),
    div(class = "mn-popover-panel", style = paste0("width: ", width, ";"), ...)
  )
}

# The north arrow of the Map Viewer overlay and the Data Setup mini-map, as a
# leaflet control (layer id "north_ctrl") on a widget or a proxy. It is drawn
# on the basemap itself, white over a shadow in both variants, so the control
# takes a class of its own: addControl()'s default, 'info legend', would put
# the white "N" on the legend's surface.
add_north_arrow <- function(map) {
  leaflet::addControl(map, position = "topleft", layerId = "north_ctrl", className = "mn-north-arrow",
    html = "<div style='text-align: center; color: white; font-family: Arial, sans-serif; pointer-events: none;'><div style='font-size: 16px; font-weight: bold; line-height: 1; margin-bottom: 4px; text-shadow: 1px 1px 2px black;'>N</div><svg width='30' height='30' viewBox='0 0 24 24' style='filter: drop-shadow(1px 1px 2px black);'><polygon points='12,2 7,22 12,17 17,22' fill='#e74c3c' stroke='white' stroke-width='1.5'/><polygon points='12,2 7,22 12,17' fill='#c0392b' stroke='white' stroke-width='1.5'/></svg></div>")
}

# Wraps a leaflet legend title so CSS can hide it: the Overlays "Variable Label
# in Legend" checkbox toggles body.mn-show-legend-title, so no legend is
# rebuilt and every legend answers in the same frame.
legend_var_title <- function(txt) {
  htmltools::HTML(paste0(
    "<span class=\"mn-legend-title\">", htmltools::htmlEscape(txt), "</span>"
  ))
}

# Stylesheet for the leaflet measure control (Map Viewer ruler), injected once
# in ui_main.R's head; a function so tests can load the exact rules. Selectors
# carry an extra qualifier to outrank the plugin's stylesheet, which is
# appended to <head> later. It restyles the button to leaflet's standard
# 26/30px `leaflet-bar` chrome (the plugin ships 36/44px) and trims the panel:
# the heading and the last-point coordinates are hidden, and Cancel/Finish
# keep only their icons (accessible names are set in add_map_ruler()).
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

# A measurement's own popup, from measure_path_metrics(). It REPLACES the
# plugin's text (spherical, CRS-unaware) and lives on the shape, so an older
# measurement keeps its own figures. Reuses the plugin's classes (h3, p,
# ul.tasks, a.zoomto, a.deletemarkup) for styling; mono-ruler-* are the
# handles add_map_ruler() wires. NULL below two vertices (the plugin's
# coordinate popup stands).
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
  # Area on both bases, as for lengths: ground area on the ellipsoid (terra;
  # the class areas use the same convention) and planimetric in the Target
  # Mapping CRS differ by the projection's area distortion.
  if (!is.null(res$area_geodesic) && is.finite(res$area_geodesic)) {
    lines <- paste0(lines, row("Area, ground (WGS84)", format_measure_area(res$area_geodesic)))
    if (!is.null(res$area_projected) && is.finite(res$area_projected)) {
      lines <- paste0(lines, row(paste0("Area, ", proj_lab),
                                 format_measure_area(res$area_projected)))
    }
  }
  # A self-crossing ring's area is withheld (its lobes cancel); say so, or the
  # missing rows read as a failure.
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

# Header row (title, PNG download, expand-to-modal) above a plot output; the
# server side is register_sci_plot() (server_map_viewer.R), which wires
# <id>_expand and <id>_dl (300-dpi PNG) to the plot's builder. Module callers
# pass namespaced ids and may set `expand_id` explicitly. `download = FALSE`
# drops the PNG button, `info` adds a tooltip icon, `click_id` wires the plot
# click, and `head_min_height` keeps wrapped titles from misaligning cards.
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

# ── Copy-a-table-to-the-clipboard ────────────────────────────────────────────
# Reads the rendered table off the DOM (exactly what is displayed, no server
# round trip) and copies it as HTML (Word, Excel cells) and tab-separated text.
# No colour literals: the payload is for another application, not app chrome.
copy_table_js <- function() {
  "
window.mnTableRows = function (targetId) {
  var root = document.getElementById(targetId);
  if (!root) return null;
  // A status message rendered as a one-cell table (sci_dt(NULL), a module's
  // empty state) is not a result: say what it says instead of copying it.
  var status = root.querySelector('table.mn-status-table tbody td');
  if (status) return { status: (status.textContent || '').trim() };
  // DataTables in scrollX mode clones the header into a table of its own, so
  // a single querySelector('table') would return a header with no body.
  var thead = root.querySelector('.dataTables_scrollHead thead, .dt-scroll-head thead');
  var tbody = root.querySelector('.dataTables_scrollBody tbody, .dt-scroll-body tbody');
  if (!thead || !tbody) {
    var t = root.querySelector('table');
    if (!t) return null;
    thead = t.querySelector('thead');
    tbody = t.querySelector('tbody');
  }
  var text = function (s) { return String(s == null ? '' : s).replace(/\\s+/g, ' ').trim(); };
  var cell = function (el) { return text(el.textContent); };
  var rowCells = function (tr) { return Array.prototype.map.call(tr.cells, cell); };
  var rows = function (sect) {
    if (!sect) return [];
    return Array.prototype.map.call(sect.rows, rowCells);
  };
  var head = rows(thead);
  var body;
  var total = null;
  // The tbody of a paginated DataTable holds the CURRENT PAGE only, so rows
  // are read through the DataTables API instead: every row that passes the
  // search, in display order, across all pages. A row not yet drawn has no
  // node, so its cells come from the API's display rendering.
  var bodyTable = tbody ? tbody.parentNode : null;
  var jq = window.jQuery;
  if (bodyTable && jq && jq.fn.dataTable && jq.fn.dataTable.isDataTable(bodyTable)) {
    var api = jq(bodyTable).DataTable();
    var sel = { search: 'applied', order: 'applied' };
    var cols = api.columns().indexes().toArray().filter(function (c) {
      return api.column(c).visible();
    });
    var scratch = document.createElement('div');
    body = api.rows(sel).indexes().toArray().map(function (r) {
      var node = api.row(r).node();
      if (node) return rowCells(node);
      return cols.map(function (c) {
        scratch.innerHTML = String(api.cell(r, c).render('display'));
        return text(scratch.textContent);
      });
    });
    // Server-side processing keeps other pages on the server; say so rather
    // than presenting one page as the whole table.
    total = api.page.info().recordsDisplay;
  } else {
    body = rows(tbody);
    // DT's empty state is one full-width 'No data available in table' cell.
    if (body.length === 1 && body[0].length === 1 && head.length && head[0].length > 1) body = [];
  }
  if (!head.length && !body.length) return null;
  return { head: head, body: body, total: total };
};

window.mnTablePayload = function (targetId) {
  var d = window.mnTableRows(targetId);
  if (!d) return null;
  if (d.status !== undefined) return { status: d.status };
  var esc = function (s) {
    return String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  };
  var isNum = function (s) { return /^-?(\\d+(\\.\\d+)?|\\.\\d+)([eE][-+]?\\d+)?$/.test(s); };
  var html = ['<table border=\"1\" cellspacing=\"0\" cellpadding=\"4\" style=\"border-collapse:collapse;font-family:Calibri,Arial,sans-serif;font-size:11pt;\">'];
  var lines = [];
  if (d.head.length) {
    html.push('<thead>');
    d.head.forEach(function (r) {
      html.push('<tr>' + r.map(function (c) {
        return '<th style=\"text-align:left;\">' + esc(c) + '</th>';
      }).join('') + '</tr>');
      lines.push(r.join('\\t'));
    });
    html.push('</thead>');
  }
  html.push('<tbody>');
  d.body.forEach(function (r) {
    html.push('<tr>' + r.map(function (c) {
      return '<td style=\"text-align:' + (isNum(c) ? 'right' : 'left') + ';\">' + esc(c) + '</td>';
    }).join('') + '</tr>');
    lines.push(r.join('\\t'));
  });
  html.push('</tbody></table>');
  return {
    html: html.join(''),
    text: lines.join('\\r\\n'),
    rows: d.body.length,
    total: d.total,
    cols: d.head.length ? d.head[0].length : (d.body[0] || []).length
  };
};

window.mnCopyFlash = function (btn, ok, msg) {
  var live = document.getElementById('mn_copy_live');
  // Clear first: a live region does not re-announce identical text, so a
  // second copy of the same table would otherwise pass in silence.
  if (live) {
    live.textContent = '';
    setTimeout(function () { live.textContent = msg; }, 60);
  }
  if (!btn) return;
  var ic = btn.querySelector('i');
  if (ic && !btn.getAttribute('data-mn-icon')) btn.setAttribute('data-mn-icon', ic.className);
  if (ic) ic.className = ok ? 'fa fa-check' : 'fa fa-exclamation-triangle';
  btn.classList.add(ok ? 'mn-copied' : 'mn-copy-failed');
  clearTimeout(btn.mnCopyTimer);
  btn.mnCopyTimer = setTimeout(function () {
    if (ic) ic.className = btn.getAttribute('data-mn-icon') || 'fa fa-copy';
    btn.classList.remove('mn-copied');
    btn.classList.remove('mn-copy-failed');
  }, 1600);
};

window.mnCopyTable = function (targetId, btn) {
  var p = window.mnTablePayload(targetId);
  if (!p) { window.mnCopyFlash(btn, false, 'Nothing to copy: this table is empty.'); return; }
  if (p.status !== undefined) {
    window.mnCopyFlash(btn, false, 'Nothing to copy: ' + (p.status || 'this table is empty.'));
    return;
  }
  var partial = p.total != null && p.total > p.rows;
  var done = function (ok) {
    window.mnCopyFlash(btn, ok && !partial, !ok
      ? 'Copy failed. Select the table and press Ctrl+C.'
      : partial
        ? ('Copied only ' + p.rows + ' of ' + p.total + ' rows: the rest are on other pages of this table.')
        : ('Copied ' + p.rows + ' rows x ' + p.cols + ' columns to the clipboard.'));
  };
  // execCommand path: the only one that reaches an insecure-origin session
  // (an app served over plain http from anything but localhost), and the
  // fallback whenever the async API is refused. The copy handler supplies both
  // flavours; the throwaway textarea exists because Chrome refuses
  // execCommand('copy') with no selection.
  var legacy = function () {
    var handler = function (e) {
      e.clipboardData.setData('text/html', p.html);
      e.clipboardData.setData('text/plain', p.text);
      e.preventDefault();
    };
    document.addEventListener('copy', handler);
    var ta = document.createElement('textarea');
    ta.value = p.text;
    ta.setAttribute('aria-hidden', 'true');
    ta.style.cssText = 'position:fixed;top:-1000px;left:-1000px;opacity:0;';
    document.body.appendChild(ta);
    ta.select();
    var ok = false;
    try { ok = document.execCommand('copy'); } catch (err) { ok = false; }
    document.removeEventListener('copy', handler);
    document.body.removeChild(ta);
    // The textarea held focus; removing it would drop keyboard focus to <body>.
    if (btn && btn.focus) btn.focus();
    done(ok);
  };
  if (navigator.clipboard && window.ClipboardItem && window.isSecureContext) {
    try {
      navigator.clipboard.write([new ClipboardItem({
        'text/html': new Blob([p.html], { type: 'text/html' }),
        'text/plain': new Blob([p.text], { type: 'text/plain' })
      })]).then(function () { done(true); }, legacy);
      return;
    } catch (err) { /* fall through to legacy */ }
  }
  legacy();
};
"
}

# Icon-only copy control: a plain <button> (no server input). `target_id` is
# the DOM id of the element containing the table (a DT output's own,
# namespaced id).
copy_table_btn <- function(target_id, label = NULL) {
  what <- if (is.character(label) && length(label) == 1 && nzchar(label)) {
    paste0(label, " table")
  } else {
    "table"
  }
  tags$button(
    type = "button",
    class = "btn btn-xs btn-light mn-copy-btn",
    title = "Copy this table to the clipboard (paste into Word or Excel)",
    "aria-label" = paste("Copy the", what, "to the clipboard"),
    onclick = sprintf("mnCopyTable('%s', this);", target_id),
    icon("copy")
  )
}

# A result table with the same title-and-tools strip a figure card carries.
# `...` is anything that belongs between the title and the table (a badge, a
# binning selector); `content` overrides the default DT output for a table that
# is emitted by a renderUI instead.
sci_table <- function(id, title = NULL, ..., title_tag = h5, label = NULL,
                      content = div(class = "table-container", DT::dataTableOutput(id))) {
  copy_label <- label %||% (if (is.character(title) && length(title) == 1) title else NULL)
  div(class = "sci-table-block",
      div(class = "sci-table-head",
          if (!is.null(title)) title_tag(title, class = "sci-table-title") else tags$span(),
          copy_table_btn(id, copy_label)
      ),
      ...,
      content
  )
}

# Unified results-card container: one plain surface with a hairline border.
# The title tells cards apart; a bar of colour per card would compete with the
# class-break and residual palettes the cards contain.
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
# (matched by column name; columns without an entry render normally). The
# browser draws a hover title as plain text, so a blank line ("\n\n")
# separates the points of a long one.
sci_metric_tooltips <- function() {
  c(
    "Source" = "Model and cross-validation design that produced this row's metrics.",
    "RMSE" = "Root Mean Square Error of the cross-validation residuals, in the variable's units. Lower is better.",
    "NRMSE (mean, %)" = "RMSE as a percentage of the absolute observed mean, i.e. CV(RMSE). Needs a ratio scale: n/a¹ when the observed values span zero, NA when the observed mean is exactly zero.",
    "NRMSE (SD)" = "RMSE divided by the standard deviation of the observed values: the prediction error as a multiple of the target's spread.\n\nUnchanged by shifting the variable's origin, so it stays defined for signed or centred targets.\n\nIt equals 1/RPD and sqrt((1 - NSE)(n - 1)/n), so it restates, rather than adds to, the evidence in those columns.",
    "MAE" = "Mean Absolute Error of the cross-validation residuals, in the variable's units. Less sensitive to single large errors than RMSE.",
    "R² (Corr)" = "Squared Pearson correlation between observed and CV-predicted values. Measures association only; insensitive to systematic bias.",
    "R² (NSE/Trad)" = "Nash-Sutcliffe efficiency (traditional R²): 1 - SSE/SStot against the observed mean. 1 = perfect, 0 = no better than predicting the mean, negative = worse than the mean.",
    "Bias (ME)" = "Mean Error, mean(observed - predicted): positive = model underpredicts on average, negative = overpredicts.",
    "Lin's CCC (Agree)" = "Lin's Concordance Correlation Coefficient: agreement with the 1:1 line, combining precision (correlation) and accuracy (bias/scale shift). 1 = perfect agreement. NA when either vector is constant.",
    "RPD (Prec)" = "Ratio of Performance to Deviation: SD(observed) / RMSE. Chemometrics convention: > 2 good, 1.4-2 fair, < 1.4 poor.",
    "RPIQ" = "Ratio of Performance to Interquartile distance: IQR(observed) / RMSE. The RPD analogue for skewed distributions, where the SD is a poor spread measure. Higher is better.",
    "SMAPE (%)" = "Symmetric Mean Absolute Percentage Error: scale-free accuracy; 0% is perfect.\n\nn/a¹ when the observed values span zero, where a single sign disagreement contributes the maximum term however small both values are.",
    "Moran's I" = "Spatial autocorrelation of the CV residuals (symmetric 8-nearest-neighbour weights). Read it against its null expectation E[I] = -1/(n-1) (shown per row on hover), not against 0.\n\nA clearly higher value is consistent with spatial structure the prediction procedure did not capture; it is not by itself an instruction to change engine. A value near E[I] is not proof of a clean model either: random folds leave each held-out point's neighbours in training, which can mask residual structure.\n\nRows scored under Spatial Block CV or kNNDM spatial folds are marked † and read differently (see the note under the table). NA* = the statistic could not be computed for this point set.",
    "Block-CV residual clustering" = "Moran's I of the pooled out-of-fold residuals under Spatial Block CV.\n\nThose residuals inherit the fold geometry and a shared extrapolation condition within each withheld block, so this statistic measures clustering of block-CV errors; fold geometry and shared extrapolation error confound it, and it cannot on its own diagnose a missing spatial trend. Its p-value is not reported.",
    "Spatial-fold residual clustering" = "Moran's I of the pooled out-of-fold residuals under kNNDM spatial folds.\n\nEach fold withholds whole groups of neighbouring samples, so those residuals inherit the fold geometry and a shared prediction condition within each group: this statistic measures clustering of the spatial-fold errors, fold geometry confounds it, and it cannot on its own diagnose a missing spatial trend. Its p-value is not reported.",
    "Moran p" = "Two-sided permutation p-value of Moran's I (999 seeded permutations; the smallest attainable value is 0.002). Small p = residual autocorrelation unlikely under spatial randomness.\n\nNA† under Spatial Block CV and kNNDM spatial folds, where residuals held out in whole groups are not exchangeable, so the permutation null does not describe them. NA* where Moran's I itself could not be computed."
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
    sci_table(dt_id, label = "regression coefficients"),
    tags$p(style = "font-size: 0.72em; opacity: 0.65; margin-top: 6px;",
           "Signif. codes: *** p ≤ 0.001, ** p ≤ 0.01, * p ≤ 0.05, . p ≤ 0.1. CI = 95% confidence interval (t-based)."),
    # OLS standard errors are biased low exactly when the residuals are
    # spatially autocorrelated, the reason RK kriges them; the estimates stay
    # unbiased. A caveat, not a GLS refit (that would be Universal Kriging).
    tags$p(style = "font-size: 0.72em; opacity: 0.65; margin-top: 2px;",
           tags$b("Read the p-values with care: "),
           "these standard errors, t statistics, confidence intervals and the F test ",
           "assume independent residuals. Regression Kriging kriges these residuals ",
           "precisely because they are spatially autocorrelated, which lowers the ",
           "effective sample size and biases the standard errors downward, so ",
           "significance is overstated. The residual field to judge that on is the ",
           "Internal Residual Variogram below, which is the variogram of these very ",
           "residuals: the more of its sill sits in the structured part rather than ",
           "the nugget, the more optimistic this table is. The coefficient ",
           "estimates themselves remain unbiased."),
    tags$details(style = "margin-top: 4px;",
      tags$summary("Raw R model summary", style = "cursor: pointer; font-size: 0.8em; opacity: 0.7;"),
      verbatimTextOutput(raw_id)
    )
  )
}

# The title of a modal that stops or fails a run.
danger_title <- function(text, icon = "exclamation-triangle") {
  shiny::tags$div(style = "color: var(--mn-danger); font-weight: 600;", shiny::icon(icon), text)
}

# The expand button over the top-right corner of a module's plot (its parent
# div is position: relative); register_expanded_modal() wires the modal to it.
expand_overlay_btn <- function(id, aria) {
  shiny::tags$button(id = id, type = "button", class = "btn btn-default action-button expand-icon-btn",
                     title = "Expand plot", "aria-label" = aria, shiny::icon("expand"))
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
    if (radar_special && inherits(p, "ggplot") && inherits(p$coordinates, "CoordPolar") &&
        nrow(p$data) > 0 && "variable" %in% colnames(p$data)) {
      d <- p$data
      fig <- plotly::plot_ly(type = 'scatterpolar')
      axis_labels <- p$scales$get_scales("x")$labels
      for(g in unique(d$group)) {
          dg <- d[d$group == g, ]
          complete <- all(is.finite(dg$value))
          if (complete) dg <- rbind(dg, dg[1, ])
          theta <- if (is.function(axis_labels)) axis_labels(dg$variable) else dg$variable
          fig <- plotly::add_trace(fig, r = dg$value, theta = theta, name = g,
            mode = if (complete) 'lines+markers' else 'markers',
            fill = if (complete) 'toself' else 'none')
      }
      upper <- max(d$value, na.rm = TRUE)
      if (!is.finite(upper) || upper <= 0) upper <- 1
      title <- plotly_title_html(p)
      fig <- plotly::layout(fig,
                            polar = list(radialaxis = list(visible = TRUE, range = c(0, upper))),
                            showlegend = TRUE,
                            legend = list(title = list(text = htmltools::htmlEscape(p$labels$colour %||% "Group"))),
                            title = list(text = title, x = 0.5))
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
    # The content scrolls in an inner element so its scrollbar stays clear of
    # the page's own.
    div(
      id = "docs_drawer_body",
      class = "docs-drawer-body",
      div(style = "display: flex; justify-content: space-between; align-items: center; border-bottom: 1px solid var(--mn-line); padding-bottom: 10px; margin-bottom: 15px;",
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
    # Floating navigation over the open drawer (top/end, previous/next
    # heading), all client-side.
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

# The text of an info popover, one paragraph per argument, so every choice
# and every separate point starts a line of its own. A named argument gets
# its name as a bold lead-in: a choice as the control labels it, or a point
# such as "Cost". The spacing comes from the theme (.popover-content p).
tip_html <- function(...) {
  x <- c(...)
  lead <- names(x) %||% character(length(x))
  paste0("<p>", ifelse(nzchar(lead), paste0("<b>", lead, ":</b> "), ""), x, "</p>",
         collapse = "")
}

# An info icon whose popover shows `text` (HTML) and closes when it loses
# focus. Bootstrap's popover sanitiser removes buttons, tables and style
# attributes, so the text uses plain tags (p, b, code, hr, links); a text of
# more than one point is built with tip_html().
info_tooltip <- function(id, text) {
  tags$span(
    id = paste0(id, "_info_icon"),
    class = "info-icon",
    style = "cursor: pointer; color: var(--mn-text-3); margin-left: 5px;",
    tabindex = "0",
    `data-toggle` = "popover",
    `data-placement` = "auto",
    `data-trigger` = "focus",
    # Attached to <body>, so sidebar and card containers cannot clip the panel.
    `data-container` = "body",
    `data-content` = text,
    `data-html` = "true",
    `data-bs-toggle` = "popover",
    `data-bs-placement` = "auto",
    `data-bs-trigger` = "focus",
    `data-bs-container` = "body",
    `data-bs-content` = text,
    `data-bs-html` = "true",
    onclick = "event.stopPropagation(); event.preventDefault(); if (typeof bootstrap !== 'undefined' && bootstrap.Popover) { new bootstrap.Popover(this, {container: 'body'}).show(); }",
    icon("info-circle")
  )
}

# The info icon of a variogram card that also draws the Total (Combined) view:
# what that view draws and why its lag axis ends where it does. Built at call
# time: POOLED_VGM_MIN_N (spatial_vgm.R) is sourced after the UI helpers.
pooled_vgm_info <- function(id) {
  info_tooltip(id, tip_html(
    "Total (Combined) pools the point pairs inside each locality; no pair joins two localities.",
    paste0("The lag axis stops at the half-diagonal of the smallest locality with at least ", POOLED_VGM_MIN_N,
           " located values, so every locality contributes over the whole axis. One small locality therefore ",
           "shortens the curve for all; select a locality to see its own lag range.")))
}


# One-line statement of the sample a matrix-valued panel used: rows complete
# across EVERY selected variable (correlations, partial correlations, PCA, the
# collinearity screen), so n differs from the selection's row count.
complete_case_note <- function(n_used, n_total) {
  dropped <- max(0, n_total - n_used)
  sprintf("Complete cases: n = %d of %d rows%s.", n_used, n_total,
          if (dropped > 0) sprintf(" (%d dropped for missing values)", dropped) else "")
}

# Browser twin of format_sig() (ui_formatting.R), mounted once in ui_main.R's
# head and applied through sig_render_defs(): numeric columns keep their
# numbers for sorting and only the display is formatted. Non-numeric cells
# pass through.
format_sig_js <- function() {
  "
window.mnFormatSig = function (d) {
  if (d === null || d === undefined || d === '') return '';
  if (typeof d === 'string' && !/^\\s*-?(\\d+\\.?\\d*|\\.\\d+)([eE][-+]?\\d+)?\\s*$/.test(d)) return d;
  var x = Number(d);
  if (!isFinite(x)) return d;
  if (x === Math.round(x) && Math.abs(x) < 1e15) return (x + 0).toFixed(0);
  if (Math.abs(x) >= 1000) return (Math.round(x) + 0).toFixed(0);
  if (Math.abs(x) < 1e-4) return x.toExponential(3).replace(/e([+-])(\\d)$/, 'e$10$2');
  return String(Number(x.toPrecision(4)));
};
"
}

# Point-layer methods of the Map Viewer's leaflet widgets, mounted once in
# ui_main.R's head. R calls them with leaflet::invokeMethod() on a leafletProxy,
# so they queue with, and after, the proxy's other calls (the markers they
# address). Markers are found by layer id: POINT_LAYER_ID_PREFIX + row. The
# labels are one layer, its HTML built from escaped label text: a marker per
# label is placed by a 3D transform, which gives each label a compositing layer
# of its own, and ten thousand of them took seconds to draw, zoom or pan.
point_layer_js <- function() {
  "
(function () {
  function esc(s) {
    return String(s).replace(/[&<>\"']/g, function (c) {
      return {'&': '&amp;', '<': '&lt;', '>': '&gt;', '\"': '&quot;', \"'\": '&#39;'}[c];
    });
  }
  function at(v, i) { return Array.isArray(v) ? v[i] : v; }
  function install() {
    var LW = window.LeafletWidget;
    if (!LW || !LW.methods) return false;
    if (LW.methods.mnStylePoints) return true;
    // Fill, outline and fill opacity (each a value or one per marker) and the radius.
    LW.methods.mnStylePoints = function (prefix, n, fill, stroke, fillOpacity, radius) {
      var lm = this.layerManager;
      for (var i = 0; i < n; i++) {
        var m = lm.getLayer('marker', prefix + (i + 1));
        if (!m) continue;
        m.setStyle({fillColor: at(fill, i), color: at(stroke, i), fillOpacity: at(fillOpacity, i)});
        m.setRadius(radius);
      }
    };
    // The label layer: one container in the marker pane holding an empty box
    // per label, placed in pixels at its point. Hidden during a zoom animation
    // (leaflet-zoom-hide) and placed again when the zoom ends.
    var Labels = L.Layer.extend({
      initialize: function (latlngs, html) { this._latlngs = latlngs; this._html = html; },
      onAdd: function (map) {
        this._el = L.DomUtil.create('div', 'mn-pt-labels leaflet-zoom-hide', map.getPane('markerPane'));
        this._el.innerHTML = this._html;
        this._place();
      },
      onRemove: function () { L.DomUtil.remove(this._el); this._el = null; },
      getEvents: function () { return {zoom: this._place, viewreset: this._place}; },
      _place: function () {
        var boxes = this._el.children;
        for (var i = 0; i < boxes.length; i++) {
          var p = this._map.latLngToLayerPoint(this._latlngs[i]);
          boxes[i].style.left = Math.round(p.x) + 'px';
          boxes[i].style.top = Math.round(p.y) + 'px';
        }
      }
    });
    // One label per marker with text (null: none); null instead of text clears them.
    // The labels are not interactive and measure nothing.
    LW.methods.mnPointLabels = function (prefix, text) {
      var map = this;
      if (map._mnPointLabels) { map.removeLayer(map._mnPointLabels); map._mnPointLabels = null; }
      if (text === null || text === undefined) return;
      if (!Array.isArray(text)) text = [text];
      var lm = map.layerManager, latlngs = [], html = [];
      for (var i = 0; i < text.length; i++) {
        if (text[i] === null) continue;
        var m = lm.getLayer('marker', prefix + (i + 1));
        if (!m) continue;
        latlngs.push(m.getLatLng());
        html.push('<div class=\"mn-pt-label\"><span>' + esc(text[i]) + '</span></div>');
      }
      map._mnPointLabels = new Labels(latlngs, html.join('')).addTo(map);
    };
    LW.methods.mnPointLabelSize = function (px) {
      this.getContainer().style.setProperty('--mn-pt-label-size', px + 'px');
    };
    return true;
  }
  install();
  if (window.jQuery) jQuery(install);
})();
"
}

# The Warnings box's method for the Map Viewer's leaflet widgets, mounted once
# in ui_main.R's head and called by add_map_warnings() right after the box.
# Keep pointer events within the box. Its close button hides every box and
# moves focus to Overlays, where the Warnings checkbox shows the boxes again.
map_warnings_js <- function() {
  "
(function () {
  function install() {
    var LW = window.LeafletWidget;
    if (!LW || !LW.methods) return false;
    if (LW.methods.mnWireWarnings) return true;
    LW.methods.mnWireWarnings = function () {
      var boxes = this.getContainer().querySelectorAll('.mn-warn-control');
      for (var i = 0; i < boxes.length; i++) {
        var ctrl = boxes[i];
        if (ctrl._mnWired) continue;
        ctrl._mnWired = true;
        L.DomEvent.disableClickPropagation(ctrl);
        L.DomEvent.disableScrollPropagation(ctrl);
        var btn = ctrl.querySelector('.mn-warn-close');
        if (btn) L.DomEvent.on(btn, 'click', function (e) {
          L.DomEvent.stop(e);
          var cb = document.getElementById('show_map_warnings');
          if (cb && this === document.activeElement) {
            var overlays = cb.closest('.mn-popover');
            if (overlays) overlays.querySelector('summary').focus({preventScroll: true});
          }
          document.body.classList.add('mn-hide-map-warnings');
          if (cb && cb.checked) { cb.checked = false; jQuery(cb).trigger('change'); }
        });
      }
    };
    return true;
  }
  install();
  if (window.jQuery) jQuery(install);
})();
"
}

# The three markers a result table can print in place of a number, and the one
# sentence each needs under every table that uses it; table_footnote() emits
# only the markers a table used.
METRIC_MARKER_NOTES <- c(
  scale = paste("n/a¹ Not reported for targets whose observed values span zero.",
                "Mean- and percentage-normalised errors have no interpretation on a",
                "signed or centred scale. NRMSE (SD) is reported instead."),
  block = paste("† Under Spatial Block CV and kNNDM spatial folds the pooled out-of-fold",
                "residuals inherit the spatial fold geometry and a shared extrapolation",
                "condition within each withheld group of samples, so this statistic measures",
                "clustering of those errors and its usual reference distribution does not",
                "hold: the p-value is not reported, and the value cannot on its own diagnose",
                "a missing spatial trend."),
  na    = "NA* The metric could not be computed for this point set. See the Run Log.",
  moran_degenerate = paste("NA* Moran's I is unavailable when",
                           "CV errors have no usable variance."),
  negative_r = paste("A superscript minus means higher observed values tend to have lower predictions.",
                     "R² (Corr) measures the strength of the linear relationship.")
)

table_footnote <- function(markers) {
  notes <- unname(METRIC_MARKER_NOTES[intersect(names(METRIC_MARKER_NOTES), markers)])
  if (!length(notes)) return(NULL)
  htmltools::tags$div(class = "mn-table-note", lapply(notes, htmltools::tags$div))
}

# DataTables columnDefs entry that displays `cols` of `df` through
# mnFormatSig() while sorting still sees the numbers. `rownames`
# shifts the column index when the table shows row names.
sig_render_defs <- function(df, cols, rownames = FALSE, minus_flag = NULL) {
  idx <- match(cols, names(df))
  idx <- idx[!is.na(idx)]
  if (!length(idx)) return(list())
  suffix <- if (is.null(minus_flag)) "" else {
    sprintf(" + (row[%d] === true ? '<sup>&minus;</sup>' : '')", minus_flag)
  }
  list(list(targets = idx - 1L + as.integer(isTRUE(rownames)),
            render = DT::JS(
              "function (data, type, row) {",
              "  if (type !== 'display' || typeof window.mnFormatSig !== 'function') return data;",
              paste0("  return window.mnFormatSig(data)", suffix, ";"),
              "}")))
}

# Shared DT wrapper for the compact Scientific Analysis tables (dom = 't', as
# in the Classification Suite). Paging is off because dom = 't' hides its
# controls, which would strand rows past the first page. `scroll_x = FALSE`
# keeps header and body one table for tables that fit the narrowest viewport.
# `signif_cols` are shown at four significant digits. `superscript_minus`
# names one of those columns and gives the logical flag for each row.
sci_dt <- function(df, escape = TRUE, header_tooltips = NULL, scroll_x = TRUE,
                   signif_cols = NULL, superscript_minus = NULL) {
  # Never return NULL: DT's binding reads `data.lazyRender` before its null
  # check, so a NULL sent to a hidden table throws inside Shiny's message
  # dispatch and drops the rest of the batch.
  if (is.null(df)) {
    df <- data.frame(Status = "No data for this selection.")
    header_tooltips <- NULL
    escape <- TRUE
  }
  # A one-cell Status/Error frame is a message, not a result: the class tells
  # the copy button to report it instead of copying a 1 x 1 "table".
  status_like <- ncol(df) == 1 && nrow(df) == 1 && names(df)[1] %in% c("Status", "Error")
  tbl_class <- if (status_like) "display mn-status-table" else "display"
  opts <- list(dom = 't', paging = FALSE, scrollX = isTRUE(scroll_x))
  if (any(superscript_minus$rows)) {
    minus_flag <- ncol(df)
    df[[".superscript_minus"]] <- superscript_minus$rows
    col <- superscript_minus$column
    opts$columnDefs <- c(sig_render_defs(df, setdiff(signif_cols, col)),
                         sig_render_defs(df, col, minus_flag = minus_flag),
                         list(list(targets = minus_flag, visible = FALSE, searchable = FALSE)))
  } else if (!is.null(signif_cols)) {
    opts$columnDefs <- sig_render_defs(df, signif_cols)
  }
  if (!is.null(header_tooltips)) {
    ths <- lapply(names(df), function(nm) {
      if (nm %in% names(header_tooltips)) {
        htmltools::tags$th(nm, title = unname(header_tooltips[[nm]]),
                           style = "cursor: help; text-decoration: underline dotted 1px;")
      } else {
        htmltools::tags$th(nm)
      }
    })
    container <- htmltools::tags$table(class = tbl_class,
                                       htmltools::tags$thead(do.call(htmltools::tags$tr, ths)))
    return(DT::datatable(df, options = opts, rownames = FALSE, escape = escape,
                         container = container, class = tbl_class))
  }
  DT::datatable(df, options = opts, rownames = FALSE, escape = escape, class = tbl_class)
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

# ── The covariate check card ────────────────────────────────────────────────
# One card per checked population: a locality in the interpolation gate, the
# pooled scope in the Classification Suite. Every number is read from the
# screen through collinearity_profile() (ui_formatting.R). The dialogs render
# on the main R process, so each card is written as strings in one vectorised
# pass: built tag by tag, twenty localities of thirty covariates took seconds.
# Labels are escaped once (.cl_esc()); column IDs appear only as checkbox
# values, element IDs come from the caller's input ID.

# Position on the chart's fixed axis, VIF 1 to 1000 on a log scale, in percent.
.vif_axis_pos <- function(v) 100 * log10(pmin(pmax(v, 1), 1000)) / 3

.cl_esc <- function(x) htmltools::htmlEscape(as.character(x), attribute = TRUE)
.cl_num <- function(n) {
  s <- sprintf("%.0f", as.numeric(n))
  big <- !is.na(n) & abs(as.numeric(n)) >= 1000
  if (any(big)) s[big] <- prettyNum(s[big], big.mark = ",")
  s
}
.cl_count <- function(n, noun) sprintf("%s %s%s", .cl_num(n), noun, if (n == 1) "" else "s")
.cl_thr <- function(thr) sprintf("%g", thr)
.cl_badge <- function(k) sprintf('<span class="mn-cl-step">%s</span>', k)
.cl_signed <- function(r, digits = 2L) sub("^-", "&#8722;", sprintf("%+.*f", digits, r))
.cl_incomplete <- function(status) !status %in% c("checked", "one_candidate", "no_candidates", "checking")
# Near-duplicate pairs among the covariates of the VIF check.
.cl_dup_count <- function(prof, pairs) {
  v <- prof$rows$var
  if (!NROW(pairs) || !length(v)) return(0L)
  sum(pairs$near_duplicate %in% TRUE & pairs$var1 %in% v & pairs$var2 %in% v)
}

# The removal checkboxes, as checkboxGroupInput(input_id, NULL, choiceValues
# = values, choiceNames = lapply(rows, HTML)) writes them (test-multicollinearity.R
# compares the two), without its per-choice rendering pass.
.cl_checkbox_group <- function(input_id, values, rows) {
  id <- .cl_esc(input_id)
  sprintf(paste0('<div id="%1$s" class="form-group shiny-input-checkboxgroup shiny-input-container" ',
                 'role="group" aria-labelledby="%1$s-label"><label class="control-label shiny-label-null" ',
                 'for="%1$s" id="%1$s-label"></label><div class="shiny-options-group">%2$s</div></div>'),
          id, paste0('<div class="checkbox"><label><input type="checkbox" name="', id, '" value="',
                     .cl_esc(values), '"/><span>', rows, '</span></label></div>', collapse = ""))
}

# Chart rows: one per covariate of `rv` (collinearity_profile()$rows plus a
# `reason` column, NA for rows of the VIF check). Marks are decorative; each
# row's title and hidden text give its values in words.
.cl_rows <- function(rv, L, thr, has_rm, n_first, missing, plot) {
  n <- nrow(rv)
  name <- L(rv$var)
  removed <- !is.na(rv$step)
  singular <- removed & !is.finite(rv$at)
  checked <- is.na(rv$reason)
  f_first <- format_vif(rv$first); f_final <- format_vif(rv$final)
  x_first <- format_vif(rv$first, exact = TRUE); x_final <- format_vif(rv$final, exact = TRUE)
  x_at <- format_vif(rv$at, above = thr, exact = TRUE)
  changed <- has_rm & !removed & !rv$sole & nzchar(f_first) & nzchar(f_final) & f_first != f_final
  miss <- if (length(missing)) unname(missing[rv$var]) else rep(NA_integer_, n)
  miss <- !is.na(miss) & miss > 0
  n_miss <- if (length(missing)) .cl_num(unname(missing[rv$var])) else ""
  value <- ifelse(!checked, paste0('<span class="mn-cl-reason">', rv$reason, '</span>'),
    ifelse(removed, paste0(.cl_badge(rv$step), "<b>", ifelse(singular, "singular", format_vif(rv$at, above = thr)), "</b>"),
      ifelse(changed, paste0('<span class="was">', f_first, '</span> &#8594; <b>', f_final, '</b>'),
        paste0("<b>", ifelse(nzchar(f_final), f_final, f_first), "</b>"))))
  with_all <- ifelse(nzchar(x_first), paste0("VIF ", x_first, " with ",
    if (n_first == 2L) "both covariates" else paste("all", n_first, "covariates")), "")
  lead <- ifelse(nzchar(with_all), paste0(with_all, "; "), "")
  words <- ifelse(!checked, rv$reason_long,
    ifelse(singular, paste0(lead, "removed by Auto-Drop at step ", rv$step, ", where the covariates left ",
                            "were linearly dependent and no VIF is defined"),
    ifelse(removed & rv$step == 1L & nzchar(with_all), paste0(with_all, "; removed by Auto-Drop at step 1"),
    ifelse(removed, paste0(lead, "VIF ", x_at, " at step ", rv$step, ", where Auto-Drop removed it"),
    ifelse(rv$sole, paste0(lead, "the only covariate left after Auto-Drop, so its VIF is 1 by definition"),
    ifelse(!has_rm | !nzchar(x_final), with_all, paste0(lead, "VIF ", x_final, " after Auto-Drop")))))))
  words <- paste0(words, ifelse(miss, paste0("; ", n_miss, " missing"), ""), ".")
  track <- if (plot) {
    pos <- function(v) ifelse(is.finite(v), .vif_axis_pos(v), NA_real_)
    p1 <- if (has_rm) pos(rv$first) else rep(NA_real_, n)
    p2 <- ifelse(removed, ifelse(singular, 100, pos(rv$at)), pos(rv$final))
    both <- is.finite(p1) & is.finite(p2)
    near <- both & abs(p1 - p2) < 0.3
    paste0('<span class="mn-cl-track" aria-hidden="true"><span class="mn-cl-zone"></span><span class="mn-cl-thr"></span>',
      ifelse(both & !near, sprintf('<span class="mn-cl-seg" style="left:%.2f%%;width:%.2f%%"></span>',
                                   pmin(p1, p2), abs(p1 - p2)), ""),
      ifelse(is.finite(p1) & !(removed & near), sprintf('<span class="mn-cl-dot first" style="left:%.2f%%"></span>', p1), ""),
      ifelse(removed, sprintf('<span class="mn-cl-x%s" style="left:%.2f%%"></span>', ifelse(singular, " sing", ""), p2),
        ifelse(is.finite(p2), sprintf('<span class="mn-cl-dot final" style="left:%.2f%%"></span>', p2), "")),
      '</span>')
  } else ""
  paste0('<span class="mn-cl-row', ifelse(removed, " is-removed", ""), if (!plot) " noplot", '" title="',
         name, ": ", words, '"><span class="mn-cl-name"><span class="t">', name, '</span>',
         ifelse(miss, paste0('<span class="mn-cl-miss" aria-hidden="true">&#183; ', n_miss, ' missing</span>'), ""),
         ifelse(rv$sole, '<span class="mn-cl-miss" aria-hidden="true">&#183; only one left</span>', ""),
         '</span>', track, '<span class="mn-cl-val" aria-hidden="true">', value,
         '</span><span class="sr-only">', words, '</span></span>')
}

# Decades and the threshold. A threshold within a third of a decade of a
# decade (5 and 10) takes that decade's place, which keeps an unlabelled tick.
.cl_axis <- function(thr) {
  major <- c(1, 10, 100, 1000)
  close <- major != thr & abs(log10(major / thr)) < 0.35
  lab <- sort(unique(c(major[!close], thr)))
  cls <- paste0(ifelse(lab == thr, " thr", ""), ifelse(lab == 1, " lo", ifelse(lab == 1000, " hi", "")))
  paste0('<div class="mn-cl-axisrow" aria-hidden="true"><span class="mn-cl-cbspace"></span>',
    '<span class="mn-cl-row"><span class="mn-cl-axlab"><span>VIF</span><span>explained by the others</span></span>',
    '<span class="mn-cl-axis">',
    paste0(sprintf('<span class="mn-cl-tick minor" style="left:%.3f%%"></span>', .vif_axis_pos(major[close])), collapse = ""),
    paste0(sprintf('<span class="mn-cl-tick%s" style="left:%.3f%%"><span class="v">%s</span><span class="r2">%s</span></span>',
                   cls, .vif_axis_pos(lab), formatC(lab, format = "fg", big.mark = ","),
                   sprintf("%g%%", round(100 * (1 - 1 / lab), 1))), collapse = ""),
    '</span><span></span></span></div>')
}

.cl_legend <- function(plot, has_rm, first_singular, hint) {
  key <- function(mark, text) paste0('<span class="k">', mark, text, '</span>')
  dot <- function(cls) sprintf('<span class="mn-cl-dot %s" aria-hidden="true"></span>', cls)
  parts <- c(
    if (plot && !has_rm) key(dot("final"), "VIF"),
    if (plot && has_rm && !first_singular) key(dot("first"), "before Auto-Drop"),
    if (plot && has_rm) key(dot("final"), "after Auto-Drop"),
    if (plot && has_rm) key('<span class="mn-cl-x" aria-hidden="true"></span>', "removed by Auto-Drop, step number at right"),
    if (plot && first_singular) '<span class="k hint">With every covariate included the design is singular, so the first step has no VIFs to plot.</span>',
    if (hint) '<span class="k hint">Tick covariates to remove them yourself, then Apply removals and recheck.</span>')
  if (!length(parts)) return("")
  paste0('<div class="mn-cl-legend">', paste(parts, collapse = ""), '</div>')
}

# The chart of one screen: a row per removable covariate, the VIF check's
# rows first (sorted as collinearity_profile() sorts them), then the
# removable covariates outside it with their reason. Without `input_id` the
# rows are read-only.
.cl_chart <- function(screen, prof, L, removable, input_id = NULL, hint = FALSE) {
  st <- screen$diagnostics$status %||% "not_checked"
  vif <- prof$rows
  vif <- if (is.null(vif)) vif else vif[vif$var %in% removable, , drop = FALSE]
  rest <- setdiff(removable, vif$var)
  plot <- NROW(vif) > 0
  if (!plot && !(.cl_incomplete(st) && length(rest))) return("")
  colloc <- screen$collocated %||% integer(0)
  short <- rest %in% screen$dropped_collocation
  constant <- rest %in% screen$dropped_constant
  if (plot) vif$reason <- vif$reason_long <- NA_character_
  rows <- rbind(
    if (plot) vif[c("var", "first", "final", "step", "at", "sole", "reason", "reason_long")],
    if (length(rest)) data.frame(var = rest, first = NA_real_, final = NA_real_, step = NA_integer_,
      at = NA_real_, sole = FALSE,
      reason = ifelse(short, paste0("co-located: ", unname(colloc[rest])), ifelse(constant, "constant here", "not checked")),
      reason_long = ifelse(short, paste0("not in the VIF check: ", unname(colloc[rest]), " co-located samples, fewer than the ",
                                         CK_MIN_COLLOCATED, " co-kriging needs"),
                           ifelse(constant, "not in the VIF check: constant here", "not checked")),
      stringsAsFactors = FALSE))
  has_rm <- length(prof$removals) > 0
  html <- .cl_rows(rows, L, prof$threshold, has_rm, NROW(prof$rows), prof$missing, plot)
  group <- if (!is.null(input_id)) .cl_checkbox_group(input_id, rows$var, html) else
    paste0('<div class="mn-cl-ro">', paste0('<div class="mn-cl-ro-row"><span class="mn-cl-cbspace"></span>',
                                             html, '</div>', collapse = ""), '</div>')
  paste0(
    if (plot) paste0('<div class="mn-cl-charthead"><span>Variance inflation factor (VIF), log scale</span>',
                     '<span class="mn-cl-key">shaded: above ', .cl_thr(prof$threshold), '</span></div>')
    else '<div class="mn-cl-charthead"><span>Covariates</span></div>',
    sprintf('<div class="mn-cl-chart%s" style="--cl-thr:%.3f%%">', if (is.null(input_id)) " mn-cl-readonly" else "",
            .vif_axis_pos(prof$threshold)),
    group, if (plot) .cl_axis(prof$threshold), '</div>',
    .cl_legend(plot, has_rm, prof$first_singular, hint && !is.null(input_id)))
}

.cl_verdict <- function(screen, prof, L) {
  d <- screen$diagnostics %||% list()
  st <- d$status %||% "not_checked"
  thr <- .cl_thr(prof$threshold)
  if (identical(st, "checking"))
    return('<div class="mn-cl-verdict wait">Checking covariates&#8230; The result appears here when the check completes.</div>')
  if (.cl_incomplete(st)) {
    reason <- if (length(d$reason)) .cl_esc(sub("[.]$", "", sub("^Warning in [^:]*: ", "", d$reason[1])))
    why <- switch(st,
      # The run log's refusal, continuing the sentence.
      insufficient_rows = if (length(reason)) sub("^Insufficient", "insufficient", reason) else
        "fewer than three locations carry every covariate",
      failed = paste0("the check failed", if (length(reason)) paste0(" (", reason, ")")),
      "these covariates were not checked")
    return(paste0('<div class="mn-cl-verdict warn"><span><b>Check incomplete:</b> ', why,
      ". This is not evidence that the covariates are independent.",
      if (any(prof$missing > 0)) " The counts after the names show where values are missing; removing the sparsest covariate usually lets the check run.",
      '</span></div>'))
  }
  if (identical(st, "one_candidate")) return('<div class="mn-cl-verdict">One covariate: a VIF needs at least two.</div>')
  if (identical(st, "no_candidates"))
    return('<div class="mn-cl-verdict">No numeric covariate to check here.</div>')
  if (!NROW(prof$rows)) return(paste0('<div class="mn-cl-verdict">', if (length(screen$kept))
    "Only one covariate varies here: a VIF needs at least two." else "Every covariate is constant here.", '</div>'))
  if (!length(prof$removals)) {
    # Constants and collocation exclusions have their chips; near-duplicate
    # pairs, measured on the locations each pair shares, the staircase.
    dup <- .cl_dup_count(prof, d$pairs)
    after <- if (dup) {
      sprintf(" %d pair%s %s |r| above %g on the locations where both are measured (All pairwise correlations).",
              dup, if (dup == 1L) "" else "s", if (dup == 1L) "has" else "have", d$pairwise_threshold %||% 0.95)
    } else if (!length(screen$dropped_constant) && !length(screen$dropped_collocation)) " Nothing to remove."
    return(paste0('<div class="mn-cl-verdict ok"><span class="mn-cl-okmark" aria-hidden="true">&#10003;</span>',
                  '<span>Every VIF is ', thr, ' or less.', after, '</span></div>'))
  }
  paste0('<div class="mn-cl-verdict act"><span>Auto-Drop removes ', length(prof$removals), ' of ', nrow(prof$rows),
         ', one at a time, until every VIF is ', thr, ' or less:</span>',
         .cl_pills(prof, L), '</div>')
}

.cl_pills <- function(prof, L) {
  v <- vapply(prof$removals, `[[`, character(1), "var")
  paste0('<span class="mn-cl-pill act" title="', L(v), '">', .cl_badge(seq_along(v)),
         '<span class="t">', L(v), '</span></span>', collapse = "")
}

.cl_chips <- function(title, vars, L, cls = "", counts = NULL) {
  if (!length(vars)) return("")
  paste0('<div class="mn-cl-chips"><span>', title, '</span>',
         paste0('<span class="mn-cl-pill', cls, '" title="', L(vars), '"><span class="t">', L(vars), '</span>',
                if (!is.null(counts)) paste0('<span class="n">', .cl_num(counts), '</span>') else "",
                '</span>', collapse = ""), '</div>')
}

# Chips every screen derives for itself: constants (`constant_title` words
# them for the engine), co-kriging collocation exclusions and non-finite
# values.
.cl_screen_chips <- function(screen, L, constant_title) {
  nf <- screen$diagnostics$nonfinite_by_var
  nf <- nf[nf > 0]
  paste0(.cl_chips(constant_title, screen$dropped_constant, L),
         .cl_chips(sprintf("Too few co-located samples for co-kriging (fewer than %d), removed whatever you answer:",
                           CK_MIN_COLLOCATED), screen$dropped_collocation, L),
         .cl_chips("Non-finite values treated as missing:", names(nf), L, counts = nf))
}

# Signed correlation bars: one hue, the sign carried by direction; dotted
# guides at the near-duplicate threshold.
.cl_bars <- function(p, L, n_ref, pw) {
  half <- 50 * abs(p$r)
  dup <- p$near_duplicate %in% TRUE
  paste0('<div class="mn-cl-corr"><span class="mn-cl-cname" title="', L(p$var), '">', L(p$var), '</span>',
    '<span class="mn-cl-cbar" aria-hidden="true">',
    sprintf('<span class="mn-cl-c95" style="left:%.2f%%"></span><span class="mn-cl-c95" style="left:%.2f%%"></span>',
            50 - 50 * pw, 50 + 50 * pw),
    '<span class="zero"></span>',
    sprintf('<span class="fill%s" style="left:%.2f%%;width:%.2f%%"></span>', ifelse(dup, " dup", ""),
            ifelse(p$r < 0, 50 - half, 50), half),
    '</span><span class="mn-cl-cval">', .cl_signed(p$r),
    ifelse(p$n != n_ref, paste0(" <small>n=", .cl_num(p$n), "</small>"), ""), '</span>',
    ifelse(dup, '<span class="mn-cl-tag">near-duplicate</span>', "<span></span>"), '</div>', collapse = "")
}

# One collapsible block per removal, the first open: the removal's VIF and
# what it means, the next highest VIF at that step, and the strongest pairs.
.cl_why <- function(prof, L, pw) {
  if (!length(prof$removals)) return("")
  thr <- prof$threshold
  blocks <- vapply(prof$removals, function(rm) {
    others <- setdiff(rm$candidates, rm$var)
    n_o <- length(others)
    lead <- if (identical(rm$status, "singular")) {
      sprintf(paste0("The %d covariates left at this step are linearly dependent, so no VIF is defined; ",
                     "Auto-Drop removes the more redundant member of the most correlated pair."), length(rm$candidates))
    } else if (n_o == 1L) {
      paste0("VIF ", format_vif(rm$at, thr), " comes from its correlation with ", L(others),
             " alone: VIF = 1 / (1 &#8722; r&#178;).")
    } else {
      paste0("VIF ", format_vif(rm$at, thr), ": the other ", n_o, " together explain ", vif_share_text(rm$at),
             " of its variation",
             if (!is.null(rm$bound)) paste0("; its strongest partner alone, ", L(rm$partners$var[1]),
                                            ", explains ", share_text(rm$bound)), ".")
    }
    runner <- if (length(rm$runner)) paste0('<span class="mn-cl-runner">Next highest at this step: ',
      L(names(rm$runner)), ", ", format_vif(rm$runner, thr),
      if (rm$runner > thr) paste0(" (also above ", .cl_thr(thr), "; rechecked after this removal)"), ".</span>") else ""
    bars <- if (NROW(rm$partners)) paste0('<div class="mn-cl-minihead">',
      if (n_o == 1L) "Correlation" else paste("Its strongest correlations with the other", n_o), '</div>',
      .cl_bars(rm$partners, L, rm$n_complete, pw)) else ""
    paste0('<details class="mn-cl-why"', if (rm$step == 1L) " open", '><summary>', .cl_badge(rm$step),
           '<span class="mn-cl-whytext"><b>', L(rm$var), '</b><span class="mn-cl-lead">', lead, '</span>',
           runner, '</span></summary><div class="mn-cl-whybody">', bars, '</div></details>')
  }, character(1))
  paste0('<div class="mn-cl-whys"><div class="mn-cl-subhead">Why each removal</div>', paste(blocks, collapse = ""),
         '<div class="mn-cl-note">Correlations are shown for context. A VIF measures redundancy with all the ',
         'other covariates together, so a single pair need not explain it.</div></div>')
}

# Every pair of the VIF check's covariates as a staircase: row i holds its
# correlations with covariates 1 to i - 1, then its own number, which heads
# column i. The tint is capped at 45% so the text keeps 4.5:1 in both variants.
# Shown with removals, or with a near-duplicate pair the VIF did not flag.
.cl_staircase <- function(prof, pairs, L) {
  v <- prof$rows$var
  k <- length(v)
  if (k < 3L || !NROW(pairs) || !(length(prof$removals) || .cl_dup_count(prof, pairs))) return("")
  R <- matrix(NA_real_, k, k); N <- matrix(NA_integer_, k, k); D <- matrix(FALSE, k, k)
  i1 <- match(pairs$var1, v); i2 <- match(pairs$var2, v)
  ok <- !is.na(i1) & !is.na(i2)
  ij <- cbind(c(i1[ok], i2[ok]), c(i2[ok], i1[ok]))
  R[ij] <- rep(pairs$r[ok], 2L); N[ij] <- rep(as.integer(pairs$n[ok]), 2L)
  D[ij] <- rep(pairs$near_duplicate[ok] %in% TRUE, 2L)
  lab <- L(v)
  step <- prof$rows$step
  # Grid items row by row: the label, cells j < i, the row's number, blanks.
  G <- matrix("<span></span>", k, k)
  low <- which(lower.tri(G))
  r <- R[low]
  G[low] <- ifelse(is.finite(r),
    sprintf('<span class="mn-cl-hc%s" style="--cl-a:%d%%" title="%s &#8596; %s: r = %s (n = %s)">%s</span>',
            ifelse(D[low], " dup", ""), as.integer(round(45 * abs(r))), lab[row(G)[low]], lab[col(G)[low]],
            .cl_signed(r, 3L), .cl_num(N[low]), sub("^-", "&#8722;", sprintf("%.2f", r))),
    '<span class="mn-cl-hc na" title="Not estimable">&#8211;</span>')
  diag(G) <- paste0('<span class="mn-cl-hn">', seq_len(k), '</span>')
  rows <- paste0('<span class="mn-cl-hl', ifelse(is.na(step), "", " auto"), '" title="', lab, '">',
                 ifelse(is.na(step), "", .cl_badge(step)), '<span class="t">', lab, '</span></span>',
                 do.call(paste0, as.data.frame(G, stringsAsFactors = FALSE)))
  paste0('<details class="mn-cl-allpairs"><summary>All pairwise correlations (', .cl_num(k * (k - 1) / 2),
         ' pairs)</summary><div class="mn-cl-heatwrap"><div class="mn-cl-heat" style="--cl-k:', k, '">',
         paste(rows, collapse = ""), '</div></div><div class="mn-cl-note">Shading grows with |r|; outlined ',
         'cells have |r| above ', sprintf("%g", prof$pairwise_threshold %||% 0.95),
         if (length(prof$removals)) '; numbered names are the Auto-Drop removals', '.</div></details>')
}

#' The meta line of a card: locations (and complete rows when some are not),
#' candidates and `extra` (the surfaces, the model).
collinearity_meta <- function(screen, n_cand, noun = "covariate", extra = NULL) {
  d <- screen$diagnostics
  rows <- d$n_rows
  loc <- if (length(rows) == 1L && !is.na(rows)) paste0(.cl_count(rows, "location"),
    if (isTRUE(d$n_complete < rows)) paste0(", ", .cl_num(d$n_complete), " complete"))
  paste(c(loc, .cl_count(n_cand, noun), extra), collapse = " \u00b7 ")
}

#' One covariate check card. `screen` is a screen with diagnostics;
#' `removable` the candidate IDs its checkboxes offer (`input_id`), `manual`
#' those already removed by hand. `secondary`, `list(name, screen)`, adds a
#' read-only section for a second surface whose screen differs; `chips` adds
#' `list(title, vars)` rows; `caveat` a line under the chart; `hint` the
#' tick hint in the legend. `collapsed` shows a clean card as its header and
#' verdict, with the chart one click away.
collinearity_card <- function(id, title, meta, screen, labels, input_id, removable,
                              scope_note = NULL, manual = character(0), secondary = NULL,
                              collapsed = FALSE, chips = list(), caveat = NULL,
                              constant_title = "Constant here, removed whatever you answer:",
                              hint = FALSE) {
  screen <- screen %||% list(kept = character(0), diagnostics = list(status = "no_candidates"))
  L <- function(v) .cl_esc(display_var_labels(as.character(v), labels))
  prof <- collinearity_profile(screen)
  prof$pairwise_threshold <- screen$diagnostics$pairwise_threshold
  pw <- prof$pairwise_threshold %||% 0.95
  head <- paste0('<div class="mn-cl-head"><h4 class="mn-cl-title">', .cl_esc(title), '</h4>',
                 '<span class="mn-cl-meta">', .cl_esc(meta), '</span></div>')
  verdict <- .cl_verdict(screen, prof, L)
  note <- if (!is.null(scope_note)) paste0('<div class="mn-cl-scope">', .cl_esc(scope_note), '</div>') else ""
  chip_html <- paste0(.cl_chips("Removed by you:", manual, L, " gone"),
                      .cl_screen_chips(screen, L, constant_title),
                      paste(vapply(chips, function(ch) .cl_chips(ch$title, ch$vars, L), character(1)), collapse = ""))
  chart <- .cl_chart(screen, prof, L, removable, input_id, hint)
  extra <- if (!is.null(caveat)) paste0('<div class="mn-cl-caveat">', .cl_esc(caveat), '</div>') else ""
  sec <- if (!is.null(secondary)) {
    s <- secondary$screen
    sp <- collinearity_profile(s)
    sp$pairwise_threshold <- pw
    same <- setequal(s$dropped_vif, screen$dropped_vif) && setequal(s$dropped_constant, screen$dropped_constant)
    rows <- s$diagnostics$n_rows
    paste0('<details class="mn-cl-surface"><summary><b>', .cl_esc(secondary$name), ' surface</b>',
           if (length(rows) == 1L && !is.na(rows)) paste0(" &#183; ", .cl_count(rows, "location")), " &#183; ",
           if (.cl_incomplete(s$diagnostics$status %||% "not_checked")) "check incomplete" else
             if (same) "same removals, its own VIFs" else "different removals",
           '</summary>', .cl_verdict(s, sp, L), .cl_screen_chips(s, L, constant_title),
           .cl_chart(s, sp, L, removable), .cl_why(sp, L, pw), .cl_staircase(sp, s$diagnostics$pairs, L),
           '<div class="mn-cl-note">Your removals apply to both surfaces of this locality.</div></details>')
  } else ""
  stair <- .cl_staircase(prof, screen$diagnostics$pairs, L)
  html <- if (isTRUE(collapsed)) {
    paste0('<details class="mn-cl-card mn-cl-clean" id="', .cl_esc(id), '"><summary>', head, verdict, '</summary>',
           note, chip_html, chart, extra, stair, sec, '</details>')
  } else {
    paste0('<section class="mn-cl-card" id="', .cl_esc(id), '">', head, note, verdict, chip_html, chart, extra,
           .cl_why(prof, L, pw), stair, sec, '</section>')
  }
  HTML(html)
}

#' The interpolation dialog's overview of its localities: one chip per card,
#' in card order, that scrolls to (and opens) it. `items`: list(name, target
#' element ID, n_remove, incomplete).
collinearity_summary_strip <- function(items) {
  n_rm <- sum(vapply(items, function(x) x$n_remove > 0, logical(1)))
  n_inc <- sum(vapply(items, function(x) isTRUE(x$incomplete), logical(1)))
  chips <- vapply(items, function(x) {
    cnt <- c(if (x$n_remove > 0) paste(x$n_remove, "to remove"), if (isTRUE(x$incomplete)) "check incomplete")
    paste0('<button type="button" class="mn-cl-chip ',
           if (x$n_remove > 0) "act" else if (isTRUE(x$incomplete)) "warn" else "ok",
           '" onclick="var c=document.getElementById(\'', .cl_esc(x$target), '\');if(c){if(c.tagName===\'DETAILS\')',
           'c.open=true;c.scrollIntoView({behavior:\'smooth\',block:\'start\'});}"><b>', .cl_esc(x$name), '</b>',
           '<span class="cnt">', if (length(cnt)) paste(cnt, collapse = " &#183; ") else "&#10003; clean",
           '</span></button>')
  }, character(1))
  HTML(paste0('<div class="mn-cl-summary"><span class="mn-cl-sumtext">', length(items),
              ' localities checked separately &#183; ', n_rm, ' with Auto-Drop removals',
              if (n_inc) paste0(" &#183; ", n_inc, " incomplete"), '</span>', paste(chips, collapse = ""), '</div>'))
}

#' The Classification Suite's summary under its Covariates picker: the scope
#' `caption`, one line on the check, the Auto-Drop removals in step order, and
#' `review_button`. `model` names the method behind the threshold.
collinearity_inline_summary <- function(chk, labels = NULL, caption = NULL, review_button = NULL, model = NULL) {
  d <- chk$diagnostics %||% list(status = "not_checked")
  st <- d$status %||% "not_checked"
  if (identical(st, "checking")) return(tags$div(class = "mn-cl-inline",
    tags$div(class = "mn-cl-inline-line", tags$b("Covariate check:"), " checking the selected scope\u2026")))
  L <- function(v) .cl_esc(display_var_labels(as.character(v), labels))
  prof <- collinearity_profile(chk)
  thr <- .cl_thr(prof$threshold)
  n_dup <- NROW(chk$pairs)
  line <- if (.cl_incomplete(st)) {
    paste0("<b>Covariate check incomplete:</b> ", if (identical(st, "failed")) "the check failed" else
      "fewer than three locations carry every covariate", ".")
  } else if (length(prof$removals)) {
    paste0("<b>Covariate check:</b> Auto-Drop would remove ", length(prof$removals), " of ", nrow(prof$rows),
           " (VIF above ", thr, if (!is.null(model)) paste0(", ", .cl_esc(model)), ")")
  } else if (NROW(prof$rows)) {
    paste0("<b>Covariate check:</b> every VIF is ", thr, " or less",
           if (n_dup) paste0("; ", n_dup, " near-duplicate pair", if (n_dup > 1) "s", " (|r| above ",
                             sprintf("%g", d$pairwise_threshold %||% 0.95), ")"))
  } else "<b>Covariate check:</b> no VIF to compute"
  tags$div(class = "mn-cl-inline",
    if (!is.null(caption)) tags$div(class = "mn-cl-inline-scope", caption),
    HTML(paste0('<div class="mn-cl-inline-line">', line, '</div>',
                if (length(prof$removals)) paste0('<div class="mn-cl-inline-names">', .cl_pills(prof, L), '</div>'),
                .cl_chips("Constant or near-constant:", chk$dropped_constant, L))),
    review_button)
}
