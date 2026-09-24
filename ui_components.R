# ui_components.R - shiny tag / widget generators (cards, tooltips, docs
# drawer, modals, DT wrappers). register_expanded_modal registers observers on
# the session passed to it; nothing here creates top-level reactives.
# Sourced via ui_helpers.R.


#' Sidebar panel of an engine parameter (IDW power, TPS smoothing) with a
#' scope switch: one setting for all localities (`global_ui`), or a value per
#' locality and target (`manual_ui` plus an Apply button). The switch keeps
#' the input id `<id>_mode` with values "auto" / "manual", which the server
#' reads. `label` names the parameter in the switch's title. Under Per locality
#' the `<id>_m_note` output says which localities have a value of their own and
#' what the others run with: the All localities setting, whose control is
#' hidden there.
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
# output. Server side pairs with register_sci_plot() in server_map_viewer.R, which
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

# ── Copy-a-table-to-the-clipboard ────────────────────────────────────────────
# The counterpart to the PNG button on every figure card: a result table is
# read off the DOM and put on the clipboard in two flavours at once - an HTML
# table (Word, Google Docs, and the flavour Excel prefers, so it lands in
# cells) and tab-separated plain text (anything else, and Excel's own fallback
# parser). Reading the rendered table rather than round-tripping to the server
# means the clipboard carries exactly what the reader is looking at, including
# the displayed rounding, and costs no reactive traffic.
#
# NO colour literals in the emitted markup: the payload is a document bound for
# another application, not app chrome, and the receiving program supplies its
# own palette. border="1" plus <th> is all the formatting Word and Excel need.
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

# Icon-only copy control. Deliberately NOT an action-button: nothing about a
# clipboard copy needs the server, so it stays a plain <button> and sends no
# input. `target_id` is the DOM id of the element that CONTAINS the table -
# for a DT output that is the output id itself, already namespaced inside a
# module.
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
# (matched by column name; columns without an entry render normally).
sci_metric_tooltips <- function() {
  c(
    "Source" = "Model and cross-validation design that produced this row's metrics.",
    "RMSE" = "Root Mean Square Error of the cross-validation residuals, in the variable's units. Lower is better.",
    "NRMSE (mean, %)" = "RMSE as a percentage of the absolute observed mean, i.e. CV(RMSE). Needs a ratio scale: n/a¹ when the observed values span zero, NA when the observed mean is exactly zero.",
    "NRMSE (SD)" = "RMSE divided by the standard deviation of the observed values: the prediction error as a multiple of the target's spread. Unchanged by shifting the variable's origin, so it stays defined for signed or centred targets. It equals 1/RPD and sqrt((1 - NSE)(n - 1)/n), so it restates, rather than adds to, the evidence in those columns.",
    "MAE" = "Mean Absolute Error of the cross-validation residuals, in the variable's units. Less sensitive to single large errors than RMSE.",
    "R² (Corr)" = "Squared Pearson correlation between observed and CV-predicted values. Measures association only; insensitive to systematic bias.",
    "R² (NSE/Trad)" = "Nash-Sutcliffe efficiency (traditional R²): 1 - SSE/SStot against the observed mean. 1 = perfect, 0 = no better than predicting the mean, negative = worse than the mean.",
    "Bias (ME)" = "Mean Error, mean(observed - predicted): positive = model underpredicts on average, negative = overpredicts.",
    "Lin's CCC (Agree)" = "Lin's Concordance Correlation Coefficient: agreement with the 1:1 line, combining precision (correlation) and accuracy (bias/scale shift). 1 = perfect agreement. NA when either vector is constant.",
    "RPD (Prec)" = "Ratio of Performance to Deviation: SD(observed) / RMSE. Chemometrics convention: > 2 good, 1.4-2 fair, < 1.4 poor.",
    "RPIQ" = "Ratio of Performance to Interquartile distance: IQR(observed) / RMSE. The RPD analogue for skewed distributions, where the SD is a poor spread measure. Higher is better.",
    "SMAPE (%)" = "Symmetric Mean Absolute Percentage Error: scale-free accuracy; 0% is perfect. n/a¹ when the observed values span zero, where a single sign disagreement contributes the maximum term however small both values are.",
    "Moran's I" = "Spatial autocorrelation of the CV residuals (symmetric 8-nearest-neighbour weights). Read it against its null expectation E[I] = -1/(n-1) (shown per row on hover), not against 0. A clearly higher value is consistent with spatial structure the prediction procedure did not capture; it is not by itself an instruction to change engine. A value near E[I] is not proof of a clean model either: random folds leave each held-out point's neighbours in training, which can mask residual structure. Rows scored under Spatial Block CV or kNNDM spatial folds are marked † and read differently (see the note under the table). NA* = the statistic could not be computed for this point set.",
    "Block-CV residual clustering" = "Moran's I of the pooled out-of-fold residuals under Spatial Block CV. Those residuals inherit the fold geometry and a shared extrapolation condition within each withheld block, so this statistic measures clustering of block-CV errors; fold geometry and shared extrapolation error confound it, and it cannot on its own diagnose a missing spatial trend. Its p-value is not reported.",
    "Spatial-fold residual clustering" = "Moran's I of the pooled out-of-fold residuals under kNNDM spatial folds. Each fold withholds whole groups of neighbouring samples, so those residuals inherit the fold geometry and a shared prediction condition within each group: this statistic measures clustering of the spatial-fold errors, fold geometry confounds it, and it cannot on its own diagnose a missing spatial trend. Its p-value is not reported.",
    "Moran p" = "Two-sided permutation p-value of Moran's I (999 seeded permutations; the smallest attainable value is 0.002). Small p = residual autocorrelation unlikely under spatial randomness. NA† under Spatial Block CV and kNNDM spatial folds, where residuals held out in whole groups are not exchangeable, so the permutation null does not describe them. NA* where Moran's I itself could not be computed."
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
    # puts it by default, the panel would inherit its parent's clipping, and
    # every tooltip in the sidebar (a scroll container) and in the map/plot
    # cards would be cut off at the container edge.
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

# What the Total (Combined) view of a variogram card draws, and why its lag
# axis ends where it does (pooled_within_variogram, spatial_vgm.R). Built at
# call time: spatial_vgm.R, which defines POOLED_VGM_MIN_N, is sourced after
# the UI helpers.
pooled_vgm_note <- function() {
  paste0("Total (Combined) pools the point pairs inside each locality; no pair joins two localities. ",
         "The lag axis stops at the half-diagonal of the smallest locality with at least ", POOLED_VGM_MIN_N,
         " located values, so every locality contributes over the whole axis. One small locality therefore ",
         "shortens the curve for all; select a locality to see its own lag range.")
}

# The info icon of a variogram card that also draws the Total (Combined) view.
pooled_vgm_info <- function(id) info_tooltip(id, pooled_vgm_note())


# One-line statement of the sample a matrix-valued panel was estimated on.
# Correlation matrices, partial correlations, PCA and the collinearity screen
# all use the rows complete across EVERY selected variable, so their n is not
# the row count of the active selection and has to be said out loud.
complete_case_note <- function(n_used, n_total) {
  dropped <- max(0, n_total - n_used)
  sprintf("Complete cases: n = %d of %d rows%s.", n_used, n_total,
          if (dropped > 0) sprintf(" (%d dropped for missing values)", dropped) else "")
}

# Browser twin of format_sig() (ui_formatting.R): four significant digits,
# "0" for zero, a whole number exactly, every integer digit from 1000 up,
# scientific notation below 1e-4. Numeric
# table columns keep their numbers (DataTables still sorts on them) and only
# their DISPLAY is formatted, through sig_render_defs(). Mounted once in
# ui_main.R's head. Non-numeric cells pass through unchanged.
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

# The three markers a result table can print in place of a number, and the
# sentence each one needs. A cell can carry the short form; the explanation
# belongs under the table, and every table that can show a marker must show
# the SAME sentence for it - which is why the texts live here and not at the
# render sites. table_footnote() emits only the ones a table actually used.
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
  na    = paste("NA* Not computable for this point set (see the Run Log); it does not mean",
                "the quantity is zero or that no structure was found.")
)

table_footnote <- function(markers) {
  notes <- unname(METRIC_MARKER_NOTES[intersect(names(METRIC_MARKER_NOTES), markers)])
  if (!length(notes)) return(NULL)
  htmltools::tags$div(class = "mn-table-note", lapply(notes, htmltools::tags$div))
}

# DataTables columnDefs entry that displays `cols` of `df` through
# mnFormatSig() while sorting and copying still see the numbers. `rownames`
# shifts the column index when the table shows row names.
sig_render_defs <- function(df, cols, rownames = FALSE) {
  idx <- match(cols, names(df))
  idx <- idx[!is.na(idx)]
  if (!length(idx)) return(list())
  list(list(targets = idx - 1L + as.integer(isTRUE(rownames)),
            render = DT::JS(
              "function (data, type) {",
              "  return type === 'display' && typeof window.mnFormatSig === 'function'",
              "    ? window.mnFormatSig(data) : data;",
              "}")))
}

# Shared DT wrapper for the compact summary tables on the Scientific Analysis
# tab, matching the Classification Suite look (dom = 't'). Paging is disabled
# because dom = 't' hides the paging controls: with the default pageLength,
# rows beyond the first page would be silently unreachable in variable-length
# tables (e.g. per-locality variogram parameters). `scroll_x = FALSE` for a
# table that fits the narrowest supported viewport: its header and body stay
# one table, so they cannot drift apart. `signif_cols` names the numeric
# columns displayed at four significant digits.
sci_dt <- function(df, escape = TRUE, header_tooltips = NULL, scroll_x = TRUE,
                   signif_cols = NULL) {
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
  # A one-cell Status/Error frame is a message, not a result: the class tells
  # the copy button to report it instead of copying a 1 x 1 "table".
  status_like <- ncol(df) == 1 && nrow(df) == 1 && names(df)[1] %in% c("Status", "Error")
  tbl_class <- if (status_like) "display mn-status-table" else "display"
  opts <- list(dom = 't', paging = FALSE, scrollX = isTRUE(scroll_x))
  if (!is.null(signif_cols)) opts$columnDefs <- sig_render_defs(df, signif_cols)
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
