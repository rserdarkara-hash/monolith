# ui_main.R - assembles `ui` from ui_sidebar_panel (ui_sidebar.R) and
# ui_main_tabs (ui_main_tabs.R), plain variables with no function wrappers.
source("ui_sidebar.R")
source("ui_main_tabs.R")

ui <- fluidPage(
  useShinyjs(),
  # Busy feedback for synchronous server work (styling APPLY, view switches);
  # the interpolation run has its own progress overlay.
  useBusyIndicators(),
  render_docs_drawer(),

  tags$head(
    # The boot script stamps data-theme on <html> before first paint, so a dark
    # reader never sees a light flash.
    monolith_theme_boot_js(),
    tags$style(HTML(monolith_theme_css())),
    # Map ruler sizing (map_ruler_css(), ui_components.R).
    tags$style(HTML(map_ruler_css())),
    # Positioning only - the button's own box is styled with the other icon
    # buttons in monolith_theme_css().
    tags$style(HTML(
      ".expand-icon-btn { position: absolute; top: 10px; right: 10px; z-index: 100; }
       .expand-icon-btn > * { margin: 0 !important; padding: 0 !important; }"
    )),
    # Sidebar sections remember their open state in localStorage; the resize
    # trigger re-renders outputs hidden inside a section when it opens.
    tags$script(HTML("
      $(function () {
        $('details.sidebar-section').each(function () {
          var key = 'monolith_sidebar_' + $(this).data('key');
          var saved = window.localStorage ? localStorage.getItem(key) : null;
          if (saved === 'closed') { $(this).removeAttr('open'); }
          if (saved === 'open') { $(this).attr('open', ''); }
        });
        // the details 'toggle' event does not bubble, so bind directly to
        // each (static) sidebar section rather than delegating from document
        $('details.sidebar-section').on('toggle', function () {
          var key = 'monolith_sidebar_' + $(this).data('key');
          if (window.localStorage) { localStorage.setItem(key, this.open ? 'open' : 'closed'); }
          $(window).trigger('resize');
        });
      });
    ")),
    # Bind popovers at startup and, for icons rendered later (renderUI), on
    # first focus.
    tags$script(HTML(
      "$(function () { $('[data-toggle=\"popover\"]').popover({html: true, container: 'body'}); });
       $(document).on('focusin', '[data-toggle=\"popover\"]', function () {
         var $t = $(this);
         if (!$t.data('bs.popover')) { $t.popover({html: true, container: 'body'}); $t.popover('show'); }
       });"
    )),
    # ionRangeSlider centres edge labels on the track ends, so large values
    # overhang the sidebar; force_edges (no sliderInput() passthrough) is set
    # per slider as it binds, and written to the data attribute so a re-init
    # keeps it.
    tags$script(HTML(
      "$(document).on('shiny:bound', function (e) {
         if (e.bindingType !== 'input') return;
         var $i = $(e.target);
         if (!$i.hasClass('js-range-slider')) return;
         var s = $i.data('ionRangeSlider');
         if (!s || s.options.force_edges) return;
         $i.attr('data-force-edges', 'true');
         s.options.force_edges = true;
         s.update({});
       });"
    )),
    # <details> has no native close-on-outside-click; the toolbar popovers need
    # one or they stay open behind whatever the next click was aimed at.
    tags$script(HTML(
      "$(document).on('click', function (e) {
         $('details.mn-popover[open]').each(function () {
           if (!this.contains(e.target)) { this.removeAttribute('open'); }
         });
       });"
    )),
    # A scrollX DataTable keeps its cloned header aligned by widths computed
    # from the rendered cells, which are wrong when computed while hidden (tabs
    # pre-render, conditionalPanels). Realign on tab reveal, output visibility
    # and the table's own redraw: columns.adjust() only (never .draw() from a
    # draw handler), once per animation frame per table.
    tags$script(HTML("
      (function () {
        function adjust(tbl) {
          if (!tbl || !$.fn.dataTable || !$.fn.dataTable.isDataTable(tbl)) return;
          if (tbl._mnAdjustPending) return;
          tbl._mnAdjustPending = true;
          window.requestAnimationFrame(function () {
            tbl._mnAdjustPending = false;
            if (!$(tbl).is(':visible')) return;
            try { $(tbl).DataTable().columns.adjust(); } catch (e) {}
          });
        }
        function adjustVisible() {
          if (!$.fn.dataTable) return;
          window.requestAnimationFrame(function () {
            try { $.fn.dataTable.tables({ visible: true, api: true }).columns.adjust(); } catch (e) {}
          });
        }
        $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', adjustVisible);
        // shiny:visualchange fires on the OUTPUT element, not on the DataTables
        // wrapper, so resolve the table from the element inside the handler.
        $(document).on('shiny:visualchange', '.shiny-datatable-output', function () {
          $(this).find('table.dataTable').each(function () { adjust(this); });
        });
        $(document).on('draw.dt', function (e, settings) {
          adjust(settings ? settings.nTable : null);
        });
      })();
    ")),
    # Four-significant-digit display of numeric table cells (format_sig_js()).
    tags$script(HTML(format_sig_js())),
    # Copy-a-table-to-the-clipboard (copy_table_js()).
    tags$script(HTML(copy_table_js()))
  ),

  # Announces the result of a clipboard copy; visually hidden.
  div(id = "mn_copy_live", class = "mn-copy-live", role = "status", "aria-live" = "polite"),

  # The wordmark is live text (inverts with the dark variant, stays crisp, read
  # as the app name); assets/banner.png remains the README masthead.
  div(class = "header-panel",
      div(class = "mn-wordmark",
          span(class = "name", "Monolith"),
          span(class = "rule"),
          span(class = "sub", "Spatial Analysis Dashboard")
      ),
      uiOutput("dataset_context", inline = TRUE),
      div(class = "header-controls",
          # The chip announces a running job outside the run tab without moving
          # focus.
          uiOutput("run_status_chip", inline = TRUE,
                   role = "status", "aria-live" = "polite"),
          theme_switcher_ui("theme_mod"),
          # Circled glyphs, not bare letters: beside the sun/moon toggle a lone
          # "i" and "?" read as stray characters rather than as controls.
          actionButton("info_btn", "", icon = icon("circle-info"), class = "mn-iconbtn",
                       "aria-label" = "Documentation"),
          actionButton("about_btn", "", icon = icon("circle-question"), class = "mn-iconbtn",
                       "aria-label" = "About Monolith")
      )
  ),
  sidebarLayout(
    ui_sidebar_panel,
    ui_main_tabs
  )
)
