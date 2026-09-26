# ui_main.R - assembles the master UI from statically defined panel variables.
# Pure variable assignment pattern: ui_sidebar.R / ui_main_tabs.R define
# ui_sidebar_panel / ui_main_tabs (no function wrappers), preserving global UI
# scope exactly as in the original inline fluidPage() block.
source("ui_sidebar.R")
source("ui_main_tabs.R")

ui <- fluidPage(
  useShinyjs(),
  # Busy feedback while the server is synchronously re-encoding map layers or
  # recomputing class-area/kappa tables (styling APPLY, view switches): a
  # pulse banner during any server work plus per-output recalculation
  # spinners. The interpolation run keeps its own premium progress overlay.
  useBusyIndicators(),
  render_docs_drawer(),

  tags$head(
    # Variant first, stylesheet second: the boot script stamps data-theme on
    # <html> before anything paints, so a reader who chose dark never sees the
    # light variant flash. Both variants live in the one stylesheet below, so
    # switching needs no server round-trip.
    monolith_theme_boot_js(),
    tags$style(HTML(monolith_theme_css())),
    # Map ruler: sizes the measure control to match the drawing toolbar and
    # trims its expanded panel. Kept in ui_components.R so the shipped rules
    # are testable; see map_ruler_css() for why each selector is qualified.
    tags$style(HTML(map_ruler_css())),
    # Positioning only - the button's own box is styled with the other icon
    # buttons in monolith_theme_css().
    tags$style(HTML(
      ".expand-icon-btn { position: absolute; top: 10px; right: 10px; z-index: 100; }
       .expand-icon-btn > * { margin: 0 !important; padding: 0 !important; }"
    )),
    # Collapsible sidebar sections remember their open state per section key in
    # localStorage; the resize trigger makes Shiny re-render outputs that were
    # hidden inside a collapsed section when it opens.
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
    # The one-shot pass binds the icons present at startup. Icons that arrive
    # later (renderUI: the descriptive suite's test picker, the CRS target
    # note) were never reached by it and did nothing when clicked, so bind any
    # unbound icon the moment it is first focused and show it in the same step.
    tags$script(HTML(
      "$(function () { $('[data-toggle=\"popover\"]').popover({html: true, container: 'body'}); });
       $(document).on('focusin', '[data-toggle=\"popover\"]', function () {
         var $t = $(this);
         if (!$t.data('bs.popover')) { $t.popover({html: true, container: 'body'}); $t.popover('show'); }
       });"
    )),
    # ionRangeSlider centres the value bubble and the outermost grid labels on
    # the track ends, so each overhangs the slider by half its own width - with
    # the six-figure sills a variogram carries, that overhang left the sidebar
    # card entirely. force_edges clamps every label inside the track, and
    # sliderInput() exposes no passthrough for it, so it is switched on per
    # slider as it binds. The data attribute is written too, so any later
    # re-init (updateSliderInput) keeps it.
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
    # A scrollX DataTable is two tables - a cloned header and the body - kept
    # aligned by pixel widths DataTables computes from the rendered cells. Those
    # widths are wrong whenever they were computed against a container that was
    # not its final size, which in this app is the normal case: the tables
    # pre-render while their tab is hidden (suspendWhenHidden = FALSE) and
    # several sit inside conditionalPanels. Realign on every event that can
    # change a table's box: a tab reveal, Shiny making an output visible, and
    # the table's own redraw. columns.adjust() only - NEVER .draw() from a draw
    # handler - and once per animation frame per table, after the layout has
    # settled, rather than on a fixed timer that can fire too early.
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
    # Four-significant-digit display formatting for numeric table cells, so a
    # small value never reads as 0. Kept in ui_components.R beside its R twin
    # format_sig(), so the shipped script is testable.
    tags$script(HTML(format_sig_js())),
    # Copy-a-result-table-to-the-clipboard: reads the rendered table off the
    # DOM and puts HTML + tab-separated text on the clipboard together, so one
    # click pastes into Word as a table and into Excel as cells. Kept in
    # ui_components.R so the shipped script is testable.
    tags$script(HTML(copy_table_js()))
  ),

  # Announces the result of a clipboard copy; visually hidden.
  div(id = "mn_copy_live", class = "mn-copy-live", role = "status", "aria-live" = "polite"),

  # The wordmark is live text rather than the banner raster: it inverts with
  # the dark variant, stays crisp at any pixel density, and is read out as the
  # application name instead of as an image. assets/banner.png is still the
  # README's masthead.
  div(class = "header-panel",
      div(class = "mn-wordmark",
          span(class = "name", "Monolith"),
          span(class = "rule"),
          span(class = "sub", "Spatial Analysis Dashboard")
      ),
      uiOutput("dataset_context", inline = TRUE),
      div(class = "header-controls",
          # role/aria-live: the chip is the only place a running job announces
          # itself outside the run tab, so screen readers should hear it change
          # ("Running · 40%" -> "Run ready · OK") without moving focus.
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
