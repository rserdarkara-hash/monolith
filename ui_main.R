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
    # DT tables in this app pre-render while their tab is hidden
    # (suspendWhenHidden = FALSE); with scrollX the cloned header is then
    # sized against a zero-width container, so realign columns on tab reveal.
    tags$script(HTML("$(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function () { setTimeout(function () { if ($.fn.dataTable) { $.fn.dataTable.tables({ visible: true, api: true }).columns.adjust(); } }, 60); });"))
  ),

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
                       "aria-label" = "Session information"),
          actionButton("about_btn", "", icon = icon("circle-question"), class = "mn-iconbtn",
                       "aria-label" = "About Monolith")
      )
  ),
  sidebarLayout(
    ui_sidebar_panel,
    ui_main_tabs
  )
)
