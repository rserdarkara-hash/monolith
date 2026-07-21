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
  
  fresh::use_theme(app_themes[["Muted Sage (modified)"]]$theme),
  tags$head(
    tags$style(HTML(app_themes[["Muted Sage (modified)"]]$manual_style))
  ),
  
  uiOutput("dynamic_theme"),
  tags$head(
    tags$style(HTML("
      .bootstrap-select .dropdown-menu li a span.text { display: flex !important; width: 100% !important; align-items: center; justify-content: space-between; }
      .shiny-notification { width: 100% !important; }
      .well { padding: 15px; }
      .header-panel { background-color: #2c3e50; color: white; padding: 10px 20px; margin-bottom: 20px; border-radius: 0 0 10px 10px; display: flex; justify-content: space-between; align-items: center; }
      .header-title { margin: 0; font-weight: bold; font-size: 24px; }
      .header-controls { display: flex; align-items: center; gap: 20px; }
      .table-container { width: 100%; overflow-x: auto; font-size: 0.95em; margin-bottom: 10px; }
      .table-container table { width: 100% !important; margin-bottom: 0; background-color: #ffffff !important; color: #000000 !important; }
      .table-container th { background-color: #f8f9fa !important; color: #000000 !important; }
      .table-container .dataTables_wrapper { background-color: #ffffff !important; border-radius: 4px; padding: 2px; }
      .table-container table.dataTable td, .table-container table.dataTable th { color: #000000 !important; }
      .popover { color: #333 !important; background-color: #fff !important; max-width: 400px; }
      .popover-header { color: #333 !important; background-color: #f8f9fa !important; border-bottom: 1px solid #ebebeb; }
      .popover-body { color: #333 !important; }
      .expand-icon-btn { position: absolute; top: 10px; right: 10px; z-index: 100; opacity: 0.8; width: 32px; height: 32px; padding: 0 !important; display: inline-flex !important; align-items: center !important; justify-content: center !important; }
      .expand-icon-btn > * { margin: 0 !important; padding: 0 !important; }
      .map-toolbar-export-container .form-group { margin-bottom: 0 !important; }
      /* Scientific Analysis plot cards: title row + per-plot tool buttons */
      .sci-plot-card { margin-bottom: 8px; }
      .sci-plot-card-head { display: flex; justify-content: space-between; align-items: center; margin: 12px 0 4px 0; }
      .sci-plot-card-tools { display: flex; gap: 6px; }
      .sci-plot-card-tools .btn { border: 1px solid #dee2e6; background: #f8f9fa; color: #495057; }
      .sci-plot-card-tools .btn:hover { background: #e9ecef; }
      /* Unified results card (generalizes the RK fit-chip styling) */
      .sci-card { background-color: #ffffff; border: 1px solid #dee2e6; border-radius: 8px; padding: 15px; margin-bottom: 20px; box-shadow: 0 1px 3px rgba(0,0,0,0.05); color: #212529; }
      .sci-card h4, .sci-card h5, .sci-card h6 { color: #212529; }
      .sci-card-sub { font-size: 0.85em; opacity: 0.75; font-style: italic; margin: 0 0 10px 0; }
      /* Sticky run area at the bottom of the sidebar */
      .sidebar-run-sticky { position: sticky; bottom: 0; z-index: 50; background: inherit; padding-top: 8px; margin: 0 -4px -4px -4px; padding-left: 4px; padding-right: 4px; padding-bottom: 4px; box-shadow: 0 -6px 12px -8px rgba(0,0,0,0.35); }
      /* Compact run-status chip in the header */
      .run-status-chip { display: inline-flex; align-items: center; gap: 7px; background: rgba(255,255,255,0.12); color: #fff; border-radius: 14px; padding: 4px 12px; font-size: 12px; line-height: 1; white-space: nowrap; }
      .run-status-chip .dot { width: 8px; height: 8px; border-radius: 50%; display: inline-block; }
      .run-status-chip .dot.idle { background: #adb5bd; }
      .run-status-chip .dot.running { background: #fab005; animation: chip-pulse 1.2s ease-in-out infinite; }
      .run-status-chip .dot.done { background: #40c057; }
      @keyframes chip-pulse { 0%, 100% { opacity: 1; } 50% { opacity: 0.35; } }
      /* Collapsible sidebar sections (details/summary with state memory) */
      details.sidebar-section > summary { cursor: pointer; list-style: none; display: flex; align-items: center; justify-content: space-between; user-select: none; }
      details.sidebar-section > summary::-webkit-details-marker { display: none; }
      details.sidebar-section > summary h4 { margin: 0; }
      details.sidebar-section > summary::after { content: '\\25B8'; font-size: 14px; opacity: 0.6; transition: transform 0.15s ease; }
      details.sidebar-section[open] > summary::after { transform: rotate(90deg); }
      details.sidebar-section > summary + * { margin-top: 10px; }
    ")),
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
    uiOutput("dynamic_manual_style"),
    tags$script(HTML("$(function () { $('[data-toggle=\"popover\"]').popover({html: true}); });")),
    # DT tables in this app pre-render while their tab is hidden
    # (suspendWhenHidden = FALSE); with scrollX the cloned header is then
    # sized against a zero-width container, so realign columns on tab reveal.
    tags$script(HTML("$(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function () { setTimeout(function () { if ($.fn.dataTable) { $.fn.dataTable.tables({ visible: true, api: true }).columns.adjust(); } }, 60); });"))
  ),
  
  div(class = "header-panel", style = "display: flex; justify-content: space-between; align-items: center; padding: 5px 20px;",
      img(src = "assets/banner.png", class = "header-banner", style = "max-height: 50px; width: auto; object-fit: contain; float: left;"),
      div(style = "flex-grow: 1;"),
      div(class = "header-controls", style = "display: flex; align-items: center; gap: 10px; margin-left: auto;",
          tags$style(HTML("
            .header-controls .shiny-input-container { width: auto !important; margin: 0 !important; }
            .header-controls .form-group { margin-bottom: 0 !important; margin-right: 0 !important; }
            .header-controls .checkbox { margin: 0 !important; padding: 0 !important; display: flex !important; align-items: center !important; }
            .header-controls .checkbox label { margin: 0 !important; padding-left: 0 !important; color: white !important; font-size: 11px !important; display: flex !important; align-items: center !important; gap: 5px !important; line-height: 1 !important; }
            .header-controls .checkbox input[type=\"checkbox\"] { position: static !important; margin: 0 !important; }
            
            .header-controls .btn-header-circle,
            .header-controls .dropdown-toggle {
              background: #ffffff !important;
              color: #2c3e50 !important;
              border: none !important;
              width: 32px !important;
              height: 32px !important;
              border-radius: 50% !important;
              padding: 0 !important;
              display: inline-flex !important;
              align-items: center !important;
              justify-content: center !important;
              font-size: 0 !important;
              cursor: pointer !important;
              box-shadow: 0 2px 4px rgba(0,0,0,0.1) !important;
              transition: all 0.2s ease !important;
              margin: 0 !important;
            }
            .header-controls .btn-header-circle:hover,
            .header-controls .dropdown-toggle:hover {
              background: #f1f3f5 !important;
              transform: scale(1.08) !important;
            }
            .header-controls .dropdown {
              margin: 0 !important;
              padding: 0 !important;
              display: inline-flex !important;
              align-items: center !important;
              justify-content: center !important;
              width: 32px !important;
              height: 32px !important;
            }
            .header-controls .dropdown-toggle::after,
            .header-controls .dropdown-toggle .caret {
              display: none !important;
            }
            .header-controls .btn-header-circle i,
            .header-controls .dropdown-toggle i {
              font-size: 15px !important;
              line-height: 1 !important;
              width: 1em !important;
              text-align: center !important;
              margin: 0 !important;
              padding: 0 !important;
              display: inline-block !important;
            }
          ")),
          uiOutput("run_status_chip", inline = TRUE),
          theme_switcher_ui("theme_mod"),
          actionButton("info_btn", "", icon = icon("info"), class = "btn-header-circle"),
          actionButton("about_btn", "", icon = icon("question"), class = "btn-header-circle")
      )
  ),
  sidebarLayout(
    ui_sidebar_panel,
    ui_main_tabs
  )
)
