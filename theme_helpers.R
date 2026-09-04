# theme_helpers.R — the single Monolith theme.
#
# One theme, two variants (light and dark). Every colour in the interface
# resolves to one of the fourteen roles in monolith_tokens(); the variants
# differ only in the values bound to those roles, never in the rules that use
# them. That is why the per-theme contrast workarounds this file used to carry
# (a body colour forced light because one theme's content background was dark,
# and so on) are gone: there is one set of rules to keep readable, not ten.
#
# The variant is a `data-theme` attribute on <html>, flipped client-side and
# remembered in localStorage. Nothing round-trips through the server, so
# switching is instant and stays responsive while a run occupies the session.

#' Design tokens for both variants.
#'
#' Named vectors of CSS custom properties. Both variants define exactly the
#' same names — a rule may reference any token without checking which variant
#' is active. Contrast: every text role clears 4.5:1 against the surface it is
#' used on, and `on-accent` clears 4.5:1 against `accent`, in both variants.
monolith_tokens <- function() {
  list(
    light = c(
      "bg"           = "#EFF1F4",
      "surface"      = "#FFFFFF",
      "surface-2"    = "#F5F7F9",
      "surface-3"    = "#EAEEF2",
      "line"         = "#D2D8E1",
      "line-2"       = "#B4BCC8",
      "text"         = "#14171C",
      "text-2"       = "#59616D",
      "text-3"       = "#666E79",
      "accent"       = "#0F6E8C",
      "accent-hover" = "#0B5D78",
      "accent-weak"  = "#E7F1F5",
      "on-accent"    = "#FFFFFF",
      "ok"           = "#1E7A4E",
      "warn"         = "#A96A00",
      "danger"       = "#B32B2B",
      "shadow"       = "0 1px 2px rgba(16,20,28,.06), 0 8px 24px -14px rgba(16,20,28,.35)",
      "scheme"       = "light"
    ),
    dark = c(
      "bg"           = "#0D0F13",
      "surface"      = "#15181E",
      "surface-2"    = "#1C2028",
      "surface-3"    = "#242933",
      "line"         = "#343B47",
      "line-2"       = "#4B5462",
      "text"         = "#E6E9EE",
      "text-2"       = "#9BA3B0",
      "text-3"       = "#838C9A",
      "accent"       = "#3FB2CE",
      "accent-hover" = "#5CC4DE",
      "accent-weak"  = "#10262E",
      "on-accent"    = "#06181F",
      "ok"           = "#46C186",
      "warn"         = "#D99A3F",
      "danger"       = "#EF6B62",
      "shadow"       = "0 1px 2px rgba(0,0,0,.55), 0 10px 28px -16px rgba(0,0,0,.9)",
      "scheme"       = "dark"
    )
  )
}

# Renders one token vector as the body of a CSS rule.
.mn_token_block <- function(tokens) {
  paste0("  --mn-", names(tokens), ": ", unname(tokens), ";", collapse = "\n")
}

#' The complete Monolith stylesheet.
#'
#' Emitted once into the document head. Selectors are qualified against the
#' markup Shiny and shinyWidgets actually produce (Bootstrap 3, selectize,
#' bootstrap-select, ionRangeSlider, DT); the `!important` declarations are
#' confined to the places where those libraries ship their own inline or
#' high-specificity colours.
monolith_theme_css <- function() {
  tk <- monolith_tokens()

  paste0(
"@import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500&family=IBM+Plex+Sans:wght@400;500;600&display=swap');

/* ==== tokens ============================================================ */
:root {
", .mn_token_block(tk$light), "
  --mn-sans: 'IBM Plex Sans', 'Segoe UI', system-ui, -apple-system, sans-serif;
  --mn-mono: 'IBM Plex Mono', 'SFMono-Regular', Consolas, monospace;
  --mn-radius: 6px;
  --mn-radius-lg: 8px;
  --mn-control-h: 32px;
  /* One disclosure mark for every closed control -- native <select>, selectize
     and bootstrap-select all draw this instead of the three different
     triangles their own stylesheets ship. */
  --mn-chevron: url('data:image/svg+xml,%3Csvg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 16 16%22%3E%3Cpath d=%22M4 6.2 8 10.2 12 6.2%22 fill=%22none%22 stroke=%22%2359616D%22 stroke-width=%221.6%22 stroke-linecap=%22round%22 stroke-linejoin=%22round%22/%3E%3C/svg%3E');
  color-scheme: var(--mn-scheme);
}
:root[data-theme='dark'] {
", .mn_token_block(tk$dark), "
  --mn-chevron: url('data:image/svg+xml,%3Csvg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 16 16%22%3E%3Cpath d=%22M4 6.2 8 10.2 12 6.2%22 fill=%22none%22 stroke=%22%239BA3B0%22 stroke-width=%221.6%22 stroke-linecap=%22round%22 stroke-linejoin=%22round%22/%3E%3C/svg%3E');
  color-scheme: var(--mn-scheme);
}

/* ==== base ============================================================== */
body {
  background-color: var(--mn-bg);
  color: var(--mn-text);
  font-family: var(--mn-sans);
  font-size: 13px;
  line-height: 1.55;
  -webkit-font-smoothing: antialiased;
}
body, h1, h2, h3, h4, h5, h6, .well, select, input, button, textarea, table, .nav-tabs, .btn {
  font-family: var(--mn-sans);
}
h1, h2, h3, h4, h5, h6 { color: var(--mn-text); font-weight: 600; letter-spacing: -.005em; }
h1 { font-size: 22px; } h2 { font-size: 18px; } h3 { font-size: 16px; }
h4 { font-size: 14px; } h5 { font-size: 12.5px; } h6 { font-size: 12px; }
a { color: var(--mn-accent); text-decoration: none; }
a:hover, a:focus { color: var(--mn-accent-hover); text-decoration: underline; }
hr { border-top: 1px solid var(--mn-line); margin: 14px 0; }
code, pre, .mn-mono { font-family: var(--mn-mono); }
::selection { background: var(--mn-accent-weak); color: var(--mn-text); }
:focus-visible { outline: 2px solid var(--mn-accent); outline-offset: 2px; }

/* ==== header ============================================================ */
.header-panel {
  background-color: var(--mn-surface) !important;
  color: var(--mn-text) !important;
  border-bottom: 1px solid var(--mn-line) !important;
  border-radius: 0 !important;
  margin-bottom: 14px;
  padding: 0 16px !important;
  min-height: 52px;
  display: flex; align-items: center; gap: 16px;
}
.mn-wordmark {
  display: flex; align-items: center; gap: 13px;
}
.mn-wordmark .name {
  font-size: 15px; font-weight: 600; letter-spacing: .19em; text-transform: uppercase;
  color: var(--mn-text); white-space: nowrap;
}
.mn-wordmark .rule { width: 1px; height: 18px; background: var(--mn-line-2); }
.mn-wordmark .sub {
  font-size: 11px; letter-spacing: .09em; text-transform: uppercase;
  color: var(--mn-text-3); white-space: nowrap;
}
.header-controls { display: flex; align-items: center; gap: 8px; margin-left: auto; }
.mn-ctx { display: flex; align-items: center; gap: 14px; flex-wrap: wrap; }
.mn-ctx::before {
  content: ''; width: 1px; height: 18px; align-self: center;
  background: var(--mn-line-2);
}
.mn-ctx-item {
  display: inline-flex; align-items: baseline; gap: 6px;
  font-size: 11.5px; color: var(--mn-text-3); white-space: nowrap;
}
.mn-ctx-item b {
  color: var(--mn-text-2); font-weight: 400;
  font-family: var(--mn-mono); font-size: 11.5px;
}
.header-controls .shiny-input-container { width: auto !important; margin: 0 !important; }
.header-controls .form-group { margin-bottom: 0 !important; }

/* header icon buttons: quiet squares, not white circles on a colour field */
.btn.mn-iconbtn {
  background: transparent !important;
  color: var(--mn-text-2) !important;
  border: 1px solid transparent !important;
  width: 30px !important; height: 30px !important;
  border-radius: var(--mn-radius) !important;
  padding: 0 !important;
  display: inline-flex !important; align-items: center !important; justify-content: center !important;
  box-shadow: none !important;
  transition: background-color .15s ease, color .15s ease;
}
.btn.mn-iconbtn:hover,
.btn.mn-iconbtn:focus,
.btn.mn-iconbtn:active {
  background: var(--mn-surface-3) !important;
  color: var(--mn-text) !important;
  border-color: transparent !important;
  transform: none !important;
}
.btn.mn-iconbtn i { font-size: 14px !important; line-height: 1 !important; }
/* actionButton() emits an .action-label span even for an empty label, and .btn
   sets a 7px gap between its flex children -- so an icon-only button was laid
   out as [icon][gap][empty span] and its glyph sat left of centre. The theme
   toggle, which carries no label span, was the one that looked right. */
.btn.mn-iconbtn { gap: 0 !important; line-height: 1 !important; }
.btn.mn-iconbtn > .action-label { display: none !important; }
/* The icon arrives wrapped in a span, whose inline line box is taller than the
   glyph and left it riding a pixel high. Make the wrapper size to the glyph. */
.btn.mn-iconbtn > .action-icon {
  display: flex !important; align-items: center; justify-content: center;
  line-height: 1 !important;
}
.mn-theme-toggle .mn-moon { display: none; }
:root[data-theme='dark'] .mn-theme-toggle .mn-moon { display: inline-block; }
:root[data-theme='dark'] .mn-theme-toggle .mn-sun { display: none; }

/* run status chip */
.run-status-chip {
  display: inline-flex; align-items: center; gap: 7px;
  background: var(--mn-surface-2); color: var(--mn-text-2);
  border: 1px solid var(--mn-line); border-radius: 999px;
  padding: 4px 11px; font-size: 11.5px; line-height: 1; white-space: nowrap;
}
.run-status-chip .dot { width: 7px; height: 7px; border-radius: 50%; display: inline-block; }
.run-status-chip .dot.idle { background: var(--mn-text-3); }
.run-status-chip .dot.running { background: var(--mn-accent); animation: chip-pulse 1.2s ease-in-out infinite; }
.run-status-chip .dot.done { background: var(--mn-ok); }
@keyframes chip-pulse { 0%, 100% { opacity: 1; } 50% { opacity: .3; } }

/* ==== sidebar =========================================================== */
.well {
  background-color: var(--mn-surface) !important;
  color: var(--mn-text) !important;
  border: 1px solid var(--mn-line) !important;
  border-radius: var(--mn-radius-lg) !important;
  box-shadow: none !important;
  padding: 14px !important;
}
/* Only the sidebar. sidebarPanel() is the one .well that is its own scroll
   container; the rule used to apply to every wellPanel in the app, which
   turned the analytics grouping panel into a clipping box that cut off the
   open select menus inside it. */
.well[role='complementary'] {
  /* No bottom padding: a sticky child resolves bottom: 0 against the CONTENT
     box, so with padding here the run dock parked 15px short of the card's
     edge and the controls it was scrolled past showed through the strip
     underneath it. The dock carries its own bottom padding instead; the two
     suite notes that can end the sidebar carry .mn-sidebar-tail. */
  padding: 14px 14px 0 14px !important;
  /* The sidebar stays in view as the results pane scrolls. That is also what
     makes the run dock below pin to the bottom of the SIDEBAR rather than to
     the viewport, where it would ride over the controls it is scrolled past. */
  position: sticky;
  top: 12px;
  max-height: calc(100vh - 24px);
  overflow-y: auto;
}

/* Sidebar sections: separated by a rule and a label, not by six tints. Every
   section carries the rule, the first one included -- without it Context was
   the one heading in the stack that read as ungrouped. */
.mn-section { border-top: 1px solid var(--mn-line); padding: 2px 0 6px 0; }

/* A nested parameter group inside a section (auxiliary variables, manual
   tuning, the per-locality resolution table). An inset surface rather than
   another border colour, so nesting reads as depth and not as a warning. */
.mn-subsection {
  background: var(--mn-surface-2);
  border: 1px solid var(--mn-line);
  border-radius: var(--mn-radius);
  padding: 10px;
  margin-bottom: 12px;
}
.mn-subsection > *:last-child { margin-bottom: 0; }
.mn-subsection:has(> #loc_res_table:empty):has(> #strict_buffer_note:empty) { display: none; }

details.sidebar-section { border: 0; }
details.sidebar-section > summary {
  cursor: pointer; list-style: none; display: flex; align-items: center;
  justify-content: space-between; user-select: none;
  padding: 11px 0 9px 0;
}
details.sidebar-section > summary::-webkit-details-marker { display: none; }
details.sidebar-section > summary h4 {
  margin: 0;
  font-size: 11px; font-weight: 700; letter-spacing: .1em; text-transform: uppercase;
  color: var(--mn-text);
}
details.sidebar-section > summary::after {
  content: '\\25B8'; font-size: 13px; color: var(--mn-text-3);
  transition: transform .15s ease;
}
details.sidebar-section[open] > summary::after { transform: rotate(90deg); }
details.sidebar-section > summary + * { margin-top: 6px; }

.mn-dock-est {
  display: flex; align-items: flex-start; gap: 7px;
  font-size: 11.5px; color: var(--mn-text-3);
  margin-bottom: 9px; line-height: 1.45;
}
.mn-dock-est b { color: var(--mn-text-2); font-weight: 500; }
.mn-dock-est > .fa, .mn-dock-est > svg { flex: 0 0 auto; margin-top: 2px; }

.sidebar-run-sticky {
  position: sticky; bottom: 0; z-index: 50;
  background: var(--mn-surface);
  border-top: 1px solid var(--mn-line);
  margin: 10px -14px 0 -14px;
  padding: 12px 14px 14px 14px;
  border-radius: 0 0 var(--mn-radius-lg) var(--mn-radius-lg);
  box-shadow: none;
}
/* Whatever ends the sidebar when the run dock is not on screen still needs the
   card's bottom padding, which the well no longer supplies. */
.mn-sidebar-tail { margin-bottom: 14px; }

/* ==== tabs ============================================================== */
.nav-tabs { border-bottom: 1px solid var(--mn-line); }
.nav-tabs > li > a {
  color: var(--mn-text-2) !important;
  font-weight: 500 !important;
  font-size: 13px;
  opacity: 1;
  border: 0 !important;
  border-bottom: 2px solid transparent !important;
  border-radius: 0 !important;
  margin-right: 2px;
  padding: 9px 13px;
  background: transparent !important;
}
.nav-tabs > li > a:hover { color: var(--mn-text) !important; background: transparent !important; }
.nav-tabs > li.active > a,
.nav-tabs > li.active > a:hover,
.nav-tabs > li.active > a:focus {
  color: var(--mn-text) !important;
  background: transparent !important;
  border-bottom-color: var(--mn-accent) !important;
}
#main_tabs.nav-tabs { display: flex; flex-wrap: wrap; align-items: stretch; }
#main_tabs > li:has(a[data-value='tab_desc']) {
  margin-left: auto;
  border-left: 1px solid var(--mn-line);
  padding-left: 10px;
}

.nav-pills > li > a {
  color: var(--mn-text-2) !important;
  background-color: transparent !important;
  border: 1px solid var(--mn-line-2) !important;
  border-radius: var(--mn-radius) !important;
  margin-right: 4px; font-size: 12px; padding: 6px 11px;
}
.nav-pills > li > a:hover { color: var(--mn-text) !important; background-color: var(--mn-surface-2) !important; }
.nav-pills > li.active > a,
.nav-pills > li.active > a:hover,
.nav-pills > li.active > a:focus {
  color: var(--mn-accent) !important;
  background-color: var(--mn-accent-weak) !important;
  border-color: var(--mn-accent) !important;
  font-weight: 500 !important;
}

/* ==== buttons =========================================================== */
/* Three levels and one destructive. The Bootstrap colour classes still in the
   markup are mapped onto them: a filled button means the primary action of
   its region, so only .btn-primary and the two run buttons get one. */
.btn {
  height: var(--mn-control-h);
  padding: 0 14px;
  display: inline-flex; align-items: center; justify-content: center; gap: 7px;
  border-radius: var(--mn-radius);
  border: 1px solid transparent;
  font-size: 12.5px; font-weight: 500;
  box-shadow: none;
  transition: background-color .15s ease, border-color .15s ease, color .15s ease;
}
/* Bootstrap rings every clicked button with its own dotted focus outline, so a
   button kept a box around it after the click that opened its panel. Focus is
   shown for keyboard users only, where it is what tells them where they are. */
.btn:focus, .btn:active:focus, .btn.active:focus { outline: none; }
.btn:focus-visible { outline: 2px solid var(--mn-accent); outline-offset: 2px; }
.btn-lg { height: 38px; font-size: 13.5px; padding: 0 18px; }
.btn-sm { height: 27px; font-size: 12px; padding: 0 10px; }
.btn-xs { height: 24px; font-size: 11.5px; padding: 0 8px; }
.btn-block { width: 100%; }

/* Qualified with .btn: actionButton() emits btn-default alongside whatever
   class the caller asked for, so these have to win on specificity, not order. */
.btn.btn-primary {
  background-color: var(--mn-accent) !important;
  border-color: var(--mn-accent) !important;
  color: var(--mn-on-accent) !important;
}
.btn.btn-primary:hover, .btn.btn-primary:focus, .btn.btn-primary:active, .btn.btn-primary.active {
  background-color: var(--mn-accent-hover) !important;
  border-color: var(--mn-accent-hover) !important;
  color: var(--mn-on-accent) !important;
  opacity: 1;
}

.btn-default, .btn-secondary, .btn-info, .btn-warning, .btn-success {
  background-color: var(--mn-surface) !important;
  border-color: var(--mn-line-2) !important;
  color: var(--mn-text) !important;
}
.btn-default:hover, .btn-secondary:hover, .btn-info:hover, .btn-warning:hover, .btn-success:hover,
.btn-default:focus-visible, .btn-secondary:focus-visible, .btn-info:focus-visible,
.btn-warning:focus-visible, .btn-success:focus-visible {
  background-color: var(--mn-surface-2) !important;
  border-color: var(--mn-text-3) !important;
  color: var(--mn-text) !important;
}
.btn.btn-light, .btn.btn-outline-secondary {
  background-color: transparent !important;
  border-color: transparent !important;
  color: var(--mn-text-2) !important;
}
.btn.btn-light:hover, .btn.btn-outline-secondary:hover {
  background-color: var(--mn-surface-3) !important;
  color: var(--mn-text) !important;
}
.btn.btn-danger {
  background-color: transparent !important;
  border-color: var(--mn-line-2) !important;
  color: var(--mn-danger) !important;
}
.btn.btn-danger:hover, .btn.btn-danger:focus {
  background-color: var(--mn-danger) !important;
  border-color: var(--mn-danger) !important;
  color: #ffffff !important;
}
.btn[disabled], .btn.disabled { opacity: .4; box-shadow: none; }
.btn-pill { border-radius: 999px !important; font-weight: 500; letter-spacing: 0; }
.btn-pill:hover, .btn-pill:focus { box-shadow: none; transform: none; }
.expand-icon-btn { width: 30px; height: 30px; padding: 0 !important; }

/* segmented controls (shinyWidgets radioGroupButtons) */
.btn-group-container-sw .radiobtn > label,
.btn-group-toggle > .btn {
  background-color: var(--mn-surface) !important;
  border-color: var(--mn-line-2) !important;
  color: var(--mn-text-2) !important;
  font-weight: 400 !important;
  box-shadow: none !important;
}
.btn-group-container-sw .radiobtn > label.active,
.btn-group-toggle > .btn.active {
  background-color: var(--mn-accent-weak) !important;
  border-color: var(--mn-accent) !important;
  color: var(--mn-accent) !important;
  font-weight: 500 !important;
}

/* Two-column segmented grid. Choices worth comparing at a glance -- the
   interpolation method, the primary view, the boundary -- are laid out as a
   grid of cells rather than hidden behind a dropdown: every option is visible
   and selecting one is a single click.

   shinyWidgets wraps EACH option in its own .btn-group holding a
   button.btn.radiobtn, so the grid goes on the container and every .btn-group
   is one cell. Borders are collapsed between neighbours so the set reads as a
   single control. */
.mn-seg-grid .btn-group-container-sw,
.mn-seg-grid-3 .btn-group-container-sw {
  display: grid !important;
  width: 100%;
  border-radius: var(--mn-radius);
  overflow: hidden;
}
.mn-seg-grid .btn-group-container-sw { grid-template-columns: repeat(2, minmax(0, 1fr)); }
/* Three-column variant, for sets whose option count fills the row exactly. */
.mn-seg-grid-3 .btn-group-container-sw { grid-template-columns: repeat(3, minmax(0, 1fr)); }
/* The container carries .btn-group itself, so every cell rule is scoped to a
   direct child -- an unscoped .btn-group rule matches the container too and,
   with !important, overrides the grid above it. */
.mn-seg-grid .btn-group-container-sw > .btn-group,
.mn-seg-grid-3 .btn-group-container-sw > .btn-group {
  display: flex !important;
  align-items: stretch;
  float: none !important;
  width: 100%;
  margin: 0 !important;
}
.mn-seg-grid .btn-group-container-sw > .btn-group > .btn.radiobtn,
.mn-seg-grid-3 .btn-group-container-sw > .btn-group > .btn.radiobtn {
  width: 100%;
  height: 100%;
  min-height: var(--mn-control-h);
  margin: 0 !important;
  padding: 6px 9px;
  display: flex; align-items: center; justify-content: center;
  text-align: center; white-space: normal; line-height: 1.25;
  border-radius: 0 !important;
}
/* Collapse the doubled 1px seams: a cell keeps its left border only when it
   opens a row, and its top border only in the first row. A final option that
   would sit alone stretches across the row instead, so the block always ends
   as a rectangle rather than a half-empty cell. */
.mn-seg-grid .btn-group-container-sw > .btn-group:not(:nth-child(2n+1)) > .btn { border-left-width: 0 !important; }
.mn-seg-grid .btn-group-container-sw > .btn-group:nth-child(n+3) > .btn { border-top-width: 0 !important; }
.mn-seg-grid .btn-group-container-sw > .btn-group:last-child:nth-child(2n+1) { grid-column: 1 / -1; }
.mn-seg-grid-3 .btn-group-container-sw > .btn-group:not(:nth-child(3n+1)) > .btn { border-left-width: 0 !important; }
.mn-seg-grid-3 .btn-group-container-sw > .btn-group:nth-child(n+4) > .btn { border-top-width: 0 !important; }
.mn-seg-grid-3 .btn-group-container-sw > .btn-group:last-child:nth-child(3n+1) { grid-column: 1 / -1; }
.mn-seg-grid-3 .btn-group-container-sw > .btn-group:last-child:nth-child(3n+2) { grid-column: span 2; }

/* A select paired with a trailing clear button. The button lines up with the
   control's own box, not with the label above it, and takes the same height
   and corner radius as the select so the pair reads as one row. */
.mn-input-with-clear { display: flex; align-items: flex-end; gap: 6px; }
.mn-input-with-clear > .form-group { flex: 1 1 0%; min-width: 0; }
.mn-input-with-clear > .btn.mn-clear-btn {
  flex: 0 0 auto;
  width: var(--mn-control-h); height: var(--mn-control-h);
  padding: 0; margin-bottom: 12px; /* matches .form-group's bottom margin */
  display: inline-flex; align-items: center; justify-content: center;
  border-radius: var(--mn-radius);
}

/* ==== forms ============================================================= */
label, .control-label {
  font-size: 12px; font-weight: 500; color: var(--mn-text); margin-bottom: 5px;
}
.help-block, .shiny-input-container > .help-block {
  font-size: 11.5px; color: var(--mn-text-3); line-height: 1.45;
}
.form-group { margin-bottom: 12px; }
.form-control, input[type='text'], input[type='number'], textarea, select {
  height: var(--mn-control-h);
  background-color: var(--mn-surface);
  border: 1px solid var(--mn-line-2);
  border-radius: var(--mn-radius);
  color: var(--mn-text);
  font-size: 12.5px;
  box-shadow: none;
  padding: 0 10px;
}
textarea.form-control { height: auto; padding: 8px 10px; }
.form-control:focus, input:focus, select:focus, textarea:focus {
  border-color: var(--mn-accent);
  box-shadow: 0 0 0 3px var(--mn-accent-weak);
  outline: none;
}
.form-control::placeholder { color: var(--mn-text-3); }
/* Bootstrap paints readonly and disabled fields #eee. In the dark variant that
   is a near-white field carrying near-white text -- which is what made the file
   input's filename box unreadable once a file was chosen. */
.form-control[readonly],
.form-control[disabled],
fieldset[disabled] .form-control {
  background-color: var(--mn-surface-2);
  color: var(--mn-text);
  opacity: 1;
}

/* A native <select> (selectize = FALSE) otherwise keeps the browser's own
   chrome and reads as a foreign control beside every other input. */
select, select.form-control {
  -webkit-appearance: none; -moz-appearance: none; appearance: none;
  padding-right: 28px;
  background-repeat: no-repeat;
  background-position: right 9px center;
  background-size: 13px 13px;
  background-image: var(--mn-chevron);
}
select::-ms-expand { display: none; }

/* selectize */
.selectize-input {
  min-height: var(--mn-control-h);
  background: var(--mn-surface) !important;
  border: 1px solid var(--mn-line-2) !important;
  border-radius: var(--mn-radius) !important;
  color: var(--mn-text) !important;
  box-shadow: none !important;
  font-size: 12.5px;
  padding: 4px 9px;
  line-height: 22px;
}
.selectize-control.single .selectize-input { padding-right: 28px; }
.selectize-input > input { line-height: 22px; height: 22px; margin: 0 !important; }
.selectize-input > .item { line-height: 22px; height: 22px; }
/* selectize draws its disclosure mark as a CSS border triangle inset 15px from
   the right while the text is inset 9px from the left, and flips it to a
   second, differently sized triangle while the menu is open. One chevron,
   inset the same as the text, rotated in place instead. */
.selectize-control.single .selectize-input::after,
.selectize-control.single .selectize-input.dropdown-active::after {
  content: '' !important;
  position: absolute !important; top: 50% !important; right: 9px !important;
  width: 13px !important; height: 13px !important; margin-top: -6.5px !important;
  border: 0 !important;
  background: var(--mn-chevron) no-repeat center center;
  background-size: 13px 13px;
  transition: transform .15s ease;
}
.selectize-control.single .selectize-input.dropdown-active::after { transform: rotate(180deg); }
.selectize-input.focus { border-color: var(--mn-accent) !important; box-shadow: 0 0 0 3px var(--mn-accent-weak) !important; }
.selectize-control.multi .selectize-input > .item {
  background: var(--mn-accent-weak) !important;
  color: var(--mn-accent) !important;
  border: 0 !important;
  border-radius: 4px !important;
  padding: 0 7px;
}
.selectize-control.single .selectize-input > .item {
  background: transparent !important;
  color: var(--mn-text) !important;
  border: 0 !important;
  border-radius: 0 !important;
  padding: 0 !important;
}
/* The open menu has to clear whatever the page draws after it -- the panel's
   own bottom rule and the controls in the next row were painting across it,
   because selectize ships z-index: 10 and both of those are later siblings in
   the same stacking context. */
.selectize-dropdown {
  background: var(--mn-surface);
  border: 1px solid var(--mn-line);
  border-radius: var(--mn-radius);
  box-shadow: var(--mn-shadow);
  color: var(--mn-text);
  font-size: 12.5px;
  z-index: 1300;
}
.selectize-control.dropdown-active { position: relative; z-index: 1300; }
.selectize-dropdown .active { background: var(--mn-surface-2); color: var(--mn-text); }

/* bootstrap-select (pickerInput) */
.bootstrap-select > .dropdown-toggle {
  background: var(--mn-surface) !important;
  border: 1px solid var(--mn-line-2) !important;
  border-radius: var(--mn-radius) !important;
  color: var(--mn-text) !important;
  height: var(--mn-control-h);
  box-shadow: none !important;
  padding: 0 30px 0 10px !important;
  position: relative;
  justify-content: flex-start;
  text-align: left;
}
/* Bootstrap's caret is a border triangle that bootstrap-select flips to point
   UP whenever the menu would open upwards, so the same control showed two
   different marks in two places. The shared chevron, inset like the text. */
.bootstrap-select > .dropdown-toggle .caret {
  position: absolute !important; top: 50% !important; right: 9px !important;
  width: 13px !important; height: 13px !important; margin-top: -6.5px !important;
  border: 0 !important;
  background: var(--mn-chevron) no-repeat center center;
  background-size: 13px 13px;
  transition: transform .15s ease;
}
.bootstrap-select.open > .dropdown-toggle .caret { transform: rotate(180deg); }
/* bootstrap-select's label box stretches to the toggle's full height and then
   leaves its text on the first line of it, so the selected value sat above the
   centre of its own control while the chevron beside it was centred. Bootstrap
   never shows this because its buttons are sized by padding; ours have a fixed
   height. Centre the label in the box it was given. */
.bootstrap-select > .dropdown-toggle .filter-option {
  display: flex !important;
  align-items: center !important;
  height: 100% !important;
  float: none !important;
  min-width: 0;
}
.bootstrap-select > .dropdown-toggle .filter-option-inner { width: 100%; min-width: 0; }
.bootstrap-select > .dropdown-toggle .filter-option-inner-inner {
  overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
}
/* bootstrap-select ships its own !important focus ring, which is the same
   leftover box the header icon buttons had. */
.bootstrap-select .dropdown-toggle:focus,
.bootstrap-select > select.mobile-device:focus + .dropdown-toggle { outline: none !important; }
.bootstrap-select .dropdown-toggle:focus-visible {
  outline: 2px solid var(--mn-accent) !important; outline-offset: 2px !important;
}
.bootstrap-select > .dropdown-toggle.bs-placeholder,
.bootstrap-select > .dropdown-toggle.bs-placeholder:hover,
.bootstrap-select > .dropdown-toggle.bs-placeholder:focus,
.bootstrap-select > .dropdown-toggle.bs-placeholder:active { color: var(--mn-text-3) !important; }
.bootstrap-select .dropdown-menu {
  background: var(--mn-surface);
  border: 1px solid var(--mn-line);
  box-shadow: var(--mn-shadow);
}
.bootstrap-select .dropdown-menu li a { color: var(--mn-text); font-size: 12.5px; }
.bootstrap-select .dropdown-menu li a:hover,
.bootstrap-select .dropdown-menu li.selected a { background: var(--mn-surface-2); color: var(--mn-text); }
.bootstrap-select .dropdown-menu li a span.text {
  display: flex !important; width: 100% !important;
  align-items: center; justify-content: space-between;
}

/* checkboxes */
.checkbox label, .radio label {
  font-weight: 400; font-size: 12.5px; color: var(--mn-text);
  display: flex; align-items: center; gap: 8px;
  padding-left: 0; min-height: 20px;
}
.checkbox label > input[type='checkbox'],
.radio label > input[type='radio'] {
  position: static; margin: 0 !important; flex: 0 0 auto;
}
.radio-inline { display: inline-flex !important; align-items: center; gap: 7px; padding-left: 0 !important; }
.radio-inline > input[type='radio'] { position: static !important; margin: 0 !important; flex: 0 0 auto; }
input[type='checkbox'], input[type='radio'] { accent-color: var(--mn-accent); }

/* ionRangeSlider */
.irs--shiny .irs-bar, .irs-bar { background: var(--mn-accent); border-color: var(--mn-accent); }
/* The unfilled part of the track: surface-3 on a surface card was a tint of a
   tint and read as empty space, so the slider looked like a handle with
   nothing behind it. line-2 is the hairline the rest of the interface uses to
   mark an edge, and it holds in both variants. */
.irs--shiny .irs-line, .irs-line { background: var(--mn-line-2); border: 0; }
.irs--shiny .irs-handle { background: var(--mn-surface); border: 2px solid var(--mn-accent); box-shadow: none; }
.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
  background: var(--mn-accent); color: var(--mn-on-accent); font-family: var(--mn-mono); font-size: 11px;
}
.irs--shiny .irs-min, .irs--shiny .irs-max {
  background: transparent; color: var(--mn-text-3); font-family: var(--mn-mono); font-size: 11px;
}
/* Grid labels are centred on their tick and collide with each other before
   ionRangeSlider's pairwise hiding catches them, which is what a six-figure
   variogram sill produces. A smaller label buys the room; force_edges (set on
   every slider as it binds, see ui_main.R) keeps the outermost two from
   overhanging the track. */
.irs--shiny .irs-grid-text { color: var(--mn-text-3); font-family: var(--mn-mono); font-size: 10px; }
.irs--shiny .irs-grid-pol { background: var(--mn-line-2); }

/* file input */
.form-control-file, input[type='file'] { font-size: 12px; color: var(--mn-text-2); }
.progress { background: var(--mn-surface-3); border-radius: 999px; height: 6px; box-shadow: none; }
.progress-bar { background-color: var(--mn-accent); }
/* Shiny's file-input bar carries a label -- Upload complete, or the error text
   when an upload fails. At the 6px height above, that label overflowed the bar
   and printed itself across whatever sat underneath, so this one bar keeps a
   height its own text fits in, and it sits below the field rather than
   against it. */
.progress.shiny-file-input-progress {
  height: 18px;
  margin: 7px 0 0 0;
  border-radius: var(--mn-radius);
  overflow: hidden;
}
.progress.shiny-file-input-progress .progress-bar {
  height: 18px; line-height: 18px;
  font-size: 11px; font-weight: 500;
  color: var(--mn-on-accent);
}
.progress.shiny-file-input-progress .progress-bar.bar-danger {
  background-color: var(--mn-danger); color: #ffffff;
}
/* The Browse button and the filename field are one control: square the joint
   and let the field take the rest of the row. */
.input-group .btn.btn-file {
  border-top-right-radius: 0 !important; border-bottom-right-radius: 0 !important;
  border-color: var(--mn-line-2) !important;
}
.input-group > .input-group-btn + .form-control {
  border-top-left-radius: 0; border-bottom-left-radius: 0; border-left: 0;
}

/* ==== cards and panels ================================================== */
.custom-box, .sci-card {
  background-color: var(--mn-surface) !important;
  color: var(--mn-text) !important;
  border: 1px solid var(--mn-line) !important;
  border-radius: var(--mn-radius-lg);
  padding: 14px;
  margin-bottom: 14px;
  box-shadow: none;
}
.sci-card h4, .sci-card h5, .sci-card h6 { color: var(--mn-text); }
.sci-card-sub { font-size: 11.5px; color: var(--mn-text-3); font-style: normal; margin: 0 0 10px 0; }
/* A figure is a card like any other result: the same hairline box, with its
   title and tools in a header row rather than floating over the plot. */
.sci-plot-card {
  background: var(--mn-surface);
  border: 1px solid var(--mn-line);
  border-radius: var(--mn-radius-lg);
  padding: 12px 14px 14px 14px;
  margin-bottom: 14px;
}
.sci-plot-card-head {
  display: flex; justify-content: space-between; align-items: center;
  margin: 0 0 10px 0;
}
.sci-plot-card-head h4 { font-size: 13.5px !important; }
.sci-plot-card-tools { display: flex; gap: 4px; }
.sci-plot-card-tools .btn {
  border: 1px solid transparent; background: transparent; color: var(--mn-text-2);
}
.sci-plot-card-tools .btn:hover { background: var(--mn-surface-3); color: var(--mn-text); }

.setup-card {
  background-color: var(--mn-surface) !important;
  color: var(--mn-text) !important;
  border: 1px solid var(--mn-line);
  border-radius: var(--mn-radius-lg);
  padding: 18px 20px;
  margin-bottom: 14px;
  box-shadow: none;
  animation: setup-fade-in .35s ease;
}
@keyframes setup-fade-in { from { opacity: 0; transform: translateY(6px); } to { opacity: 1; transform: none; } }
.setup-card-header { display: flex; align-items: center; gap: 12px; margin: 0 0 4px 0; }
.setup-step-badge {
  flex: 0 0 auto; width: 24px; height: 24px; border-radius: 50%;
  background-color: var(--mn-accent-weak); color: var(--mn-accent);
  display: inline-flex; align-items: center; justify-content: center;
  font-weight: 600; font-size: 11.5px;
}
.setup-card-title { font-size: 13.5px; font-weight: 600; margin: 0; color: var(--mn-text) !important; }
.setup-card-sub { font-size: 11.5px; color: var(--mn-text-3); opacity: 1; margin: 0 0 14px 36px; }
.setup-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(240px, 1fr)); gap: 0 16px; align-items: start; }
.setup-grid .shiny-input-container { width: 100% !important; }
.setup-hint { font-size: 11.5px; font-style: normal; color: var(--mn-text-3); line-height: 1.45; margin: 2px 0 10px 0; }
/* The dataset and the variable list that describes its columns: two fields of
   one pass, side by side, so the optional one is chosen while the required one
   is still on screen rather than three cards further down. */
.setup-primary-upload {
  display: flex; gap: 22px; flex-wrap: wrap; align-items: flex-start;
}
.setup-upload-field { flex: 1 1 320px; max-width: 420px; min-width: 0; }
.setup-upload-field .shiny-input-container { width: 100% !important; }
/* The optional boundary shapefile: one compact row, stated as optional, after
   the upload it is optional to -- not a second field of equal weight sitting
   beside the required one, which read as though the analysis were waiting for
   a shapefile before it would start. */
.setup-optional {
  display: flex; align-items: center; justify-content: space-between;
  gap: 16px; flex-wrap: wrap;
  border: 1px solid var(--mn-line);
  border-radius: var(--mn-radius);
  background: var(--mn-surface-2);
  padding: 11px 14px;
  margin-top: 4px;
}
.setup-optional-text { flex: 1 1 340px; min-width: 0; }
.setup-optional-title { font-size: 12.5px; font-weight: 600; color: var(--mn-text); }
.setup-optional-sub { font-size: 11.5px; color: var(--mn-text-3); line-height: 1.45; margin: 2px 0 0 0; }
/* The field keeps its Browse button and drops the read-only filename box; the
   progress bar under the button reports what happened to the upload. */
.setup-optional .shiny-input-container,
.setup-optional .form-group { width: auto !important; margin: 0 !important; }
/* Clipped rather than display: none -- the row heading above carries the same
   words visually, but the field still needs its own accessible name. */
.setup-optional .shiny-input-container > label {
  position: absolute; width: 1px; height: 1px; padding: 0; margin: -1px;
  overflow: hidden; clip: rect(0 0 0 0); white-space: nowrap; border: 0;
}
.setup-optional .input-group { display: block; width: auto; }
.setup-optional .input-group > .form-control { display: none; }
.setup-optional .btn.btn-file {
  border-radius: var(--mn-radius) !important;
  border-color: var(--mn-line-2) !important;
  white-space: nowrap;
}
/* The progress bar sits under the button inside the same flex item, so with
   it in flow the item centred as button-plus-bar and the button itself rode
   high in the row. Out of flow, the button is what gets centred. */
.setup-optional .shiny-input-container { position: relative; }
.setup-optional .progress.shiny-file-input-progress {
  position: absolute; top: 100%; left: 0; right: 0; margin-top: 6px;
}

/* ==== tables ============================================================ */
.table-container { width: 100%; overflow-x: auto; font-size: 12.5px; margin-bottom: 10px; }
.table-container table,
.table-container .dataTables_wrapper {
  width: 100% !important;
  background-color: var(--mn-surface) !important;
  color: var(--mn-text) !important;
  margin-bottom: 0;
  border-radius: var(--mn-radius);
}
.table-container th,
.table-container table.dataTable th {
  background-color: var(--mn-surface-2) !important;
  color: var(--mn-text-3) !important;
  font-size: 11px; font-weight: 600;
  border-bottom: 1px solid var(--mn-line-2) !important;
  white-space: nowrap;
}
.table-container td,
.table-container table.dataTable td {
  color: var(--mn-text) !important;
  border-top: 1px solid var(--mn-line) !important;
  font-variant-numeric: tabular-nums;
  white-space: nowrap;
}
/* Numbers get the monospaced face so decimal points line up down a column;
   the first cell of a row is its label, so it stays in the UI face. */
.table-container table td:not(:first-child) { font-family: var(--mn-mono); }
table.dataTable.stripe tbody tr.odd,
table.dataTable tbody tr.odd { background-color: var(--mn-surface-2) !important; }
/* The Total and the Selected-locality summaries are stacked and read as one
   block, but DataTables sizes each table against its own content, so the two
   Metric columns ended up different widths and the numbers did not line up
   down the page. Fixed layout with a declared first column pins both. */
#stats_table_total table.dataTable,
#stats_table_loc table.dataTable { table-layout: fixed !important; width: 100% !important; }
#stats_table_total table.dataTable th,
#stats_table_total table.dataTable td,
#stats_table_loc table.dataTable th,
#stats_table_loc table.dataTable td { width: auto !important; }
#stats_table_total table.dataTable th:first-child,
#stats_table_total table.dataTable td:first-child,
#stats_table_loc table.dataTable th:first-child,
#stats_table_loc table.dataTable td:first-child { width: 38% !important; }
.dataTables_wrapper .dataTables_info,
.dataTables_wrapper .dataTables_paginate,
.dataTables_wrapper .dataTables_filter,
.dataTables_wrapper .dataTables_length { color: var(--mn-text-2) !important; font-size: 12px; }
.dataTables_wrapper .paginate_button { color: var(--mn-text-2) !important; }
.dataTables_wrapper .paginate_button.current {
  background: var(--mn-accent-weak) !important; color: var(--mn-accent) !important; border-color: var(--mn-accent) !important;
}

/* ==== overlays, drawers, popovers ======================================= */
.map-processing-overlay {
  position: absolute; top: 0; left: 0; width: 100%; height: 100%; min-height: 750px;
  background-color: var(--mn-surface) !important;
  color: var(--mn-text) !important;
  z-index: 2000;
  display: flex; align-items: center; justify-content: center; flex-direction: column;
  gap: 16px; padding: 40px;
  border-radius: var(--mn-radius-lg);
  border: 1px solid var(--mn-line);
  box-shadow: none;
  transition: opacity .3s ease;
}
#map_processing_title { margin: 0; font-size: 16px; font-weight: 600; }
#map_progress_text {
  margin: 0; font-size: 12.5px; color: var(--mn-text-2) !important;
  max-width: 440px; text-align: center; line-height: 1.55;
}
.premium-progress-bar-container {
  width: 320px; max-width: 100%;
  background-color: var(--mn-surface-3);
  height: 6px; border-radius: 3px; overflow: hidden;
  border: 0; position: relative;
}
.premium-progress-bar-inner {
  width: 5%; height: 100%;
  background-color: var(--mn-accent);
  transition: width .4s cubic-bezier(.4, 0, .2, 1);
}
/* Phase strip under the bar: pending / running / finished. */
.mn-run-steps {
  display: flex; gap: 22px; flex-wrap: wrap; justify-content: center;
  font-family: var(--mn-mono); font-size: 11.5px; color: var(--mn-text-3);
}
.mn-run-step.on { color: var(--mn-text); font-weight: 600; }
.mn-run-step.done { color: var(--mn-ok); }

.docs-drawer {
  position: fixed; right: -600px; top: 0; width: 600px; height: 100%;
  background-color: var(--mn-surface) !important;
  color: var(--mn-text) !important;
  z-index: 2500; transition: right .3s ease;
  box-shadow: var(--mn-shadow);
  overflow: hidden; padding: 0;
  border-left: 1px solid var(--mn-line) !important;
}
/* The scroller sits inside the frame with a right inset, so its bar keeps a
   visible gap from the page scrollbar running along the window edge. */
.docs-drawer-body {
  height: 100%; overflow-y: auto; overscroll-behavior: contain;
  padding: 20px 14px 20px 20px; margin-right: 6px;
  scrollbar-width: thin;
}
.docs-drawer-body::-webkit-scrollbar { width: 8px; }
.docs-drawer-body::-webkit-scrollbar-track { background: transparent; }
.docs-drawer-body::-webkit-scrollbar-thumb {
  background: var(--mn-line-2); border-radius: 4px;
}
.docs-drawer.open { right: 0; }
.docs-drawer .nav-tabs > li.active > a { background-color: transparent !important; }
.docs-nav-fab { display: none; }
.docs-drawer.open .docs-nav-fab {
  display: flex; flex-direction: column; gap: 6px;
  position: fixed; right: 24px; bottom: 40px; z-index: 2510;
}
.docs-nav-fab .btn {
  width: 36px; height: 36px; border-radius: 50%; padding: 0;
  display: flex; align-items: center; justify-content: center;
  background-color: var(--mn-accent) !important;
  color: var(--mn-on-accent) !important;
  border: none; box-shadow: var(--mn-shadow);
}

.modal { z-index: 2610 !important; }
.modal-backdrop { z-index: 2600 !important; }
.modal-content { background: var(--mn-surface); color: var(--mn-text); border: 1px solid var(--mn-line); border-radius: var(--mn-radius-lg); }
.modal-header, .modal-footer { border-color: var(--mn-line); }

.popover {
  /* Above every overlay the app raises: the run overlay (2000), the docs
     drawer (2500) and modals (2610). Bootstrap's own 1060 put the info
     tooltips behind the interpolation overlay. */
  z-index: 2700 !important;
  background-color: var(--mn-surface) !important;
  color: var(--mn-text) !important;
  border: 1px solid var(--mn-line) !important;
  border-radius: var(--mn-radius-lg) !important;
  box-shadow: var(--mn-shadow);
  max-width: 400px;
  font-size: 12px;
}
.popover-title, .popover-header {
  background-color: var(--mn-surface-2) !important;
  color: var(--mn-text) !important;
  border-bottom: 1px solid var(--mn-line) !important;
  font-size: 12.5px; font-weight: 600;
}
.popover-content, .popover-body { color: var(--mn-text-2) !important; }
.popover.right > .arrow:after { border-right-color: var(--mn-surface) !important; }
.info-icon { color: var(--mn-text-3) !important; }
.info-icon:hover { color: var(--mn-accent) !important; }

/* Notifications. Shiny parks the panel in the bottom-RIGHT corner at a fixed
   300px, which is the corner furthest from where the reader is working, and
   paints every severity the same grey. Centred along the TOP edge instead --
   where the eye already is after pressing a control -- and tinted with the
   role the message carries, on one uniform border rather than an accent stub
   down the left side. */
#shiny-notification-panel {
  position: fixed;
  left: 50%; right: auto; top: 14px; bottom: auto;
  transform: translateX(-50%);
  width: min(560px, calc(100vw - 48px));
  padding: 0; margin: 0;
}
.shiny-notification {
  width: 100% !important;
  background: var(--mn-surface);
  color: var(--mn-text);
  border: 1px solid var(--mn-line-2);
  border-radius: var(--mn-radius-lg);
  box-shadow: var(--mn-shadow);
  opacity: 1;
  font-size: 12.5px;
  line-height: 1.5;
  padding: 11px 32px 11px 14px;
  /* Stacked downwards from the top edge, so the gap goes below each box. */
  margin: 0 0 8px 0;
}
/* color-mix keeps one tint recipe for both variants; the plain surface-2
   declaration above it is the fallback where color-mix is unavailable. */
.shiny-notification-message {
  border-color: var(--mn-accent);
  background: var(--mn-surface-2);
  background: color-mix(in srgb, var(--mn-accent) 26%, var(--mn-surface));
}
.shiny-notification-warning {
  border-color: var(--mn-warn);
  background: var(--mn-surface-2);
  background: color-mix(in srgb, var(--mn-warn) 26%, var(--mn-surface));
}
.shiny-notification-error {
  border-color: var(--mn-danger);
  background: var(--mn-surface-2);
  background: color-mix(in srgb, var(--mn-danger) 26%, var(--mn-surface));
}
.shiny-notification-close {
  color: var(--mn-text-3); opacity: 1;
  top: 5px; right: 9px; font-size: 15px; line-height: 1;
}
.shiny-notification-close:hover { color: var(--mn-text); }

/* ==== leaflet =========================================================== */
.leaflet-bar a, .leaflet-control-layers, .leaflet-popup-content-wrapper, .leaflet-popup-tip {
  background: var(--mn-surface); color: var(--mn-text); border-color: var(--mn-line);
}
.leaflet-bar a:hover { background: var(--mn-surface-2); color: var(--mn-text); }
.leaflet-container { font-family: var(--mn-sans); }

/* Legend. leaflet lays the box out as a title block above two floated columns
   (colour ramp, tick labels), and sizes it shrink-to-fit, so the TITLE decides
   the width: a long variable label stretched a 60px ramp into a translucent
   slab of empty white. Cap the width so the title wraps instead, and give the
   box the app's own surface rather than leaflet's rgba white, which read as a
   wash over the raster underneath. */
.leaflet .info.legend {
  max-width: 190px;
  /* Contains the floats, so the box ends at the ramp instead of leaving the
     trailing <br> line of empty fill under it. */
  overflow: hidden;
  padding: 7px 9px;
  font: 11.5px/16px var(--mn-sans);
  color: var(--mn-text-2);
  background: var(--mn-surface);
  border: 1px solid var(--mn-line);
  border-radius: var(--mn-radius);
  box-shadow: var(--mn-shadow);
}
/* The prepended title block. Long single words (a raw column name) break
   rather than push the box back out past the cap. */
.leaflet .info.legend > div:first-child {
  white-space: normal; overflow-wrap: anywhere;
  color: var(--mn-text); line-height: 1.35;
}
.leaflet .info.legend svg text { fill: var(--mn-text-2); font-family: var(--mn-mono); font-size: 10.5px; }
.leaflet .info.legend svg line { stroke: var(--mn-line-2); }
/* Categorical legends end each entry with <br>; only the trailing one is
   dead space. */
.leaflet .info.legend > br:last-child { display: none; }
/* The variable label the title carries is already printed as the map heading,
   so the legend drops it and shrinks back to its ramp instead of holding the
   full 190px cap open over the surface. Overlays > 'Variable Label in Legend'
   puts it back. Hiding the span alone would leave the title block's inline
   margin-bottom above the ramp, so that margin is answered here too. */
.mn-legend-title { display: none; }
/* The title block collapses with it: leaflet stamps an inline margin-bottom on
   that div, which would otherwise sit over the ramp as dead space. Matched via
   :has() so the point-styling legend - whose title names the grouping column
   and is not ours to hide - keeps its own spacing. Where :has() is unavailable
   the only cost is those 3px. */
.leaflet .info.legend > div:first-child:has(.mn-legend-title) { display: none; }
body.mn-show-legend-title .mn-legend-title { display: inline; }
body.mn-show-legend-title .leaflet .info.legend > div:first-child:has(.mn-legend-title) { display: block; }

/* Leaflet's own controls: built into the widget, so the Overlays checkboxes
   that hide them do it from here rather than by re-rendering three maps. */
body.mn-hide-drawtools .leaflet-draw { display: none !important; }
body.mn-hide-ruler .leaflet-control-measure { display: none !important; }

/* Export registry: one row per asset -- kind, name, time -- separated by rules
   rather than run together in a single label string. */
#selected_assets .checkbox { margin: 0 !important; border-bottom: 1px solid var(--mn-line); }
#selected_assets .checkbox:last-child { border-bottom: 0; }
#selected_assets .checkbox label { display: flex; align-items: center; gap: 10px; width: 100%; padding: 7px 0; }
#selected_assets .checkbox input[type='checkbox'] { margin: 0 !important; flex: 0 0 auto; }
.mn-asset { display: flex; align-items: center; gap: 10px; flex: 1 1 auto; min-width: 0; font-size: 12.5px; }
.mn-asset-kind {
  flex: 0 0 auto; min-width: 46px; text-align: center;
  font-size: 10px; font-weight: 600; letter-spacing: .06em; text-transform: uppercase;
  color: var(--mn-text-3);
  border: 1px solid var(--mn-line); border-radius: 3px; padding: 1px 6px;
}
.mn-asset-label { flex: 1 1 auto; min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.mn-asset-time { flex: 0 0 auto; margin-left: auto; color: var(--mn-text-3); font-size: 11.5px; }
.mn-empty-line { color: var(--mn-text-3); font-size: 12.5px; }

/* The two independent suites lay their own controls out in Bootstrap columns
   with no container of their own, which left their configuration panels bare
   against the page while every other panel in the app sits in a card. This
   gives the narrow control column the same card as everything else without
   touching the modules' internal markup. */
.mn-suite > div > .row > .col-sm-3,
.mn-suite > div > .row > .col-sm-4,
.mn-suite .tab-pane > div > .row > .col-sm-3,
.mn-suite .tab-pane > div > .row > .col-sm-4 {
  background: var(--mn-surface);
  border: 1px solid var(--mn-line);
  border-radius: var(--mn-radius-lg);
  padding: 16px;
}
.mn-suite .row > .col-sm-3 > .form-group:last-child,
.mn-suite .row > .col-sm-4 > .form-group:last-child { margin-bottom: 0 !important; }

/* Server-rendered strips and sections. These used to carry their colours
   inline (a pale blue info strip, an amber archive panel); state colour is
   reserved for state now, so both resolve to the shared surfaces. */
.mn-notice {
  background: var(--mn-surface-2);
  border: 1px solid var(--mn-line);
  border-left: 2px solid var(--mn-accent);
  border-radius: var(--mn-radius);
  padding: 10px 13px;
  margin-bottom: 10px;
  color: var(--mn-text-2);
  font-size: 12px;
}
.mn-notice strong, .mn-notice b { color: var(--mn-text); }
.mn-notice-mono { font-family: var(--mn-mono); line-height: 1.6; }
/* Same shape as .mn-notice, keyed to the warning role: a setting that is on
   screen but not consumed by the selected method. The amber it used to carry
   inline was a fixed light-mode swatch. */
.mn-note-warn {
  background: var(--mn-surface-2);
  border: 1px solid var(--mn-line);
  border-left: 3px solid var(--mn-warn);
  border-radius: var(--mn-radius);
  padding: 6px 9px;
  margin: 8px 0 0 0;
  color: var(--mn-text-2);
  font-size: 11.5px;
  line-height: 1.4;
}
.mn-note-warn b, .mn-note-warn strong { color: var(--mn-warn); }

/* Scientific Analysis stack. The tab's panels are laid out as a flex column so
   two states can be expressed with a single class on the container rather than
   with a second copy of any panel -- every output id in there exists exactly
   once and cannot render twice.
   vgm-first: the sidebar is fitting variograms, so the curve the sliders move
   comes directly after the header instead of below three result cards.
   vgm-only: the run on screen came from IDW or TPS, which have no variogram,
   so its result cards would describe a model unrelated to those curves. */
.sci-stack { display: flex; flex-direction: column; }
.sci-stack > * { order: 2; }
.sci-stack > .sci-stack-head { order: 0; }
.sci-stack.vgm-first > .sci-vgm-block { order: 1; }
.sci-stack.vgm-only > *:not(.sci-stack-head):not(.sci-vgm-block):not(.sci-keep) {
  display: none !important;
}
.mn-panel {
  background: var(--mn-surface);
  border: 1px solid var(--mn-line);
  border-radius: var(--mn-radius-lg);
  padding: 16px 18px;
}
.mn-panel > h4 { margin-top: 0; }

/* ==== map toolbar ======================================================= */
.mn-maptoolbar {
  display: flex; align-items: center; gap: 8px; flex-wrap: wrap;
  /* The toolbar is the first thing under the tab strip; without the top
     margin its controls sat against that border. */
  margin: 12px 0 12px;
}
.mn-maptoolbar > .form-group,
.mn-maptoolbar .shiny-input-container { margin-bottom: 0 !important; width: auto !important; }
/* One height for the whole row. Every button in it is btn-sm (27px) while the
   form default is 32px, so left alone the three selects and the CARTO key
   field stood a head taller than the buttons they sit between. */
.mn-maptoolbar select.form-control,
.mn-maptoolbar input.form-control {
  height: 27px; font-size: 12px;
}
/* One width for the three toolbar selects. Left to size themselves a native
   select is as wide as its longest OPTION, so the basemap picker (which
   carries 'Light / Positron (key)') ran half again as wide as the view picker
   beside it and the row read as ragged. 195px fits the longest label any of
   them can show; anything longer ellipsises rather than widening the row. */
.mn-maptoolbar select.form-control {
  width: 195px !important;
  text-overflow: ellipsis;
}
.mn-tb-group { display: flex; align-items: center; gap: 8px; }
.mn-tb-spacer { flex: 1 1 auto; }

/* A <details> disclosure styled as a button with a floating panel. */
.mn-popover { position: relative; display: inline-block; }
.mn-popover > summary {
  list-style: none; cursor: pointer; user-select: none;
  display: inline-flex; align-items: center; gap: 6px;
}
.mn-popover > summary::-webkit-details-marker { display: none; }
.mn-popover[open] > summary {
  background: var(--mn-surface-2) !important;
  border-color: var(--mn-text-3) !important;
}
.mn-popover-panel {
  position: absolute; z-index: 1200; top: calc(100% + 6px); left: 0;
  background: var(--mn-surface);
  border: 1px solid var(--mn-line);
  border-radius: var(--mn-radius-lg);
  box-shadow: var(--mn-shadow);
  padding: 12px; text-align: left;
}
.mn-popover-right .mn-popover-panel { left: auto; right: 0; }
.mn-popover-panel .form-group { margin-bottom: 10px !important; width: 100% !important; }
/* The panel is its own column: neither the toolbar's fixed select width nor
   its btn-sm control height applies to the controls inside it. */
.mn-popover-panel select.form-control,
.mn-popover-panel input.form-control { width: 100% !important; height: var(--mn-control-h); font-size: 12.5px; }
.mn-popover-panel .form-group:last-child { margin-bottom: 0 !important; }
.mn-popover-panel .checkbox { margin: 0 0 9px 0 !important; }
.mn-popover-panel .checkbox:last-child { margin-bottom: 0 !important; }
.mn-popover-panel .btn { width: 100%; margin-bottom: 6px; }
.mn-popover-panel .btn:last-child { margin-bottom: 0; }
.mn-popover-panel > span { display: block !important; width: 100%; }

/* ==== utilities ========================================================= */
.mn-num { font-family: var(--mn-mono); font-variant-numeric: tabular-nums; }
")
}

#' Early boot script: applies the remembered variant before first paint.
#'
#' Placed in <head> so the document never paints the light variant and then
#' repaints dark on a reader who chose dark.
monolith_theme_boot_js <- function() {
  shiny::tags$script(shiny::HTML(
    "(function () {
       try {
         var v = window.localStorage ? localStorage.getItem('monolith_theme') : null;
         if (v !== 'dark' && v !== 'light') {
           v = (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches) ? 'dark' : 'light';
         }
         document.documentElement.setAttribute('data-theme', v);
       } catch (e) {
         document.documentElement.setAttribute('data-theme', 'light');
       }
     })();"
  ))
}

#' Light / dark toggle.
#'
#' Client-side only: the variant is a presentation choice with no server-side
#' consequence, so flipping it never queues a Shiny message and stays instant
#' while a run holds the session. `id` namespaces the button so it can be
#' placed more than once.
theme_switcher_ui <- function(id) {
  ns <- shiny::NS(id)
  btn_id <- ns("toggle")
  shiny::tagList(
    shiny::tags$button(
      id = btn_id,
      type = "button",
      class = "btn mn-iconbtn mn-theme-toggle",
      title = "Switch between the light and dark variant",
      `aria-label` = "Switch between the light and dark variant",
      shiny::icon("sun", class = "mn-sun"),
      shiny::icon("moon", class = "mn-moon")
    ),
    shiny::tags$script(shiny::HTML(sprintf(
      "$(function () {
         $('#%s').on('click', function () {
           var root = document.documentElement;
           var next = root.getAttribute('data-theme') === 'dark' ? 'light' : 'dark';
           root.setAttribute('data-theme', next);
           try { localStorage.setItem('monolith_theme', next); } catch (e) {}
           $(window).trigger('resize');
         });
       });", btn_id
    )))
  )
}

#' Write a styled plot to disk at the Export Styler's dimensions.
#'
#' `width`/`height`/`dpi` default to the styler controls; the preview passes the
#' same figure at a screen resolution so that what it shows is this writer's
#' output, not a separate rendering of it.
export_plot_to_file <- function(p, filepath, ext, input,
                                width = NULL, height = NULL, dpi = NULL) {
  if (is.null(width))  width  <- input$styler_width %||% 10
  if (is.null(height)) height <- if(isTruthy(input$styler_height)) input$styler_height else 8
  if (is.null(dpi))    dpi    <- input$styler_dpi %||% 300

  # PDF is a vector device measured in points; the raster devices carry the
  # requested resolution. Either way showtext has to be told which, or the point
  # sizes in the theme are not the point sizes on the page (see with_showtext_dpi).
  with_showtext_dpi(if (ext == "pdf") 72 else dpi, {
    ggplot2::ggsave(
      filename = filepath,
      plot = p,
      device = if(ext == "pdf") "pdf" else (if(ext == "tiff") "tiff" else NULL),
      dpi = dpi,
      width = width,
      height = height,
      units = "in"
    )
  })
}
