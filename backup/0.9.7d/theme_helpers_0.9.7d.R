# theme_helpers_0.9.7c.R - UI Dashboard & Map Themes


create_app_theme <- function(light_blue, dark_bg, content_bg, font_family, map_tiles, box_bg = "#ffffff", sidebar_text_color = "#ffffff", body_text_color = "#333333", header_text_color = "#ffffff", banner_style = "standard") {
  theme <- create_theme(
    adminlte_color(
      light_blue = light_blue
    ),
    adminlte_sidebar(
      width = "400px",
      dark_bg = dark_bg,
      dark_hover_bg = adjustcolor(dark_bg, offset = c(0.1, 0.1, 0.1, 0)),
      dark_color = sidebar_text_color
    ),
    adminlte_global(
      content_bg = content_bg,
      box_bg = box_bg, 
      info_box_bg = box_bg
    )
  )
  
  # Ensure the font family is applied universally in manual_style
  font_url_name <- gsub(" ", "+", font_family)
  
  # Calculate a slightly lighter version of the dark_bg for panels
  rgb_dark <- col2rgb(dark_bg)
  panel_bg <- rgb(min(255, rgb_dark[1] + 20)/255, min(255, rgb_dark[2] + 20)/255, min(255, rgb_dark[3] + 20)/255)
  
  # Banner Styling Alternatives
  banner_css <- if (banner_style == "standard") {
    sprintf("
    .header-banner {
      max-height: 50px;
      width: auto;
      object-fit: contain;
      filter: drop-shadow(0 2px 4px rgba(0,0,0,0.3));
      border: 1px solid rgba(255,255,255,0.15);
      border-radius: 4px;
      padding: 2px;
      background-color: rgba(255,255,255,0.05);
      transition: transform 0.3s ease;
    }
    .header-banner:hover {
      transform: scale(1.02);
    }
    ")
  } else if (banner_style == "accent") {
    sprintf("
    .header-banner {
      max-height: 50px;
      width: auto;
      object-fit: contain;
      border-left: 4px solid %s;
      padding-left: 12px;
      filter: grayscale(0.2);
      transition: filter 0.3s ease;
    }
    .header-banner:hover {
      filter: grayscale(0);
    }
    ", light_blue)
  } else {
    ".header-banner { max-height: 50px; width: auto; object-fit: contain; }"
  }

  manual_style <- glue::glue(
    "
    @import url('https://fonts.googleapis.com/css2?family={{font_url_name}}:wght@300;400;700&display=swap');
    
    body, h1, h2, h3, h4, h5, h6, .header-title, .well, select, input, button, table, .nav-tabs {
      font-family: '{{font_family}}', sans-serif !important;
    }
    
    /* Header Banner Integrated */
    {{banner_css}}
 
    /* Global Body and Text */
    body {
      background-color: {{content_bg}} !important;
      color: {{body_text_color}} !important;
    }
    
    /* Sidebar Emulation */
    .well {
      background-color: {{dark_bg}} !important;
      color: {{sidebar_text_color}} !important;
      border: 1px solid {{panel_bg}} !important;
    }
    
    /* Header Panel */
    .header-panel {
      background-color: {{light_blue}} !important;
      color: {{header_text_color}} !important;
      border-bottom: 3px solid {{light_blue}} !important;
    }
    
    /* Sidebar Panels */
    .well div[style*='background-color'] {
      background-color: {{panel_bg}} !important;
      color: {{sidebar_text_color}} !important;
      border-color: {{light_blue}} !important;
    }
    
    /* Custom Boxes */
    .custom-box {
      background-color: {{box_bg}} !important;
      color: {{body_text_color}} !important;
      padding: 15px;
      border-radius: 5px;
      border-left: 5px solid {{light_blue}};
      box-shadow: 0 1px 1px rgba(0,0,0,0.1);
      margin-bottom: 15px;
    }
    
    /* Table styling for visibility */
    table, .table, th, td {
      color: {{body_text_color}} !important;
    }
    
    /* Dashboard Tabs Readability */
    .nav-tabs > li > a {
      color: {{body_text_color}} !important;
      font-weight: bold !important;
      opacity: 0.7;
    }
    .nav-tabs > li.active > a {
      color: {{body_text_color}} !important;
      background-color: {{box_bg}} !important;
      opacity: 1 !important;
      border-bottom-color: transparent !important;
    }
    .nav-tabs > li > a:hover {
      opacity: 1;
      background-color: rgba(255,255,255,0.1) !important;
    }
    
    /* Documentation Drawer */
    .docs-drawer {
      position: fixed;
      right: -600px;
      top: 0;
      width: 600px;
      height: 100%;
      background-color: {{box_bg}} !important;
      color: {{body_text_color}} !important;
      z-index: 1050;
      transition: right 0.3s ease;
      box-shadow: -2px 0 5px rgba(0,0,0,0.2);
      overflow-y: auto;
      padding: 20px;
      border-left: 2px solid {{light_blue}} !important;
    }
    .docs-drawer .nav-tabs > li.active > a {
      background-color: {{panel_bg}} !important;
    }
    
    /* Map Processing Overlay */
    .map-processing-overlay {
      position: absolute;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      min-height: 750px;
      background-color: {{box_bg}} !important;
      color: {{body_text_color}} !important;
      z-index: 2000;
      display: none;
      align-items: center;
      justify-content: center;
      flex-direction: column;
      border-radius: 8px;
      box-shadow: 0 4px 15px rgba(0,0,0,0.05);
      transition: all 0.3s ease;
    }
    
    /* Premium Spinner */
    .premium-spinner {
      width: 60px;
      height: 60px;
      border: 5px solid rgba(0, 0, 0, 0.05);
      border-top: 5px solid {{light_blue}};
      border-radius: 50%;
      animation: premium-spin 1.2s cubic-bezier(0.5, 0, 0.5, 1) infinite;
      margin-bottom: 20px;
    }
    
    @keyframes premium-spin {
      0% { transform: rotate(0deg); }
      100% { transform: rotate(360deg); }
    }
    
    /* Premium Progress Bar */
    .premium-progress-bar-container {
      width: 60%;
      max-width: 500px;
      background-color: rgba(0,0,0,0.05);
      height: 12px;
      border-radius: 6px;
      overflow: hidden;
      margin-bottom: 25px;
      border: 1px solid rgba(0,0,0,0.05);
      position: relative;
    }
    
    .premium-progress-bar-inner {
      width: 5%;
      height: 100%;
      background-color: {{light_blue}};
      transition: width 0.4s cubic-bezier(0.4, 0, 0.2, 1);
    }
    
    /* Popover inherit colors */
    .popover {
      background-color: {{box_bg}} !important;
      color: {{body_text_color}} !important;
      border: 1px solid {{light_blue}} !important;
    }
    .popover-header {
      background-color: {{panel_bg}} !important;
      color: {{body_text_color}} !important;
      border-bottom: 1px solid {{light_blue}} !important;
    }
    .popover-body {
      color: {{body_text_color}} !important;
    }
    .popover.right > .arrow:after {
      border-right-color: {{box_bg}} !important;
    }
    ",
    .open = "{{",
    .close = "}}"
  )
  
  list(
    theme = theme,
    manual_style = manual_style,
    map_tiles = map_tiles
  )
}

# --- Data-Driven Theme Configurations ---
themes_params <- list(
  "Deep Forest" = list(
    light_blue = "#2d5a27",
    dark_bg = "#22252a",
    content_bg = "#e9ecef",
    font_family = "Inter",
    map_tiles = "Esri.WorldImagery",
    sidebar_text_color = "#f0f0f0",
    body_text_color = "#2d5a27",
    header_text_color = "#ffffff"
  ),
  "Obsidian Night" = list(
    light_blue = "#00d2ff",
    dark_bg = "#121212",
    content_bg = "#1e1e1e",
    font_family = "Roboto Mono",
    map_tiles = "CartoDB.DarkMatter",
    box_bg = "#2d2d2d",
    sidebar_text_color = "#f0f0f0",
    body_text_color = "#d4d4d4",
    header_text_color = "#000000",
    banner_style = "standard"
  ),
  "Terra Cotta" = list(
    light_blue = "#e2725b",
    dark_bg = "#3e2723",
    content_bg = "#f5f5dc",
    font_family = "Playfair Display",
    map_tiles = "Esri.WorldImagery",
    sidebar_text_color = "#fdf5e6",
    body_text_color = "#2c3e50",
    header_text_color = "#ffffff"
  ),
  "Arctic Mineral" = list(
    light_blue = "#007acc",
    dark_bg = "#e3f2fd",
    content_bg = "#ffffff",
    font_family = "Montserrat",
    map_tiles = "CartoDB.Positron",
    sidebar_text_color = "#0d47a1",
    body_text_color = "#333333",
    header_text_color = "#ffffff",
    banner_style = "standard"
  ),
  "Midnight Neon" = list(
    light_blue = "#ff007f",
    dark_bg = "#1a0033",
    content_bg = "#2b0052",
    font_family = "Fira Code",
    map_tiles = "CartoDB.DarkMatter",
    box_bg = "#3c0073",
    sidebar_text_color = "#f0f0f0",
    body_text_color = "#2b0052",
    header_text_color = "#ffffff"
  ),
  "Muted Sage" = list(
    light_blue = "#7c9885",
    dark_bg = "#4b534d",
    content_bg = "#f4f6f4",
    font_family = "Lato",
    map_tiles = "CartoDB.Positron",
    sidebar_text_color = "#f8f9fa",
    body_text_color = "#2c3e50",
    header_text_color = "#ffffff"
  ),
  "Slate & Gold" = list(
    light_blue = "#d4af37",
    dark_bg = "#2c3e50",
    content_bg = "#34495e",
    font_family = "Source Sans Pro",
    map_tiles = "CartoDB.DarkMatter",
    box_bg = "#455a64",
    sidebar_text_color = "#ecf0f1",
    body_text_color = "#3e2723",
    header_text_color = "#000000"
  ),
  "Oceanic Deep" = list(
    light_blue = "#008080",
    dark_bg = "#001f3f",
    content_bg = "#e0f7fa",
    font_family = "Open Sans",
    map_tiles = "CartoDB.DarkMatter",
    sidebar_text_color = "#f0f8ff",
    body_text_color = "#002b36",
    header_text_color = "#ffffff"
  ),
  "Sandstone" = list(
    light_blue = "#d2a679",
    dark_bg = "#3e2723",
    content_bg = "#fdf5e6",
    font_family = "Arvo",
    map_tiles = "Esri.WorldImagery",
    sidebar_text_color = "#fffaf0",
    body_text_color = "#3e2723",
    header_text_color = "#3e2723"
  ),
  "Cyberpunk" = list(
    light_blue = "#f3ec18",
    dark_bg = "#0d0d0d",
    content_bg = "#1a1a1a",
    font_family = "Orbitron",
    map_tiles = "CartoDB.DarkMatter",
    box_bg = "#262626",
    sidebar_text_color = "#00ff00",
    body_text_color = "#00ff00",
    header_text_color = "#000000",
    banner_style = "standard"
  )
)

app_themes <- lapply(themes_params, function(params) {
  do.call(create_app_theme, params)
})

# UI Module for Theme Switcher
theme_switcher_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyWidgets::dropdownButton(
      shiny::selectInput(
        inputId = ns("theme_selector"),
        label = "Select App Theme",
        choices = names(app_themes),
        selected = "Muted Sage" # Default fallback
      ),
      circle = TRUE, status = "primary", icon = shiny::icon("paint-brush"), width = "300px",
      tooltip = shinyWidgets::tooltipOptions(title = "Click to change theme")
    ),
    shinyjs::useShinyjs(),
    shiny::tags$script(shiny::HTML(sprintf(
      "
      $(document).on('shiny:connected', function(event) {
        var saved_theme = localStorage.getItem('app_selected_theme');
        if (saved_theme) {
          Shiny.setInputValue('%s', saved_theme);
        } else {
          Shiny.setInputValue('%s', 'Muted Sage');
        }
      });
      ",
      ns("saved_theme_js"), ns("saved_theme_js")
    )))
  )
}

# Server Module for Theme Switcher
theme_switcher_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Reactive value to store the currently active theme
    active_theme <- shiny::reactiveVal("Muted Sage")
    
    # Observe the localStorage theme on connection
    shiny::observeEvent(input$saved_theme_js, {
      req(input$saved_theme_js)
      if (input$saved_theme_js %in% names(app_themes)) {
        shiny::updateSelectInput(session, "theme_selector", selected = input$saved_theme_js)
        active_theme(input$saved_theme_js)
      }
    }, ignoreInit = FALSE, once = TRUE)
    
    # Observe changes from the UI dropdown and save to localStorage
    shiny::observeEvent(input$theme_selector, {
      req(input$theme_selector)
      if (input$theme_selector != active_theme()) {
        active_theme(input$theme_selector)
        # Save to local storage
        shinyjs::runjs(sprintf("localStorage.setItem('app_selected_theme', '%s');", input$theme_selector))
      }
    }, ignoreInit = TRUE)
    
    return(active_theme)
  })
}
