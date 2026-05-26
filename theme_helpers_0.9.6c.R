# theme_helpers_0.9.6c.R - Dynamic Theme Helpers


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

  manual_style <- sprintf(
    "
    @import url('https://fonts.googleapis.com/css2?family=%s:wght@300;400;700&display=swap');
    
    body, h1, h2, h3, h4, h5, h6, .header-title, .well, select, input, button, table, .nav-tabs {
      font-family: '%s', sans-serif !important;
    }
    
    /* Header Banner Integrated */
    %s
 
    /* Global Body and Text */
    body {
      background-color: %s !important;
      color: %s !important;
    }
    
    /* Sidebar Emulation */
    .well {
      background-color: %s !important;
      color: %s !important;
      border: 1px solid %s !important;
    }
    
    /* Header Panel */
    .header-panel {
      background-color: %s !important;
      color: %s !important;
      border-bottom: 3px solid %s !important;
    }
    
    /* Sidebar Panels */
    .well div[style*='background-color'] {
      background-color: %s !important;
      color: %s !important;
      border-color: %s !important;
    }
    
    /* Custom Boxes */
    .custom-box {
      background-color: %s !important;
      color: %s !important;
      padding: 15px;
      border-radius: 5px;
      border-left: 5px solid %s;
      box-shadow: 0 1px 1px rgba(0,0,0,0.1);
      margin-bottom: 15px;
    }
    
    /* Table styling for visibility */
    table, .table, th, td {
      color: %s !important;
    }
    
    /* Dashboard Tabs Readability */
    .nav-tabs > li > a {
      color: %s !important;
      font-weight: bold !important;
      opacity: 0.7;
    }
    .nav-tabs > li.active > a {
      color: %s !important;
      background-color: %s !important;
      opacity: 1 !important;
      border-bottom-color: transparent !important;
    }
    .nav-tabs > li > a:hover {
      opacity: 1;
      background-color: rgba(255,255,255,0.1) !important;
    }
    
    /* Documentation Drawer */
    .docs-drawer {
      background-color: %s !important;
      color: %s !important;
      border-left: 2px solid %s !important;
    }
    .docs-drawer .nav-tabs > li.active > a {
      background-color: %s !important;
    }
    
    /* Popover inherit colors */
    .popover {
      background-color: %s !important;
      color: %s !important;
      border: 1px solid %s !important;
    }
    .popover-header {
      background-color: %s !important;
      color: %s !important;
      border-bottom: 1px solid %s !important;
    }
    .popover-body {
      color: %s !important;
    }
    .popover.right > .arrow:after {
      border-right-color: %s !important;
    }
    ",
    font_url_name, font_family, 
    banner_css,
    content_bg, body_text_color,
    dark_bg, sidebar_text_color, panel_bg,
    light_blue, header_text_color, light_blue,
    panel_bg, sidebar_text_color, light_blue,
    box_bg, body_text_color, light_blue,
    body_text_color,
    body_text_color,
    body_text_color, box_bg,
    # Drawer args
    box_bg, body_text_color, light_blue,
    panel_bg,
    # Popover args
    box_bg, body_text_color, light_blue,
    panel_bg, body_text_color, light_blue,
    body_text_color, box_bg
  )
  
  list(
    theme = theme,
    manual_style = manual_style,
    map_tiles = map_tiles
  )
}

app_themes <- list(
  "Deep Forest" = create_app_theme(
    light_blue = "#2d5a27",
    dark_bg = "#22252a",
    content_bg = "#e9ecef",
    font_family = "Inter",
    map_tiles = "Esri.WorldImagery",
    sidebar_text_color = "#f0f0f0",
    body_text_color = "#2d5a27",
    header_text_color = "#ffffff"
  ),
  "Obsidian Night" = create_app_theme(
    light_blue = "#00d2ff",
    dark_bg = "#121212",
    content_bg = "#1e1e1e",
    font_family = "Roboto Mono",
    map_tiles = "CartoDB.DarkMatter",
    box_bg = "#2d2d2d",
    sidebar_text_color = "#f0f0f0",
    body_text_color = "#4b534d",
    header_text_color = "#000000", # High contrast on cyan
    banner_style = "standard"
  ),
  "Terra Cotta" = create_app_theme(
    light_blue = "#e2725b",
    dark_bg = "#3e2723",
    content_bg = "#f5f5dc",
    font_family = "Playfair Display",
    map_tiles = "Esri.WorldImagery",
    sidebar_text_color = "#fdf5e6",
    body_text_color = "#2c3e50",
    header_text_color = "#ffffff"
  ),
  "Arctic Mineral" = create_app_theme(
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
  "Midnight Neon" = create_app_theme(
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
  "Muted Sage" = create_app_theme(
    light_blue = "#7c9885",
    dark_bg = "#4b534d",
    content_bg = "#f4f6f4",
    font_family = "Lato",
    map_tiles = "CartoDB.Positron",
    sidebar_text_color = "#f8f9fa",
    body_text_color = "#2c3e50",
    header_text_color = "#ffffff"
  ),
  "Slate & Gold" = create_app_theme(
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
  "Oceanic Deep" = create_app_theme(
    light_blue = "#008080",
    dark_bg = "#001f3f",
    content_bg = "#e0f7fa",
    font_family = "Open Sans",
    map_tiles = "CartoDB.DarkMatter",
    sidebar_text_color = "#f0f8ff",
    body_text_color = "#002b36",
    header_text_color = "#ffffff"
  ),
  "Sandstone" = create_app_theme(
    light_blue = "#d2a679",
    dark_bg = "#3e2723",
    content_bg = "#fdf5e6",
    font_family = "Arvo",
    map_tiles = "Esri.WorldImagery",
    sidebar_text_color = "#fffaf0",
    body_text_color = "#3e2723",
    header_text_color = "#3e2723"
  ),
  "Cyberpunk" = create_app_theme(
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
