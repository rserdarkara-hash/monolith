# server_run_config.R (sourced with local = TRUE inside server) - display/run
# context resolvers (get_current_meta / get_display_meta), docs drawer,
# classification params, config persistence, palette + selector UIs.
  get_current_meta <- function() {
    var <- input$var_id
    if (is.null(var) || var == "" || is.null(rv$mapping$vars)) return(NULL)
    
    idx <- which(sapply(rv$mapping$vars, function(x) x$actual == var))
    if (length(idx) == 0) return(NULL)
    m <- rv$mapping$vars[[idx]]
    
    pal <- "YlOrRd"
    if (!is.null(input$palette_select) && input$palette_select != "") {
      pal <- input$palette_select
    } else if (!is.null(m$palette) && m$palette != "") {
      pal <- m$palette
    }
    
    pred_col <- if(is_valid_col_ref(m$pred)) as.character(m$pred) else NULL
    pred_ss_col <- if(is_valid_col_ref(m$pred_ss)) as.character(m$pred_ss) else NULL

    view_col <- switch(input$value_type,
      "actual" = as.character(m$actual),
      "pred"   = pred_col,
      "pred_ss"= pred_ss_col,
      "resid"  = as.character(m$actual)
    )

    list(
      actual = as.character(m$actual),
      pred = pred_col,
      pred_ss = pred_ss_col,
      view_col = view_col,
      label = as.character(m$label %||% m$actual),
      palette = as.character(pal),
      unit = as.character(m$unit %||% "")
    )
  }

  # Display-side twin of get_current_meta(): returns the context committed at
  # run dispatch (rv$disp) instead of reading the live sidebar inputs, so the
  # Map Viewer and Scientific Analysis tabs keep describing the run that is
  # actually on screen while the sidebar is reconfigured for the next run.
  # Only the colour palette stays live - styling may be changed on the
  # displayed map at any time. Returns NULL before the first run.
  get_display_meta <- function() {
    d <- rv$disp
    if (is.null(d)) return(NULL)
    if (isTruthy(input$palette_select)) d$palette <- as.character(input$palette_select)
    d
  }

  # Display-name resolver for the Scientific Analysis tab: honours the tab's
  # "Variable naming" radio without touching the committed rv$disp snapshot
  # (the Map Viewer keeps using the metadata label regardless of this toggle).
  sci_disp_label <- function(meta = get_display_meta()) {
    if (is.null(meta)) return(NULL)
    if (identical(input$sci_name_mode, "colname")) meta$var_id %||% meta$actual else meta$label
  }
  # Metadata handed to Scientific Analysis name lookups: NULL in column-name
  # mode so get_var_label()/rk_coef_table() fall back to raw column names.
  sci_vars_meta <- function() {
    if (identical(input$sci_name_mode, "colname")) NULL else rv$mapping$vars
  }

  observeEvent(list(rv$mapping$vars, input$var_category), {
    req(rv$mapping$vars)
    vars <- rv$mapping$vars
    cats <- unique(sapply(vars, function(x) x$category))

    current_cat <- input$var_category
    sel_cat <- if(!is.null(current_cat) && current_cat %in% cats) current_cat else cats[1]
    updateSelectInput(session, "var_category", choices = cats, selected = sel_cat)
    
    filtered <- Filter(function(x) x$category == sel_cat, vars)
    choices <- setNames(sapply(filtered, function(x) x$actual), sapply(filtered, function(x) x$label))
    shinyWidgets::updatePickerInput(session, "var_id", choices = choices)
  })

  # Guard: only offer ML prediction/residual views when the selected variable
  # actually has the corresponding prediction column in the uploaded data.
  observeEvent(list(input$var_id, rv$mapping$vars), {
    var <- input$var_id
    if (is.null(var) || var == "" || is.null(rv$mapping$vars)) return(NULL)
    idx <- which(sapply(rv$mapping$vars, function(x) x$actual == var))
    if (length(idx) == 0) return(NULL)
    m <- rv$mapping$vars[[idx[1]]]

    has_col <- is_valid_col_ref
    choices <- c("Actual Values" = "actual")
    if (has_col(m$pred)) choices <- c(choices, "Best ML Predictions (_cve)" = "pred")
    if (has_col(m$pred_ss)) choices <- c(choices, "Single Split ML Predictions (_ss)" = "pred_ss")
    if (has_col(m$pred)) choices <- c(choices, "Residuals (v - pv) of ML Predictions" = "resid")

    sel <- if (isTruthy(input$value_type) && input$value_type %in% choices) input$value_type else "actual"
    updateSelectInput(session, "value_type", choices = choices, selected = sel)

    # No prediction columns at all: Comparison Mode has nothing to compare,
    # so clear it even though its (hidden) checkbox keeps its last state.
    if (!has_col(m$pred) && !has_col(m$pred_ss) && isTRUE(input$comp_mode)) {
      updateCheckboxInput(session, "comp_mode", value = FALSE)
    }
  })

  # Open/close via the .open class (not inline right) so the drawer's CSS -
  # including the floating nav buttons and the outside-click closer - keys on
  # a single source of truth.
  observeEvent(input$info_btn, {
    shinyjs::runjs("document.getElementById('docs_drawer').classList.add('open');")
  })

  observeEvent(input$close_docs_btn, {
    shinyjs::runjs("document.getElementById('docs_drawer').classList.remove('open');")
  })

  observeEvent(input$about_btn, {
    showModal(modalDialog(
      title = "About Monolith",
      size = "m",
      easyClose = TRUE,
      footer = modalButton("Close"),
      div(style = "text-align: center; padding: 20px;",
          img(src = "assets/banner.png", style = "max-width: 100%; height: auto; margin-bottom: 20px;"),
          h4("Workbench for statistics and optimized mapping in life sciences."),
          p("Integrated geostatistical modeling, classification and statistical interpretation."),
          hr(),
          p("Designed for high-performance parallel processing and spatial diagnostics, multi-scale interpolation via kriging, inverse distance weighting, and thin plate splines with practical multi-criteria optimization."),
          p("Supported with the Descriptive and Exploratory Suite with dynamic visualizations and statistics."),
          hr(),
          p(strong("A product of `that` couple of months following the loss of institutional e-mail address.")),
          p(style = "color: #666; font-size: 0.9em;", "  by Recep Serdar Kara in cooperation with Antigravity CLI and Claude Code - 2026 (v1.0.5"),
          hr(),
          tags$details(
            tags$summary(style = "cursor: pointer; color: #007bff;", "Session Info (reproducibility)"),
            tags$pre(style = "text-align: left; font-size: 0.72em; max-height: 250px; overflow-y: auto; margin-top: 8px;",
                     paste(utils::capture.output(utils::sessionInfo()), collapse = "\n"))
          ),
          downloadButton("download_session_info", "Download session info (.txt)", class = "btn-sm btn-default", style = "margin-top: 8px;")
      )
    ))
  })

  output$download_session_info <- downloadHandler(
    filename = function() { paste0("monolith_session_info_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt") },
    content = function(file) {
      writeLines(c(
        paste0("Monolith session info: generated ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        "",
        utils::capture.output(utils::sessionInfo())
      ), file)
    }
  )
  
  output$render_user_guide <- renderUI({
    withMathJax(HTML(commonmark::markdown_html(paste(readLines("docs/user_guide.md", warn = FALSE), collapse = "\n"))))
  })
  
  output$render_desc_exploratory_guide <- renderUI({
    withMathJax(HTML(commonmark::markdown_html(paste(readLines("docs/desc_exploratory_guide.md", warn = FALSE), collapse = "\n"))))
  })
  
  output$render_scientific_guide <- renderUI({
    withMathJax(HTML(commonmark::markdown_html(paste(readLines("docs/scientific_guide.md", warn = FALSE), collapse = "\n"))))
  })
  
  # Unwrapped display-layer values of the merged run rasters, cached per run.
  # rv$rast / rv$rast_pred are PackedSpatRasters (they crossed the future
  # boundary) and Packed rasters are NOT subsettable, so every consumer must
  # go through raster_value_layer() - and caching here means the unwrap +
  # values() read happens once per run instead of on every styling tick.
  rast_vals_act <- reactive({ raster_value_layer(rv$rast) })
  rast_vals_pre <- reactive({ raster_value_layer(rv$rast_pred) })

  # ── Agronomical styling commit flow ─────────────────────────────────────
  # Agro sub-settings (algorithm, class count, supervised limits) are edited
  # freely and only take effect when APPLY TO MAPS & STATS is pressed: every
  # input tick used to trigger a raster re-encode of all visible map layers
  # plus the class-area / kappa recomputes, so the controls felt frozen while
  # the maps caught up. Continuous and Binned styling stay immediate - they
  # have no sub-settings to stage.
  gather_agro_limits <- function(n_c) {
    sapply(seq_len(n_c - 1), function(i) {
      val <- input[[paste0("agro_limit_", i)]]
      if (is.null(val) || is.na(val)) i * 10 else val
    })
  }
  # Comparable one-line signature of an agro settings list (identical() is too
  # strict across integer/double input round-trips).
  agro_signature <- function(s) {
    if (is.null(s)) return("")
    paste(s$method, s$n_classes,
          paste(signif(as.numeric(s$limits %||% numeric(0)), 10), collapse = ","))
  }
  gather_agro_live <- function() {
    n_c <- input$agro_n_classes
    if (!isTruthy(n_c)) return(NULL)
    m <- input$agro_method %||% "limits"
    list(method = m, n_classes = as.integer(n_c),
         limits = if (identical(m, "limits")) as.numeric(gather_agro_limits(n_c)) else NULL)
  }
  agro_applied <- reactiveVal(NULL)
  observeEvent(input$agro_apply, {
    live <- gather_agro_live()
    req(live)
    agro_applied(live)
  })

  output$agro_pending_note <- renderUI({
    req(input$color_style == "agro")
    live <- gather_agro_live()
    req(live)
    ap <- agro_applied()
    if (identical(agro_signature(ap), agro_signature(live))) return(NULL)
    msg <- if (is.null(ap)) {
      "Class settings are staged: press APPLY TO MAPS & STATS to classify the displayed maps and statistics."
    } else {
      "Class settings changed: press APPLY TO MAPS & STATS to reflect them on the maps and statistics."
    }
    div(style = "background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; padding: 6px 8px; margin-bottom: 6px; color: #664d03; font-size: 0.82em;",
        icon("triangle-exclamation"), msg,
        tags$div(style = "margin-top: 4px; font-style: italic;",
          "Applying re-encodes every visible map layer and recomputes class areas - expect a few seconds, longer for multi-locality or comparison views."))
  })

  joint_vv <- reactive({
    is_uncertainty <- isTruthy(input$show_uncertainty) && isTruthy((rv$disp$method %||% "") %in% c("OK", "RK", "RFK", "CK"))
    get_joint_scale_values(rv$rast, rv$rast_pred, input$match_scales, is_uncertainty)
  })

  # Values the class breaks are computed on: joint scale when Match Scales is
  # on, else the displayed surface, else (pre-run) the raw data column.
  classification_values <- function(meta, n_min) {
    vv <- joint_vv()
    if (is.null(vv)) {
      vv <- if (identical(input$map_view, "view_pred") || identical(input$map_view, "view_comp")) rast_vals_pre() else rast_vals_act()
    }
    if (is.null(vv)) {
      v_data <- rv$user_data[[meta$actual]]
      if (!is.null(v_data)) vv <- v_data[is.finite(v_data)]
    }
    if (is.null(vv) || length(vv) < n_min) return(NULL)
    vv
  }

  classification_params <- reactive({
    req(input$color_style %in% c("agro", "bin"))
    # Displayed run's variable when one exists (class breaks must describe the
    # map on screen); live selection as pre-run fallback so the styling
    # controls stay usable before the first interpolation.
    meta <- get_display_meta()
    if (is.null(meta)) meta <- get_current_meta()
    req(meta)

    if (input$color_style == "agro") {
      # Committed snapshot only: live agro inputs never reach the maps/stats
      # until APPLY is pressed (the pending-note UI flags the divergence).
      ap <- agro_applied()
      if (is.null(ap)) return(NULL)
      n_c <- ap$n_classes

      if(ap$method == "limits") {
        brks_inner <- ap$limits
        if (is.null(brks_inner)) return(NULL)
      } else {
        vv <- classification_values(meta, n_c)
        if (is.null(vv)) return(NULL)
        # Seeded (and, for Jenks, subsampled) break computation: classInt's
        # jenks is O(n^2)-slow on raster-sized vectors and both jenks and
        # kmeans draw unseeded random numbers internally.
        brks_inner <- calc_class_breaks(vv, n_c, ap$method)
        if (is.null(brks_inner)) return(NULL)
      }

      brks <- sort(unique(c(-Inf, brks_inner, Inf)))
      n_c_actual <- length(brks) - 1

      rcl_mat <- matrix(NA, nrow = n_c_actual, ncol = 3)
      for(i in 1:n_c_actual) {
        rcl_mat[i, ] <- c(brks[i], brks[i+1], i)
      }

      colors <- get_agro_colors(n_c_actual)
      labels <- if(n_c_actual==3) c("Low", "Med", "High") else paste("Class", 1:n_c_actual)

      leg_labels <- character(n_c_actual)
      for(i in 1:n_c_actual) {
        if(i==1) leg_labels[i] <- paste("<", round(brks[2], 3))
        else if(i==n_c_actual) leg_labels[i] <- paste(">", round(brks[n_c_actual], 3))
        else leg_labels[i] <- paste(round(brks[i],3), "-", round(brks[i+1],3))
      }
      if(n_c_actual == 3) leg_labels <- paste(labels, ":", leg_labels)

      list(brks = brks, rcl_mat = rcl_mat, colors = colors, labels = labels, leg_labels = leg_labels, n_c = n_c_actual)

    } else {
      n_c <- 5
      vv <- classification_values(meta, n_c)
      if (is.null(vv)) return(NULL)

      rng <- range(vv, na.rm = TRUE)
      if(is.infinite(rng[1]) || is.infinite(rng[2]) || rng[1] == rng[2]) {
        brks_inner <- seq(rng[1], rng[1] + 1, length.out = n_c + 1)[2:n_c]
      } else {
        brks_inner <- seq(rng[1], rng[2], length.out = n_c + 1)[2:n_c]
      }
      
      brks <- sort(unique(c(-Inf, brks_inner, Inf)))
      n_c_actual <- length(brks) - 1
      
      rcl_mat <- matrix(NA, nrow = n_c_actual, ncol = 3)
      for(i in 1:n_c_actual) {
        rcl_mat[i, ] <- c(brks[i], brks[i+1], i)
      }
      
      is_viridis <- meta$palette == "viridis"
      colors <- if(is_viridis) {
        viridis::viridis(n_c_actual, option = meta$palette)
      } else {
        colorRampPalette(RColorBrewer::brewer.pal(min(8, max(3, n_c_actual)), meta$palette))(n_c_actual)
      }
      
      labels <- paste("Bin", 1:n_c_actual)
      
      leg_labels <- character(n_c_actual)
      for(i in 1:n_c_actual) {
        if(i==1) leg_labels[i] <- paste("<", round(brks[2], 3))
        else if(i==n_c_actual) leg_labels[i] <- paste(">", round(brks[n_c_actual], 3))
        else leg_labels[i] <- paste(round(brks[i],3), "-", round(brks[i+1],3))
      }
      
      list(brks = brks, rcl_mat = rcl_mat, colors = colors, labels = labels, leg_labels = leg_labels, n_c = n_c_actual)
    }
  })

  agro_params <- reactive({
    req(input$color_style == "agro")
    classification_params()
  })

  volumes <- c(Home = fs::path_home(), Project = getwd())
  shinyFileChoose(input, "load_config", roots = volumes, session = session, filetypes = c("json"))

  
  observeEvent(input$save_config, {
    showModal(modalDialog(
      title = "Save Session Configuration",
      size = "m",
      easyClose = TRUE,
      footer = modalButton("Cancel"),
      div(style = "padding: 10px;",
          h4("Export active parameters to a local JSON file:"),
          p("This configuration file saves active coordinate column pairings, variable lists, category associations, custom color palettes, and active spatial interpolation engines. You can load this file back in a future session."),
          hr(),
          div(style = "text-align: center; margin-top: 20px;",
              downloadButton("download_config_json", "DOWNLOAD CONFIGURATION FILE", class = "btn-success btn-lg")
          )
      )
    ))
  })

  output$download_config_json <- downloadHandler(
    filename = function() {
      paste0("monolith_config_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
    },
    content = function(file) {
      config_list <- list(
        map_x = input$map_x,
        map_y = input$map_y,
        map_loc = input$map_loc,
        map_crs = input$map_crs,
        crs_selection = input$crs_selection,
        var_category = input$var_category,
        var_id = input$var_id,
        value_type = input$value_type,
        method = input$method,
        boundary_type = input$boundary_type,
        buff_mode = input$buff_mode,
        buff_dist = input$buff_dist,
        res_mode = input$res_mode,
        grid_res = input$grid_res,
        color_style = input$color_style,
        vars_mapping = rv$mapping$vars
      )
      writeLines(jsonlite::toJSON(config_list, auto_unbox = TRUE, pretty = TRUE), file)
    }
  )

  observeEvent(input$load_config, {
    req(input$load_config)
    file_info <- shinyFiles::parseFilePaths(volumes, input$load_config)
    req(nrow(file_info) > 0)
    config_path <- file_info$datapath[1]
    
    tryCatch({
      cfg <- jsonlite::fromJSON(config_path, simplifyVector = FALSE)
      
      if (!is.null(cfg$vars_mapping)) {
        rv$mapping$vars <- cfg$vars_mapping
      }
      
      if (!is.null(cfg$map_x)) updateSelectInput(session, "map_x", selected = cfg$map_x)
      if (!is.null(cfg$map_y)) updateSelectInput(session, "map_y", selected = cfg$map_y)
      if (!is.null(cfg$map_loc)) updateSelectInput(session, "map_loc", selected = cfg$map_loc)
      if (!is.null(cfg$map_crs)) updateSelectizeInput(session, "map_crs", selected = cfg$map_crs)
      if (!is.null(cfg$crs_selection)) updateSelectizeInput(session, "crs_selection", selected = cfg$crs_selection)
      
      if (!is.null(cfg$var_category)) updateSelectInput(session, "var_category", selected = cfg$var_category)
      if (!is.null(cfg$var_id)) shinyWidgets::updatePickerInput(session, "var_id", selected = cfg$var_id)
      if (!is.null(cfg$value_type)) updateSelectInput(session, "value_type", selected = cfg$value_type)
      
      if (!is.null(cfg$method)) updateSelectInput(session, "method", selected = cfg$method)
      if (!is.null(cfg$boundary_type)) updateSelectInput(session, "boundary_type", selected = cfg$boundary_type)
      if (!is.null(cfg$buff_mode)) shinyWidgets::updateRadioGroupButtons(session, "buff_mode", selected = cfg$buff_mode)
      if (!is.null(cfg$buff_dist)) updateNumericInput(session, "buff_dist", value = cfg$buff_dist)
      if (!is.null(cfg$res_mode)) shinyWidgets::updateRadioGroupButtons(session, "res_mode", selected = cfg$res_mode)
      if (!is.null(cfg$grid_res)) updateSliderInput(session, "grid_res", value = cfg$grid_res)
      if (!is.null(cfg$color_style)) updateSelectInput(session, "color_style", selected = cfg$color_style)
      
      showNotification("Configuration loaded successfully!", type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste("Failed to load configuration:", e$message), type = "error", duration = 7)
    })
  })

  output$palette_ui <- renderUI({
    # Follow the displayed run's variable once one exists so a context change
    # cannot silently reset the palette of the map on screen (input$var_id is
    # only a reactive dependency before the first run).
    vid <- if (!is.null(rv$disp)) rv$disp$var_id else input$var_id
    req(vid, rv$mapping$vars)
    # Agronomical styling supplies its own class palette, so the manual
    # colour-palette picker is irrelevant there — hide it.
    if (isTruthy(input$color_style) && input$color_style == "agro") return(NULL)
    idx <- which(sapply(rv$mapping$vars, function(x) x$actual == vid))
    if (length(idx) == 0) return(NULL)
    m <- rv$mapping$vars[[idx]]
    choices <- palette_choices_precomputed
    pickerInput("palette_select", "Color Palette", 
                choices = choices, 
                selected = m$palette %||% "YlOrRd",
                options = list(`live-search` = TRUE),
                choicesOpt = list(content = names(choices)))
  })


  # The range slider parameterizes a variogram fitted in the analysis CRS
  # (metric), so its scale must be measured there; the raw x/y columns may be
  # in degrees. The projection is cached against (data, mapping, CRS) so the
  # slider-bounds observer below does not re-transform every coordinate each
  # time a run finishes or the diagnostics locality changes.
  projected_max_dist <- reactive({
    req(rv$user_data)
    x_col <- rv$mapping$x
    y_col <- rv$mapping$y
    req(x_col, y_col, x_col %in% colnames(rv$user_data), y_col %in% colnames(rv$user_data))
    coords_df <- rv$user_data[, c(x_col, y_col)]
    coords_df <- coords_df[complete.cases(coords_df), , drop = FALSE]
    tryCatch({
      pts_proj <- st_transform(st_as_sf(coords_df, coords = c(x_col, y_col), crs = rv$mapping$crs), input$crs_selection)
      bb <- st_bbox(pts_proj)
      as.numeric(sqrt((bb["xmax"] - bb["xmin"])^2 + (bb["ymax"] - bb["ymin"])^2))
    }, error = function(e) NA_real_)
  })

  observeEvent(list(input$var_id, input$m_loc, input$crs_selection, rv$user_data, rv$v_fit_list), {
    req(rv$user_data, input$var_id, rv$mapping$vars)
    meta <- get_current_meta()
    req(meta)

    col_name <- meta$actual
    v_data <- rv$user_data[[col_name]]
    if (!is.null(v_data) && is.numeric(v_data) && length(na.omit(v_data)) >= 3) {
      variance <- var(v_data, na.rm = TRUE)
      if (!is.na(variance) && variance > 0) {
        max_sill <- round(variance * 2, 2)
        step_val <- round(variance / 100, 4)
        if(step_val == 0) step_val <- 0.01

        max_dist <- tryCatch(projected_max_dist(), error = function(e) NA_real_)
        if (!is.null(max_dist) && !is.na(max_dist) && max_dist > 0) {
          max_range <- round(max_dist * 1.5, 0)
          step_range <- round(max_range / 100, 0)
          if(step_range == 0) step_range <- 1

          fit <- rv$v_fit_list[[paste0(input$m_loc %||% "global", "_act")]]
          if (is.null(fit)) {
            updateSliderInput(session, "m_nugget", min = 0, max = max_sill, value = 0, step = step_val)
            updateSliderInput(session, "m_psill", min = 0, max = max_sill, value = round(variance, 2), step = step_val)
            updateSliderInput(session, "m_range", min = 1, max = max_range, value = round(max_dist / 4, 0), step = step_range)
          }
        }
      }
    }
  })

  output$agro_options <- renderUI({
    req(input$color_style == "agro", input$agro_method == "limits")
    # input$var_id is only a reactive dependency before the first run;
    # afterwards the limits follow the displayed variable and are not reset
    # by sidebar context changes.
    vid <- if (!is.null(rv$disp)) rv$disp$var_id else input$var_id
    req(vid)
    nut <- get_nut_key(vid)
    def_limits <- if(!is.null(nut) && nut %in% names(nutrient_limits)) nutrient_limits[[nut]] else NULL

    # Range hint of the surface the limits will cut (cached per run) so
    # sensible thresholds can be typed without leaving the sidebar.
    rng_note <- tryCatch({
      vv <- classification_values(get_display_meta() %||% get_current_meta(), 2)
      if (is.null(vv)) NULL else {
        rng <- range(vv, na.rm = TRUE)
        tags$small(style = "display:block; color:#888; margin-bottom:4px;",
                   sprintf("Displayed surface range: %.3g - %.3g", rng[1], rng[2]))
      }
    }, error = function(e) NULL)

    tagList(
      rng_note,
      lapply(1:(input$agro_n_classes - 1), function(i) {
        val <- if(!is.null(def_limits) && i <= length(def_limits)) def_limits[i] else i * 10
        numericInput(paste0("agro_limit_", i), paste("Limit", i), value = val)
      })
    )
  })

  output$locality_selector_ui <- renderUI({
      req(rv$loc_names); selectInput("sel_loc_stats", "Filter Analysis View:", choices = c("Total (Combined)", rv$loc_names))
    })
  
  output$covariate_selector_ui <- renderUI({
    req(rv$user_data, input$var_id)
    cols <- colnames(rv$user_data)
    num_cols <- cols[sapply(rv$user_data, is.numeric)]
    exclude <- c(input$map_x, input$map_y, input$var_id)
    raw_choices <- num_cols[!(num_cols %in% exclude)]
    
    vars_metadata <- rv$mapping$vars
    choices_named <- setNames(raw_choices, sapply(raw_choices, function(v) {
      match <- Filter(function(x) x$actual == v, vars_metadata)
      if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") {
        match[[1]]$label
      } else {
        v
      }
    }))
    
    pickerInput("aux_vars", "Select Predictors:", 
                choices = choices_named, multiple = TRUE, 
                options = list(`live-search` = TRUE, `actions-box` = TRUE))
  })

      observeEvent(input$calc_corr, {
        req(rv$user_data, input$var_id)
        # Scope the correlations to the Context panel's locality selection:
        # ranks computed across ALL samples can be driven by between-locality
        # contrasts and mislead covariate choice for a single-locality run.
        df_base <- rv$user_data
        scope_lbl <- "all localities"
        if (!is.null(rv$mapping$loc) && rv$mapping$loc %in% colnames(df_base) &&
            length(input$locality) > 0 && !("ALL" %in% input$locality)) {
          df_base <- df_base[as.character(df_base[[rv$mapping$loc]]) %in% input$locality, , drop = FALSE]
          scope_lbl <- paste(input$locality, collapse = ", ")
        }
        if (nrow(df_base) < 3) {
          showNotification("Fewer than 3 samples in the selected localities - cannot rank predictors.", type = "error")
          return()
        }
        df <- df_base[sapply(df_base, is.numeric)]
        df <- df[, !(colnames(df) %in% c(rv$mapping$x, rv$mapping$y))]

        target <- input$var_id
        if(!(target %in% colnames(df))) {
          showNotification("Target variable not in numeric data.", type = "error")
          return()
        }
        
        res_list <- lapply(setdiff(colnames(df), target), function(v) {
          test <- tryCatch(cor.test(df[[target]], df[[v]], use = "pairwise.complete.obs"), error = function(e) NULL)
          if(!is.null(test)) {
             data.frame(Variable = v, Corr = test$estimate, Pval = test$p.value, stringsAsFactors = FALSE)
          } else {
             NULL
          }
        })
        res_df <- do.call(rbind, Filter(Negate(is.null), res_list))
        
        if(is.null(res_df) || nrow(res_df) == 0) {
          showNotification("Could not calculate correlations. Ensure numeric data is available.", type = "error")
          return()
        }
        
        rv$full_cor_matrix <- res_df # Re-using variable name but storing dataframe instead of matrix
        rv$cor_scope_label <- sprintf("%s, n = %d", scope_lbl, nrow(df_base))
        rv$show_corr_panel <- TRUE
      })
    
      output$corr_results_ui <- renderUI({
        req(rv$show_corr_panel, rv$full_cor_matrix, input$var_id)
        res_df <- rv$full_cor_matrix
        target <- input$var_id
        
        thresh <- as.numeric(input$corr_pval_thresh %||% 1)
        if(thresh < 1) {
           res_df <- res_df[!is.na(res_df$Pval) & res_df$Pval <= thresh, ]
        }
        
        if(nrow(res_df) == 0) return(tags$p("No variables meet the significance threshold."))
        
        vars_metadata <- rv$mapping$vars
        var_to_cat <- sapply(res_df$Variable, function(v) {
          match <- Filter(function(x) x$actual == v, vars_metadata)
          if(length(match) > 0) match[[1]]$category else "Uploaded Data"
        })

        var_to_label <- sapply(res_df$Variable, function(v) {
          match <- Filter(function(x) x$actual == v, vars_metadata)
          if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") {
            match[[1]]$label
          } else {
            v # Fallback to column name
          }
        })
        
        res_df$Category <- var_to_cat
        res_df$Label <- var_to_label
        res_df$AbsCorr <- abs(res_df$Corr)

        cats <- unique(res_df$Category)

        tabs <- list()

        results_all <- res_df[order(res_df$AbsCorr, decreasing = TRUE), ]
        res_all <- head(results_all, 8)

        tabs[[1]] <- tabPanel("All",
          tags$ul(style="font-size: 0.85em; padding-left: 15px; margin-top: 5px; list-style-type: none;",
            lapply(seq_len(nrow(res_all)), function(i) {
              tags$li(sprintf("%s: %.3f (p=%.3f)", res_all$Label[i], res_all$Corr[i], res_all$Pval[i]))
            })
          )
        )

        cat_dfs <- split(res_df, res_df$Category)
        cat_tabs <- lapply(names(cat_dfs), function(cat) {
          results_cat <- cat_dfs[[cat]]
          results_cat <- results_cat[order(results_cat$AbsCorr, decreasing = TRUE), ]
          res_cat <- head(results_cat, 8)
          
          tabPanel(cat,
            tags$ul(style="font-size: 0.85em; padding-left: 15px; margin-top: 5px; list-style-type: none;",
              lapply(seq_len(nrow(res_cat)), function(i) {
                tags$li(sprintf("%s: %.3f (p=%.3f)", res_cat$Label[i], res_cat$Corr[i], res_cat$Pval[i]))
              })
            )
          )
        })
        
        tabs <- c(list(tabs[[1]]), cat_tabs)
        tagList(
          hr(),
          tags$h6("Predictor Ranks (Correlation):"),
          # Scope stamp: ranks are frozen at Calculate time, so make the data
          # subset they refer to explicit even if the locality selection has
          # changed since (press Calculate again to refresh).
          if (!is.null(rv$cor_scope_label))
            tags$p(style = "font-size: 0.78em; color: #888; margin: 0 0 4px 0;",
                   paste0("Scope: ", rv$cor_scope_label)),
          tags$div(style = "overflow-x: auto; white-space: nowrap; border-bottom: 1px solid #ddd; margin-bottom: 5px;",
            do.call(tabsetPanel, c(list(id = "cor_tabs", type = "pills"), tabs))
          )
        )
      })
    
