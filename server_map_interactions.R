# server_map_interactions.R (sourced with local = TRUE inside server) - draw
# handlers, locality assignment, regional params, popups, point styling and
# header status chips.
  handle_new_feature <- function(feature) {
    rv$drawn_feature <- feature
    showModal(modalDialog(
      title = "Assign Locality / Analysis Group",
      textInput("new_group_name", "Group Name:", placeholder = "Enter name (e.g. Zone A)"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_group", "Save Group", class = "btn-primary")
      ),
      easyClose = FALSE
    ))
  }

  # One handler set per map: a newly drawn shape opens the assign-locality
  # modal, and polygons are also kept in rv$drawn_polygons for map exports.
  # Keys are prefixed with the map id; leaflet ids are only unique per map.
  for (.map_id in c("main_map", "comp_map_left", "comp_map_right")) local({
    map_id <- .map_id
    poly_key <- function(feat) paste0(map_id, "_", feat$properties$`_leaflet_id`)
    observeEvent(input[[paste0(map_id, "_draw_new_feature")]], {
      feat <- input[[paste0(map_id, "_draw_new_feature")]]
      if (feat$geometry$type %in% c("Polygon", "MultiPolygon")) rv$drawn_polygons[[poly_key(feat)]] <- feat
      handle_new_feature(feat)
    })
    observeEvent(input[[paste0(map_id, "_draw_edited_features")]], {
      for (feat in input[[paste0(map_id, "_draw_edited_features")]]$features) {
        if (feat$geometry$type %in% c("Polygon", "MultiPolygon")) rv$drawn_polygons[[poly_key(feat)]] <- feat
      }
    })
    observeEvent(input[[paste0(map_id, "_draw_deleted_features")]], {
      for (feat in input[[paste0(map_id, "_draw_deleted_features")]]$features) {
        rv$drawn_polygons[[poly_key(feat)]] <- NULL
      }
    })
  })

  observeEvent(input$save_group, {
    req(rv$drawn_feature, input$new_group_name, rv$user_data, rv$mapping$x, rv$mapping$y, rv$mapping$crs)
    group_name <- trimws(input$new_group_name)
    if (group_name == "") {
      showNotification("Group name cannot be empty.", type = "error")
      return()
    }

    feature <- rv$drawn_feature
    feat_json <- jsonlite::toJSON(feature, auto_unbox = TRUE)
    poly_sf <- sf::st_read(feat_json, quiet = TRUE)
    sf::st_crs(poly_sf) <- 4326

    df_map <- rv$user_data %>% 
      filter(!is.na(!!sym(rv$mapping$x)) & !is.na(!!sym(rv$mapping$y)))
    
    pts_sf <- sf::st_as_sf(df_map, coords = c(rv$mapping$x, rv$mapping$y), crs = rv$mapping$crs)
    poly_sf_trans <- sf::st_transform(poly_sf, sf::st_crs(pts_sf))
    
    intersect_idx <- sf::st_intersects(pts_sf, poly_sf_trans, sparse = FALSE)
    
    in_poly <- rowSums(intersect_idx) > 0
    
    if (any(in_poly)) {
      if (!"Assigned_Locality" %in% names(rv$user_data)) {
        rv$user_data$Assigned_Locality <- NA
      }
      
      valid_rows <- which(!is.na(rv$user_data[[rv$mapping$x]]) & !is.na(rv$user_data[[rv$mapping$y]]))
      intersecting_user_rows <- valid_rows[in_poly]
      
      rv$user_data$Assigned_Locality[intersecting_user_rows] <- group_name
      
      showNotification(paste("Assigned", length(intersecting_user_rows), "points to group:", group_name), type = "message")
      
      current_loc <- input$map_loc
      updateSelectInput(session, "map_loc", choices = colnames(rv$user_data), selected = current_loc)
      
      rv$user_data <- rv$user_data 
    } else {
      showNotification("No points found within the selected area.", type = "warning")
    }
    
    removeModal()
    rv$drawn_feature <- NULL
  })
  
  get_regional_param <- function(type, loc, target, default = NULL) {
    field <- if(type == "IDW") "idw_factors" else "tps_lambdas"
    val <- rv[[field]][[loc]][[target]]
    if(is.null(val)) {
      if(!is.null(default)) return(default)
      if(type == "IDW") return(2.0)
      if(type == "TPS") return(-1.0)
    }
    return(val)
  }
  
  set_regional_param <- function(type, loc, target, value) {
    field <- if(type == "IDW") "idw_factors" else "tps_lambdas"
    if(is.null(rv[[field]][[loc]])) {
      params <- list()
      params[[target]] <- value
      rv[[field]][[loc]] <- params
    } else {
      rv[[field]][[loc]][[target]] <- value
    }
  }

  popup_metadata_cache <- reactive({
    req(rv$mapping$vars)
    meta_list <- rv$mapping$vars
    
    vars_to_show <- rv$pop_up_vars
    if(is.null(vars_to_show) || length(vars_to_show) == 0) {
      soil_vars <- Filter(function(x) grepl("Soil|Physicochem", x$category, ignore.case = TRUE), meta_list)
      if(length(soil_vars) > 0) {
        vars_to_show <- sapply(soil_vars, function(x) x$actual)
      } else {
        vars_to_show <- sapply(meta_list, function(x) x$actual)
      }
    }
    
    all_cats <- unique(sapply(meta_list, function(x) x$category))
    priority_cats <- all_cats[grepl("Soil|Physicochem", all_cats, ignore.case = TRUE)]
    other_cats <- setdiff(all_cats, priority_cats)
    cats <- c(priority_cats, other_cats)
    
    grouped_vars <- list()
    for(cat in cats) {
      cat_vars <- Filter(function(x) x$category == cat && x$actual %in% vars_to_show, meta_list)
      if(length(cat_vars) > 0) {
        grouped_vars[[cat]] <- cat_vars
      }
    }
    
    meta_actuals <- sapply(meta_list, function(x) x$actual)
    
    list(
      vars_to_show = vars_to_show,
      grouped_vars = grouped_vars,
      meta_actuals = meta_actuals
    )
  })

  # Resolve each popup variable to its data column once per point set, so
  # generate_popup does a plain lookup per point instead of up to four regex
  # searches per variable per point. Returns a popup_fn(data_row) closure.
  make_popup_fn <- function(cnames) {
    resolve_col <- function(key) {
      key <- as.character(key)
      if (key %in% cnames) return(key)
      idx <- grep(paste0("^", key, "$"), cnames, ignore.case = TRUE)
      if (length(idx) > 0) return(cnames[idx[1]])
      idx <- grep(paste0(key, "$"), cnames, ignore.case = TRUE)
      if (length(idx) > 0) return(cnames[idx[1]])
      idx <- grep(key, cnames, ignore.case = TRUE)
      if (length(idx) > 0) return(cnames[idx[1]])
      NA_character_
    }
    cache <- popup_metadata_cache()
    keys <- unique(c(
      unlist(lapply(cache$grouped_vars, function(cvs) vapply(cvs, function(v) as.character(v$actual), character(1)))),
      as.character(setdiff(cache$vars_to_show, cache$meta_actuals))
    ))
    col_map <- vapply(keys, resolve_col, character(1))
    function(data_row) generate_popup(data_row, col_map = col_map)
  }

  generate_popup <- function(data_row, col_map = NULL) {
    data_row <- as.list(data_row)
    names_in_row <- names(data_row)

    find_val <- function(key) {
      key <- as.character(key)
      if (!is.null(col_map) && key %in% names(col_map)) {
        col <- col_map[[key]]
        if (is.na(col)) return(NULL)
        return(data_row[[col]])
      }
      if (key %in% names_in_row) return(data_row[[key]])
      idx <- grep(paste0("^", key, "$"), names_in_row, ignore.case = TRUE)
      if (length(idx) > 0) return(data_row[[idx[1]]])
      idx <- grep(paste0(key, "$"), names_in_row, ignore.case = TRUE)
      if (length(idx) > 0) return(data_row[[idx[1]]])
      idx <- grep(key, names_in_row, ignore.case = TRUE)
      if (length(idx) > 0) return(data_row[[idx[1]]])
      return(NULL)
    }

    cache <- popup_metadata_cache()
    vars_to_show <- cache$vars_to_show
    grouped_vars <- cache$grouped_vars
    meta_actuals <- cache$meta_actuals
    
    html_content <- "<div style='max-height: 300px; overflow-y: auto; font-family: sans-serif; min-width: 200px;'>"
    html_content <- paste0(html_content, "<h4>Point Details</h4><table style='width: 100%; border-collapse: collapse;'>")
    
    for(cat in names(grouped_vars)) {
      cat_vars <- grouped_vars[[cat]]
      html_content <- paste0(html_content, "<tr style='background-color: var(--mn-surface-2);'><td colspan='2'><b>", cat, "</b></td></tr>")
      for(v in cat_vars) {
        val <- find_val(as.character(v$actual))
        val_str <- if(!is.null(val) && (is.numeric(val) || !is.na(suppressWarnings(as.numeric(val))))) round(as.numeric(val), 3) else as.character(val %||% "N/A")
        html_content <- paste0(html_content, "<tr><td style='padding: 3px;'>", v$label, "</td><td style='padding: 3px; text-align: right;'>", val_str, "</td></tr>")
      }
    }
    
    other_vars <- setdiff(vars_to_show, meta_actuals)
    if(length(other_vars) > 0) {
      html_content <- paste0(html_content, "<tr style='background-color: var(--mn-surface-2);'><td colspan='2'><b>Other Variables</b></td></tr>")
      for(ov in other_vars) {
        val <- find_val(as.character(ov))
        val_str <- if(!is.null(val) && (is.numeric(val) || !is.na(suppressWarnings(as.numeric(val))))) round(as.numeric(val), 3) else as.character(val %||% "N/A")
        html_content <- paste0(html_content, "<tr><td style='padding: 3px;'>", ov, "</td><td style='padding: 3px; text-align: right;'>", val_str, "</td></tr>")
      }
    }
    
    html_content <- paste0(html_content, "</table></div>")
    return(html_content)
  }

  observeEvent(input$show_popup_settings, {
    req(rv$mapping$vars)
    vars_list <- rv$mapping$vars
    cats <- unique(sapply(vars_list, function(x) x$category))
    choices <- list()
    for(cat in cats) {
      cat_vars <- Filter(function(x) x$category == cat, vars_list)
      choices[[cat]] <- setNames(sapply(cat_vars, function(x) x$actual), sapply(cat_vars, function(x) x$label))
    }
    
    default_selected <- rv$pop_up_vars
    if (is.null(default_selected)) {
      soil_vars <- Filter(function(x) grepl("Soil|Physicochem", x$category, ignore.case = TRUE), vars_list)
      if (length(soil_vars) > 0) {
        default_selected <- sapply(soil_vars, function(x) x$actual)
      } else {
        default_selected <- sapply(vars_list, function(x) x$actual)
      }
    }
    
    showModal(modalDialog(
      title = "Sampling Point Pop-up Settings",
      pickerInput("popup_var_select", "Select Variables to Display in Pop-ups:", 
                  choices = choices, 
                  selected = default_selected, 
                  multiple = TRUE, 
                  options = list(`actions-box` = TRUE, `live-search` = TRUE)),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_popups", "Apply Settings", class = "btn-primary")
      )
    ))
  })

  observeEvent(input$confirm_popups, {
    rv$pop_up_vars <- input$popup_var_select
    removeModal()
    showNotification("Pop-up settings updated.", type = "message")
  })

  observeEvent(input$toggle_pt_style, {
    shinyjs::toggle("pt_style_toolbar", anim = TRUE, animType = "slide", time = 0.3)
  })

  observeEvent(list(rv$user_data, rv$mapping$vars, rv$mapping$loc, rv$mapping$x, rv$mapping$y), {
    req(rv$user_data)
    df <- rv$user_data
    cols <- colnames(df)

    vars_meta <- rv$mapping$vars

    cat_cols <- cols[sapply(df, function(x) {
      is.character(x) || is.factor(x) || (is.integer(x) && length(unique(x)) <= 20)
    })]

    color_choices <- c("None (Cyan)" = "none")
    if (!is.null(rv$mapping$loc) && rv$mapping$loc %in% cols) {
      color_choices <- c(color_choices, stats::setNames(rv$mapping$loc, paste0("Locality (", rv$mapping$loc, ")")))
    }
    other_cats <- setdiff(cat_cols, c(rv$mapping$loc, rv$mapping$x, rv$mapping$y))
    if (length(other_cats) > 0) {
      color_choices <- c(color_choices, stats::setNames(other_cats, other_cats))
    }
    
    curr_color_by <- isolate(input$pt_color_by)
    selected_color_by <- if (!is.null(curr_color_by) && curr_color_by %in% color_choices) curr_color_by else "none"
    updateSelectInput(session, "pt_color_by", choices = color_choices, selected = selected_color_by)

    col_labels <- sapply(cols, function(c) {
      get_var_label(c, vars_meta)
    })
    label_choices <- c("(none)" = "none", stats::setNames(cols, col_labels))
    
    curr_label_field <- isolate(input$pt_label_field)
    selected_label_field <- if (!is.null(curr_label_field) && curr_label_field %in% label_choices) curr_label_field else "none"
    updateSelectInput(session, "pt_label_field", choices = label_choices, selected = selected_label_field)
  })

  observeEvent(list(input$pt_color_by, input$pt_palette), {
    req(input$pt_color_by)
    if (input$pt_color_by == "none") {
      rv$pt_style_colors <- NULL
      return()
    }
    req(rv$user_data, input$pt_color_by %in% colnames(rv$user_data))
    groups <- sort(unique(as.character(rv$user_data[[input$pt_color_by]])))
    pal_name <- input$pt_palette %||% "Set1"
    rv$pt_style_colors <- generate_group_palette(groups, pal_name)
  }, ignoreInit = TRUE)

  observeEvent(input$pt_custom_colors, {
    req(rv$pt_style_colors)
    groups <- names(rv$pt_style_colors)

    color_inputs <- lapply(seq_along(groups), function(i) {
      g <- groups[i]
      col_hex <- rv$pt_style_colors[g]
      div(style = "display: flex; align-items: center; gap: 10px; margin-bottom: 8px;",
        div(style = paste0("width: 16px; height: 16px; border-radius: 3px; background-color: ", col_hex, "; border: 1px solid #ccc; flex-shrink: 0;")),
        tags$span(g, style = "width: 120px; font-size: 13px; font-weight: 500; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; flex-shrink: 0;"),
        div(class = "custom-color-row-input", style = "width: 90px; flex-shrink: 0;",
          textInput(paste0("pt_grp_col_", i), NULL, value = col_hex, width = "90px")
        )
      )
    })

    showModal(modalDialog(
      title = tags$span(icon("palette"), " Custom Group Colors"),
      div(style = "max-height: 400px; overflow-y: auto; padding: 5px;",
        tags$style(HTML("
          .custom-color-row-input .form-group { margin-bottom: 0 !important; margin-top: 0 !important; }
          .custom-color-row-input .shiny-input-container { margin-bottom: 0 !important; margin-top: 0 !important; }
        ")),
        p("Enter hex color codes (e.g. #FF5733) for each group:", style = "font-size: 12px; color: var(--mn-text-3); margin-bottom: 12px;"),
        color_inputs
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("pt_apply_custom_colors", "Apply Colors", class = "btn-primary", icon = icon("check"))
      ),
      size = "s"
    ))
  })

  observeEvent(input$pt_apply_custom_colors, {
    req(rv$pt_style_colors)
    groups <- names(rv$pt_style_colors)
    for (i in seq_along(groups)) {
      col_val <- input[[paste0("pt_grp_col_", i)]]
      if (!is.null(col_val) && grepl("^#[0-9A-Fa-f]{6}$", col_val)) {
        rv$pt_style_colors[groups[i]] <- col_val
      }
    }
    removeModal()
    showNotification("Custom colors applied.", type = "message", duration = 3)
  })

  output$file_uploaded <- reactive({ !is.null(input$user_file) })
  outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
  
  output$model_ready <- reactive({ if(!is.null(rv$rast) || length(rv$v_emp_list) > 0) "yes" else "no" })
  outputOptions(output, "model_ready", suspendWhenHidden = FALSE)

  # Compact run-status chip in the header, visible from any tab: amber while a
  # run executes (per-locality progress-file average), green once the
  # displayed run's results exist, grey before the first run.
  output$run_status_chip <- renderUI({
    if (isTRUE(rv$model_running)) {
      pct <- rv$run_pct
      span(class = "run-status-chip", span(class = "dot running"),
           if (!is.null(pct)) paste0("Running · ", pct, "%") else "Running…")
    } else if (!is.null(rv$rast) || length(rv$v_emp_list) > 0) {
      span(class = "run-status-chip", span(class = "dot done"),
           paste0("Run ready · ", rv$disp$method %||% ""))
    } else {
      span(class = "run-status-chip", span(class = "dot idle"), "Idle")
    }
  })

  # Committed run context for JS conditionalPanels (same pattern as
  # model_ready): the Scientific Analysis sections must describe the run on
  # screen, not the live sidebar method/value-type selections.
  output$disp_method <- reactive({ rv$disp$method %||% "" })
  outputOptions(output, "disp_method", suspendWhenHidden = FALSE)
  # Which diagnostic panel family Tab 3 shows. Post-run this is the displayed
  # run's method. Pre-run (no committed rv$disp) the variogram fitting tools
  # (OPTIMIZE ALL VARIOGRAMS / Manual tuning) operate on the plain data
  # variograms, so any kriging-family selection maps to the OK "Data
  # Structure" panels - otherwise the fallback placeholder would cover the
  # advertised auto-fit -> manual pre-run tuning workflow.
  output$sci_diag_method <- reactive({
    d_method <- rv$disp$method %||% ""
    if (nzchar(d_method)) return(d_method)
    if ((input$method %||% "") %in% c("OK", "RK", "RFK", "CK")) "OK" else ""
  })
  outputOptions(output, "sci_diag_method", suspendWhenHidden = FALSE)

  # Variogram tuning context. TRUE while the sidebar is working on kriging
  # variograms: Manual mode, or an OPTIMIZE ALL VARIOGRAMS that no later run
  # has superseded. The Scientific Analysis tab must then show the curves those
  # controls move whatever engine produced the run currently on screen -
  # without this, switching to Manual after an IDW/TPS run left the variogram
  # panels hidden behind the "Diagnostic Mode Active" placeholder.
  sci_vgm_tuning <- reactive({
    (input$method %||% "") %in% c("OK", "RK", "RFK", "CK") &&
      (identical(input$vgm_mode, "manual") || isTRUE(rv$vgm_preview))
  })
  output$sci_vgm_tuning <- reactive({ if (isTRUE(sci_vgm_tuning())) "yes" else "no" })
  outputOptions(output, "sci_vgm_tuning", suspendWhenHidden = FALSE)

  # The displayed run came from an engine that fits no variogram, so while
  # tuning is active its result cards describe a model that has nothing to do
  # with what the tab is showing. They are held back, not discarded: the run's
  # maps and exports are still valid and stay where they are.
  sci_stale_run <- reactive({
    isTRUE(sci_vgm_tuning()) && (rv$disp$method %||% "") %in% c("IDW", "TPS")
  })
  output$sci_stale_run <- reactive({ if (isTRUE(sci_stale_run())) "yes" else "no" })
  outputOptions(output, "sci_stale_run", suspendWhenHidden = FALSE)

  observe({
    shinyjs::toggleClass("sci_stack", "vgm-first", condition = isTRUE(sci_vgm_tuning()))
    shinyjs::toggleClass("sci_stack", "vgm-only", condition = isTRUE(sci_stale_run()))
  })
  output$disp_has_pred <- reactive({
    d <- rv$disp
    active <- !is.null(d) && (isTRUE(d$comp_mode) || !identical(d$value_type, "actual"))
    if (active || isTRUE(rv$has_predictions)) "yes" else "no"
  })
  outputOptions(output, "disp_has_pred", suspendWhenHidden = FALSE)
  
