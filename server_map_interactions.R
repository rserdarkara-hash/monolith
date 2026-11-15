# server_map_interactions.R (chunk C, sourced with local = TRUE inside server):
# draw handlers, locality assignment, regional params, popups, point styling
# and header status chips.
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

  # One handler set per map: a newly drawn polygon or rectangle is kept in
  # rv$drawn_polygons for map exports and opens the assign-locality modal (a
  # marker encloses no point, so it stays an annotation). Keys are prefixed
  # with the map id; leaflet ids are only unique per map. A click on a sampling
  # point opens its pop-up.
  for (.map_id in overlay_map_ids) local({
    map_id <- .map_id
    poly_key <- function(feat) paste0(map_id, "_", feat$properties$`_leaflet_id`)
    observeEvent(input[[paste0(map_id, "_draw_new_feature")]], {
      feat <- input[[paste0(map_id, "_draw_new_feature")]]
      if (!isTRUE(feat$geometry$type %in% c("Polygon", "MultiPolygon"))) return()
      rv$drawn_polygons[[poly_key(feat)]] <- feat
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
    # A point's pop-up, built for that point when it is clicked: a styled
    # sampling point, or a point of the Residuals view's point residuals.
    # pts_view_4326() (chunk H) is looked up when the click arrives.
    observeEvent(input[[paste0(map_id, "_marker_click")]], {
      ev <- input[[paste0(map_id, "_marker_click")]]
      prefix <- switch(ev$group %||% "", styled_points = POINT_LAYER_ID_PREFIX,
                       resid_points = RESID_POINT_ID_PREFIX, NULL)
      id <- as.character(ev$id %||% "")
      if (is.null(prefix) || !startsWith(id, prefix)) return(invisible(NULL))
      pts <- pts_view_4326()
      i <- suppressWarnings(as.integer(substring(id, nchar(prefix) + 1L)))
      if (is.null(pts) || !isTRUE(i >= 1L && i <= nrow(pts))) return(invisible(NULL))
      # A click sent before a rebuild names a marker of the previous point set.
      xy <- sf::st_coordinates(pts)[i, ]
      near <- function(a, b) isTRUE(abs(a - b) < 1e-9)
      if (!near(xy[["X"]], ev$lng) || !near(xy[["Y"]], ev$lat)) return(invisible(NULL))
      row <- sf::st_drop_geometry(pts)[i, , drop = FALSE]
      leafletProxy(map_id) %>%
        leaflet::addPopups(ev$lng, ev$lat, point_popup_html(row, popup_groups()), layerId = POINT_POPUP_ID)
    })
  })

  observeEvent(input$save_group, {
    req(rv$drawn_feature, input$new_group_name, rv$user_data, rv$mapping$x, rv$mapping$y, rv$mapping$crs)
    group_name <- trimws(input$new_group_name)
    if (group_name == "") {
      showNotification("Group name cannot be empty.", type = "error")
      return()
    }

    # The uploaded rows with a position (a text cell in a coordinate column is
    # none) that the shape covers, tested in the Input Data CRS. An error here
    # would end the session, so a shape or CRS that cannot be compared is
    # reported instead.
    xs <- coord_values(rv$user_data[[rv$mapping$x]])
    ys <- coord_values(rv$user_data[[rv$mapping$y]])
    located <- which(is.finite(xs) & is.finite(ys))
    inside <- tryCatch({
      poly_sf <- sf::st_read(jsonlite::toJSON(rv$drawn_feature, auto_unbox = TRUE), quiet = TRUE)
      sf::st_crs(poly_sf) <- 4326
      pts_sf <- sf::st_as_sf(data.frame(x = xs[located], y = ys[located]),
                             coords = c("x", "y"), crs = rv$mapping$crs)
      located[lengths(sf::st_intersects(pts_sf, sf::st_transform(poly_sf, sf::st_crs(pts_sf)))) > 0]
    }, error = function(e) {
      showNotification(paste("The shape could not be compared with the sample positions:", conditionMessage(e)),
                       type = "error", duration = 10)
      NULL
    })

    if (length(inside)) {
      if (!"Assigned_Locality" %in% names(rv$user_data)) {
        rv$user_data$Assigned_Locality <- NA
      }
      rv$user_data$Assigned_Locality[inside] <- group_name
      showNotification(paste("Assigned", length(inside), "points to group:", group_name), type = "message")
      updateSelectInput(session, "map_loc", choices = colnames(rv$user_data), selected = input$map_loc)
    } else if (!is.null(inside)) {
      showNotification("No points found within the selected area.", type = "warning")
    }

    removeModal()
    rv$drawn_feature <- NULL
  })
  
  # Stored entries are list(value, key): a value is used only for the tuning
  # key (column + effective subset) it was tuned or applied for.
  get_regional_param <- function(type, loc, target, default = NULL, key) {
    field <- if(type == "IDW") "idw_factors" else "tps_lambdas"
    if (is.null(default)) default <- if (type == "IDW") 2.0 else -1.0
    resolve_regional_param(rv[[field]][[loc]][[target]], key, default)
  }

  set_regional_param <- function(type, loc, target, value, key) {
    field <- if(type == "IDW") "idw_factors" else "tps_lambdas"
    entry <- list(value = value, key = key)
    if(is.null(rv[[field]][[loc]])) {
      params <- list()
      params[[target]] <- entry
      rv[[field]][[loc]] <- params
    } else {
      rv[[field]][[loc]][[target]] <- entry
    }
  }

  # The variable rows of a point's pop-up, by category, as Pop-up Content sets.
  popup_groups <- reactive({
    group_vars_by_category(popup_variables(rv$mapping$vars %||% list(), input$pt_popup_content %||% "auto",
                                           input$pt_popup_vars, focus_var()))
  })

  # Pop-up Content and its variable chooser for the variable list `vars`.
  # `fresh` (a new table) starts from the mapped variable's category; otherwise
  # a choice is kept while the list still offers it.
  sync_popup_inputs <- function(vars, fresh = FALSE) {
    choices <- popup_content_choices(vars)
    content <- if (fresh) "auto" else isolate(input$pt_popup_content)
    updateSelectInput(session, "pt_popup_content", choices = choices,
                      selected = if (isTRUE(content %in% unlist(choices))) content else "auto")
    var_choices <- lapply(group_vars_by_category(vars), function(g) {
      stats::setNames(var_actuals(g), vapply(g, function(x) as.character(x$label %||% x$actual), character(1)))
    })
    chosen <- if (fresh) character(0) else intersect(as.character(isolate(input$pt_popup_vars)), var_actuals(vars))
    shinyWidgets::updatePickerInput(session, "pt_popup_vars", choices = var_choices, selected = chosen)
  }
  # They follow the variable list. An upload announces a new table through
  # session_state$popup_fresh (server_data_setup.R).
  observeEvent(rv$mapping$vars, {
    fresh <- isTRUE(session_state$popup_fresh)
    session_state$popup_fresh <- FALSE
    sync_popup_inputs(rv$mapping$vars %||% list(), fresh)
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

  # Each group's colour for Color By: the palette's, with the custom colours
  # chosen for this column and palette on top. NULL for "none".
  pt_group_colors <- reactive({
    by <- input$pt_color_by %||% "none"
    ud <- rv$user_data
    if (identical(by, "none") || is.null(ud) || !by %in% colnames(ud)) return(NULL)
    pal_name <- input$pt_palette %||% "Set1"
    cols <- generate_group_palette(sort(unique(point_group_values(ud[[by]]))), pal_name)
    over <- rv$pt_color_overrides[[by]][[pal_name]]
    hit <- intersect(names(over), names(cols))
    cols[hit] <- over[hit]
    cols
  })

  observeEvent(input$pt_custom_colors, {
    cols <- pt_group_colors(); req(cols)
    groups <- names(cols)

    color_inputs <- lapply(seq_along(groups), function(i) {
      g <- groups[i]
      col_hex <- cols[[i]]
      div(style = "display: flex; align-items: center; gap: 10px; margin-bottom: 8px;",
        div(style = paste0("width: 16px; height: 16px; border-radius: 3px; background-color: ", col_hex, "; border: 1px solid var(--mn-line-2); flex-shrink: 0;")),
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

  # Custom colours are kept per column and palette (nested lists, so a single
  # group keeps its name), and come back when Color By and Palette return to
  # them. Only a colour changed here is kept; the other groups follow the palette.
  observeEvent(input$pt_apply_custom_colors, {
    cols <- pt_group_colors(); req(cols)
    by <- input$pt_color_by; pal_name <- input$pt_palette %||% "Set1"
    all_over <- rv$pt_color_overrides %||% list()
    per_col <- all_over[[by]] %||% list()
    over <- per_col[[pal_name]] %||% character(0)
    for (i in seq_along(cols)) {
      col_val <- input[[paste0("pt_grp_col_", i)]]
      if (!is.null(col_val) && grepl("^#[0-9A-Fa-f]{6}$", col_val) && !identical(col_val, cols[[i]])) {
        over[names(cols)[i]] <- col_val
      }
    }
    if (length(over)) {
      per_col[[pal_name]] <- over
      all_over[[by]] <- per_col
      rv$pt_color_overrides <- all_over
    }
    removeModal()
    showNotification("Custom colors applied.", type = "message", duration = 3)
  })

  output$file_uploaded <- reactive({ !is.null(input$user_file) })
  outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
  
  output$model_ready <- reactive({ if(length(rv$rast_list_act) > 0 || length(rv$v_emp_list) > 0) "yes" else "no" })
  outputOptions(output, "model_ready", suspendWhenHidden = FALSE)

  # Compact run-status chip in the header, visible from any tab: amber while a
  # run executes (per-locality progress-file average), green once the
  # displayed run's results exist, grey before the first run and after a
  # cancelled one.
  output$run_status_chip <- renderUI({
    if (isTRUE(rv$model_running)) {
      pct <- rv$run_pct
      span(class = "run-status-chip", span(class = "dot running"),
           if (!is.null(pct)) paste0("Running · ", pct, "%") else "Running…")
    } else if (identical(rv$run_config_summary$status, "cancelled")) {
      span(class = "run-status-chip", span(class = "dot idle"),
           paste0("Cancelled · Run #", rv$run_config_summary$run_id))
    } else if (length(rv$rast_list_act) > 0 || length(rv$v_emp_list) > 0) {
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
  # Which diagnostic panel family Tab 3 shows: the displayed run's method, or
  # before any run the OK "Data Structure" panels for any kriging selection,
  # where pre-run auto-fit and manual tuning work.
  output$sci_diag_method <- reactive({
    d_method <- rv$disp$method %||% ""
    if (nzchar(d_method)) return(d_method)
    if ((input$method %||% "") %in% c("OK", "RK", "RFK", "CK")) "OK" else ""
  })
  outputOptions(output, "sci_diag_method", suspendWhenHidden = FALSE)

  # Variogram tuning context: TRUE in Manual mode or after an OPTIMIZE ALL
  # VARIOGRAMS that no later run superseded. The tab then shows the curves
  # those controls move, whatever engine produced the run on screen.
  sci_vgm_tuning <- reactive({
    (input$method %||% "") %in% c("OK", "RK", "RFK", "CK") &&
      (identical(input$vgm_mode, "manual") || isTRUE(rv$vgm_preview))
  })
  output$sci_vgm_tuning <- reactive({ if (isTRUE(sci_vgm_tuning())) "yes" else "no" })
  outputOptions(output, "sci_vgm_tuning", suspendWhenHidden = FALSE)

  # While tuning, the cards of a run from a variogram-free engine are held back
  # (not discarded): they describe a model unrelated to the curves shown.
  sci_stale_run <- reactive({
    isTRUE(sci_vgm_tuning()) && (rv$disp$method %||% "") %in% c("IDW", "TPS")
  })
  output$sci_stale_run <- reactive({ if (isTRUE(sci_stale_run())) "yes" else "no" })
  outputOptions(output, "sci_stale_run", suspendWhenHidden = FALSE)

  observe({
    shinyjs::toggleClass("sci_stack", "vgm-first", condition = isTRUE(sci_vgm_tuning()))
    shinyjs::toggleClass("sci_stack", "vgm-only", condition = isTRUE(sci_stale_run()))
  })
  # Whether the displayed run has a prediction side (configured or produced);
  # shared with chunk I's directional-variogram switch so the rule exists once.
  disp_has_pred <- reactive({
    d <- rv$disp
    active <- !is.null(d) && (isTRUE(d$comp_mode) || !identical(d$value_type, "actual"))
    active || isTRUE(rv$has_predictions)
  })
  output$disp_has_pred <- reactive({
    if (isTRUE(disp_has_pred())) "yes" else "no"
  })
  outputOptions(output, "disp_has_pred", suspendWhenHidden = FALSE)
  
