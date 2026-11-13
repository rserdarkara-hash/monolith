# server_export.R (chunk B, sourced with local = TRUE inside server): export
# registry, run-config/run-history panels, WYSIWYG styler and export handlers.

  # Returns the item's registry key (safe_key() of `id`), which a caller that
  # selects the item must use.
  register_export_item <- function(id, label, type, obj, kind = "value",
                                   var_label = NULL, legend = NULL, derived = NULL,
                                   surface = NULL) {
    # `derived` items carry no payload of their own: export_item_obj()
    # (global_utils.R) rebuilds them from a raster the registry already holds.
    req(!is.null(obj) || !is.null(derived))
    clean_id <- safe_key(id)
    # The variable label that opens every registry label, so a batch workbook
    # can drop that prefix from sheet names; defaults to the displayed run's.
    if (is.null(var_label)) {
      var_label <- tryCatch(isolate(get_display_meta()$label), error = function(e) NULL)
    }

    new_item <- list(
      id = clean_id,
      label = label,
      var_label = var_label,
      type = type, # "plot", "table", "map"
      obj = obj,
      # Maps: "value" (may be class-styled), "residual" or "uncertainty" (never
      # classified: class limits are in the variable's units).
      kind = kind,
      # A map's legend title (map_legend_title): the variable and its unit,
      # as the Map Viewer's legend reads.
      legend = legend,
      # NULL, or the spec export_item_obj() rebuilds `obj` from.
      derived = derived,
      # A value map's surface ("act"/"pre"), whose own class breaks style it.
      surface = surface,
      timestamp = Sys.time()
    )
    
    current_reg <- isolate(rv$export_registry)
    current_reg[[clean_id]] <- new_item
    rv$export_registry <- current_reg
    invisible(clean_id)
  }
  
  output$export_registry_ui <- renderUI({
    req(rv$export_registry)
    reg <- rv$export_registry
    if (length(reg) == 0) {
      return(tags$p(class = "mn-empty-line", "Registry is empty. Run models to populate."))
    }

    # choiceValues stay names(reg); each label is a row (kind chip, name, time).
    checkboxGroupInput(
      "selected_assets", NULL, width = "100%",
      choiceValues = names(reg),
      choiceNames = unname(lapply(reg, function(x) {
        tags$span(
          class = "mn-asset",
          tags$span(class = "mn-asset-kind", x$type),
          tags$span(class = "mn-asset-label", x$label),
          tags$span(class = "mn-asset-time mn-num", format(x$timestamp, "%H:%M:%S"))
        )
      }))
    )
  })

  output$run_config_display <- renderUI({
    cfg <- rv$run_config_summary
    if (is.null(cfg)) return(NULL)
    cancelled <- identical(cfg$status, "cancelled")
    div(class = "mn-notice mn-notice-mono",
      tags$strong(icon("info-circle"), if (cancelled) {
        paste0(" Run #", cfg$run_id, " - CANCELLED (requested ", format(cfg$timestamp, "%Y-%m-%d %H:%M:%S"),
               ", cancelled ", format(cfg$cancelled_at, "%H:%M:%S"), ")")
      } else {
        paste0(" Run #", cfg$run_id, " Configuration (", format(cfg$timestamp, "%Y-%m-%d %H:%M:%S"), ")")
      }),
      if (cancelled) tagList(tags$br(), tags$span(RUN_CANCELLED_NOTE)),
      tags$br(),
      tags$span(paste0("Variable: ", cfg$variable, " | Method: ", cfg$method, " | Localities: ", cfg$localities)),
      tags$br(),
      tags$span(paste0("Subset: ", cfg$subset, " | View: ", cfg$value_type,
                       " | Input CRS: ", cfg$input_crs, " | Target CRS: ", cfg$target_crs)),
      if (!is.null(covariate_record_text(cfg))) tagList(tags$br(), tags$span(covariate_record_text(cfg))),
      tags$br(),
      tags$span(paste0("Boundary: ", cfg$boundary_type, " | Buffer: ", if (is.null(cfg$buffer_mode) || cfg$buffer_mode == "fixed") paste0(cfg$buffer_dist, "m") else "Dynamic", " | Resolution: ", cfg$resolution, " (", res_mode_label(cfg$res_mode), ")")),
      # Settings that change the reported numbers; method-specific ones print
      # only where they apply.
      tags$br(),
      tags$span(paste0(
        # An absent field (an archive from an older session) asserts nothing.
        if (!is.null(cfg$cv_strategy)) paste0("CV strategy: ", switch(cfg$cv_strategy,
                                                                     "loocv" = "Standard LOOCV",
                                                                     "knndm" = "kNNDM (map-matched)",
                                                                     "block" = "Spatial Block CV",
                                                                     "Auto")) else "CV strategy: not recorded",
        if (!is.null(cfg$cv_repeats) && !is.na(cfg$cv_repeats) && cfg$cv_repeats > 1) paste0(" | Repeated CV: ", cfg$cv_repeats, " fold realizations") else "",
        # What the metrics were measured on and what each fold re-estimated.
        if (!is.null(cfg$cv_population) && !is.na(cfg$cv_population)) paste0(" | CV population: ", cfg$cv_population) else "",
        if (!is.null(cfg$cv_refit) && !is.na(cfg$cv_refit)) paste0(" | Model refit: ", cfg$cv_refit) else "",
        if (!is.null(cfg$cv_covariate_screen) && !is.na(cfg$cv_covariate_screen)) paste0(" | Covariate screen: ", cfg$cv_covariate_screen) else "",
        if (!is.null(cfg$sep_fit) && !is.na(cfg$sep_fit)) paste0(" | Fit Actual/Predicted separately: ", if (isTRUE(cfg$sep_fit)) "yes" else "no") else "",
        if (!is.null(cfg$vif_threshold) && !is.na(cfg$vif_threshold)) paste0(" | Collinearity gate: ", if (is.finite(cfg$vif_threshold)) paste0("VIF > ", cfg$vif_threshold, " dropped") else "Keep all (user override)") else "",
        # Printed only when the CRS distorts distances enough to matter, so a
        # well-chosen CRS adds no noise and an overridden one leaves a trace.
        if (!is.null(cfg$crs_scale_factor) && !is.na(cfg$crs_scale_factor) &&
            abs(cfg$crs_scale_factor - 1) > 0.001)
          paste0(" | CRS scale factor: k = ", format(round(cfg$crs_scale_factor, 6), nsmall = 6),
                 if (isTRUE(cfg$crs_gate_override)) " (user override)" else "") else "",
        if (!is.null(cfg$rf_ntree) && !is.na(cfg$rf_ntree)) paste0(" | RF trees: ", cfg$rf_ntree) else "",
        if (!is.null(cfg$rfk_uncertainty) && !is.na(cfg$rfk_uncertainty)) paste0(" | RFK uncertainty: ", cfg$rfk_uncertainty) else "",
        if (!is.null(cfg$ck_nmax) && !is.na(cfg$ck_nmax)) paste0(" | CK nmax: ", cfg$ck_nmax) else ""
      )),
      if (!is.null(cfg$method_params) && nzchar(cfg$method_params)) tagList(tags$br(), tags$span(cfg$method_params))
    )
  })

  # Reproducibility record of the CURRENT run: the run-history summary, the
  # per-locality tuning consumed, and the versions of every package that
  # computes a reported number.
  output$download_run_config <- downloadHandler(
    filename = function() {
      cfg <- rv$run_config_summary
      paste0("monolith_run_", if (is.null(cfg)) "config" else cfg$run_id, "_",
             format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
    },
    content = function(file) {
      cfg <- rv$run_config_summary
      req(cfg)
      pkgs <- c("sf", "gstat", "fields", "randomForest", "terra", "spdep", "FNN",
                "Ckmeans.1d.dp", "concaveman", "units")
      pkg_versions <- setNames(
        lapply(pkgs, function(p) tryCatch(as.character(utils::packageVersion(p)),
                                          error = function(e) NA_character_)),
        pkgs
      )
      writeLines(run_record_json(run_record_payload(cfg, rv$disp$regional_params, app_version, pkg_versions)),
                 file)
    }
  )

  output$run_config_display_map <- renderUI({
    cfg <- rv$run_config_summary
    if (is.null(cfg)) return(NULL)
    cancelled <- identical(cfg$status, "cancelled")
    div(class = "mn-notice mn-notice-mono",
      tags$strong(paste0("Run #", cfg$run_id, if (cancelled) " - CANCELLED" else "", ": ")),
      tags$span(paste0(cfg$variable, " | ", cfg$method, " | ", cfg$localities, " | ", format(cfg$timestamp, "%H:%M:%S"))),
      if (cancelled) tagList(tags$br(), tags$span(RUN_CANCELLED_NOTE))
    )
  })

  output$run_history_ui <- renderUI({
    hist <- rv$run_history
    if (length(hist) == 0) return(tags$p(class = "mn-empty-line", "No archived runs yet. Previous runs will appear here when a new model generation begins."))

    run_panels <- lapply(seq_along(hist), function(i) {
      run <- hist[[i]]
      cfg <- run$config
      n_items <- length(run$registry)
      div(style = "background-color: var(--mn-surface-2); padding: 12px; border: 1px solid var(--mn-line); border-radius: 6px; margin-bottom: 8px;",
        fluidRow(
          column(8,
            tags$strong(paste0("Run #", cfg$run_id, " - ", cfg$variable, " (", cfg$method, ")")),
            tags$br(),
            tags$small(style="color: var(--mn-text-2);", paste0(
              format(cfg$timestamp, "%Y-%m-%d %H:%M:%S"),
              " | ", cfg$localities,
              " | ", n_items, " registry items"
            ))
          ),
          column(4, style = "text-align: right;",
            actionButton(paste0("restore_run_", cfg$run_id), "Restore", class = "btn-sm btn-info", icon = icon("undo")),
            actionButton(paste0("delete_run_", cfg$run_id), "Remove", class = "btn-sm btn-danger", icon = icon("trash"))
          )
        )
      )
    })
    tagList(run_panels)
  })

  env_hist <- new.env(parent = emptyenv())
  env_hist$history_obs <- list()
  observeEvent(rv$run_history, {
    hist <- rv$run_history
    lapply(env_hist$history_obs, function(o) {
      if (!is.null(o)) o$destroy()
    })
    env_hist$history_obs <- list()
    
    lapply(seq_along(hist), function(i) {
      run <- hist[[i]]
      run_id <- run$config$run_id
      
      obs_restore <- observeEvent(input[[paste0("restore_run_", run_id)]], {
        # The run in flight is about to fill every panel a restore replaces.
        if (isTRUE(rv$model_running)) {
          showNotification("A run is in progress. Restore an archived run once it has finished or been cancelled.",
                           type = "warning")
          return()
        }
        history_list <- isolate(rv$run_history)
        idx <- which(sapply(history_list, function(x) x$config$run_id) == run_id)[1]
        if (is.na(idx)) return()

        run_to_restore <- history_list[[idx]]

        # The run on screen goes into the archive through the same snapshot
        # the archive takes, or a second restore would bring back half a run.
        if (displayed_run_has_results()) {
          push_run_history(snapshot_display_state(), base = history_list[-idx])
        } else {
          rv$run_history <- history_list[-idx]
        }

        restore_display_state(run_to_restore)
        # Its warnings were announced when it ran; the log line-watcher would
        # otherwise announce them again.
        mark_log_warnings_announced()
        fit_maps_to_data()

        cfg <- run_to_restore$config
        n_loc <- length(run_to_restore$state$disp$localities)
        showNotification(sprintf("Restored Run #%s (%s, %d %s). Maps, metrics and the export registry now describe that run.",
                                 cfg$run_id, get_method_label(cfg$method), n_loc,
                                 if (n_loc == 1) "locality" else "localities"),
                         type = "message")
      }, ignoreInit = TRUE)

      # Removing an archived run cannot be undone, so it is confirmed first
      # (input$confirm_delete_run, below).
      obs_delete <- observeEvent(input[[paste0("delete_run_", run_id)]], {
        cfg <- run$config
        env_hist$pending_delete <- run_id
        showModal(modalDialog(
          title = "Remove Archived Run",
          tags$p(sprintf("Run #%s (%s, %s) will be removed from the archive, with its maps, tables and export items.",
                         cfg$run_id, cfg$variable, cfg$method)),
          tags$p("This cannot be undone."),
          footer = tagList(
            actionButton("confirm_delete_run", "Remove Run", class = "btn-danger", icon = icon("trash")),
            modalButton("Cancel")
          ),
          size = "m", easyClose = TRUE
        ))
      }, ignoreInit = TRUE)

      env_hist$history_obs[[paste0("restore_", run_id)]] <- obs_restore
      env_hist$history_obs[[paste0("delete_", run_id)]] <- obs_delete
    })
  }, ignoreNULL = FALSE)

  observeEvent(input$confirm_delete_run, {
    removeModal()
    history_list <- rv$run_history
    idx <- which(vapply(history_list, function(x) identical(x$config$run_id, env_hist$pending_delete),
                        logical(1)))[1]
    env_hist$pending_delete <- NULL
    if (is.na(idx)) return()
    rv$run_history <- history_list[-idx]
    showNotification("Archived run removed.", type = "message")
  })

  active_styler_item <- reactiveVal(NULL)
  
  observeEvent(input$selected_assets, {
    req(input$selected_assets)
    if (length(input$selected_assets) > 0) {
      active_styler_item(input$selected_assets[length(input$selected_assets)])
    }
  }, ignoreNULL = FALSE)
  
  # Legend placement follows the item's shape: a paired comparison map takes a
  # horizontal legend underneath, everything else the right-hand default.
  # Re-applied after the remembered config is replayed over the new modal.
  item_legend_defaults <- function(item) {
    combined <- identical(item$type, "map_combined")
    list(pos = if (combined) "bottom" else "right",
         dir = if (combined) "horizontal" else "auto",
         angle = if (combined) "90" else "0")
  }

  apply_item_legend_defaults <- function() {
    item <- tryCatch(rv$export_registry[[active_styler_item()]], error = function(e) NULL)
    if (is.null(item)) return(invisible(NULL))
    d <- item_legend_defaults(item)
    updateSelectInput(session, "styler_legend_pos", selected = d$pos)
    updateSelectInput(session, "styler_legend_dir", selected = d$dir)
    updateSelectInput(session, "styler_legend_text_angle", selected = d$angle)
    invisible(NULL)
  }

  # The class definition(s) an exported map is drawn with, as in the Map
  # Viewer: the shown surface's classes (both for the Actual vs Predicted
  # figure); NULL draws it continuous. Residual and uncertainty maps are never
  # classified.
  export_class_params <- function(item) {
    if (!isTRUE(input$color_style %in% c("agro", "bin"))) return(NULL)
    get <- function(s) tryCatch(classification_params(s), error = function(e) NULL)
    if (identical(item$type, "map_combined")) return(list(act = get("act"), pre = get("pre")))
    get(item$surface %||% "act")
  }

  # The palette an exported map is drawn in: the displayed variable's, as the
  # Map Viewer draws it. The registry holds the displayed run's items only.
  export_palette <- function() get_display_meta()$palette %||% "YlOrRd"

  base_preview_plot <- reactive({
    req(active_styler_item(), rv$export_registry)
    item <- rv$export_registry[[active_styler_item()]]
    req(item)
    g <- preview_geom_d()

    # A layer draws no more cells than the preview canvas has pixels, so a
    # large surface redraws quickly; the exported file draws every cell.
    generate_base_plot(
      item = item,
      input = input,
      agro_params = export_class_params(item),
      palette = export_palette(),
      max_cells = ceiling(g$w_in * g$h_in * g$res^2)
    )
  })
  
  styled_preview_obj <- reactive({
    req(active_styler_item(), rv$export_registry)
    item <- rv$export_registry[[active_styler_item()]]
    req(item)
    
    base_p <- base_preview_plot()
    req(base_p)
    
    apply_styler_theme(
      p_obj = base_p,
      input = input,
      item_label = item$label,
      item_type = item$type
    )
  })

  styled_preview_obj_d <- styled_preview_obj %>% debounce(500)

  # The preview is drawn on a canvas of the EXPORT's physical size and only
  # rasterised coarser, so its layout is the file's. The canvas is fitted to
  # the width the browser reports: a wider guess would overflow the centred
  # pane off its left edge, out of the scrollbar's reach.
  preview_geom <- reactive({
    w_in <- if (isTruthy(input$styler_width)) input$styler_width else 10
    h_in <- if (isTruthy(input$styler_height)) input$styler_height else 8
    pane_h <- 578  # the 600px pane less its 10px padding and 1px border
    avail_w <- session$clientData$output_styler_preview_plot_width
    # 0 while the output is hidden (GeoTIFF selected) or not yet bound.
    if (!isTruthy(avail_w) || avail_w < 100) avail_w <- 520
    fit <- min(1, avail_w / (w_in * 96), pane_h / (h_in * 96))
    list(w_in = w_in, h_in = h_in,
         w_disp = round(w_in * 96 * fit), h_disp = round(h_in * 96 * fit),
         # 2x supersample: same layout, crisp on high-density displays. The
         # floor keeps type renderable when a very large canvas is fitted into
         # the pane; surplus pixels are just scaled down by the browser.
         res = max(48, 96 * fit * 2))
  })
  # Debounced: a window drag streams widths.
  preview_geom_d <- preview_geom %>% debounce(300)

  # renderImage, not renderPlot: the preview goes through the very writer the
  # download uses, so the two cannot drift apart.
  output$styler_preview_plot <- renderImage({
    p <- styled_preview_obj_d()
    req(p)
    g <- preview_geom_d()
    f <- tempfile(fileext = ".png")
    export_plot_to_file(p, f, "png", input,
                        width = g$w_in, height = g$h_in, dpi = g$res)
    list(src = f, contentType = "image/png",
         width = g$w_disp, height = g$h_disp,
         # Scales into the pane even if the reported width is stale.
         style = paste0("max-width: 100%; height: auto; display: block; margin: 0 auto;",
                        " background-color: var(--mn-surface); box-shadow: var(--mn-shadow);"),
         alt = "Preview of the figure as it will be exported")
  }, deleteFile = TRUE)
  
  observeEvent(input$select_all_assets, {
    req(rv$export_registry)
    updateCheckboxGroupInput(session, "selected_assets", selected = names(rv$export_registry))
  })
  
  observeEvent(input$deselect_all_assets, {
    updateCheckboxGroupInput(session, "selected_assets", selected = character(0))
  })
  
  # Clearing empties the export list of the run on screen; nothing rebuilds it
  # but another run, so it is confirmed first.
  observeEvent(input$clear_registry, {
    n_items <- length(rv$export_registry)
    if (n_items == 0) {
      showNotification("The export registry is already empty.", type = "message")
      return()
    }
    showModal(modalDialog(
      title = "Clear Session Registry",
      tags$p(sprintf("All %d export item%s of the displayed run will be removed from this list.",
                     n_items, if (n_items == 1) "" else "s")),
      tags$p("The maps, tables and plots stay on screen, and archived runs keep their own items. Apart from a Quick export of the map on screen, this run's items are rebuilt only by a new run."),
      footer = tagList(
        actionButton("confirm_clear_registry", "Clear Registry", class = "btn-danger", icon = icon("trash")),
        modalButton("Cancel")
      ),
      size = "m", easyClose = TRUE
    ))
  })

  observeEvent(input$confirm_clear_registry, {
    removeModal()
    rv$export_registry <- list()
    showNotification("Export registry cleared.", type = "message")
  })
  
  observeEvent(input$quick_export_map, {
    # Export exactly what the viewer shows: the committed run's surface for
    # the currently selected view.
    meta <- get_display_meta(); req(meta)
    view <- map_view_base()
    # The run's own variance surfaces decide, so a stale menu value cannot
    # export an uncertainty layer from a concentration map.
    layer <- if (length(meta$variance_surfaces)) map_view_layer() else "value"
    # An SE or variance view exports that layer as single-band rasters, the
    # same product the run registered as "Uncertainty Map": the localities
    # that hold a variance, the SE named var1.se (the GeoTIFF band description).
    shown <- function(surface) {
      if (layer == "value") return(surface)
      surface_band(rasters_with_variance(surface), "var1.var",
                   fun = if (layer == "se") "sqrt",
                   name = if (layer == "se") "var1.se" else "var1.var")
    }
    target <- switch(view,
      "view_pred"  = shown(rv$rast_list_pre),
      "view_resid" = rv$rast_list_res,
      "view_comp"  = {
        a <- shown(rv$rast_list_act); p <- shown(rv$rast_list_pre)
        if (length(a) && length(p)) list(act = a, pre = p)
      },
      shown(rv$rast_list_act))
    req(length(target) > 0)

    type <- if (view == "view_comp") "map_combined" else "map"
    kind <- if (view == "view_resid") "residual" else if (layer != "value") "uncertainty" else "value"
    view_lab <- switch(view,
      "view_pred" = "Predicted", "view_resid" = "Residuals",
      "view_comp" = "Actual vs Predicted", "Actual")
    if (layer != "value") view_lab <- paste0(if (layer == "se") "SE" else "Variance", " - ", view_lab)

    id <- paste0("quick_", view, if (layer != "value") paste0("_", layer), "_", meta$actual)
    label <- paste0("Quick Export: ", meta$label, " (", view_lab, ") - ", get_method_label(meta$method))
    legend <- map_legend_title(meta$label, meta$unit,
                               if (view == "view_resid") "resid" else layer)

    # The registry key, not the raw id: a column name such as "Total N (%)"
    # is stored under its safe key, which every lookup below reads.
    key <- register_export_item(id, label, type, target, kind = kind, legend = legend,
                                surface = if (view == "view_pred") "pre" else if (view == "view_act") "act")
    active_styler_item(key)

    shinyjs::click("open_styler")
  })
  
  # Persist the styler config once per settled state (debounced), not on
  # every keystroke/slider tick in every styler field.
  styler_persist_cfg <- debounce(reactive({
    req(input$styler_title_size)
    lapply(styler_fields, function(field) {
      input[[field$name]]
    })
  }), 750)
  observeEvent(styler_persist_cfg(), {
    shinyjs::runjs(sprintf("localStorage.setItem('monolith_styler_config', JSON.stringify(%s));", jsonlite::toJSON(styler_persist_cfg(), auto_unbox = TRUE)))
  })

  observeEvent(input$open_styler, {
    # priority: 'event' re-fires the restore on every open (Shiny dedupes equal
    # payloads), or the persist observer would save the new modal's defaults.
    shinyjs::runjs("
      var cfg = localStorage.getItem('monolith_styler_config');
      if(cfg) {
        Shiny.setInputValue('styler_local_config', JSON.parse(cfg), {priority: 'event'});
      }
    ")
    
    # GeoTIFF is offered only for items whose payload IS a raster surface;
    # every styling control switches off while it is selected.
    styler_item <- tryCatch(rv$export_registry[[active_styler_item()]], error = function(e) NULL)
    fmt_choices <- c("PNG" = "png", "TIFF (image)" = "tiff", "PDF" = "pdf", "JPEG" = "jpg")
    has_raster <- export_raster_count(styler_item) > 0L
    if (has_raster) {
      fmt_choices <- c(fmt_choices, "GeoTIFF (data)" = "gtiff")
    }
    image_only_map <- !has_raster &&
      !is.null(styler_item) && styler_item$type %in% c("map", "map_combined")
    legend_defaults <- item_legend_defaults(styler_item)

    showModal(modalDialog(
      title = "Monolith Export Styler",
      size = "l",
      easyClose = FALSE,
      fluidRow(
        column(4,
               tabsetPanel(
                 tabPanel("Basic",
                          wellPanel(
                            h4("1. Output Quality"),
                            selectInput("styler_format", "File Format", choices = fmt_choices),
                            conditionalPanel(
                              condition = "input.styler_format == 'gtiff'",
                              tags$p(style = "font-size: 0.8em; color: var(--mn-text-2); background: var(--mn-surface-2); border: 1px solid var(--mn-line); border-left: 2px solid var(--mn-accent); padding: 8px; border-radius: 3px;",
                                     icon("info-circle"),
                                     " GeoTIFF writes the raster values, coordinate reference system and extent of this surface as the run produced it, in the run's Target Mapping CRS, for use in QGIS or ArcGIS. ",
                                     if (has_raster && !gtiff_single(styler_item)) {
                                       tags$b("These localities need separate files to preserve every grid and value. The download is a zip with one GeoTIFF per locality, each at its own cell size. ")
                                     },
                                     "Typography, palette, DPI and layout settings do not apply to it. Kriging surfaces carry two bands, prediction then variance. The variance is empty over a locality whose variogram could not be estimated, and a file left without any variance carries the prediction only.")
                            ),
                            if (image_only_map) {
                              tags$p(style = "font-size: 0.8em; color: var(--mn-text-2); background: var(--mn-surface-2); border: 1px solid var(--mn-line); border-left: 2px solid var(--mn-warn); padding: 8px; border-radius: 3px;",
                                     icon("exclamation-triangle"),
                                     " This item is not a raster surface (a paired comparison map, or point geometry), so it exports as an image only. For GIS layers of point and polygon geometry, use the Class zones and Drawn polygons entries in the Map Viewer toolbar's Downloads menu.")
                            },
                            conditionalPanel(
                              condition = "input.styler_format != 'gtiff'",
                              numericInput("styler_dpi", "Export DPI", value = 300, min = 72, max = 600),
                              hr(),
                              h4("2. Typography Overrides"),
                              textInput("styler_title", "Main Title", placeholder = "Auto-generated"),
                              fluidRow(
                                column(6, textInput("styler_x_title", "X-Axis Label", placeholder = "Default")),
                                column(6, textInput("styler_y_title", "Y-Axis Label", placeholder = "Default"))
                              ),
                              hr(),
                              h4("3. Residual / Error Maps"),
                              selectInput("styler_resid_palette", "Diverging Palette",
                                          choices = c("Red-Blue" = "RdBu", "Red-Yellow-Blue" = "RdYlBu",
                                                      "Purple-Orange" = "PuOr", "Brown-Teal" = "BrBG",
                                                      "Pink-Green" = "PiYG", "Purple-Green" = "PRGn",
                                                      "Spectral" = "Spectral", "Red-Yellow-Green" = "RdYlGn",
                                                      "Red-Grey" = "RdGy"),
                                          selected = "RdBu"),
                              tags$p(style = "font-size: 0.8em; color: var(--mn-text-2); margin-top: -8px;",
                                     "Applies to residual and point error maps only. Zero is always centered.")
                            )
                          )
                 ),
                 tabPanel("Advanced",
                          conditionalPanel(
                            condition = "input.styler_format == 'gtiff'",
                            wellPanel(
                              tags$p(style = "margin: 0; color: var(--mn-text-2);",
                                     "Styling does not apply to a GeoTIFF export. Choose an image format on the Basic tab to use these controls.")
                            )
                          ),
                          conditionalPanel(
                            condition = "input.styler_format != 'gtiff'",
                          wellPanel(
                            h4("Font Sizes (pt)"),
                            fluidRow(
                              column(6, sliderInput("styler_title_size", "Main Title", min = 6, max = 40, value = 15)),
                              column(6, sliderInput("styler_base_size", "Base Text", min = 4, max = 30, value = 13))
                            ),
                            fluidRow(
                              column(6, sliderInput("styler_x_size", "X-Axis Label Size", min = 4, max = 30, value = 13)),
                              column(6, sliderInput("styler_y_size", "Y-Axis Label Size", min = 4, max = 30, value = 13))
                            ),
                            fluidRow(
                              column(6, sliderInput("styler_label_size", "Axis Text", min = 4, max = 30, value = 15)),
                              column(6, sliderInput("styler_legend_size", "Legend Text", min = 4, max = 30, value = 15))
                            ),
                            fluidRow(
                              column(6, sliderInput("styler_legend_key_size", "Legend Element Size", min = 0.5, max = 5.0, value = 0.5, step = 0.1)),
                              column(6, selectInput("styler_font_family", "Font Family", 
                                          choices = c("sans", "serif", "mono", "Roboto", "Open Sans", "Lato", "Montserrat")))
                            ),
                            selectInput("styler_label_orient", "X-Label Orientation", 
                                        choices = c("Vertical" = 90, "Horizontal" = 0, "Angled (45)" = 45)),
                            hr(),
                            h4("Layout & Spacing"),
                            selectInput("styler_legend_pos", "Legend Position",
                                        choices = c("Right" = "right", "Bottom" = "bottom", "Left" = "left", "Top" = "top", "None" = "none"),
                                        selected = legend_defaults$pos),
                            selectInput("styler_legend_dir", "Legend Orientation",
                                        choices = c("Automatic" = "auto", "Horizontal" = "horizontal", "Vertical" = "vertical"),
                                        selected = legend_defaults$dir),
                            selectInput("styler_legend_text_angle", "Legend Text Orientation",
                                        choices = c("Horizontal" = 0, "Vertical" = 90, "Angled (45)" = 45),
                                        selected = legend_defaults$angle),
                            fluidRow(
                              column(3, numericInput("styler_margin_t", "Top", value = 10)),
                              column(3, numericInput("styler_margin_r", "Right", value = 10)),
                              column(3, numericInput("styler_margin_b", "Bottom", value = 10)),
                              column(3, numericInput("styler_margin_l", "Left", value = 10))
                            ),
                            checkboxInput("styler_show_grid", "Show Coordinate Grid", TRUE),
                            hr(),
                            h4("Publication Modifiers"),
                            checkboxInput("styler_high_contrast", "High Contrast Palette (Colorblind Safe)", FALSE),
                            fluidRow(
                              column(6, numericInput("styler_width", "Export Width (in)", value = 10, min = 1, max = 50, step = 0.5)),
                              column(6, numericInput("styler_height", "Export Height (in)", value = 8, min = 1, max = 50, step = 0.5))
                            ),
                            numericInput("styler_aspect_ratio", "Custom Aspect Ratio (Width/Height)", value = 1.25, step = 0.1)
                          )
                          )
                 )
               )
        ),
        column(8,
               conditionalPanel(
                 condition = "input.styler_format != 'gtiff'",
                 # min-width: 0 lets the image output report the pane's real
                 # width; overflow: hidden is only a guard.
                 div(style = paste0("background-color: var(--mn-surface-2); border: 1px solid var(--mn-line); height: 600px;",
                                    " padding: 10px; display: flex; justify-content: center;",
                                    " align-items: center; overflow: hidden;"),
                     div(style = "width: 100%; min-width: 0;",
                         imageOutput("styler_preview_plot", height = "auto", width = "100%")
                     )
                 ),
                 tags$p(style="font-size: 0.85em; color: var(--mn-text-2); margin-top: 5px;",
                        "The preview is the export figure at screen resolution: same size, typography and margins. Only the pixel count differs.")
               ),
               conditionalPanel(
                 condition = "input.styler_format == 'gtiff'",
                 div(style = "background-color: var(--mn-surface-2); border: 1px solid var(--mn-line); height: 600px; display: flex; flex-direction: column; justify-content: center; align-items: center; text-align: center; padding: 40px; color: var(--mn-text-2);",
                     icon("layer-group", class = "fa-3x", style = "margin-bottom: 20px; color: var(--mn-text-3);"),
                     tags$h4("GeoTIFF export", style = "margin-top: 0;"),
                     tags$p("There is nothing to preview: the file carries the raster values themselves, georeferenced in the run's Target Mapping CRS, not a rendering of them."),
                     tags$p(style = "font-size: 0.9em;", "Open it in a GIS to symbolise it there.")
                 )
               )
        )
      ),
      footer = tagList(
        div(style = "float: left; display: flex; gap: 10px;",
            downloadButton("styler_download_config", "Save Config", class = "btn-secondary btn-sm"),
            fileInput("styler_upload_config", NULL, buttonLabel = "Load Config", accept = c(".json"), multiple = FALSE, placeholder = "No file")
        ),
        modalButton("Cancel"),
        downloadButton("confirm_export", "Finalize & Download", class = "btn-success", icon = icon("check"))
      )
    ))
  })

  observeEvent(input$styler_local_config, {
    cfg <- input$styler_local_config
    # A remembered "GeoTIFF" is dropped for an item without a raster payload
    # (it is not among that item's formats).
    if (identical(cfg$format, "gtiff") || identical(cfg$styler_format, "gtiff")) {
      item <- tryCatch(rv$export_registry[[active_styler_item()]], error = function(e) NULL)
      if (export_raster_count(item) == 0L) {
        cfg$format <- NULL
        cfg$styler_format <- NULL
      }
    }
    sync_styler_config(cfg, session)
    # The remembered config belongs to the last item styled, so the legend
    # layout is re-derived for this one (an uploaded config still wins).
    apply_item_legend_defaults()
  })

  # Width or height sets the ratio; the ratio sets the height (below).
  observeEvent(list(input$styler_width, input$styler_height), {
    req(input$styler_width, input$styler_height)
    new_ratio <- round(input$styler_width / input$styler_height, 2)
    if (abs(new_ratio - (input$styler_aspect_ratio %||% 0)) > 0.01) {
      updateNumericInput(session, "styler_aspect_ratio", value = new_ratio)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$styler_aspect_ratio, {
    req(input$styler_aspect_ratio, input$styler_width)
    new_height <- round(input$styler_width / input$styler_aspect_ratio, 2)
    if (abs(new_height - (input$styler_height %||% 0)) > 0.01) {
      updateNumericInput(session, "styler_height", value = new_height)
    }
  }, ignoreInit = TRUE)
  
  output$styler_download_config <- downloadHandler(
    filename = function() { paste0("styler_config_", format(Sys.time(), "%Y%m%d"), ".json") },
    content = function(file) {
      cfg <- lapply(styler_fields, function(field) {
        input[[field$name]]
      })
      names(cfg) <- sapply(styler_fields, function(f) f$name)
      write(jsonlite::toJSON(cfg, auto_unbox = TRUE), file)
    }
  )

  observeEvent(input$styler_upload_config, {
    req(input$styler_upload_config)
    tryCatch({
      cfg <- jsonlite::fromJSON(input$styler_upload_config$datapath)
      
      sync_styler_config(cfg, session)
      
      showNotification("Styler configuration loaded successfully.", type = "message")
    }, error = function(e) {
      showNotification("Failed to load configuration. Invalid JSON.", type = "error")
    })
  })
  
  # One writer for every table sheet: bold frozen header and auto widths (bare
  # writeData() truncated labels at 8.43 characters). The fill is a worksheet
  # colour, not app chrome, so a literal is correct.
  .export_header_style <- createStyle(textDecoration = "bold", fgFill = "#EFEFEF",
                                      border = "bottom", halign = "left",
                                      valign = "center")
  write_table_sheet <- function(wb, sheet, df) {
    addWorksheet(wb, sheet)
    writeData(wb, sheet, df, headerStyle = .export_header_style)
    freezePane(wb, sheet, firstRow = TRUE)
    setColWidths(wb, sheet, cols = seq_len(max(1L, ncol(df))), widths = "auto")
  }

  # A raster item's GeoTIFF is ONE file when it holds one locality or the
  # run's localities share a lattice without overlapping (rv$disp$mergeable,
  # set when the run lands), and a zip of one file per locality otherwise. The
  # registry and rv$disp always describe the same run (a restore swaps them
  # together).
  gtiff_single <- function(item) {
    export_raster_count(item) == 1L || isTRUE(rv$disp$mergeable)
  }

  # What a registry item is written as: "gtiff" only for items holding a
  # raster, else PNG (the batch handler logs the swap).
  export_ext_for <- function(item, fmt) {
    if (!item$type %in% c("plot", "map", "map_combined")) return("xlsx")
    if (!identical(fmt, "gtiff")) return(styler_format_ext(fmt))
    if (export_raster_count(item) == 0L) return("png")
    if (gtiff_single(item)) styler_format_ext(fmt) else "zip"
  }

  # GeoTIFF metadata identifying the run and the localities the file holds.
  geotiff_tags <- function(item, localities) {
    d <- rv$disp
    cfg <- rv$run_config_summary
    lab <- item$var_label %||% d$label %||% ""
    product <- if (nzchar(lab) && startsWith(item$label, paste0(lab, " - "))) {
      substring(item$label, nchar(lab) + 4L)
    } else item$label
    c(MONOLITH_VARIABLE = lab,
      MONOLITH_UNIT = d$unit %||% "",
      MONOLITH_PRODUCT = product,
      MONOLITH_METHOD = get_method_label(d$method %||% ""),
      MONOLITH_LOCALITY = localities,
      MONOLITH_RUN_ID = as.character(cfg$run_id %||% ""),
      MONOLITH_APP_VERSION = cfg$app_version %||% app_version,
      MONOLITH_TARGET_CRS = d$crs_sel %||% "",
      MONOLITH_CREATED = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
  }

  # Writes a raster item's GeoTIFFs into `dir` under `stem` and returns their
  # file names: one file, or one per locality with the locality in its name.
  # A missing tag must never cost a file: write_geotiff keeps the raster and
  # warns, and the warning is put where the user can see it.
  write_item_geotiffs <- function(item, dir, stem) {
    rasters <- export_rasters(item, merge = isTRUE(rv$disp$mergeable))
    if (length(rasters) == 0) stop("No raster surface to export.")
    filenames <- unique_export_filenames(
      paste0(stem, if (length(rasters) > 1) paste0("_", safe_key(names(rasters))), ".tif"),
      used = list.files(dir))
    vapply(seq_along(rasters), function(i) {
      loc <- names(rasters)[i]
      name <- filenames[i]
      withCallingHandlers(
        write_geotiff(rasters[[i]], file.path(dir, name), tags = geotiff_tags(item, loc)),
        warning = function(w) {
          rv$log <- paste0(rv$log, "\n[Export] ", item$label, " (", loc, "): ", conditionMessage(w))
          showNotification(conditionMessage(w), type = "warning", duration = 10)
          invokeRestart("muffleWarning")
        })
      name
    }, character(1))
  }

  output$confirm_export <- downloadHandler(
    filename = function() {
      req(active_styler_item())
      item <- rv$export_registry[[active_styler_item()]]
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      ext <- export_ext_for(item, input$styler_format)
      export_file_name("Export", item$id, rv$disp$method, timestamp, ext)
    },
    content = function(file) {
      req(active_styler_item())
      item <- rv$export_registry[[active_styler_item()]]
      ext <- export_ext_for(item, input$styler_format)

      withProgress(message = paste("Exporting", item$type, "..."), {
        tryCatch({
          if (ext %in% c("tif", "zip")) {
            dir <- tempfile("gtiff_")
            dir.create(dir)
            on.exit(unlink(dir, recursive = TRUE), add = TRUE)
            stem <- tools::file_path_sans_ext(export_file_name(
              "Export", item$id, rv$disp$method, format(Sys.time(), "%Y%m%d_%H%M%S"), "tif"))
            files <- write_item_geotiffs(item, dir, stem)
            if (identical(ext, "zip")) {
              zip::zip(zipfile = file, files = files, root = dir)
            } else {
              file.copy(file.path(dir, files), file, overwrite = TRUE)
            }
          } else if (item$type %in% c("plot", "map", "map_combined")) {
            p_obj <- generate_styled_plot(
              item, input,
              agro_params = export_class_params(item),
              palette = export_palette()
            )

            export_plot_to_file(p_obj, file, ext, input)
          } else if (item$type == "table") {
            # export_ext_for() writes every table as .xlsx.
            wb <- createWorkbook()
            write_table_sheet(wb, "Data", item$obj)
            saveWorkbook(wb, file, overwrite = TRUE)
          } else {
            stop(sprintf("No writer for export type '%s'.", item$type))
          }

          removeModal()
        }, error = function(e) {
          # Swallowing this would return an unwritten file and land the browser
          # on a dead download URL; raising it puts the reason on screen.
          stop(safeError(paste("Export Failed:", conditionMessage(e))))
        })
      })
    }
  )
  
  output$batch_export <- downloadHandler(
    filename = function() export_file_name("Batch_Export", NULL, rv$disp$method, format(Sys.time(), "%Y%m%d_%H%M%S"), "zip"),
    contentType = "application/zip",
    content = function(file) {
      req(input$selected_assets, length(input$selected_assets) > 0)
      
      # A unique directory per call; a name taken from the clock would be shared
      # by two exports started within the same second.
      temp_dir <- tempfile("export_")
      dir.create(temp_dir, showWarnings = FALSE)
      
      withProgress(message = "Batch Exporting...", value = 0, {
        selected_ids <- input$selected_assets
        items <- rv$export_registry[selected_ids]
        
        table_items <- Filter(function(x) x$type == "table", items)
        plot_items <- Filter(function(x) x$type %in% c("plot", "map", "map_combined"), items)
        
        n_plots <- length(plot_items)
        has_tables <- length(table_items) > 0
        total_steps <- n_plots + (if(has_tables) 1 else 0)

        files_to_zip <- c()

        if(has_tables) {
          incProgress(1/total_steps, detail = "Compiling statistical tables into Excel...")
          
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          excel_name <- export_file_name("Batch_Statistics", NULL, rv$disp$method, timestamp, "xlsx")
          excel_path <- file.path(temp_dir, excel_name)
          
          wb <- createWorkbook()
          used_sheet_names <- c()
          n_sheets <- 0L
          for(item in table_items) {
            # export_sheet_name(): 31 characters, shared variable prefix
            # dropped, unique ignoring case.
            sheet_name <- export_sheet_name(item$label, item$id, used_sheet_names,
                                            item$var_label)
            used_sheet_names <- c(used_sheet_names, sheet_name)
            # Per item, like the plot loop below: one unwritable table costs one
            # table, not the whole zip (every selected figure included).
            ok <- tryCatch({
              write_table_sheet(wb, sheet_name, item$obj)
              TRUE
            }, error = function(e) {
              rv$log <- paste0(rv$log, "\n[Batch] Failed to add table sheet '",
                               item$label, "': ", conditionMessage(e))
              FALSE
            })
            if (ok) n_sheets <- n_sheets + 1L
          }
          if (n_sheets > 0) {
            saveWorkbook(wb, excel_path, overwrite = TRUE)
            files_to_zip <- c(files_to_zip, excel_name)
          }
        }
        
        for (i in seq_along(plot_items)) {
          item <- plot_items[[i]]
          incProgress(1/total_steps, detail = paste("Exporting Plot:", item$label))
          
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          ext <- export_ext_for(item, input$styler_format)
          if (identical(input$styler_format, "gtiff") && identical(ext, "png")) {
            rv$log <- paste0(rv$log, "\n[Batch] ", item$label,
                             " is not a raster surface; exported as PNG instead of GeoTIFF.")
          }

          filename <- unique_export_filenames(
            export_file_name("Batch", item$id, rv$disp$method, timestamp, ext),
            used = list.files(temp_dir))

          tryCatch({
            if (ext %in% c("tif", "zip")) {
              # Per-locality GeoTIFFs sit beside the other files, not in a
              # zip of their own.
              filename <- write_item_geotiffs(item, temp_dir, tools::file_path_sans_ext(filename))
            } else {
              p <- generate_styled_plot(
                item, input,
                agro_params = export_class_params(item),
                palette = export_palette()
              )
              export_plot_to_file(p, file.path(temp_dir, filename), ext, input)
            }
            files_to_zip <- c(files_to_zip, filename)
          }, error = function(e) {
            rv$log <- paste0(rv$log, "\n[Batch] Failed to export ", item$label, ": ", e$message)
          })
        }
      })
      
      if (length(files_to_zip) == 0) {
        unlink(temp_dir, recursive = TRUE)
        showNotification("Batch export failed: none of the selected assets could be exported. Check the Log tab for details.", type = "error", duration = 10)
        stop("No assets could be exported.")
      }

      zip_path <- file.path(temp_dir, "export.zip")
      zip::zip(zipfile = zip_path, files = files_to_zip, root = temp_dir)

      file.copy(zip_path, file)

      unlink(temp_dir, recursive = TRUE)
    }
  )
  
  
