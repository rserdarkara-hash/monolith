# server_export.R (sourced with local = TRUE inside server) - export registry,
# run-config/run-history panels, WYSIWYG styler and export download handlers.
  register_export_item <- function(id, label, type, obj, category = "General", kind = "value") {
    req(obj)
    clean_id <- gsub("[^a-zA-Z0-9_]", "_", id)

    new_item <- list(
      id = clean_id,
      label = label,
      type = type, # "plot", "table", "map"
      obj = obj,
      category = category,
      # For maps: "value" (concentration surface, may be agro/bin classified),
      # "residual" (diverging scale, never classified), "uncertainty"
      # (sequential continuous, never classified). Agronomic class limits are
      # defined on the variable's units, so classifying errors or variances
      # with them is scientifically meaningless.
      kind = kind,
      timestamp = Sys.time()
    )
    
    current_reg <- isolate(rv$export_registry)
    current_reg[[clean_id]] <- new_item
    rv$export_registry <- current_reg
    
    rv$log <- paste0(rv$log, "\n[Registry] Registered ", type, ": ", label)
  }
  
  output$export_registry_ui <- renderUI({
    req(rv$export_registry)
    reg <- rv$export_registry
    if (length(reg) == 0) return(tags$p("Registry is empty. Run models to populate."))
    
    choices <- setNames(names(reg), sapply(reg, function(x) {
      sprintf("%s [%s] - %s", x$label, x$type, format(x$timestamp, "%H:%M:%S"))
    }))
    
    checkboxGroupInput("selected_assets", NULL, choices = choices, width = "100%")
  })

  output$run_config_display <- renderUI({
    cfg <- rv$run_config_summary
    if (is.null(cfg)) return(NULL)
    div(style = "background-color: #e8f4fd; padding: 12px 15px; border-left: 4px solid #2196F3; border-radius: 4px; margin-bottom: 10px; font-family: monospace; font-size: 0.85em;",
      tags$strong(icon("info-circle"), paste0(" Run #", cfg$run_id, " Configuration (", format(cfg$timestamp, "%Y-%m-%d %H:%M:%S"), ")")),
      tags$br(),
      tags$span(paste0("Variable: ", cfg$variable, " | Method: ", cfg$method, " | Localities: ", cfg$localities)),
      tags$br(),
      tags$span(paste0("Subset: ", cfg$subset, " | View: ", cfg$value_type, " | CRS: ", cfg$crs)),
      tags$br(),
      tags$span(paste0("Boundary: ", cfg$boundary_type, " | Buffer: ", if (is.null(cfg$buffer_mode) || cfg$buffer_mode == "fixed") paste0(cfg$buffer_dist, "m") else "Dynamic", " | Resolution: ", cfg$resolution, " (", cfg$res_mode, ")")),
      # Method-agnostic settings that used to be invisible here even though they
      # change the reported numbers; the method-specific ones print only where
      # they apply (they are NA for the other engines).
      tags$br(),
      tags$span(paste0(
        # An absent field means the entry predates this record (an archived run
        # restored from an older session), so nothing is asserted about it.
        if (!is.null(cfg$cv_strategy)) paste0("CV strategy: ", switch(cfg$cv_strategy,
                                                                     "loocv" = "Standard LOOCV",
                                                                     "block" = "Spatial Block CV",
                                                                     "Auto")) else "CV strategy: not recorded",
        if (!is.null(cfg$cv_repeats) && !is.na(cfg$cv_repeats) && cfg$cv_repeats > 1) paste0(" | Repeated CV: ", cfg$cv_repeats, " fold realizations") else "",
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

  # Reproducibility record for the CURRENT run: the same summary the run-history
  # entries store, plus the per-locality tuning actually consumed and the
  # software versions it ran under. jsonlite is a hard dependency of shiny, so
  # this adds no package to required_packages.
  output$download_run_config <- downloadHandler(
    filename = function() {
      cfg <- rv$run_config_summary
      paste0("monolith_run_", if (is.null(cfg)) "config" else cfg$run_id, "_",
             format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
    },
    content = function(file) {
      cfg <- rv$run_config_summary
      req(cfg)
      pkgs <- c("sf", "gstat", "fields", "randomForest", "terra")
      pkg_versions <- setNames(
        lapply(pkgs, function(p) tryCatch(as.character(utils::packageVersion(p)),
                                          error = function(e) NA_character_)),
        pkgs
      )
      payload <- list(
        config = cfg,
        regional_params = rv$disp$regional_params,
        provenance = list(
          app_version = cfg$app_version %||% app_version,
          r_version = R.version.string,
          platform = R.version$platform,
          exported_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
          packages = pkg_versions
        )
      )
      writeLines(
        jsonlite::toJSON(payload, pretty = TRUE, auto_unbox = TRUE,
                         null = "null", force = TRUE, POSIXt = "ISO8601"),
        file
      )
    }
  )

  output$run_config_display_map <- renderUI({
    cfg <- rv$run_config_summary
    if (is.null(cfg)) return(NULL)
    div(style = "background-color: #e8f4fd; padding: 8px 12px; border-left: 4px solid #2196F3; border-radius: 4px; margin-bottom: 8px; font-size: 0.82em;",
      tags$strong(paste0("Run #", cfg$run_id, ": ")),
      tags$span(paste0(cfg$variable, " | ", cfg$method, " | ", cfg$localities, " | ", format(cfg$timestamp, "%H:%M:%S")))
    )
  })

  output$run_history_ui <- renderUI({
    hist <- rv$run_history
    if (length(hist) == 0) return(tags$p(style="color: #888;", "No archived runs yet. Previous runs will appear here when a new model generation begins."))

    run_panels <- lapply(seq_along(hist), function(i) {
      run <- hist[[i]]
      cfg <- run$config
      n_items <- length(run$registry)
      div(style = "background-color: #fff; padding: 12px; border: 1px solid #ddd; border-radius: 6px; margin-bottom: 8px;",
        fluidRow(
          column(8,
            tags$strong(paste0("Run #", cfg$run_id, " - ", cfg$variable, " (", cfg$method, ")")),
            tags$br(),
            tags$small(style="color: #666;", paste0(
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
        history_list <- isolate(rv$run_history)
        idx <- which(sapply(history_list, function(x) x$config$run_id) == run_id)[1]
        if (is.na(idx)) return()
        
        run_to_restore <- history_list[[idx]]
        
        current_cfg <- isolate(rv$run_config_summary)
        current_reg <- isolate(rv$export_registry)
        if (!is.null(current_cfg) && length(current_reg) > 0) {
          push_run_history(list(config = current_cfg, registry = current_reg),
                           base = history_list[-idx])
        } else {
          rv$run_history <- history_list[-idx]
        }
        
        rv$export_registry <- run_to_restore$registry
        rv$run_config_summary <- run_to_restore$config
        
        showNotification(paste0("Restored Run #", run_to_restore$config$run_id, " to active session."), type = "message")
      }, ignoreInit = TRUE)

      obs_delete <- observeEvent(input[[paste0("delete_run_", run_id)]], {
        history_list <- isolate(rv$run_history)
        idx <- which(sapply(history_list, function(x) x$config$run_id) == run_id)[1]
        if (!is.na(idx)) {
          rv$run_history <- history_list[-idx]
          showNotification("Archived run removed.", type = "warning")
        }
      }, ignoreInit = TRUE)
      
      env_hist$history_obs[[paste0("restore_", run_id)]] <- obs_restore
      env_hist$history_obs[[paste0("delete_", run_id)]] <- obs_delete
    })
  }, ignoreNULL = FALSE)

  active_styler_item <- reactiveVal(NULL)
  
  observeEvent(input$selected_assets, {
    req(input$selected_assets)
    if (length(input$selected_assets) > 0) {
      active_styler_item(input$selected_assets[length(input$selected_assets)])
    }
  }, ignoreNULL = FALSE)
  
  # Legend placement follows the item's shape, not whatever was styled last: a
  # paired comparison map is two panels side by side with no room for a
  # right-hand legend, so it takes a horizontal legend underneath; everything
  # else keeps the right-hand default. The styler's controls are built with
  # these already selected; this re-applies them after the remembered config
  # has been replayed over the freshly built modal.
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

  base_preview_plot <- reactive({
    req(active_styler_item(), rv$export_registry)
    item <- rv$export_registry[[active_styler_item()]]
    req(item)
    
    generate_base_plot(
      item = item,
      input = input,
      agro_params = tryCatch(agro_params(), error = function(e) NULL)
    )
  })
  
  styled_preview_obj <- reactive({
    req(active_styler_item(), rv$export_registry)
    item <- rv$export_registry[[active_styler_item()]]
    req(item)
    
    base_p <- base_preview_plot()
    req(base_p)
    
    apply_styler_theme(
      p = base_p,
      input = input,
      item_label = item$label,
      item_type = item$type
    )
  })

  styled_preview_obj_d <- styled_preview_obj %>% debounce(500)

  # The preview is drawn on a canvas of the EXPORT's physical size and only
  # rasterised coarser, so its layout is the file's layout. Shrinking the canvas
  # instead (the former behaviour) leaves point-sized text and millimetre
  # margins competing for less room, which crowded and clipped axis labels the
  # export had space for - worst on wide areas and on two-panel comparisons.
  #
  # The rasterised canvas is then fitted to the pane the browser actually
  # reports rather than to a fixed guess. A guess wider than the pane overflows
  # it, and the pane centres its content, so that overflow goes off the LEFT
  # edge where no scrollbar reaches it: the file was complete but the preview
  # lost its title, y-axis labels and grid numbers.
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
  # Window resizes arrive as a stream of widths; re-rendering the figure on each
  # one would redraw the whole plot dozens of times per drag.
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
         # Belt and braces: even if the reported pane width is stale, the image
         # scales into the pane instead of spilling out of it. Scaling keeps the
         # layout exact - only the pixel count changes.
         style = paste0("max-width: 100%; height: auto; display: block; margin: 0 auto;",
                        " background-color: #ffffff; box-shadow: 0 4px 8px rgba(0,0,0,0.2);"),
         alt = "Preview of the figure as it will be exported")
  }, deleteFile = TRUE)
  
  observeEvent(input$select_all_assets, {
    req(rv$export_registry)
    updateCheckboxGroupInput(session, "selected_assets", selected = names(rv$export_registry))
  })
  
  observeEvent(input$deselect_all_assets, {
    updateCheckboxGroupInput(session, "selected_assets", selected = character(0))
  })
  
  observeEvent(input$clear_registry, {
    rv$export_registry <- list()
    showNotification("Export registry cleared.", type = "message")
  })
  
  observeEvent(input$quick_export_map, {
    # Export exactly what the viewer shows: the committed run's surface for
    # the currently selected view.
    meta <- get_display_meta(); req(meta)
    view <- input$map_view %||% "view_act"
    target <- switch(view,
      "view_pred"  = rv$rast_pred,
      "view_resid" = rv$rast_res,
      "view_comp"  = if (!is.null(rv$rast) && !is.null(rv$rast_pred)) list(act = rv$rast, pre = rv$rast_pred) else NULL,
      rv$rast)
    req(target)

    type <- if (view == "view_comp") "map_combined" else "map"
    kind <- if (view == "view_resid") "residual" else "value"
    view_lab <- switch(view,
      "view_pred" = "Predicted", "view_resid" = "Residuals",
      "view_comp" = "Actual vs Predicted", "Actual")

    id <- paste0("quick_", view, "_", meta$actual)
    label <- paste("Quick Export:", meta$label, "(", view_lab, ")")

    register_export_item(id, label, type, target, meta$category, kind = kind)
    active_styler_item(id)

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
    # priority: 'event' forces the sync to re-fire on every open: Shiny
    # dedupes identical input payloads, and the modal re-creates its inputs
    # with default values each time, so a deduped (skipped) restore would let
    # the debounced persist observer overwrite the saved config with defaults.
    shinyjs::runjs("
      var cfg = localStorage.getItem('monolith_styler_config');
      if(cfg) {
        Shiny.setInputValue('styler_local_config', JSON.parse(cfg), {priority: 'event'});
      }
    ")
    
    # A GeoTIFF is the data rather than a picture of it, so it is offered only
    # for items whose payload IS one raster surface, and every styling control
    # switches off while it is selected.
    styler_item <- tryCatch(rv$export_registry[[active_styler_item()]], error = function(e) NULL)
    fmt_choices <- c("PNG" = "png", "TIFF (image)" = "tiff", "PDF" = "pdf", "JPEG" = "jpg")
    if (!is.null(export_raster_payload(styler_item))) {
      fmt_choices <- c(fmt_choices, "GeoTIFF (data)" = "gtiff")
    }
    image_only_map <- is.null(export_raster_payload(styler_item)) &&
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
                              tags$p(style = "font-size: 0.8em; color: #31708f; background: #d9edf7; border: 1px solid #bce8f1; padding: 8px; border-radius: 3px;",
                                     icon("info-circle"),
                                     " GeoTIFF writes the raster values, coordinate reference system and extent as computed, for use in QGIS or ArcGIS. Typography, palette, DPI and layout settings do not apply to it. Kriging surfaces are written as multi-band files (prediction, then variance).")
                            ),
                            if (image_only_map) {
                              tags$p(style = "font-size: 0.8em; color: #8a6d3b; background: #fcf8e3; border: 1px solid #faebcc; padding: 8px; border-radius: 3px;",
                                     icon("exclamation-triangle"),
                                     " This item is not a single raster surface (a paired comparison map, or point geometry), so it exports as an image only. For GIS layers of point and polygon geometry, use the Export Class Zones and Export Drawn Polygons buttons on the Map Viewer toolbar.")
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
                              tags$p(style = "font-size: 0.8em; color: #666; margin-top: -8px;",
                                     "Applies to residual and point error maps only. Zero is always centered.")
                            )
                          )
                 ),
                 tabPanel("Advanced",
                          conditionalPanel(
                            condition = "input.styler_format == 'gtiff'",
                            wellPanel(
                              tags$p(style = "margin: 0; color: #666;",
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
                 # min-width: 0 on the image output lets it report (and take)
                 # the pane's real width; overflow: hidden is a guard, nothing
                 # should reach it now that the canvas is fitted to the pane.
                 div(style = paste0("background-color: #f0f0f0; border: 1px solid #ccc; height: 600px;",
                                    " padding: 10px; display: flex; justify-content: center;",
                                    " align-items: center; overflow: hidden;"),
                     div(style = "width: 100%; min-width: 0;",
                         imageOutput("styler_preview_plot", height = "auto", width = "100%")
                     )
                 ),
                 tags$p(style="font-size: 0.85em; color: #666; margin-top: 5px;",
                        "The preview is the export figure at screen resolution: same size, typography and margins. Only the pixel count differs.")
               ),
               conditionalPanel(
                 condition = "input.styler_format == 'gtiff'",
                 div(style = "background-color: #f0f0f0; border: 1px solid #ccc; height: 600px; display: flex; flex-direction: column; justify-content: center; align-items: center; text-align: center; padding: 40px; color: #555;",
                     icon("layer-group", class = "fa-3x", style = "margin-bottom: 20px; color: #888;"),
                     tags$h4("GeoTIFF export", style = "margin-top: 0;"),
                     tags$p("There is nothing to preview: the file carries the raster values themselves, georeferenced in the run's analysis CRS, not a rendering of them."),
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
    # A remembered "GeoTIFF" choice must not be restored onto an item that has
    # no raster payload: that choice is not in the select for such an item, so
    # restoring it would leave the format blank.
    if (identical(cfg$format, "gtiff") || identical(cfg$styler_format, "gtiff")) {
      item <- tryCatch(rv$export_registry[[active_styler_item()]], error = function(e) NULL)
      if (is.null(export_raster_payload(item))) {
        cfg$format <- NULL
        cfg$styler_format <- NULL
      }
    }
    sync_styler_config(cfg, session)
    # The remembered config is whatever item was styled last, so it must not
    # carry that item's legend layout onto a differently shaped one. An
    # explicitly uploaded config still wins: that is a deliberate user action.
    apply_item_legend_defaults()
  })

  observeEvent(input$styler_width, {
    req(input$styler_width, input$styler_height)
    new_ratio <- round(input$styler_width / input$styler_height, 2)
    if (abs(new_ratio - (input$styler_aspect_ratio %||% 0)) > 0.01) {
      updateNumericInput(session, "styler_aspect_ratio", value = new_ratio)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$styler_height, {
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
  
  # One place decides what a registry item is written as. "gtiff" only survives
  # for items that actually hold a raster; anything else silently written as a
  # GeoTIFF would be a corrupt file, so it falls back to PNG (and the batch
  # handler says so in the run log).
  export_ext_for <- function(item, fmt) {
    if (!item$type %in% c("plot", "map", "map_combined")) return("xlsx")
    if (identical(fmt, "gtiff") && is.null(export_raster_payload(item))) return("png")
    styler_format_ext(fmt)
  }

  output$confirm_export <- downloadHandler(
    filename = function() {
      req(active_styler_item())
      item <- rv$export_registry[[active_styler_item()]]
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      ext <- export_ext_for(item, input$styler_format)
      sprintf("Export_%s_%s.%s", item$id, timestamp, ext)
    },
    content = function(file) {
      req(active_styler_item())
      item <- rv$export_registry[[active_styler_item()]]
      ext <- export_ext_for(item, input$styler_format)

      withProgress(message = paste("Exporting", item$type, "..."), {
        tryCatch({
          if (identical(ext, "tif")) {
            write_geotiff(export_raster_payload(item), file)
          } else if (item$type %in% c("plot", "map", "map_combined")) {
            p_obj <- generate_styled_plot(
              item, input,
              agro_params = tryCatch(agro_params(), error = function(e) NULL)
            )

            export_plot_to_file(p_obj, file, ext, input)
          } else if (item$type == "table") {
            if (ext == "xlsx") {
              wb <- createWorkbook()
              addWorksheet(wb, "Data")
              writeData(wb, "Data", item$obj)
              saveWorkbook(wb, file, overwrite = TRUE)
            } else {
              write.csv(item$obj, file, row.names = FALSE)
            }
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
    filename = function() { paste0("Batch_Export_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip") },
    contentType = "application/zip",
    content = function(file) {
      req(input$selected_assets, length(input$selected_assets) > 0)
      
      temp_dir <- file.path(tempdir(), paste0("export_", as.integer(Sys.time())))
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
          excel_name <- sprintf("Batch_Statistics_%s.xlsx", timestamp)
          excel_path <- file.path(temp_dir, excel_name)
          
          wb <- createWorkbook()
          used_sheet_names <- c()
          n_sheets <- 0L
          for(item in table_items) {
            # openxlsx rejects an empty or whitespace-only sheet name; fall back to
            # the registry id (then to a positional name) so a label-less item cannot
            # take the whole batch down with it.
            clean_label <- trimws(gsub("[^a-zA-Z0-9 ]", "_", item$label %||% ""))
            if (!nzchar(clean_label)) clean_label <- trimws(gsub("[^a-zA-Z0-9 ]", "_", item$id %||% ""))
            if (!nzchar(clean_label)) clean_label <- paste0("Table_", n_sheets + 1L)
            sheet_name <- substr(clean_label, 1, 31)
            
            if(sheet_name %in% used_sheet_names) {
              suffix <- if(grepl("_pre_|_pre$", item$id)) "_Pre" else "_2"
              counter <- 2
              candidate <- paste0(substr(sheet_name, 1, 31 - nchar(suffix)), suffix)
              while(candidate %in% used_sheet_names) {
                counter <- counter + 1
                suffix <- paste0("_", counter)
                candidate <- paste0(substr(sheet_name, 1, 31 - nchar(suffix)), suffix)
              }
              sheet_name <- candidate
            }
            used_sheet_names <- c(used_sheet_names, sheet_name)
            # Per item, like the plot loop below: one unwritable table costs one
            # table, not the whole zip (every selected figure included).
            ok <- tryCatch({
              addWorksheet(wb, sheet_name)
              writeData(wb, sheet_name, item$obj)
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
          if(ext == "csv") ext <- "png" # Extra safety
          if (identical(input$styler_format, "gtiff") && !identical(ext, "tif")) {
            rv$log <- paste0(rv$log, "\n[Batch] ", item$label,
                             " is not a single raster surface; exported as PNG instead of GeoTIFF.")
          }

          filename <- sprintf("Batch_%s_%s.%s", item$id, timestamp, ext)
          filepath <- file.path(temp_dir, filename)

          tryCatch({
            if (identical(ext, "tif")) {
              write_geotiff(export_raster_payload(item), filepath)
            } else {
              p <- generate_styled_plot(
                item, input,
                agro_params = tryCatch(agro_params(), error = function(e) NULL)
              )
              export_plot_to_file(p, filepath, ext, input)
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
  
  
