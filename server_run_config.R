# server_run_config.R (sourced with local = TRUE inside server) - display/run
# context resolvers (get_current_meta / get_display_meta), docs drawer,
# classification params, config persistence, palette + selector UIs.

  # The palette a variable is drawn in (resolve_var_palette): the one picked for
  # it this session, else its default. `fallback` serves a displayed run whose
  # variable has left the variable list (another dataset loaded since).
  palette_of <- function(var, fallback = "YlOrRd") {
    resolve_var_palette(var, rv$mapping$vars, rv$palette_picks, fallback)
  }
  # The variable the Color Palette picker styles: the displayed run's while it is
  # in the variable list, else the sidebar's (before the first run, and after
  # another dataset replaced the list).
  palette_var <- function() {
    shown <- rv$disp$var_id
    listed <- vapply(rv$mapping$vars %||% list(), function(v) as.character(v$actual), character(1))
    if (!is.null(shown) && shown %in% listed) shown else input$var_id
  }

  get_current_meta <- function() {
    var <- input$var_id
    if (is.null(var) || var == "" || is.null(rv$mapping$vars)) return(NULL)

    idx <- which(sapply(rv$mapping$vars, function(x) x$actual == var))
    if (length(idx) == 0) return(NULL)
    m <- rv$mapping$vars[[idx]]

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
      palette = palette_of(var),
      unit = as.character(m$unit %||% "")
    )
  }

  # Display-side twin of get_current_meta(): returns the context committed at
  # run dispatch (rv$disp) instead of reading the live sidebar inputs, so the
  # Map Viewer and Scientific Analysis tabs keep describing the run that is
  # actually on screen while the sidebar is reconfigured for the next run.
  # Only the colour palette stays live: a palette picked for the displayed
  # variable restyles the map on screen. Returns NULL before the first run.
  get_display_meta <- function() {
    d <- rv$disp
    if (is.null(d)) return(NULL)
    d$palette <- palette_of(d$var_id, fallback = d$palette)
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
    # A new variable list opens on a variable with predictions, not on its
    # first category, which is often the covariates (default_var_pick).
    sel_cat <- if(!is.null(current_cat) && current_cat %in% cats) current_cat else default_var_pick(vars)$category
    updateSelectInput(session, "var_category", choices = cats, selected = sel_cat)

    filtered <- Filter(function(x) x$category == sel_cat, vars)
    choices <- setNames(sapply(filtered, function(x) x$actual), sapply(filtered, function(x) x$label))
    sel_var <- if (isTruthy(input$var_id) && input$var_id %in% choices) input$var_id else default_var_pick(vars, sel_cat)$var
    shinyWidgets::updatePickerInput(session, "var_id", choices = choices, selected = sel_var)
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
    choices <- c("Actual values" = "actual")
    if (has_col(m$pred)) choices <- c(choices, "ML predictions" = "pred")
    if (has_col(m$pred_ss)) choices <- c(choices, "Single split" = "pred_ss")
    if (has_col(m$pred)) choices <- c(choices, "Residuals" = "resid")

    sel <- if (isTruthy(input$value_type) && input$value_type %in% choices) input$value_type else "actual"
    shinyWidgets::updateRadioGroupButtons(session, "value_type", choices = choices,
                                          selected = sel, size = "sm")

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
          img(src = "assets/banner.png", alt = "Monolith - Spatial Analysis Dashboard",
              style = "max-width: 100%; height: auto; margin-bottom: 20px;"),
          h4("Workbench for statistics and optimized mapping in life sciences."),
          p("Integrated geostatistical modeling, classification and statistical interpretation."),
          hr(),
          p("Designed for high-performance parallel processing and spatial diagnostics, multi-scale interpolation via kriging, inverse distance weighting, and thin plate splines with practical multi-criteria optimization."),
          p("Supported with the Descriptive and Exploratory Suite with dynamic visualizations and statistics."),
          hr(),
          p(strong("A product of `that` couple of months following the loss of institutional e-mail address.")),
          p(style = "color: var(--mn-text-2); font-size: 0.9em;", paste0("  by Recep Serdar Kara in cooperation with Antigravity CLI and Claude Code - 2026 (v", app_version, ")")),
          hr(),
          tags$details(
            tags$summary(style = "cursor: pointer; color: var(--mn-accent);", "Session Info (reproducibility)"),
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
  
  # Rendered once at app start (DOCS_HTML, global.R).
  output$render_user_guide <- renderUI(withMathJax(DOCS_HTML$user))
  output$render_desc_exploratory_guide <- renderUI(withMathJax(DOCS_HTML$desc))
  output$render_scientific_guide <- renderUI(withMathJax(DOCS_HTML$sci))
  
  # Unwrapped display-layer values of the merged run rasters, cached per run.
  # rv$rast / rv$rast_pred are PackedSpatRasters (they crossed the future
  # boundary) and Packed rasters are NOT subsettable, so every consumer must
  # go through raster_value_layer() - and caching here means the unwrap +
  # values() read happens once per run instead of on every styling tick.
  rast_vals_act <- reactive({ raster_value_layer(rv$rast) })
  rast_vals_pre <- reactive({ raster_value_layer(rv$rast_pred) })

  # ── Agronomical styling commit flow ─────────────────────────────────────
  # Agro sub-settings (algorithm, class count, supervised limits) are edited
  # freely and only take effect when Apply to maps and statistics is pressed:
  # applying re-encodes every visible map layer and recomputes the class-area
  # and agreement tables, which would otherwise run on every input tick.
  # Continuous and Binned styling stay immediate - they have no sub-settings to
  # stage.
  # A box that has not rendered yet, or was cleared, counts as the limit it
  # opens with (agro_limit_defaults).
  gather_agro_limits <- function(n_c) {
    d <- tryCatch(agro_limit_defaults()$limits, error = function(e) NULL)
    sapply(seq_len(n_c - 1), function(i) {
      val <- input[[paste0("agro_limit_", i)]]
      if (is.null(val) || is.na(val)) (if (i <= length(d)) d[i] else NA_real_) else val
    })
  }
  # The variable Supervised limits are typed for: the displayed run's, or the
  # sidebar's before a run (the variable the limit boxes open on).
  agro_limits_var <- function() (rv$disp %||% get_current_meta())$actual
  # Comparable one-line signature of an agro settings list (identical() is too
  # strict across integer/double input round-trips).
  agro_signature <- function(s) {
    if (is.null(s)) return("")
    paste(s$method, s$n_classes, s$var %||% "",
          paste(signif(as.numeric(s$limits %||% numeric(0)), 10), collapse = ","))
  }
  gather_agro_live <- function() {
    n_c <- input$agro_n_classes
    if (!isTruthy(n_c)) return(NULL)
    m <- input$agro_method %||% "limits"
    lims <- identical(m, "limits")
    list(method = m, n_classes = as.integer(n_c),
         limits = if (lims) as.numeric(gather_agro_limits(n_c)) else NULL,
         var = if (lims) agro_limits_var())
  }
  agro_applied <- reactiveVal(NULL)
  # The committed settings as they apply to the displayed variable. Supervised
  # limits are numbers in one variable's units, so they classify only the
  # variable they were applied for; a Jenks or K-Means commitment is recomputed
  # from whichever surface is displayed.
  agro_applied_now <- reactive({
    ap <- agro_applied()
    if (is.null(ap) || !identical(ap$method, "limits") || identical(ap$var, agro_limits_var())) ap
  })
  observeEvent(input$agro_apply, {
    live <- gather_agro_live()
    req(live)
    # Committing the staged settings is instant, but what it triggers is not:
    # classification_params() -> calc_class_breaks() -> the proxy restyler ->
    # the class-area and kappa tables can take seconds, during which an idle
    # button invites a second click. Re-enable on the next completed flush;
    # that is the honest end point, because the work is spread over several
    # independent reactives rather than one observable result.
    shinyjs::disable("agro_apply")
    updateActionButton(session, "agro_apply", label = "Applying...")
    session$onFlushed(function() {
      shinyjs::enable("agro_apply")
      # Label must match ui_sidebar.R exactly: the button never comes back
      # with different wording.
      updateActionButton(session, "agro_apply", label = "Apply to maps and statistics")
    }, once = TRUE)
    agro_applied(live)
  })

  output$agro_pending_note <- renderUI({
    req(input$color_style == "agro")
    live <- gather_agro_live()
    req(live)
    ap <- agro_applied()
    if (identical(agro_signature(ap), agro_signature(live))) return(NULL)
    # Limits committed for another variable leave this one's maps unclassified.
    msg <- if (is.null(agro_applied_now())) {
      "Class settings are staged: press Apply to maps and statistics to classify the displayed maps and statistics."
    } else {
      "Class settings changed: press Apply to maps and statistics to reflect them on the maps and statistics."
    }
    div(style = "background-color: var(--mn-surface-2); border: 1px solid var(--mn-line); border-left: 2px solid var(--mn-warn); color: var(--mn-text-2); border-radius: var(--mn-radius); padding: 6px 8px; margin-bottom: 6px; font-size: 0.82em;",
        icon("triangle-exclamation"), msg,
        tags$div(style = "margin-top: 4px; font-style: italic;",
          "Applying re-encodes every visible map layer and recomputes class areas - expect a few seconds, longer for multi-locality or comparison views."))
  })

  # Match Scales counts only while its checkbox is on screen
  # (match_scales_shown): the sidebar hides it outside a comparison but it
  # keeps its value, so a box ticked for an earlier comparison went on pooling
  # colour ranges and class breaks with no visible control saying so.
  match_scales_on <- reactive({
    isTRUE(input$match_scales) &&
      match_scales_shown(input$comp_mode, input$value_type, disp_has_pred(), map_view_base())
  })

  # Pooled prediction values of both surfaces under Match Scales. Always the
  # PREDICTION band: class breaks are computed from it, and they describe the
  # concentration surfaces whichever layer the Map Viewer is showing.
  joint_vv <- reactive({
    get_joint_scale_values(rv$rast, rv$rast_pred, match_scales_on(), "value")
  })
  # Display-only twin for the SE/variance views: one colour range across both
  # surfaces' uncertainty layers under Match Scales. Never used for breaks.
  joint_uncert_vv <- reactive({
    layer <- map_view_layer()
    if (!layer %in% c("se", "var")) return(NULL)
    get_joint_scale_values(rv$rast, rv$rast_pred, match_scales_on(), layer)
  })

  # Values ONE surface's class breaks are computed on: that surface's own
  # values, so each map's classes describe the surface it shows, whichever map
  # is on screen; both surfaces pooled under Match Scales, the explicit request
  # for one common scale. Before the first run the raw data column stands in
  # for the Actual surface.
  classification_values <- function(meta, n_min, surface = "act") {
    vv <- joint_vv()
    if (is.null(vv)) {
      vv <- if (identical(surface, "pre")) rast_vals_pre() else rast_vals_act()
    }
    if (is.null(vv) && !identical(surface, "pre")) {
      v_data <- rv$user_data[[meta$actual]]
      if (!is.null(v_data)) vv <- v_data[is.finite(v_data)]
    }
    if (is.null(vv) || length(vv) < n_min) return(NULL)
    vv
  }

  # The inner class breaks of one surface ("act" or "pre"), or NULL. Kept apart
  # from the colours, which follow the live palette: natural breaks over every
  # cell take seconds on a large surface, and a palette change must not
  # recompute them. rv$disp is the committed run context WITHOUT the palette
  # that get_display_meta() adds.
  compute_class_breaks <- function(surface) {
    req(input$color_style %in% c("agro", "bin"))
    # Displayed run's variable when one exists (class breaks must describe the
    # map on screen); live selection as pre-run fallback so the styling
    # controls stay usable before the first interpolation.
    meta <- rv$disp %||% get_current_meta()
    req(meta)

    if (input$color_style == "agro") {
      # Committed snapshot only: live agro inputs never reach the maps/stats
      # until APPLY is pressed (the pending-note UI flags the divergence), and
      # Supervised limits only for the variable they were applied for.
      ap <- agro_applied_now()
      if (is.null(ap)) return(NULL)
      if (ap$method == "limits") return(ap$limits)
      vv <- classification_values(meta, ap$n_classes, surface)
      if (is.null(vv)) return(NULL)
      # Jenks: exact natural breaks over every value; k-means: seeded
      # (calc_class_breaks, spatial_pipeline.R).
      return(calc_class_breaks(vv, ap$n_classes, ap$method))
    }
    n_c <- 5
    vv <- classification_values(meta, n_c, surface)
    if (is.null(vv)) return(NULL)
    rng <- range(vv, na.rm = TRUE)
    if (is.infinite(rng[1]) || is.infinite(rng[2]) || rng[1] == rng[2]) {
      seq(rng[1], rng[1] + 1, length.out = n_c + 1)[2:n_c]
    } else {
      seq(rng[1], rng[2], length.out = n_c + 1)[2:n_c]
    }
  }
  class_breaks_act <- reactive(compute_class_breaks("act"))
  class_breaks_pre <- reactive(compute_class_breaks("pre"))

  # The class definition built on inner breaks: breaks, the reclassification
  # matrix ([low, high) per class), colours and labels.
  build_classification_params <- function(brks_inner) {
    if (is.null(brks_inner)) return(NULL)
    meta <- get_display_meta() %||% get_current_meta()
    req(meta)

    cb <- class_breaks_matrix(brks_inner)
    brks <- cb$brks
    n_c_actual <- cb$n_c
    rcl_mat <- cb$rcl_mat
    # The range of each class, for the legend and the area tables.
    ranges <- class_legend_labels(brks)
    if (input$color_style == "agro") {
      colors <- get_agro_colors(n_c_actual)
      labels <- if (n_c_actual == 3) c("Low", "Med", "High") else paste("Class", seq_len(n_c_actual))
      leg_labels <- if (n_c_actual == 3) paste(labels, ":", ranges) else ranges
    } else {
      is_viridis <- meta$palette == "viridis"
      colors <- if (is_viridis) {
        viridis::viridis(n_c_actual, option = meta$palette)
      } else {
        colorRampPalette(RColorBrewer::brewer.pal(min(8, max(3, n_c_actual)), meta$palette))(n_c_actual)
      }
      labels <- paste("Bin", seq_len(n_c_actual))
      leg_labels <- ranges
    }
    list(brks = brks, rcl_mat = rcl_mat, colors = colors, labels = labels,
         leg_labels = leg_labels, ranges = ranges, n_c = n_c_actual)
  }

  classification_params_act <- reactive(build_classification_params(class_breaks_act()))
  # The Predicted surface's own classes. One definition serves both surfaces
  # where the breaks cannot differ: supervised limits, or Match Scales.
  classification_params_pre <- reactive({
    shared <- match_scales_on() ||
      (identical(input$color_style, "agro") && identical(agro_applied_now()$method, "limits"))
    if (shared) return(classification_params_act())
    build_classification_params(class_breaks_pre())
  })
  classification_params <- function(surface = "act") {
    if (identical(surface, "pre")) classification_params_pre() else classification_params_act()
  }

  # The agreement tables bin each measured value and its uploaded prediction
  # with ONE class definition, or the agreement would compare two different
  # class systems: the Actual surface's.
  agro_params <- reactive({
    req(input$color_style == "agro")
    classification_params_act()
  })

  # ── Session configuration: Save config / Load config ──────────────────────
  volumes <- c(Home = fs::path_home(), Project = getwd())
  shinyFileChoose(input, "load_config", roots = volumes, session = session, filetypes = c("json"))

  observeEvent(input$save_config, {
    showModal(modalDialog(
      title = "Save Session Configuration",
      size = "m",
      easyClose = TRUE,
      footer = modalButton("Cancel"),
      div(style = "padding: 10px;",
          h4("Export the session's settings to a local JSON file:"),
          p("The file holds the column mapping and both coordinate systems, the variable list with its labels, categories and units, the context (localities, variable, view, data subset, comparison settings), the spatial engine with its cross-validation design, covariates and parameters, the manual variogram models and per-locality IDW powers and TPS lambdas you applied, the domain and grid, and the map styling with the colour palette you picked for each variable."),
          p("Load it after loading the same data. Settings that do not match the loaded data are skipped and listed; an uploaded boundary shapefile is not part of the file."),
          hr(),
          div(style = "text-align: center; margin-top: 20px;",
              downloadButton("download_config_json", "DOWNLOAD CONFIGURATION FILE", class = "btn-success btn-lg")
          )
      )
    ))
  })

  # Every run-defining input of the sidebar and the Data Setup tab, the
  # variable list, the palettes picked per variable and the tuning stores.
  # Supervised class limits are saved while their boxes are on screen.
  session_config_snapshot <- function() {
    n_c <- input$agro_n_classes
    lims <- if (isTruthy(n_c) && n_c > 1) {
      lapply(seq_len(n_c - 1), function(i) input[[paste0("agro_limit_", i)]])
    }
    lims <- if (length(lims) && all(vapply(lims, function(v) length(v) == 1 && is.finite(v), logical(1)))) {
      as.numeric(unlist(lims))
    }
    cfg <- list(
      config_version = SESSION_CONFIG_VERSION, app_version = app_version,
      map_x = input$map_x, map_y = input$map_y, map_loc = input$map_loc,
      map_crs = input$map_crs, crs_selection = input$crs_selection,
      vars_mapping = rv$mapping$vars,
      locality = as.character(input$locality %||% character(0)),
      var_category = input$var_category, var_id = input$var_id, value_type = input$value_type,
      subset = input$subset, comp_mode = isTRUE(input$comp_mode), sep_fit = isTRUE(input$sep_fit),
      match_scales = isTRUE(input$match_scales),
      method = input$method, cv_strategy = input$cv_strategy, cv_population = input$cv_population,
      cv_repeat_on = isTRUE(input$cv_repeat_on), cv_repeat_n = input$cv_repeat_n,
      rfk_uncertainty = input$rfk_uncertainty, ck_nmax = input$ck_nmax,
      aux_vars = as.character(input$aux_vars %||% character(0)), vgm_mode = input$vgm_mode,
      idw_mode = input$idw_mode, idw_p_mode = input$idw_p_mode, idw_p = input$idw_p,
      idw_nmax = input$idw_nmax, tps_mode = input$tps_mode,
      tps_lambda_mode = input$tps_lambda_mode, tps_lambda = input$tps_lambda,
      boundary_type = input$boundary_type, buff_mode = input$buff_mode, buff_dist = input$buff_dist,
      res_mode = input$res_mode, grid_res = input$grid_res,
      color_style = input$color_style,
      palettes = config_palettes_out(rv$palette_picks, rv$mapping$vars),
      agro_method = input$agro_method, agro_n_classes = n_c, agro_limits = lims,
      stores = config_stores_out(rv$v_fit_list, rv$idw_factors, rv$tps_lambdas)
    )
    Filter(Negate(is.null), cfg)
  }

  output$download_config_json <- downloadHandler(
    filename = function() {
      paste0("monolith_config_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
    },
    content = function(file) {
      # digits = NA: full precision, or a TPS lambda of 2.4e-06 saves as 0.
      writeLines(jsonlite::toJSON(session_config_snapshot(), auto_unbox = TRUE, pretty = TRUE,
                                  digits = NA, null = "null"), file)
    }
  )

  # A configuration is restored stage by stage, because a parent control
  # re-renders or re-chooses its children: the data mapping; the variable
  # category; the variable; the rest of the context; the engine, domain and
  # styling switches; the controls built for them (covariates, Supervised
  # limits); then the tuning stores, once the data-mapping signature
  # the stores are cleared on (tuning_data_sig, server_model_tuning.R) is
  # current. Each item is re-sent until the browser reports the saved value; an
  # item not accepted within CFG_STAGE_TIMEOUT_S is skipped, and one
  # notification names every skipped setting, those that do not match the
  # loaded data included.
  CFG_TICK_MS <- 250
  CFG_RESEND_S <- 1
  CFG_STAGE_TIMEOUT_S <- 8
  cfg_restore <- new.env(parent = emptyenv())
  cfg_restore_active <- reactiveVal(FALSE)

  # The value a control built by renderUI (aux_vars, agro_limit_*) opens
  # with: the restored one while a restore runs, and after
  # it for a control that was not on screen, until its first render. Read in
  # isolate(), so a restore never re-renders the control itself.
  restore_ui_value <- function(id) {
    v <- cfg_restore$ui[[id]]
    if (!is.null(v) && !isTRUE(cfg_restore$running)) cfg_restore$ui[[id]] <- NULL
    v
  }

  cfg_item_done <- function(it) {
    cur <- input[[it$id]]
    switch(it$kind,
      checkbox = identical(isTRUE(cur), isTRUE(it$value)),
      numeric = , slider = length(cur) == 1 && is.finite(as.numeric(cur)) &&
        abs(as.numeric(cur) - as.numeric(it$value)) <= 1e-9 * max(1, abs(as.numeric(it$value))),
      multi_select = , multi_picker = setequal(as.character(cur %||% character(0)), as.character(it$value)),
      identical(as.character(cur %||% ""), as.character(it$value)))
  }
  cfg_item_send <- function(it) {
    v <- it$value
    switch(it$kind,
      select = , multi_select = updateSelectInput(session, it$id, selected = v),
      picker = , multi_picker = shinyWidgets::updatePickerInput(session, it$id, selected = v),
      radiogroup = shinyWidgets::updateRadioGroupButtons(session, it$id, selected = v),
      radio = updateRadioButtons(session, it$id, selected = v),
      checkbox = updateCheckboxInput(session, it$id, value = isTRUE(v)),
      numeric = updateNumericInput(session, it$id, value = v),
      slider = updateSliderInput(session, it$id, value = v),
      # record = FALSE: a loaded CRS is the user's choice, which the
      # identification observer must not replace.
      crs_in = set_input_crs(v, record = FALSE),
      crs_target = set_target_crs(v, record = FALSE))
  }

  # The stages of one restore, checked against the loaded data: list(stages,
  # vars, ui, built, skipped). A setting that does not match the data is left out and
  # named in `skipped`.
  config_restore_plan <- function(cfg) {
    ud <- rv$user_data
    cols <- names(ud)
    num_cols <- cols[vapply(ud, is.numeric, logical(1))]
    one <- config_scalar
    many <- function(x) as.character(unlist(x) %||% character(0))
    skipped <- character(0)
    skip <- function(fmt, ...) skipped <<- c(skipped, sprintf(fmt, ...))
    item <- function(id, kind, value, quiet = FALSE) list(id = id, kind = kind, value = value, quiet = quiet)
    plain <- function(id, kind) {
      v <- one(cfg[[id]])
      if (!is.null(v)) item(id, kind, if (kind %in% c("numeric", "slider")) as.numeric(v) else v)
    }
    col_item <- function(id, role) {
      v <- one(cfg[[id]])
      if (is.null(v)) return(NULL)
      if (!v %in% cols) {
        skip("%s column %s (not in the data)", role, v)
        return(NULL)
      }
      item(id, "select", v)
    }
    crs_item <- function(id, kind) {
      v <- one(cfg[[id]])
      if (!is.null(v)) item(id, kind, normalize_crs_input(as.character(v)))
    }
    mapping <- list(col_item("map_x", "X"), col_item("map_y", "Y"), col_item("map_loc", "Locality"),
                    crs_item("map_crs", "crs_in"), crs_item("crs_selection", "crs_target"))

    vars <- NULL
    if (!is.null(cfg$vars_mapping)) {
      vin <- config_vars_in(cfg$vars_mapping, cols)
      if (length(vin$skipped)) skip("variables not in the data (%s)", paste(vin$skipped, collapse = ", "))
      if (length(vin$vars)) vars <- vin$vars
    }
    vars_now <- vars %||% rv$mapping$vars
    find_var <- function(id) Find(function(v) identical(as.character(v$actual), as.character(id)), vars_now)

    # The variable decides its category, so the two cannot disagree.
    var_v <- one(cfg$var_id)
    var_e <- if (!is.null(var_v)) find_var(var_v)
    if (!is.null(var_v) && is.null(var_e)) skip("variable %s (not in the variable list)", var_v)
    cat_v <- if (!is.null(var_e)) as.character(var_e$category) else one(cfg$var_category)
    cats <- unique(vapply(vars_now, function(v) as.character(v$category %||% ""), character(1)))
    if (!is.null(cat_v) && !cat_v %in% cats) {
      skip("category %s (not in the variable list)", cat_v)
      cat_v <- NULL
    }

    ve <- var_e %||% find_var(input$var_id)
    has_pred <- !is.null(ve) && is_valid_col_ref(ve$pred)
    has_ss <- !is.null(ve) && is_valid_col_ref(ve$pred_ss)
    vt <- one(cfg$value_type)
    if (!is.null(vt) && !vt %in% c("actual", if (has_pred) c("pred", "resid"), if (has_ss) "pred_ss")) {
      skip("view %s (the variable has no such column)", vt)
      vt <- NULL
    }
    sub_col <- find_subset_column(cols)
    sb <- one(cfg$subset)
    if (!is.null(sb) && !sb %in% c("all", if (!is.na(sub_col)) as.character(ud[[sub_col]]))) {
      skip("data subset %s (not in the data)", sb)
      sb <- NULL
    }
    loc_col <- one(cfg$map_loc)
    if (is.null(loc_col) || !loc_col %in% cols) loc_col <- rv$mapping$loc
    data_locs <- if (!is.null(loc_col) && loc_col %in% cols) unique(as.character(ud[[loc_col]])) else character(0)
    loc_item <- NULL
    if (!is.null(cfg$locality)) {
      want <- many(cfg$locality)
      miss <- setdiff(want, c("ALL", data_locs))
      if (length(miss)) skip("localities not in the data (%s)", paste(miss, collapse = ", "))
      keep <- intersect(want, c("ALL", data_locs))
      if (length(keep) || !length(want)) loc_item <- item("locality", "multi_select", keep)
    }
    context <- list(
      if (!is.null(vt)) item("value_type", "radiogroup", vt),
      if (!is.null(sb)) item("subset", "select", sb),
      if (!is.null(cfg$comp_mode)) item("comp_mode", "checkbox", isTRUE(one(cfg$comp_mode)) && (has_pred || has_ss)),
      plain("sep_fit", "checkbox"), plain("match_scales", "checkbox"),
      loc_item)

    switches <- list(
      plain("method", "radiogroup"), plain("cv_strategy", "radiogroup"),
      plain("cv_population", "radiogroup"), plain("cv_repeat_on", "checkbox"),
      if (!is.null(one(cfg$cv_repeat_n))) item("cv_repeat_n", "select", as.character(one(cfg$cv_repeat_n))),
      plain("rfk_uncertainty", "radio"), plain("ck_nmax", "slider"), plain("vgm_mode", "radiogroup"),
      plain("idw_mode", "radio"), plain("idw_p_mode", "radio"), plain("idw_p", "numeric"),
      plain("idw_nmax", "slider"), plain("tps_mode", "radio"), plain("tps_lambda_mode", "radio"),
      plain("tps_lambda", "numeric"),
      plain("boundary_type", "radiogroup"), plain("buff_mode", "radiogroup"),
      plain("buff_dist", "numeric"), plain("res_mode", "radiogroup"), plain("grid_res", "slider"),
      plain("color_style", "radiogroup"), plain("agro_method", "select"),
      plain("agro_n_classes", "slider"))

    # Controls built by renderUI for the settings above: covariates only where
    # the method shows them, the Supervised limits at the saved class count.
    # The limits live in a sidebar section that may be collapsed, so they are
    # not reported when their boxes are not on screen to accept them.
    method <- one(cfg$method) %||% input$method
    population <- one(cfg$cv_population) %||% input$cv_population
    style <- one(cfg$color_style) %||% input$color_style
    built <- list(); ui <- list()
    if (!is.null(cfg$aux_vars) &&
        (method %in% c("RK", "RFK", "CK") || (identical(method, "OK") && identical(population, "comparable")))) {
      want <- many(cfg$aux_vars)
      offered <- setdiff(num_cols, c(one(cfg$map_x) %||% input$map_x, one(cfg$map_y) %||% input$map_y,
                                     var_v %||% input$var_id))
      miss <- setdiff(want, offered)
      if (length(miss)) skip("covariates not in the data (%s)", paste(miss, collapse = ", "))
      built <- c(built, list(item("aux_vars", "multi_picker", intersect(want, offered))))
      ui$aux_vars <- intersect(want, offered)
    }
    # Palettes are kept per variable, apart from the picker, so none is a
    # control to re-send.
    pals <- config_palettes_in(cfg$palettes, vars_now, legacy_palette = one(cfg$palette_select),
                               legacy_var = if (!is.null(var_e)) var_v)
    skipped <- c(skipped, pals$skipped)
    lims <- suppressWarnings(as.numeric(unlist(cfg$agro_limits)))
    n_c <- one(cfg$agro_n_classes)
    if (length(lims) && identical(style, "agro") &&
        identical(one(cfg$agro_method) %||% input$agro_method, "limits") &&
        !is.null(n_c) && length(lims) == as.integer(n_c) - 1L && all(is.finite(lims))) {
      for (i in seq_along(lims)) {
        id <- paste0("agro_limit_", i)
        built <- c(built, list(item(id, "numeric", lims[i], quiet = TRUE)))
        ui[[id]] <- lims[i]
      }
    }

    stages <- lapply(list(mapping,
                          if (!is.null(cat_v)) list(item("var_category", "select", cat_v)),
                          if (!is.null(var_e)) list(item("var_id", "picker", var_v)),
                          context, switches, built),
                     function(s) Filter(Negate(is.null), s))
    stages <- Filter(length, stages)

    if (!is.null(cfg$stores)) {
      stores <- config_stores_in(cfg$stores, data_locs)
      if (length(stores$skipped)) {
        skip("tuning values of localities not in the data (%s)", paste(stores$skipped, collapse = ", "))
      }
      stages <- c(stages, list(function() {
        sig <- list(rv$user_data, rv$mapping$x, rv$mapping$y, rv$mapping$loc, rv$mapping$crs)
        if (!identical(tuning_data_sig, sig)) return(FALSE)
        fits <- Filter(function(m) !identical(attr(m, "monolith_source"), "manual"), rv$v_fit_list)
        fits[names(stores$v_fit_list)] <- stores$v_fit_list
        rv$v_fit_list <- fits
        rv$idw_factors <- stores$idw_factors
        rv$tps_lambdas <- stores$tps_lambdas
        rv$tuning_revision <- (rv$tuning_revision %||% 0L) + 1L
        TRUE
      }))
    }
    list(stages = stages, vars = vars, palettes = pals$picks, ui = ui, built = built,
         skipped = skipped)
  }

  cfg_restore_next_stage <- function() {
    cfg_restore$i <- cfg_restore$i + 1L
    cfg_restore$stage_t0 <- NULL
  }

  cfg_restore_finish <- function() {
    cfg_restore_active(FALSE)
    cfg_restore$running <- FALSE
    # A confirmed control needs no opening value; one that was not on screen
    # keeps it for its first render (restore_ui_value).
    for (it in cfg_restore$built) if (isTRUE(cfg_item_done(it))) cfg_restore$ui[[it$id]] <- NULL
    removeNotification("cfg_restore")
    sk <- cfg_restore$skipped
    if (length(sk)) {
      showNotification(paste0("Configuration restored, except: ", paste(sk, collapse = "; "), "."),
                       type = "warning", duration = 20)
    } else {
      showNotification("Configuration restored.", type = "message", duration = 5)
    }
  }

  cfg_restore_tick <- function() {
    if (cfg_restore$i > length(cfg_restore$stages)) return(cfg_restore_finish())
    stage <- cfg_restore$stages[[cfg_restore$i]]
    now <- Sys.time()
    if (is.null(cfg_restore$stage_t0)) cfg_restore$stage_t0 <- now
    late <- as.numeric(difftime(now, cfg_restore$stage_t0, units = "secs")) > CFG_STAGE_TIMEOUT_S
    if (is.function(stage)) {
      if (isTRUE(stage())) return(cfg_restore_next_stage())
      if (late) {
        cfg_restore$skipped <- c(cfg_restore$skipped, "the tuning stores (the data mapping did not settle)")
        cfg_restore_next_stage()
      }
      return()
    }
    open <- Filter(Negate(cfg_item_done), stage)
    if (!length(open)) return(cfg_restore_next_stage())
    if (late) {
      loud <- Filter(function(it) !isTRUE(it$quiet), open)
      if (length(loud)) {
        cfg_restore$skipped <- c(cfg_restore$skipped, vapply(loud, function(it) {
          sprintf("%s = %s (not accepted)", it$id, paste(it$value, collapse = ", "))
        }, character(1)))
      }
      return(cfg_restore_next_stage())
    }
    for (it in open) {
      last <- cfg_restore$sent[[it$id]]
      if (is.null(last) || as.numeric(difftime(now, last, units = "secs")) >= CFG_RESEND_S) {
        cfg_item_send(it)
        cfg_restore$sent[[it$id]] <- now
      }
    }
  }

  observe({
    if (!isTRUE(cfg_restore_active())) return()
    invalidateLater(CFG_TICK_MS)
    isolate(cfg_restore_tick())
  })

  observeEvent(input$load_config, {
    req(input$load_config)
    file_info <- shinyFiles::parseFilePaths(volumes, input$load_config)
    req(nrow(file_info) > 0)
    cfg <- tryCatch(jsonlite::fromJSON(file_info$datapath[1], simplifyVector = FALSE), error = function(e) {
      showNotification(paste("Failed to load configuration:", conditionMessage(e)), type = "error", duration = 7)
      NULL
    })
    req(cfg)
    # A run record carries no session field, so it would "load" as nothing.
    refusal <- session_config_refusal(cfg)
    if (!is.null(refusal)) {
      showNotification(refusal, type = "error", duration = 15)
      return()
    }
    if (is.null(rv$user_data)) {
      showNotification(paste("Load the data first: a session configuration restores the column mapping,",
                             "localities and settings of a loaded table."), type = "error", duration = 10)
      return()
    }
    plan <- config_restore_plan(cfg)
    if (!is.null(plan$vars)) rv$mapping$vars <- plan$vars
    rv$palette_picks <- plan$palettes
    cfg_restore$stages <- plan$stages
    cfg_restore$built <- plan$built
    cfg_restore$ui <- plan$ui
    cfg_restore$skipped <- plan$skipped
    cfg_restore$sent <- list()
    cfg_restore$i <- 1L
    cfg_restore$stage_t0 <- NULL
    cfg_restore$running <- TRUE
    showNotification("Restoring the configuration...", id = "cfg_restore", type = "message", duration = NULL)
    cfg_restore_active(TRUE)
  })

  # The picker styles the displayed run's variable once a run exists, so a
  # context change cannot restyle the map on screen, and it opens on that
  # variable's palette (palette_of): a run, a Styling switch or a restore redraws
  # it without undoing a pick.
  output$palette_ui <- renderUI({
    vid <- palette_var()
    req(vid, rv$mapping$vars)
    # Agronomical styling supplies its own class palette, so the manual
    # colour-palette picker is irrelevant there — hide it.
    if (isTruthy(input$color_style) && input$color_style == "agro") return(NULL)
    if (!any(vapply(rv$mapping$vars, function(x) identical(x$actual, vid), logical(1)))) return(NULL)
    choices <- palette_choices_precomputed
    pickerInput("palette_select", "Color Palette",
                choices = choices,
                selected = palette_of(vid),
                options = list(`live-search` = TRUE),
                choicesOpt = list(content = names(choices)))
  })

  # A pick belongs to the variable whose map it restyles and, when the sidebar
  # already names another variable for the next run, to that one too, so the
  # next run uses what the picker shows. A value equal to the palette the
  # picker was drawn with is its redraw arriving, not a pick.
  observeEvent(input$palette_select, {
    pal <- as.character(input$palette_select)
    vid <- palette_var()
    req(isTRUE(pal %in% dashboard_palettes), vid)
    if (identical(pal, palette_of(vid))) return()
    picks <- rv$palette_picks %||% list()
    for (v in unique(c(vid, input$var_id))) if (isTruthy(v)) picks[[v]] <- pal
    rv$palette_picks <- picks
  })

  # The values the Actual surface's classes are cut from: the displayed run's
  # surface (cached per run), before a run the data column.
  actual_surface_values <- function(meta) {
    vv <- rast_vals_act()
    if (is.null(vv)) {
      v_data <- rv$user_data[[meta$actual]]
      vv <- if (is.numeric(v_data)) v_data[is.finite(v_data)] else numeric(0)
    }
    vv
  }

  # The limits the Supervised boxes open with (class_limit_defaults): the
  # reference limits of a recognised nutrient at three classes, else the data
  # quantiles of the Actual surface. The displayed run's variable and unit once
  # a run exists, so a sidebar context change does not reset them.
  agro_limit_defaults <- reactive({
    n_c <- input$agro_n_classes
    req(isTruthy(n_c))
    meta <- rv$disp %||% get_current_meta()
    req(meta)
    class_limit_defaults(meta$actual, meta$unit, n_c, actual_surface_values(meta))
  })

  # The variable and class count the Supervised boxes were last built for. The
  # boxes are rebuilt whenever their defaults move (a run lands, the view
  # changes before a run); for the same variable and class count they keep the
  # values on screen, typed or applied, and open on the defaults only for a new
  # variable or class count. Reopening on the defaults would leave the maps on
  # the applied limits and the pending note reporting a change nobody made.
  agro_box_ctx <- NULL
  output$agro_options <- renderUI({
    req(input$color_style == "agro", input$agro_method == "limits")
    meta <- rv$disp %||% get_current_meta()
    req(meta)
    n_c <- input$agro_n_classes
    d <- agro_limit_defaults()
    # Reference limits exactly as published; data quantiles at the four
    # significant digits every displayed number uses.
    shown <- if (identical(d$source, "reference")) d$limits else signif(d$limits, 4)
    ctx <- list(meta$actual, as.integer(n_c))
    keep <- identical(ctx, agro_box_ctx)
    agro_box_ctx <<- ctx

    # Range hint of each surface the limits will cut (cached per run) so
    # sensible thresholds can be typed without leaving the sidebar.
    rng_note <- tryCatch({
      act_v <- actual_surface_values(meta)
      pre_v <- rast_vals_pre()
      rng_line <- function(lab, v) {
        if (length(v)) sprintf("%s surface range: %s - %s", lab, format_sig(min(v)), format_sig(max(v)))
      }
      lines <- c(rng_line("Actual", act_v), rng_line("Predicted", pre_v))
      if (!length(lines)) NULL else {
        tags$small(style = "display:block; color: var(--mn-text-3); margin-bottom:4px;",
                   HTML(paste(htmltools::htmlEscape(lines), collapse = "<br>")))
      }
    }, error = function(e) NULL)

    tagList(
      tags$label(class = "control-label", "Class limits",
                 info_tooltip("agro_limits_info", "Default limits from published agronomic classes; sources, extraction methods and units are listed in the Scientific Guide, section 9.4.1 (Reference class limits). Each class holds its lower limit.")),
      rng_note,
      lapply(seq_len(n_c - 1), function(i) {
        id <- paste0("agro_limit_", i)
        cur <- if (keep) isolate(input[[id]])
        numericInput(id, paste("Limit", i),
                     value = isolate(restore_ui_value(id)) %||%
                       (if (isTRUE(is.finite(cur))) cur) %||% shown[i])
      }),
      tags$small(style = "display:block; color: var(--mn-text-3); margin: -4px 0 6px 0;",
                 HTML(paste(htmltools::htmlEscape(class_limit_note(d, meta$unit)), collapse = "<br>")))
    )
  })

  # The run owns rv$loc_names; a variogram tuning session adds the localities
  # it tuned without rewriting the displayed run's list.
  output$locality_selector_ui <- renderUI({
    tuned <- if (isTRUE(sci_vgm_tuning())) {
      tuned_names <- sub("_(act|pre)$", "", names(tuning_vgm_entries(rv$v_fit_list)))
      locs <- resolve_selected_localities(input$locality, rv$user_data, rv$mapping$loc)
      locs[locs %in% tuned_names]
    }
    choices <- sci_locality_choices(rv$loc_names, tuned)
    req(length(choices) > 0)
    current <- isolate(input$sel_loc_stats)
    selected <- if (isTruthy(current) && current %in% choices) current else choices[1]
    selectInput("sel_loc_stats", "Filter Analysis View:", choices = choices, selected = selected)
  })
  
  output$sci_multiple_localities <- renderText({
    if (length(rv$loc_names) > 1) "yes" else "no"
  })
  outputOptions(output, "sci_multiple_localities", suspendWhenHidden = FALSE)

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
                selected = isolate(restore_ui_value("aux_vars")),
                options = list(`live-search` = TRUE, `actions-box` = TRUE))
  })

  corr_source_value <- reactiveVal("predictions")
  corr_subset_value <- reactiveVal("all")
  # Reset the source when Primary View changes. The server state is authoritative
  # while the updated control makes its browser round trip.
  observeEvent(input$value_type, {
    corr_source_value("predictions")
    shinyWidgets::updateRadioGroupButtons(session, "corr_source", selected = "predictions")
  }, priority = 10)
  observeEvent(input$corr_source, {
    req(input$corr_source %in% c("predictions", "actual"))
    corr_source_value(input$corr_source)
  })

  corr_subset_choices <- reactive({
    req(rv$user_data)
    col <- find_subset_column(names(rv$user_data))
    vals <- if (!is.na(col)) sort(unique(na.omit(as.character(rv$user_data[[col]])))) else character()
    c("All" = "all", setNames(vals, vals))
  })
  observeEvent(list(input$value_type, input$subset, corr_subset_choices()), {
    choices <- corr_subset_choices()
    selected <- input$subset %||% "all"
    if (!selected %in% choices) selected <- "all"
    corr_subset_value(selected)
    shinyWidgets::updateRadioGroupButtons(session, "corr_subset", choices = choices, selected = selected)
  }, priority = 10)
  observeEvent(input$corr_subset, {
    req(input$corr_subset %in% corr_subset_choices())
    corr_subset_value(input$corr_subset)
  })
  output$corr_subset_ui <- renderUI({
    req(input$value_type == "pred_ss")
    div(class = "mn-seg-grid",
        shinyWidgets::radioGroupButtons("corr_subset", "Correlation data subset",
          choices = corr_subset_choices(), selected = isolate(corr_subset_value()), size = "sm"))
  })

  corr_ranks <- reactive({
    req(rv$user_data, input$var_id, isTruthy(input$calc_corr) && input$calc_corr > 0)
    # Button starts the screen; changing its context then refreshes it. No fit,
    # future or interpolation worker is involved in this bivariate calculation.
    tryCatch(rank_auxiliary_correlations(rv$user_data, rv$mapping, input$var_id,
      input$value_type %||% "actual", corr_source_value(), input$locality, corr_subset_value()),
      error = function(e) list(error = conditionMessage(e)))
  })

  corr_display_data <- reactive({
    ranks <- corr_ranks()
    if (!is.null(ranks$error)) return(NULL)
    df <- ranks$results
    df$Label <- get_var_labels(df$Variable, rv$mapping$vars)
    df$Category <- vapply(df$Variable, function(v) {
      m <- Filter(function(x) identical(x$actual, v), rv$mapping$vars)
      if (length(m) && isTruthy(m[[1]]$category)) m[[1]]$category else "Uploaded Data"
    }, character(1))
    df
  })
  output$corr_category_ui <- renderUI({
    df <- corr_display_data()
    req(df, nrow(df) > 0)
    cats <- sort(unique(df$Category))
    current <- isolate(input$corr_category)
    selected <- if (isTruthy(current) && current %in% cats) current else "__all__"
    selectInput("corr_category", "Predictor category", choices = c("All categories" = "__all__", setNames(cats, cats)),
                selected = selected)
  })

  output$corr_results_ui <- renderUI({
    ranks <- corr_ranks()
    if (!is.null(ranks$error)) return(div(class = "mn-corr-results", tags$p(ranks$error)))
    tagList(
      div(class = "mn-corr-results",
        tags$h6("Predictor correlations"),
        tags$dl(class = "mn-corr-scope",
          tags$dt("Target"), tags$dd(paste0(ranks$source, ": ", ranks$target)),
          tags$dt("Localities"), tags$dd(ranks$scope),
          tags$dt("Rows"), tags$dd(paste0(ranks$n, " | Subset: ", if (ranks$subset == "all") "All" else ranks$subset))),
        uiOutput("corr_category_ui"),
        uiOutput("corr_table_ui"),
        tags$p(class = "mn-corr-note", "Pearson r, ordered by |r|. n = finite paired observations. Raw two-sided p-values are exploratory; spatial dependence and multiple screening are not adjusted."),
        if (length(ranks$skipped)) tags$details(class = "mn-corr-note",
          tags$summary(sprintf("%d predictors unavailable", length(ranks$skipped))),
          tags$p("Fewer than 3 finite pairs or a constant variable: ", paste(ranks$skipped, collapse = ", ")))
      )
    )
  })
  output$corr_table_ui <- renderUI({
    df <- corr_display_data()
    req(df)
    thresh <- as.numeric(input$corr_pval_thresh %||% 1)
    df <- df[is.finite(df$Pval) & df$Pval <= thresh, , drop = FALSE]
    category <- input$corr_category %||% "__all__"
    if (category != "__all__" && category %in% corr_display_data()$Category) {
      df <- df[df$Category == category, , drop = FALSE]
    }
    if (nrow(df) == 0) return(tags$p("No predictors meet this filter or have enough varying paired observations."))
    div(class = "mn-corr-table-wrap", tabindex = "0", role = "region", `aria-label` = "Predictor correlation ranks",
      tags$table(class = "mn-corr-table",
        tags$caption(sprintf("%d predictors", nrow(df))),
        tags$thead(tags$tr(tags$th("Predictor", scope = "col"), tags$th("r", scope = "col"),
                          tags$th("p (raw)", scope = "col"), tags$th("n", scope = "col"))),
        tags$tbody(lapply(seq_len(nrow(df)), function(i) {
          tags$tr(tags$td(title = df$Variable[i], df$Label[i]),
                  tags$td(sprintf("%+.3f", df$Corr[i])),
                  tags$td(title = format(df$Pval[i], digits = 6), format_p_value(df$Pval[i])),
                  tags$td(df$N[i]))
        }))
      )
    )
  })
