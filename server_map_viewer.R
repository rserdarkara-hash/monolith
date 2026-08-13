# server_map_viewer.R (sourced with local = TRUE inside server) - draw_map,
# proxy-managed overlays (keyed on map_overlay_rev), view switcher and the
# main/comparison renderLeaflet blocks.
  output$loc_res_table <- renderTable({
    req(rv$loc_resolutions)
    res_list <- rv$loc_resolutions
    if(length(res_list) == 0) return(NULL)
    
    show_buffer <- input$boundary_type %in% c("wrapped", "strict")
    res_mode_val <- input$res_mode %||% "local"
    manual_res_val <- input$grid_res %||% 50
    
    df <- data.frame(
      Locality = names(res_list),
      Resolution = sapply(res_list, function(x) {
        if (res_mode_val == "fixed") {
          paste0(round(manual_res_val, 1), " m")
        } else {
          if (is.numeric(x)) paste0(round(x, 1), " m") else x
        }
      })
    )
    
    if (show_buffer) {
      buff_mode_val <- input$buff_mode %||% "dynamic"
      method_val <- input$method %||% "OK"
      fixed_dist <- input$buff_dist %||% 250
      
      df$`Buffer (m)` <- sapply(res_list, function(x) {
        if (res_mode_val == "fixed") {
          base_res <- manual_res_val
        } else {
          if (!is.numeric(x)) return("-")
          base_res <- x
        }
        
        if (buff_mode_val == "dynamic" && input$boundary_type == "wrapped") {
          val <- get_buffer_multiplier(method_val) * base_res
          val <- max(5, min(2000, val))
          paste0(round(val, 1), " m")
        } else {
          paste0(fixed_dist, " m")
        }
      })
    }
    df
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")

  # Normalise a raster / packed raster / list-of-rasters argument into a
  # plain list with NULL entries dropped (shared by draw_map and the proxy
  # restyler).
  as_raster_list <- function(r_obj) {
    if (is.null(r_obj)) return(list())
    r_list <- if (inherits(r_obj, "SpatRaster") || inherits(r_obj, "PackedSpatRaster")) list(r_obj) else r_obj
    Filter(Negate(is.null), r_list)
  }

  # Styling signature per map widget: stamped by draw_map on a full render
  # and checked by the proxy restyler, so a styling tick that matches what a
  # map already shows (e.g. the invalidation wave right after a run
  # completes, when the widgets have just re-rendered) costs nothing.
  map_style_sig <- new.env(parent = emptyenv())

  current_style_sig <- function(lab, class_params) {
    # Effective style: agro/bin without computable class params renders the
    # continuous fallback, so it must share the continuous signature (e.g.
    # selecting Agronomical before pressing APPLY changes nothing on screen).
    style_eff <- input$color_style %||% "cont"
    if (style_eff %in% c("agro", "bin") && is.null(class_params)) style_eff <- "cont"
    paste(
      rv$run_counter, lab,
      style_eff,
      input$palette_select %||% "",
      isTRUE(input$show_uncertainty), input$uncertainty_type %||% "",
      isTRUE(input$match_scales),
      if (!is.null(class_params)) paste(signif(class_params$brks, 10), collapse = ",") else "none",
      sep = "|"
    )
  }

  # Adds/replaces the raster image layers and their legend on `m`, which may
  # be a fresh leaflet widget (initial render inside draw_map) or a
  # leafletProxy (in-place restyle). Stable layerIds make proxy re-adds
  # REPLACE the previous images/legend instead of stacking, so a styling
  # change costs one PNG encode per layer - never a widget rebuild (tile
  # refetch, toolbar, overlay re-adds).
  style_map_rasters <- function(m, r_list, lab) {
    meta <- get_display_meta()
    if (is.null(meta) || length(r_list) == 0) return(m)

    r_names <- names(r_list)
    layer_key <- function(i) {
      r_name <- if (!is.null(r_names) && length(r_names) >= i && !is.na(r_names[i]) && r_names[i] != "") r_names[i] else as.character(i)
      paste0(rv$run_counter, "_", lab, "_", r_name)
    }
    img_id <- function(i) paste0("rast_img_", i)
    legend_id <- "rast_legend"
    select_active_layer <- function(r_w) {
      is_uncertainty <- isTruthy(input$show_uncertainty) && meta$method %in% c("OK", "RK", "RFK", "CK") && "var1.var" %in% names(r_w)
      if (is_uncertainty) {
        al <- r_w[["var1.var"]]
        if (input$uncertainty_type == "se") sqrt(al) else al
      } else {
        if("var1.pred" %in% names(r_w)) r_w[["var1.pred"]] else r_w[[1]]
      }
    }
    # Lazy palette-domain extraction: only the continuous branch reads the
    # cell values (classified and residual branches build their own
    # domains), so classified styling ticks skip a per-locality values()
    # pass entirely.
    get_vv_scale <- function() {
      jv <- joint_vv()
      if (!is.null(jv)) return(jv)
      unlist(lapply(seq_along(r_list), function(i) {
        r_proj <- get_projected_raster(r_list[[i]], layer_key(i))
        if (is.null(r_proj)) return(NULL)
        as.vector(values(select_active_layer(r_proj), na.rm=TRUE))
      }))
    }

    is_viridis <- meta$palette == "viridis"
    is_uncert_view <- isTruthy(input$show_uncertainty) && meta$method %in% c("OK", "RK", "RFK", "CK")
    legend_title <- if (is_uncert_view) {
      if (input$uncertainty_type == "se") {
        paste0("SE: ", meta$label, if (nzchar(meta$unit)) paste0(" ", meta$unit) else "")
      } else {
        paste0("Variance: ", meta$label, if (nzchar(meta$unit)) paste0(" (", meta$unit, ")^2") else " (squared units)")
      }
    } else paste(meta$label, meta$unit)

    if(lab == "resid_raster") {
      # The residual view always displays the var1.pred difference, so the
      # palette domain must come from that layer too (vv would hold the
      # var1.var difference when show_uncertainty is on)
      resid_layers <- list()
      for (i in seq_along(r_list)) {
        r_w <- get_projected_raster(r_list[[i]], layer_key(i))
        if (is.null(r_w)) next
        resid_layers[[length(resid_layers) + 1]] <- if("var1.pred" %in% names(r_w)) r_w[["var1.pred"]] else r_w[[1]]
      }
      vv_resid <- unlist(lapply(resid_layers, function(al) as.vector(values(al, na.rm = TRUE))))
      abs_max <- max(abs(vv_resid), na.rm = TRUE)
      if(is.infinite(abs_max) || is.na(abs_max)) abs_max <- 1
      pal <- colorNumeric("RdBu", domain = c(-abs_max, abs_max), na.color = "transparent")

      for (i in seq_along(resid_layers)) {
        m <- m %>% addRasterImage(resid_layers[[i]], colors = pal, opacity = 0.8, project = FALSE, layerId = img_id(i))
      }
      m <- m %>% leaflet::addLegend(pal = pal, values = c(-abs_max, abs_max), title = paste("Resid:", meta$label), layerId = legend_id)
    } else {
      # Classified styling when requested AND computable; any failure or
      # not-yet-applied class breaks fall back to the continuous palette so
      # the viewer NEVER renders an empty base map over a completed run.
      class_params <- if (input$color_style %in% c("agro", "bin") && !is_uncert_view) {
        tryCatch(classification_params(), error = function(e) NULL)
      } else NULL

      if (!is.null(class_params)) {
        pal <- colorBin(class_params$colors, bins = class_params$brks, na.color = "transparent", right = FALSE)

        for (i in seq_along(r_list)) {
          r_w <- get_projected_raster(r_list[[i]], layer_key(i))
          if (is.null(r_w)) next
          m <- m %>% addRasterImage(select_active_layer(r_w), colors = pal, opacity = 0.8, project = FALSE, layerId = img_id(i))
        }
        m <- m %>% leaflet::addLegend(colors = class_params$colors, labels = class_params$leg_labels, opacity = 0.8, title = paste(meta$label, meta$unit), layerId = legend_id)
      } else {
        vv_scale <- get_vv_scale()
        pal <- if(is_viridis) colorNumeric(viridis::viridis(256, option = meta$palette), vv_scale, na.color = "transparent")
               else colorNumeric(meta$palette, vv_scale, na.color = "transparent")

        for (i in seq_along(r_list)) {
          r_w <- get_projected_raster(r_list[[i]], layer_key(i))
          if (is.null(r_w)) next
          m <- m %>% addRasterImage(select_active_layer(r_w), colors = pal, opacity = 0.8, project = FALSE, layerId = img_id(i))
        }

        v_range <- diff(range(vv_scale, na.rm=TRUE))
        d_format <- if(is.na(v_range)) 2 else if(v_range < 0.01) 6 else if(v_range < 0.1) 4 else 2
        m <- m %>% leaflet::addLegend(pal = pal, values = vv_scale, title = legend_title, labFormat = labelFormat(digits = d_format), layerId = legend_id)
      }
    }
    m
  }

  # draw_map builds the full widget only for STRUCTURAL changes (new run,
  # view switch): styling reads are isolate()d here, and pure styling ticks
  # (palette, continuous/binned/agro, class breaks, uncertainty toggle) are
  # handled by the proxy restyler observer below. Cheap overlays (styled
  # points, borders, north arrow, scale, resolution box, base tiles) are
  # applied by leafletProxy observers keyed on map_overlay_rev.
  draw_map <- function(r_obj, lab, map_id = NULL) {
    current_tiles <- isolate(input$base_map_layer) %||% "Esri.WorldImagery"

    if((is.null(r_obj) || (is.list(r_obj) && length(r_obj) == 0)) && lab != "resid_points") {
      # Placeholder map (no raster layers yet, e.g. mid-run): a Leaflet widget
      # without layers has no auto-fit limits, and a map that never receives a
      # view never loads tiles - it stays solid gray. Always give it one.
      if (!is.null(map_id) && exists(map_id, envir = map_style_sig)) rm(list = map_id, envir = map_style_sig)
      m0 <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% addProviderTiles(current_tiles, layerId="base_tiles")
      bb <- run_area_bbox()
      m0 <- if (!is.null(bb)) {
        m0 %>% fitBounds(as.numeric(bb$xmin), as.numeric(bb$ymin), as.numeric(bb$xmax), as.numeric(bb$ymax))
      } else {
        m0 %>% setView(lng = 0, lat = 0, zoom = 2)
      }
      return(m0)
    }
    
    m <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% addProviderTiles(current_tiles, layerId="base_tiles") %>%
      leaflet.extras::addDrawToolbar(
        targetGroup = "drawn_features",
        polylineOptions = FALSE,
        polygonOptions = drawPolygonOptions(),
        circleOptions = FALSE,
        rectangleOptions = drawRectangleOptions(),
        markerOptions = drawMarkerOptions(),
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
      )
    meta <- isolate(get_display_meta())
    req(meta)

    if(!is.null(r_obj) && !(is.list(r_obj) && length(r_obj) == 0)) {
      r_list <- as_raster_list(r_obj)

      if (length(r_list) > 0) {
        vgm_target <- if (lab %in% c("actual", "Actual")) "act"
                      else if (lab %in% c("pred", "pred_ss", "Predicted")) "pre"
                      else NULL  # residual maps derive from both fits
        vgm_warn_html <- build_vgm_warning_html(rv$v_fit_list, target = vgm_target)
        if (!is.null(vgm_warn_html)) {
          m <- m %>% addControl(html = vgm_warn_html, position = "bottomleft")
        }
        # Styling reads (palette, class breaks, uncertainty toggles) are
        # isolated: a styling tick must invalidate only the proxy restyler,
        # never this full widget build.
        isolate({
          m <- style_map_rasters(m, r_list, lab)
          if (!is.null(map_id)) {
            cp <- if ((input$color_style %||% "cont") %in% c("agro", "bin")) {
              tryCatch(classification_params(), error = function(e) NULL)
            } else NULL
            assign(map_id, current_style_sig(lab, cp), envir = map_style_sig)
          }
        })
      }
    }


    if(lab == "resid_points") {
       req(rv$sf, "resid" %in% colnames(rv$sf))
       pts_view <- st_transform(rv$sf, 4326)
       abs_max_p <- max(abs(pts_view$resid), na.rm=T)
       if(is.infinite(abs_max_p) || is.na(abs_max_p)) abs_max_p <- 1
       pal_pts <- colorNumeric("RdBu", domain = c(-abs_max_p, abs_max_p), na.color = "black")
       df_clean <- st_drop_geometry(pts_view)
       popup_builder <- make_popup_fn(colnames(df_clean))
       popups <- vapply(seq_len(nrow(df_clean)), function(i) popup_builder(df_clean[i, ]), character(1))
       
       m <- m %>% addCircleMarkers(data = pts_view, radius = 5, color = "black", weight = 1,
                                  fillColor = ~pal_pts(resid), fillOpacity = 0.9,
                                  popup = popups)
       m <- m %>% leaflet::addLegend(pal = pal_pts, values = c(-abs_max_p, abs_max_p), title = paste("Point Resid:", meta$label))
    }
    
    m
  }

  # ── Proxy restyler: styling ticks swap raster images + legend in place ──
  # Invalidated by the pure styling inputs (palette, styling mode, APPLIED
  # class breaks, uncertainty toggles, match scales). Structural changes
  # (new run, view switch) still go through the full renderLeaflet path,
  # which stamps map_style_sig; priority = -10 runs this AFTER the outputs
  # in the same flush, so a freshly rendered widget is never re-encoded.
  observe({
    # Styling dependencies, registered unconditionally so the observer is
    # armed even before the first run.
    style_now <- input$color_style %||% "cont"
    input$palette_select; input$show_uncertainty; input$uncertainty_type; input$match_scales
    cp <- if (style_now %in% c("agro", "bin")) tryCatch(classification_params(), error = function(e) NULL) else NULL

    if (is.null(rv$disp)) return(invisible(NULL))
    view <- input$map_view %||% "view_act"

    targets <- list()
    if (view %in% c("view_act", "view_pred") && isTRUE(session_state$main_map_rendered)) {
      lab <- if (view == "view_pred") {
        if (identical(rv$disp$value_type, "pred_ss")) "pred_ss" else "pred"
      } else "actual"
      targets$main_map <- list(r = if (view == "view_pred") rv$rast_list_pre else rv$rast_list_act, lab = lab)
    } else if (view == "view_comp" && isTRUE(session_state$comp_maps_rendered)) {
      targets$comp_map_left  <- list(r = rv$rast_list_act, lab = "Actual")
      targets$comp_map_right <- list(r = rv$rast_list_pre, lab = "Predicted")
    } else if (view == "view_resid" && isTRUE(session_state$comp_maps_rendered)) {
      # comp_map_right (point residuals) has no surface styling to swap
      targets$comp_map_left <- list(r = rv$rast_list_res, lab = "resid_raster")
    }

    for (map_id in names(targets)) {
      tgt <- targets[[map_id]]
      r_list <- as_raster_list(tgt$r)
      if (length(r_list) == 0) next
      sig <- current_style_sig(tgt$lab, cp)
      if (identical(get0(map_id, envir = map_style_sig), sig)) next
      style_map_rasters(leafletProxy(map_id), r_list, tgt$lab)
      assign(map_id, sig, envir = map_style_sig)
    }
  }, priority = -10)

  # --- proxy-managed overlays (no raster re-encode on toggle) ---

  # Run points reprojected for Leaflet, cached on rv$sf only: styling ticks
  # (marker size slider, label toggles, palette) reuse the projected object
  # instead of re-running st_transform on every invalidation.
  pts_view_4326 <- reactive({
    pts <- rv$sf
    if (is.null(pts) || nrow(pts) == 0) return(NULL)
    tryCatch(st_transform(pts, 4326), error = function(e) NULL)
  })

  # Styled sampling points + labels + their legend
  observe({
    map_overlay_rev()
    show <- isTRUE(input$show_points_viewer)
    is_resid <- identical(input$map_view, "view_resid")

    pts_view <- NULL
    popup_fn <- NULL
    if (show) {
      pts_view <- pts_view_4326()
      if (!is.null(pts_view)) popup_fn <- make_popup_fn(colnames(st_drop_geometry(pts_view)))
    }

    for (map_id in overlay_map_ids) {
      proxy <- leafletProxy(map_id) %>%
        clearGroup("styled_points") %>%
        clearGroup("styled_labels") %>%
        removeControl("styled_points_legend")
      # Same rule as the old draw_map: no styled points on the residual
      # comparison maps (resid_raster / resid_points views)
      eligible <- map_id == "main_map" || !is_resid
      if (eligible && !is.null(pts_view)) {
        add_styled_points(proxy, pts_view,
          color_by = input$pt_color_by %||% "none",
          custom_colors = rv$pt_style_colors,
          show_labels = isTRUE(input$pt_show_labels),
          label_field = input$pt_label_field %||% "none",
          label_size = input$pt_label_size %||% 11,
          marker_size = input$pt_marker_size %||% 3,
          popup_fn = popup_fn,
          legend_layer_id = "styled_points_legend"
        )
      }
    }
  })

  # Boundary outlines
  observe({
    map_overlay_rev()
    bound_4326 <- if (isTRUE(input$show_borders) && !is.null(rv$bound)) {
      tryCatch(st_transform(st_as_sf(rv$bound), 4326), error = function(e) NULL)
    } else NULL
    for (map_id in overlay_map_ids) {
      proxy <- leafletProxy(map_id) %>% clearGroup("bound_borders")
      if (!is.null(bound_4326)) {
        proxy %>% addPolygons(data = bound_4326, fill = FALSE, color = "white", weight = 2, group = "bound_borders")
      }
    }
  })

  # North arrow (markup shared with the Data Setup mini-map: ui_components.R)
  north_arrow_html <- map_north_arrow_html()
  observe({
    map_overlay_rev()
    show <- isTRUE(input$show_north)
    for (map_id in overlay_map_ids) {
      proxy <- leafletProxy(map_id) %>% removeControl("north_ctrl")
      if (show) proxy %>% addControl(html = north_arrow_html, position = "topleft", layerId = "north_ctrl")
    }
  })

  # Per-locality resolution box
  observe({
    map_overlay_rev()
    show <- isTRUE(input$show_res_overlay) && length(rv$loc_resolutions) > 0
    res_html <- if (show) {
      paste0("<div style='background:white; padding:5px; border-radius:4px; border: 1px solid #ccc; font-size:12px; font-family:sans-serif;'><b>Resolutions:</b><br>", paste(names(rv$loc_resolutions), sapply(rv$loc_resolutions, function(x) round(x,2)), sep=": ", collapse="<br>"), "</div>")
    } else NULL
    for (map_id in overlay_map_ids) {
      proxy <- leafletProxy(map_id) %>% removeControl("res_overlay_ctrl")
      if (!is.null(res_html)) proxy %>% addControl(html = res_html, position = "bottomright", layerId = "res_overlay_ctrl")
    }
  })

  # Distance scale. The control lives in the external #distance_scale_container:
  # it is moved (and styled) there right after creation, so no DOM polling is
  # needed, and it is toggled with plain JS on the live map instances instead
  # of a re-render.
  observe({
    map_overlay_rev()
    show <- isTRUE(input$show_scale)
    shinyjs::runjs(sprintf("
      setTimeout(function() {
        var show = %s;
        var c = document.getElementById('distance_scale_container');
        if (c) c.innerHTML = '';
        ['main_map','comp_map_left','comp_map_right'].forEach(function(id) {
          var el = document.getElementById(id);
          if (!el || el.offsetParent === null) return;
          var w = HTMLWidgets.find('#' + id);
          if (!w || !w.getMap) return;
          var map = w.getMap();
          if (map._monolithScale) { try { map.removeControl(map._monolithScale); } catch(e) {} map._monolithScale = null; }
          if (show) {
            map._monolithScale = L.control.scale({position: 'bottomleft', metric: true, imperial: false}).addTo(map);
            var sc = map._monolithScale.getContainer();
            if (c && sc) {
              c.appendChild(sc);
              sc.style.background = 'white';
              sc.style.padding = '5px';
              sc.style.border = '1px solid #ccc';
              sc.style.borderRadius = '4px';
              sc.style.margin = '0 auto';
            }
          }
        });
      }, 400);
    ", if (show) "true" else "false"))
  })

  # View switcher for the Map Viewer: offers only the surfaces the committed
  # run actually computed, so switching views is instant (no recompute) and a
  # context change in the sidebar can never blank the displayed map.
  # Re-renders only when a run is dispatched/completed, defaulting to the view
  # implied by the committed run configuration.
  output$map_view_ui <- renderUI({
    req(rv$disp)
    choices <- c("View: Actual" = "view_act")
    if (length(rv$rast_list_pre) > 0) {
      choices <- c(choices,
                   "View: ML Predicted" = "view_pred",
                   "View: Actual vs Predicted" = "view_comp")
    }
    if (length(rv$rast_list_res) > 0) choices <- c(choices, "View: ML Residuals" = "view_resid")

    d <- isolate(rv$disp)
    default_view <- if (identical(d$value_type, "resid")) "view_resid"
      else if (isTRUE(d$comp_mode) && d$value_type %in% c("pred", "pred_ss")) "view_comp"
      else if (d$value_type %in% c("pred", "pred_ss")) "view_pred"
      else "view_act"
    if (!default_view %in% choices) default_view <- "view_act"

    selectInput("map_view", NULL, choices = choices, selected = default_view, width = "210px", selectize = FALSE)
  })
  # keep the view choices in sync even while the Map Viewer tab is hidden -
  # the layout conditionalPanels depend on input$map_view being current
  outputOptions(output, "map_view_ui", suspendWhenHidden = FALSE)

  disp_method_label <- function(d) {
    if (is.null(d$method)) "" else paste0(" (", get_method_label(d$method), ")")
  }
  disp_pred_label <- function(d, long = FALSE) {
    if (identical(d$value_type, "pred_ss")) {
      if (long) "Single Split ML Predictions View (_ss)" else "Single Split ML Predictions (_ss)"
    } else {
      if (long) "Best ML Predictions View (_cve)" else "Best ML Predictions (_cve)"
    }
  }

  output$main_map_title <- renderText({
    d <- rv$disp; req(d)
    type_lab <- if (identical(input$map_view, "view_pred")) disp_pred_label(d, long = TRUE) else "Actual Data View"
    paste0(d$label, " - ", type_lab, disp_method_label(d))
  })

  output$comp_left_title <- renderText({
    d <- rv$disp; req(d)
    if (identical(input$map_view, "view_resid")) return(paste0(d$label, " - Interpolated Residuals", disp_method_label(d)))
    paste0(d$label, " - Actual Data", disp_method_label(d))
  })

  output$comp_right_title <- renderText({
    d <- rv$disp; req(d)
    if (identical(input$map_view, "view_resid")) return(paste0(d$label, " - Point Residuals", disp_method_label(d)))
    paste0(d$label, " - ", disp_pred_label(d), disp_method_label(d))
  })

  observeEvent(input$base_map_layer, {
    # draw_map reads the tile choice under isolate(), so this proxy swap is
    # the only path that updates tiles on an already-rendered map
    for (map_id in overlay_map_ids) {
      leafletProxy(map_id) %>%
        clearTiles() %>%
        addProviderTiles(input$base_map_layer, layerId="base_tiles", options = providerTileOptions(zIndex = -10))
    }
  })

  # WGS84 bbox of the current run area: committed boundary when one exists,
  # else the uploaded sample points. NULL before any data is loaded.
  run_area_bbox <- function() {
    if (!is.null(rv$bound)) {
      bb <- tryCatch(sf::st_bbox(sf::st_transform(sf::st_as_sf(rv$bound), 4326)), error = function(e) NULL)
      if (!is.null(bb)) return(bb)
    }
    if (!is.null(rv$user_data) && !is.null(rv$mapping$x) && !is.null(rv$mapping$y)) {
      return(tryCatch({
        df_map <- rv$user_data %>%
          dplyr::select(x = !!sym(rv$mapping$x), y = !!sym(rv$mapping$y)) %>%
          na.omit()
        pts <- st_as_sf(df_map, coords = c("x", "y"), crs = rv$mapping$crs) %>% st_transform(4326)
        st_bbox(pts)
      }, error = function(e) NULL))
    }
    NULL
  }

  # Leaflet refuses to draw into a zero-size container: it stashes the layers in
  # `pendingRenderData` and only flushes them once the widget is resized. A map
  # whose FIRST render happens while its tab (or its view conditionalPanel) is
  # hidden therefore stays a blank gray box until a resize reaches it - which is
  # exactly what happens when a run completes while the user is on another tab.
  # A single resize event races the Bootstrap tab fade and the overlay
  # transition, so pump resizes until every VISIBLE map reports it has actually
  # rendered (capped at ~3 s so a genuinely hidden map costs nothing).
  pump_map_resize <- function() {
    shinyjs::runjs("
      (function() {
        var ids = ['main_map', 'comp_map_left', 'comp_map_right'];
        var tries = 0;
        function pump() {
          tries++;
          var pending = false, visible = false;
          ids.forEach(function(id) {
            var el = document.getElementById(id);
            if (!el || el.offsetWidth === 0 || el.offsetHeight === 0) return;
            visible = true;
            var m = $(el).data('leaflet-map');
            if (!m || !m.leafletr || !m.leafletr.hasRendered) pending = true;
          });
          // nothing visible yet: the tab or view panel may still be fading in
          if (!visible && tries < 8) pending = true;
          if (pending || tries <= 2) window.dispatchEvent(new Event('resize'));
          if (pending && tries < 20) setTimeout(pump, 150);
        }
        setTimeout(pump, 50);
      })();
    ")
  }

  # Fit the visible map canvases to the run area and nudge Leaflet to
  # re-measure its container. Shared by the Refresh Map Area button and the
  # reveal handler: a widget that initialized without a valid view (the
  # placeholder rendered mid-run) never loads tiles, so revealing the maps
  # must always end with an explicit fit.
  fit_maps_to_data <- function() {
    bbox <- run_area_bbox()
    if (!is.null(bbox)) {
      leafletProxy("main_map") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))
      if (isTRUE(input$map_view %in% c("view_comp", "view_resid"))) {
        leafletProxy("comp_map_left") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))
        leafletProxy("comp_map_right") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))
      }
    }
    pump_map_resize()
  }

  # Companion to pump_map_resize() for every OTHER htmlwidget, DT above all.
  # DT stashes a payload delivered to a zero-size element and flushes it only
  # from its own resize handler; the Scientific Analysis tables are pushed
  # eagerly (suspendWhenHidden = FALSE) and the run observer switches to the Map
  # Viewer at dispatch, so those tables are one run behind until something
  # resizes them.
  #
  # Dispatching a window resize event is NOT enough: Shiny routes each output
  # binding's resize through makeResizeFilter(), which drops the call when the
  # element's box is unchanged since its last non-zero measurement - which is
  # exactly the case when returning to a tab that was already visited once.
  # (Shiny itself already calls the filtered onResize on shown.bs.tab, so a
  # window event adds nothing.) Call the bindings directly instead: same work,
  # without the filter. Each widget then decides what it owes - DT re-renders
  # its stash, leaflet invalidates its size and flushes pendingRenderData - and
  # anything unbound or still hidden is skipped. Fails closed: if Shiny's
  # internals ever move, the guards make this a no-op rather than an error.
  pump_widget_resize <- function() {
    shinyjs::runjs("
      (function() {
        function flush() {
          $('.html-widget-output').each(function() {
            var el = this;
            var r = el.getBoundingClientRect();
            if (r.width === 0 || r.height === 0) return;
            var adapter = $(el).data('shiny-output-binding');
            if (!adapter || !adapter.binding ||
                typeof adapter.binding.resize !== 'function') return;
            try { adapter.binding.resize(el, r.width, r.height); } catch (e) {}
          });
        }
        [0, 150, 400].forEach(function(d) { setTimeout(flush, d); });
      })();
    ")
  }

  # The Data Setup mini-map needs more than a flush. When it was rendered at a
  # different container size (it re-renders on locality assignment and point
  # restyling, both driven from the Map Viewer), invalidateSize repaints the
  # full container but KEEPS the stale centre and zoom, leaving the samples
  # outside the frame. So re-fit it to the stored sample bounds on reveal,
  # after the size has been invalidated - repeated across the Bootstrap tab
  # fade because a fit computed for a zero-size container is worthless.
  refit_setup_minimap <- function() {
    bb <- session_state$minimap_bbox
    if (is.null(bb) || !all(is.finite(bb)) ||
        !isTRUE(session_state$minimap_rendered)) return(invisible(NULL))
    shinyjs::runjs(sprintf("
      (function() {
        function fit() {
          var el = document.getElementById('setup_minimap');
          if (!el || el.offsetWidth === 0 || el.offsetHeight === 0) return;
          var w = HTMLWidgets.find('#setup_minimap');
          var map = (w && w.getMap) ? w.getMap() : null;
          if (!map) return;
          map.invalidateSize();
          map.fitBounds([[%s, %s], [%s, %s]], {animate: false});
        }
        [200, 500, 900].forEach(function(d) { setTimeout(fit, d); });
      })();
    ", format(bb[["ymin"]], digits = 15), format(bb[["xmin"]], digits = 15),
       format(bb[["ymax"]], digits = 15), format(bb[["xmax"]], digits = 15)))
  }

  # A widget that initialized (or re-rendered) hidden must repair itself as soon
  # as it becomes visible, even when the user never clicks Reveal Maps:
  # returning to a tab, or switching view (which swaps which conditionalPanel is
  # shown), are the moments a stashed render can finally be flushed.
  observeEvent(input$main_tabs, {
    if (identical(input$main_tabs, "tab_map")) pump_map_resize()
    if (identical(input$main_tabs, "tab_data")) refit_setup_minimap()
    # Every tab: the stale-widget failure mode is not specific to one of them.
    pump_widget_resize()
  }, ignoreInit = TRUE)

  observeEvent(input$map_view, {
    pump_map_resize()
  }, ignoreInit = TRUE)

  observeEvent(input$refresh_map_area, {
    req(input$base_map_layer)
    leafletProxy("main_map") %>%
      clearTiles() %>%
      addProviderTiles(input$base_map_layer, layerId="base_tiles", options = providerTileOptions(zIndex = -10))

    leafletProxy("comp_map_left") %>%
      clearTiles() %>%
      addProviderTiles(input$base_map_layer, layerId="base_tiles", options = providerTileOptions(zIndex = -10))

    leafletProxy("comp_map_right") %>%
      clearTiles() %>%
      addProviderTiles(input$base_map_layer, layerId="base_tiles", options = providerTileOptions(zIndex = -10))

    fit_maps_to_data()
  })

  output$main_map <- renderLeaflet({
    d <- rv$disp; req(d)
    view <- input$map_view %||% "view_act"
    req(view %in% c("view_act", "view_pred"))
    target <- if (view == "view_pred") rv$rast_list_pre else rv$rast_list_act
    view_lab <- if (view == "view_pred") {
      if (identical(d$value_type, "pred_ss")) "pred_ss" else "pred"
    } else "actual"
    m <- draw_map(target, view_lab, map_id = "main_map")
    session_state$main_map_rendered <- TRUE
    map_overlay_rev(isolate(map_overlay_rev()) + 1L)
    m
  })

  output$comp_map_left <- renderLeaflet({
    req(rv$disp, input$map_view %in% c("view_comp", "view_resid"))
    m <- if(input$map_view == "view_resid") {
      draw_map(rv$rast_list_res, "resid_raster", map_id = "comp_map_left")
    } else {
      draw_map(rv$rast_list_act, "Actual", map_id = "comp_map_left")
    }
    session_state$comp_maps_rendered <- TRUE
    map_overlay_rev(isolate(map_overlay_rev()) + 1L)
    m
  })

  output$comp_map_right <- renderLeaflet({
    req(rv$disp, input$map_view %in% c("view_comp", "view_resid"))
    m <- if(input$map_view == "view_resid") {
      draw_map(NULL, "resid_points", map_id = "comp_map_right")
    } else {
      draw_map(rv$rast_list_pre, "Predicted", map_id = "comp_map_right")
    }
    session_state$comp_maps_rendered <- TRUE
    map_overlay_rev(isolate(map_overlay_rev()) + 1L)
    m
  })
      output$locality_pan_ui <- renderUI({
      req(rv$loc_names)
      render_locality_pan_input(rv$loc_names)
      })

      observeEvent(input$locality_pan, {
      req(input$locality_pan, rv$user_data, rv$mapping$x, rv$mapping$y, rv$mapping$crs)

      bbox <- if (input$locality_pan == "global") {
        df_map <- rv$user_data %>% 
          dplyr::filter(!!sym(rv$mapping$loc) %in% rv$loc_names) %>%
          dplyr::select(x = !!sym(rv$mapping$x), y = !!sym(rv$mapping$y)) %>% 
          na.omit()
        pts <- st_as_sf(df_map, coords = c("x", "y"), crs = rv$mapping$crs) %>% st_transform(4326)
        st_bbox(pts)
      } else {        # Filter by selected locality
        df_map <- rv$user_data %>% 
          dplyr::filter(!!sym(rv$mapping$loc) == input$locality_pan) %>%
          dplyr::select(x = !!sym(rv$mapping$x), y = !!sym(rv$mapping$y)) %>% 
          na.omit()
        pts <- st_as_sf(df_map, coords = c("x", "y"), crs = rv$mapping$crs) %>% st_transform(4326)
        st_bbox(pts)
      }

      leafletProxy("main_map") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))

      if (isTRUE(input$map_view %in% c("view_comp", "view_resid"))) {
        leafletProxy("comp_map_left") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))
        leafletProxy("comp_map_right") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))
      }
   })

   # Manual-mode slider values feed a live overlay line on the fitted
   # variogram plots; they belong in the cache key only while they actually
   # affect the plot (the short-circuit reads mirror the plot's own logic).
   vgm_manual_overlay_key <- function(target) {
     if (identical(input$vgm_mode, "manual") && identical(input$sel_loc_stats, input$m_loc)) {
       applies <- if (target == "act") {
         is.null(input$m_target) || input$m_target == "act"
       } else {
         identical(input$m_target, "pre")
       }
       if (applies) return(list(input$m_psill, input$k_mod, input$m_range, input$m_nugget))
     }
     NULL
   }

   # ── SA diagnostics build closures ────────────────────────────────────────
   # Every Scientific Analysis plot card shares ONE ggplot builder between its
   # in-page cached render, the expand modal (static + interactive plotly) and
   # the 300-dpi PNG download. Variogram panels are ggplot rebuilds of the
   # former lattice plots (same empirical values and fitted lines via
   # variogramLine; presentation only).
   sci_placeholder <- function(msg, size = 5) {
     ggplot() + annotate("text", x = 4, y = 4, label = msg, size = size, color = "grey40") + theme_void()
   }

   register_sci_plot <- function(id, title, build_fn) {
     register_expanded_modal(input, output, session,
       btn_id = paste0(id, "_expand"), mode_id = paste0(id, "_mode"),
       ui_id = paste0(id, "_modal_ui"), plot_static_id = paste0(id, "_modal_static"),
       plot_plotly_id = paste0(id, "_modal_plotly"), title_text = title,
       build_fn = build_fn)
     output[[paste0(id, "_dl")]] <- downloadHandler(
       filename = function() paste0("monolith_", id, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
       content = function(file) {
         p <- tryCatch(build_fn(), error = function(e) NULL)
         if (!inherits(p, "ggplot")) {
           showNotification("This plot is not available to download yet.", type = "warning")
           req(FALSE)
         }
         # Screen builders use theme_minimal(base_size = 12), which is far too
         # small on a 9 x 7 in canvas at 300 dpi; rescale the text to the
         # export-registry convention (~16 pt body, bold 19 pt title).
         p <- p + ggplot2::theme(
           text = ggplot2::element_text(size = 16),
           plot.title = ggplot2::element_text(size = 19, face = "bold"),
           plot.subtitle = ggplot2::element_text(size = 14),
           axis.title = ggplot2::element_text(size = 16),
           axis.text = ggplot2::element_text(size = 14),
           legend.title = ggplot2::element_text(size = 14),
           legend.text = ggplot2::element_text(size = 13),
           strip.text = ggplot2::element_text(size = 13)
         )
         suppressWarnings(ggsave(file, plot = p, width = 9, height = 7, dpi = 300, bg = "white"))
       }
     )
   }

   build_vgm_structure_plot <- function(target) {
     loc <- input$sel_loc_stats
     # Displayed run's context when one exists; live sidebar selection as the
     # pre-run fallback so "OPTIMIZE ALL VARIOGRAMS" -> Manual tuning shows
     # the fitted curves BEFORE the first interpolation (the advertised
     # workflow), instead of a blank panel.
     meta <- get_display_meta()
     if (is.null(meta)) meta <- get_current_meta()
     req(loc, meta)
     tgt_label <- if (target == "act") "Actual" else "Predicted"
     if (loc == "Total (Combined)") {
       if (target == "act") {
         pts_sf <- if(!is.null(rv$sf)) {
           rv$sf
         } else {
           req(rv$user_data, rv$mapping$x, rv$mapping$y, rv$mapping$crs)
           act_col <- meta$actual
           req(act_col %in% colnames(rv$user_data))
           df_clean <- rv$user_data %>%
             dplyr::select(x = !!sym(rv$mapping$x), y = !!sym(rv$mapping$y), v = !!sym(act_col)) %>%
             na.omit()
           req(nrow(df_clean) >= 3)
           validate_and_project_sf(sf::st_as_sf(df_clean, coords = c("x", "y"), crs = rv$mapping$crs))
         }
         req(pts_sf)
         return(build_variogram_ggplot(gstat::variogram(v ~ 1, pts_sf),
                                       title = paste("Global Variogram (Actual):", sci_disp_label(meta))))
       }
       pred_col <- if(identical(meta$value_type, "pred_ss")) meta$pred_ss else meta$pred
       pts_sf <- if(!is.null(rv$sf) && "pv" %in% colnames(rv$sf)) {
         rv$sf
       } else if(!is.null(pred_col) && pred_col %in% colnames(rv$user_data)) {
         req(rv$user_data, rv$mapping$x, rv$mapping$y, rv$mapping$crs)
         df_clean <- rv$user_data %>%
           dplyr::select(x = !!sym(rv$mapping$x), y = !!sym(rv$mapping$y), pv = !!sym(pred_col)) %>%
           na.omit()
         if(nrow(df_clean) < 3) NULL else {
           validate_and_project_sf(sf::st_as_sf(df_clean, coords = c("x", "y"), crs = rv$mapping$crs))
         }
       } else {
         NULL
       }
       if (is.null(pts_sf) || !("pv" %in% colnames(pts_sf))) {
         return(sci_placeholder("Predicted data structure is not available.\nPlease run spatial interpolation first."))
       }
       return(build_variogram_ggplot(gstat::variogram(pv ~ 1, pts_sf %>% filter(!is.na(pv))),
                                     title = paste("Global Variogram (Predicted):", sci_disp_label(meta))))
     }

     v_emp <- rv$v_emp_list[[paste0(loc, "_", target)]]
     if (is.null(v_emp)) {
       return(sci_placeholder(paste0("No fitted variogram for this locality yet (", tgt_label, ").\nPress OPTIMIZE ALL VARIOGRAMS in the sidebar (Fitting Mode: Auto-Fit)\nor run an interpolation first.")))
     }
     v_fit <- rv$v_fit_list[[paste0(loc, "_", target)]]
     manual_model <- NULL; sub <- NULL
     manual_applies <- input$vgm_mode == "manual" && loc == input$m_loc &&
       (if (target == "act") is.null(input$m_target) || input$m_target == "act"
        else !is.null(input$m_target) && input$m_target == "pre")
     if (isTRUE(manual_applies)) {
       manual_model <- vgm(psill = input$m_psill, model = input$k_mod, range = input$m_range, nugget = input$m_nugget)
       v_line_at_emp <- variogramLine(manual_model, dist_vector = v_emp$dist)
       sub <- paste("Manual model (red dashed) - SSE:", round(sum((v_emp$gamma - v_line_at_emp$gamma)^2), 4))
     }
     build_variogram_ggplot(v_emp, v_fit,
                            title = paste0("Fitted (", tgt_label, "): ", loc),
                            subtitle = sub, manual_model = manual_model)
   }

   output$vgm_plot_main <- renderCachedPlot({
     p <- build_vgm_structure_plot("act"); req(p); p
   }, cacheKeyExpr = {
     loc <- input$sel_loc_stats
     # input$var_id covers the pre-run state (no committed rv$disp yet):
     # switching the sidebar variable must invalidate the pre-run panels
     list("vgm_main", loc, rv$results_rev, rv$disp$actual %||% input$var_id, input$sci_name_mode,
          rv$v_emp_list[[paste0(loc, "_act")]], rv$v_fit_list[[paste0(loc, "_act")]],
          vgm_manual_overlay_key("act"))
   }, cache = "session")
   output$vgm_plot_pred <- renderCachedPlot({
     p <- build_vgm_structure_plot("pre"); req(p); p
   }, cacheKeyExpr = {
     loc <- input$sel_loc_stats
     list("vgm_pred", loc, rv$results_rev, rv$disp$actual %||% input$var_id, rv$disp$value_type %||% input$value_type, input$sci_name_mode,
          rv$v_emp_list[[paste0(loc, "_pre")]], rv$v_fit_list[[paste0(loc, "_pre")]],
          vgm_manual_overlay_key("pre"))
   }, cache = "session")

