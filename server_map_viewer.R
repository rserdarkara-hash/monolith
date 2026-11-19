# server_map_viewer.R (chunk H, sourced with local = TRUE inside server):
# draw_map, proxy-managed overlays (keyed on map_overlay_rev), view switcher
# and the main/comparison renderLeaflet blocks.

  # Stale-results banner: the maps are a snapshot of the last run, so a later
  # Input Data CRS or X/Y change moves nothing here until the next run. Warn,
  # never clear (the archive and the registry hold these results).
  output$map_crs_stale_note <- renderUI({
    d <- rv$disp
    req(d)
    fmt <- function(x) if (isTruthy(x)) as.character(x) else "not set"
    diffs <- list(
      if (!identical(d$map_crs, rv$mapping$crs))
        sprintf("Input Data CRS %s -> %s", fmt(d$map_crs), fmt(rv$mapping$crs)),
      if (!identical(d$map_x, rv$mapping$x))
        sprintf("X column %s -> %s", fmt(d$map_x), fmt(rv$mapping$x)),
      if (!identical(d$map_y, rv$mapping$y))
        sprintf("Y column %s -> %s", fmt(d$map_y), fmt(rv$mapping$y))
    )
    diffs <- unlist(Filter(Negate(is.null), diffs))
    if (!length(diffs)) return(NULL)
    div(style = "margin-bottom: 10px; padding: 10px 14px; border-radius: 5px; border: 1px solid var(--mn-line); border-left: 2px solid var(--mn-warn); background-color: var(--mn-surface-2); color: var(--mn-text-2);",
        tags$b("These maps are out of date. "),
        "They were computed under ", tags$b(fmt(d$map_crs)),
        ", and the Data Setup tab has changed since: ",
        tags$b(paste(diffs, collapse = "; ")),
        ". Nothing on this tab moves until you press ", tags$b("Run Interpolation"),
        " again. The existing results are kept for the archive and the Export Registry.")
  })

  output$loc_res_table <- renderTable({
    req(rv$loc_buffer_res)
    res_list <- rv$loc_buffer_res
    if(length(res_list) == 0) return(NULL)
    
    show_buffer <- input$boundary_type %in% c("wrapped", "strict")
    res_mode_val <- input$res_mode %||% "local"
    manual_res_val <- input$grid_res %||% 50
    
    # In the Auto modes the cell size is set during the run; the resolution
    # overlay lists the sizes a run used.
    df <- data.frame(
      Locality = names(res_list),
      Resolution = if (res_mode_val == "fixed") paste0(round(manual_res_val, 1), " m")
                   else if (res_mode_val == "global") "Shared, set at run"
                   else "Own, set at run"
    )
    
    if (show_buffer) {
      buff_mode_val <- input$buff_mode %||% "dynamic"
      method_val <- input$method %||% "OK"
      fixed_dist <- input$buff_dist %||% 250
      
      # The run's own rule (dynamic_buffer_dist) on the run's own basis: the
      # slider in Fixed mode, else the locality's rv$loc_buffer_res.
      df$`Buffer (m)` <- vapply(res_list, function(x) {
        if (!(buff_mode_val == "dynamic" && input$boundary_type == "wrapped")) {
          return(paste0(fixed_dist, " m"))
        }
        base_res <- if (res_mode_val == "fixed") manual_res_val else x
        if (!isTRUE(is.finite(base_res))) return("-")
        paste0(round(dynamic_buffer_dist(method_val, base_res), 1), " m")
      }, character(1))
    }
    df
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")

  # Advisory for a Point buffer narrower than half the cell diagonal
  # (strict_buffer_gap()), for Fixed resolution only: in the Auto modes the run
  # derives the cell size and warns itself.
  output$strict_buffer_note <- renderUI({
    if (!identical(input$boundary_type, "strict")) return(NULL)
    if (!identical(input$res_mode %||% "local", "fixed")) return(NULL)
    msg <- strict_buffer_message(input$buff_dist %||% 250, input$grid_res %||% 50,
                                 res_floor = 1)
    if (is.null(msg)) return(NULL)
    p(style = paste("font-size: 0.78em; margin-top: 8px; border-left: 2px solid var(--mn-warn);",
                    "padding-left: 8px; color: var(--mn-text-2); line-height: 1.35;"),
      msg,
      # An uploaded boundary replaces the point buffers only for the localities
      # it matches, so qualify rather than hide the advisory.
      if (!is.null(rv$shp_bound)) {
        span(" This applies to localities your uploaded boundary shapefile does not cover, which fall back to the point buffers.")
      })
  })

  # Normalise a raster / packed raster / list-of-rasters argument into a
  # plain list with NULL entries dropped (shared by draw_map and the proxy
  # restyler).
  as_raster_list <- function(r_obj) {
    if (is.null(r_obj)) return(list())
    r_list <- if (inherits(r_obj, "SpatRaster") || inherits(r_obj, "PackedSpatRaster")) list(r_obj) else r_obj
    Filter(Negate(is.null), r_list)
  }

  # The map widgets the current view displays, once rendered: the main map in
  # Actual and Predicted, both panels in Comparison and Residuals.
  shown_map_ids <- function() {
    if (map_view_base() %in% c("view_act", "view_pred")) {
      if (isTRUE(session_state$main_map_rendered)) "main_map" else character(0)
    } else if (isTRUE(session_state$comp_maps_rendered)) {
      c("comp_map_left", "comp_map_right")
    } else character(0)
  }

  # What a map widget draws in the current view: its rasters and its draw_map()
  # label. The Residuals view's right panel draws the point residuals.
  map_surface <- function(map_id, view = map_view_base()) {
    switch(map_id,
      main_map = if (view == "view_pred") {
        list(r = rv$rast_list_pre, lab = if (identical(rv$disp$value_type, "pred_ss")) "pred_ss" else "pred")
      } else list(r = rv$rast_list_act, lab = "actual"),
      comp_map_left = if (view == "view_resid") list(r = rv$rast_list_res, lab = "resid_raster")
                      else list(r = rv$rast_list_act, lab = "Actual"),
      comp_map_right = if (view == "view_resid") list(r = NULL, lab = "resid_points")
                       else list(r = rv$rast_list_pre, lab = "Predicted"))
  }

  # Styling signature per map widget, stamped by draw_map and checked by the
  # proxy restyler, so a tick matching what a map already shows costs nothing.
  map_style_sig <- new.env(parent = emptyenv())

  # Raster image layers each widget carries, so a restyle with fewer images
  # retires the surplus (remove_surplus_raster_images()).
  map_raster_count <- new.env(parent = emptyenv())

  # The sampling-point layer each map widget shows, as last sent: point set,
  # style, legend, labels, label size. A pass sends only the parts that differ;
  # draw_map() clears a map's entry with its widget.
  pt_state <- new.env(parent = emptyenv())

  # The surface a map widget's label shows, whose own class breaks and variance
  # band style it; NULL for the residual maps, which have neither.
  class_surface_of <- function(lab) {
    if (lab %in% c("actual", "Actual")) "act"
    else if (lab %in% c("pred", "pred_ss", "Predicted")) "pre"
    else NULL
  }
  # Classification of the map with this label, or NULL (continuous styling).
  class_params_for <- function(lab) {
    surface <- class_surface_of(lab)
    if (is.null(surface) || !isTRUE(input$color_style %in% c("agro", "bin"))) return(NULL)
    tryCatch(classification_params(surface), error = function(e) NULL)
  }

  current_style_sig <- function(lab, class_params) {
    # Agro/bin without computable class params renders continuous, so it
    # shares the continuous signature.
    style_eff <- input$color_style %||% "cont"
    if (style_eff %in% c("agro", "bin") && is.null(class_params)) style_eff <- "cont"
    paste(
      rv$run_counter, lab,
      style_eff,
      get_display_meta()$palette %||% "",
      map_view_layer(),
      match_scales_on(),
      if (!is.null(class_params)) paste(signif(class_params$brks, 10), collapse = ",") else "none",
      sep = "|"
    )
  }

  # Adds/replaces the raster image layers and legend on a fresh widget (inside
  # draw_map) or a leafletProxy (restyle). Stable layerIds make re-adds
  # REPLACE, so a styling change costs one PNG encode per layer, no rebuild.
  style_map_rasters <- function(m, r_list, lab, map_id = NULL) {
    meta <- get_display_meta()
    if (is.null(meta) || length(r_list) == 0) return(m)

    r_names <- names(r_list)
    layer_key <- function(i) {
      r_name <- if (!is.null(r_names) && length(r_names) >= i && !is.na(r_names[i]) && r_names[i] != "") r_names[i] else as.character(i)
      paste0(rv$run_counter, "_", lab, "_", r_name)
    }
    # Positional image ids are handed out as images are added, so a skipped
    # locality leaves no hole in the sequence.
    n_img <- 0L
    add_img <- function(m, r, pal) {
      n_img <<- n_img + 1L
      m %>% addRasterImage(r, colors = pal, opacity = 0.8, project = FALSE,
                           layerId = raster_img_layer_id(n_img))
    }
    legend_id <- "rast_legend"
    # Every legend is drawn opaque (leaflet's default is half-transparent), so
    # its swatches are the palette's own colours on a light and a dark surface.
    # The surface's own variance band decides, so a stale view-menu value (the
    # previous run's) cannot style a surface that has none.
    uncert_layer <- map_view_layer()
    is_uncert_view <- uncert_layer %in% c("se", "var") &&
      isTRUE(class_surface_of(lab) %in% meta$variance_surfaces)
    # NULL for an uncertainty view on a raster without a variance band: that
    # locality is left undrawn.
    select_active_layer <- function(r_w) {
      if (is_uncert_view) {
        if (!"var1.var" %in% names(r_w)) return(NULL)
        al <- r_w[["var1.var"]]
        if (uncert_layer == "se") sqrt(al) else al
      } else {
        if("var1.pred" %in% names(r_w)) r_w[["var1.pred"]] else r_w[[1]]
      }
    }
    # Palette domain read lazily: only the continuous branch needs cell values.
    get_vv_scale <- function() {
      jv <- if (is_uncert_view) joint_uncert_vv() else joint_vv()$v
      if (!is.null(jv)) return(jv)
      unlist(lapply(seq_along(r_list), function(i) {
        r_proj <- get_projected_raster(r_list[[i]], layer_key(i))
        if (is.null(r_proj)) return(NULL)
        al <- select_active_layer(r_proj)
        if (is.null(al)) return(NULL)
        as.vector(values(al, na.rm=TRUE))
      }))
    }

    # Uncertainty is a non-negative magnitude with no midpoint, so a diverging
    # palette choice is swapped for a sequential one on those layers only.
    pal_name <- if (is_uncert_view) uncertainty_palette(meta$palette) else meta$palette
    is_viridis <- pal_name == "viridis"
    # Same composition as the exported figure's legend (map_legend_title).
    legend_title <- map_legend_title(meta$label, meta$unit, if (is_uncert_view) uncert_layer else "value")

    if(lab == "resid_raster") {
      # The residual view shows the var1.pred difference (no SE/variance form).
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
        m <- add_img(m, resid_layers[[i]], pal)
      }
      m <- m %>% leaflet::addLegend(pal = pal, values = c(-abs_max, abs_max), opacity = 1, title = legend_var_title(map_legend_title(meta$label, layer = "resid")), layerId = legend_id)
    } else {
      # Classified styling with this map's surface classes when computable;
      # otherwise continuous, so a completed run never shows an empty map.
      class_params <- if (!is_uncert_view) class_params_for(lab) else NULL

      if (!is.null(class_params)) {
        pal <- colorBin(class_params$colors, bins = class_params$brks, na.color = "transparent", right = FALSE)

        for (i in seq_along(r_list)) {
          r_w <- get_projected_raster(r_list[[i]], layer_key(i))
          if (is.null(r_w)) next
          m <- add_img(m, select_active_layer(r_w), pal)
        }
        m <- m %>% leaflet::addLegend(colors = class_params$colors, labels = class_params$leg_labels, opacity = 1, title = legend_var_title(map_legend_title(meta$label, meta$unit)), layerId = legend_id)
      } else {
        vv_scale <- get_vv_scale()
        vv_scale <- vv_scale[is.finite(vv_scale)]
        if (length(vv_scale) > 0) {
          pal <- if(is_viridis) colorNumeric(viridisLite::viridis(256, option = pal_name), vv_scale, na.color = "transparent")
                 else colorNumeric(pal_name, vv_scale, na.color = "transparent")

          for (i in seq_along(r_list)) {
            r_w <- get_projected_raster(r_list[[i]], layer_key(i))
            if (is.null(r_w)) next
            al <- select_active_layer(r_w)
            if (is.null(al)) next
            m <- add_img(m, al, pal)
          }

          lg <- map_legend_scale(vv_scale)
          m <- if (!is.null(lg$constant)) {
            m %>% leaflet::addLegend(colors = pal(mean(vv_scale)), labels = lg$constant, opacity = 1, title = legend_var_title(legend_title), layerId = legend_id)
          } else {
            m %>% leaflet::addLegend(pal = pal, values = vv_scale, opacity = 1, title = legend_var_title(legend_title), labFormat = labelFormat(digits = lg$digits), layerId = legend_id)
          }
        } else {
          m <- m %>% leaflet::removeControl(legend_id)
        }
      }
    }

    if (!is.null(map_id)) {
      m <- remove_surplus_raster_images(m, n_img, get0(map_id, envir = map_raster_count, ifnotfound = 0L))
      assign(map_id, n_img, envir = map_raster_count)
    }
    m
  }

  # draw_map builds the full widget only for STRUCTURAL changes (new run, view
  # switch); styling reads are isolate()d and pure styling ticks go to the
  # proxy restyler. Cheap overlays (points, borders, north arrow, scale,
  # cell-size box, base tiles) are proxy observers keyed on map_overlay_rev.
  draw_map <- function(r_obj, lab, map_id = NULL) {
    # A new widget holds no points: the point overlay redraws them in full.
    if (!is.null(map_id)) pt_state[[map_id]] <- NULL
    current_tiles <- isolate(input$base_map_layer) %||% "Esri.WorldImagery"
    current_key <- isolate(carto_api_key())

    if((is.null(r_obj) || (is.list(r_obj) && length(r_obj) == 0)) && lab != "resid_points") {
      # Placeholder map (no rasters yet): give it a view, or it never loads tiles.
      if (!is.null(map_id)) {
        if (exists(map_id, envir = map_style_sig)) rm(list = map_id, envir = map_style_sig)
        assign(map_id, 0L, envir = map_raster_count)
      }
      m0 <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        add_base_tiles(current_tiles, current_key) %>%
        add_map_ruler()
      bb <- run_area_bbox()
      m0 <- if (!is.null(bb)) {
        m0 %>% fitBounds(as.numeric(bb$xmin), as.numeric(bb$ymin), as.numeric(bb$xmax), as.numeric(bb$ymax))
      } else {
        m0 %>% setView(lng = 0, lat = 0, zoom = 2)
      }
      return(m0)
    }
    
    m <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% add_base_tiles(current_tiles, current_key) %>%
      leaflet.extras::addDrawToolbar(
        targetGroup = "drawn_features",
        polylineOptions = FALSE,
        polygonOptions = drawPolygonOptions(),
        circleOptions = FALSE,
        rectangleOptions = drawRectangleOptions(),
        markerOptions = drawMarkerOptions(),
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
      ) %>%
      add_map_ruler()
    meta <- isolate(get_display_meta())
    req(meta)

    if(!is.null(r_obj) && !(is.list(r_obj) && length(r_obj) == 0)) {
      r_list <- as_raster_list(r_obj)

      if (length(r_list) > 0) {
        # The surface's own fits (NULL for the residual maps, which derive from
        # both). `of`: an RK/RFK locality that fell back to OK holds the
        # measured values' variogram, and its bands are worded for that.
        vgm_warn_html <- build_vgm_warning_html(rv$disp$v_fits, target = class_surface_of(lab),
                                                engine = rv$disp$method, of = disp_vgm_of())
        if (!is.null(vgm_warn_html)) {
          # className matters: the default "info legend" class would clip the
          # banner under the legend's 190px overflow rule.
          m <- m %>% addControl(html = vgm_warn_html, position = "bottomleft",
                                className = "vgm-warn-control leaflet-control")
        }
        # Styling reads are isolated so a styling tick invalidates only the
        # proxy restyler.
        isolate({
          m <- style_map_rasters(m, r_list, lab, map_id)
          if (!is.null(map_id)) {
            assign(map_id, current_style_sig(lab, class_params_for(lab)), envir = map_style_sig)
          }
        })
      }
    }


    # The point residuals; a click opens a point's pop-up by its layer id.
    if(lab == "resid_points") {
       pts_view <- pts_view_4326()
       req(pts_view, "resid" %in% colnames(pts_view))
       abs_max_p <- max(abs(pts_view$resid), na.rm = TRUE)
       if(is.infinite(abs_max_p) || is.na(abs_max_p)) abs_max_p <- 1
       pal_pts <- colorNumeric("RdBu", domain = c(-abs_max_p, abs_max_p), na.color = "black")
       m <- m %>% addCircleMarkers(data = pts_view, radius = 5, color = "black", weight = 1,
                                  fillColor = ~pal_pts(resid), fillOpacity = 0.9,
                                  layerId = paste0(RESID_POINT_ID_PREFIX, seq_len(nrow(pts_view))),
                                  group = "resid_points")
       m <- m %>% leaflet::addLegend(pal = pal_pts, values = c(-abs_max_p, abs_max_p), opacity = 1, title = legend_var_title(map_legend_title(meta$label, layer = "point_resid")))
    }
    
    m
  }

  # ── Proxy restyler: styling ticks swap raster images + legend in place ──
  # Invalidated by pure styling inputs (palette, style, APPLIED class breaks,
  # SE/variance layer, match scales). Structural changes take the full
  # renderLeaflet path; priority = -10 runs this after the outputs, so a
  # freshly rendered widget is never re-encoded.
  observe({
    # Styling dependencies, registered unconditionally so the observer is
    # armed even before the first run.
    style_now <- input$color_style %||% "cont"
    rv$palette_picks; match_scales_on(); map_view_layer()
    # Both surfaces' classes are dependencies: each map is restyled with its
    # own (class_params_for below).
    if (style_now %in% c("agro", "bin")) {
      tryCatch(classification_params("act"), error = function(e) NULL)
      tryCatch(classification_params("pre"), error = function(e) NULL)
    }

    if (is.null(rv$disp)) return(invisible(NULL))
    view <- map_view_base()
    for (map_id in shown_map_ids()) {
      s <- map_surface(map_id, view)
      r_list <- as_raster_list(s$r)        # empty for the point-residual panel
      if (length(r_list) == 0) next
      sig <- current_style_sig(s$lab, class_params_for(s$lab))
      if (identical(get0(map_id, envir = map_style_sig), sig)) next
      style_map_rasters(leafletProxy(map_id), r_list, s$lab, map_id)
      assign(map_id, sig, envir = map_style_sig)
    }
  }, priority = -10)

  # --- proxy-managed overlays (no raster re-encode on toggle) ---

  # Run points reprojected for Leaflet, cached on rv$sf only: the point overlay,
  # the point residuals and the point pop-ups (chunk C) read this one copy.
  pts_view_4326 <- reactive({
    pts <- rv$sf
    if (is.null(pts) || nrow(pts) == 0) return(NULL)
    tryCatch(st_transform(pts, 4326), error = function(e) NULL)
  })

  # Styled sampling points: a new point set (or a rebuilt widget) gets the
  # whole layer; a styling change restyles, relabels or resizes in place. The
  # Residuals view's panels carry no styled points. rv$sf is the DISPLAY set
  # (never NA-target filtered), so points without a measured target ("v") are
  # styled apart even when covariates or predictions exist.
  observe({
    map_overlay_rev()
    pts <- if (isTRUE(input$show_points_viewer)) pts_view_4326()
    targets <- if (identical(map_view_base(), "view_resid")) character(0) else shown_map_ids()
    if (!length(targets)) return(invisible(NULL))
    coords <- if (!is.null(pts)) sf::st_coordinates(pts)
    if (!is.null(pts)) {
      spec <- point_style_spec(pts, input$pt_color_by %||% "none", pt_group_colors(), value_col = "v")
      radius <- input$pt_marker_size %||% 3
      style <- list(spec$fill, spec$stroke, spec$fill_opacity, radius)
      labels <- if (isTRUE(input$pt_show_labels)) point_label_text(pts, input$pt_label_field)
      label_px <- input$pt_label_size %||% 11
    }
    for (map_id in targets) {
      st <- pt_state[[map_id]] %||% list()
      proxy <- leafletProxy(map_id)
      if (!identical(st$coords, coords)) {
        if (!is.null(st$coords)) {
          proxy %>% leaflet::removePopup(POINT_POPUP_ID) %>%
            leaflet::invokeMethod(NULL, "mnPointLabels", POINT_LAYER_ID_PREFIX, NULL) %>%
            clearGroup("styled_points") %>% removeControl("styled_points_legend")
        }
        st <- list(coords = coords, label_px = st$label_px)
        if (!is.null(pts)) {
          add_styled_points(proxy, pts, spec, marker_size = radius, legend_layer_id = "styled_points_legend",
                            layer_ids = paste0(POINT_LAYER_ID_PREFIX, seq_len(nrow(pts))))
          st$style <- style; st$legend <- spec$legend
        }
      }
      if (!is.null(pts)) {
        if (!identical(st$style, style)) {
          proxy %>% leaflet::invokeMethod(NULL, "mnStylePoints", POINT_LAYER_ID_PREFIX, nrow(pts),
                                          spec$fill, spec$stroke, spec$fill_opacity, radius)
          st$style <- style
        }
        if (!identical(st$legend, spec$legend)) {
          proxy %>% removeControl("styled_points_legend") %>%
            add_point_legend(spec$legend, "styled_points_legend")
          st$legend <- spec$legend
        }
        if (!identical(st$labels, labels)) {
          proxy %>% leaflet::invokeMethod(NULL, "mnPointLabels", POINT_LAYER_ID_PREFIX, labels)
          st$labels <- labels
        }
        if (!identical(st$label_px, label_px)) {
          proxy %>% leaflet::invokeMethod(NULL, "mnPointLabelSize", label_px)
          st$label_px <- label_px
        }
      }
      pt_state[[map_id]] <- st
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

  # North arrow (add_north_arrow(), shared with the Data Setup mini-map)
  observe({
    map_overlay_rev()
    show <- isTRUE(input$show_north)
    for (map_id in overlay_map_ids) {
      proxy <- leafletProxy(map_id) %>% removeControl("north_ctrl")
      if (show) add_north_arrow(proxy)
    }
  })

  # Cell-size box: the cell size each locality of the DISPLAYED run was gridded
  # at, not the sidebar's settings for the next run.
  observe({
    map_overlay_rev()
    used <- rv$disp$grid_res_used
    show <- isTRUE(input$show_res_overlay) && length(used) > 0
    res_html <- if (show) {
      paste0("<div style='background:var(--mn-surface); color:var(--mn-text); padding:5px; border-radius:4px; border:1px solid var(--mn-line); font-size:12px; font-family:var(--mn-sans);'><b>Cell sizes:</b><br>", paste(htmltools::htmlEscape(names(used)), paste0(sapply(used, function(x) round(x, 2)), " m"), sep=": ", collapse="<br>"), "</div>")
    } else NULL
    for (map_id in overlay_map_ids) {
      proxy <- leafletProxy(map_id) %>% removeControl("res_overlay_ctrl")
      if (!is.null(res_html)) proxy %>% addControl(html = res_html, position = "bottomright", layerId = "res_overlay_ctrl")
    }
  })

  # Chrome toggles (Overlays panel), answered by a class on <body> and a CSS
  # rule: the toolbar and measure control are built into the widget, and the
  # legend title sits on three maps. isFALSE/isTRUE so the first flush, before
  # the input arrives, matches the checkbox default.
  observe({
    shinyjs::toggleClass(selector = "body", class = "mn-hide-drawtools",
                         condition = isFALSE(input$show_draw_tools))
  })
  observe({
    shinyjs::toggleClass(selector = "body", class = "mn-hide-ruler",
                         condition = isFALSE(input$show_ruler))
  })
  observe({
    shinyjs::toggleClass(selector = "body", class = "mn-show-legend-title",
                         condition = isTRUE(input$show_legend_title))
  })

  # Ruler readout, one observer per map (`local()` captures each id). Not
  # keyed on map_overlay_rev: it answers a gesture. The reply goes to the
  # token add_map_ruler() minted, landing in that measurement's own popup.
  for (rid in overlay_map_ids) local({
    map_id <- rid
    input_id <- paste0(map_id, "_ruler")
    observeEvent(input[[input_id]], {
      val <- input[[input_id]]
      token <- as.character(val$token %||% "")
      if (length(token) != 1 || !nzchar(token)) return(invisible(NULL))
      lon <- suppressWarnings(as.numeric(val$lng))
      lat <- suppressWarnings(as.numeric(val$lat))
      if (length(lon) < 2 || length(lon) != length(lat)) return(invisible(NULL))
      # The Target Mapping CRS of the run ON SCREEN; the live selection only
      # before a first run.
      ruler_crs <- rv$disp$crs_sel %||% input$crs_selection
      res <- tryCatch(measure_path_metrics(cbind(lon, lat), ruler_crs),
                      error = function(e) NULL)
      html <- map_ruler_popup_html(res)
      if (!is.null(html)) {
        session$sendCustomMessage("monolith_ruler_result",
                                  list(token = token, html = html))
      }
    }, ignoreInit = TRUE)
  })

  # Distance scale: the control is moved into #distance_scale_container on
  # creation and toggled with plain JS on the live maps (no re-render).
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
              sc.classList.add('mn-map-scale');
            }
          }
        });
      }, 400);
    ", if (show) "true" else "false"))
  })

  # View switcher: only the surfaces the committed run computed, so switching
  # is instant and a sidebar change cannot blank the map. SE/variance views
  # are offered for each surface the displayed run produced a variance band
  # for (rv$disp$variance_surfaces). Re-renders only on run dispatch/completion,
  # defaulting to the run's configured view.
  output$map_view_ui <- renderUI({
    req(rv$disp)
    d <- isolate(rv$disp)
    choices <- map_view_choices(has_pred = length(rv$rast_list_pre) > 0,
                                has_resid = length(rv$rast_list_res) > 0,
                                variance_surfaces = d$variance_surfaces)
    all_ids <- unlist(choices, use.names = FALSE)

    default_view <- if (identical(d$value_type, "resid")) "view_resid"
      else if (isTRUE(d$comp_mode) && d$value_type %in% c("pred", "pred_ss")) "view_comp"
      else if (d$value_type %in% c("pred", "pred_ss")) "view_pred"
      else "view_act"
    if (!default_view %in% all_ids) default_view <- "view_act"
    # An SE or variance view stays on that layer across runs while the new
    # run offers it, as a styling choice would.
    prev_layer <- parse_map_view(isolate(input$map_view))$layer
    carried <- paste0(default_view, "_", prev_layer)
    if (prev_layer != "value" && carried %in% all_ids) default_view <- carried

    selectInput("map_view", NULL, choices = choices, selected = default_view, width = "230px", selectize = FALSE)
  })
  # Kept current while hidden: the layout conditionalPanels read input$map_view.
  outputOptions(output, "map_view_ui", suspendWhenHidden = FALSE)

  # Runs before the renderers in the same flush, so a map reads the new view.
  observeEvent(input$map_view, {
    pv <- parse_map_view(input$map_view)
    map_view_base(pv$base)
    map_view_layer(pv$layer)
  }, priority = 100)

  map_layer_title <- function(surface) {
    if (!surface %in% rv$disp$variance_surfaces) return("")
    switch(map_view_layer(), se = ": Standard Error", var = ": Variance", "")
  }

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
    pred_view <- identical(map_view_base(), "view_pred")
    type_lab <- if (pred_view) disp_pred_label(d, long = TRUE) else "Actual Data View"
    paste0(d$label, " - ", type_lab, map_layer_title(if (pred_view) "pre" else "act"), disp_method_label(d))
  })

  output$comp_left_title <- renderText({
    d <- rv$disp; req(d)
    if (identical(map_view_base(), "view_resid")) return(paste0(d$label, " - Interpolated Residuals", disp_method_label(d)))
    paste0(d$label, " - Actual Data", map_layer_title("act"), disp_method_label(d))
  })

  output$comp_right_title <- renderText({
    d <- rv$disp; req(d)
    if (identical(map_view_base(), "view_resid")) return(paste0(d$label, " - Point Residuals", disp_method_label(d)))
    paste0(d$label, " - ", disp_pred_label(d), map_layer_title("pre"), disp_method_label(d))
  })

  # The base tiles of every map widget, re-added: the only tile update path for
  # rendered maps (draw_map isolates the choice). The CARTO key is part of
  # those tile URLs, so a new key re-adds them too.
  refresh_base_tiles <- function() {
    for (map_id in overlay_map_ids) {
      leafletProxy(map_id) %>%
        clearTiles() %>%
        add_base_tiles(input$base_map_layer, carto_api_key())
    }
  }
  observeEvent(list(input$base_map_layer, carto_api_key()), refresh_base_tiles())

  # WGS84 bbox of the uploaded rows with a position (all, or those the logical
  # `rows` selects); NULL when there is none or it cannot be computed. The CRS
  # is checked too: x/y can be mapped while the Input Data CRS is unset.
  upload_bbox <- function(rows = NULL) {
    ud <- rv$user_data
    if (is.null(ud) || is.null(rv$mapping$x) || is.null(rv$mapping$y) || !isTruthy(rv$mapping$crs)) return(NULL)
    tryCatch({
      xs <- coord_values(ud[[rv$mapping$x]])
      ys <- coord_values(ud[[rv$mapping$y]])
      keep <- is.finite(xs) & is.finite(ys)
      if (!is.null(rows)) keep <- keep & rows
      if (!any(keep)) return(NULL)
      st_bbox(st_transform(st_as_sf(data.frame(x = xs[keep], y = ys[keep]), coords = c("x", "y"),
                                    crs = rv$mapping$crs), 4326))
    }, error = function(e) NULL)
  }

  # WGS84 bbox of the current run area: committed boundary when one exists,
  # else the uploaded sample points. NULL before any data is loaded.
  run_area_bbox <- function() {
    if (!is.null(rv$bound)) {
      bb <- tryCatch(sf::st_bbox(sf::st_transform(sf::st_as_sf(rv$bound), 4326)), error = function(e) NULL)
      if (!is.null(bb)) return(bb)
    }
    upload_bbox()
  }

  # Fit map widgets to a WGS84 bbox (nothing for NULL or an empty one): by
  # default the main map, and the comparison panels in the views that show them.
  fit_maps_to_bbox <- function(bbox, ids = if (map_view_base() %in% c("view_comp", "view_resid")) overlay_map_ids
                                            else "main_map") {
    b <- as.numeric(unclass(bbox)[c("xmin", "ymin", "xmax", "ymax")])
    if (length(b) != 4 || !all(is.finite(b))) return(invisible(NULL))
    for (map_id in ids) leafletProxy(map_id) %>% fitBounds(b[1], b[2], b[3], b[4])
  }

  # Leaflet stashes layers drawn into a zero-size container until a resize, so
  # a map first rendered on a hidden tab stays grey. One resize races the tab
  # fade, so resizes are pumped until every VISIBLE map has rendered (capped
  # at ~3 s).
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

  # Fit the visible maps to the run area and re-measure their containers
  # (Refresh Map Area and reveal): a widget initialised without a view never
  # loads tiles.
  fit_maps_to_data <- function() {
    fit_maps_to_bbox(run_area_bbox())
    pump_map_resize()
  }

  # pump_map_resize() for every OTHER htmlwidget, DT above all: DT flushes a
  # payload stashed while hidden only from its resize handler, and the eager
  # Scientific Analysis tables are hidden when results land. A window resize
  # is dropped by Shiny's makeResizeFilter() for an unchanged box, so the
  # bindings are called directly; unbound or hidden widgets are skipped, and
  # the guards make it a no-op if Shiny's internals move.
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

  # The Data Setup mini-map also needs a re-fit: invalidateSize keeps a stale
  # centre and zoom after a re-render at another size, so it is re-fitted to
  # the stored sample bounds, repeatedly across the tab fade.
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

  # A widget rendered while hidden repairs itself when it becomes visible (tab
  # return or view switch), not only on Reveal Maps.
  observeEvent(input$main_tabs, {
    if (identical(input$main_tabs, "tab_map")) pump_map_resize()
    if (identical(input$main_tabs, "tab_data")) refit_setup_minimap()
    # Every tab: the stale-widget failure mode is not specific to one of them.
    pump_widget_resize()
  }, ignoreInit = TRUE)

  observeEvent(map_view_base(), {
    pump_map_resize()
  }, ignoreInit = TRUE)

  observeEvent(input$refresh_map_area, {
    req(input$base_map_layer)
    refresh_base_tiles()
    fit_maps_to_data()
  })

  output$main_map <- renderLeaflet({
    req(rv$disp, map_view_base() %in% c("view_act", "view_pred"))
    s <- map_surface("main_map")
    m <- draw_map(s$r, s$lab, map_id = "main_map")
    session_state$main_map_rendered <- TRUE
    map_overlay_rev(isolate(map_overlay_rev()) + 1L)
    m
  })

  # Both comparison panels, in the Comparison and Residuals views.
  for (.comp_id in c("comp_map_left", "comp_map_right")) local({
    map_id <- .comp_id
    output[[map_id]] <- renderLeaflet({
      req(rv$disp, map_view_base() %in% c("view_comp", "view_resid"))
      s <- map_surface(map_id)
      m <- draw_map(s$r, s$lab, map_id = map_id)
      session_state$comp_maps_rendered <- TRUE
      map_overlay_rev(isolate(map_overlay_rev()) + 1L)
      m
    })
  })
  output$locality_pan_ui <- renderUI({
    req(rv$loc_names)
    render_locality_pan_input(rv$loc_names)
  })

  # Pan to one locality's uploaded points, or to all the run's localities.
  observeEvent(input$locality_pan, {
    req(input$locality_pan, rv$user_data, rv$mapping$loc %in% colnames(rv$user_data))
    locs <- if (input$locality_pan == "global") rv$loc_names else input$locality_pan
    fit_maps_to_bbox(upload_bbox(rv$user_data[[rv$mapping$loc]] %in% locs))
  })

   # Manual-mode slider values enter the cache key only while they draw the
   # overlay line (mirroring the plot's own condition).
   vgm_manual_overlay_key <- function(target) {
     if (identical(input$vgm_mode, "manual") && identical(input$sel_loc_stats, input$m_loc)) {
       if (identical(manual_vgm_target(), target)) return(list(input$m_psill, input$k_mod, input$m_range, input$m_nugget))
     }
     NULL
   }

   # ── SA diagnostics build closures ────────────────────────────────────────
   # Each Scientific Analysis plot card shares ONE ggplot builder between its
   # cached in-page render, the expand modal and the 300-dpi PNG download.
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
         # Points land on the page as written, with showtext told the dpi.
         with_showtext_dpi(300, suppressWarnings(
           ggsave(file, plot = p, width = 9, height = 7, dpi = 300, bg = "white")))
       }
     )
   }

   build_vgm_structure_plot <- function(target) {
     loc <- input$sel_loc_stats
     # The displayed run's context, or the live sidebar while tuning
     # variograms and before a run, so tuned curves carry the tuned variable.
     meta <- if (isTRUE(sci_vgm_tuning())) get_current_meta() else get_display_meta()
     if (is.null(meta)) meta <- get_current_meta()
     req(loc, meta)
     tgt_label <- if (target == "act") "Actual" else "Predicted"
     if (loc == "Total (Combined)") {
       if (isTRUE(sci_vgm_tuning())) {
         return(sci_placeholder("Select a locality to inspect its stored variogram for the current variable and data subset."))
       }
       # Pooled within localities (no pair joins two); before the run's points
       # exist, the uploaded rows of the Total's localities stand in.
       col <- if (target == "act") "v" else "pv"
       pts_list <- if (!is.null(rv$sf) && col %in% colnames(rv$sf)) {
         split(rv$sf[col], rv$sf$loc)
       } else {
         src <- if (target == "act") meta$actual else if (identical(meta$value_type, "pred_ss")) meta$pred_ss else meta$pred
         req(rv$user_data, rv$mapping$x, rv$mapping$y, rv$mapping$crs, rv$mapping$loc)
         if (is.null(src) || !src %in% colnames(rv$user_data)) NULL else {
           d <- rv$user_data[rv$user_data[[rv$mapping$loc]] %in% rv$loc_names, , drop = FALSE]
           d <- data.frame(x = coord_values(d[[rv$mapping$x]]), y = coord_values(d[[rv$mapping$y]]),
                           val = d[[src]], loc = d[[rv$mapping$loc]])
           d <- d[is.finite(d$x) & is.finite(d$y), , drop = FALSE]
           names(d)[3] <- col
           split(sf::st_as_sf(d[c("x", "y", col)], coords = c("x", "y"), crs = rv$mapping$crs), d$loc)
         }
       }
       if (!length(pts_list)) {
         return(sci_placeholder(if (target == "act") {
           "The measured column of this variable is not in the uploaded data."
         } else {
           "Predicted data structure is not available.\nPlease run spatial interpolation first."
         }))
       }
       v <- pooled_within_variogram(pts_list, col)
       if (is.null(v)) {
         return(sci_placeholder(sprintf("No locality has %d located %s values.", POOLED_VGM_MIN_N,
                                        if (target == "act") "measured" else "prediction")))
       }
       cap <- pooled_within_caption(v)
       return(build_variogram_ggplot(v, title = paste0("Pooled within-locality variogram (", tgt_label, "): ",
                                                       sci_disp_label(meta), ", ", cap$count),
                                     subtitle = cap$note))
     }

     if (isTRUE(sci_vgm_tuning())) {
       key <- current_tuning_keys()[[target]]
       v_emp <- rv$v_emp_list[[paste0(loc, "_", target)]]
       v_fit <- rv$v_fit_list[[paste0(loc, "_", target)]]
       if (is.na(key)) {
         return(sci_placeholder(paste0("The current variable has no ",
           if (target == "act") "measured" else "uploaded prediction", " column to tune for this view.")))
       }
       if (!vgm_key_matches(v_emp, key)) {
         return(sci_placeholder(paste0("No variogram of ", key, " is stored for ", loc,
           ".\nSwitch to Auto-Fit and press OPTIMIZE ALL VARIOGRAMS,\nor run Ordinary Kriging, then return to Manual.")))
       }
       if (!vgm_key_matches(v_fit, key)) v_fit <- NULL
     } else {
       v_emp <- rv$disp$v_emps[[paste0(loc, "_", target)]]
       v_fit <- rv$disp$v_fits[[paste0(loc, "_", target)]]
     }
     if (is.null(v_emp)) {
       return(sci_placeholder(paste0("No fitted variogram for this locality yet (", tgt_label, ").\nPress OPTIMIZE ALL VARIOGRAMS in the sidebar (Fitting Mode: Auto-Fit)\nor run an interpolation first.")))
     }
     manual_model <- NULL; manual_sub <- NULL
     manual_applies <- isTRUE(sci_vgm_tuning()) && input$vgm_mode == "manual" && loc == input$m_loc &&
       identical(manual_vgm_target(), target)
     if (isTRUE(manual_applies)) {
       manual_model <- manual_vgm(input$m_psill, input$k_mod, input$m_range, input$m_nugget)
       manual_sub <- paste("Manual model (red dashed) - weighted SSE (same criterion as Auto-Fit):",
                           signif(vgm_weighted_sse(v_emp, manual_model), 4))
       if (isTRUE(vgm_smooth_nugget_share(manual_model) < VGM_SMOOTH_NUGGET_WARN_SHARE)) {
         manual_sub <- paste0(manual_sub, "\nUnstable in kriging: ", input$k_mod,
                              " with a nugget below 5% of the sill")
       }
     }
     # Parameters head the subtitle; a target without usable variance shows
     # its points, not a model of the noise (build_fitted_variogram_plot()).
     build_fitted_variogram_plot(v_emp, v_fit,
                                 title = paste0("Fitted (", tgt_label, "): ", loc),
                                 extra_sub = manual_sub, manual_model = manual_model)
   }

   output$vgm_plot_main <- cached_plot(function() build_vgm_structure_plot("act"), function() {
     loc <- input$sel_loc_stats
     # input$var_id covers the pre-run state (no rv$disp yet).
     list("vgm_main", loc, rv$results_rev, rv$disp$actual %||% input$var_id,
          sci_vgm_tuning(), input$var_id, input$sci_name_mode,
          current_tuning_keys(),
          if (isTRUE(sci_vgm_tuning())) rv$v_emp_list[[paste0(loc, "_act")]] else rv$disp$v_emps[[paste0(loc, "_act")]],
          if (isTRUE(sci_vgm_tuning())) rv$v_fit_list[[paste0(loc, "_act")]] else rv$disp$v_fits[[paste0(loc, "_act")]],
          vgm_manual_overlay_key("act"))
   })
   output$vgm_plot_pred <- cached_plot(function() build_vgm_structure_plot("pre"), function() {
     loc <- input$sel_loc_stats
     list("vgm_pred", loc, rv$results_rev, rv$disp$actual %||% input$var_id,
          sci_vgm_tuning(), input$var_id,
          rv$disp$value_type %||% input$value_type, input$sci_name_mode,
          current_tuning_keys(),
          if (isTRUE(sci_vgm_tuning())) rv$v_emp_list[[paste0(loc, "_pre")]] else rv$disp$v_emps[[paste0(loc, "_pre")]],
          if (isTRUE(sci_vgm_tuning())) rv$v_fit_list[[paste0(loc, "_pre")]] else rv$disp$v_fits[[paste0(loc, "_pre")]],
          vgm_manual_overlay_key("pre"))
   })
