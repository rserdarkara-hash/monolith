# ui_plotting.R - ggplot/plotly/leaflet-layer builders (variogram, styler,
# descriptive, correlation, PCA, styled points). Pure functions; no
# reactivity. Sourced via ui_helpers.R.


# Builds the Map Viewer variogram-quality banner from the per-locality fit
# list, one band per distinct condition. Red = the fit failed entirely and a
# heuristic model was used; amber = a non-converged/singular candidate was
# selected, a converged fit whose range the lags cannot resolve, a variogram
# still rising at the cutoff, or a smooth family at a negligible nugget.
# A converged fit is NEVER reported as a failed one. Returns NULL when clean.
# `target` limits the banner to the fits a specific map actually used:
# "act" (actual maps), "pre" (predicted maps), or NULL for both (residual
# maps, which derive from both fits).
# `engine` is the displayed run's method. It selects the wording of the
# still-rising band only: RK and RFK fit the variogram to the residuals of a
# trend model, so what a value-scale variogram is advised to do about a trend
# is what they already did. CK never reaches here (it sets no `res$fit`, so it
# contributes no entry to the fit list) and, like OK, does not detrend.
build_vgm_warning_html <- function(v_fit_list, target = NULL, engine = NULL) {
  # "LocA_act" -> "LocA" when the map's target is known, otherwise
  # "LocA (actual)" so mixed banners stay unambiguous.
  display_key <- function(n) {
    base <- sub("_(act|pre)$", "", n)
    if (!is.null(target)) return(base)
    suffix <- if (grepl("_act$", n)) " (actual)" else if (grepl("_pre$", n)) " (predicted)" else ""
    paste0(base, suffix)
  }

  fallback_keys <- character(0)
  flawed_keys <- character(0)
  smooth_keys <- character(0)
  beyond_keys <- character(0)
  below_keys <- character(0)
  trend_keys <- character(0)
  for (n in names(v_fit_list)) {
    if (!is.null(target) && !grepl(paste0("_", target, "$"), n)) next
    f <- v_fit_list[[n]]
    status <- vgm_fit_status(f)
    if (status %in% c("fit_failed", "heuristic_fallback")) {
      fallback_keys <- c(fallback_keys, display_key(n))
    } else if (identical(status, "singular_selected")) {
      flawed_keys <- c(flawed_keys, display_key(n))
    } else if (identical(status, "range_unresolved")) {
      # The window fails at BOTH ends and the two mean opposite things, so the
      # side is recorded at fit time rather than re-derived here.
      if (identical(attr(f, "vgm_diagnostics")$range_side, "below")) {
        below_keys <- c(below_keys, display_key(n))
      } else {
        beyond_keys <- c(beyond_keys, display_key(n))
      }
    }
    if (isTRUE(attr(f, "vgm_diagnostics")$trend_suspected)) {
      trend_keys <- c(trend_keys, display_key(n))
    }
    if (isTRUE(vgm_smooth_nugget_share(f) < VGM_SMOOTH_NUGGET_WARN_SHARE)) {
      smooth_keys <- c(smooth_keys, display_key(n))
    }
  }
  if (length(fallback_keys) == 0 && length(flawed_keys) == 0 && length(smooth_keys) == 0 &&
      length(beyond_keys) == 0 && length(below_keys) == 0 && length(trend_keys) == 0) return(NULL)

  red_part <- if (length(fallback_keys) > 0) {
    paste0("<span style='color:var(--mn-danger);'>Variogram fit failed for: ",
           paste(fallback_keys, collapse = ", "),
           ".<br>-A default spherical variogram model was used, so the spatial structure was not ",
           "estimated from your data. Interpret with caution.</span>")
  } else ""
  amber_part <- if (length(flawed_keys) > 0) {
    paste0("<span style='color:var(--mn-warn);'>-Auto-fit selected a non-converged or singular variogram for: ",
           paste(flawed_keys, collapse = ", "),
           ".<br>No candidate converged cleanly, so the lowest-error fit was used. Its parameters ",
           "may be imprecise; the map remains usable.</span>")
  } else ""
  # A converged fit whose practical range lies outside the span the empirical
  # variogram resolves. The model is used; what it CLAIMS is what is limited.
  unresolved_part <- if (length(beyond_keys) > 0 || length(below_keys) > 0) {
    beyond_line <- if (length(beyond_keys) > 0) {
      paste0("<br>-Practical range extends beyond sampled lag support for: ",
             paste(beyond_keys, collapse = ", "),
             ". Sill and range are extrapolated.")
    } else ""
    below_line <- if (length(below_keys) > 0) {
      paste0("<br>-Practical range is below sampled-distance resolution for: ",
             paste(below_keys, collapse = ", "),
             ". Under the effective short-range resolution threshold the lags cannot resolve ",
             "structure at that scale, so the fit behaves as near-pure nugget.")
    } else ""
    paste0("<span style='color:var(--mn-warn);'>-Variogram range not resolved within the empirical lag window.",
           "<br>The fit converged; what the lags cannot resolve is its range.",
           beyond_line, below_line, "</span>")
  } else ""
  # Hedged on purpose: a monotone rising variogram is a strong diagnostic for
  # trend, not proof of it. Never state that non-stationarity was detected.
  # Neither branch names an engine to switch to: the empirical variogram cannot
  # say which cause is acting, and naming one would be circular for whichever
  # engine is already running.
  trend_part <- if (length(trend_keys) > 0) {
    if (isTRUE(engine %in% c("RK", "RFK"))) {
      paste0("<span style='color:var(--mn-warn);'>-Residual variogram still rising at the lag cutoff for: ",
             paste(trend_keys, collapse = ", "),
             ".<br>The fitted trend has not removed the large-scale structure, so range and sill are ",
             "weakly constrained and the kriging variance with them. Causes it cannot separate: ",
             "covariates missing that variation, too rigid a trend form, or a correlation length ",
             "beyond this locality's extent, which no covariate fixes. Predictions near samples stay ",
             "well supported; see the trend diagnostics and Internal Residual Variogram ",
             "(Scientific Analysis).</span>")
    } else {
      paste0("<span style='color:var(--mn-warn);'>-Sill not observed; possible large-scale trend for: ",
             paste(trend_keys, collapse = ", "),
             ".<br>The variogram is still rising at the cutoff, so range and sill are weakly ",
             "constrained: long-range dependence or non-stationarity, which it cannot separate. ",
             "Modelling the trend with covariates and kriging its residuals addresses the trend ",
             "case; the variogram alone cannot say which case applies.</span>")
    }
  } else ""
  smooth_part <- if (length(smooth_keys) > 0) {
    paste0("<span style='color:var(--mn-warn);'>-Gaussian or Mat&eacute;rn variogram with a nugget below 5% of the sill for: ",
           paste(smooth_keys, collapse = ", "),
           ".<br>Predictions can land far outside the observed range. Check the map against the ",
           "data, or add a nugget.</span>")
  } else ""
  parts <- c(red_part, amber_part, unresolved_part, trend_part, smooth_part)

  paste0("<div class='vgm-fallback-warn' style='font-weight:bold; background:var(--mn-surface); border:1px solid var(--mn-line); padding:5px 25px 5px 5px; border-radius:4px; position:relative;'>",
         "<button onclick='this.parentElement.style.display=\"none\";' style='position:absolute; top:2px; right:2px; background:none; border:none; color:var(--mn-danger); font-size:16px; font-weight:bold; cursor:pointer;'>&times;</button>",
         paste(parts[nzchar(parts)], collapse = "<br>"),
         "</div>")
}

# Text-only panel for a diagnostic that has nothing to plot, so the card states
# why instead of standing empty: the one empty state of every Scientific
# Analysis plot card, the server chunks' and the selection panels' below alike.
sci_placeholder <- function(msg, size = 5) {
  ggplot() +
    annotate("text", x = 4, y = 4, label = msg, size = size, color = "grey40") +
    theme_void()
}

#' TPS smoothing-selection panel from the run's own fit record for one
#' locality and surface (`fit` = res$tps_fit: mode, lambda, eff_df and, for
#' Auto (GCV), the GCV grid, whose values it was computed on, and the end of
#' the family GCV's minimum reached, with exact interpolation's
#' cross-validation at the least-smoothing end). The curve is fields' coarse
#' lambda grid on a log axis; fields optimises lambda continuously, so the
#' vertical line is the FITTED lambda, not a grid point.
build_tps_gcv_plot <- function(fit, loc, target = c("act", "pre")) {
  target <- match.arg(target)
  target_label <- if (target == "act") "Actual" else "Predicted"
  if (identical(loc, "Total (Combined)")) {
    return(sci_placeholder("TPS smoothing is selected per locality.\nSelect a locality in the filter above."))
  }
  if (is.null(fit) || !isTRUE(is.finite(fit$lambda))) {
    return(sci_placeholder(paste0(
      "No TPS fit for '", loc, "' (", target_label, "):\n",
      "the run fell back to IDW or skipped this locality (see the Run Log).")))
  }
  lam_txt <- format_sig(fit$lambda)
  if (identical(fit$mode, "exact")) {
    return(sci_placeholder(paste0("λ = 0 (exact interpolation) for '", loc,
                                   "' (", target_label, "): no GCV search.")))
  }
  if (identical(fit$mode, "fixed")) {
    return(sci_placeholder(paste0("Fixed λ = ", lam_txt, " for '", loc,
                                   "' (", target_label, "): no GCV search.")))
  }
  df <- fit$gcv
  if (is.null(df) || nrow(df) == 0) {
    return(sci_placeholder(paste0("No GCV curve was recorded for '", loc, "' (", target_label, ").")))
  }
  cols <- if (target == "act") c("steelblue", "darkblue") else c("firebrick", "darkred")
  ex <- fit$exact_cv_rmse
  run <- fit$run_cv_rmse
  end_txt <- switch(fit$gcv_end %||% "",
    plane = "GCV's minimum is the smoothest end of the family, the least-squares plane: nothing lies beyond it.",
    interpolation = if (isTRUE(is.finite(ex)) && isTRUE(is.finite(run))) {
      paste0("GCV's minimum is the least smoothing it evaluates. Exact interpolation (λ = 0), beyond it, ",
             "cross-validates at RMSE ", format_sig(ex), " against ", format_sig(run), " for this run",
             if (ex < run) ": select Exact (λ = 0)." else ".")
    } else {
      paste0("GCV's minimum is the least smoothing it evaluates; exact interpolation (λ = 0) lies beyond it ",
             "and could not be compared on these folds: set Smoothing (λ) to Exact (λ = 0) and run again to compare.")
    },
    NULL)
  sub <- paste0("Fitted λ = ", lam_txt, " (effective df ", format_sig(fit$eff_df), ")",
                if (identical(fit$gcv_source, "measured")) "; λ from GCV on the measured values",
                if (!is.null(end_txt)) paste0("\n", wrap_lines(end_txt, 100)))
  ggplot(df, aes(x = lambda, y = gcv)) +
    geom_line(color = cols[1], linewidth = 1) +
    geom_point(color = cols[2]) +
    geom_vline(xintercept = fit$lambda, linetype = "dashed", color = "grey30") +
    scale_x_log10() + theme_minimal() +
    labs(title = paste0("GCV Curve (", target_label, "): ", loc), subtitle = sub,
         x = "λ (log scale)", y = "GCV score")
}

#' IDW power-selection panel from the run's own fit record for one locality and
#' surface (`fit` = res$idw_fit): under Auto (CV) the pooled CV RMSE of every
#' power searched on all rows, from equal weights (p = 0) to the
#' nearest-neighbour limit, the selected power, the spread of the powers the
#' CV folds selected from their own training rows, how well the data separate
#' the powers (filled points are within one standard error of the best;
#' idw_flatness_note) and, for a selection at an end of the family or at a
#' steep power, what it means (idw_limit_note). An unseparated Predicted
#' surface's power and profile come from the measured values, and the panel
#' says so.
build_idw_power_plot <- function(fit, loc, target = c("act", "pre")) {
  target <- match.arg(target)
  target_label <- if (target == "act") "Actual" else "Predicted"
  if (identical(loc, "Total (Combined)")) {
    return(sci_placeholder("The IDW power is selected per locality.\nSelect a locality in the filter above."))
  }
  if (is.null(fit)) {
    return(sci_placeholder(paste0("No IDW fit for '", loc, "' (", target_label, "): see the Run Log.")))
  }
  if (identical(fit$mode, "fixed")) {
    return(sci_placeholder(paste0("Fixed ", idw_power_text(fit$p), " for '", loc,
                                   "' (", target_label, "): no selection.")))
  }
  if (!is.null(fit$skipped) || is.null(fit$profile)) {
    return(sci_placeholder(paste0("Auto (CV) did not search for '", loc, "' (", target_label, "):\n",
                                   fit$skipped %||% "no profile recorded", "; p = 2.")))
  }
  fp <- fit$fold_p
  prof <- fit$profile
  note <- idw_limit_note(fit$limit, fit$nmax %||% "Max Neighbors", fit[["n_samples"]], fit$p)
  flat <- idw_flatness_note(prof, fit$p)
  measured <- identical(fit$select_source, "measured")
  sub <- paste0("Selected ", idw_power_text(fit$p),
                if (measured) " on the measured values of all rows" else " on all rows",
                if (length(fp)) paste0("; CV folds selected ", format_power(stats::median(fp)),
                                       " [", format_power(min(fp)), "–", format_power(max(fp)), "]"),
                if (!is.null(flat)) paste0("\n", wrap_lines(flat, 100)),
                if (!is.null(note)) paste0("\n", wrap_lines(note, 100)))
  col <- if (target == "act") "steelblue" else "firebrick"
  # Filled points are within one standard error of the best, hollow ones the
  # data separate from it; a record without that flag draws every point filled.
  prof$shape <- ifelse(if (is.null(prof$within_se)) TRUE else prof$within_se, 16, 1)
  cap <- paste0(if (!is.null(prof$within_se)) "Filled: within one standard error of the best (paired, on the same folds). ",
                "Each power is scored at that power in every fold; Model Performance reports the CV with the power ",
                "re-selected in each fold.")
  # The whole family on one axis: p on a log(1 + p) scale from equal weights
  # (0) to the largest finite power, and the nearest-neighbour limit set apart
  # at its right end.
  top <- log1p(max(prof$p[is.finite(prof$p)]))
  pos <- function(p) ifelse(is.finite(p), log1p(p), top + 0.45)
  prof$x <- pos(prof$p)
  fin <- prof[is.finite(prof$p), , drop = FALSE]
  brk <- c(0, 1, 2, 4, 6, 12, 24, 48)
  brk <- brk[brk <= max(fin$p)]
  p <- ggplot(fin, aes(x = x, y = rmse)) +
    geom_line(color = col, linewidth = 1) +
    geom_point(aes(shape = shape), color = col, size = 2) +
    scale_shape_identity() +
    geom_vline(xintercept = pos(fit$p), linetype = "dashed", color = "grey30") +
    theme_minimal() +
    labs(title = paste0("IDW Power Selection (", target_label, "): ", loc), subtitle = sub,
         x = "Power p (0 = equal weights, ∞ = nearest neighbour)",
         y = if (measured) "Pooled CV RMSE of the measured values" else "Pooled CV RMSE",
         caption = wrap_lines(cap, 110))
  nn <- prof[!is.finite(prof$p), , drop = FALSE]
  if (nrow(nn)) {
    p <- p + geom_point(data = nn, aes(shape = shape), color = col, size = 2.6) +
      scale_x_continuous(breaks = c(log1p(brk), pos(Inf)), labels = c(format_power(brk), "∞"))
  } else {
    p <- p + scale_x_continuous(breaks = log1p(brk), labels = format_power(brk))
  }
  p
}

#' CV Distance Match panel from cv_distance_summary() records: one design, or a
#' named list of them (one facet per surface, the name leading its strip). The
#' cumulative distributions of three nearest-neighbour distances, drawn as
#' steps through their percentiles: from the map's locations to the nearest
#' sample (the target, strongest line), from each held-out sample to the
#' nearest training sample under the run's folds, and from each sample to the
#' nearest other sample (the sampling density, dashed). The distance axis is a
#' square root: the distances are right-skewed, and the root keeps zeros. W,
#' the area between the map and held-out curves, is given for the run's folds
#' and for the record's random reference partition (`reference`). Colour
#' follows the curve, never the surface; line type and width carry the same
#' identity, so it is never colour alone.
build_cv_distance_plot <- function(design, title = NULL) {
  designs <- if (!is.null(design$probs)) list(design) else Filter(Negate(is.null), design %||% list())
  if (!length(designs)) {
    return(sci_placeholder(paste0("No distance record: the cross-validation was skipped,\n",
                                  "or the map had no prediction domain (see the Run Log).")))
  }
  d1 <- designs[[1]]
  unit_txt <- if (is.na(d1$units %||% NA_character_)) "map units" else d1$units
  w_pair <- function(d) sprintf("W, this CV: %s · W, %s: %s",
                                format_sig(d$W_cv), d$reference, format_sig(d$W_random))
  series <- c(map = "Map cells → nearest sample",
              cv = "Held-out → nearest training sample (this CV)",
              sample = "Sample → nearest other sample")
  faceted <- !is.null(names(designs))
  panel_lab <- if (faceted) {
    sprintf("%s, n = %d\n%s", names(designs), vapply(designs, function(d) as.integer(d$n), integer(1)),
            vapply(designs, w_pair, character(1)))
  } else ""
  df <- do.call(rbind, lapply(seq_along(designs), function(i) {
    d <- designs[[i]]
    data.frame(panel = panel_lab[i], series = rep(names(series), each = length(d$probs)),
               x = c(d$map, d$cv, d$sample), y = rep(d$probs, 3))
  }))
  df$panel <- factor(df$panel, levels = unique(panel_lab))
  df$series <- factor(df$series, levels = names(series), labels = unname(series))
  # Round-number breaks spread evenly in square-root space, so the short
  # distances, where most of the mass lies, carry labels too.
  sqrt_breaks <- function(lim) {
    hi <- max(lim, na.rm = TRUE)
    if (!is.finite(hi) || hi <= 0) return(0)
    cand <- sort(as.vector(outer(c(1, 2, 5), 10^(-3:7))))
    cand <- cand[cand <= hi]
    keep <- 0
    for (b in cand) if (sqrt(b) - sqrt(keep[length(keep)]) >= sqrt(hi) / 7) keep <- c(keep, b)
    keep
  }
  p <- ggplot(df, aes(x = x, y = y, colour = series, linetype = series, linewidth = series)) +
    geom_step(direction = "hv") +
    scale_colour_manual(values = c("#262626", "#2a78d6", "#8c8c8c"), name = NULL) +
    scale_linetype_manual(values = c("solid", "solid", "22"), name = NULL) +
    scale_linewidth_manual(values = c(1.3, 0.9, 0.7), name = NULL) +
    scale_x_sqrt(breaks = sqrt_breaks, labels = function(b) format(b, scientific = FALSE, trim = TRUE,
                                                                   drop0trailing = TRUE)) +
    scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE), linetype = guide_legend(nrow = 2, byrow = TRUE),
           linewidth = guide_legend(nrow = 2, byrow = TRUE)) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.key.width = grid::unit(1.8, "lines"),
          legend.key.height = grid::unit(0.9, "lines"), legend.key.spacing.y = grid::unit(0, "pt"),
          legend.margin = ggplot2::margin(0, 0, 0, 0), legend.box.spacing = grid::unit(2, "pt")) +
    labs(title = title %||% "CV Distance Match",
         subtitle = if (faceted) paste0("W, in ", unit_txt, ", is the area between the map and held-out curves")
                    else sprintf("%s (%s); n = %d samples", w_pair(d1), unit_txt, as.integer(d1$n)),
         x = paste0("Distance (", unit_txt, ", square-root scale)"), y = "Cumulative share",
         caption = wrap_lines(sprintf(paste0(
           "Map cells: %d points spread evenly inside the boundary. Held-out curve left of the map ",
           "curve: metrics optimistic for this map; right: pessimistic."), as.integer(d1$n_domain)), 85))
  if (faceted) p <- p + facet_wrap(~ panel, nrow = 1)
  p
}

# RF variable-importance dot chart from a randomForest fit: one panel per
# measure rf_importance_df() reports (the numeric record exported beside it),
# covariates ordered by the out-of-bag MSE increase, largest at the top.
# Covariate names map to display labels when variable metadata is supplied
# (NULL keeps raw column names).
build_rf_importance_plot <- function(rf_mod, title, vars_metadata = NULL) {
  df <- rf_importance_df(rf_mod)
  if (nrow(df) == 0) return(NULL)
  # make.unique: two covariates sharing a metadata label would otherwise
  # collapse into one factor level.
  df$Variable <- make.unique(unname(get_var_labels(df$Variable, vars_metadata)))
  measures <- setdiff(names(df), "Variable")
  long <- tidyr::pivot_longer(df, -Variable, names_to = "Measure", values_to = "Importance")
  long$Measure <- factor(long$Measure, levels = measures)
  long$Variable <- factor(long$Variable, levels = rev(df$Variable))
  ggplot(long, aes(x = Importance, y = Variable)) +
    geom_segment(aes(x = 0, xend = Importance, yend = Variable), color = "grey70", linewidth = 0.4) +
    geom_point(color = "steelblue", size = 2.5) +
    facet_wrap(~Measure, scales = "free_x", labeller = label_wrap_gen(width = 20)) +
    labs(title = title, x = NULL, y = NULL,
         caption = if (RF_IMPORTANCE_LABELS[["scaled"]] %in% measures) {
           paste0("The scaled measure grows with the number of trees (", rf_mod$ntree,
                  " here); IncNodePurity is computed on the trees' own training rows.")
         }) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(size = 12, face = "bold"))
}

# Rename the ids of a CK cross-variogram (empirical vm + fitted model list)
# to display labels. id_labels is a named character vector keyed by the
# gstat ids; compound cross ids ("a.b") are matched against the exact
# pairwise combinations, so ids containing dots cannot be mis-split.
relabel_ck_variogram <- function(vm, model, id_labels) {
  ids <- names(id_labels)
  if (is.null(ids) || length(ids) == 0) return(list(vm = vm, model = model))
  resolve <- function(lv) {
    if (lv %in% ids) return(id_labels[[lv]])
    for (i in seq_along(ids)) for (j in seq_along(ids)) {
      if (i != j && identical(lv, paste(ids[i], ids[j], sep = "."))) {
        return(paste(id_labels[[i]], id_labels[[j]], sep = " × "))
      }
    }
    lv
  }
  old_lev <- levels(vm$id)
  new_lev <- make.unique(vapply(old_lev, resolve, character(1)), sep = " ")
  levels(vm$id) <- new_lev
  if (!is.null(model) && !is.null(names(model))) {
    names(model) <- vapply(names(model), function(nm) {
      idx <- match(nm, old_lev)
      if (!is.na(idx)) new_lev[idx] else resolve(nm)
    }, character(1))
  }
  list(vm = vm, model = model)
}

# Semivariogram as ggplot, replacing the lattice plot(v_emp, v_fit) look:
# empirical lags as points (hover text carries np / distance / semivariance
# for the plotly expand mode), the fitted model as a line via
# gstat::variogramLine, and an optional manual-tuning overlay (red dashed).
# Identical numbers to the lattice version - presentation only. The `text`
# aesthetic lives on the point layer alone so it cannot fragment the
# grouping of any line layer.
build_variogram_ggplot <- function(v_emp, v_fit = NULL, title = "", subtitle = NULL,
                                   manual_model = NULL) {
  if (is.null(v_emp) || nrow(v_emp) == 0) return(NULL)
  df <- as.data.frame(v_emp)
  max_d <- max(df$dist, na.rm = TRUE)
  # suppressWarnings: `text` is a plotly-only aesthetic; ggplot2 warns
  # "Ignoring unknown aesthetics" at layer construction but keeps the mapping,
  # which is exactly what ggplotly_smart() consumes.
  p <- ggplot(df, aes(x = dist, y = gamma)) +
    suppressWarnings(geom_point(aes(text = paste0("Distance: ", round(dist, 1),
                                                  "\nSemivariance: ", signif(gamma, 4),
                                                  "\nPairs (np): ", np)),
                                color = "steelblue", size = 2, alpha = 0.85))
  if (!is.null(v_fit)) {
    line_df <- tryCatch(gstat::variogramLine(v_fit, maxdist = max_d), error = function(e) NULL)
    if (!is.null(line_df)) {
      p <- p + geom_line(data = line_df, aes(x = dist, y = gamma),
                         inherit.aes = FALSE, color = "#1c3d5a", linewidth = 0.7)
    }
  }
  if (!is.null(manual_model)) {
    m_df <- tryCatch(gstat::variogramLine(manual_model, maxdist = max_d), error = function(e) NULL)
    if (!is.null(m_df)) {
      p <- p + geom_line(data = m_df, aes(x = dist, y = gamma),
                         inherit.aes = FALSE, color = "#e03131", linewidth = 0.8, linetype = "dashed")
    }
  }
  p + expand_limits(y = 0) +
    labs(title = title, subtitle = subtitle, x = "Distance (m)", y = "Semivariance") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(size = 12, face = "bold"),
          plot.subtitle = element_text(size = 9, color = "grey30", lineheight = 1.3))
}

# A fitted variogram's plot with the model's parameters as its subtitle
# (vgm_fit_subtitle), the one form every variogram panel and export uses. A
# variable with no usable variance draws its points and the note instead: its
# fitted curve and parameters describe numerical noise. `extra_sub` lines (a
# manual-tuning overlay's) follow the parameters.
build_fitted_variogram_plot <- function(v_emp, v_fit, title, extra_sub = NULL,
                                        manual_model = NULL) {
  fit_sub <- vgm_fit_subtitle(v_fit)
  if (vgm_target_degenerate(v_fit)) {
    v_fit <- NULL
    fit_sub <- VGM_DEGENERATE_NOTE
  }
  lines <- c(fit_sub, extra_sub)
  build_variogram_ggplot(v_emp, v_fit, title = title,
                         subtitle = if (length(lines)) paste(lines, collapse = "\n"),
                         manual_model = manual_model)
}

# Directional variogram (anisotropy diagnostic) from calc_directional_variogram:
# one coloured curve per angular cone on shared axes, because the question the
# panel answers ("does the range differ with direction?") is a comparison
# between the curves, which facets would make harder to see.
build_directional_variogram_ggplot <- function(vd, title = "", subtitle = NULL) {
  if (is.null(vd) || nrow(vd) == 0 || !"dir.hor" %in% names(vd)) return(NULL)
  df <- as.data.frame(vd)

  # gstat's alpha is a bearing clockwise from north; spell the axis out so the
  # reader does not have to remember the convention.
  compass <- c("0" = "N-S", "45" = "NE-SW", "90" = "E-W", "135" = "NW-SE")
  ang <- sort(unique(df$dir.hor))
  dir_labels <- vapply(ang, function(a) {
    nm <- compass[[as.character(a)]]
    if (is.null(nm) || is.na(nm)) sprintf("%g°", a) else sprintf("%g° (%s)", a, nm)
  }, character(1))
  df$dir <- factor(df$dir.hor, levels = ang, labels = dir_labels)

  pal <- c("#1f78b4", "#e31a1c", "#33a02c", "#ff7f00", "#6a3d9a", "#b15928")

  # suppressWarnings: `text` is a plotly-only aesthetic (see build_variogram_ggplot).
  ggplot(df, aes(x = dist, y = gamma, colour = dir)) +
    suppressWarnings(geom_point(aes(text = paste0("Direction: ", dir,
                                                  "\nDistance: ", round(dist, 1),
                                                  "\nSemivariance: ", signif(gamma, 4),
                                                  "\nPairs (np): ", np)),
                                size = 1.9, alpha = 0.9)) +
    geom_line(linewidth = 0.6, alpha = 0.9) +
    expand_limits(y = 0) +
    scale_colour_manual(values = rep_len(pal, nlevels(df$dir))) +
    labs(title = title, subtitle = subtitle, x = "Distance (m)",
         y = "Semivariance", colour = "Direction") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(size = 12, face = "bold"),
          plot.subtitle = element_text(size = 9, color = "grey30"),
          legend.position = "bottom")
}

# CK cross-variogram matrix as ggplot: one facet per gstat id (direct and
# cross semivariograms), fitted LMC lines matched to facets by model name.
# Call relabel_ck_variogram first when display labels are wanted; ids absent
# from `model` simply get no line. Same numbers as plot(vm, model = ...).
build_ck_variogram_ggplot <- function(vm, model, title = "") {
  if (is.null(vm) || nrow(vm) == 0) return(NULL)
  df <- as.data.frame(vm)
  lev <- if (is.factor(vm$id)) levels(vm$id) else unique(df$id)
  df$id <- factor(df$id, levels = lev)
  max_d <- max(df$dist, na.rm = TRUE)

  line_df <- NULL
  if (!is.null(model) && !is.null(names(model))) {
    pieces <- lapply(intersect(names(model), lev), function(nm) {
      ld <- tryCatch(gstat::variogramLine(model[[nm]], maxdist = max_d), error = function(e) NULL)
      if (is.null(ld)) return(NULL)
      ld$id <- nm
      ld
    })
    pieces <- Filter(Negate(is.null), pieces)
    if (length(pieces) > 0) {
      line_df <- do.call(rbind, pieces)
      line_df$id <- factor(line_df$id, levels = lev)
    }
  }

  p <- ggplot(df, aes(x = dist, y = gamma)) +
    suppressWarnings(geom_point(aes(text = paste0("Distance: ", round(dist, 1),
                                                  "\nSemivariance: ", signif(gamma, 4),
                                                  "\nPairs (np): ", np)),
                                color = "steelblue", size = 1.8, alpha = 0.85))
  if (!is.null(line_df)) {
    p <- p + geom_line(data = line_df, aes(x = dist, y = gamma),
                       inherit.aes = FALSE, color = "#1c3d5a", linewidth = 0.6)
  }
  p + facet_wrap(~id, scales = "free_y") +
    labs(title = title, x = "Distance (m)", y = "Semivariance") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(size = 12, face = "bold"),
          strip.text = element_text(size = 8.5))
}

# A ggplot's title with its subtitle and caption as smaller lines under it, for
# a plotly layout title. ggplotly() drops both, yet they carry what the figure
# rests on (the complete-case n, the variables a PCA left out, the estimator),
# so they must reach the interactive view and the PNG its camera button saves.
plotly_title_html <- function(p) {
  paste0(plotly_text(p$labels$title %||% ""), plotly_notes_html(plot_notes(p)))
}
plotly_notes_html <- function(notes) {
  paste0("<br><sup>", plotly_text(notes), "</sup>", collapse = "", recycle0 = TRUE)
}
plotly_text <- function(x) gsub("\n", "<br>", htmltools::htmlEscape(x), fixed = TRUE)
plot_notes <- function(p) {
  notes <- as.character(c(p$labels$subtitle, p$labels$caption))
  notes[nzchar(notes)]
}

# Each line of `x` wrapped at word boundaries to fewer than `width` characters;
# the line breaks already in it stay. Unnamed, so a scale's labels stay matched
# to its breaks by position.
wrap_lines <- function(x, width) {
  vapply(strsplit(x, "\n", fixed = TRUE),
         function(l) paste(unlist(lapply(l, strwrap, width = width)), collapse = "\n"), "",
         USE.NAMES = FALSE)
}

# The text metrics the expanded view budgets with: plotly.js sets a line of
# text every 1.3 em, an average sans-serif character is about 0.55 em wide, and
# a <sup> line is drawn at 70% of the title's size.
PLOTLY_LINE_EM <- 1.3
PLOTLY_CHAR_EM <- 0.55
PLOTLY_SUP_SCALE <- 0.7

# The facet strips of a ggplotly layout (the centred, bottom-anchored labels it
# puts above each panel), each wrapped to the width of its column, and the room
# their extra lines need: above the top row (`top`) and between rows (`gap`),
# in pixels. ggplotly reserves one line of strip text and lets a longer label
# run across its neighbours. `plot_w` is the plotting area's width in pixels.
fit_facet_strips <- function(layout, plot_w) {
  ann <- layout$annotations
  is_strip <- vapply(ann, function(a) {
    is.null(a$annotationType) && identical(a$xanchor, "center") &&
      identical(a$yanchor, "bottom") && identical(a$yref, "paper")
  }, logical(1))
  if (!any(is_strip)) return(list(layout = layout, top = 0, gap = 0))
  idx <- which(is_strip)
  x <- vapply(ann[idx], function(a) a$x, 0)
  y <- vapply(ann[idx], function(a) a$y, 0)
  cols <- sort(unique(x))
  pitch <- if (length(cols) > 1) min(diff(cols)) * plot_w else plot_w
  extra <- numeric(length(idx))
  for (k in seq_along(idx)) {
    size <- ann[[idx[k]]]$font$size
    txt <- wrap_lines(gsub("<br\\s*/?>", "\n", ann[[idx[k]]]$text),
                      max(1, floor((pitch - 4) / (PLOTLY_CHAR_EM * size))))
    ann[[idx[k]]]$text <- txt
    extra[k] <- (lengths(strsplit(txt, "\n", fixed = TRUE)) - 1) * PLOTLY_LINE_EM * size
  }
  layout$annotations <- ann
  top_row <- y == max(y)
  list(layout = layout, top = max(extra[top_row]),
       gap = if (any(!top_row)) max(extra[!top_row]) else 0)
}

# ggplotly conversion for the expanded view (register_expanded_modal: the large
# modal's 870 px body by a 700 px plot). The tooltip is restricted to the
# dedicated `text` aesthetic when a layer defines one (avoids the duplicated
# x/y lines); plots without one keep plotly's default tooltip. The conversion
# runs at the view's size, so ggplotly's relative units resolve against it, and
# the widget then fills its container. Facet strips are wrapped to their
# columns, and a lower row's extra strip lines are given room by widening the
# panel spacing and converting again (the domains are ggplotly's to lay out).
# The subtitle and caption, wrapped to the plot's width, go under the title,
# which is pinned to the top of the figure; the top margin grows by every line
# they and the top row's strips add, so no text runs into another.
ggplotly_smart <- function(p, width = 870, height = 700) {
  has_text <- "text" %in% names(p$mapping) ||
    any(vapply(p$layers, function(l) "text" %in% names(l$mapping), logical(1)))
  convert <- function(p) {
    fig <- plotly::ggplotly(p, tooltip = if (has_text) "text" else "all",
                            width = width, height = height)
    plot_w <- width - fig$x$layout$margin$l - fig$x$layout$margin$r
    strips <- fit_facet_strips(fig$x$layout, plot_w)
    fig$x$layout <- strips$layout
    list(fig = fig, plot_w = plot_w, top = strips$top, gap = strips$gap)
  }
  res <- convert(p)
  lay <- res$fig$x$layout
  title_px <- lay$title$font$size %||% lay$font$size
  notes <- wrap_lines(plot_notes(p), floor(res$plot_w / (PLOTLY_CHAR_EM * PLOTLY_SUP_SCALE * title_px)))
  top <- lay$margin$t + res$top +
    sum(lengths(strsplit(notes, "\n", fixed = TRUE))) * PLOTLY_LINE_EM * title_px
  if (res$gap > 0) {
    # panel.spacing is converted against the whole figure's height, while the
    # domains it becomes are shares of the plotting area; 72/96 takes px to pt.
    area <- height - top - lay$margin$b
    spacing <- ggplot2::calc_element("panel.spacing.y", ggplot2::complete_theme(p$theme))
    res <- convert(p + ggplot2::theme(panel.spacing.y = spacing +
                                        grid::unit(res$gap * height / area * 72 / 96, "pt")))
  }
  fig <- res$fig
  fig$x$layout[c("width", "height")] <- NULL
  fig[c("width", "height")] <- NULL
  if (top == lay$margin$t) return(fig)
  # The title's first baseline is pinned one line below the top edge. It is
  # anchored by that baseline because plotly.js drops a top anchor's offset
  # once a title runs to several lines. ggplotly's own title markup keeps the
  # plot's title face.
  plotly::layout(fig, title = list(text = paste0(lay$title$text %||% "", plotly_notes_html(notes)),
                                   y = 1 - (8 + title_px) / height, yref = "container",
                                   yanchor = "bottom"),
                 margin = list(t = top))
}

# `agro_params` is one class definition, or list(act =, pre =) for the Actual
# vs Predicted figure, whose panels each keep their own surface's classes.
# `palette` is the displayed variable's palette, as the Map Viewer draws it.
generate_base_plot <- function(item, input, agro_params = NULL, palette = "YlOrRd") {
  req(item)

  if (item$type == "map" || item$type == "map_combined") {
    params_of <- function(surface) {
      if (is.null(agro_params) || !is.null(agro_params$rcl_mat)) agro_params else agro_params[[surface]]
    }

    build_map <- function(obj, label, is_tiled = FALSE, kind = "value", agro_params = params_of("act")) {
      # Point error maps: sample-location errors drawn as markers, mirroring
      # the Map Viewer's Point Residuals panel (never a raster surface).
      if (is.list(obj) && inherits(obj$pts, "sf")) {
        vv <- obj$pts$resid
        abs_max <- suppressWarnings(max(abs(vv), na.rm = TRUE))
        if (is.infinite(abs_max) || is.na(abs_max)) abs_max <- 1
        bp <- ggplot()
        if (!is.null(obj$bound)) {
          bp <- bp + geom_sf(data = sf::st_as_sf(obj$bound), fill = NA,
                             color = "grey35", linewidth = 0.4)
        }
        bp <- bp +
          geom_sf(data = obj$pts, aes(fill = resid),
                  shape = 21, color = "black", size = 3, stroke = 0.3) +
          scale_fill_distiller(palette = resolve_resid_palette(input), direction = 1,
                               limits = c(-abs_max, abs_max), na.value = "grey50",
                               name = item$legend) +
          coord_sf()
        return(bp)
      }
      if (inherits(obj, "PackedSpatRaster")) obj <- terra::unwrap(obj)
      pal_name <- palette %||% "YlOrRd"
      # Registry items carry kind ("value"/"residual"/"uncertainty"); the
      # label grepl is a fallback for items archived before kind existed.
      is_resid <- identical(kind, "residual") ||
        (is.null(kind) && grepl("Residual|Point Error", label))
      is_uncert <- identical(kind, "uncertainty") ||
        (is.null(kind) && grepl("Uncertainty Map", label))
      # Agro/bin class limits are defined on the variable's concentration
      # units: only value surfaces may be classified (mirrors the in-app
      # viewer, which always draws residual/uncertainty layers continuously).
      is_class <- input$color_style %in% c("agro", "bin") && !is_resid && !is_uncert
      if (is_uncert) pal_name <- uncertainty_palette(pal_name)

      if (isTruthy(input$styler_high_contrast)) {
          if (!is_class && !is_resid) pal_name <- "viridis"
      }
      
      # The variable and its unit, as the Map Viewer's legend reads
      # (map_legend_title, stamped on the registry item).
      leg_name <- item$legend
      
      bp <- ggplot() + geom_spatraster(data = obj[[1]])
      
      if (is_resid) {
        vv <- as.vector(terra::values(obj[[1]], na.rm=TRUE))
        abs_max <- max(abs(vv), na.rm = TRUE)
        if(is.infinite(abs_max) || is.na(abs_max)) abs_max <- 1
        resid_pal <- resolve_resid_palette(input)
        bp <- bp + scale_fill_distiller(palette = resid_pal, direction = 1, limits = c(-abs_max, abs_max), na.value = "transparent", name = leg_name) +
          coord_sf()
      } else if (is_class && !is.null(agro_params)) {
        obj_c <- terra::classify(obj[[1]], agro_params$rcl_mat, right = FALSE)
        names(obj_c) <- "category"
        
        labels_to_use <- if(input$color_style == "bin") agro_params$leg_labels else agro_params$labels
        lvls <- data.frame(value = 1:agro_params$n_c, category = labels_to_use)
        levels(obj_c) <- lvls
        
        a_cols <- agro_params$colors
        if (isTruthy(input$styler_high_contrast)) {
            a_cols <- viridis::viridis(agro_params$n_c)
        }
        
        # The legend is the class scheme, as the Map Viewer's is: every class
        # with its swatch (ggplot2 >= 3.5 draws the key of a class absent from
        # the surface only under show.legend = TRUE) and no "NA" entry for the
        # cells outside the boundary, which stay transparent.
        bp <- ggplot() +
          tidyterra::geom_spatraster(data = obj_c, aes(fill = category), show.legend = TRUE) +
          scale_fill_manual(values = a_cols,
                            labels = agro_params$leg_labels,
                            na.translate = FALSE, name = leg_name, drop = FALSE) +
          coord_sf()
      } else {
        is_viridis <- pal_name %in% c("viridis", "cividis")
        if (input$color_style == "bin" && !is_uncert) {
          if(is_viridis) bp <- bp + scale_fill_viridis_b(option = pal_name, na.value = "transparent", n.breaks = 5, name = leg_name)
          else bp <- bp + scale_fill_fermenter(palette = pal_name, direction = 1, na.value = "transparent", n.breaks = 5, name = leg_name)
        } else {
          if(is_viridis) bp <- bp + scale_fill_viridis_c(option = pal_name, na.value = "transparent", name = leg_name)
          else bp <- bp + scale_fill_distiller(palette = pal_name, direction = 1, na.value = "transparent", name = leg_name)
        }
        bp <- bp + coord_sf()
      }
      bp
    }

    # export_item_obj: an uncertainty item stores a derivation of a surface the
    # registry already holds, not a second copy of its values.
    obj <- export_item_obj(item)
    if (item$type == "map") {
      return(build_map(obj, item$label, kind = item$kind))
    } else {
      p1 <- build_map(obj$act, "Actual", is_tiled = TRUE, kind = item$kind, agro_params = params_of("act"))
      p2 <- build_map(obj$pre, "Predicted", is_tiled = TRUE, kind = item$kind, agro_params = params_of("pre"))
      return(list(p1 = p1, p2 = p2))
    }
    
  } else {
    return(item$obj)
  }
}

# Apply the Export Styler's controls to a base plot. Sizes are points and
# margins are millimetres, both physical: the figure therefore looks the same
# whether it is rasterised for the preview or written at 300/600 dpi, provided
# the device's resolution reaches showtext (with_showtext_dpi, global_utils.R).
apply_styler_theme <- function(p_obj, input, item_label = "", item_type = "plot") {
  req(p_obj)

  s_title <- input$styler_title_size %||% 16
  s_base  <- input$styler_base_size %||% 12
  s_x     <- input$styler_x_size %||% 12
  s_y     <- input$styler_y_size %||% 12
  s_lab   <- input$styler_label_size %||% 10
  s_leg   <- input$styler_legend_size %||% 10

  font_f <- input$styler_font_family %||% "sans"
  
  is_combined <- identical(item_type, "map_combined")
  leg_pos <- input$styler_legend_pos %||% (if (is_combined) "bottom" else "right")
  leg_dir <- input$styler_legend_dir %||% (if (is_combined) "horizontal" else "auto")
  if (leg_dir == "auto") {
    leg_dir <- if (leg_pos %in% c("bottom", "top")) "horizontal" else "vertical"
  }
  leg_text_angle <- as.numeric(input$styler_legend_text_angle %||% (if (is_combined) 90 else 0))

  style_pane <- function(p, label, is_combined_pane = FALSE) {
    f_title <- label
    f_x <- if(isTruthy(input$styler_x_title)) input$styler_x_title else NULL
    f_y <- if(isTruthy(input$styler_y_title)) input$styler_y_title else NULL
    
    key_size <- input$styler_legend_key_size %||% 1.0
    margin_t <- input$styler_margin_t %||% 10
    margin_r <- input$styler_margin_r %||% 10
    margin_b <- input$styler_margin_b %||% 10
    margin_l <- input$styler_margin_l %||% 15
    
    if (is_combined_pane) {
      key_size <- key_size * 0.6
      margin_t <- margin_t * 0.3
      margin_r <- margin_r * 0.3
      margin_b <- margin_b * 0.3
      margin_l <- margin_l * 0.3
    }
    
    legend_theme <- if (is_combined_pane) {
      list(
        legend.key.size = unit(key_size, "cm"),
        legend.key.width = unit(key_size * 2.5, "cm"),
        legend.key.height = unit(key_size * 0.5, "cm")
      )
    } else {
      list(legend.key.size = unit(key_size, "cm"))
    }
    
    p + theme_minimal(base_size = s_base, base_family = font_f) +
      theme(
        plot.title = element_text(size = if(is_combined_pane) s_title * 0.85 else s_title, face = "bold"),
        plot.subtitle = element_text(size = s_title * 0.8),
        axis.title.x = element_text(size = s_x),
        axis.title.y = element_text(size = s_y),
        axis.text = element_text(size = s_lab),
        legend.text = element_text(
          size = if(is_combined_pane) s_leg * 0.85 else s_leg, 
          angle = leg_text_angle,
          hjust = if(leg_text_angle != 0) 0.5 else NULL,
          vjust = if(leg_text_angle != 0) 0.5 else NULL
        ),
        legend.title = element_text(size = if(is_combined_pane) s_leg * 0.85 else s_leg, face = "bold"),
        legend.position = leg_pos,
        legend.direction = leg_dir,
        panel.grid.major = if(isTRUE(input$styler_show_grid)) element_line(color = "grey90") else element_blank(),
        panel.grid.minor = if(isTRUE(input$styler_show_grid)) element_line(color = "grey95") else element_blank(),
        plot.margin = ggplot2::margin(margin_t, margin_r, margin_b, margin_l, unit = "mm"),
        axis.text.x = element_text(
          angle = as.numeric(input$styler_label_orient %||% 0),
          hjust = if(as.numeric(input$styler_label_orient %||% 0) != 0) 1 else 0.5,
          vjust = if(as.numeric(input$styler_label_orient %||% 0) != 0) 1 else 0.5
        )
      ) +
      do.call(theme, legend_theme) +
      labs(title = f_title, x = f_x, y = f_y)
  }

  if (item_type == "map_combined") {
    p1_s <- style_pane(p_obj$p1, "Actual", is_combined_pane = TRUE)
    p2_s <- style_pane(p_obj$p2, "Predicted", is_combined_pane = TRUE)
    
    main_t <- if(isTruthy(input$styler_title)) input$styler_title else item_label
    comb_key_size <- (input$styler_legend_key_size %||% 1.0) * 0.6
    
    return(p1_s + p2_s + plot_layout(ncol = 2, guides = "collect") & 
           theme(legend.position = leg_pos, 
                 legend.direction = leg_dir,
                 legend.key.size = unit(comb_key_size, "cm"),
                 legend.key.width = unit(comb_key_size * 2.5, "cm"),
                 legend.key.height = unit(comb_key_size * 0.5, "cm"),
                 legend.margin = ggplot2::margin(2, 2, 2, 2),
                 legend.box.margin = ggplot2::margin(0, 0, 0, 0),
                 legend.text = element_text(
                   size = s_leg * 0.85,
                   angle = leg_text_angle,
                   hjust = if(leg_text_angle != 0) 0.5 else NULL,
                   vjust = if(leg_text_angle != 0) 0.5 else NULL
                 )) & 
           plot_annotation(title = main_t, theme = theme(plot.title = element_text(size = s_title, face = "bold", family = font_f))))
           
  } else {
    t <- if(isTruthy(input$styler_title)) input$styler_title else item_label
    return(style_pane(p_obj, t))
  }
}

generate_styled_plot <- function(item, input, agro_params = NULL, palette = "YlOrRd") {
  base_p <- generate_base_plot(item, input, agro_params, palette)
  apply_styler_theme(base_p, input, item_label = item$label, item_type = item$type)
}

get_stat_letters <- function(df, var_name, group_col, test_type) {
  # "" is the control's None choice. It must return before the if-chain below,
  # because the ANOVA branch also fires on `n_groups == 2` regardless of
  # test_type - so without this guard, "None" still annotated a two-group plot.
  if (is.null(test_type) || length(test_type) != 1 || is.na(test_type) || !nzchar(test_type)) return(NULL)
  df_proc <- df[!is.na(df[[var_name]]) & !is.na(df[[group_col]]), ]
  if (nrow(df_proc) < 3) return(NULL)
  
  df_proc[[group_col]] <- as.factor(as.character(df_proc[[group_col]]))
  n_groups <- length(levels(df_proc[[group_col]]))
  if (n_groups < 2) return(NULL)
  if (nrow(df_proc) <= n_groups) return(NULL)
  
  tryCatch({
    formula_str <- paste0("`", var_name, "` ~ `", group_col, "`")
    aov_res <- aov(as.formula(formula_str), data = df_proc)
    
    if (test_type == "tukey" && n_groups > 2 && requireNamespace("agricolae", quietly = TRUE)) {
      res <- agricolae::HSD.test(aov_res, group_col, console = FALSE)
      df_let <- data.frame(group = rownames(res$groups), letter = as.character(res$groups$groups))
      colnames(df_let)[1] <- group_col
      return(df_let)
    } else if (test_type == "duncan" && n_groups > 2 && requireNamespace("agricolae", quietly = TRUE)) {
      # Duncan's MRT controls the COMPARISON-wise error rate only; its
      # family-wise rate grows toward 1 with k. Offered because older agronomy
      # literature reports it, labelled "(liberal)" in the control, and flagged
      # in scientific_guide's post-hoc section. Tukey's HSD is the conservative
      # default - do not present the two as interchangeable.
      res <- agricolae::duncan.test(aov_res, group_col, console = FALSE)
      df_let <- data.frame(group = rownames(res$groups), letter = as.character(res$groups$groups))
      colnames(df_let)[1] <- group_col
      return(df_let)
    } else if (test_type == "kruskal" && requireNamespace("agricolae", quietly = TRUE)) {
      # BH-adjusted pairwise comparisons, consistent with the correlation
      # table's BH policy.
      res <- agricolae::kruskal(df_proc[[var_name]], df_proc[[group_col]], p.adj = "BH", console = FALSE)
      df_let <- data.frame(group = rownames(res$groups), letter = as.character(res$groups$groups))
      colnames(df_let)[1] <- group_col
      return(df_let)
    } else if (test_type == "anova" || n_groups == 2) {
      s_aov <- summary(aov_res)
      f_val <- s_aov[[1]][["F value"]][1]
      p_val <- s_aov[[1]][["Pr(>F)"]][1]
      df1 <- s_aov[[1]][["Df"]][1]
      df2 <- s_aov[[1]][["Df"]][2]
      
      p_label <- if(is.null(p_val) || is.na(p_val)) "p = N/A" else if(p_val < 0.001) "p < 0.001" else paste0("p = ", signif(p_val, 3))
      f_label <- if(is.null(f_val) || is.na(f_val)) "F = N/A" else paste0("F(", df1, ",", df2, ") = ", round(f_val, 2))
      full_label <- paste0("ANOVA: ", f_label, ", ", p_label)
      
      df_let <- data.frame(group = levels(df_proc[[group_col]]), letter = as.character(full_label))
      colnames(df_let)[1] <- group_col
      return(df_let)
    }
  }, error = function(e) { return(NULL) })
  return(NULL)
}

add_stat_layer <- function(p, df, var_name, group_col, stat_test, stat_letter_pos, facet_var = NULL) {
   # NULL/empty = no control rendered; "" = the control's None choice.
   if (is.null(stat_test) || length(stat_test) == 0) return(p)
   if (is.na(stat_test[1]) || !nzchar(stat_test[1])) return(p)
   if (!group_col %in% colnames(df)) return(p)
   
   if (!is.null(facet_var)) {
       vars_to_test <- unique(as.character(df[[facet_var]]))
       all_letters <- data.frame()
       for (v in vars_to_test) {
           sub_df <- df[df[[facet_var]] == v, ]
           l_df <- get_stat_letters(sub_df, "Value", group_col, stat_test[1])
           if (!is.null(l_df)) {
               if (stat_test[1] == "anova") {
                   l_df <- l_df[1, , drop=FALSE]
                   max_y <- max(sub_df$Value, na.rm = TRUE)
                   l_df$y_pos <- max_y + (max_y - min(sub_df$Value, na.rm=TRUE)) * 0.15
               } else if (stat_letter_pos == "top") {
                   max_y <- max(sub_df$Value, na.rm = TRUE)
                   l_df$y_pos <- max_y + (max_y - min(sub_df$Value, na.rm=TRUE)) * 0.1
               } else {
                   form <- as.formula(paste0("Value ~ `", group_col, "`"))
                   agg_df <- aggregate(form, data = sub_df, max, na.rm = TRUE)
                   colnames(agg_df) <- c(group_col, "y_pos")
                   l_df <- merge(l_df, agg_df, by = group_col)
               }
               l_df[[facet_var]] <- v
               all_letters <- rbind(all_letters, l_df)
           }
       }
       if (nrow(all_letters) > 0) {
           if (stat_test[1] == "anova") {
              p <- p + geom_text(data = all_letters, aes(x = -Inf, y = y_pos, label = letter), hjust = -0.1, vjust = 1, size = 3.5, fontface = "italic", inherit.aes = FALSE)
           } else {
              p <- p + geom_text(data = all_letters, aes(x = .data[[group_col]], y = y_pos, label = letter), vjust = -0.5, size = 4, fontface = "bold", inherit.aes = FALSE)
              # vjust = -0.5 draws the glyph above y_pos; the default 5% scale
              # expansion is not always enough headroom, cutting letters in
              # half at the panel top, so widen the upper expansion
              p <- p + scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))
           }
       }
   } else {
       l_df <- get_stat_letters(df, var_name, group_col, stat_test[1])
       if (!is.null(l_df)) {
           if (stat_test[1] == "anova") {
               l_df <- l_df[1, , drop=FALSE]
               max_y <- max(df[[var_name]], na.rm = TRUE)
               y_pos <- max_y + (max_y - min(df[[var_name]], na.rm=TRUE)) * 0.15
               p <- p + annotate("text", x = -Inf, y = y_pos, label = l_df$letter[1], hjust = -0.1, vjust = 1, size = 4, fontface = "italic")
           } else {
               if (stat_letter_pos == "top") {
                   max_y <- max(df[[var_name]], na.rm = TRUE)
                   y_pos <- max_y + (max_y - min(df[[var_name]], na.rm=TRUE)) * 0.1
                   l_df$y_pos <- y_pos
               } else {
                   agg_df <- aggregate(df[[var_name]] ~ df[[group_col]], FUN=max, na.rm=TRUE)
                   colnames(agg_df) <- c(group_col, "y_pos")
                   l_df <- merge(l_df, agg_df, by = group_col)
               }
               p <- p + geom_text(data = l_df, aes(x = .data[[group_col]], y = y_pos, label = letter), vjust = -0.5, size = 4, fontface = "bold", inherit.aes = FALSE)
               # Same headroom fix as the faceted branch: keep the letters
               # from being clipped at the panel top
               p <- p + scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))
           }
       }
   }
   return(p)
}



# Categorical group axes (boxplot/violin/sina): when many groups or long
# interaction labels ("A | B | C") would crowd the x axis, drop the axis
# text entirely, since group identity is already colour-coded in the legend
hide_x_labels_if_crowded <- function(p, df, group_col) {
  if (is.null(group_col) || !group_col %in% colnames(df)) return(p)
  lv <- unique(as.character(df[[group_col]]))
  lv <- lv[!is.na(lv)]
  if (length(lv) >= 6 || (length(lv) > 1 && max(nchar(lv)) > 12)) {
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
                   axis.title.x = element_blank())
  }
  return(p)
}

# Variable-name x axes (parallel coordinates): the names are NOT in the
# legend, so rotate rather than hide when crowded
rotate_x_labels_if_crowded <- function(p, df, group_col, labels = NULL) {
  if (is.null(group_col) || !group_col %in% colnames(df)) return(p)
  lv <- unique(display_var_labels(as.character(df[[group_col]]), labels))
  lv <- lv[!is.na(lv)]
  if (length(lv) >= 6 || (length(lv) > 1 && max(nchar(lv)) > 12)) {
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  }
  return(p)
}

generate_core_plot <- function(df, var_name, y_var = NULL, group_col = NULL, plot_type = "histogram", scatter_fit = "none", stat_test = NULL, stat_letter_pos = "above", labels = NULL, group_label = "Group") {


  
  if (is.null(group_col) || !group_col %in% colnames(df)) {
    df$group_id <- factor("All")
    group_col <- "group_id"
  }
  
  df$index_seq <- seq_len(nrow(df))
  
  if (plot_type %in% c("boxplot", "violin") && !is.null(y_var) && y_var != "") {

    df_long <- pivot_longer(df, cols = c(all_of(var_name), all_of(y_var)), names_to = "Variable", values_to = "Value")
    
    if (plot_type == "boxplot") {
      p <- ggplot(df_long, aes(x = .data[[group_col]], y = Value, fill = .data[[group_col]])) + geom_boxplot()
    } else {
      p <- ggplot(df_long, aes(x = .data[[group_col]], y = Value, fill = .data[[group_col]])) + geom_violin(alpha = 0.7)
    }
    p <- p + facet_wrap(~Variable, scales = "free_y", labeller = as_labeller(function(v) display_var_labels(v, labels))) + theme_minimal() +
         labs(title = paste(tools::toTitleCase(plot_type), "Comparison"), y = "Value")
    p <- add_stat_layer(p, df_long, "Value", group_col, stat_test, stat_letter_pos, facet_var = "Variable")
    p <- hide_x_labels_if_crowded(p, df_long, group_col)
  } else {
    p <- ggplot(df) + theme_minimal()
    
    if (plot_type == "histogram") {
      p <- p + geom_histogram(aes(x = .data[[var_name]], fill = .data[[group_col]]), alpha = 0.7, position = "identity", bins = 30)
    } else if (plot_type == "density") {
      p <- p + geom_density(aes(x = .data[[var_name]], fill = .data[[group_col]], color = .data[[group_col]]), alpha = 0.5)
    } else if (plot_type == "boxplot") {
      p <- p + geom_boxplot(aes(x = .data[[group_col]], y = .data[[var_name]], fill = .data[[group_col]]))
    } else if (plot_type == "violin") {
      p <- p + geom_violin(aes(x = .data[[group_col]], y = .data[[var_name]], fill = .data[[group_col]]), alpha = 0.7)
    } else if (plot_type == "scatter") {
      if (!is.null(y_var) && y_var %in% colnames(df) && y_var != "") {
        p <- p + geom_point(aes(x = .data[[var_name]], y = .data[[y_var]], color = .data[[group_col]]), alpha = 0.8)
      } else {
        p <- p + geom_point(aes(x = .data[["index_seq"]], y = .data[[var_name]], color = .data[[group_col]]), alpha = 0.8)
      }
      
      if (!is.null(scatter_fit) && scatter_fit != "none") {
         fit_methods <- list(
           linear = list(method = "lm", se = FALSE),
           loess = list(method = "loess", se = FALSE),
           polynomial = list(method = "lm", formula = y ~ poly(x, 2), se = FALSE),
           gam = list(method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE)
         )
         fit_params <- fit_methods[[scatter_fit]]
         if (!is.null(fit_params)) {
           x_col <- if (!is.null(y_var) && y_var != "") var_name else "index_seq"
           y_col <- if (!is.null(y_var) && y_var != "") y_var else var_name
           fit_params$mapping <- aes(x = .data[[x_col]], y = .data[[y_col]], color = .data[[group_col]])
           p <- p + do.call(geom_smooth, fit_params)
         }
      }
      
    } else if (plot_type == "ecdf") {
      p <- p + stat_ecdf(aes(x = .data[[var_name]], color = .data[[group_col]]), geom = "step", linewidth = 1)
    }
    
    if (plot_type %in% c("boxplot", "violin")) {
      p <- add_stat_layer(p, df, var_name, group_col, stat_test, stat_letter_pos)
      p <- hide_x_labels_if_crowded(p, df, group_col)
    }
    
    p <- p + labs(title = paste(tools::toTitleCase(plot_type), "of", display_var_labels(var_name, labels)))
  }
  # Grouping and axis labels for every branch, from the selected context.
  paired <- is_valid_col_ref(y_var)
  p <- p + labs(fill = group_label, color = group_label,
    x = if (plot_type %in% c("boxplot", "violin")) group_label else if (plot_type == "scatter" && !paired) "Observation index" else display_var_labels(var_name, labels))
  if (plot_type == "scatter") p <- p + labs(y = display_var_labels(if (paired) y_var else var_name, labels))
  if (plot_type %in% c("boxplot", "violin") && !paired) p <- p + labs(y = display_var_labels(var_name, labels))
  return(p)
}

generate_ghosted_plot <- function(df_global, df_local, var_name, y_var = NULL, group_col = NULL, plot_type = "histogram", labels = NULL, group_label = "Group") {

  
  if (is.null(group_col) || !group_col %in% colnames(df_local)) {
    df_local$group_id <- factor("All")
    df_global$group_id <- factor("All")
    group_col <- "group_id"
  }
  
  p <- ggplot() + theme_minimal()
  
  if (plot_type == "histogram") {
    p <- p + geom_histogram(data = df_global, aes(x = .data[[var_name]]), fill = "lightgray", alpha = 0.5, bins = 30) +
             geom_histogram(data = df_local, aes(x = .data[[var_name]], fill = .data[[group_col]]), alpha = 0.7, position = "identity", bins = 30)
  } else if (plot_type == "density") {
    p <- p + geom_density(data = df_global, aes(x = .data[[var_name]]), fill = "lightgray", color = "gray", alpha = 0.3) +
             geom_density(data = df_local, aes(x = .data[[var_name]], fill = .data[[group_col]], color = .data[[group_col]]), alpha = 0.5)
  } else if (plot_type == "boxplot") {
    p <- p + geom_boxplot(data = df_global, aes(x = "Global", y = .data[[var_name]]), fill = "lightgray", color = "gray") +
             geom_boxplot(data = df_local, aes(x = .data[[group_col]], y = .data[[var_name]], fill = .data[[group_col]]))
  } else if (plot_type == "violin") {
    p <- p + geom_violin(data = df_global, aes(x = "Global", y = .data[[var_name]]), fill = "lightgray", color = "gray") +
             geom_violin(data = df_local, aes(x = .data[[group_col]], y = .data[[var_name]], fill = .data[[group_col]]), alpha = 0.7)
  } else if (plot_type == "scatter") {
    if (!is.null(y_var) && y_var %in% colnames(df_local)) {
      p <- p + geom_point(data = df_global, aes(x = .data[[var_name]], y = .data[[y_var]]), color = "lightgray", alpha = 0.3) +
               geom_point(data = df_local, aes(x = .data[[var_name]], y = .data[[y_var]], color = .data[[group_col]]), alpha = 0.8)
    } else {
      df_global$index_seq <- seq_len(nrow(df_global))
      df_local$index_seq <- seq_len(nrow(df_local))
      p <- p + geom_point(data = df_global, aes(x = .data[["index_seq"]], y = .data[[var_name]]), color = "lightgray", alpha = 0.3) +
               geom_point(data = df_local, aes(x = .data[["index_seq"]], y = .data[[var_name]], color = .data[[group_col]]), alpha = 0.8)
    }
  } else if (plot_type == "ecdf") {
    p <- p + stat_ecdf(data = df_global, aes(x = .data[[var_name]]), geom = "step", color = "lightgray", linewidth = 1) +
             stat_ecdf(data = df_local, aes(x = .data[[var_name]], color = .data[[group_col]]), geom = "step", linewidth = 1)
  }
  
  paired <- is_valid_col_ref(y_var)
  p <- p + labs(title = paste("Ghosted", tools::toTitleCase(plot_type), "of", display_var_labels(var_name, labels)),
    x = if (plot_type %in% c("boxplot", "violin")) group_label else if (plot_type == "scatter" && !paired) "Observation index" else display_var_labels(var_name, labels),
    fill = group_label, color = group_label)
  if (plot_type %in% c("boxplot", "violin", "scatter")) p <- p + labs(
    y = display_var_labels(if (plot_type == "scatter" && paired) y_var else var_name, labels))

  if (plot_type %in% c("boxplot", "violin")) {
    p <- hide_x_labels_if_crowded(p, df_local, group_col)
  }

  return(p)
}

generate_advanced_plot <- function(df, vars, group_col = NULL, plot_type = "qq", xyz_fit = "linear", stat_test = NULL, stat_letter_pos = "above", labels = NULL, group_label = "Group") {


  
  if (is.null(group_col) || !group_col %in% colnames(df)) {
    df$group_id <- factor("All")
    group_col <- "group_id"
  }
  
  p <- ggplot()
  v1 <- if(length(vars) > 0 && isTruthy(vars[1]) && vars[1] != "") vars[1] else NULL
  v2 <- if(length(vars) > 1 && isTruthy(vars[2]) && vars[2] != "") vars[2] else NULL
  v3 <- if(length(vars) > 2 && isTruthy(vars[3]) && vars[3] != "") vars[3] else NULL
  unavailable <- character(0)
  if (plot_type %in% c("parallel", "radar")) {
    unavailable <- vars[!vapply(df[, vars, drop = FALSE], function(v) any(is.finite(v)), logical(1))]
    minimum <- if (plot_type == "radar") 3L else 2L
    if (length(vars) - length(unavailable) < minimum) {
      return(sci_placeholder(paste0(
        if (plot_type == "radar") "Radar" else "Parallel coordinates", " requires at least ", minimum,
        " variables with observed values.",
        if (length(unavailable)) paste0("\nNo observed values: ", paste(display_var_labels(unavailable, labels), collapse = ", ")))))
    }
  }
  normalize <- function(values, reference) {
    observed <- reference[is.finite(reference)]
    out <- rep(NA_real_, length(values))
    if (!length(observed)) return(out)
    rng <- range(observed)
    ok <- is.finite(values)
    out[ok] <- if (diff(rng) > 0) (values[ok] - rng[1]) / diff(rng) else 0
    out
  }
  
  if (plot_type == "qq") {
    p <- ggplot(df, aes(sample = .data[[v1]], color = .data[[group_col]])) + 
         stat_qq() + stat_qq_line() + labs(x="Theoretical", y="Sample", title="QQ Plot")
  } else if (plot_type == "sinaplot") {
    if (!is.null(v2)) {
       df_long <- tidyr::pivot_longer(df, cols = c(all_of(v1), all_of(v2)), names_to = "Variable", values_to = "Value")
       p <- ggplot(df_long, aes(x = .data[[group_col]], y = Value, fill = .data[[group_col]])) + 
            geom_violin(alpha=0.5, color=NA) + 
            geom_jitter(aes(color = .data[[group_col]]), width = 0.2, alpha=0.7) +
            facet_wrap(~Variable, scales="free_y", labeller = as_labeller(function(v) display_var_labels(v, labels))) +
            labs(title="Sina-style Plot Comparison")
       
       p <- add_stat_layer(p, df_long, "Value", group_col, stat_test, stat_letter_pos, facet_var = "Variable")
       p <- hide_x_labels_if_crowded(p, df_long, group_col)
    } else {
       p <- ggplot(df, aes(x = .data[[group_col]], y = .data[[v1]], fill = .data[[group_col]])) +
            geom_violin(alpha=0.5, color=NA) +
            geom_jitter(aes(color = .data[[group_col]]), width = 0.2, alpha=0.7) +
            labs(title="Sina-style Plot")

       p <- add_stat_layer(p, df, v1, group_col, stat_test, stat_letter_pos)
       p <- hide_x_labels_if_crowded(p, df, group_col)
    }
  } else if (plot_type == "ridge") {
    p <- ggplot(df, aes(x = .data[[v1]], fill = .data[[group_col]])) + 
         geom_density(alpha = 0.6) + 
         facet_grid(as.formula(paste(group_col, "~ ."))) +
         labs(title="Ridge/Joyplot Proxy")
  } else if (plot_type == "density_heatmap") {
    if (!is.null(v2)) {
      p <- ggplot(df, aes(x = .data[[v1]], y = .data[[v2]])) + 
           geom_density_2d_filled(alpha = 0.9) +
           labs(title="2D Density Heatmap")
    } else {
      p <- sci_placeholder("Density Heatmap requires two numeric variables")
    }
  } else if (plot_type == "parallel") {
    # At least two variables with observed values (checked above).
    df_sub <- df[, c(vars, group_col), drop=FALSE]
    long_df <- data.frame(id = integer(), variable = character(), value = numeric(), group = character())
    for(v in vars) {
      if(is.numeric(df_sub[[v]])) {
        norm_val <- normalize(df_sub[[v]], df_sub[[v]])
        long_df <- rbind(long_df, data.frame(id=seq_len(nrow(df_sub)), variable=v, value=norm_val, group=as.character(df_sub[[group_col]])))
      }
    }
    p <- ggplot(long_df, aes(x=variable, y=value, group=id, color=group)) +
         geom_line(alpha=0.4) + labs(title="Parallel Coordinates")
    p <- rotate_x_labels_if_crowded(p, long_df, "variable", labels = labels)
  } else if (plot_type == "radar") {
    # At least three variables with observed values (checked above).
    agg <- aggregate(df[, vars, drop=FALSE], by=list(group=df[[group_col]]), FUN=mean, na.rm=TRUE)
    long_df <- data.frame(group = character(), variable = character(), value = numeric())
    for(v in vars) {
      norm_val <- normalize(agg[[v]], df[[v]])
      long_df <- rbind(long_df, data.frame(group=agg$group, variable=v, value=norm_val))
    }
    complete_groups <- names(which(vapply(split(long_df$value, long_df$group), function(v) all(is.finite(v)), logical(1))))
    p <- ggplot(long_df, aes(x=variable, y=value, group=group, color=group, fill=group)) +
         geom_polygon(data = long_df[long_df$group %in% complete_groups, ], alpha=0.2) + geom_point(na.rm = TRUE) + coord_polar(clip = "off") +
         labs(title="Radar Chart (Normalized Means)")
  } else if (plot_type == "xyz_surface") {
    if (!is.null(v1) && !is.null(v2) && !is.null(v3)) {
      df_clean <- na.omit(df[, c(v1, v2, v3)])
      if(nrow(df_clean) > 10) {
        df_safe <- df_clean
        colnames(df_safe) <- c("var1_safe", "var2_safe", "var3_safe")
        
        x_seq <- seq(min(df_safe[["var1_safe"]]), max(df_safe[["var1_safe"]]), length.out=50)
        y_seq <- seq(min(df_safe[["var2_safe"]]), max(df_safe[["var2_safe"]]), length.out=50)
        grid <- expand.grid(var1_safe=x_seq, var2_safe=y_seq)
        
        mod <- NULL
        try({
          if(xyz_fit == "linear") mod <- lm(var3_safe ~ var1_safe + var2_safe, data=df_safe)
          else if(xyz_fit == "loess") mod <- loess(var3_safe ~ var1_safe * var2_safe, data=df_safe, span=0.7)
          else if(xyz_fit == "polynomial") mod <- lm(var3_safe ~ poly(var1_safe,2) + poly(var2_safe,2), data=df_safe)
          else if(xyz_fit == "gam") {
            if(requireNamespace("mgcv", quietly=TRUE)) {
              mod <- mgcv::gam(var3_safe ~ s(var1_safe) + s(var2_safe), data=df_safe)
            } else {
              mod <- lm(var3_safe ~ var1_safe + var2_safe, data=df_safe)
            }
          } else if(xyz_fit == "tps") {
            if(requireNamespace("fields", quietly=TRUE)) {
              # give.warnings = FALSE: fields' console note on a GCV minimum
              # at an end of its grid reaches no one in the app.
              mod <- fields::Tps(df_safe[,c("var1_safe","var2_safe")], df_safe[["var3_safe"]],
                                 give.warnings = FALSE)
            }
          }
        }, silent=TRUE)
        
        if(!is.null(mod)) {
          if(xyz_fit == "tps") {
            grid[["var3_safe"]] <- as.vector(predict(mod, x=as.matrix(grid[,c("var1_safe","var2_safe")])))
          } else {
            grid[["var3_safe"]] <- as.vector(predict(mod, newdata=grid))
          }
          
          colnames(grid) <- c(v1, v2, v3)
          
          p <- ggplot(grid, aes(x=.data[[v1]], y=.data[[v2]], z=.data[[v3]])) +
               geom_tile(aes(fill=.data[[v3]])) +
               geom_contour(color="white", alpha=0.5) +
               scale_fill_viridis_c() +
               labs(title=paste("XYZ Surface (", xyz_fit, ")", sep=""))
        } else {
          p <- sci_placeholder("Model fitting failed")
        }
      } else {
        p <- sci_placeholder("Not enough data for surface")
      }
    } else {
      p <- sci_placeholder("XYZ Surface requires 3 numeric variables.\nSelect the Primary (X), Secondary (Y) and Tertiary (Z) variables.")
    }
  }
  
  p <- p + (theme_minimal() + p$theme)
  if (plot_type %in% c("qq", "sinaplot", "parallel", "radar")) p <- p + labs(color = group_label)
  if (plot_type %in% c("sinaplot", "ridge", "radar")) p <- p + labs(fill = group_label)
  if (plot_type %in% c("parallel", "radar")) {
    p <- p + scale_x_discrete(labels = function(v) {
      text <- display_var_labels(v, labels)
      if (plot_type == "radar") wrap_lines(text, 16) else text
    }) + labs(x = "Variable", y = "Normalized value")
    if (length(unavailable)) p <- p + labs(caption = paste("No observed values:", paste(display_var_labels(unavailable, labels), collapse = ", ")))
    if (plot_type == "radar" && anyNA(p$data$value)) p <- p + labs(caption = paste(
      c(p$labels$caption, "Groups with missing dimensions show available points only."), collapse = "\n"))
  } else if (plot_type == "sinaplot") {
    p <- p + labs(x = group_label, y = if (is.null(v2)) display_var_labels(v1, labels) else "Value")
  } else if (plot_type %in% c("ridge", "density_heatmap", "xyz_surface")) {
    p <- p + labs(x = display_var_labels(v1, labels))
    if (!is.null(v2)) p <- p + labs(y = display_var_labels(v2, labels))
    if (plot_type == "xyz_surface") p <- p + labs(fill = display_var_labels(v3, labels))
    if (plot_type == "density_heatmap") p <- p + labs(fill = "Density level")
  }
  return(p)
}


#' Align a caller-supplied correlation matrix to the plotted variable set.
#' Both correlation panels index the matrix POSITIONALLY (`cormat[i, j]` against
#' `vars[i]`, plus `vars[hc$order]`), which is only correct while the caller
#' happens to build it over the same variables in the same order. Subset by name
#' so a superset or a re-ordered matrix cannot mislabel a cell; a matrix with no
#' usable dimnames is left alone (positional is then the only contract there is).
align_cormat <- function(cormat, vars) {
  if (is.null(cormat) || is.null(rownames(cormat)) || is.null(colnames(cormat))) return(cormat)
  if (!all(vars %in% rownames(cormat)) || !all(vars %in% colnames(cormat))) return(cormat)
  cormat[vars, vars, drop = FALSE]
}

constant_var_plot <- function() {
  sci_placeholder("A selected variable is constant;\ncorrelation is undefined for it.")
}

generate_correlation_heatmap <- function(df, vars, method = "pearson", cormat = NULL, labels = NULL) {

  if (length(vars) < 2) return(sci_placeholder("Need >=2 variables"))
  
  df_clean <- na.omit(df[, vars, drop=FALSE])
  if (nrow(df_clean) < 3) return(sci_placeholder("Insufficient data"))
  
  if (is.null(cormat)) {
    cormat <- cor(df_clean, method = method)
  }
  cormat <- align_cormat(cormat, vars)
  # A constant variable makes cor() return an NA row and column; as.dist() then
  # hands those NAs to hclust, which errors out ("NA/NaN/Inf in foreign function
  # call") and the panel goes blank with no explanation.
  if (anyNA(cormat)) return(constant_var_plot())

  distmat <- as.dist(1 - abs(cormat))
  hc <- hclust(distmat)
  ordered_vars <- vars[hc$order]
  
  cormat_df <- melt_cormat(cormat, "Corr")
  
  cormat_df$Var1 <- factor(cormat_df$Var1, levels = ordered_vars)
  cormat_df$Var2 <- factor(cormat_df$Var2, levels = rev(ordered_vars))
  
  p <- ggplot(cormat_df, aes(x=Var1, y=Var2, fill=Corr)) + 
    geom_tile(color = "white") +
    geom_text(aes(label = round(Corr, 2)), color = ifelse(abs(cormat_df$Corr) > 0.5, "white", "black"), size=3) +
    scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0, limits = c(-1,1), name=paste(tools::toTitleCase(method), "\nCorrelation")) +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(x="", y="", title="Hierarchical Clustering Correlation Heatmap") +
    scale_x_discrete(labels = function(v) display_var_labels(v, labels)) +
    scale_y_discrete(labels = function(v) display_var_labels(v, labels))
  return(p)
}

generate_correlation_network <- function(df, vars, threshold = 0.3, method = "pearson", cormat = NULL, labels = NULL) {

  if (length(vars) < 2) return(sci_placeholder("Need >=2 variables"))
  
  df_clean <- na.omit(df[, vars, drop=FALSE])
  if (nrow(df_clean) < 3) return(sci_placeholder("Insufficient data"))
  
  if (is.null(cormat)) {
    cormat <- cor(df_clean, method = method)
  }
  cormat <- align_cormat(cormat, vars)
  # An NA correlation (constant variable) would reach `if (abs(w) >= threshold)`
  # as a missing value and abort the plot with "missing value where TRUE/FALSE
  # needed"; name the cause instead.
  if (anyNA(cormat)) return(constant_var_plot())

  n <- length(vars)
  angles <- seq(0, 2*pi, length.out = n + 1)[1:n]
  nodes <- data.frame(
    name = vars, label = display_var_labels(vars, labels),
    x = cos(angles),
    y = sin(angles)
  )
  
  edges <- data.frame(from=character(), to=character(), x=numeric(), y=numeric(), xend=numeric(), yend=numeric(), weight=numeric(), sign=character())
  
  for(i in 1:(n-1)) {
    for(j in (i+1):n) {
      w <- cormat[i, j]
      if (abs(w) >= threshold) {
        edges <- rbind(edges, data.frame(
          from = vars[i], to = vars[j],
          x = nodes$x[i], y = nodes$y[i],
          xend = nodes$x[j], yend = nodes$y[j],
          weight = abs(w),
          sign = ifelse(w > 0, "Positive", "Negative")
        ))
      }
    }
  }
  
  p <- ggplot() + theme_void() + labs(title = paste0("Correlation Network (|r| ≥ ", threshold, ")"))
  
  if (nrow(edges) > 0) {
    p <- p + geom_segment(data = edges, aes(x=x, y=y, xend=xend, yend=yend, color=sign, linewidth=weight), alpha=0.6) +
             scale_linewidth_continuous(range = c(0.5, 3)) +
             scale_color_manual(values = c("Positive" = "blue", "Negative" = "red"))
  }
  
  p <- p + geom_point(data = nodes, aes(x=x, y=y), size=10, color="lightblue") +
           geom_text(data = nodes, aes(x=x, y=y, label=label), fontface="bold")
           
  return(p)
}

generate_partial_correlation <- function(df, vars, control_vars = NULL, method = "pearson", labels = NULL) {

  if (length(vars) < 2) return(sci_placeholder("Need >=2 variables to correlate"))

  # Residualization + the rank/kendall conventions live in
  # compute_partial_correlation() so this plot and the correlation summary
  # table can never disagree about what "partial" means.
  pc <- compute_partial_correlation(df, vars, control_vars, method = method)
  refusal <- partial_correlation_refusal(pc, labels)
  if (!is.null(refusal)) return(sci_placeholder(wrap_lines(refusal, 70)))
  if (is.null(pc$cormat) || pc$n < 5) return(sci_placeholder("Insufficient data"))

  cormat <- pc$cormat
  n_ctrl <- pc$k

  cormat_df <- melt_cormat(cormat, "pCorr")
  
  cormat_df$Var1 <- factor(cormat_df$Var1, levels = vars)
  cormat_df$Var2 <- factor(cormat_df$Var2, levels = rev(vars))
  
  p <- ggplot(cormat_df, aes(x=Var1, y=Var2, fill=pCorr)) + 
    geom_tile(color = "white") +
    geom_text(aes(label = round(pCorr, 2)), color = ifelse(abs(cormat_df$pCorr) > 0.5, "white", "black"), size=3) +
    scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0, limits = c(-1,1),
                         name = if (n_ctrl == 0) "Correlation" else "Partial\nCorrelation") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(x="", y="",
         title = if (n_ctrl == 0) "Standard Correlation Heatmap" else paste0("Partial Correlation (control df = ", n_ctrl, ")"),
         subtitle = if (n_ctrl == 0) NULL else switch(method,
           spearman = "Spearman: ranks residualized on the controls (ppcor convention)",
           kendall  = "Kendall: partial tau from the inverted tau matrix (ppcor convention)",
           NULL)) +
    scale_x_discrete(labels = function(v) display_var_labels(v, labels)) +
    scale_y_discrete(labels = function(v) display_var_labels(v, labels))
  return(p)
}

generate_correlogram <- function(df, vars, method = "pearson", cormat = NULL, labels = NULL) {

  if (length(vars) < 2) return(sci_placeholder("Need >=2 variables"))
  
  df_clean <- na.omit(df[, vars, drop=FALSE])
  if (nrow(df_clean) < 3) return(sci_placeholder("Insufficient data"))
  
  if (is.null(cormat)) {
    cormat <- cor(df_clean, method = method)
  }
  cormat <- align_cormat(cormat, vars)
  # Same guard as the heatmap and network: a constant variable's NA cells
  # would otherwise vanish from the plot without a word.
  if (anyNA(cormat)) return(constant_var_plot())

  cormat_df <- melt_cormat(cormat, "Corr")
  
  cormat_df$Var1 <- factor(cormat_df$Var1, levels = vars)
  cormat_df$Var2 <- factor(cormat_df$Var2, levels = rev(vars))
  
  p <- ggplot(cormat_df, aes(x=Var1, y=Var2, color=Corr, size=abs(Corr))) + 
    geom_point() +
    scale_size_continuous(range = c(1, 15), guide="none") +
    scale_color_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0, limits = c(-1,1)) +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(x="", y="", title="Correlogram") +
    scale_x_discrete(labels = function(v) display_var_labels(v, labels)) +
    scale_y_discrete(labels = function(v) display_var_labels(v, labels))
  return(p)
}

# ── Spatial cross-correlogram ──────────────────────────────────────────────
# Lag is a distance here, not a row offset: stats::ccf() measures lag in ROWS
# of the uploaded table, which for a soil point table is upload order, not
# distance, and its +/-1.96/sqrt(n) bands additionally assume a stationary
# series - neither holds for cross-sectional spatial samples.
#
# The estimator here is the standard geostatistical one. With both variables
# standardised to zero mean and unit variance, the empirical cross variogram
# gamma_12(h) - computed by gstat with the same pair binning as every other
# variogram in the app - and the cross-covariance are related by
#     gamma_12(h) = C_12(0) - (C_12(h) + C_21(h)) / 2
# so the spatial cross-correlation at lag h is
#     rho_12(h) = r_12 - gamma_12(h)
# with r_12 the ordinary non-spatial correlation of the standardised variables
# (Goovaerts 1997 section 4.2.3; Isaaks & Srivastava 1989 ch. 4). Only the
# SYMMETRIC part of the cross-covariance is estimable from omnidirectional bins
# (pairs (i,j) and (j,i) land in the same bin), which is also why there is no
# negative-lag half: unlike a time series, an unordered point set has no
# "leads" and "lags".
#
# Coordinates are projected before binning (the app's never-measure-distance-in-
# degrees invariant). Returns list(bins, r0, n, unit, ranked, message); `message`
# is a user-facing placeholder reason and is non-NULL exactly when `bins` is NULL.
compute_spatial_cross_correlogram <- function(df, var1, var2, x_col, y_col,
                                              src_crs, proj_crs = NULL,
                                              n_bins = 15, method = "pearson") {
  out <- list(bins = NULL, r0 = NA_real_, n = 0L, unit = "m",
              ranked = !identical(method, "pearson"), message = NULL)
  fail <- function(msg) { out$message <- msg; out }

  if (is.null(x_col) || is.null(y_col) || is.null(src_crs) ||
      !all(c(x_col, y_col) %in% colnames(df))) {
    return(fail("Coordinates are not mapped yet.\nSet the X/Y columns and the Input Data CRS on the Data Setup tab."))
  }
  if (is.null(var1) || is.null(var2) || !all(c(var1, var2) %in% colnames(df))) {
    return(fail("Invalid variables specified"))
  }
  if (identical(var1, var2)) return(fail("Select two different variables"))

  d <- na.omit(df[, unique(c(var1, var2, x_col, y_col)), drop = FALSE])
  if (!is.numeric(d[[var1]]) || !is.numeric(d[[var2]])) return(fail("Both variables must be numeric"))
  if (nrow(d) < 15) return(fail("Insufficient data: at least 15 complete observations are needed"))
  # Fixed internal names: the coordinate columns are themselves selectable as
  # variables, so copying by position keeps a var1 == x_col choice from
  # producing a duplicated column name in the working frame.
  work <- data.frame(.cx = d[[x_col]], .cy = d[[y_col]],
                     .v1 = d[[var1]], .v2 = d[[var2]])

  pts <- tryCatch({
    p <- sf::st_as_sf(work, coords = c(".cx", ".cy"), crs = src_crs)
    if (!is.null(proj_crs) && !identical(proj_crs, "")) {
      p <- tryCatch(sf::st_transform(p, proj_crs), error = function(e) p)
    }
    validate_and_project_sf(p)
  }, error = function(e) NULL)
  if (is.null(pts) || nrow(pts) == 0) return(fail("Could not project the coordinates"))

  # Co-located samples merged as the interpolation point sets merge them
  # (merge_colocated: both variables averaged per location), so replicates do
  # not stack the shortest bin.
  pts <- merge_colocated(pts)
  out$n <- nrow(pts)
  if (out$n < 15) return(fail("Insufficient data: at least 15 distinct locations are needed"))

  crs_unit <- tryCatch(sf::st_crs(pts)$units, error = function(e) NULL)
  if (!is.null(crs_unit) && nzchar(crs_unit)) out$unit <- crs_unit

  # Rank transform for the rank-based methods: Spearman is the product-moment
  # correlation of ranks, so the whole estimator simply runs on ranks. Kendall
  # has no distance-binned analogue and is served by the same rank-based curve
  # (named as such in the plot subtitle).
  v1v <- pts$.v1; v2v <- pts$.v2
  if (out$ranked) { v1v <- rank(v1v); v2v <- rank(v2v) }
  s1 <- stats::sd(v1v); s2 <- stats::sd(v2v)
  if (!is.finite(s1) || !is.finite(s2) || s1 == 0 || s2 == 0) {
    return(fail("A selected variable is constant - no correlation is defined"))
  }
  pts$z1 <- (v1v - mean(v1v)) / s1
  pts$z2 <- (v2v - mean(v2v)) / s2
  out$r0 <- stats::cor(pts$z1, pts$z2)

  lags <- calc_scientific_lags(pts)
  # A cleared numericInput arrives as NA, not as NULL.
  nb <- suppressWarnings(as.integer(n_bins)[1])
  if (is.na(nb)) nb <- 15L
  width <- lags$cutoff / max(3L, min(50L, nb))
  if (!is.finite(width) || width <= 0) return(fail("Could not derive distance bins from the coordinates"))

  vg <- tryCatch({
    g <- gstat::gstat(NULL, id = "z1", formula = z1 ~ 1, data = pts)
    g <- gstat::gstat(g, id = "z2", formula = z2 ~ 1, data = pts)
    gstat::variogram(g, cross = TRUE, cutoff = lags$cutoff, width = width)
  }, error = function(e) NULL)
  if (is.null(vg)) return(fail("The cross variogram could not be computed for this pair"))

  cross <- vg[as.character(vg$id) == "z1.z2", , drop = FALSE]
  if (nrow(cross) == 0) return(fail("No point pairs fall inside the lag distance range"))

  out$bins <- data.frame(
    dist  = as.numeric(cross$dist),
    np    = as.integer(cross$np),
    gamma = as.numeric(cross$gamma),
    rho   = out$r0 - as.numeric(cross$gamma),
    stringsAsFactors = FALSE
  )
  out
}

generate_spatial_cross_correlogram <- function(df, var1, var2, x_col, y_col,
                                               src_crs, proj_crs = NULL,
                                               n_bins = 15, method = "pearson", labels = NULL) {
  res <- compute_spatial_cross_correlogram(df, var1, var2, x_col, y_col,
                                           src_crs, proj_crs, n_bins, method)
  if (is.null(res$bins)) {
    return(sci_placeholder(res$message, size = 4.5))
  }

  b <- res$bins
  # Journel & Huijbregts' rule of thumb for variogram bins: below ~30 pairs the
  # estimate is too noisy to read, so those bins are greyed instead of dropped.
  b$reliable <- b$np >= 30

  p <- ggplot(b, aes(x = dist, y = rho)) +
    geom_hline(yintercept = 0, color = "gray50") +
    geom_hline(yintercept = res$r0, linetype = "dashed", color = "#2c7fb8") +
    geom_line(color = "gray40", linewidth = 0.6) +
    geom_point(aes(size = np, color = reliable)) +
    scale_color_manual(values = c("TRUE" = "#2c7fb8", "FALSE" = "grey65"), guide = "none") +
    scale_size_continuous(range = c(1.5, 6), name = "Pairs") +
    theme_minimal() +
    labs(
      title = sprintf("Spatial Cross-Correlogram: %s vs %s", display_var_labels(var1, labels), display_var_labels(var2, labels)),
      subtitle = sprintf("%srho(h) = r - gamma12(h) on standardised values | non-spatial r = %.3f | n = %d",
                         if (res$ranked) "Rank-based (Spearman-type). " else "",
                         res$r0, res$n),
      x = sprintf("Lag distance (%s)", res$unit),
      y = "Cross-correlation",
      caption = "Dashed line = non-spatial correlation. Point size = pairs per bin; grey = fewer than 30 pairs."
    )
  return(p)
}

# The PCA guard's two findings, kept apart because they mean different things:
# `pairs` (var1, var2, r), the near-duplicate variables with |r| above
# `threshold`, and `constant`, the variables with no variance.
# `has_collinearity` covers the pairs only. PCA exists to summarise correlated
# variables, so correlation as such is no objection to it; a near-duplicate
# pair, though, carries one latent dimension twice and gives it double weight
# in a standardised PCA, which the user may accept or resolve. VIF measures how
# collinearity inflates the variance of REGRESSION coefficients and has no
# meaning for a PCA, so it plays no part here. A constant cannot enter a
# standardised PCA at all and is excluded by desc_pca_fit().
check_collinearity <- function(df, vars, threshold = 0.95) {
  res <- detect_multicollinearity_engine(df, vars = vars, pairwise_threshold = threshold,
                                         vif_threshold = Inf)
  pairs <- res$pairs %||% data.frame(var1 = character(), var2 = character(), r = numeric(),
                                     stringsAsFactors = FALSE)
  list(has_collinearity = nrow(pairs) > 0,
       pairs = pairs,
       constant = res$dropped_constant %||% character(0))
}

pca_axis_refusal <- function(pca_res, axes, count = length(axes)) {
  n <- ncol(pca_res$x)
  if (count == 3L && n < 3L) return("3D PCA Scores requires at least three available components.")
  if (!length(axes) || length(axes) != count || anyNA(axes) ||
      any(!is.finite(axes) | axes != floor(axes) | axes < 1 | axes > n) || anyDuplicated(axes)) {
    return(sprintf("Select distinct available components between 1 and %d.", n))
  }
  NULL
}

generate_pca_scree <- function(pca_res) {

  var_explained <- pca_res$sdev^2 / sum(pca_res$sdev^2)
  df_scree <- data.frame(PC = 1:length(var_explained), Variance = var_explained)
  
  p <- ggplot(df_scree, aes(x = PC, y = Variance)) +
    geom_bar(stat = "identity", fill = "steelblue", alpha=0.7) +
    geom_line(color = "red", linewidth=1) +
    geom_point(color = "red", size=2) +
    scale_x_continuous(breaks = 1:nrow(df_scree)) +
    theme_minimal() +
    labs(title = "Scree Plot (Variance Explained by PC)", y = "Proportion of Variance", x = "Principal Component")
  return(p)
}

generate_pca_biplot <- function(pca_res, original_df, pc_x = 1, pc_y = 2, group_col = NULL) {
  refusal <- pca_axis_refusal(pca_res, c(pc_x, pc_y), 2L)
  if (!is.null(refusal)) return(sci_placeholder(refusal))
  scores <- as.data.frame(pca_res$x)
  
  if (!is.null(group_col) && group_col %in% colnames(original_df)) {
    scores$Group <- original_df[[group_col]]
  } else {
    scores$Group <- "All"
  }
  
  loadings <- as.data.frame(pca_res$rotation)
  var_exp <- round(pca_res$sdev^2 / sum(pca_res$sdev^2) * 100, 1)
  
  mult <- min(
    (max(scores[, pc_x]) - min(scores[, pc_x])) / (max(loadings[, pc_x]) - min(loadings[, pc_x])),
    (max(scores[, pc_y]) - min(scores[, pc_y])) / (max(loadings[, pc_y]) - min(loadings[, pc_y]))
  ) * 0.7
  
  loadings_scaled <- loadings * mult
  
  p <- ggplot() +
    geom_point(data = scores, aes(x = .data[[paste0("PC", pc_x)]], y = .data[[paste0("PC", pc_y)]], color = .data[["Group"]]), alpha = 0.6) +
    geom_segment(data = loadings_scaled, aes(x = 0, y = 0, xend = .data[[paste0("PC", pc_x)]], yend = .data[[paste0("PC", pc_y)]]), arrow = arrow(length = unit(0.2, "cm")), color = "red", alpha=0.8) +
    geom_text(data = loadings_scaled, aes(x = .data[[paste0("PC", pc_x)]], y = .data[[paste0("PC", pc_y)]], label = rownames(loadings_scaled)), color = "darkred", vjust = "outward", hjust = "outward", size=4) +
    theme_minimal() +
    labs(title = paste("Biplot (PC", pc_x, " vs PC", pc_y, ")", sep=""),
         x = paste("PC", pc_x, " (", var_exp[pc_x], "%)", sep=""),
         y = paste("PC", pc_y, " (", var_exp[pc_y], "%)", sep=""))
  
  return(p)
}

generate_pca_bar_plot <- function(values_named, val_name, title_text, y_label, fill_color = NULL, exp_cont = NULL) {
  df <- data.frame(Variable = names(values_named), Value = values_named)
  
  if (val_name == "Loading") {
    df <- df[order(abs(df$Value), decreasing = TRUE), ]
  } else {
    df <- df[order(df$Value, decreasing = TRUE), ]
  }
  df$Variable <- factor(df$Variable, levels = rev(df$Variable))
  
  p <- ggplot(df, aes(x = Variable, y = Value))
  if (val_name == "Loading") {
    p <- p + geom_bar(stat = "identity", aes(fill = Value > 0)) +
      scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "indianred"), guide = "none")
  } else {
    p <- p + geom_bar(stat = "identity", fill = fill_color, alpha = 0.8)
  }
  
  if (!is.null(exp_cont)) {
    p <- p + geom_hline(yintercept = exp_cont, linetype = "dashed", color = "red")
  }
  
  p <- p + coord_flip() + theme_minimal() + labs(title = title_text, x = "Variable", y = y_label)
  if (!is.null(exp_cont)) p <- p + labs(caption = "Dashed line indicates expected average contribution")
  
  return(p)
}

generate_pca_loadings <- function(pca_res, pc = 1) {
  refusal <- pca_axis_refusal(pca_res, pc, 1L)
  if (!is.null(refusal)) return(sci_placeholder(refusal))
  generate_pca_bar_plot(pca_res$rotation[, pc], "Loading", paste("Loadings for PC", pc), "Loading Weight")
}

generate_pca_contribution <- function(pca_res, pc = 1) {
  refusal <- pca_axis_refusal(pca_res, pc, 1L)
  if (!is.null(refusal)) return(sci_placeholder(refusal))
  loadings <- pca_res$rotation[, pc]
  contrib <- (loadings^2) * 100
  generate_pca_bar_plot(contrib, "Contribution", paste("Variable Contribution to PC", pc), "Contribution (%)", "coral", 100 / length(contrib))
}

# cos2 = the SHARE of a variable's variance captured by the selected PCs, so
# the denominator has to be that variable's total variance across ALL retained
# components: rowSums((V S)^2) = (V S^2 V')_jj = Var(x_j). For a correlation
# PCA (scale. = TRUE) that denominator is 1 for every variable, so the sum of
# the selected axes alone is the share; for a covariance PCA (scale. = FALSE)
# the unnormalised sum is an absolute variance in the variable's own squared
# units, and mg/kg next to a 0-1 fraction on an axis labelled cos2 is not a
# cos2. Normalising makes the quantity mean the same thing (bounded [0, 1],
# factoextra's definition) in both modes and changes nothing for a scaled PCA.
generate_pca_cos2 <- function(pca_res, axes = 1:2) {
  refusal <- pca_axis_refusal(pca_res, axes)
  if (!is.null(refusal)) return(sci_placeholder(refusal))
  coord <- sweep(pca_res$rotation, 2, pca_res$sdev, "*")
  total_var <- rowSums(coord^2)
  cos2 <- rowSums(coord[, axes, drop = FALSE]^2) / total_var
  # A variable with no variance (or a rank-deficient rotation that carries no
  # component for it) has no representation to report - NA, never 0/0 = NaN.
  cos2[!is.finite(cos2)] <- NA_real_
  generate_pca_bar_plot(cos2, "Cos2", paste("Quality of Representation (cos2) on PC", paste(axes, collapse=" & ")), "cos2", "mediumseagreen")
}

generate_pca_cumvar <- function(pca_res) {

  var_explained <- pca_res$sdev^2 / sum(pca_res$sdev^2)
  cum_var <- cumsum(var_explained)
  
  df_cum <- data.frame(PC = 1:length(cum_var), CumVar = cum_var)
  
  p <- ggplot(df_cum, aes(x = PC, y = CumVar)) +
    geom_line(color = "darkblue", linewidth=1.2) +
    geom_point(color = "orange", size=3) +
    geom_hline(yintercept = 0.8, linetype="dashed", color="red", alpha=0.6) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_x_continuous(breaks = 1:nrow(df_cum)) +
    theme_minimal() +
    labs(title = "Cumulative Variance Explained", 
         x = "Principal Component", y = "Cumulative Variance",
         caption = "Dashed line indicates 80% threshold")
  return(p)
}

generate_pca_mahalanobis <- function(pca_res) {

  # PC scores are uncorrelated by construction, so their covariance is
  # diag(sdev^2); near-zero-variance PCs (collinear inputs) would make that
  # matrix numerically singular, so they are excluded from the distance
  #
  # This is the CLASSICAL Mahalanobis distance: the centre and scatter are the
  # ordinary mean/covariance, which the outliers themselves contribute to. A
  # small cluster of extreme observations therefore inflates the covariance and
  # can pull its own distance back under the threshold (masking). A robust
  # estimator (MCD, robustbase::covMcd on the retained scores) is the standard
  # remedy but is a new dependency; the panel names its estimator instead so the
  # limitation is visible where it matters. See scientific_guide.md 8.2.
  keep <- pca_res$sdev > max(pca_res$sdev) * 1e-8
  scores <- as.data.frame(pca_res$x[, keep, drop = FALSE])
  center <- colMeans(scores)
  cov_mat <- diag(pca_res$sdev[keep]^2, nrow = sum(keep))

  md <- mahalanobis(scores, center, cov_mat)
  df_md <- data.frame(Index = 1:length(md), Distance = md)

  thresh <- qchisq(0.975, df = ncol(scores))
  
  p <- ggplot(df_md, aes(x = Index, y = Distance)) +
    geom_point(aes(color = Distance > thresh), size=2, alpha=0.8) +
    geom_segment(aes(x=Index, xend=Index, y=0, yend=Distance, color=Distance > thresh)) +
    geom_hline(yintercept = thresh, linetype="dashed", color="red") +
    scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"), guide="none") +
    theme_minimal() +
    labs(title = "Mahalanobis Distance (Classical Estimator)",
         subtitle = "Classical mean/covariance: outliers inflate the covariance and can mask themselves",
         x = "Observation Index", y = "Distance",
         caption = sprintf("Dashed line indicates the 97.5%% Chi-Square threshold (df = %d retained PCs)", ncol(scores)))
  return(p)
}

generate_pca_biplot_3d <- function(pca_res, df, pc_x=1, pc_y=2, pc_z=3, group_col=NULL) {
  refusal <- pca_axis_refusal(pca_res, c(pc_x, pc_y, pc_z), 3L)
  if (!is.null(refusal)) return(sci_placeholder(refusal))
  
  scores <- as.data.frame(pca_res$x)
  if (!is.null(group_col) && group_col %in% colnames(df)) {
    scores$Group <- df[[group_col]]
  } else {
    scores$Group <- factor("All")
  }
  
  var_exp <- round(pca_res$sdev^2 / sum(pca_res$sdev^2) * 100, 1)
  
  p <- plot_ly(scores, x = ~get(paste0("PC", pc_x)), y = ~get(paste0("PC", pc_y)), z = ~get(paste0("PC", pc_z)), 
               color = ~Group, type = "scatter3d", mode = "markers",
               marker = list(size = 4, opacity = 0.8)) %>%
       layout(title = "3D PCA Scores",
              scene = list(
                xaxis = list(title = paste0("PC", pc_x, " (", var_exp[pc_x], "%)")),
                yaxis = list(title = paste0("PC", pc_y, " (", var_exp[pc_y], "%)")),
                zaxis = list(title = paste0("PC", pc_z, " (", var_exp[pc_z], "%)"))
              ))
  return(p)
}

# Distance ruler for the Map Viewer's leaflet widgets.
#
# addMeasure ships with leaflet (the leaflet-measure plugin and its CSS are
# bundled), so this costs no dependency and no external asset. draw_map applies
# it to EVERY widget it builds, which is what puts the tool on the single map
# and on both comparison maps without a per-map code path.
#
# Bottom-left keeps the control clear of the top-left drawing stack.
#
# The plugin's own popup reports SPHERICAL figures (mean-Earth radius) and knows
# nothing about the analysis CRS, so the readout is taken over: on finish the
# clicked vertices go to R, measure_path_metrics() recomputes them on the WGS84
# ellipsoid and in the Target Mapping CRS, and map_ruler_popup_html() comes back
# to REPLACE that shape's popup contents. The numbers therefore travel with the
# shape - clicking a measurement made ten minutes ago reopens its own figures -
# where a single control in the corner could only ever hold the latest one.
# Coordinates travel as two flat arrays rather than a list of pairs, so the R
# side receives plain numeric vectors whatever Shiny's JSON simplification does.
#
# Cancel calls the same _finishMeasure() the Finish link does, so measurefinish
# alone cannot tell a completed shape from an abandoned one; only a real finish
# adds the result layer on the same tick, which is why the layeradd that follows
# is what actually triggers the round trip. The record lives in a page-level
# store because the reply arrives through one custom message handler shared by
# all three map widgets.
# Basemap tiles for every interactive map in the app: the Map Viewer's three
# widgets, their proxy swap when the toolbar dropdown changes, and the Data
# Setup mini-map. One entry point because the widget build and the proxy swap
# have to add the layer on identical terms - a basemap that behaves differently
# from the one the map was born with is a bug by construction.
#
# zIndex = 0 is load-bearing. leaflet::addRasterImage() does not use an image
# overlay: it paints the surface as a canvas GridLayer, so the interpolated
# raster and the basemap are siblings in the same Leaflet tile pane, both on
# the GridLayer default z-index of 1, and only DOM insertion order keeps the
# surface on top. That order holds when the widget is built (basemap first,
# rasters after) and inverts on a proxy swap, which appends the new basemap
# last and paints it straight over the surface. Pinning the basemap one level
# below the rasters makes the stacking explicit instead of incidental. It has
# to be 0 rather than a negative level: a negative z-index drops the layer
# behind the tile pane's own painted ground in some browsers, which is a map
# that answers a basemap switch with a bare grey rectangle.
#
# The five providers stop at different native zooms. Left to Leaflet, adding a
# shallower layer to a map already zoomed past its limit drops the map's zoom
# to that layer's maximum, and the drop is permanent: switching back to a
# deeper provider does not raise it again. A layer whose maximum sits below the
# current zoom draws no tiles at all. Declaring the provider's depth as
# `maxNativeZoom` instead makes Leaflet upscale the deepest tiles it has, so
# all five cover the same zoom range and switching basemaps changes the imagery
# and nothing else.
BASE_TILE_NATIVE_ZOOM <- c(
  "Esri.WorldImagery"  = 19,
  "OpenTopoMap"        = 17,
  "OpenStreetMap"      = 19,
  "CartoDB.DarkMatter" = 20,
  "CartoDB.Positron"   = 20
)
BASE_TILE_MAX_ZOOM <- 20

# CARTO's raster basemaps now require a free API key: an unkeyed request is
# answered with an "API key required" watermark stamped over the tiles, and
# CARTO has the raster service on a retirement path. leaflet-providers' CartoDB
# entry carries no key placeholder, so a keyed request has to be issued as a
# plain tile layer built from CARTO's own template
# (https://{s}.basemaps.cartocdn.com/{style}/{z}/{x}/{y}{r}.png?key=...).
# Everything the switcher depends on - layerId, zIndex, and the two zoom limits
# below - is set identically on both paths, so a keyed CARTO layer behaves
# exactly like every other basemap.
BASE_TILE_CARTO_VARIANT <- c(
  "CartoDB.Positron"   = "light_all",
  "CartoDB.DarkMatter" = "dark_all"
)
BASE_TILE_CARTO_ATTRIBUTION <- paste0(
  '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors ',
  '&copy; <a href="https://carto.com/attributions">CARTO</a>'
)

# api_key is used only by the two CARTO providers; every other provider ignores
# it. An empty key keeps the previous unkeyed behaviour (watermarked tiles)
# rather than blanking the map, so the switcher never answers with nothing.
add_base_tiles <- function(map, provider, api_key = NULL) {
  if (is.null(provider) || !nzchar(provider)) provider <- "Esri.WorldImagery"
  native <- unname(BASE_TILE_NATIVE_ZOOM[provider])
  if (is.na(native)) native <- BASE_TILE_MAX_ZOOM

  carto_variant <- unname(BASE_TILE_CARTO_VARIANT[provider])
  key <- if (is.null(api_key)) "" else trimws(as.character(api_key)[1])
  if (is.na(key)) key <- ""
  if (!is.na(carto_variant) && nzchar(key)) {
    return(leaflet::addTiles(
      map,
      urlTemplate = paste0("https://{s}.basemaps.cartocdn.com/", carto_variant,
                           "/{z}/{x}/{y}{r}.png?key=",
                           utils::URLencode(key, reserved = TRUE)),
      attribution = BASE_TILE_CARTO_ATTRIBUTION,
      layerId = "base_tiles",
      options = leaflet::tileOptions(
        subdomains = "abcd",
        zIndex = 0,
        maxZoom = BASE_TILE_MAX_ZOOM,
        maxNativeZoom = native
      )
    ))
  }

  leaflet::addProviderTiles(
    map, provider, layerId = "base_tiles",
    options = leaflet::providerTileOptions(
      zIndex = 0,
      maxZoom = BASE_TILE_MAX_ZOOM,
      maxNativeZoom = native
    )
  )
}

add_map_ruler <- function(map, position = "bottomleft") {
  map %>%
    leaflet::addMeasure(
      position = position,
      primaryLengthUnit = "meters", secondaryLengthUnit = "kilometers",
      primaryAreaUnit = "hectares", secondaryAreaUnit = "sqmeters",
      activeColor = "#fab005", completedColor = "#e74c3c"
    ) %>%
    htmlwidgets::onRender("
      function(el, x) {
        var map = this;
        var store = window.__monolithRuler = window.__monolithRuler || {};
        if (!window.__monolithRulerInit) {
          window.__monolithRulerInit = true;
          Shiny.addCustomMessageHandler('monolith_ruler_result', function(msg) {
            var rec = store[msg.token];
            if (rec) { rec.filled = true; rec.render(msg.html); }
          });
        }
        // A re-rendered widget takes its shapes with it: those records name a
        // map instance that no longer exists and can never be reached again.
        Object.keys(store).forEach(function(k) {
          if (store[k].mapId === el.id) delete store[k];
        });

        // map_ruler_css() hides the Cancel / Finish label text so the two sit
        // in the panel as their icons alone. Hidden text is not an accessible
        // name, so give them a real one; re-applied on measurestart because
        // that is the moment they become visible.
        var label = function() {
          [['.js-cancel', 'Cancel measurement'],
           ['.js-finish', 'Finish measurement']].forEach(function(p) {
            var a = el.querySelector('.leaflet-control-measure ' + p[0]);
            if (a) { a.setAttribute('title', p[1]); a.setAttribute('aria-label', p[1]); }
          });
        };
        label();

        // The tile provider's attribution runs along the bottom of the map and
        // wraps to two or three lines when the provider is wordy (Esri's imagery
        // credit does) or the map is narrow, and it then covers a control pinned
        // to the bottom corner - the ruler button and its panel both disappeared
        // behind it. Lift the control clear of whatever height the attribution
        // currently has: the basemap is switchable, so the offset is measured
        // rather than assumed, and re-measured whenever the map resizes or a
        // layer (a new basemap) arrives with its own credit line.
        var lift = function() {
          var ctrl = el.querySelector('.leaflet-control-measure');
          if (!ctrl) return;
          var attr = el.querySelector('.leaflet-control-attribution');
          var h = attr ? attr.getBoundingClientRect().height : 0;
          ctrl.style.marginBottom = (h > 0 ? Math.round(h) + 4 : 0) + 'px';
        };
        lift();
        setTimeout(lift, 300);
        map.on('resize layeradd baselayerchange', lift);

        // Built as a live DOM node, not a string: the two task links are wired
        // here, so they keep working after the popup is closed and reopened.
        var content = function(html, layer) {
          var div = document.createElement('div');
          div.innerHTML = html;
          var wire = function(sel, fn) {
            var a = div.querySelector(sel);
            if (a) L.DomEvent.on(a, 'click', function(ev) { L.DomEvent.stop(ev); fn(); });
          };
          wire('.mono-ruler-zoom', function() {
            if (layer.getBounds) map.fitBounds(layer.getBounds(), { padding: [20, 20], maxZoom: 17 });
            else if (layer.getLatLng) map.panTo(layer.getLatLng());
          });
          wire('.mono-ruler-delete', function() { map.removeLayer(layer); });
          return div;
        };

        var pending = null;
        map.on('measurestart', function() { label(); });
        map.on('measurefinish', function(e) {
          // Copied, not referenced: e.points IS the plugin's own vertex array,
          // and it pushes the closing vertex into it after this event to build
          // the polygon. Holding the reference would report one point too many
          // (the shape's first vertex, repeated) once the layer arrives.
          pending = (e.points || []).map(function(p) { return { lng: p.lng, lat: p.lat }; });
          setTimeout(function() { pending = null; }, 0);
        });
        map.on('layeradd', function(e) {
          if (pending === null) return;
          var pts = pending, layer = e.layer;
          pending = null;
          // A single point has no length to report; the plugin's own
          // coordinate popup is left alone.
          if (pts.length < 2) return;
          var token = 'mr' + Date.now() + Math.random().toString(36).slice(2, 8);
          var rec = store[token] = {
            mapId: el.id, layer: layer, filled: false,
            render: function(html) { layer.setPopupContent(content(html, layer)); }
          };
          // The plugin binds and opens its popup immediately after this event,
          // so the placeholder can only be written on the next tick - and only
          // if the round trip has not already beaten it there.
          setTimeout(function() {
            if (!rec.filled) {
              layer.setPopupContent(
                '<div class=\"monolith-ruler-popup\"><h3>Measurement</h3>' +
                '<p style=\"color: var(--mn-text-3);\">Computing...</p></div>');
            }
          }, 0);
          Shiny.setInputValue(
            el.id + '_ruler',
            { lng: pts.map(function(p) { return p.lng; }),
              lat: pts.map(function(p) { return p.lat; }),
              token: token },
            { priority: 'event' }
          );
        });
        map.on('layerremove', function(e) {
          Object.keys(store).forEach(function(k) {
            if (store[k].layer === e.layer) delete store[k];
          });
        });
      }
    ")
}

# The outline of a point with no measured value. A literal, not a theme token:
# leaflet writes this straight into the SVG `stroke` presentation attribute,
# where a CSS var() does not resolve. Mid grey, so it reads on both a light
# and a dark basemap.
MISSING_VALUE_POINT_COLOR <- "#9AA0A6"
MISSING_VALUE_POINT_LABEL <- "No measured value (not used in the fit)"

# `value_col` names the mapped variable's column. Points with no value there
# render HOLLOW - position still visible, but unmistakably not a measurement -
# and gain their own legend entry. The display set is deliberately wider than
# the fitted set (see run_regional_interpolation), so the distinction has to be
# on the map rather than in a caption nobody reads.
add_styled_points <- function(map, pts_sf, color_by = "none", custom_colors = NULL,
                              show_labels = FALSE, label_field = "none",
                              label_size = 11, marker_size = 3,
                              popup_fn = NULL, legend_layer_id = NULL,
                              value_col = NULL) {

  crs_obj <- sf::st_crs(pts_sf)
  pts_view <- if (is.na(crs_obj$epsg) || crs_obj$epsg != 4326) sf::st_transform(pts_sf, 4326) else pts_sf
  if (nrow(pts_view) == 0) return(map)

  use_groups <- color_by != "none" && color_by %in% colnames(pts_view)
  no_value <- if (!is.null(value_col) && value_col %in% colnames(pts_view)) {
    is.na(pts_view[[value_col]])
  } else rep(FALSE, nrow(pts_view))

  legend_colors <- character(0)
  legend_labels <- character(0)
  legend_title <- NULL

  if (use_groups && !is.null(custom_colors)) {
    grp_vals <- as.character(pts_view[[color_by]])
    groups <- sort(unique(grp_vals))
    missing <- setdiff(groups, names(custom_colors))
    if (length(missing) > 0) {
      extra <- generate_group_palette(missing, "Set1")
      custom_colors <- c(custom_colors, extra)
    }
    pal_fn <- leaflet::colorFactor(
      palette = unname(custom_colors[groups]),
      domain = groups
    )
    fill_colors <- pal_fn(grp_vals)
    border_color <- "white"
    fill_opacity <- 0.85
    legend_colors <- unname(custom_colors[groups])
    legend_labels <- groups
    legend_title <- color_by
  } else {
    fill_colors <- "cyan"
    border_color <- "cyan"
    fill_opacity <- 0.5
  }

  # Applied last, so switching the colour-by mode cannot hide the distinction.
  if (any(no_value)) {
    n <- nrow(pts_view)
    fill_colors <- rep(fill_colors, length.out = n)
    border_color <- rep(border_color, length.out = n)
    fill_opacity <- rep(fill_opacity, length.out = n)
    fill_colors[no_value] <- MISSING_VALUE_POINT_COLOR
    border_color[no_value] <- MISSING_VALUE_POINT_COLOR
    fill_opacity[no_value] <- 0
    legend_colors <- c(legend_colors, MISSING_VALUE_POINT_COLOR)
    legend_labels <- c(legend_labels, MISSING_VALUE_POINT_LABEL)
  }

  if (length(legend_colors) > 0) {
    map <- map %>% leaflet::addLegend(
      position = "bottomleft",
      colors = legend_colors,
      labels = legend_labels,
      title = legend_title,
      opacity = 0.9,
      layerId = legend_layer_id
    )
  }

  popups <- NULL
  if (!is.null(popup_fn)) {
    df_clean <- sf::st_drop_geometry(pts_view)
    popups <- vapply(seq_len(nrow(df_clean)), function(i) popup_fn(df_clean[i, ]), character(1))
  }

  map <- map %>% leaflet::addCircleMarkers(
    data = pts_view,
    radius = marker_size,
    color = border_color,
    weight = 1,
    fillColor = fill_colors,
    fillOpacity = fill_opacity,
    opacity = 1,
    popup = popups,
    group = "styled_points"
  )

  if (show_labels && label_field != "none" && label_field %in% colnames(pts_view)) {
    raw_vals <- pts_view[[label_field]]
    label_vals <- if (is.numeric(raw_vals)) {
      ifelse(is.na(raw_vals), NA_character_, sprintf("%.2f", raw_vals))
    } else {
      as.character(raw_vals)
    }
    map <- map %>% leaflet::addLabelOnlyMarkers(
      data = pts_view,
      label = label_vals,
      labelOptions = leaflet::labelOptions(
        noHide = TRUE, direction = "top", textOnly = TRUE,
        offset = c(0, -8),
        style = list(
          "font-size" = paste0(label_size, "px"),
          "font-weight" = "bold",
          "color" = "white",
          "text-shadow" = "1px 1px 2px rgba(0,0,0,0.9), -1px -1px 2px rgba(0,0,0,0.9), 1px -1px 2px rgba(0,0,0,0.9), -1px 1px 2px rgba(0,0,0,0.9)"
        )
      ),
      group = "styled_labels"
    )
  }

  map
}
