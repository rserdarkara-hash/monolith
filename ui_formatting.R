# ui_formatting.R - text/label/chip formatters and pure data-wrangling
# helpers (metadata matching, discretization, grouping). No reactivity.
# Sourced via ui_helpers.R.


# Single source of truth for turning the sidebar locality selection into the
# locality set an analysis should run on. "ALL", an empty selection, and NULL
# all resolve to every non-NA locality in the data; anything else is taken
# verbatim. Returns character(0) when the data or locality column is missing,
# so callers can length()-guard without NULL checks.
resolve_selected_localities <- function(sel, user_data, loc_col) {
  all_locs <- if (!is.null(user_data) && !is.null(loc_col) &&
                  loc_col %in% colnames(user_data)) {
    unique(user_data[[loc_col]][!is.na(user_data[[loc_col]])])
  } else {
    character(0)
  }
  if (is.null(sel) || length(sel) == 0 || "ALL" %in% sel) return(all_locs)
  sel
}

#' Choices of the Scientific Analysis locality filter: the displayed run's
#' localities, then any locality whose variogram is being tuned and that the
#' run did not cover. "Total (Combined)" leads only for a multi-locality run,
#' because the combined tables and the pooled panels describe that run.
sci_locality_choices <- function(run_locs, tuned_locs = character(0)) {
  run_locs <- as.character(run_locs %||% character(0))
  locs <- unique(c(run_locs, as.character(tuned_locs %||% character(0))))
  if (length(run_locs) > 1) c("Total (Combined)", locs) else locs
}

#' Title tail and subtitle of a pooled within-locality variogram panel
#' (pooled_within_variogram / pooled_within_directional): "k localities", and
#' a note naming the localities left out, NULL when none was.
pooled_within_caption <- function(v) {
  used <- attr(v, "localities")
  excl <- attr(v, "excluded")
  list(count = sprintf("%d %s", length(used), if (length(used) == 1) "locality" else "localities"),
       note = if (length(excl))
         sprintf("Left out (fewer than %d located values): %s", POOLED_VGM_MIN_N,
                 paste(excl, collapse = ", ")))
}

#' Which column should an axis dropdown default to after an upload?
#' Whole-name matching on the same token lists is_coord_col() (spatial_metrics.R)
#' uses; substring matching ("^lon" / "^lat") would take variables such as
#' Longevity_index or Lateral_flow for a coordinate.
#' Returns NULL when nothing matches, which leaves the first column selected.
pick_coord_column <- function(cols, axis = c("x", "y")) {
  axis <- match.arg(axis)
  tokens <- if (axis == "x") .coord_names_x else .coord_names_y
  if (is.null(cols) || !length(cols)) return(NULL)
  hit <- cols[tolower(trimws(cols)) %in% tokens]
  if (length(hit)) hit[1] else NULL
}

melt_cormat <- function(cormat, value_name = "Corr") {
  rn <- rownames(cormat)
  cn <- colnames(cormat)
  df <- data.frame(
    Var1 = rep(rn, each = length(cn)),
    Var2 = rep(cn, times = length(rn)),
    Value = as.vector(t(cormat)),
    stringsAsFactors = FALSE
  )
  colnames(df)[3] <- value_name
  df
}

# Partial correlation matrix for `vars`, controlling for `control_vars`.
# Single source for BOTH the Partial Correlation heatmap and the correlation
# summary table, so the two cannot residualize differently or disagree on
# quoting (labels containing spaces).
#
# Conventions follow ppcor, which the table's p-value block already cites:
#   pearson  - residualize the RAW values on the controls, product-moment
#              correlation of the residuals (algebraically identical to
#              inverting the Pearson correlation matrix of the pair plus the
#              controls).
#   spearman - rank-transform EVERY column first, then residualize and take the
#              product-moment correlation of the rank residuals. Correlating
#              raw-value residuals with method = "spearman" is NOT a partial
#              rank correlation: ppcor residualizes the ranks,
#              and cor(method = "spearman") of the residuals would instead
#              re-rank residuals of an unranked fit.
#   kendall  - no residualization analogue exists; ppcor inverts the Kendall
#              tau matrix (the multi-control generalisation of Kendall's
#              first-order partial tau). Each PAIR inverts the tau matrix of
#              that pair plus the controls only: inverting one matrix of every
#              target would condition each pair on the other targets as well,
#              so adding a target to the table would change every estimate
#              while the p-values still counted only the controls.
# Residualization uses one pivoted QR over the shared control design matrix
# (model.matrix keeps factor controls and awkward column names working) — the
# same fit lm() would produce, but computed once for all variables.
#
# Returns list(cormat, n, k, df, method, failed, reason). For Pearson/Spearman,
# k is the control design's rank minus its intercept, and df = n - k - 2.
# `cormat` is NULL when the partial correlation could not be computed; `failed`
# then names the offending columns and `reason` says why, so the caller can
# abort (partial_correlation_refusal() words it) instead of silently reporting
# raw correlations under a "partial" label.
compute_partial_correlation <- function(df, vars, control_vars = NULL,
                                        method = "pearson") {
  vars <- unique(vars)
  # A variable must never control for itself: residualizing v against a set
  # containing v yields ~zero residuals and a NaN row.
  ctrl <- setdiff(unique(control_vars), vars)
  out <- list(cormat = NULL, n = 0L, k = length(ctrl), df = NA_integer_, method = method,
              failed = character(0), reason = NULL)

  cols <- c(vars, ctrl)
  missing_cols <- setdiff(cols, colnames(df))
  if (length(missing_cols) > 0) {
    out$failed <- missing_cols
    return(out)
  }

  d <- stats::na.omit(df[, cols, drop = FALSE])
  out$n <- nrow(d)
  out$df <- out$n - out$k - 2L
  if (length(vars) < 2 || out$n < 3) return(out)

  if (length(ctrl) == 0) {
    out$cormat <- stats::cor(d[, vars, drop = FALSE], method = method)
    return(out)
  }

  if (identical(method, "kendall")) {
    tau <- stats::cor(d, method = "kendall")
    pc <- diag(length(vars))
    dimnames(pc) <- list(vars, vars)
    bad <- character(0)
    for (a in seq_len(length(vars) - 1L)) {
      for (b in (a + 1L):length(vars)) {
        idx <- c(vars[a], vars[b], ctrl)
        inv <- tryCatch(solve(tau[idx, idx, drop = FALSE]), error = function(e) NULL)
        if (is.null(inv) || any(!is.finite(inv))) {
          bad <- union(bad, vars[c(a, b)])
          next
        }
        pc[a, b] <- pc[b, a] <- -inv[1, 2] / sqrt(inv[1, 1] * inv[2, 2])
      }
    }
    if (length(bad) > 0) {
      out$failed <- bad
      return(out)
    }
    out$cormat <- pc
    return(out)
  }

  fit_df <- d
  if (identical(method, "spearman")) fit_df[] <- lapply(d, rank)
  rank_tol <- 1e-7  # stats::lm / base::qr rank tolerance
  centre_scale <- function(m) {
    m <- sweep(m, 2, colMeans(m), "-")
    scale <- apply(abs(m), 2, max)
    scale[scale == 0] <- 1
    sweep(m, 2, scale, "/")
  }
  fit <- tryCatch({
    X <- stats::model.matrix(~ ., data = fit_df[, ctrl, drop = FALSE])
    # The intercept and fitted subspace are unchanged. Centering and scaling
    # keep measurement units or a large offset from determining numerical rank.
    X <- cbind(`(Intercept)` = 1, centre_scale(X[, -1, drop = FALSE]))
    Y <- centre_scale(as.matrix(fit_df[, vars, drop = FALSE]))
    q <- qr(X, tol = rank_tol)
    list(q = q, residuals = qr.resid(q, Y), targets = Y)
  }, error = function(e) NULL)
  if (is.null(fit)) {
    out$failed <- vars
    return(out)
  }
  out$k <- fit$q$rank - 1L
  out$df <- out$n - fit$q$rank - 1L
  if (out$df <= 0L) {
    out$reason <- "the controls leave no residual degrees of freedom"
    return(out)
  }
  resid_mat <- fit$residuals
  colnames(resid_mat) <- vars
  # Roundoff residuals from an exactly explained target are not information.
  # Compare their norm with the original CENTRED target at the QR tolerance.
  # The common rescaling keeps both norms invariant to units and offsets.
  explained <- vapply(seq_along(vars), function(i) {
    y <- fit$targets[, i]
    scale <- max(abs(y))
    if (!is.finite(scale) || scale == 0) return(TRUE)
    sqrt(sum((resid_mat[, i] / scale)^2)) <= rank_tol * sqrt(sum((y / scale)^2))
  }, logical(1))
  if (any(explained)) {
    out$failed <- vars[explained]
    out$reason <- "no residual variation remains after controlling for the selected variables"
    return(out)
  }
  out$cormat <- stats::cor(resid_mat, method = "pearson")
  out
}

# The user-facing refusal for a compute_partial_correlation() result that has no
# estimate, naming the offending variables by their display labels; NULL when
# there is nothing to refuse. Shared by the partial-correlation plot and table.
partial_correlation_refusal <- function(pc, labels = NULL) {
  named <- paste(display_var_labels(pc$failed, labels), collapse = ", ")
  if (!is.null(pc$reason)) {
    paste0("Partial correlation is undefined", if (length(pc$failed)) paste0(" for ", named), ": ", pc$reason, ".")
  } else if (length(pc$failed)) {
    paste0("Could not partial out the control variables for ", named, ".")
  }
}

# Map Viewer view id -> the surfaces it shows (`base`) and the layer drawn from
# them (`layer`: "value", "se" or "var"). The uncertainty views read the
# prediction-variance band of the same surfaces as their base view; the
# residual view has no variance band, so its layer is always "value".
parse_map_view <- function(view) {
  view <- if (is.null(view) || length(view) != 1 || is.na(view)) "" else as.character(view)
  m <- regmatches(view, regexec("^(view_(act|pred|comp|resid))(_(se|var))?$", view))[[1]]
  if (length(m) == 0) return(list(base = "view_act", layer = "value"))
  layer <- if (nzchar(m[5]) && m[2] != "view_resid") m[5] else "value"
  list(base = m[2], layer = layer)
}

# Choices of the Map Viewer view menu: the surfaces the displayed run computed
# and, for a method with a prediction variance, the standard-error and
# variance views of those surfaces. Groups become <optgroup>s.
map_view_choices <- function(has_pred, has_resid, has_variance) {
  surf <- c("View: Actual" = "view_act")
  if (has_pred) surf <- c(surf, "View: ML Predicted" = "view_pred",
                          "View: Actual vs Predicted" = "view_comp")
  if (has_resid) surf <- c(surf, "View: ML Residuals" = "view_resid")
  if (!isTRUE(has_variance)) return(surf)
  uncert <- function(prefix, suffix) {
    u <- c("Actual" = paste0("view_act", suffix))
    if (has_pred) u <- c(u, "ML Predicted" = paste0("view_pred", suffix),
                         "Actual vs Predicted" = paste0("view_comp", suffix))
    stats::setNames(as.list(u), paste0(prefix, names(u)))
  }
  list("Surfaces" = as.list(surf),
       "Uncertainty: standard error" = uncert("SE: ", "_se"),
       "Uncertainty: variance" = uncert("Variance: ", "_var"))
}

method_labels <- c(
  "OK"  = "Ordinary Kriging",
  "RK"  = "Regression Kriging",
  "RFK" = "Random Forest Kriging",
  "CK"  = "Co-Kriging",
  "IDW" = "IDW",
  "TPS" = "Thin Plate Spline"
)

# Resolution-logic id -> the sidebar's own wording, for records that print the
# stored value ("local"/"global"/"fixed") back to the user.
res_mode_label <- function(res_mode) {
  if (is.null(res_mode) || length(res_mode) != 1 || is.na(res_mode)) return("not recorded")
  switch(as.character(res_mode),
         "fixed" = "Fixed", "global" = "Auto (Global)",
         "local" = "Auto (Per Locality)", as.character(res_mode))
}

get_method_label <- function(method) {
  if (is.null(method) || length(method) == 0 || is.na(method) || method == "") return("")
  if (method %in% names(method_labels)) {
    return(method_labels[[method]])
  }
  return(method)
}

# The stored encoding of an IDW power or TPS smoothing setting, which the
# engines read: -1 for Auto (CV) / Auto (GCV), 0 for exact TPS, else the value.
idw_param_value <- function(mode, p) if (identical(mode, "cv")) -1 else p
tps_param_value <- function(mode, lambda) {
  switch(mode %||% "gcv", exact = 0, fixed = lambda, -1)
}

# The panel mode an encoded value stands for ("cv"/"fixed" for IDW,
# "gcv"/"exact"/"fixed" for TPS). NA or negative is Auto.
param_value_mode <- function(type, val) {
  auto <- length(val) != 1 || is.na(val) || val < 0
  if (type == "IDW") return(if (auto) "cv" else "fixed")
  if (auto) "gcv" else if (val == 0) "exact" else "fixed"
}

# Whether a typed fixed value can be stored or run: an IDW power inside the
# range Auto (CV) searches (0 to IDW_MAX_FINITE_POWER), a TPS lambda finite
# and above 0 (0 is the Exact mode).
idw_fixed_ok <- function(p) length(p) == 1 && isTRUE(is.finite(p) && p >= 0 && p <= IDW_MAX_FINITE_POWER)
tps_fixed_ok <- function(lambda) length(lambda) == 1 && isTRUE(is.finite(lambda) && lambda > 0)

# An encoded IDW power or TPS smoothing setting in words: "Auto (CV)",
# "Fixed p = 3.5", "Fixed p = 0 (equal weights)", "Auto (GCV)", "Exact (λ = 0)",
# "Fixed λ = 0.001".
param_setting_text <- function(type, val) {
  mode <- param_value_mode(type, val)
  if (type == "IDW") {
    if (mode == "cv") return("Auto (CV)")
    return(paste0("Fixed p = ", format_power(val), if (val == 0) " (equal weights)"))
  }
  switch(mode, gcv = "Auto (GCV)", exact = "Exact (λ = 0)", paste0("Fixed λ = ", format_sig(val)))
}

#' The lines under the Per locality panel of the IDW power or TPS smoothing:
#' whether the selected locality has a value of its own, which localities do,
#' and the setting the others run with (`global_text`), which the hidden All
#' localities control holds. `store` is rv$idw_factors or rv$tps_lambdas; only
#' values applied for `key` (the tuned variable and data subset) count.
per_locality_note <- function(type, store, target, locs, key, global_text, selected = NULL) {
  own <- list()
  for (l in locs) {
    v <- resolve_regional_param(store[[l]][[target]], key, NULL)
    if (!is.null(v)) own[[l]] <- param_setting_text(type, v)
  }
  sel_line <- if (length(selected) == 1 && selected %in% locs) {
    if (!is.null(own[[selected]])) {
      sprintf("%s runs with its own value, %s.", selected, own[[selected]])
    } else {
      sprintf("%s has no value of its own: it runs with the setting for all localities, %s, which the controls below start from.",
              selected, global_text)
    }
  }
  others <- setdiff(locs, names(own))
  listed <- paste0(names(own), ": ", unlist(own), collapse = "; ")
  rest <- if (!length(own)) {
    sprintf("No locality has a value of its own yet; all run with the setting for all localities, %s.", global_text)
  } else if (!length(others)) {
    sprintf("Every locality has a value of its own (%s).", listed)
  } else {
    sprintf("Own values: %s. %s run%s with the setting for all localities, %s.", listed,
            paste(others, collapse = ", "), if (length(others) == 1) "s" else "", global_text)
  }
  c(sel_line, rest)
}

#' The sidebar IDW panel's pooled figures: the displayed run's cross-validation
#' over all its localities, every residual weighted once (RMSE as
#' sqrt(sum(n * RMSE^2) / sum(n)), bias as the n-weighted mean error). NULL
#' unless the displayed run is an IDW run, so the panel never shows another
#' engine's metrics.
idw_panel_metrics <- function(disp_method, cv_metrics) {
  if (!identical(disp_method, "IDW") || !length(cv_metrics)) return(NULL)
  ns <- vapply(cv_metrics, function(x) as.numeric(x$n %||% 0), numeric(1))
  rmses <- vapply(cv_metrics, function(x) as.numeric(x$rmse %||% NA), numeric(1))
  mes <- vapply(cv_metrics, function(x) as.numeric(x$me %||% NA), numeric(1))
  # A locality with fewer than 2 predicted pairs has no RMSE to weight.
  ok <- is.finite(rmses) & is.finite(mes) & ns > 0
  if (!any(ok)) return(NULL)
  n <- sum(ns[ok])
  data.frame(Metric = c("CV RMSE (pooled)", "Bias, ME (pooled)"),
             Value = c(sqrt(sum(ns[ok] * rmses[ok]^2) / n), sum(ns[ok] * mes[ok]) / n))
}

#' The IDW power profile as an export sheet: each candidate named
#' (idw_power_text), its power as a number, empty at the nearest-neighbour
#' limit because a spreadsheet cell cannot hold Inf, its pooled CV RMSE and
#' whether it is within one standard error of the best (the panel's filled
#' points).
idw_profile_export_df <- function(profile) {
  out <- data.frame(Candidate = vapply(profile$p, idw_power_text, character(1)),
                    `Power (p)` = ifelse(is.finite(profile$p), profile$p, NA_real_),
                    `Pooled CV RMSE` = profile$rmse, check.names = FALSE)
  if (!is.null(profile$within_se)) out[["Within one SE of the best"]] <- profile$within_se
  out
}

# The fold powers of an Auto (CV) IDW fit as "median [min-max]"; NA without any.
idw_fold_power_text <- function(fold_p) {
  if (!length(fold_p)) return(NA_character_)
  paste0(format_power(stats::median(fold_p)), " [", format_power(min(fold_p)), "–",
         format_power(max(fold_p)), "]")
}

# Regional Parameters table for IDW/TPS, built from the run-committed
# per-locality snapshot (rv$disp$regional_params): the setting each surface ran
# with and the engine's own fit record (an IDW run's map and fold powers, a TPS
# run's fitted lambda and effective df). Never the live tuning store: a
# locality without a stored value ran with the setting for all localities,
# which the store does not hold. has_pre = FALSE means the displayed run mapped
# no prediction surface, so the Predicted column is dropped rather than filled
# with "N/A". The export states, for a selecting run, whose values the power or
# lambda was selected on: an unseparated Predicted surface takes the measured
# values' selection.
build_regional_params_df <- function(type, loc, regional_params, has_pre, export = FALSE) {
  if (is.null(regional_params) || length(regional_params) == 0) return(NULL)
  locs <- if (loc == "Total (Combined)") names(regional_params) else intersect(loc, names(regional_params))
  if (!length(locs)) return(NULL)
  if (export) {
    selected_on <- function(source, tgt) {
      if (is.null(source)) NA_character_
      else if (identical(source, "measured") || tgt == "act") "measured values" else "predicted values"
    }
    rows <- lapply(locs, function(l) {
      do.call(rbind, lapply(if (has_pre) c("act", "pre") else "act", function(tgt) {
        rp <- regional_params[[l]]
        out <- data.frame(Locality = l, Surface = if (tgt == "act") "Actual" else "Predicted")
        if (type == "IDW") {
          val <- rp[[paste0("idw_p_", tgt)]] %||% NA_real_
          fit <- rp[[paste0("idw_fit_", tgt)]]
          fp <- as.numeric(fit$fold_p)
          auto <- identical(param_value_mode("IDW", val), "cv")
          map_p <- as.numeric(fit$p %||% if (auto) NA_real_ else val)
          # A spreadsheet cell cannot hold Inf (openxlsx writes it empty): the
          # nearest-neighbour limit is named in Selected and counted over the
          # folds, and its numeric cells stay empty.
          fin <- function(x) if (length(x) == 1 && is.finite(x)) x else NA_real_
          out$Mode <- if (auto) "Auto (CV)" else "Fixed"
          out$Selected <- if (is.na(map_p)) NA_character_ else idw_power_text(map_p)
          out[["Selected on"]] <- if (auto) selected_on(fit$select_source, tgt) else NA_character_
          out[["Not searched (reason)"]] <- fit$skipped %||% NA_character_
          out[["Power (map)"]] <- fin(map_p)
          out[["Fold power (median)"]] <- if (length(fp)) fin(stats::median(fp)) else NA_real_
          out[["Fold power (min)"]] <- if (length(fp)) fin(min(fp)) else NA_real_
          out[["Fold power (max)"]] <- if (length(fp)) fin(max(fp)) else NA_real_
          out[["Folds at the nearest-neighbour limit"]] <- if (length(fp)) sum(is.infinite(fp)) else NA_integer_
          # How well the data separate the powers (idw_flatness_note).
          w <- fit$profile$within_se
          two <- which(fit$profile$p == 2)
          out[["Powers within one SE of the best"]] <- if (length(w)) sum(w) else NA_integer_
          out[["p = 2 within one SE"]] <- if (length(w) && length(two) == 1) w[two] else NA
        } else {
          val <- rp[[paste0("tps_lambda_", tgt)]] %||% -1
          fit <- rp[[paste0("tps_fit_", tgt)]]
          mode <- param_value_mode("TPS", val)
          auto <- identical(mode, "gcv")
          out$Mode <- switch(mode, gcv = "Auto (GCV)", exact = "Exact", "Fixed")
          out[["Selected on"]] <- if (auto && isTRUE(is.finite(fit$lambda))) selected_on(fit$gcv_source, tgt) else NA_character_
          out$Lambda <- as.numeric(fit$lambda %||% if (auto) NA_real_ else val)
          out[["Effective df"]] <- as.numeric(fit$eff_df %||% NA_real_)
          # Where GCV's minimum sat on its grid, and at the least-smoothing end
          # exact interpolation's cross-validation beside the run's
          # (tps_gcv_end, apply_TPS).
          out[["GCV minimum"]] <- if (!auto || !isTRUE(is.finite(fit$lambda))) NA_character_ else {
            switch(fit$gcv_end %||% "", plane = "smoothest end (least-squares plane)",
                   interpolation = "least-smoothing end", "inside the grid")
          }
          out[["Exact (λ = 0) CV RMSE"]] <- as.numeric(fit$exact_cv_rmse %||% NA_real_)
          out[["Run CV RMSE"]] <- as.numeric(fit$run_cv_rmse %||% NA_real_)
        }
        out
      }))
    })
    return(do.call(rbind, rows))
  }
  # One compact wording for every row: the mode, then the power or lambda
  # (equal weights and the nearest-neighbour limit named). A row without a fit
  # record, whose surface was not produced, states the setting it ran with.
  idw_short <- function(p) switch(idw_power_limit(p) %||% "", equal_weights = "0 (equal weights)",
                                  nearest_neighbour = "∞ (nearest neighbour)", format_power(p))
  fmt <- function(l, tgt) {
    key <- paste0(if (type == "IDW") "idw_p_" else "tps_lambda_", tgt)
    val <- regional_params[[l]][[key]]
    if (is.null(val)) return("N/A")
    mode <- param_value_mode(type, val)
    if (type == "IDW") {
      fit <- regional_params[[l]][[paste0("idw_fit_", tgt)]]
      if (is.null(fit)) return(if (mode == "cv") "Auto (CV)" else paste0("Fixed: ", idw_short(val)))
      sel <- idw_short(fit$p)
      if (identical(fit$mode, "fixed")) return(paste0("Fixed: ", sel))
      folds <- idw_fold_power_text(fit$fold_p)
      return(paste0("Auto (CV): ", sel,
                    if (!is.null(fit$skipped)) " (not searched)"
                    else if (!is.na(folds)) paste0(" (folds ", folds, ")")))
    }
    fit <- regional_params[[l]][[paste0("tps_fit_", tgt)]]
    if (is.null(fit)) return(switch(mode, gcv = "Auto (GCV)", exact = "Exact: 0", paste0("Fixed: ", format_sig(val))))
    if (!isTRUE(is.finite(fit$lambda))) return("TPS fit unavailable")
    sprintf("%s: %s (df %s)", switch(mode, gcv = "Auto (GCV)", exact = "Exact", "Fixed"),
            format_sig(fit$lambda), format_sig(fit$eff_df))
  }
  param_lab <- if (type == "IDW") "Power (p)" else "Lambda"
  if (loc == "Total (Combined)") {
    locs <- names(regional_params)
    out <- data.frame(
      Locality = locs,
      Param = param_lab,
      Actual = unname(vapply(locs, fmt, character(1), tgt = "act"))
    )
    if (has_pre) out$Predicted <- unname(vapply(locs, fmt, character(1), tgt = "pre"))
    return(out)
  }
  if (!loc %in% names(regional_params)) return(NULL)
  out <- data.frame(
    Param = param_lab,
    Actual = fmt(loc, "act")
  )
  if (has_pre) out$Predicted <- fmt(loc, "pre")
  out
}

# ── Shared result-table builders ─────────────────────────────────────────────
# One definition per result table, consumed by BOTH the on-screen card and the
# export registry. Before these existed each export re-derived its own subset
# of the metrics under its own labels, so an exported workbook reported fewer
# statistics than the screen it was taken from. Values stay NUMERIC: a metric
# written to a worksheet as text cannot be sorted, charted or recomputed.

# Column order and labels of the Model Performance table: CV_METRIC_LABELS,
# defined in spatial_metrics.R beside CV_REPEAT_METRICS, which it extends.

# One wide row of cross-validation metrics from a perform_cv() result.
# `cv_design` is the fold plan the row was scored under (applied_cv_plan()); it
# is a column of its own here because the on-screen Source string that carries
# it is not machine-readable. `cv_info` carries the same row's cross-validation
# population: which samples it scored (`population`), the id that lets two
# archived runs be checked for having scored the same rows in the same folds
# (`pop_id`), and what the folds re-estimated (`refit`). Coverage comes off
# perform_cv: metrics are computed on the predicted samples, so a row below
# 100% describes fewer samples than the design asked for. `moran` is the
# row's moran_reading(): under a contiguous fold design (Spatial Block CV,
# kNNDM spatial folds) the p-value is not reported
# and `Moran Context` names what the statistic measures there, so a reader of
# the file alone cannot take it for the random-CV quantity. `Target spans
# zero` explains an NA in NRMSE (mean) and SMAPE, which do not apply there.
cv_metrics_export_df <- function(res, source_label, cv_design = NA_character_,
                                 cv_info = NULL, moran = moran_reading(NA_character_)) {
  if (is.null(res)) return(NULL)
  cov_pct <- res$coverage %||% NA_real_
  out <- data.frame(Source = source_label,
                    `CV Design` = cv_design,
                    `CV Population` = as.character(cv_info$population %||% NA_character_),
                    `CV Population ID` = as.character(cv_info$pop_id %||% NA_character_),
                    `CV Refit` = as.character(cv_info$refit %||% NA_character_),
                    `n expected` = as.integer(res$n_expected %||% NA),
                    `n predicted` = as.integer(res$n %||% NA),
                    `Coverage (%)` = if (is.na(cov_pct)) NA_real_ else 100 * as.numeric(cov_pct),
                    `Target spans zero` = isTRUE(res$signed_target),
                    check.names = FALSE, stringsAsFactors = FALSE)
  for (k in names(CV_METRIC_LABELS)) {
    v <- res[[k]]
    out[[unname(CV_METRIC_LABELS[[k]])]] <-
      if (is.null(v) || length(v) != 1) NA_real_ else as.numeric(v)
  }
  if (!moran$report_p) out[["Moran p"]] <- NA_real_
  out[["Moran Context"]] <- moran$context
  out
}

# The fold design one CV metrics row was scored under: the plan the strategy
# resolves to at this n (resolve_cv_plan), unless the metrics record otherwise:
# a Spatial Block request fell back to random folds because k-means failed, or
# a kNNDM request chose between random and spatial folds (`knndm_branch`,
# perform_cv) - "knndm_random" / "knndm_spatial", or random folds with the
# reason where kNNDM had no domain or no valid partition. Returns
# resolve_cv_plan()'s list, retyped and relabelled in those cases.
applied_cv_plan <- function(n_obs, strategy = "auto", res = NULL) {
  plan <- resolve_cv_plan(strategy, n_obs)
  if (identical(plan$type, "block") && isTRUE(res$block_fallback)) {
    plan <- list(type = "random_kfold", k = plan$k,
                 label = "Random 10-fold CV [Spatial Block clustering failed]")
  }
  if (identical(plan$type, "knndm")) {
    plan <- switch(res$knndm_branch %||% NA_character_,
      random   = list(type = "knndm_random", k = plan$k, label = "kNNDM CV [random folds]"),
      spatial  = list(type = "knndm_spatial", k = plan$k, label = "kNNDM CV [spatial folds]"),
      fallback = list(type = "random_kfold", k = plan$k,
                      label = "Random 10-fold CV [kNNDM: no valid spatial partition]"),
      none     = list(type = "random_kfold", k = plan$k,
                      label = "Random 10-fold CV [kNNDM: no prediction domain]"),
      plan)
  }
  plan
}

# Fold designs whose held-out samples form contiguous spatial groups: a whole
# group is withheld at once, so its residuals share one prediction condition.
CONTIGUOUS_CV_DESIGNS <- c("block", "knndm_spatial")

# How the residual Moran's I of a CV metrics row is read, from the fold
# designs behind it (one type for a locality; one per pooled locality for a
# "Total (Combined)" row). Under ordinary CV it diagnoses model-error
# structure and its p-value is reported. Under a contiguous design (Spatial
# Block CV, kNNDM spatial folds) the pooled out-of-fold residuals also inherit
# the fold geometry and a shared extrapolation condition inside each withheld
# group; spdep's reference distribution knows neither, so the statistic is
# reported as residual clustering of that design and the p-value is not. A
# pool of localities scored under different designs is read the ordinary way
# and flagged `mixed`. `block` means a contiguous design, and `design` names it.
moran_reading <- function(plan_types) {
  plan_types <- unique(plan_types[!is.na(plan_types)])
  block <- length(plan_types) == 1 && plan_types %in% CONTIGUOUS_CV_DESIGNS
  mixed <- length(plan_types) > 1
  kn <- block && identical(plan_types, "knndm_spatial")
  list(block = block, mixed = mixed,
       design = if (kn) "kNNDM spatial folds" else if (block) "Spatial Block CV" else NA_character_,
       label = if (!block) "Moran's I" else if (kn) "Spatial-fold residual clustering" else "Block-CV residual clustering",
       context = if (kn) {
         "spatial-fold residual clustering (kNNDM spatial folds); p not reported"
       } else if (block) {
         "block-CV residual clustering (transfer/extrapolation error); p not reported"
       } else if (mixed) {
         "model-error structure; pooled residuals mix fold designs"
       } else "model-error structure",
       report_p = !block)
}

# The CV Distance Match record (cv_distance_summary) as an exportable frame:
# one row per percentile of the three distance distributions the panel draws,
# with the two W values, the sample count and the map locations as constant
# columns, so the sheet alone carries what the figure shows.
cv_distance_export_df <- function(design) {
  if (is.null(design)) return(NULL)
  u <- if (is.na(design$units %||% NA_character_)) "map units" else design$units
  cols <- list(design$probs, design$map, design$cv, design$sample,
               design$W_cv, design$W_random, as.integer(design$n), as.integer(design$n_domain))
  names(cols) <- c("Cumulative share",
                   sprintf("Map cell to nearest sample (%s)", u),
                   sprintf("Held-out to nearest training sample, this CV (%s)", u),
                   sprintf("Sample to nearest other sample (%s)", u),
                   sprintf("W, this CV (%s)", u),
                   sprintf("W, %s (%s)", design$reference, u),
                   "Samples", "Map cells (points inside the boundary)")
  as.data.frame(cols, check.names = FALSE)
}

# Mean and SD across fold realizations (repeated CV) as an exportable frame:
# one row per metric, with the mean and the SD in columns of their own rather
# than fused into the "m ± s" string the screen shows.
cv_repeats_export_df <- function(summ, source_label) {
  if (is.null(summ)) return(NULL)
  keys <- names(CV_REPEAT_METRICS)
  data.frame(
    Source = source_label,
    `Fold realizations` = as.integer(summ$n_repeats),
    # Realizations can differ in how many samples they managed to predict, so
    # the spread of the counts travels beside the spread of the metrics.
    `n expected` = as.integer(summ$n_expected %||% NA),
    `min n predicted` = as.integer(summ$n_min %||% NA),
    `max n predicted` = as.integer(summ$n_max %||% NA),
    n = as.integer(summ$n),
    Metric = unname(CV_REPEAT_METRICS[keys]),
    Mean = vapply(keys, function(k) as.numeric(summ$mean[[k]] %||% NA_real_), numeric(1)),
    SD = vapply(keys, function(k) as.numeric(summ$sd[[k]] %||% NA_real_), numeric(1)),
    check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL
  )
}

# Metric dictionary for an externally supplied prediction column (uploaded ML
# predictions). perform_cv() owns every definition, so this table and Model
# Performance cannot drift apart. Two documented departures from Model
# Performance (Scientific Guide 5): MBE is reported predicted-minus-observed,
# and NMAE has no CV counterpart, so it is computed here under the same
# sign-crossing rule as NRMSE (mean). moran = FALSE: an uploaded prediction
# column carries no CV residual field. `Note` names a value that is NA because
# the metric does not apply (the target spans zero), so the exported sheet
# can tell it from one that could not be computed.
pred_perf_df <- function(obs, pre) {
  ok <- !is.na(obs) & !is.na(pre)
  obs <- obs[ok]; pre <- pre[ok]
  if (length(obs) < 3) return(NULL)
  m <- perform_cv(data.frame(var1.observed = obs, var1.pred = pre), moran = FALSE)
  signed <- isTRUE(m$signed_target)
  mean_v <- mean(obs)
  mae <- mean(abs(obs - pre))
  nmae <- if (!signed && is.finite(mae) && abs(mean_v) > 0) (mae / abs(mean_v)) * 100 else NA_real_
  metric <- c("R² (NSE/Traditional)", "R² (Correlation)", "RMSE", "NRMSE (mean, %)",
              "NRMSE (SD)", "MAE", "NMAE (mean, %)", "MBE (ML pred - observed)",
              "Lin's CCC (Agree)", "RPD (Precision)", "RPIQ", "SMAPE (%)", "n")
  ratio_scale_only <- c("NRMSE (mean, %)", "NMAE (mean, %)", "SMAPE (%)")
  data.frame(
    Metric = metric,
    Value = as.numeric(c(m$nse, m$r2, m$rmse, m$nrmse_mean, m$nrmse_sd, m$mae, nmae,
                         -m$me, m$ccc, m$rpd, m$rpiq, m$smape, m$n)),
    Note = ifelse(signed & metric %in% ratio_scale_only, SIGNED_TARGET_NOTE, ""),
    stringsAsFactors = FALSE
  )
}

# The uploaded-prediction card and the Model Performance table can score
# different samples, and neither number is wrong. The model's point set drops
# rows with no measured target FIRST and deduplicates co-located points after
# (dedup_valid_points); the displayed set deduplicates first and is then
# filtered to rows carrying both values, so a co-located pair whose measurement
# sits on one member and whose prediction sits on the other contributes a
# different member to each. Returns NULL when the two agree - a note that fires
# on every run is noise.
pred_pop_note <- function(card_n, model_n) {
  if (!isTRUE(is.finite(card_n)) || !isTRUE(is.finite(model_n))) return(NULL)
  if (isTRUE(card_n == model_n)) return(NULL)
  paste0("Scored on the displayed sample locations carrying both a measured and a predicted ",
         "value (n = ", card_n, "). This is not the model's cross-validation population (n = ",
         model_n, "), which is selected on the measured values (and, for the covariate engines ",
         "or OK Comparable, on the covariates), not on the uploaded predictions.")
}

# Class-agreement table from a compute_agreement_metrics() result.
agreement_metrics_df <- function(ag) {
  if (is.null(ag) || !is.null(ag$status)) return(NULL)
  data.frame(
    Metric = c("Overall Accuracy", "Balanced Accuracy", "Off-by-one Accuracy",
               "Matthews Corr. Coef. (MCC)", "Kappa (Unweighted)",
               "Weighted Kappa (Linear)"),
    Value = as.numeric(c(ag$accuracy, ag$bal_accuracy, ag$off_by_one,
                         ag$mcc, ag$kappa, ag$kappa_linear)),
    stringsAsFactors = FALSE
  )
}

# Display formatting for every statistic the app prints: four significant
# digits, so a small value never reads as zero. Zero prints as "0", a whole
# number prints exactly (a count must not lose digits), a value of 1000 or more
# keeps every integer digit (123456.7 ha reads 123457, not 123500), values
# below 1e-4 in magnitude switch to scientific notation, everything else is
# fixed notation at four significant digits. Returns character, NA for a
# non-finite value.
# mnFormatSig() (format_sig_js(), ui_components.R) applies the same rule in
# the browser to numeric table columns, so the two must change together; they
# can differ in the last digit only at an exact decimal tie.
format_sig <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- rep(NA_character_, length(x))
  ok <- is.finite(x)
  whole <- ok & x == round(x) & abs(x) < 1e15
  big <- ok & !whole & abs(x) >= 1000
  small <- ok & !whole & abs(x) < 1e-4
  rest <- ok & !whole & !big & !small
  # + 0 turns a negative zero into 0, which formatC would print as "-0".
  out[whole] <- formatC(x[whole] + 0, format = "f", digits = 0)
  out[big] <- formatC(round(x[big]) + 0, format = "f", digits = 0)
  out[small] <- formatC(x[small], format = "e", digits = 3)
  out[rest] <- trimws(formatC(signif(x[rest], 4), format = "fg", digits = 4))
  out
}

# Rows of a map point's pop-up table. Labels come from the variable list and
# values from the uploaded table, so both are escaped before they enter the
# HTML; a number is shown at four significant digits, anything else as text.
popup_value_row <- function(label, val) {
  is_num <- !is.null(val) && (is.numeric(val) || !is.na(suppressWarnings(as.numeric(val))))
  val_str <- if (is_num) format_sig(as.numeric(val)) else as.character(val %||% "N/A")
  paste0("<tr><td style='padding: 3px;'>", htmltools::htmlEscape(as.character(label)),
         "</td><td style='padding: 3px; text-align: right;'>", htmltools::htmlEscape(val_str),
         "</td></tr>")
}
popup_group_row <- function(title) {
  paste0("<tr style='background-color: var(--mn-surface-2);'><td colspan='2'><b>",
         htmltools::htmlEscape(as.character(title)), "</b></td></tr>")
}

# The classes built on inner breaks: every break with the open ends -Inf/Inf,
# and terra's reclassification matrix with one [low, high) row per class. The
# map and the class areas classify with it at right = FALSE, so a value equal
# to a break falls in the class above it.
class_breaks_matrix <- function(brks_inner) {
  brks <- sort(unique(c(-Inf, brks_inner, Inf)))
  n <- length(brks) - 1L
  list(brks = brks, n_c = n, rcl_mat = cbind(brks[-length(brks)], brks[-1], seq_len(n)))
}

# Range text of the classes [b_i, b_(i+1)) that `brks` (outer ends -Inf/Inf)
# define, at the display precision every result uses. The top class holds its
# lower break, hence ">=".
class_legend_labels <- function(brks) {
  n <- length(brks) - 1L
  if (n < 2L) return(rep("All values", max(n, 0L)))
  inner <- brks[2:n]
  b <- format_sig(inner)
  # format_sig keeps whole units from 1000 up, so narrow classes of a large
  # variable (a flat field's elevation in metres) could print two breaks alike;
  # add significant digits until they differ.
  d <- 5L
  while (anyDuplicated(b) && d <= 15L) {
    b <- trimws(formatC(inner, format = "fg", digits = d))
    d <- d + 1L
  }
  vapply(seq_len(n), function(i) {
    if (i == 1L) paste("<", b[1])
    else if (i == n) paste("≥", b[n - 1L])
    else paste(b[i - 1L], "-", b[i])
  }, character(1))
}

# The lines under the Supervised limit boxes for a class_limit_defaults()
# result: where the prefilled limits come from, with the method, unit and
# source of a reference, and why a reference was not used when one exists.
class_limit_note <- function(d, var_unit = "") {
  ref <- d$ref
  if (identical(d$source, "reference")) {
    lim <- format_sig(ref$limits)
    out <- sprintf("Reference limits: %s, %s, %s. Low < %s ≤ Moderate < %s ≤ High.",
                   ref$method, ref$unit, ref$source, lim[1], lim[2])
    if (identical(d$unit_status, "empty")) {
      out <- c(out, sprintf("No unit is recorded for this variable; the limits assume %s in %s.",
                            ref$method, ref$unit))
    }
    return(out)
  }
  out <- "Data quantiles (not agronomic limits)."
  if (!is.null(ref) && identical(d$unit_status, "different")) {
    out <- c(out, sprintf("The reference limits (%s) are in %s; this variable is recorded in %s.",
                          ref$method, ref$unit, trimws(var_unit)))
  } else if (!is.null(ref)) {
    out <- c(out, sprintf("Reference limits for %s (%s) define three classes; set Classes to 3 to use them.",
                          ref$method, ref$source))
  }
  out
}

# Whether `label` already names the unit `u`: a case-insensitive match that is
# not part of a longer word, so "K (mg/kg)" names "mg/kg" while "Organic
# matter" does not name "g".
unit_in_label <- function(label, u) {
  lab <- tolower(as.character(label)); uu <- tolower(u)
  hits <- gregexpr(uu, lab, fixed = TRUE)[[1]]
  if (hits[1] < 0) return(FALSE)
  n <- nchar(uu)
  any(vapply(hits, function(s) {
    before <- if (s > 1) substr(lab, s - 1, s - 1) else ""
    after <- substr(lab, s + n, s + n)
    !grepl("[[:alnum:]]", before) && !grepl("[[:alnum:]]", after)
  }, logical(1)))
}

# Legend title of a map layer, one composition for the Map Viewer and the
# exported figure: the variable and its unit, prefixed for an uncertainty or
# residual layer. A label that already names its unit ("K (mg/kg)") is not
# given it twice on the value and SE layers; the variance layer states the
# squared unit whatever the label says.
map_legend_title <- function(label, unit = "", layer = "value") {
  u <- if (length(unit) == 1 && !is.na(unit) && nzchar(trimws(unit))) trimws(unit) else ""
  shown <- if (nzchar(u) && unit_in_label(label, u)) "" else u
  switch(layer,
    se = paste0("SE: ", label, if (nzchar(shown)) paste0(" ", shown) else ""),
    var = paste0("Variance: ", label, if (nzchar(u)) paste0(" (", u, ")^2") else " (squared units)"),
    resid = paste("Resid:", label),
    point_resid = paste("Point Resid:", label),
    trimws(paste(label, shown)))
}

# What a cancelled run's record says wherever its results would be.
RUN_CANCELLED_NOTE <- paste(
  "This run was cancelled before it produced results. The configuration shown",
  "is what was requested; no surface, metrics or exports were produced.")

# One phrase for the NA a signed target gets in NRMSE (mean), NMAE and SMAPE:
# the export's Note column and the screen's footnote say the same thing.
SIGNED_TARGET_NOTE <- "not reported: observed values span zero"

# summary() of one or two numeric vectors as a tidy frame, values left numeric.
# A FIXED row set keeps the two columns in step: summary() appends an "NA's"
# element only for a vector that actually has missing values, so building the
# second column by assignment failed outright ("replacement has 6 rows, data
# has 7") whenever exactly one of the two sides carried them. The
# missing-value row is shown only when there is something to report. A second
# vector with no observed value keeps its column (statistics NA, the NA's row
# counting it), so an empty predicted side reads as empty rather than absent.
summary_stats_df <- function(a, b = NULL, labels = c("Value", "Predicted")) {
  rows <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "NA's")
  stat_rows <- rows[rows != "NA's"]
  col <- function(x) {
    if (is.null(x) || length(x) == 0) return(NULL)
    x <- as.numeric(x)
    v <- stats::setNames(rep(NA_real_, length(rows)), rows)
    if (any(!is.na(x))) {
      s <- summary(x)
      keep <- intersect(names(s), stat_rows)
      v[keep] <- as.numeric(s[keep])
    }
    v[["NA's"]] <- sum(is.na(x))
    v
  }
  ca <- col(a)
  if (is.null(ca) || all(is.na(ca[stat_rows]))) return(NULL)
  cb <- col(b)
  no_na <- function(v) is.null(v) || v[["NA's"]] == 0
  keep <- if (no_na(ca) && no_na(cb)) stat_rows else rows
  out <- data.frame(Metric = keep, V = unname(ca[keep]), stringsAsFactors = FALSE)
  names(out)[2] <- labels[1]
  if (!is.null(cb)) out[[labels[2]]] <- unname(cb[keep])
  out
}

# The two vectors a Descriptive Statistics table summarises, shared by the card
# and its export so both describe ONE sample: the uploaded rows of `localities`
# (every row when NULL). Not the run's point set, which is coordinate-
# deduplicated and gives a different n, mean and quartiles wherever co-located
# samples exist. The predicted side is the column the displayed run mapped
# (_ss for a Single-Split run, never a fallback to the other), and only when
# that run mapped predictions. NULL when the displayed variable is not in `df`.
stats_table_vectors <- function(df, meta, loc_col, localities = NULL) {
  if (is.null(df) || is.null(meta)) return(NULL)
  if (!is.null(localities) && !is.null(loc_col) && loc_col %in% names(df)) {
    df <- df[df[[loc_col]] %in% localities, , drop = FALSE]
  }
  subset_col <- find_subset_column(names(df))
  if (!is.null(meta$subset) && meta$subset != "all" && !is.na(subset_col)) {
    df <- df[!is.na(df[[subset_col]]) & df[[subset_col]] == meta$subset, , drop = FALSE]
  }
  if (!is_valid_col_ref(meta$actual) || !meta$actual %in% names(df)) return(NULL)
  has_pred <- isTRUE(meta$comp_mode) ||
    (!is.null(meta$value_type) && !identical(meta$value_type, "actual"))
  pv_col <- if (identical(meta$value_type, "pred_ss")) meta$pred_ss else meta$pred
  list(act = df[[meta$actual]],
       pre = if (has_pred && is_valid_col_ref(pv_col) && pv_col %in% names(df)) df[[pv_col]] else NULL)
}

# ── Fitted variogram parameters ──────────────────────────────────────────────
# The reported parameters of one fitted gstat variogramModel. Sill is the TOTAL
# sill (nugget + partial sills) and Structural Dependency is the partial-sill
# share of it in percent, so 100% is a pure spatial structure and 0% a pure
# nugget (NA when the sill is zero).
#
# Range (a) is gstat's range PARAMETER, which is not comparable across model
# families: the distance at which the model reaches 95% of its sill is a for
# Sph, 3a for Exp, sqrt(3)a for Gau and about 4.75a for Mat nu = 1.5
# (.vgm_practical_range_factor, spatial_vgm.R). Practical Range is that
# distance, so rows fitted with different families compare on it. Kappa is
# reported for a Matern structure only (it is inert for the others). A nested
# model names every structure, and its ranges are those of the structure that
# reaches its sill last. A pure-nugget model reports nugget and sill with no
# range. Values are unrounded; displays format them.
#
# `max_lag` and `sill_resolved` come from the fit's own diagnostics: a sill the
# empirical variogram never reached is MODEL-EXTRAPOLATED, so the structural
# dependency derived from it is not identified by the data. NA for a supplied
# (manual) model, where the user chose the parameters and no empirical support
# was recorded - "not reliably identified" is a statement about a FIT, not
# about a model the user applied.
vgm_params_row <- function(f) {
  out <- list(model = NA_character_, kappa = NA_real_, nugget = NA_real_,
              sill = NA_real_, range = NA_real_, practical_range = NA_real_,
              sdep = NA_real_, max_lag = NA_real_, sill_resolved = NA,
              target_degenerate = FALSE)
  if (is.null(f) || NROW(f) == 0) return(out)
  mdl <- as.character(f$model)
  is_nug <- mdl == "Nug"
  st <- which(!is_nug)
  out$nugget <- sum(f$psill[is_nug])
  out$sill <- sum(f$psill)
  out$model <- if (length(st)) paste(mdl[st], collapse = " + ") else "Nug"
  if (length(st)) {
    prac <- vapply(st, function(i) {
      f$range[i] * .vgm_practical_range_factor(mdl[i], f$kappa[i])
    }, numeric(1))
    lead <- st[which.max(prac)]
    out$range <- f$range[lead]
    out$practical_range <- f$range[lead] * .vgm_practical_range_factor(mdl[lead], f$kappa[lead])
    if (identical(mdl[lead], "Mat")) out$kappa <- f$kappa[lead]
  }
  if (isTRUE(out$sill > 0)) out$sdep <- ((out$sill - out$nugget) / out$sill) * 100
  d <- attr(f, "vgm_diagnostics")
  if (!is.null(d$max_lag)) out$max_lag <- suppressWarnings(as.numeric(d$max_lag)[1])
  out$sill_resolved <- if (!is.null(d$sill_resolved)) {
    as.logical(d$sill_resolved)[1]
  } else if (is.na(out$max_lag) || is.na(out$practical_range)) {
    NA
  } else {
    out$practical_range <= out$max_lag
  }
  out$target_degenerate <- isTRUE(d$target_degenerate)
  out
}

# One-line plot subtitle of a variogram model, read from the record the
# Variogram Parameters card reports (vgm_params_row): what the model is (a
# fit, a flawed fit, the heuristic stand-in when nothing could be fitted, or a
# model the user applied), its family (with the smoothness of a Matern),
# nugget, partial sill, the range parameter a and the practical range, which
# is model-extrapolated where the sill lies beyond the longest lag and says
# so. NULL for no model.
vgm_fit_subtitle <- function(fit) {
  p <- vgm_params_row(fit)
  if (is.na(p$model)) return(NULL)
  lead <- if (identical(attr(fit, "monolith_source"), "manual")) {
    "Applied manual model: "
  } else {
    switch(vgm_fit_status(fit),
           fit_failed = , heuristic_fallback = "Heuristic fallback, not fitted: ",
           singular_selected = "Fitted (singular/non-converged): ",
           "Fitted: ")
  }
  if (identical(p$model, "Nug")) return(paste0(lead, "pure nugget (Nugget: ", format_sig(p$nugget), ")"))
  paste0(lead, p$model, if (!is.na(p$kappa)) paste0(", kappa = ", format_sig(p$kappa)),
         " (Nugget: ", format_sig(p$nugget), ", Partial Sill: ", format_sig(p$sill - p$nugget),
         ", Range (a): ", format_sig(p$range), ", Practical Range: ", format_sig(p$practical_range),
         if (identical(p$sill_resolved, FALSE)) {
           paste0(", model-extrapolated beyond the longest lag ", format_sig(p$max_lag))
         }, ")")
}

# The Variogram Parameters cell text when the target carries no usable
# variance: the fitted sill is then numerical noise, not an estimate.
VGM_DEGENERATE_NOTE <- paste(
  "Target has no usable variance in this locality. No spatial structure can be",
  "estimated; the fitted parameters below the numerical noise floor are not reported.")

# Screen flavour: "NA" for an absent value, the smoothness beside a Matern
# model name and the percent sign on Structural Dependency, all character so
# one column can mix the model name with numbers. Where the sill is not
# resolved inside the observed lag range the Structural Dependency cell states
# that instead of a percentage, and carries the model-extrapolated figure in
# its tooltip - a number a reader would otherwise take as measured. Cells are
# HTML, so the card renders with escape = FALSE.
.vgm_params_chr <- function(f) {
  p <- vgm_params_row(f)
  if (is.na(p$model)) return(rep("NA", 6))
  chr <- function(x) if (is.na(x)) "NA" else format_sig(x)
  if (isTRUE(p$target_degenerate)) return(rep("Not estimated", 6))
  tip <- function(txt, title) {
    paste0("<span title='", htmltools::htmlEscape(title, attribute = TRUE),
           "' style='cursor: help; text-decoration: underline dotted 1px;'>", txt, "</span>")
  }
  unresolved <- identical(p$sill_resolved, FALSE)
  qualifier <- paste0("The fitted sill is not reached within the observed lag range (max lag ",
                      chr(p$max_lag), ").")
  sdep_cell <- if (unresolved) {
    tip("Not reliably identified",
        paste0("Model-extrapolated: ",
               if (is.na(p$sdep)) "NA" else paste0(format_sig(p$sdep), "%"), ". ", qualifier))
  } else if (is.na(p$sdep)) "NA" else paste0(format_sig(p$sdep), "%")
  c(if (is.na(p$kappa)) p$model else paste0(p$model, " (kappa = ", format_sig(p$kappa), ")"),
    chr(p$nugget), chr(p$sill), chr(p$range),
    if (unresolved) tip(chr(p$practical_range), qualifier) else chr(p$practical_range),
    sdep_cell)
}

# The Variogram Parameters card and export title: RK and RFK krige the
# residuals of their trend, so their fits are residual variograms.
vgm_params_title <- function(method) {
  if ((method %||% "") %in% c("RK", "RFK")) "Residual Variogram Parameters" else "Variogram Parameters"
}

# What a residual-kriging fit describes, per vgm_params_export_df's `of`.
VGM_OF_RESIDUALS <- "Residuals"
VGM_OF_FALLBACK <- "Measured values (OK fallback)"

# The Variogram Parameters card. "Total (Combined)" lists every fitted
# locality/target (variograms are fitted per locality); a named locality is
# transposed so Actual and Predicted sit side by side. NULL when nothing in the
# store was fitted, which sci_dt() renders as the empty state. `of` is passed
# to the Total listing (vgm_params_export_df).
vgm_params_table_df <- function(v_fit_list, loc, of = NULL) {
  if (identical(loc, "Total (Combined)")) {
    return(vgm_params_export_df(v_fit_list, of = of))
  }
  f_a <- v_fit_list[[paste0(loc, "_act")]]
  f_p <- v_fit_list[[paste0(loc, "_pre")]]
  if (is.null(f_a) && is.null(f_p)) return(NULL)
  # A target with no usable variance produces a sill 60 orders of magnitude
  # below the data and a "Structural Dependence 100%" manufactured from
  # numerical noise. Report the cause instead of the parameters.
  fits <- Filter(Negate(is.null), list(f_a, f_p))
  if (all(vapply(fits, vgm_target_degenerate, logical(1)))) {
    return(data.frame(Status = VGM_DEGENERATE_NOTE, stringsAsFactors = FALSE))
  }
  res <- data.frame(Param = c("Model", "Nugget", "Sill", "Range (a)",
                              "Practical Range", "Structural Dep."),
                    Actual = .vgm_params_chr(f_a), stringsAsFactors = FALSE)
  # Predicted column only when a predicted-surface fit exists: an all-"NA"
  # column for a run that never mapped predictions is just noise.
  if (!is.null(f_p)) res$Predicted <- .vgm_params_chr(f_p)
  res
}

# Export flavour: tidy, one row per fitted locality/target, numeric columns.
# `of`, named like `v_fit_list`, adds a "Variogram Of" column saying what each
# fit describes (a residual-kriging run's VGM_OF_RESIDUALS / VGM_OF_FALLBACK).
vgm_params_export_df <- function(v_fit_list, locs = NULL, of = NULL) {
  if (is.null(v_fit_list) || length(v_fit_list) == 0) return(NULL)
  if (is.null(locs)) locs <- unique(sub("_(act|pre)$", "", names(v_fit_list)))
  rows <- list()
  for (l in locs) {
    for (tgt in c("act", "pre")) {
      key <- paste0(l, "_", tgt)
      f <- v_fit_list[[key]]
      if (is.null(f)) next
      p <- vgm_params_row(f)
      rows[[length(rows) + 1]] <- data.frame(
        Locality = l,
        Target = if (tgt == "act") "Actual" else "Predicted",
        `Variogram Of` = if (is.null(of)) NA_character_ else unname(of[key]),
        Model = p$model, Kappa = p$kappa, Nugget = p$nugget, Sill = p$sill,
        `Range (a)` = p$range, `Practical Range` = p$practical_range,
        # A file cannot carry a tooltip, so the numeric structural dependency
        # stays and the two columns that qualify it travel beside it.
        `Structural Dep. (%)` = p$sdep,
        `Max Lag` = p$max_lag, `Sill Resolved` = p$sill_resolved,
        check.names = FALSE, stringsAsFactors = FALSE)
    }
  }
  if (length(rows) == 0) return(NULL)
  out <- do.call(rbind, rows)
  if (is.null(of)) out[["Variogram Of"]] <- NULL
  out
}

# Manual variogram sliders for one locality and target. Bounds come from the
# data being tuned (its variance and bounding-box diagonal) and widen only when
# the stored model lies beyond them, so applying a model never narrows them.
# Nugget and partial sill share one axis. Steps are 1/200 of each axis at two
# significant digits. ion.rangeSlider reads its decimal count from the step's
# JavaScript string, which switches to exponent notation below 1e-6, so
# `step_ok` is FALSE when the sill axis cannot be represented (variance below
# about 1e-4). NULL without a positive variance and extent.
manual_vgm_slider_spec <- function(variance, max_dist, fit = NULL) {
  if (!isTRUE(is.finite(variance) && variance > 0 && is.finite(max_dist) && max_dist > 0)) {
    return(NULL)
  }
  model <- NULL
  nugget <- 0
  psill <- variance
  range <- max_dist / 4
  if (!is.null(fit) && NROW(fit) > 0) {
    mdl <- as.character(fit$model)
    st <- which(mdl != "Nug")
    nugget <- sum(fit$psill[mdl == "Nug"])
    psill <- if (length(st)) fit$psill[st[1]] else 0
    if (length(st)) {
      range <- fit$range[st[1]]
      model <- mdl[st[1]]
    }
  }
  sill_max <- signif(2 * max(variance, nugget + psill), 3)
  sill_step <- signif(sill_max / 200, 2)
  range_max <- signif(max(1.5 * max_dist, 3 * range), 3)
  range_min <- signif(range_max / 1000, 2)
  list(model = model,
       nugget = list(min = 0, max = sill_max, value = nugget, step = sill_step),
       psill = list(min = 0, max = sill_max, value = psill, step = sill_step),
       range = list(min = range_min, max = range_max, value = max(range, range_min),
                    step = signif(range_max / 200, 2)),
       step_ok = sill_step >= 1e-6)
}

# Names of randomForest's regression importance measures, shared by every panel
# and export that shows them (RFK trend forest, Governing Factors).
RF_IMPORTANCE_LABELS <- c(
  increase = "Increase in out-of-bag MSE",
  scaled = "Scaled: increase / its SE (%IncMSE)",
  purity = "IncNodePurity")

# Every importance measure a randomForest recorded, one row per covariate,
# ordered by the first. A regression forest grown with importance = TRUE
# carries the out-of-bag permutation importance, given unscaled (the increase
# in MSE, squared target units, which orders the covariates) and divided by its
# standard error across trees (randomForest's %IncMSE, which grows with ntree),
# then IncNodePurity; without it, only IncNodePurity. Raw covariate names: this
# is the numeric record behind the labelled importance plot.
rf_importance_df <- function(rf_mod) {
  imp <- randomForest::importance(rf_mod, scale = FALSE)
  out <- data.frame(Variable = rownames(imp), stringsAsFactors = FALSE)
  if ("%IncMSE" %in% colnames(imp)) {
    out[[RF_IMPORTANCE_LABELS[["increase"]]]] <- unname(imp[, "%IncMSE"])
    out[[RF_IMPORTANCE_LABELS[["scaled"]]]] <-
      unname(randomForest::importance(rf_mod, type = 1, scale = TRUE)[rownames(imp), 1])
  }
  if ("IncNodePurity" %in% colnames(imp)) {
    out[[RF_IMPORTANCE_LABELS[["purity"]]]] <- unname(imp[, "IncNodePurity"])
  }
  out <- out[order(out[[2]], decreasing = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# --- RK linear-trend presentation --------------------------------------------
# Structured replacement for the raw print(summary.lm) dump on the Scientific
# Analysis tab: compact fit-statistic chips + a publication-style coefficient
# table. rk_coef_table() is the panel's text flavour; the export registers the
# numeric flavours rk_coef_export_df() and rk_fit_stats_df().

# --- Ruler readouts -----------------------------------------------------------
# Length and area formatting for the Map Viewer ruler. Metres and hectares are
# the app's units everywhere else (grid resolution, buffer radii, class areas),
# so the ruler stays on them and adds the larger unit in brackets rather than
# switching to it: a reader comparing a measured separation against a variogram
# range needs the metres, not a rounded kilometre.
format_measure_length <- function(m) {
  if (is.null(m) || length(m) != 1 || !is.finite(m)) return("n/a")
  if (m >= 1000) {
    sprintf("%s m (%.3f km)", formatC(m, format = "f", digits = 1, big.mark = ","), m / 1000)
  } else if (m >= 1) {
    sprintf("%.1f m", m)
  } else {
    sprintf("%.2f m", m)
  }
}

format_measure_area <- function(m2) {
  if (is.null(m2) || length(m2) != 1 || !is.finite(m2)) return("n/a")
  if (m2 >= 10000) {
    sprintf("%.2f ha (%s m²)", m2 / 10000,
            formatC(m2, format = "f", digits = 0, big.mark = ","))
  } else {
    sprintf("%.1f m² (%.4f ha)", m2, m2 / 10000)
  }
}

format_p_value <- function(p) {
  if (is.null(p) || length(p) == 0 || is.na(p)) return("NA")
  if (p < 0.001) return("< 0.001")
  sprintf("%.3f", p)
}

signif_stars <- function(p) {
  if (is.null(p) || length(p) == 0 || is.na(p)) return("")
  if (p <= 0.001) return("***")
  if (p <= 0.01) return("**")
  if (p <= 0.05) return("*")
  if (p <= 0.1) return(".")
  ""
}

# Fit statistics from a summary.lm object; NULL when the object does not look
# like one (rv$model_summaries entries are only ever summary.lm today, but the
# UI degrades to the raw print if that ever changes).
rk_fit_stats <- function(lm_sum) {
  if (is.null(lm_sum) || is.null(lm_sum$coefficients) || is.null(lm_sum$df)) return(NULL)
  f <- lm_sum$fstatistic
  f_p <- if (!is.null(f) && length(f) == 3) {
    stats::pf(f[[1]], f[[2]], f[[3]], lower.tail = FALSE)
  } else NA_real_
  list(
    r2      = lm_sum$r.squared,
    adj_r2  = lm_sum$adj.r.squared,
    sigma   = lm_sum$sigma,
    df_res  = lm_sum$df[2],
    f_value = if (!is.null(f)) unname(f[[1]]) else NA_real_,
    f_df1   = if (!is.null(f)) unname(f[[2]]) else NA_real_,
    f_df2   = if (!is.null(f)) unname(f[[3]]) else NA_real_,
    f_p     = unname(f_p),
    n       = sum(lm_sum$df[1:2])
  )
}

# Coefficients of a summary.lm object with their t-based confidence bounds (the
# t quantile on the residual df; NA without one). NULL when the object does not
# carry the four summary.lm coefficient columns.
.rk_coef_core <- function(lm_sum, conf_level) {
  if (is.null(lm_sum) || is.null(lm_sum$coefficients)) return(NULL)
  cf <- as.data.frame(lm_sum$coefficients)
  need <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  if (!all(need %in% colnames(cf))) return(NULL)
  df_res <- lm_sum$df[2]
  tq <- if (is.finite(df_res) && df_res >= 1) stats::qt(1 - (1 - conf_level) / 2, df_res) else NA_real_
  est <- cf[["Estimate"]]; se <- cf[["Std. Error"]]
  list(terms = rownames(cf), est = est, se = se, tq = tq,
       lo = est - tq * se, hi = est + tq * se,
       t = cf[["t value"]], p = cf[["Pr(>|t|)"]])
}

.rk_term_labels <- function(terms, vars_metadata) {
  unname(vapply(terms, function(tm) {
    # lm backquotes a term whose name is not syntactic ("`Fe (mg/kg)`").
    if (tm == "(Intercept)") "(Intercept)" else get_var_label(gsub("^`|`$", "", tm), vars_metadata)
  }, character(1)))
}

# Coefficient table (estimate, SE, 95% CI, t, p, significance) as the RK trend
# panel shows it: display text, p-values as "< 0.001" and the CI as one string.
# Term names map to display labels when variable metadata is supplied.
rk_coef_table <- function(lm_sum, vars_metadata = NULL, conf_level = 0.95) {
  k <- .rk_coef_core(lm_sum, conf_level)
  if (is.null(k)) return(NULL)
  ci <- if (is.finite(k$tq)) sprintf("[%.4g, %.4g]", k$lo, k$hi) else rep("NA", length(k$est))
  data.frame(
    Term = .rk_term_labels(k$terms, vars_metadata),
    Estimate = signif(k$est, 4),
    `Std. Error` = signif(k$se, 4),
    `95% CI` = ci,
    `t value` = round(k$t, 2),
    `p value` = vapply(k$p, format_p_value, character(1)),
    `Sig.` = vapply(k$p, signif_stars, character(1)),
    check.names = FALSE
  )
}

# Export flavour of the same table: every statistic a full-precision number,
# the CI as two columns, so the sheet can be recomputed on.
rk_coef_export_df <- function(lm_sum, vars_metadata = NULL, conf_level = 0.95) {
  k <- .rk_coef_core(lm_sum, conf_level)
  if (is.null(k)) return(NULL)
  pct <- paste0(format(100 * conf_level), "%")
  out <- data.frame(Term = .rk_term_labels(k$terms, vars_metadata),
                    Estimate = k$est, `Std. Error` = k$se,
                    check.names = FALSE, stringsAsFactors = FALSE)
  out[[paste0("CI Lower (", pct, ")")]] <- k$lo
  out[[paste0("CI Upper (", pct, ")")]] <- k$hi
  out[["t value"]] <- k$t
  out[["p value"]] <- k$p
  out[["Sig."]] <- vapply(k$p, signif_stars, character(1))
  out
}

# The fit-statistic chips above the coefficient table, as an exportable frame:
# the chips are the only place R², the residual SE, the F test and n are
# reported, so an export of the coefficients alone loses the model's fit.
# Full precision: the chips format for the screen, the sheet keeps the numbers.
rk_fit_stats_df <- function(lm_sum) {
  s <- rk_fit_stats(lm_sum)
  if (is.null(s)) return(NULL)
  data.frame(
    Statistic = c("R²", "Adj. R²", "Residual SE", "Residual df", "F statistic",
                  "F df1", "F df2", "Model p", "n"),
    Value = unname(c(s$r2, s$adj_r2, s$sigma, s$df_res,
                     s$f_value, s$f_df1, s$f_df2, s$f_p, s$n)),
    stringsAsFactors = FALSE
  )
}

get_var_label <- function(v, vars_metadata) {
  if (is.null(v) || is.na(v) || v == "") return(v)
  if (!is.null(vars_metadata)) {
    all_actuals <- sapply(vars_metadata, function(x) x$actual)
    fuzzy_actual <- fuzzy_match_column(v, all_actuals)
    if (!is.null(fuzzy_actual)) {
      match_fuzzy <- Filter(function(x) x$actual == fuzzy_actual, vars_metadata)
      if (length(match_fuzzy) > 0 && !is.null(match_fuzzy[[1]]$label) && match_fuzzy[[1]]$label != "") {
        return(match_fuzzy[[1]]$label)
      }
    }
  }
  return(v)
}

get_var_labels <- function(vars, vars_metadata) {
  if (is.null(vars)) return(NULL)
  sapply(vars, get_var_label, vars_metadata = vars_metadata)
}

fuzzy_match_column <- function(act_name, user_cols) {
  if (act_name %in% user_cols) {
    return(act_name)
  }
  if (tolower(act_name) %in% tolower(user_cols)) {
    return(user_cols[tolower(user_cols) == tolower(act_name)][1])
  }
  clean_act <- tolower(gsub("[^a-zA-Z0-9]", "", act_name))
  clean_user <- tolower(gsub("[^a-zA-Z0-9]", "", user_cols))
  if (clean_act %in% clean_user) {
    return(user_cols[clean_user == clean_act][1])
  }
  
  dists <- as.vector(adist(clean_act, clean_user))
  min_idx <- which.min(dists)
  if (length(min_idx) > 0) {
    min_dist <- dists[min_idx]
    if (min_dist <= 2 && (min_dist / max(1, nchar(clean_act))) <= 0.3) {
      return(user_cols[min_idx])
    }
  }
  
  return(NULL)
}

# A presentation map, never a rename of analysis columns. Ambiguous labels
# include their source identifier; even a user label containing that suffix
# cannot collide with another variable's displayed name.
desc_var_labels <- function(vars, vars_metadata = NULL) {
  vars <- unique(vars)
  labels <- unname(get_var_labels(vars, vars_metadata))
  repeat {
    dup <- duplicated(labels) | duplicated(labels, fromLast = TRUE)
    if (!any(dup)) break
    labels[dup] <- paste0(labels[dup], " [", vars[dup], "]")
  }
  stats::setNames(labels, vars)
}

display_var_labels <- function(vars, labels = NULL) {
  if (is.null(labels)) return(vars)
  text <- unname(labels[vars])
  text[is.na(text)] <- vars[is.na(text)]
  text
}

filter_active_groups <- function(df, active_groups) {
  if (is.null(df)) return(df)
  if ("group_id" %in% colnames(df)) {
    if (!is.null(active_groups) && length(active_groups) > 0) {
      df <- df[df$group_id %in% active_groups, , drop = FALSE]
    } else if (!is.null(active_groups) && length(active_groups) == 0) {
      df <- df[0, , drop = FALSE]
    }
  }
  return(df)
}

# TRUE only for a usable single column name. detect_pred_column returns NA
# (not NULL) when no prediction column exists, so a bare is.null() check
# wrongly treats "no predictions uploaded" as "predictions present".
is_valid_col_ref <- function(x) {
  !is.null(x) && length(x) == 1 && !is.na(x) && nzchar(x)
}

# TRUE while the Match Scales checkbox is on screen: the sidebar is set up for
# a comparison of predictions, or the Map Viewer shows the comparison view of a
# run with a predicted surface. The checkbox's conditionalPanel (ui_sidebar.R)
# states the same condition in JavaScript; change the two together.
match_scales_shown <- function(comp_mode, value_type, has_pred, view_base) {
  (isTRUE(comp_mode) && isTRUE(value_type %in% c("pred", "pred_ss"))) ||
    (isTRUE(has_pred) && identical(view_base, "view_comp"))
}

# TRUE when a locality of a run failed: its processing stopped part-way (the
# pipeline's "Error in <locality>:" line), or a surface the run asked for is
# missing and its log reports an error. A cross-validation error under finished
# maps is not a failure: the log and the Model Performance row report it.
locality_run_failed <- function(res, want_pre) {
  log <- res$log_msg %||% ""
  if (grepl(paste0("Error in ", res$l, ":"), log, fixed = TRUE)) return(TRUE)
  lost <- is.null(res$r_a) || (isTRUE(want_pre) && is.null(res$r_p))
  lost && grepl("Error", log, fixed = TRUE)
}

# Per locality and surface, the covariates each mapped model used and the ones
# the collinearity screen removed, from the worker results (aux_used_* and
# aux_dropped_*, RK/RFK/CK only). The screen runs on each locality's own rows,
# so the record is kept per locality: a union would describe no fitted model.
covariate_screen_record <- function(res_all) {
  retained <- list(); dropped <- list(); design <- list()
  for (res in res_all) {
    for (tg in c("act", "pre")) {
      used <- res[[paste0("aux_used_", tg)]]
      if (is.null(used)) next
      surface <- if (tg == "act") "actual" else "predicted"
      if (is.null(retained[[res$l]])) { retained[[res$l]] <- list(); dropped[[res$l]] <- list() }
      retained[[res$l]][[surface]] <- paste(used, collapse = ", ")
      dropped[[res$l]][[surface]] <- paste(res[[paste0("aux_dropped_", tg)]], collapse = ", ")
      # Co-kriging's data design: "heterotopic" when locations without the
      # target contributed covariate values, "isotopic" otherwise.
      des <- res[[paste0("ck_design_", tg)]]
      if (!is.null(des)) {
        if (is.null(design[[res$l]])) design[[res$l]] <- list()
        design[[res$l]][[surface]] <- des
      }
    }
  }
  list(retained = retained, dropped = dropped, design = design)
}

# The covariate line of the run configuration panel: what was selected and
# what the fitted models used. One statement when every locality and surface
# kept the same set, otherwise one per locality and surface. A model that used
# none fell back to Ordinary Kriging (RK/RFK/CK only reach none that way).
covariate_record_text <- function(cfg) {
  sel <- cfg$covariates_selected
  if (is.null(sel) || is.na(sel) || !nzchar(sel)) return(NULL)
  txt <- paste0("Covariates selected: ", sel)
  flat <- function(x) unlist(lapply(names(x), function(l) {
    v <- unlist(x[[l]])
    stats::setNames(v, paste0(l, if (length(v) > 1) paste0(" (", names(v), ")") else ""))
  }))
  ret <- flat(cfg$covariates_retained)
  if (!length(ret)) return(txt)
  drp <- flat(cfg$covariates_dropped)[names(ret)]
  used <- ifelse(nzchar(ret), ret, "none (Ordinary Kriging fallback)")
  gone <- ifelse(is.na(drp) | !nzchar(drp), "none", drp)
  des <- flat(cfg$ck_design)
  des_txt <- if (!length(des)) "" else if (length(unique(des)) == 1) {
    paste0(" | co-kriging design: ", des[1])
  } else {
    paste0(" | co-kriging design: ", paste0(names(des), ": ", des, collapse = "; "))
  }
  if (length(unique(used)) == 1 && length(unique(gone)) == 1) {
    return(paste0(txt, " | used: ", used[1], " | removed by the screen: ", gone[1], des_txt))
  }
  paste0(txt, " | used per locality: ",
         paste0(names(ret), ": ", used, " (removed: ", gone, ")", collapse = "; "), des_txt)
}

# Where the Context panel opens: the first category holding a variable with an
# uploaded prediction column, and in the category the first such variable.
# Those are the variables the app can map end to end; the variable list has no
# target/covariate flag, so this is the one signal there is. Falls back to the
# first category and its first variable.
default_var_pick <- function(vars, category = NULL) {
  cat_of <- vapply(vars, function(x) as.character(x$category %||% ""), character(1))
  has_pred <- vapply(vars, function(x) is_valid_col_ref(x$pred) || is_valid_col_ref(x$pred_ss),
                     logical(1))
  cats <- unique(cat_of)
  if (is.null(category)) category <- c(cats[cats %in% cat_of[has_pred]], cats)[1]
  pick <- c(which(cat_of == category & has_pred), which(cat_of == category))[1]
  list(category = category, var = if (is.na(pick)) NULL else as.character(vars[[pick]]$actual))
}

# Sidebar bivariate screen, independent of interpolation and its CV. SS uses
# the chosen partition on both sources; CVE and Actual use all scoped rows.
rank_auxiliary_correlations <- function(data, mapping, variable, value_type = "actual",
                                       source = "predictions", localities = NULL, subset = "all") {
  meta <- Filter(function(m) identical(m$actual, variable), mapping$vars)
  if (length(meta) == 0) stop("Target variable is not mapped.")
  meta <- meta[[1]]
  predicted <- value_type %in% c("pred", "pred_ss", "resid") && source == "predictions"
  target <- if (predicted) {
    if (value_type == "pred_ss") meta$pred_ss else meta$pred
  } else meta$actual
  if (!is_valid_col_ref(target) || !target %in% names(data)) {
    stop(if (predicted) "The mapped prediction column is unavailable." else "Target column is unavailable.")
  }
  if (!is.numeric(data[[target]])) stop("Target column must be numeric.")
  locs <- resolve_selected_localities(localities, data, mapping$loc)
  if (is_valid_col_ref(mapping$loc) && mapping$loc %in% names(data)) {
    data <- data[as.character(data[[mapping$loc]]) %in% locs, , drop = FALSE]
  }
  subset_col <- find_subset_column(names(data))
  subset <- if (value_type == "pred_ss" && !is.na(subset_col)) subset else "all"
  if (subset != "all") {
    data <- data[!is.na(data[[subset_col]]) & as.character(data[[subset_col]]) == subset, , drop = FALSE]
  }
  # Retain the existing candidate set, excluding the screened target itself.
  exclude <- c(mapping$x, mapping$y, meta$actual, target)
  candidates <- setdiff(names(data)[vapply(data, is.numeric, logical(1))], exclude)
  rows <- lapply(candidates, function(v) {
    ok <- is.finite(data[[target]]) & is.finite(data[[v]])
    if (sum(ok) < 3 || stats::sd(data[[target]][ok]) == 0 || stats::sd(data[[v]][ok]) == 0) return(NULL)
    test <- stats::cor.test(data[[target]][ok], data[[v]][ok], method = "pearson")
    data.frame(Variable = v, Corr = unname(test$estimate), Pval = test$p.value,
               N = sum(ok), stringsAsFactors = FALSE)
  })
  results <- do.call(rbind, rows)
  if (is.null(results)) results <- data.frame(Variable = character(), Corr = numeric(), Pval = numeric(), N = integer())
  results <- results[order(abs(results$Corr), decreasing = TRUE), , drop = FALSE]
  list(results = results, target = target, source = if (predicted) "ML predictions" else "Actual values",
       scope = if (length(locs)) paste(locs, collapse = ", ") else "all localities",
       subset = subset, n = nrow(data), skipped = setdiff(candidates, results$Variable))
}

detect_pred_column <- function(target, candidates, type = "cve") {
  if (is.null(target) || is.na(target) || length(candidates) == 0) return(NA)
  
  patterns <- if (type == "cve") {
    c(
      paste0("^", target, "_cve$"),
      paste0("^", target, "_pred$"),
      paste0("^", target, "_predicted$"),
      paste0("^", target, "Pred$"),
      paste0("^", target, "Predicted$"),
      paste0("^pred_", target, "$"),
      paste0("^predicted_", target, "$")
    )
  } else {
    c(
      paste0("^", target, "_ss$"),
      paste0("^", target, "_split$"),
      paste0("^", target, "_test$"),
      paste0("^", target, "Split$"),
      paste0("^", target, "Test$")
    )
  }
  
  for (pat in patterns) {
    matches <- grep(pat, candidates, ignore.case = TRUE, value = TRUE)
    if (length(matches) > 0) return(matches[1])
  }
  
  return(NA)
}

match_metadata_columns <- function(m_df, user_cols) {
  cols <- colnames(m_df)
  col_act <- if (length(grep("actual|column|variable", cols, ignore.case=TRUE)) > 0) grep("actual|column|variable", cols, ignore.case=TRUE, value=TRUE)[1] else 1
  col_lab <- if (length(grep("label|name|display|ID", cols, ignore.case=TRUE)) > 0) grep("label|name|display|ID", cols, ignore.case=TRUE, value=TRUE)[1] else NA
  col_cat <- if (length(grep("cat|group|type", cols, ignore.case=TRUE)) > 0) grep("cat|group|type", cols, ignore.case=TRUE, value=TRUE)[1] else NA
  # The measurement unit: a header that is, or contains the word, "unit" /
  # "units" (whole word, so "Community" is not a unit column).
  col_unit <- grep("^units?$|\\bunits?\\b", cols, ignore.case = TRUE, value = TRUE)[1]

  new_vars <- list()
  # seq_len, not 1:nrow: a headers-only metadata upload (0 rows) must yield an
  # empty mapping, not an iteration over the phantom row 1:0 produces. The NA
  # check runs BEFORE the fuzzy match so an NA name never reaches adist().
  for (i in seq_len(nrow(m_df))) {
    act_name <- as.character(m_df[i, col_act])
    if (is.na(act_name) || act_name == "") next
    matched_col <- fuzzy_match_column(act_name, user_cols)

    if (!is.null(matched_col)) {
      cat_val <- if (!is.na(col_cat)) as.character(m_df[i, col_cat]) else "Uploaded Data"
      lab_val <- if (!is.na(col_lab)) as.character(m_df[i, col_lab]) else act_name
      unit_val <- if (!is.na(col_unit)) trimws(as.character(m_df[[col_unit]][i])) else ""
      if (is.na(unit_val)) unit_val <- ""

      already_mapped <- sapply(new_vars, function(x) x$actual)
      if (length(already_mapped) > 0 && matched_col %in% already_mapped) next

      p_cve <- detect_pred_column(matched_col, user_cols, "cve")
      p_ss  <- detect_pred_column(matched_col, user_cols, "ss")

      new_var <- list(
        actual = matched_col,
        pred = p_cve,
        pred_ss = p_ss,
        label = lab_val,
        category = cat_val,
        unit = unit_val,
        palette = get_default_palette(matched_col, cat_val, lab_val)
      )
      
      new_vars[[length(new_vars) + 1]] <- new_var
    }
  }
  return(new_vars)
}

discretize_numeric_var <- function(x, method = "median", custom_breaks = NULL, var_name = "") {
  prefix <- if(nchar(var_name) > 0) paste0(var_name, ": ") else ""
  if (all(is.na(x))) return(factor(rep(NA, length(x))))
  
  if (method == "median") {
    val <- median(x, na.rm = TRUE)
    lbls <- paste0(prefix, c("<= Median", "> Median"))
    return(factor(ifelse(x <= val, lbls[1], lbls[2]), levels = lbls))
  } else if (method == "mean") {
    val <- mean(x, na.rm = TRUE)
    lbls <- paste0(prefix, c("<= Mean", "> Mean"))
    return(factor(ifelse(x <= val, lbls[1], lbls[2]), levels = lbls))
  } else if (method == "tertiles") {
    q <- quantile(x, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
    q <- unique(q)
    if (length(q) < 4) return(factor(ifelse(is.na(x), NA_character_, paste0(prefix, "Low Variation"))))
    lbls <- paste0(prefix, c("Low", "Medium", "High"))
    return(cut(x, breaks = q, include.lowest = TRUE, labels = lbls))
  } else if (method == "quintiles") {
    q <- quantile(x, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
    q <- unique(q)
    if (length(q) < 6) return(factor(ifelse(is.na(x), NA_character_, paste0(prefix, "Low Variation"))))
    lbls <- paste0(prefix, c("Q1", "Q2", "Q3", "Q4", "Q5"))
    return(cut(x, breaks = q, include.lowest = TRUE, labels = lbls))
  } else if (method == "custom" && !is.null(custom_breaks)) {
    brks <- sort(unique(c(-Inf, custom_breaks, Inf)))
    lbls <- character(length(brks) - 1)
    for (i in 1:(length(brks)-1)) {
      if (i == 1) lbls[i] <- paste(prefix, "<=", brks[i+1])
      else if (i == length(brks)-1) lbls[i] <- paste(prefix, ">", brks[i])
      else lbls[i] <- paste0(prefix, "(", brks[i], "-", brks[i+1], "]")
    }
    return(cut(x, breaks = brks, include.lowest = TRUE, labels = lbls))
  }
  return(as.factor(x))
}

process_grouping_vars <- function(df, vars, types) {
  if (length(vars) == 0 || is.null(vars)) {
    df$group_id <- as.factor("All")
    return(df)
  }
  
  group_list <- list()
  for (i in seq_along(vars)) {
    v <- vars[i]
    t <- types[i]
    if (t == "categorical") {
      group_list[[v]] <- as.factor(df[[v]])
    } else if (grepl("^numeric", t)) {
      method <- if(grepl("_", t)) sub("numeric_", "", t) else "median"
      group_list[[v]] <- discretize_numeric_var(df[[v]], method = method, var_name = v)
    } else {
      group_list[[v]] <- as.factor(df[[v]])
    }
  }
  
  if (length(vars) == 1) {
    df$group_id <- group_list[[1]]
  } else {
    df$group_id <- interaction(group_list, sep = " | ", drop = TRUE)
  }
  return(df)
}


# --- Descriptive Suite statistics ---------------------------------------------
# Pure counterparts of the Descriptive/Exploratory module's summary table,
# per-group trend fits and PCA. The module keeps the reactive reads and the
# formatting; the arithmetic lives here so it is reachable from the test suite.

# Per-group summary plus a pooled row, at full precision (the module formats).
# `x` and `group` are parallel vectors. Each group is summarised on its rows
# with both values present, while the pooled row summarises every non-NA x
# regardless of its group, with the same statistics. `is_pooled` identifies the
# pooled row; it is labelled TOTAL, or "TOTAL (pooled)" when a real group
# already carries that name.
# Beside the mean and SD, which a few outliers can move a long way, the table
# carries their robust counterparts: the median, the quartiles (quantile type 7,
# R's default, so it agrees with summary() elsewhere in the app), the IQR
# (Q3 - Q1) and the MAD. MAD is stats::mad(): the median absolute deviation
# scaled by 1.4826, a consistent estimator of sigma for a normal sample, so it
# reads on the same scale as the SD beside it.
DESC_SUMMARY_STATS <- c("Mean", "SD", "Median", "Q1", "Q3", "IQR", "MAD", "Min", "Max")

desc_summary_table <- function(x, group) {
  stats_of <- function(v) {
    if (!length(v)) return(c(n = 0L, stats::setNames(rep(NA_real_, length(DESC_SUMMARY_STATS)), DESC_SUMMARY_STATS)))
    q <- stats::quantile(v, c(0.25, 0.5, 0.75), type = 7, names = FALSE)
    c(n = length(v), Mean = mean(v), SD = stats::sd(v), Median = q[2],
      Q1 = q[1], Q3 = q[3], IQR = q[3] - q[1], MAD = stats::mad(v),
      Min = min(v), Max = max(v))
  }
  observed <- !is.na(x) & !is.na(group)
  grouped <- split(x[observed], droplevels(factor(group[observed])))
  ok <- !is.na(x)
  m <- do.call(rbind, c(lapply(grouped, stats_of), list(stats_of(x[ok]))))
  # unname(): with a single group a column of the statistics matrix is a
  # length-1 vector that still carries the COLUMN name, which data.frame()
  # would then adopt as the row name - and DT renders row names, so the default
  # "All" grouping showed a leading column reading "mean".
  pooled_label <- "TOTAL"
  while (pooled_label %in% as.character(group)) pooled_label <- paste0(pooled_label, " (pooled)")
  res <- data.frame(Group = c(names(grouped), pooled_label),
                    is_pooled = c(rep(FALSE, length(grouped)), TRUE),
                    Count = as.integer(unname(m[, "n"])),
                    row.names = NULL, stringsAsFactors = FALSE)
  for (s in DESC_SUMMARY_STATS) res[[s]] <- unname(m[, s])
  res
}

# Trend statistic per group for the scatter panel's fitted curve.
# `groups` is the label vector to report on, in order; `pooled` explicitly
# identifies rows fitted on the whole frame. Groups with fewer than `min_n`
# rows, and any fit that errors, return NA for both columns.
#
# What each fit reports, and why they are not interchangeable:
#   linear / polynomial - summary(lm)$r.squared with the model's overall F test.
#   loess               - cor(y, fitted)^2. A loess has no R^2: this is the
#                         squared correlation between observed and fitted, which
#                         is why the caller labels it as such, and there is no
#                         F test to report with it.
#   gam                 - summary(gam)$r.sq (adjusted) and the smooth's p-value.
desc_group_fit_stats <- function(df, x_var, y_var, fit, groups,
                                 group_col = "group_id", min_n = 5,
                                 pooled = rep(FALSE, length(groups))) {
  stopifnot(length(pooled) == length(groups), !anyNA(pooled))
  f_pval <- function(s) {
    if (is.null(s$fstatistic)) return(NA_real_)
    unname(stats::pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE))
  }
  none <- c(r2 = NA_real_, p = NA_real_)

  out <- lapply(seq_along(groups), function(i) {
    g <- as.character(groups[i])
    keep <- !is.na(df[[group_col]]) & !is.na(g) & as.character(df[[group_col]]) == g
    sub_df <- if (pooled[i]) df else df[keep, , drop = FALSE]
    sub_df <- sub_df[stats::complete.cases(sub_df[, c(x_var, y_var), drop = FALSE]), , drop = FALSE]
    if (nrow(sub_df) < min_n) return(none)
    tryCatch({
      form_lin  <- stats::as.formula(paste0("`", y_var, "` ~ `", x_var, "`"))
      form_poly <- stats::as.formula(paste0("`", y_var, "` ~ poly(`", x_var, "`, 2)"))
      form_gam  <- stats::as.formula(paste0("`", y_var, "` ~ s(`", x_var, "`, bs = 'cs')"))
      if (fit == "linear") {
        s <- summary(stats::lm(form_lin, data = sub_df))
        c(r2 = s$r.squared, p = f_pval(s))
      } else if (fit == "polynomial") {
        if (length(unique(sub_df[[x_var]])) <= 3) return(none)
        s <- summary(stats::lm(form_poly, data = sub_df))
        c(r2 = s$r.squared, p = f_pval(s))
      } else if (fit == "loess") {
        mod <- stats::loess(form_lin, data = sub_df, span = 0.7)
        # The response must come back from the FIT (y = fitted + residual), not
        # from the unfiltered column: loess's na.action drops incomplete rows,
        # so fitted() is shorter than sub_df whenever the group carries a
        # missing x or y, and cor() on two different lengths errors - which the
        # tryCatch below would report as a blank column rather than a number.
        fv <- stats::fitted(mod)
        c(r2 = stats::cor(fv + stats::residuals(mod), fv)^2, p = NA_real_)
      } else if (fit == "gam") {
        if (!requireNamespace("mgcv", quietly = TRUE)) return(none)
        s <- summary(mgcv::gam(form_gam, data = sub_df))
        c(r2 = s$r.sq, p = s$s.table[1, "p-value"])
      } else none
    }, error = function(e) none)
  })

  data.frame(Group = as.character(groups),
             r2 = vapply(out, function(v) unname(v[["r2"]]), numeric(1)),
             p  = vapply(out, function(v) unname(v[["p"]]), numeric(1)))
}

# Complete-case PCA on source identifiers `vars`; `labels` describe refusals.
# Returns the prcomp object, the frame it was fitted on, the row mask (so the
# caller can align a grouping vector to it) and how many rows the complete-case
# filter removed. `scale = TRUE` is a correlation PCA, FALSE a covariance PCA;
# both centre.
# A column with exactly or effectively no variance over those rows (the
# engines' own rule, .is_degenerate_covariate) carries no information and
# cannot be standardised, so it is left out and named; the remaining columns
# keep the requested scaling. Below five complete rows or two informative
# columns the PCA is refused: `res` is NULL and `refusal` says why.
desc_pca_fit <- function(df, vars, labels = vars, scale = TRUE) {
  keep <- stats::complete.cases(df[, vars, drop = FALSE])
  df_clean <- df[keep, vars, drop = FALSE]
  if (nrow(df_clean) < 5L) {
    return(list(res = NULL, data = df_clean, keep = keep, dropped = nrow(df) - nrow(df_clean),
                dropped_constant = character(0),
                refusal = sprintf("PCA needs at least 5 complete observations across the selected variables; %d remain.", nrow(df_clean))))
  }
  informative <- vapply(df_clean, function(v) !.is_degenerate_covariate(v), logical(1))
  out <- list(res = NULL, data = df_clean[, informative, drop = FALSE], keep = keep,
              # `dropped` counts ROWS removed by the complete-case filter;
              # `dropped_constant` names the COLUMNS removed for having no variance.
              dropped = nrow(df) - nrow(df_clean),
              dropped_constant = labels[!informative], refusal = NULL)
  if (sum(informative) < 2) {
    n_const <- sum(!informative)
    out$refusal <- sprintf(
      "PCA needs at least two variables with variance. %s %s none over the %d complete rows and %s excluded; %s.",
      paste(labels[!informative], collapse = ", "), if (n_const == 1) "has" else "have",
      nrow(df_clean), if (n_const == 1) "was" else "were",
      if (sum(informative) == 1) "only one usable variable remains" else "no usable variable remains")
    return(out)
  }
  out$res <- stats::prcomp(out$data, scale. = isTRUE(scale), center = TRUE)
  out
}



find_subset_column <- function(cols) {
  hit <- grep("^subset$", cols, ignore.case = TRUE, value = TRUE)
  if (length(hit) == 0) NA_character_ else hit[1]
}

# One row-selection rule for dispatch, tuning and previews of a run.
effective_subset <- function(value_type, subset, cols) {
  if (identical(value_type, "pred_ss") && is_valid_col_ref(subset) &&
      subset != "all" && !is.na(find_subset_column(cols))) subset else "all"
}

run_locality_rows <- function(df, loc_col, l, eff_subset = "all") {
  keep <- !is.na(df[[loc_col]]) & df[[loc_col]] %in% l
  if (eff_subset != "all") {
    subset_col <- find_subset_column(names(df))
    keep <- keep & !is.na(df[[subset_col]]) & df[[subset_col]] == eff_subset
  }
  df[keep, , drop = FALSE]
}

tuning_key <- function(col, eff_subset = "all") {
  if (!is_valid_col_ref(col)) return(NA_character_)
  if (eff_subset == "all") as.character(col) else paste0(col, " [subset ", eff_subset, "]")
}
