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

# NOTE: is_coord_col() used to live here. It now sits in spatial_metrics.R
# (with the .coord_names_x / .coord_names_y token lists it shares with
# perform_cv's coordinate detection), because PSOCK workers source only
# spatial_helpers.R and never the ui_*.R files. Both files are sourced at
# startup in the main session, so every UI/server call site is unaffected.

#' Which column should an axis dropdown default to after an upload?
#' Whole-name matching on the same token lists is_coord_col() uses. Substring
#' matching ("^lon" / "^lat") pre-selected variables such as Longevity_index or
#' Lateral_flow as a coordinate, which the user then had to notice and undo.
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
# summary table, which used to residualize independently (and disagreed with
# each other on quoting, so labels containing spaces broke the plot).
#
# Conventions follow ppcor, which the table's p-value block already cites:
#   pearson  - residualize the RAW values on the controls, product-moment
#              correlation of the residuals (algebraically identical to
#              inverting the Pearson correlation matrix).
#   spearman - rank-transform EVERY column first, then residualize and take the
#              product-moment correlation of the rank residuals. Correlating
#              raw-value residuals with method = "spearman" (the old behaviour)
#              is NOT a partial rank correlation: ppcor residualizes the ranks,
#              and cor(method = "spearman") of the residuals would instead
#              re-rank residuals of an unranked fit.
#   kendall  - no residualization analogue exists; ppcor inverts the Kendall
#              tau matrix (the multi-control generalisation of Kendall's
#              first-order partial tau), so that is what is done here.
# Residualization uses one pivoted QR over the shared control design matrix
# (model.matrix keeps factor controls and awkward column names working) — the
# same fit lm() would produce, but computed once for all variables.
#
# Returns list(cormat, n, k, method, failed). `cormat` is NULL when the partial
# correlation could not be computed; `failed` then names the offending columns
# so the caller can abort instead of silently reporting raw correlations under
# a "partial" label.
compute_partial_correlation <- function(df, vars, control_vars = NULL,
                                        method = "pearson") {
  vars <- unique(vars)
  # A variable must never control for itself: residualizing v against a set
  # containing v yields ~zero residuals and a NaN row.
  ctrl <- setdiff(unique(control_vars), vars)
  out <- list(cormat = NULL, n = 0L, k = length(ctrl), method = method,
              failed = character(0))

  cols <- c(vars, ctrl)
  missing_cols <- setdiff(cols, colnames(df))
  if (length(missing_cols) > 0) {
    out$failed <- missing_cols
    return(out)
  }

  d <- stats::na.omit(df[, cols, drop = FALSE])
  out$n <- nrow(d)
  if (length(vars) < 2 || out$n < 3) return(out)

  if (length(ctrl) == 0) {
    out$cormat <- stats::cor(d[, vars, drop = FALSE], method = method)
    return(out)
  }

  if (identical(method, "kendall")) {
    tau <- stats::cor(d, method = "kendall")
    inv <- tryCatch(solve(tau), error = function(e) NULL)
    if (is.null(inv) || any(!is.finite(inv))) {
      out$failed <- vars
      return(out)
    }
    pc <- -inv / sqrt(outer(diag(inv), diag(inv)))
    diag(pc) <- 1
    out$cormat <- pc[vars, vars, drop = FALSE]
    return(out)
  }

  fit_df <- d
  if (identical(method, "spearman")) fit_df[] <- lapply(d, rank)
  resid_mat <- tryCatch({
    X <- stats::model.matrix(~ ., data = fit_df[, ctrl, drop = FALSE])
    qr.resid(qr(X), as.matrix(fit_df[, vars, drop = FALSE]))
  }, error = function(e) NULL)
  if (is.null(resid_mat)) {
    out$failed <- vars
    return(out)
  }
  colnames(resid_mat) <- vars
  out$cormat <- stats::cor(resid_mat, method = "pearson")
  out
}

method_labels <- c(
  "OK"  = "Ordinary Kriging",
  "UK"  = "Universal Kriging",
  "RK"  = "Regression Kriging",
  "RFK" = "Random Forest Kriging",
  "CK"  = "Co-Kriging",
  "IDW" = "IDW",
  "TPS" = "Thin Plate Spline"
)

get_method_label <- function(method) {
  if (is.null(method) || length(method) == 0 || is.na(method) || method == "") return("")
  if (method %in% names(method_labels)) {
    return(method_labels[[method]])
  }
  return(method)
}

format_param_val <- function(type, val) {
  if(type == "TPS" && !is.na(val) && val < 0) return("Auto (GCV)")
  as.character(round(val, 6))
}

# Regional Parameters table for IDW/TPS, built from the run-committed
# per-locality params snapshot (rv$disp$regional_params) — never the live
# tuning store: a run made without optimizer/manual entries consumes the
# global slider value, which the store does not hold, so reading the store
# mislabels e.g. a fixed lambda = 0 run as "Auto (GCV)".
# has_pre = FALSE means the displayed run mapped no prediction surface, so
# there is no second parameter to report: the column is dropped rather than
# filled with a column of "N/A", which read as a failed optimization.
build_regional_params_df <- function(type, loc, regional_params, has_pre) {
  if (is.null(regional_params) || length(regional_params) == 0) return(NULL)
  fmt <- function(l, tgt) {
    key <- paste0(if (type == "IDW") "idw_p_" else "tps_lambda_", tgt)
    val <- regional_params[[l]][[key]]
    if (is.null(val)) return("N/A")
    format_param_val(type, val)
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
# `cv_design` is the fold plan the row was scored under (cv_type_label()); it
# is a column of its own here because the on-screen Source string that carries
# it is not machine-readable.
cv_metrics_export_df <- function(res, source_label, cv_design = NA_character_) {
  if (is.null(res)) return(NULL)
  out <- data.frame(Source = source_label,
                    `CV Design` = cv_design,
                    n = as.integer(res$n %||% NA),
                    check.names = FALSE, stringsAsFactors = FALSE)
  for (k in names(CV_METRIC_LABELS)) {
    v <- res[[k]]
    out[[unname(CV_METRIC_LABELS[[k]])]] <-
      if (is.null(v) || length(v) != 1) NA_real_ else as.numeric(v)
  }
  out
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
# and NMAE has no CV counterpart - it comes off the raw residuals so a
# small-mean variable does not carry display rounding into a percentage.
# moran = FALSE: an uploaded prediction column carries no CV residual field.
# round_values = TRUE is the card's display rounding (perform_cv's own
# dictionary: 4 dp, 2 dp for the percentage and ratio metrics).
pred_perf_df <- function(obs, pre, round_values = FALSE) {
  ok <- !is.na(obs) & !is.na(pre)
  obs <- obs[ok]; pre <- pre[ok]
  if (length(obs) < 3) return(NULL)
  m <- perform_cv(data.frame(var1.observed = obs, var1.pred = pre), moran = FALSE,
                  round_values = round_values)
  mean_v <- mean(obs)
  mae_raw <- mean(abs(obs - pre))
  nmae <- if (is.finite(mae_raw) && abs(mean_v) > 0) (mae_raw / abs(mean_v)) * 100 else NA_real_
  if (isTRUE(round_values)) nmae <- round(nmae, 2)
  data.frame(
    Metric = c("R² (NSE/Traditional)", "R² (Correlation)", "RMSE", "NRMSE (%)",
               "MAE", "NMAE (%)", "MBE (ML pred - observed)", "Lin's CCC (Agree)",
               "RPD (Precision)", "RPIQ", "SMAPE (%)", "n"),
    Value = as.numeric(c(m$nse, m$r2, m$rmse, m$nrmse_mean, m$mae, nmae, -m$me,
                         m$ccc, m$rpd, m$rpiq, m$smape, m$n)),
    stringsAsFactors = FALSE
  )
}

# Class-agreement table from a compute_agreement_metrics() result.
agreement_metrics_df <- function(ag, round_values = FALSE) {
  if (is.null(ag) || !is.null(ag$status)) return(NULL)
  v <- as.numeric(c(ag$accuracy, ag$bal_accuracy, ag$off_by_one,
                    ag$mcc, ag$kappa, ag$kappa_linear))
  data.frame(
    Metric = c("Overall Accuracy", "Balanced Accuracy", "Off-by-one Accuracy",
               "Matthews Corr. Coef. (MCC)", "Kappa (Unweighted)",
               "Weighted Kappa (Linear)"),
    Value = if (isTRUE(round_values)) round(v, 4) else v,
    stringsAsFactors = FALSE
  )
}

# Screen rounding for a statistic in the variable's own units: three decimals,
# or four significant digits below 1. A fixed number of decimals collapses a
# small-unit variable (total N spans 0.0175-0.214 %; a fitted nugget of
# 2.151e-4) to one significant digit or to zero.
display_num <- function(x) {
  x <- as.numeric(x)
  ifelse(is.finite(x) & abs(x) < 1, signif(x, 4), round(x, 3))
}

# summary() of one or two numeric vectors as a tidy frame, values left numeric.
# A FIXED row set keeps the two columns in step: summary() appends an "NA's"
# element only for a vector that actually has missing values, so building the
# second column by assignment failed outright ("replacement has 6 rows, data
# has 7") whenever exactly one of the two sides carried them. The
# missing-value row is shown only when there is something to report. A second
# vector with no observed value keeps its column (statistics NA, the NA's row
# counting it), so an empty predicted side reads as empty rather than absent.
summary_stats_df <- function(a, b = NULL, labels = c("Value", "Predicted"),
                             round_values = FALSE) {
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
    if (isTRUE(round_values)) v[] <- display_num(v)
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
# range. Unrounded unless round_values = TRUE (the card's display rounding).
vgm_params_row <- function(f, round_values = FALSE) {
  out <- list(model = NA_character_, kappa = NA_real_, nugget = NA_real_,
              sill = NA_real_, range = NA_real_, practical_range = NA_real_,
              sdep = NA_real_)
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
  if (isTRUE(round_values)) {
    out$nugget <- display_num(out$nugget)
    out$sill <- display_num(out$sill)
    out$range <- round(out$range, 1)
    out$practical_range <- round(out$practical_range, 1)
    out$sdep <- round(out$sdep, 1)
  }
  out
}

# Screen flavour: "NA" for an absent value, the smoothness beside a Matern
# model name and the percent sign on Structural Dependency, all character so
# one column can mix the model name with numbers.
.vgm_params_chr <- function(f) {
  p <- vgm_params_row(f, round_values = TRUE)
  if (is.na(p$model)) return(rep("NA", 6))
  chr <- function(x) if (is.na(x)) "NA" else as.character(x)
  c(if (is.na(p$kappa)) p$model else paste0(p$model, " (kappa = ", p$kappa, ")"),
    chr(p$nugget), chr(p$sill), chr(p$range), chr(p$practical_range),
    if (is.na(p$sdep)) "NA" else paste0(p$sdep, "%"))
}

# The Variogram Parameters card. "Total (Combined)" lists every fitted
# locality/target (variograms are fitted per locality); a named locality is
# transposed so Actual and Predicted sit side by side. NULL when nothing in the
# store was fitted, which sci_dt() renders as the empty state.
vgm_params_table_df <- function(v_fit_list, loc) {
  if (identical(loc, "Total (Combined)")) {
    return(vgm_params_export_df(v_fit_list, round_values = TRUE))
  }
  f_a <- v_fit_list[[paste0(loc, "_act")]]
  f_p <- v_fit_list[[paste0(loc, "_pre")]]
  if (is.null(f_a) && is.null(f_p)) return(NULL)
  res <- data.frame(Param = c("Model", "Nugget", "Sill", "Range (a)",
                              "Practical Range", "Structural Dep."),
                    Actual = .vgm_params_chr(f_a), stringsAsFactors = FALSE)
  # Predicted column only when a predicted-surface fit exists: an all-"NA"
  # column for a run that never mapped predictions is just noise.
  if (!is.null(f_p)) res$Predicted <- .vgm_params_chr(f_p)
  res
}

# Export flavour: tidy, one row per fitted locality/target, numeric columns.
vgm_params_export_df <- function(v_fit_list, locs = NULL, round_values = FALSE) {
  if (is.null(v_fit_list) || length(v_fit_list) == 0) return(NULL)
  if (is.null(locs)) locs <- unique(sub("_(act|pre)$", "", names(v_fit_list)))
  rows <- list()
  for (l in locs) {
    for (tgt in c("act", "pre")) {
      f <- v_fit_list[[paste0(l, "_", tgt)]]
      if (is.null(f)) next
      p <- vgm_params_row(f, round_values = round_values)
      rows[[length(rows) + 1]] <- data.frame(
        Locality = l,
        Target = if (tgt == "act") "Actual" else "Predicted",
        Model = p$model, Kappa = p$kappa, Nugget = p$nugget, Sill = p$sill,
        `Range (a)` = p$range, `Practical Range` = p$practical_range,
        `Structural Dep. (%)` = p$sdep,
        check.names = FALSE, stringsAsFactors = FALSE)
    }
  }
  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}

# Every importance measure a randomForest recorded, one row per covariate,
# ordered by the first. A regression forest grown with importance = TRUE stores
# %IncMSE and IncNodePurity; without it, only IncNodePurity. Raw column names:
# this is the numeric record behind the labelled importance plot.
rf_importance_df <- function(rf_mod) {
  imp_mat <- randomForest::importance(rf_mod)
  out <- data.frame(Variable = rownames(imp_mat), stringsAsFactors = FALSE)
  for (cn in colnames(imp_mat)) out[[cn]] <- unname(imp_mat[, cn])
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
    if (tm == "(Intercept)") "(Intercept)" else get_var_label(tm, vars_metadata)
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

apply_labels_to_df <- function(df, vars, vars_metadata) {
  if (is.null(df) || length(vars) == 0) return(df)
  
  labels <- get_var_labels(vars, vars_metadata)
  for (i in seq_along(vars)) {
    if (vars[i] %in% colnames(df)) {
      colnames(df)[colnames(df) == vars[i]] <- labels[i]
    }
  }
  return(df)
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
        palette = "YlOrBr" 
      )
      
      new_var$palette <- get_default_palette(matched_col, cat_val, lab_val)
      
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
    if (length(q) < 4) return(factor(rep(paste0(prefix, "Low Variation"), length(x))))
    lbls <- paste0(prefix, c("Low", "Medium", "High"))
    return(cut(x, breaks = q, include.lowest = TRUE, labels = lbls))
  } else if (method == "quintiles") {
    q <- quantile(x, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
    q <- unique(q)
    if (length(q) < 6) return(factor(rep(paste0(prefix, "Low Variation"), length(x))))
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

# Per-group n / mean / sd / min / max plus a TOTAL row.
# `x` and `group` are parallel vectors. Groups are formed the way aggregate()'s
# formula interface does (rows with an NA in either vector are dropped), so
# every group statistic is computed on complete pairs, while the TOTAL row
# summarises every non-NA x regardless of its group.
desc_summary_table <- function(x, group, digits = 3) {
  agg <- stats::aggregate(x ~ group, data = data.frame(x = x, group = group),
                          FUN = function(v) c(n = length(v), mean = mean(v), sd = stats::sd(v),
                                              min = min(v), max = max(v)))
  m <- agg[, 2]
  # unname(): with a single group m[, "mean"] drops to a length-1 vector that
  # still carries the COLUMN name, which data.frame() would then adopt as the
  # row name - and DT renders row names, so the default "All" grouping showed a
  # leading column reading "mean". Every column is stripped for symmetry.
  res <- data.frame(
    Group = agg[, 1],
    Count = as.integer(m[, "n"]),
    Mean = unname(round(m[, "mean"], digits)),
    SD = unname(round(m[, "sd"], digits)),
    Min = unname(round(m[, "min"], digits)),
    Max = unname(round(m[, "max"], digits)),
    row.names = NULL
  )
  ok <- !is.na(x)
  rbind(res, data.frame(
    Group = "TOTAL",
    Count = sum(ok),
    Mean = round(mean(x[ok]), digits),
    SD = round(stats::sd(x[ok]), digits),
    Min = round(min(x[ok]), digits),
    Max = round(max(x[ok]), digits)
  ))
}

# Trend statistic per group for the scatter panel's fitted curve.
# `groups` is the label vector to report on, in order; the literal "TOTAL"
# means the whole frame rather than a subset. Groups with fewer than `min_n`
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
                                 group_col = "group_id", min_n = 5) {
  f_pval <- function(s) {
    if (is.null(s$fstatistic)) return(NA_real_)
    unname(stats::pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE))
  }
  none <- c(r2 = NA_real_, p = NA_real_)

  out <- lapply(as.character(groups), function(g) {
    sub_df <- if (identical(g, "TOTAL")) df else df[as.character(df[[group_col]]) == g, , drop = FALSE]
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

# Complete-case PCA on `vars`, with the fitted frame relabelled to `labels`.
# Returns the prcomp object, the frame it was fitted on, the row mask (so the
# caller can align a grouping vector to it) and how many rows the complete-case
# filter removed. `scale = TRUE` is a correlation PCA, FALSE a covariance PCA;
# both centre.
desc_pca_fit <- function(df, vars, labels = vars, scale = TRUE) {
  keep <- stats::complete.cases(df[, vars, drop = FALSE])
  df_clean <- df[keep, vars, drop = FALSE]
  colnames(df_clean) <- labels
  list(res = stats::prcomp(df_clean, scale. = isTRUE(scale), center = TRUE),
       data = df_clean,
       keep = keep,
       dropped = nrow(df) - nrow(df_clean))
}



# Human-readable label for the CV actually applied to a locality of n_obs
# points under the chosen strategy. Delegates to resolve_cv_plan
# (spatial_helpers.R) so the label can never disagree with the folds that were
# built, including Spatial Block's small-n degradation to LOOCV.
cv_type_label <- function(n_obs, strategy = "auto") {
  resolve_cv_plan(strategy, n_obs)$label
}

find_subset_column <- function(cols) {
  hit <- grep("^subset$", cols, ignore.case = TRUE, value = TRUE)
  if (length(hit) == 0) NA_character_ else hit[1]
}
