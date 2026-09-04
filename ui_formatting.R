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

# --- RK linear-trend presentation --------------------------------------------
# Structured replacement for the raw print(summary.lm) dump on the Scientific
# Analysis tab: compact fit-statistic chips + a publication-style coefficient
# table. Display-only: exports keep the raw numeric coefficient table.

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

# Coefficient table (estimate, SE, 95% CI, t, p, significance) from a
# summary.lm object. Term names map to display labels when variable metadata
# is supplied; the CI uses the t quantile on the residual df.
rk_coef_table <- function(lm_sum, vars_metadata = NULL, conf_level = 0.95) {
  if (is.null(lm_sum) || is.null(lm_sum$coefficients)) return(NULL)
  cf <- as.data.frame(lm_sum$coefficients)
  need <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  if (!all(need %in% colnames(cf))) return(NULL)
  df_res <- lm_sum$df[2]
  est <- cf[["Estimate"]]; se <- cf[["Std. Error"]]; p <- cf[["Pr(>|t|)"]]
  ci <- if (is.finite(df_res) && df_res >= 1) {
    tq <- stats::qt(1 - (1 - conf_level) / 2, df_res)
    sprintf("[%.4g, %.4g]", est - tq * se, est + tq * se)
  } else rep("NA", length(est))
  terms <- rownames(cf)
  labels <- vapply(terms, function(tm) {
    if (tm == "(Intercept)") "(Intercept)" else get_var_label(tm, vars_metadata)
  }, character(1))
  data.frame(
    Term = unname(labels),
    Estimate = signif(est, 4),
    `Std. Error` = signif(se, 4),
    `95% CI` = ci,
    `t value` = round(cf[["t value"]], 2),
    `p value` = vapply(p, format_p_value, character(1)),
    `Sig.` = vapply(p, signif_stars, character(1)),
    check.names = FALSE
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
