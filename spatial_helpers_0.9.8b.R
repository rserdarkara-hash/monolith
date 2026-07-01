# spatial_helpers_0.9.8a.R - Geospatial & Interpolation Engine

# Helper to robustly detect predicted and observed column names
detect_cv_columns <- function(cnames) {
  pre_col <- grep("^var1\\.pred$|^target\\.pred$|^pred$", cnames, value = TRUE)[1]
  if (is.na(pre_col)) pre_col <- grep("\\.pred$", cnames, value = TRUE)[1]
  
  obs_col <- grep("^var1\\.observed$|^observed$|^target\\.observed$", cnames, value = TRUE)[1]
  if (is.na(obs_col)) obs_col <- grep("\\.observed$", cnames, value = TRUE)[1]
  
  list(pred = pre_col, observed = obs_col)
}

# Calculate Concordance Correlation Coefficient (CCC)
calc_ccc <- function(observed, predicted) {
  if (length(observed) < 2) return(NA)
  
  mean_obs <- mean(observed, na.rm = TRUE)
  mean_pred <- mean(predicted, na.rm = TRUE)
  
  var_obs <- var(observed, na.rm = TRUE)
  var_pred <- var(predicted, na.rm = TRUE)
  
  # Handle zero-variance edge case to prevent division by zero
  if (is.na(var_obs) || is.na(var_pred) || var_obs == 0 || var_pred == 0) {
    if (mean_obs == mean_pred) return(1.0)
    return(NA)
  }
  
  sd_obs <- sqrt(var_obs)
  sd_pred <- sqrt(var_pred)
  
  cov_op <- cov(observed, predicted, use = "pairwise.complete.obs")
  rho <- cov_op / (sd_obs * sd_pred)
  
  numerator <- 2 * rho * sd_obs * sd_pred
  denominator <- var_obs + var_pred + (mean_obs - mean_pred)^2
  
  if (is.na(denominator) || denominator == 0) return(NA)
  
  ccc <- numerator / denominator
  return(ccc)
}

# --- High Precision Augmented Metrics ---
augment_metrics <- function(obs, pre) {
  res <- list(nse = NA, nrmse_mean = NA, nrmse_sd = NA, rpd = NA, rpiq = NA, smape = NA)
  if (length(obs) < 2) return(res)
  
  residuals <- obs - pre
  rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
  
  # 1. Nash-Sutcliffe Efficiency (NSE) / Traditional R2
  sst <- sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
  sse <- sum(residuals^2, na.rm = TRUE)
  res$nse <- round(1 - (sse / sst), 4)
  
  # 2. Normalized RMSE (%)
  res$nrmse_mean <- round((rmse / mean(obs, na.rm = TRUE)) * 100, 2)
  res$nrmse_sd <- round((rmse / sd(obs, na.rm = TRUE)) * 100, 2)
  
  # 3. RPD (Ratio of Performance to Deviation)
  res$rpd <- round(sd(obs, na.rm = TRUE) / rmse, 2)
  
  # 4. RPIQ (Ratio of Performance to Interquartile Range)
  iqr_obs <- IQR(obs, na.rm = TRUE)
  res$rpiq <- if(iqr_obs > 0) round(iqr_obs / rmse, 2) else NA
  
  # 5. SMAPE (Symmetric Mean Absolute Percentage Error)
  res$smape <- round(mean(2 * abs(residuals) / (abs(obs) + abs(pre)), na.rm = TRUE) * 100, 2)
  
  return(res)
}

# Calculate Moran's I for residuals
calc_moran <- function(residuals, coords) {
  if (is.null(residuals) || is.null(coords)) return(NA)
  n <- length(residuals)
  if (n < 3 || nrow(coords) != n) return(NA)
  
  tryCatch({
    # Jitter coordinates slightly if there are duplicate coordinates to prevent distance = 0 issues
    coords_matrix <- as.matrix(coords)
    if (any(duplicated(coords_matrix))) {
      coords_matrix[,1] <- jitter(coords_matrix[,1], amount = 1e-8)
      coords_matrix[,2] <- jitter(coords_matrix[,2], amount = 1e-8)
      coords <- coords_matrix
    }
    
    # Safe check to ensure we have enough rows for k=1
    knn_res <- if(nrow(coords) > 1) {
      tryCatch({ FNN::get.knn(coords, k = 1) }, error = function(e) list(nn.dist = mean(dist(coords), na.rm = TRUE)))
    } else {
      list(nn.dist = 1)
    }
    
    d_thresh <- mean(knn_res$nn.dist, na.rm = TRUE) * 5 # Wide enough to capture local neighbors
    
    # Create sparse weights using spdep
    nb <- spdep::dnearneigh(as.matrix(coords), 0, d_thresh)
    
    # Check if any points have no neighbors, if so, increase threshold slightly or handle
    if(any(spdep::card(nb) == 0)) {
       nb <- spdep::knn2nb(spdep::knearneigh(as.matrix(coords), k = min(3, nrow(coords) - 1)))
    }
    
    lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
    
    m_res <- spdep::moran.test(residuals, lw, zero.policy = TRUE, randomisation = FALSE)
    return(as.numeric(m_res$estimate[1]))
  }, error = function(e) {
    # Fallback to dense if spdep fails or not available, but with small cap
    if (n > 500) return(NA)
    dists <- as.matrix(dist(coords))
    diag(dists) <- 0
    weights <- 1 / dists
    weights[is.infinite(weights)] <- 0
    diag(weights) <- 0
    mean_res <- mean(residuals, na.rm = TRUE)
    diffs <- residuals - mean_res
    numerator <- n * sum(weights * outer(diffs, diffs), na.rm = TRUE)
    denominator <- sum(weights, na.rm = TRUE) * sum(diffs^2, na.rm = TRUE)
    return(numerator / denominator)
  })
}

# Shared CV object to dataframe converter helper
.cv_to_df <- function(cv_obj) {
  if (is.null(cv_obj)) return(NULL)
  if (inherits(cv_obj, "Spatial")) {
    as.data.frame(cv_obj)
  } else if (inherits(cv_obj, "sf")) {
    coords <- st_coordinates(cv_obj)
    df <- st_drop_geometry(cv_obj)
    df$x <- coords[, 1]
    df$y <- coords[, 2]
    df
  } else {
    as.data.frame(cv_obj)
  }
}

# Main CV Logic Abstraction
perform_cv <- function(cv_obj) {
  # Initialize results
  res <- list(rmse = NA, r2 = NA, nse = NA, me = NA, mae = NA, ccc = NA, 
              nrmse_mean = NA, rpd = NA, rpiq = NA, smape = NA, moran_i = NA, n = 0)
  
  if (is.null(cv_obj)) return(res)
  
  # Convert to standard dataframe (handles SP and SF)
  df <- .cv_to_df(cv_obj)
        
  if (nrow(df) == 0) return(res)
  cnames <- colnames(df)
  
  cols <- detect_cv_columns(cnames)
  pre_col <- cols$pred
  obs_col <- cols$observed
  
  if (is.na(pre_col) || is.na(obs_col)) return(res)
  
  observed <- df[[obs_col]]
  predicted <- df[[pre_col]]
  
  valid <- !is.na(observed) & !is.na(predicted)
  obs <- observed[valid]
  pre <- predicted[valid]
  
  if (length(obs) < 2) return(res)
  
  residuals <- obs - pre
  
  # Basic Metrics
  res$rmse <- round(sqrt(mean(residuals^2, na.rm = TRUE)), 4)
  res$me <- round(mean(residuals, na.rm = TRUE), 4)
  res$mae <- round(mean(abs(residuals), na.rm = TRUE), 4)
  r2_val <- tryCatch(cor(obs, pre)^2, error = function(e) NA)
  res$r2 <- round(r2_val, 4)
  res$n <- length(obs)
  
  # Advanced & Augmented Metrics
  res$ccc <- round(calc_ccc(obs, pre), 4)
  aug <- augment_metrics(obs, pre)
  res$nse <- aug$nse
  res$nrmse_mean <- aug$nrmse_mean
  res$rpd <- aug$rpd
  res$rpiq <- aug$rpiq
  res$smape <- aug$smape
  
  # Moran's I (requires coordinates)
  x_col <- grep("^x$|^lon|^easting", cnames, ignore.case=TRUE, value=TRUE)[1]
  y_col <- grep("^y$|^lat|^northing", cnames, ignore.case=TRUE, value=TRUE)[1]
  if (!is.na(x_col) && !is.na(y_col)) {
      coords <- df[valid, c(x_col, y_col)]
      res$moran_i <- round(calc_moran(residuals, coords), 4)
  }
  
  return(res)
}

# --- Progress File Helper ---
update_progress_file <- function(l, prefix, step, total) {
  clean_l <- gsub("[^a-zA-Z0-9_]", "_", as.character(l))
  
  # Retrieve session-specific progress directory from options (set in main server)
  progress_dir <- getOption("monolith_progress_dir", tempdir())
  session_id <- getOption("monolith_session_id", "default")
  
  # Ensure the directory exists
  if (!dir.exists(progress_dir)) {
    tryCatch(dir.create(progress_dir, recursive = TRUE, showWarnings = FALSE), error = function(e) NULL)
  }
  
  file_name <- file.path(progress_dir, paste0("progress_", session_id, "_", clean_l, "_", prefix, ".txt"))
  pct <- round((step / total) * 100)
  
  # 1. Critical: Write actual progress file
  tryCatch({
    writeLines(as.character(pct), file_name)
  }, error = function(e) {
    # Fail silently to avoid interrupting parallel execution loops
  })
  
  # 2. Optional: Write debug log (isolated from progress file output to prevent file-locking issues on Windows)
  tryCatch({
    debug_file <- file.path(progress_dir, paste0("progress_debug_", session_id, ".log"))
    write(paste0("update_progress_file: l=", l, ", prefix=", prefix, ", pct=", pct, ", file=", file_name, ", dir=", progress_dir), file = debug_file, append = TRUE)
  }, error = function(e) {
    # Fail silently if debug log is locked by another parallel process
  })
}

write_warning_file <- function(l, prefix, message) {
  clean_l <- gsub("[^a-zA-Z0-9_]", "_", as.character(l))
  progress_dir <- getOption("monolith_progress_dir", tempdir())
  session_id <- getOption("monolith_session_id", "default")
  
  if (!dir.exists(progress_dir)) {
    tryCatch(dir.create(progress_dir, recursive = TRUE, showWarnings = FALSE), error = function(e) NULL)
  }
  
  file_name <- file.path(progress_dir, paste0("warn_", session_id, "_", clean_l, "_", prefix, ".txt"))
  tryCatch({
    writeLines(as.character(message), file_name)
  }, error = function(e) NULL)
}

# --- Unified Kriging Full-Pipeline CV ---
perform_kriging_loocv <- function(pts, target_var, aux_vars, lags_func, vgm_fit_func, model_type = c("lm", "rf"), l = "region", prefix = "act") {
  model_type <- match.arg(model_type)
  pts <- pts[complete.cases(sf::st_drop_geometry(pts)[, c(target_var, aux_vars), drop=FALSE]), ]
  n <- nrow(pts)
  if (n < 3) return(NULL)
  form_reg <- as.formula(paste0("`", target_var, "` ~ ", paste(paste0("`", aux_vars, "`"), collapse = " + ")))
  
  # Track original point indices to prevent spatial mismatch when binding folds back
  pts$orig_idx <- seq_len(n)
  
  # Adaptive K-Fold CV: If n > 50, use 10-fold CV instead of LOOCV to speed up computation by 30x
  if (n > 50) {
    k <- 10
    set.seed(12345) # for scientific reproducibility
    folds <- sample(cut(seq_len(n), breaks = k, labels = FALSE))
    
    results_list <- lapply(seq_len(k), function(i) {
      test_idx <- which(folds == i)
      train <- pts[-test_idx, ]; test <- pts[test_idx, ]
      
      if (model_type == "lm") {
        lm_mod <- lm(form_reg, data = train)
        train$residuals <- residuals(lm_mod)
        pred_trend <- predict(lm_mod, newdata = test)
      } else {
        # Speed up CV forest evaluation by using 50 trees instead of 100
        rf_mod <- randomForest::randomForest(form_reg, data = train, ntree = 50)
        train$residuals <- train[[target_var]] - rf_mod$predicted
        pred_trend <- predict(rf_mod, test)
      }
      
      lags <- lags_func(train)
      v_emp <- variogram(residuals ~ 1, train, width = lags$width, cutoff = lags$cutoff)
      v_fit <- vgm_fit_func(v_emp, train$residuals)
      res_krig <- krige(residuals ~ 1, train, test, model = v_fit, debug.level = 0)
      
      # Keep sf geometry and construct proper spatial sf object for this fold
      fold_sf <- test[, c("orig_idx", target_var), drop = FALSE]
      names(fold_sf)[names(fold_sf) == target_var] <- "observed"
      fold_sf$var1.pred <- as.numeric(pred_trend) + res_krig$var1.pred
      fold_sf$residual <- fold_sf$observed - fold_sf$var1.pred
      fold_sf
    })
  } else {
    # Traditional LOOCV for small datasets (n <= 50)
    results_list <- lapply(1:n, function(i) {
      train <- pts[-i, ]; test <- pts[i, ]
      
      if (model_type == "lm") {
        lm_mod <- lm(form_reg, data = train)
        train$residuals <- residuals(lm_mod)
        pred_trend <- predict(lm_mod, newdata = test)
      } else {
        rf_mod <- randomForest::randomForest(form_reg, data = train, ntree = 100)
        train$residuals <- train[[target_var]] - rf_mod$predicted
        pred_trend <- predict(rf_mod, test)
      }
      
      lags <- lags_func(train)
      v_emp <- variogram(residuals ~ 1, train, width = lags$width, cutoff = lags$cutoff)
      v_fit <- vgm_fit_func(v_emp, train$residuals)
      res_krig <- krige(residuals ~ 1, train, test, model = v_fit, debug.level = 0)
      
      # Keep sf geometry and construct proper spatial sf object for this fold
      fold_sf <- test[, c("orig_idx", target_var), drop = FALSE]
      names(fold_sf)[names(fold_sf) == target_var] <- "observed"
      fold_sf$var1.pred <- as.numeric(pred_trend) + res_krig$var1.pred
      fold_sf$residual <- fold_sf$observed - fold_sf$var1.pred
      fold_sf
    })
  }
  
  # Combine folds back using sf S3 rbind
  res_combined <- do.call(rbind, results_list)
  
  # Sort back by original index to eliminate spatial mismatch
  res_combined <- res_combined[order(res_combined$orig_idx), ]
  res_combined$orig_idx <- NULL
  
  return(res_combined)
}


# --- Adaptive LMC Model Suggester ---
suggest_lmc_model <- function(primary_vgm) {
  if (is.null(primary_vgm)) return("Sph")
  m_type <- as.character(primary_vgm$model[primary_vgm$model != "Nug"])
  if (length(m_type) == 0) return("Sph")
  return(m_type[1])
}

# --- High-Precision IDW Optimization ---
optimize_idw_p <- function(pts, target_var, nmax = 12) {
  factors <- seq(0.5, 5.0, by = 0.5)
  form <- as.formula(paste0("`", target_var, "` ~ 1"))
  
  # For speed, use 5-fold CV instead of LOOCV if n > 50
  folds <- if(nrow(pts) > 50) 5 else nrow(pts)

  rmses <- sapply(factors, function(f) {
    cv <- tryCatch({ krige.cv(form, pts, nmax = nmax, set = list(idp = f), nfold = folds, debug.level = 0) }, error = function(e) NULL)
    if(!is.null(cv)) {
      return(sqrt(mean(cv$residual^2, na.rm = TRUE)))
    } else {
      return(Inf)
    }
  })
  
  best_idx <- which.min(rmses)
  if(length(best_idx) > 0 && rmses[best_idx] != Inf) {
    return(factors[best_idx])
  }
  return(2.0)
}

# --- Central Collinearity Engine ---
detect_multicollinearity_engine <- function(df, vars = NULL, vif_threshold = 10, pairwise_threshold = 0.95) {
  if (is.null(vars)) {
    df_num <- df[sapply(df, is.numeric)]
    vars <- colnames(df_num)
  } else {
    df_num <- df[, vars, drop = FALSE]
    df_num <- df_num[sapply(df_num, is.numeric)]
    vars <- colnames(df_num)
  }
  
  kept <- vars
  dropped <- c()
  
  collinear_pairs <- data.frame(var1 = character(), var2 = character(), r = numeric(), stringsAsFactors = FALSE)
  has_collinearity <- FALSE
  
  if (length(kept) >= 2) {
    df_clean <- na.omit(df[, kept, drop = FALSE])
    if (nrow(df_clean) >= 3) {
      vars_var <- sapply(df_clean, var)
      valid_vars <- kept[vars_var > 1e-6]
      
      if (length(valid_vars) >= 2) {
        cormat <- cor(df_clean[, valid_vars], use = "pairwise.complete.obs")
        
        for (i in 1:(length(valid_vars) - 1)) {
          for (j in (i + 1):length(valid_vars)) {
            if (abs(cormat[i, j]) > pairwise_threshold) {
              collinear_pairs <- rbind(collinear_pairs, data.frame(
                var1 = valid_vars[i], 
                var2 = valid_vars[j], 
                r = cormat[i, j],
                stringsAsFactors = FALSE
              ))
            }
          }
        }
        has_collinearity <- nrow(collinear_pairs) > 0
      }
    }
  }
  
  if (length(kept) >= 2) {
    repeat {
      if (length(kept) == 1) {
        warning("VIF Iterative Pruning: only one covariate remains. Multicollinearity is extremely high.")
        break
      }
      if (length(kept) < 2) break
      df_clean_vif <- na.omit(df[, kept, drop = FALSE])
      if (nrow(df_clean_vif) < 3) break
      
      cor_mat <- cor(df_clean_vif, use = "pairwise.complete.obs")
      vif_vals <- tryCatch({ diag(solve(cor_mat)) }, error = function(e) { NULL })
      
      if (is.null(vif_vals)) {
        cor_mat_no_diag <- cor_mat
        diag(cor_mat_no_diag) <- 0
        max_idx <- which(abs(cor_mat_no_diag) == max(abs(cor_mat_no_diag)), arr.ind = TRUE)[1,]
        var_to_drop <- kept[max_idx[1]]
        dropped <- c(dropped, var_to_drop)
        kept <- setdiff(kept, var_to_drop)
        next
      }
      
      max_vif <- max(vif_vals)
      if (max_vif > vif_threshold) {
        var_to_drop <- names(vif_vals)[which.max(vif_vals)]
        dropped <- c(dropped, var_to_drop)
        kept <- setdiff(kept, var_to_drop)
      } else {
        break
      }
    }
  }
  
  return(list(
    has_collinearity = has_collinearity,
    pairs = if (nrow(collinear_pairs) > 0) collinear_pairs else NULL,
    kept = kept,
    dropped = dropped
  ))
}

# --- Collinearity Check (VIF) Wrapper ---
check_vif <- function(df, threshold = 10) {
  res <- detect_multicollinearity_engine(df, vif_threshold = threshold)
  return(list(kept = res$kept, dropped = res$dropped))
}

# --- Shared Covariate-Kriging Helper ---
krige_covariates <- function(data, grid_p, aux_vars, lags, method_params) {
  grid_aux <- grid_p
  log_msg <- ""
  for(av in aux_vars) {
    kr_res <- tryCatch({
      v_emp_av <- variogram(as.formula(paste0("`", av, "` ~ 1")), data, width = lags$width, cutoff = lags$cutoff)
      fit_av <- robust_vgm_fit(v_emp_av, data[[av]])
      res_av <- krige(as.formula(paste0("`", av, "` ~ 1")), data, grid_p, model = fit_av, debug.level = 0)
      list(pred = res_av$var1.pred, warn = NULL)
    }, error = function(e) {
      warn_msg <- sprintf(" [WARN] Covariate %s kriging failed, falling back to IDW. ", av)
      idw_p <- if(!is.null(method_params$idw_p)) method_params$idw_p else 2
      idw_nmax <- if(!is.null(method_params$idw_nmax)) method_params$idw_nmax else 12
      res_av <- idw(as.formula(paste0("`", av, "` ~ 1")), data, grid_p, nmax = idw_nmax, idp = idw_p, debug.level = 0)
      list(pred = res_av$var1.pred, warn = warn_msg)
    })
    grid_aux[[av]] <- kr_res$pred
    if (!is.null(kr_res$warn)) {
      log_msg <- paste0(log_msg, kr_res$warn)
    }
  }
  return(list(grid_aux = grid_aux, log_msg = log_msg))
}

# --- Helper to Extract CV Residuals ---
get_cv_residuals <- function(cv_obj, n_rows) {
  if (is.null(cv_obj)) return(rep(NA_real_, n_rows))
  df <- .cv_to_df(cv_obj)
  cnames <- colnames(df)
  cols <- detect_cv_columns(cnames)
  pre_col <- cols$pred
  obs_col <- cols$observed
  
  if (is.na(pre_col) || is.na(obs_col)) {
    res_col <- grep("^residual$", cnames, ignore.case = TRUE, value = TRUE)[1]
    if (!is.na(res_col)) return(df[[res_col]])
    return(rep(NA_real_, n_rows))
  }
  return(df[[obs_col]] - df[[pre_col]])
}

# --- Method-Specific Interpolation Helpers ---

init_interpolation_res <- function() {
  list(v_emp = NULL, fit = NULL, cv_metrics = NULL, model_summary = NULL, 
       rf_model = NULL, gstat_obj = NULL, res_sf = NULL, log_msg = "", cv_obj = NULL, residuals = NULL)
}

safe_run_cv <- function(res, expr, label, n_data) {
  cv_obj <- tryCatch({
    expr
  }, error = function(e) {
    err <- list(error_msg = paste0(label, " CV Error: ", e$message))
    class(err) <- "cv_error"
    err
  })
  
  if (inherits(cv_obj, "cv_error")) {
    res$log_msg <- paste0(res$log_msg, cv_obj$error_msg)
    cv_obj <- NULL
  }
  
  res$cv_obj <- cv_obj
  res$cv_metrics <- perform_cv(cv_obj)
  if (!is.null(cv_obj)) {
    res$residuals <- get_cv_residuals(cv_obj, n_data)
  }
  return(res)
}

sanitize_spatial_predictions <- function(res_sf) {
  if (!is.null(res_sf)) {
    if ("var1.pred" %in% colnames(res_sf)) {
      res_sf$var1.pred[is.nan(res_sf$var1.pred) | is.infinite(res_sf$var1.pred)] <- NA
    }
    if ("var1.var" %in% colnames(res_sf)) {
      res_sf$var1.var[is.nan(res_sf$var1.var) | is.infinite(res_sf$var1.var)] <- NA
    }
  }
  return(res_sf)
}

# --- Centralized Kriging Interpolation Pipeline ---
apply_kriging_pipeline <- function(engine = c("OK", "RK", "RFK"), data, target_var, grid_p, lags, method_params, aux_vars = NULL, l = "region", prefix = "act") {
  engine <- match.arg(engine)
  res <- init_interpolation_res()
  
  if (engine == "OK") {
    update_progress_file(l, prefix, 20, 100)
    form_ok <- reformulate("1", response = target_var)
    res$v_emp <- variogram(form_ok, data, width = lags$width, cutoff = lags$cutoff)
    res$fit <- if(!is.null(method_params$pre_fit)) method_params$pre_fit else robust_vgm_fit(res$v_emp, data[[target_var]])
    
    update_progress_file(l, prefix, 50, 100)
    folds_count <- if (nrow(data) > 50) 10 else nrow(data)
    res <- safe_run_cv(res, krige.cv(form_ok, data, model = res$fit, nfold = folds_count, debug.level = 0), "OK", nrow(data))
    res$res_sf <- krige(form_ok, data, grid_p, model = res$fit, debug.level = 0)
  } else {
    update_progress_file(l, prefix, 10, 100)
    krig_res <- tryCatch({
      if (engine == "RK" && length(aux_vars) > 1) {
        vif_res <- check_vif(st_drop_geometry(data)[, aux_vars, drop = FALSE])
        if (length(vif_res$dropped) > 0) {
          res$log_msg <- paste0(res$log_msg, " [VIF] Dropped: ", paste(vif_res$dropped, collapse=", "))
          aux_vars <- vif_res$kept
        }
      }
      
      if (!is.null(method_params$grid_aux)) {
        grid_aux <- method_params$grid_aux
      } else {
        krig_cov <- krige_covariates(data, grid_p, aux_vars, lags, method_params)
        grid_aux <- krig_cov$grid_aux
        res$log_msg <- paste0(res$log_msg, krig_cov$log_msg)
      }
      
      form_reg <- as.formula(paste(paste0("`", target_var, "`"), "~", paste(paste0("`", aux_vars, "`"), collapse = " + ")))
      
      if (engine == "RK") {
        lm_mod <- lm(form_reg, data = data)
        res$model_summary <- summary(lm_mod)
        
        data$residuals <- residuals(lm_mod)
        res$residuals <- residuals(lm_mod)
        
        res$v_emp <- variogram(residuals ~ 1, data, width = lags$width, cutoff = lags$cutoff)
        res$fit <- robust_vgm_fit(res$v_emp, data$residuals)
        res_krig <- krige(residuals ~ 1, data, grid_p, model = res$fit, debug.level = 0)
        
        pred_trend <- predict(lm_mod, newdata = grid_aux, se.fit = TRUE)
        trend_var <- (pred_trend$se.fit)^2
        
        res$res_sf <- grid_p %>% mutate(
          var1.pred = as.vector(pred_trend$fit + res_krig$var1.pred), 
          var1.var = as.vector(trend_var + res_krig$var1.var)
        )
        res <- safe_run_cv(res, perform_kriging_loocv(data, target_var, aux_vars, calc_scientific_lags, robust_vgm_fit, model_type = "lm", l, prefix), "RK", nrow(data))
      } else if (engine == "RFK") {
        rf_mod <- randomForest::randomForest(form_reg, data = data, ntree = 200, importance = TRUE)
        res$rf_model <- rf_mod
        
        residuals_val <- data[[target_var]] - rf_mod$predicted
        data$residuals <- residuals_val
        res$residuals <- residuals_val
        
        res$v_emp <- variogram(residuals ~ 1, data, width = lags$width, cutoff = lags$cutoff)
        res$fit <- robust_vgm_fit(res$v_emp, data$residuals)
        res_krig <- krige(residuals ~ 1, data, grid_p, model = res$fit, debug.level = 0)
        
        pred_trend_all <- predict(rf_mod, grid_aux, predict.all = TRUE)
        M <- pred_trend_all$individual
        trend_var <- rowSums((M - rowMeans(M))^2) / (ncol(M) - 1)
        
        res$res_sf <- grid_p %>% mutate(
          var1.pred = as.vector(pred_trend_all$aggregate + res_krig$var1.pred), 
          var1.var = as.vector(trend_var + res_krig$var1.var)
        )
        res <- safe_run_cv(res, perform_kriging_loocv(data, target_var, aux_vars, calc_scientific_lags, robust_vgm_fit, model_type = "rf", l, prefix), "RFK", nrow(data))
      }
      res
    }, error = function(e) {
      res$log_msg <- paste0(res$log_msg, "\n", engine, " failed: ", e$message, ". Falling back to OK.")
      res$res_sf <- NULL
      res
    })
    res <- krig_res
    
    if (is.null(res$res_sf)) {
      write_warning_file(l, prefix, paste0(engine, " failed, using Ordinary Kriging fallback."))
      form_ok <- reformulate("1", response = target_var)
      res$v_emp <- variogram(form_ok, data, width = lags$width, cutoff = lags$cutoff)
      res$fit <- robust_vgm_fit(res$v_emp, data[[target_var]])
      res$res_sf <- krige(form_ok, data, grid_p, model = res$fit, debug.level = 0)
      folds_count <- if (nrow(data) > 50) 10 else nrow(data)
      res <- safe_run_cv(res, krige.cv(form_ok, data, model = res$fit, nfold = folds_count, debug.level = 0), paste0(engine, " OK Fallback"), nrow(data))
    }
  }
  
  res$res_sf <- sanitize_spatial_predictions(res$res_sf)
  update_progress_file(l, prefix, 100, 100)
  return(res)
}

# --- Ordinary Kriging API ---
apply_OK <- function(data, target_var, grid_p, lags, method_params, l = "region", prefix = "act") {
  apply_kriging_pipeline("OK", data, target_var, grid_p, lags, method_params, NULL, l, prefix)
}

# --- Regression Kriging API ---
apply_RK <- function(data, target_var, grid_p, lags, method_params, aux_vars, l = "region", prefix = "act") {
  apply_kriging_pipeline("RK", data, target_var, grid_p, lags, method_params, aux_vars, l, prefix)
}

# --- Random Forest Kriging API ---
apply_RFK <- function(data, target_var, grid_p, lags, method_params, aux_vars, l = "region", prefix = "act") {
  apply_kriging_pipeline("RFK", data, target_var, grid_p, lags, method_params, aux_vars, l, prefix)
}

apply_CK <- function(data, target_var, grid_p, lags, method_params, aux_vars, l = "region", prefix = "act") {
  res <- init_interpolation_res()
  
  update_progress_file(l, prefix, 10, 100)
  
  ck_res <- tryCatch({
    data_scaled <- data
    for(av in aux_vars) {
      data_scaled[[av]] <- scale(data_scaled[[av]])
    }
    
    form_ok <- reformulate("1", response = target_var)
    g <- gstat(NULL, id = target_var, formula = form_ok, data = data_scaled)
    for(av in aux_vars) {
      g <- gstat(g, id = av, formula = as.formula(paste0("`", av, "` ~ 1")), data = data_scaled)
    }
    
    vm <- variogram(g, width = lags$width, cutoff = lags$cutoff)
    
    v_emp_ok <- variogram(form_ok, data_scaled, width = lags$width, cutoff = lags$cutoff)
    fit_ok_init <- robust_vgm_fit(v_emp_ok, data_scaled[[target_var]])
    m_type <- suggest_lmc_model(fit_ok_init)
    
    g_or_err <- tryCatch({
      fit.lmc(vm, g, vgm(var(data_scaled[[target_var]]), m_type, lags$cutoff / 2, 0), correct.diagonal = 1.01)
    }, error = function(e) {
      list(error_msg = paste0("LMC Fit Failed: ", e$message, ". Falling back to OK."))
    })
    
    if (is.list(g_or_err) && !is.null(g_or_err$error_msg)) {
      res$log_msg <- paste0(res$log_msg, g_or_err$error_msg)
      g <- NULL
      write_warning_file(l, prefix, "LMC model fit failed, using Ordinary Kriging fallback.")
    } else {
      g <- g_or_err
    }
    
    if(!is.null(g)) {
      res$gstat_obj <- g
      res <- safe_run_cv(res, {
        folds_count <- if (nrow(data) > 50) 10 else nrow(data)
        cv_val <- gstat.cv(g, nfold = folds_count, debug.level = 0)
        if (!is.null(cv_val)) {
          cnames <- names(cv_val)
          pred_col_src <- paste0(target_var, ".pred")
          obs_col_src <- paste0(target_var, ".observed")
          
          if (pred_col_src %in% cnames) {
            names(cv_val)[names(cv_val) == pred_col_src] <- "var1.pred"
          }
          if (obs_col_src %in% cnames) {
            names(cv_val)[names(cv_val) == obs_col_src] <- "var1.observed"
          }
        }
        cv_val
      }, "CK", nrow(data))
      
      res_sf_or_err <- tryCatch({
        pred_obj <- predict(g, grid_p, debug.level = 0) %>% st_as_sf()
        pred_col <- paste0(target_var, ".pred")
        var_col <- paste0(target_var, ".var")
        pred_obj %>% dplyr::rename(var1.pred = !!rlang::sym(pred_col), var1.var = !!rlang::sym(var_col))
      }, error = function(e) {
        list(error_msg = paste0("CK Prediction Failed: ", e$message, ". Falling back to OK."))
      })
      
      if (is.list(res_sf_or_err) && !is.null(res_sf_or_err$error_msg)) {
        res$log_msg <- paste0(res$log_msg, res_sf_or_err$error_msg)
        res$res_sf <- NULL
      } else {
        res$res_sf <- res_sf_or_err
      }
    }
    res
  }, error = function(e) {
    res$log_msg <- paste0(res$log_msg, "\nCK failed: ", e$message, ". Falling back to OK.")
    res$res_sf <- NULL
    res
  })
  
  res <- ck_res
  
  if(is.null(res$res_sf)) {
    write_warning_file(l, prefix, "CK failed, using Ordinary Kriging fallback.")
    form_ok <- reformulate("1", response = target_var)
    v_emp_ok <- variogram(form_ok, data, width = lags$width, cutoff = lags$cutoff)
    fit_ok <- robust_vgm_fit(v_emp_ok, data[[target_var]])
    res$v_emp <- v_emp_ok
    res$fit <- fit_ok
    res$res_sf <- krige(form_ok, data, grid_p, model = fit_ok, debug.level = 0)
    folds_count <- if (nrow(data) > 50) 10 else nrow(data)
    res <- safe_run_cv(res, krige.cv(form_ok, data, model = fit_ok, nfold = folds_count, debug.level = 0), "OK Fallback", nrow(data))
  }
  
  res$res_sf <- sanitize_spatial_predictions(res$res_sf)
  
  update_progress_file(l, prefix, 100, 100)
  return(res)
}

apply_IDW <- function(data, target_var, grid_p, method_params, l = "region", prefix = "act") {
  res <- init_interpolation_res()
  
  update_progress_file(l, prefix, 20, 100)
  form_ok <- reformulate("1", response = target_var)
  folds_count <- if (nrow(data) > 50) 10 else nrow(data)
  res <- safe_run_cv(res, krige.cv(form_ok, data, nmax = method_params$idw_nmax, set = list(idp = method_params$idw_p), nfold = folds_count, debug.level = 0), "IDW", nrow(data))
  
  res$res_sf <- idw(form_ok, data, grid_p, nmax = method_params$idw_nmax, idp = method_params$idw_p, debug.level = 0)
  res$res_sf <- sanitize_spatial_predictions(res$res_sf)
  
  update_progress_file(l, prefix, 100, 100)
  return(res)
}

apply_TPS <- function(data, target_var, grid_p, method_params, l = "region", prefix = "act") {
  res <- init_interpolation_res()
  
  update_progress_file(l, prefix, 10, 100)
  res$res_sf <- tryCatch({
    raw_pts <- st_coordinates(data)
    xm <- min(raw_pts[,1]); xM <- max(raw_pts[,1])
    ym <- min(raw_pts[,2]); yM <- max(raw_pts[,2])
    max_range <- max(xM - xm, yM - ym)
    if(max_range == 0) max_range <- 1
    pts_sc <- cbind((raw_pts[,1]-xm)/max_range, (raw_pts[,2]-ym)/max_range)
    gr_raw <- st_coordinates(grid_p)
    gr_sc <- cbind((gr_raw[,1]-xm)/max_range, (gr_raw[,2]-ym)/max_range)
    mod <- fields::Tps(pts_sc, data[[target_var]], lambda = method_params$tps_lambda)
    p_v <- fields::predict.Krig(mod, gr_sc)
    
    n_pts <- nrow(data)
    update_progress_file(l, prefix, 40, 100)
    if (n_pts > 50) {
      k <- 10
      set.seed(12345)
      folds <- sample(cut(seq_len(n_pts), breaks = k, labels = FALSE))
      cv_vals <- numeric(n_pts)
      for (i in seq_len(k)) {
        test_idx <- which(folds == i)
        tmp_mod <- tryCatch({
          fields::Tps(pts_sc[-test_idx, , drop=FALSE], data[[target_var]][-test_idx], lambda = method_params$tps_lambda)
        }, error = function(e) NULL)
        
        if (!is.null(tmp_mod)) {
          cv_vals[test_idx] <- as.numeric(fields::predict.Krig(tmp_mod, pts_sc[test_idx, , drop=FALSE]))
        } else {
          cv_vals[test_idx] <- NA_real_
        }
      }
    } else {
      cv_vals <- vapply(1:n_pts, function(i) {
        tryCatch({
          tmp_mod <- fields::Tps(pts_sc[-i, , drop=FALSE], data[[target_var]][-i], lambda = method_params$tps_lambda)
          as.numeric(fields::predict.Krig(tmp_mod, pts_sc[i, , drop=FALSE]))
        }, error = function(e) NA_real_)
      }, numeric(1))
    }
    
    
    cv_res <- data.frame(observed = data[[target_var]], var1.pred = cv_vals, x = raw_pts[,1], y = raw_pts[,2])
    res$cv_obj <- cv_res
    res$cv_metrics <- perform_cv(cv_res)
    res$residuals <- get_cv_residuals(cv_res, nrow(data))
    
    grid_p %>% mutate(var1.pred = as.vector(p_v))
  }, error = function(e) {
    idw_p <- if(!is.null(method_params$idw_p)) method_params$idw_p else 2
    idw_nmax <- if(!is.null(method_params$idw_nmax)) method_params$idw_nmax else 12
    form_ok <- reformulate("1", response = target_var)
    idw_res <- idw(form_ok, data, grid_p, nmax = idw_nmax, idp = idw_p, debug.level = 0)
    
    folds_count <- if (nrow(data) > 50) 10 else nrow(data)
    cv_obj_fallback <- tryCatch({
      krige.cv(form_ok, data, nmax = idw_nmax, set = list(idp = idw_p), nfold = folds_count, debug.level = 0)
    }, error = function(e) NULL)
    res$cv_obj <- cv_obj_fallback
    res$cv_metrics <- perform_cv(cv_obj_fallback)
    res$residuals <- get_cv_residuals(cv_obj_fallback, nrow(data))
    
    idw_res
  })
  
  res$res_sf <- sanitize_spatial_predictions(res$res_sf)
  
  update_progress_file(l, prefix, 100, 100)
  return(res)
}

# --- Main Dispatcher apply_interpolation ---
apply_interpolation <- function(data, target_var, method, grid_p, aux_vars, lags, method_params, l, prefix) {
  res <- tryCatch({
    if(method == "OK") {
      apply_OK(data, target_var, grid_p, lags, method_params, l, prefix)
    } else if(method == "RK" && length(aux_vars) > 0) {
      apply_RK(data, target_var, grid_p, lags, method_params, aux_vars, l, prefix)
    } else if(method == "RFK" && length(aux_vars) > 0) {
      apply_RFK(data, target_var, grid_p, lags, method_params, aux_vars, l, prefix)
    } else if(method == "CK" && length(aux_vars) > 0) {
      apply_CK(data, target_var, grid_p, lags, method_params, aux_vars, l, prefix)
    } else if(method == "IDW") {
      apply_IDW(data, target_var, grid_p, method_params, l, prefix)
    } else if(method == "TPS") {
      apply_TPS(data, target_var, grid_p, method_params, l, prefix)
    } else {
      stop("Unknown interpolation method: ", method)
    }
  }, error = function(e) {
    list(
      v_emp = NULL, fit = NULL, cv_metrics = NULL, model_summary = NULL, 
      rf_model = NULL, gstat_obj = NULL, res_sf = NULL, 
      log_msg = paste0("Error in apply_interpolation: ", e$message), cv_obj = NULL, residuals = NULL
    )
  })
  
  return(res)
}

# --- Global Scale Synchronization ---
get_joint_scale_values <- function(r1_packed, r2_packed, match_scales, is_uncertainty) {
  if(match_scales && !is_uncertainty) {
    v1 <- if(!is.null(r1_packed)) as.vector(terra::values(terra::unwrap(r1_packed), na.rm=TRUE)) else NULL
    v2 <- if(!is.null(r2_packed)) as.vector(terra::values(terra::unwrap(r2_packed), na.rm=TRUE)) else NULL
    res <- c(v1, v2)
    if(length(res) == 0) return(NULL)
    return(res)
  }
  return(NULL)
}

# Sourced from global.R

# --- Analytical Backend for Governing Factors ---
compute_governing_factors <- function(df, target_col, predictors, n_permutations = 10) {
  # 1. Prepare data
  req_cols <- c(target_col, predictors)
  df_clean <- df[, req_cols, drop = FALSE]
  df_clean <- df_clean[complete.cases(df_clean), , drop = FALSE]
  
  if (nrow(df_clean) < 10) return(NULL) # Not enough data
  
  # 2. Fit Random Forest
  formula_str <- paste(target_col, "~ .")
  set.seed(12345)
  rf_model <- randomForest::randomForest(as.formula(formula_str), data = df_clean, ntree = 100, importance = TRUE)
  
  # 3. Create DALEX explainer
  explainer_rf <- DALEX::explain(
    model = rf_model, 
    data = df_clean[, predictors, drop = FALSE], 
    y = df_clean[[target_col]], 
    label = "Random Forest",
    verbose = FALSE
  )
  
  # 4. Global Importance
  vip <- DALEX::model_parts(explainer_rf, B = n_permutations)
  
  vip_df <- as.data.frame(vip)
  vip_df <- vip_df[vip_df$variable != "_baseline_" & vip_df$variable != "_full_model_", ]
  vip_agg <- aggregate(dropout_loss ~ variable, data = vip_df, FUN = mean)
  top_var <- as.character(vip_agg$variable[which.max(vip_agg$dropout_loss)])
  
  # 5. ALE Profile for top variable
  ale_prof <- DALEX::model_profile(explainer_rf, variables = top_var, type = "accumulated")
  ale_df <- as.data.frame(ale_prof$agr_profiles)

  # 6. PDP (Partial Dependence Plot) for top variable
  pdp_prof <- DALEX::model_profile(explainer_rf, variables = top_var, type = "partial")
  pdp_df <- as.data.frame(pdp_prof$agr_profiles)

  # 7. SHAP profile (using sample) - used for Causality/Interaction (A)
  set.seed(12345)
  sample_idx <- sample(1:nrow(df_clean), min(20, nrow(df_clean)))
  shap_list <- lapply(sample_idx, function(i) {
    sp <- DALEX::predict_parts(explainer_rf, new_observation = df_clean[i, predictors, drop = FALSE], type = "shap")
    sp <- as.data.frame(sp)
    sp$obs_id <- i
    sp
  })
  shap_df <- do.call(rbind, shap_list)

  # To make a scatterplot, we need the original feature values and the shap contribution
  shap_val_df <- data.frame(
    feature_value = df_clean[[top_var]][sample_idx],
    contribution = sapply(sample_idx, function(i) {
      sub <- shap_df[shap_df$obs_id == i & shap_df$variable_name == top_var, ]
      if(nrow(sub) > 0) sum(sub$contribution) else 0
    })
  )

  list(
    model = rf_model,
    explainer = explainer_rf,
    importance = vip_agg,
    top_var = top_var,
    ale = ale_df,
    pdp = pdp_df,
    shap = shap_val_df
  )
}

# Helper to merge lists of wrapped/unwrapped spatial rasters
merge_wrapped_rasters <- function(raster_list) {
  if (is.null(raster_list) || length(raster_list) == 0) return(NULL)
  valid_list <- Filter(Negate(is.null), raster_list)
  if (length(valid_list) == 0) return(NULL)
  
  unwrap_if_needed <- function(r) {
    if (inherits(r, "PackedSpatRaster")) terra::unwrap(r) else r
  }
  
  merged <- if (length(valid_list) > 1) {
    do.call(terra::merge, lapply(unname(valid_list), unwrap_if_needed))
  } else {
    unwrap_if_needed(valid_list[[1]])
  }
  merged
}

# --- Centralized Spatial Projection Helper (Issue a) ---
validate_and_project_sf <- function(pts_sf, current_crs = NULL) {
  if (is.null(pts_sf) || nrow(pts_sf) == 0) return(NULL)
  
  if (sf::st_is_longlat(pts_sf)) {
    coords_4326 <- sf::st_coordinates(sf::st_transform(pts_sf, 4326))
    lon_c <- mean(coords_4326[, 1], na.rm = TRUE)
    lat_c <- mean(coords_4326[, 2], na.rm = TRUE)
    if (is.na(lon_c) || is.na(lat_c)) {
      stop("Calculated geographic center contains NA.")
    }
    utm_zone <- floor((lon_c + 180) / 6) + 1
    utm_crs <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m +no_defs")
    if (lat_c < 0) utm_crs <- paste0(utm_crs, " +south")
    
    pts_sf <- sf::st_transform(pts_sf, utm_crs)
  }
  
  return(pts_sf)
}

# --- Wrapped regional model engine dispatcher (Issue c) ---
run_regional_interpolation <- function(item, current_method, current_crs, aux_vars, shp_bound, b_type, buff_mode, b_dist, res_mode, grid_res, crs_sel, comp_mode, val_type, progress_dir_val = tempdir(), session_id_val = "default", cancel_file_val = NULL) {
  options(monolith_progress_dir = progress_dir_val)
  options(monolith_session_id = session_id_val)
  
  if (!is.null(cancel_file_val) && file.exists(cancel_file_val)) {
    stop("Model generation cancelled by user.")
  }
  
  l <- item$l
  pts_data <- item$pts_data
  m_params <- item$m_params        
  
  res_out <- list(l = l, r_a = NULL, r_p = NULL, r_res = NULL, bound = NULL, pts = NULL, 
                  v_emp_act = NULL, v_fit_act = NULL, cv_act = NULL, cv_obj_act = NULL, summ_act = NULL, rf_act = NULL, gstat_act = NULL,
                  v_emp_pre = NULL, v_fit_pre = NULL, cv_pre = NULL, cv_obj_pre = NULL, summ_pre = NULL, rf_pre = NULL, gstat_pre = NULL, log_msg = "", actual_res = NULL)
  
  res_out <- tryCatch({
    # Ensure x and y coordinates are numeric
    if (!is.numeric(pts_data$x)) pts_data$x <- as.numeric(as.character(pts_data$x))
    if (!is.numeric(pts_data$y)) pts_data$y <- as.numeric(as.character(pts_data$y))
    
    pts_raw <- pts_data %>% dplyr::filter(!is.na(x), !is.na(y))
    if (nrow(pts_raw) < 3) {
      res_out$log_msg <- paste0("Warning in ", l, ": Insufficient data points after cleaning (needed >= 3, got ", nrow(pts_raw), ").")
      return(res_out)
    }
    
    pts_raw <- pts_raw %>% sf::st_as_sf(coords=c("x","y"), crs=current_crs)
    if (current_method %in% c("RK", "RFK", "CK") && length(aux_vars) > 0) {
       pts_raw <- pts_raw %>% dplyr::filter(dplyr::if_all(dplyr::all_of(aux_vars), ~!is.na(.)))
    }
    
    if (nrow(pts_raw) < 3) {
      res_out$log_msg <- paste0("Warning in ", l, ": Insufficient data points after covariate filtering (needed >= 3, got ", nrow(pts_raw), ").")
      return(res_out)
    }
    
    # Centralized Spatial Projection Handling (Issue a)
    pts_projected <- validate_and_project_sf(pts_raw)
    utm_crs <- sf::st_crs(pts_projected)$wkt
    pts <- pts_projected
    
    if(nrow(pts) < 3) {
      res_out$log_msg <- paste0("Warning in ", l, ": Insufficient data points after UTM conversion (needed >= 3, got ", nrow(pts), ").")
      return(res_out)
    }
    
    coords <- sf::st_coordinates(pts)
    c_round <- data.frame(
      x = round(coords[, "X"], 2),
      y = round(coords[, "Y"], 2)
    )
    pts <- pts[!duplicated(c_round), ]
    if(nrow(pts) < 3) {
      res_out$log_msg <- paste0("Warning in ", l, ": Insufficient unique points after duplicate coordinate removal (needed >= 3, got ", nrow(pts), ").")
      return(res_out)
    }
    
    # Calculate local spatial density and dynamic buffer distance with robust safety fallbacks
    b_mode_safe <- if (!is.null(buff_mode) && length(buff_mode) > 0) buff_mode else "dynamic"
    b_dist_safe <- if (!is.null(b_dist) && length(b_dist) > 0) b_dist else 250
    grid_res_safe <- if (!is.null(grid_res) && length(grid_res) > 0) grid_res else 50
    current_method_safe <- if (!is.null(current_method) && length(current_method) > 0) current_method else "OK"

    coords_local <- sf::st_coordinates(pts)
    if (!is.null(res_mode) && res_mode == "fixed") {
      local_res <- grid_res_safe
    } else if (nrow(coords_local) > 1) {
      knn_res <- FNN::get.knn(coords_local, k = 1)
      local_res <- mean(knn_res$nn.dist) * 0.5
    } else {
      local_res <- grid_res_safe
    }
    
    b_dist_local <- if (b_mode_safe == "dynamic" && b_type == "wrapped") {
      val <- get_buffer_multiplier(current_method_safe) * local_res
      max(5, min(2000, val))
    } else {
      b_dist_safe
    }
    
    local_shp <- NULL
    if (!is.null(shp_bound)) {
      match_col <- NULL
      for(col_name in colnames(shp_bound)) {
        if (any(as.character(shp_bound[[col_name]]) == l)) {
          match_col <- col_name
          break
        }
      }
      
      if (!is.null(match_col)) {
        local_shp <- shp_bound %>% dplyr::filter(!!sym(match_col) == l)
        local_shp <- sf::st_transform(local_shp, sf::st_crs(pts)) %>% sf::st_union()
      } else {
        local_shp <- tryCatch({
          shp_trans <- tryCatch(sf::st_transform(shp_bound, sf::st_crs(pts)), error = function(e) NULL)
          if (!is.null(shp_trans)) {
            intersects <- sf::st_intersects(shp_trans, sf::st_union(pts), sparse = FALSE)
            if (any(intersects)) {
              shp_trans[which(intersects)[1], ] %>% sf::st_union()
            } else {
              NULL
            }
          } else NULL
        }, error = function(e) NULL)
      }
    }
    
    bound <- NULL
    if (!is.null(local_shp)) {
      bound <- local_shp
    } else {
      bound <- tryCatch({
        b <- switch(b_type,
               "convex"  = sf::st_convex_hull(sf::st_union(pts)),
               "concave" = concaveman::concaveman(pts),
               "wrapped" = sf::st_buffer(concaveman::concaveman(pts), dist = b_dist_local),
               "strict"  = sf::st_union(sf::st_buffer(pts, dist = b_dist_local)))
        sf::st_as_sf(sf::st_sfc(sf::st_geometry(b), crs = sf::st_crs(pts)))
      }, error = function(e) {
        sf::st_as_sf(sf::st_sfc(sf::st_convex_hull(sf::st_union(pts)), crs = sf::st_crs(pts)))
      })
    }
    
    bbox <- sf::st_bbox(bound)
    area_m2 <- as.numeric(sf::st_area(bound))
    cell_area_target <- area_m2 / 100000 
    
    if (!is.null(res_mode) && res_mode == "fixed") {
      actual_res <- grid_res_safe
    } else {
      actual_res <- sqrt(cell_area_target)
      actual_res <- max(5, min(1000, actual_res)) 
    }
    
    min_res_safe <- max(0.1, grid_res_safe * 0.1)
    if (actual_res < min_res_safe) actual_res <- min_res_safe

    grid_r <- terra::rast(terra::ext(bbox), res=actual_res, crs=sf::st_crs(pts)$wkt)
    grid_p <- terra::as.points(grid_r, values=FALSE) %>% sf::st_as_sf() %>%
      dplyr::mutate(x = sf::st_coordinates(.)[,1], y = sf::st_coordinates(.)[,2])
    
    r_a <- NULL; r_p <- NULL
    
    # Pre-krige covariates once to avoid redundant expensive computations in comparison mode
    grid_aux <- grid_p
    cov_log_msg <- ""
    if (current_method %in% c("RK", "RFK", "CK") && length(aux_vars) > 0) {
        lags_cov <- calc_scientific_lags(pts)
        mp_cov <- list(idw_p = m_params$idw_p_act, idw_nmax = m_params$idw_nmax)
        krig_cov <- krige_covariates(pts, grid_p, aux_vars, lags_cov, mp_cov)
        grid_aux <- krig_cov$grid_aux
        cov_log_msg <- krig_cov$log_msg
    }
    
    pts_a <- pts %>% dplyr::filter(!is.na(v)) %>% dplyr::mutate(x = sf::st_coordinates(.)[,1], y = sf::st_coordinates(.)[,2])
    if(nrow(pts_a) >= 3) {
        lags_a <- calc_scientific_lags(pts_a)
        mp_a <- list(idw_p = m_params$idw_p_act, idw_nmax = m_params$idw_nmax, tps_lambda = m_params$tps_lambda_act, pre_fit = m_params$pre_fit_act, grid_aux = grid_aux)
        if (!is.null(cancel_file_val) && file.exists(cancel_file_val)) stop("Model generation cancelled by user.")
        res_a_list <- apply_interpolation(pts_a, "v", current_method, grid_p, aux_vars, lags_a, mp_a, l, "act")
        res_out$v_emp_act <- res_a_list$v_emp; res_out$v_fit_act <- res_a_list$fit; res_out$cv_act <- res_a_list$cv_metrics; res_out$cv_obj_act <- res_a_list$cv_obj
        res_out$summ_act <- res_a_list$model_summary; res_out$rf_act <- res_a_list$rf_model; res_out$gstat_act <- res_a_list$gstat_obj
        res_out$log_msg <- paste0(res_out$log_msg, "\n", res_a_list$log_msg)
        if (cov_log_msg != "") res_out$log_msg <- paste0(res_out$log_msg, "\n", cov_log_msg)
        
        if(!is.null(res_a_list$res_sf)) {
            fields_a <- if("var1.var" %in% colnames(res_a_list$res_sf)) c("var1.pred", "var1.var") else "var1.pred"
            r_a <- terra::rasterize(res_a_list$res_sf, grid_r, field=fields_a) %>% terra::mask(terra::vect(bound)) %>% terra::project(crs_sel)
            res_out$r_a <- terra::wrap(r_a)
        }
    }
    
    if(comp_mode || val_type != "actual") {
        pts_p <- pts %>% dplyr::filter(!is.na(pv)) %>% dplyr::mutate(x = sf::st_coordinates(.)[,1], y = sf::st_coordinates(.)[,2])
        if(nrow(pts_p) >= 3) {
            lags_p <- calc_scientific_lags(pts_p)
            mp_p <- list(idw_p = m_params$idw_p_pre, idw_nmax = m_params$idw_nmax, tps_lambda = m_params$tps_lambda_pre, pre_fit = m_params$pre_fit_pre, grid_aux = grid_aux)
            if (!is.null(cancel_file_val) && file.exists(cancel_file_val)) stop("Model generation cancelled by user.")
            res_p_list <- apply_interpolation(pts_p, "pv", current_method, grid_p, aux_vars, lags_p, mp_p, l, "pre")
            res_out$v_emp_pre <- res_p_list$v_emp; res_out$v_fit_pre <- res_p_list$fit; res_out$cv_pre <- res_p_list$cv_metrics; res_out$cv_obj_pre <- res_p_list$cv_obj
            res_out$summ_pre <- res_p_list$model_summary; res_out$rf_pre <- res_p_list$rf_model; res_out$gstat_pre <- res_p_list$gstat_obj
            res_out$log_msg <- paste0(res_out$log_msg, "\n", res_p_list$log_msg)
            
            if(!is.null(res_p_list$res_sf)) {
                fields_p <- if("var1.var" %in% colnames(res_p_list$res_sf)) c("var1.pred", "var1.var") else "var1.pred"
                r_p <- terra::rasterize(res_p_list$res_sf, grid_r, field=fields_p) %>% terra::mask(terra::vect(bound)) %>% terra::project(crs_sel)
                res_out$r_p <- terra::wrap(r_p)
            }
        }
    }
    
    if(!is.null(r_a) && !is.null(r_p)) res_out$r_res <- terra::wrap(r_a - r_p)
    
    # --- Extended Residuals: Kriged Point Errors ---
    pts_err_raw <- pts_data %>% dplyr::filter(!is.na(v), !is.na(pv))
    if(nrow(pts_err_raw) >= 3) {
        pts_err <- sf::st_as_sf(pts_err_raw, coords=c("x","y"), crs=utm_crs) %>%
                   dplyr::mutate(err = v - pv)
        err_mod <- gstat::idw(err ~ 1, pts_err, grid_p, nmax = m_params$idw_nmax, idp = 2, debug.level = 0)
        r_err <- terra::rasterize(err_mod, grid_r, field="var1.pred") %>% terra::mask(terra::vect(bound)) %>% terra::project(crs_sel)
        res_out$r_point_err <- terra::wrap(r_err)
    }
    
    pts$model_resid_act <- NA_real_
    if (nrow(pts_a) >= 3 && exists("res_a_list") && !is.null(res_a_list$residuals)) {
      pts$model_resid_act[!is.na(pts$v)] <- res_a_list$residuals
    }
    
    pts$model_resid_pre <- NA_real_
    if ((comp_mode || val_type != "actual") && nrow(pts_p) >= 3 && exists("res_p_list") && !is.null(res_p_list$residuals)) {
      pts$model_resid_pre[!is.na(pts$pv)] <- res_p_list$residuals
    }

    res_out$bound <- sf::st_transform(bound, crs_sel)
    res_out$pts <- sf::st_transform(pts, crs_sel) %>% dplyr::mutate(loc = l, resid = v - pv)
    res_out$actual_res <- actual_res
    
    res_out
  }, error = function(e) {
    res_out$log_msg <- paste0(res_out$log_msg, "\nError in ", l, ": ", e$message)
    res_out
  })
  
  return(res_out)
}

# --- Scientific Variogram Parameters ---
calc_scientific_lags <- function(sf_pts) {
  # Reputable heuristic: cutoff = max distance / 2, width = cutoff / 15
  bbox <- sf::st_bbox(sf_pts)
  max_dist <- as.numeric(sqrt((bbox$xmax - bbox$xmin)^2 + (bbox$ymax - bbox$ymin)^2))
  cutoff <- max_dist / 2
  list(width = cutoff / 15, cutoff = cutoff)
}
