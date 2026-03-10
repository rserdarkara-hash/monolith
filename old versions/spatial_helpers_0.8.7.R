library(sf)
library(dplyr)
library(gstat)

# Calculate Concordance Correlation Coefficient (CCC)
calc_ccc <- function(observed, predicted) {
  if (length(observed) < 2) return(NA)
  
  mean_obs <- mean(observed, na.rm = TRUE)
  mean_pred <- mean(predicted, na.rm = TRUE)
  
  var_obs <- var(observed, na.rm = TRUE)
  var_pred <- var(predicted, na.rm = TRUE)
  sd_obs <- sqrt(var_obs)
  sd_pred <- sqrt(var_pred)
  
  cov_op <- cov(observed, predicted, use = "pairwise.complete.obs")
  rho <- cov_op / (sd_obs * sd_pred)
  
  numerator <- 2 * rho * sd_obs * sd_pred
  denominator <- var_obs + var_pred + (mean_obs - mean_pred)^2
  
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
  if (length(residuals) < 3 || nrow(coords) != length(residuals)) return(NA)
  
  # Performance safeguard: Use sparse matrix approach for better RAM efficiency
  # We'll use a distance-based weight matrix with a reasonable threshold
  n <- length(residuals)
  
  tryCatch({
    # Identify reasonable distance threshold (e.g., 2x average NN distance)
    # Using FNN for efficiency if available, otherwise falling back to simple dist
    knn_res <- tryCatch({ FNN::get.knn(coords, k = 1) }, error = function(e) list(nn.dist = mean(dist(coords), na.rm = TRUE)))
    d_thresh <- mean(knn_res$nn.dist, na.rm = TRUE) * 5 # Wide enough to capture local neighbors
    
    # Create sparse weights using spdep
    nb <- spdep::dnearneigh(as.matrix(coords), 0, d_thresh)
    
    # Check if any points have no neighbors, if so, increase threshold slightly or handle
    if(any(spdep::card(nb) == 0)) {
       nb <- spdep::knn2nb(spdep::knearneigh(as.matrix(coords), k = 3))
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

# Main CV Logic Abstraction
perform_cv <- function(cv_obj) {
  # Initialize results
  res <- list(rmse = NA, r2 = NA, nse = NA, me = NA, mae = NA, ccc = NA, 
              nrmse_mean = NA, rpd = NA, rpiq = NA, smape = NA, moran_i = NA, n = 0)
  
  if (is.null(cv_obj)) return(res)
  
  # Convert to standard dataframe (handles SP and SF)
  df <- if (inherits(cv_obj, "Spatial")) as.data.frame(cv_obj) 
        else if (inherits(cv_obj, "sf")) {
            coords <- st_coordinates(cv_obj)
            df <- st_drop_geometry(cv_obj)
            df$x <- coords[,1]
            df$y <- coords[,2]
            df
        } else as.data.frame(cv_obj)
        
  if (nrow(df) == 0) return(res)
  cnames <- colnames(df)
  
  # Robust column detection
  pre_col <- grep("\\.pred$|^var1\\.pred$", cnames, value = TRUE)[1]
  obs_col <- grep("\\.observed$|^observed$", cnames, value = TRUE)[1]
  
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

# --- Regression Kriging Full-Pipeline CV ---
perform_rk_cv <- function(pts, target_var, aux_vars, lags_func, vgm_fit_func) {
  n <- nrow(pts)
  form_reg <- as.formula(paste0("`", target_var, "` ~ ", paste(paste0("`", aux_vars, "`"), collapse = " + ")))
  
  results_list <- furrr::future_map(1:n, function(i) {
    train <- pts[-i, ]; test <- pts[i, ]
    
    lm_mod <- lm(form_reg, data = train); train$residuals <- residuals(lm_mod)
    lags <- lags_func(train)
    v_emp <- variogram(residuals ~ 1, train, width = lags$width, cutoff = lags$cutoff)
    v_fit <- vgm_fit_func(v_emp, train$residuals)
    res_krig <- krige(residuals ~ 1, train, test, model = v_fit, debug.level = 0)
    pred_trend <- predict(lm_mod, newdata = test)
    data.frame(observed = test[[target_var]], var1.pred = as.numeric(pred_trend) + res_krig$var1.pred)
  }, .options = furrr::furrr_options(seed = TRUE))
  
  return(do.call(rbind, results_list))
}

# --- RF Kriging Full-Pipeline CV ---
perform_rfk_cv <- function(pts, target_var, aux_vars, lags_func, vgm_fit_func) {
  n <- nrow(pts)
  form_reg <- as.formula(paste0("`", target_var, "` ~ ", paste(paste0("`", aux_vars, "`"), collapse = " + ")))
  
  results_list <- furrr::future_map(1:n, function(i) {
    train <- pts[-i, ]; test <- pts[i, ]
    
    rf_mod <- randomForest::randomForest(form_reg, data = train, ntree = 100); train$residuals <- train[[target_var]] - rf_mod$predicted
    lags <- lags_func(train)
    v_emp <- variogram(residuals ~ 1, train, width = lags$width, cutoff = lags$cutoff)
    v_fit <- vgm_fit_func(v_emp, train$residuals)
    res_krig <- krige(residuals ~ 1, train, test, model = v_fit, debug.level = 0)
    pred_trend <- predict(rf_mod, test)
    data.frame(observed = test[[target_var]], var1.pred = as.numeric(pred_trend) + res_krig$var1.pred)
  }, .options = furrr::furrr_options(seed = TRUE))
  
  return(do.call(rbind, results_list))
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

  rmses <- furrr::future_map_dbl(factors, function(f) {
    cv <- tryCatch({ krige.cv(form, pts, nmax = nmax, set = list(idp = f), nfold = folds, debug.level = 0) }, error = function(e) NULL)
    if(!is.null(cv)) {
      return(sqrt(mean(cv$residual^2, na.rm = TRUE)))
    } else {
      return(Inf)
    }
  }, .options = furrr::furrr_options(seed = TRUE, packages = c("gstat")))
  
  best_idx <- which.min(rmses)
  if(length(best_idx) > 0 && rmses[best_idx] != Inf) {
    return(factors[best_idx])
  }
  return(2.0)
}

# --- Collinearity Check (VIF) ---
check_vif <- function(df, threshold = 10) {
  df_num <- df[sapply(df, is.numeric)]; kept <- colnames(df_num); dropped <- c()
  if (length(kept) < 2) return(list(kept = kept, dropped = dropped))
  repeat {
    if (length(kept) < 2) break
    cor_mat <- cor(df_num[, kept], use = "pairwise.complete.obs")
    vif_vals <- tryCatch({ diag(solve(cor_mat)) }, error = function(e) { NULL })
    if (is.null(vif_vals)) {
      cor_mat_no_diag <- cor_mat; diag(cor_mat_no_diag) <- 0
      max_idx <- which(abs(cor_mat_no_diag) == max(abs(cor_mat_no_diag)), arr.ind = TRUE)[1,]
      var_to_drop <- kept[max_idx[1]]; dropped <- c(dropped, var_to_drop); kept <- setdiff(kept, var_to_drop); next
    }
    max_vif <- max(vif_vals)
    if (max_vif > threshold) {
      var_to_drop <- names(vif_vals)[which.max(vif_vals)]; dropped <- c(dropped, var_to_drop); kept <- setdiff(kept, var_to_drop)
    } else break
  }
  return(list(kept = kept, dropped = dropped))
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

library(DALEX)
library(randomForest)

# --- Analytical Backend for Governing Factors ---
compute_governing_factors <- function(df, target_col, predictors, n_permutations = 10) {
  # 1. Prepare data
  req_cols <- c(target_col, predictors)
  df_clean <- df[, req_cols, drop = FALSE]
  df_clean <- df_clean[complete.cases(df_clean), , drop = FALSE]
  
  if (nrow(df_clean) < 10) return(NULL) # Not enough data
  
  # 2. Fit Random Forest
  formula_str <- paste(target_col, "~ .")
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
  
  # 6. SHAP profile (using sample)
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
      sub <- shap_df[shap_df$obs_id == i & grepl(paste0("^", top_var), shap_df$variable_name), ]
      if(nrow(sub) > 0) sum(sub$contribution) else 0
    })
  )
  
  list(
    model = rf_model,
    explainer = explainer_rf,
    importance = vip_agg,
    top_var = top_var,
    ale = ale_df,
    shap = shap_val_df
  )
}

