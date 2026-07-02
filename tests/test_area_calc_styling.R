# tests/test_area_calc_styling.R
library(terra)
library(sf)
library(dplyr)
library(classInt)

# Universal Agronomical Colors mock
get_agro_colors <- function(n) {
  if (n == 2) {
    c("#E69F00", "#009E73")
  } else if (n == 3) {
    c("#E69F00", "#F0E442", "#009E73")
  } else if (n == 4) {
    c("#E69F00", "#F0E442", "#56B4E9", "#009E73")
  } else if (n == 5) {
    c("#D55E00", "#E69F00", "#F0E442", "#56B4E9", "#009E73")
  } else {
    colorRampPalette(c("#E69F00", "#F0E442", "#009E73"))(n)
  }
}

# Corrected classification_params implementation mock
classification_params_test <- function(color_style, agro_n_classes, agro_method, vv, meta_palette) {
  n_c <- if(color_style == "bin") 5 else agro_n_classes
  
  if (color_style == "agro") {
    if(agro_method == "limits") {
      # Stub for supervised limits
      brks_inner <- c(20, 40, 60, 80)[1:(n_c-1)]
    } else {
      if (length(vv) < n_c) return(NULL)
      brks_inner <- tryCatch({
        classIntervals(vv, n=n_c, style=agro_method)$brks[2:n_c]
      }, error = function(e) {
        seq(min(vv, na.rm=TRUE), max(vv, na.rm=TRUE), length.out = n_c + 1)[2:n_c]
      })
    }
    
    brks <- sort(unique(c(-Inf, brks_inner, Inf)))
    n_c_actual <- length(brks) - 1
    
    rcl_mat <- matrix(NA, nrow = n_c_actual, ncol = 3)
    for(i in 1:n_c_actual) {
      rcl_mat[i, ] <- c(brks[i], brks[i+1], i)
    }
    
    colors <- get_agro_colors(n_c_actual)
    labels <- if(n_c_actual==3) c("Low", "Med", "High") else paste("Class", 1:n_c_actual)
    
    leg_labels <- character(n_c_actual)
    for(i in 1:n_c_actual) {
      if(i==1) leg_labels[i] <- paste("<", round(brks[2], 3))
      else if(i==n_c_actual) leg_labels[i] <- paste(">", round(brks[n_c_actual], 3))
      else leg_labels[i] <- paste(round(brks[i],3), "-", round(brks[i+1],3))
    }
    if(n_c_actual == 3) leg_labels <- paste(labels, ":", leg_labels)
    
    list(brks = brks, rcl_mat = rcl_mat, colors = colors, labels = labels, leg_labels = leg_labels, n_c = n_c_actual)
    
  } else {
    # Binned (5) styling
    if(length(vv) < n_c) return(NULL)
    rng <- range(vv, na.rm = TRUE)
    if(is.infinite(rng[1]) || is.infinite(rng[2]) || rng[1] == rng[2]) {
      brks_inner <- seq(rng[1], rng[1] + 1, length.out = n_c + 1)[2:n_c]
    } else {
      brks_inner <- seq(rng[1], rng[2], length.out = n_c + 1)[2:n_c]
    }
    
    brks <- sort(unique(c(-Inf, brks_inner, Inf)))
    n_c_actual <- length(brks) - 1
    
    rcl_mat <- matrix(NA, nrow = n_c_actual, ncol = 3)
    for(i in 1:n_c_actual) {
      rcl_mat[i, ] <- c(brks[i], brks[i+1], i)
    }
    
    is_viridis <- meta_palette == "viridis" || meta_palette == "inferno"
    colors <- if(is_viridis) {
      viridis::viridis(n_c_actual, option = meta_palette)
    } else {
      colorRampPalette(RColorBrewer::brewer.pal(min(8, max(3, n_c_actual)), meta_palette))(n_c_actual)
    }
    
    labels <- paste("Bin", 1:n_c_actual)
    
    leg_labels <- character(n_c_actual)
    for(i in 1:n_c_actual) {
      if(i==1) leg_labels[i] <- paste("<", round(brks[2], 3))
      else if(i==n_c_actual) leg_labels[i] <- paste(">", round(brks[n_c_actual], 3))
      else leg_labels[i] <- paste(round(brks[i],3), "-", round(brks[i+1],3))
    }
    
    list(brks = brks, rcl_mat = rcl_mat, colors = colors, labels = labels, leg_labels = leg_labels, n_c = n_c_actual)
  }
}

# Corrected calc_area_df implementation mock
calc_area_df_test <- function(r_obj, color_style, agro_n_classes, agro_method, vv, meta_palette) {
  if (is.null(r_obj)) return(NULL)
  
  params <- classification_params_test(color_style, agro_n_classes, agro_method, vv, meta_palette)
  if (is.null(params)) {
    return("Awaiting Classification Params")
  }
  
  r_class <- classify(r_obj[[1]], params$rcl_mat, right = FALSE)
  area_df <- as.data.frame(expanse(r_class, unit = "ha", byValue = TRUE))
  
  class_names <- if(color_style == "bin") params$leg_labels else params$labels
  full_res <- data.frame(value = as.numeric(1:params$n_c), Class = class_names)
  
  if(!"value" %in% names(area_df)) {
    res_df <- data.frame(Class = class_names, Ha = 0)
    return(res_df)
  }
  
  is_label <- any(as.character(area_df$value) %in% class_names)
  if (is_label) {
     area_df$value <- match(as.character(area_df$value), class_names)
  } else {
     area_df$value <- as.numeric(as.character(area_df$value))
  }
  
  area_df <- area_df[!is.na(area_df$value), ]
  area_df <- area_df %>%
    group_by(value) %>%
    summarise(Ha = round(sum(area, na.rm = TRUE), 2), .groups = "drop")

  res_df <- full_res %>%
    left_join(area_df, by = "value") %>%
    mutate(Ha = ifelse(is.na(Ha), 0, Ha)) %>%
    select(Class, Ha)
  
  return(res_df)
}

# Test 1: Binned params generation (Expect: Pass)
cat("--- Running Test 1: Binned Params Generation ---\n")
vv_dummy <- 1:100
params_bin <- classification_params_test("bin", 5, "equal", vv_dummy, "YlOrRd")
if (is.null(params_bin)) {
  cat("Test 1: FAILED\n\n")
} else {
  cat("Test 1: PASSED\n")
  cat("Breaks: ", paste(params_bin$brks, collapse = ", "), "\n")
  cat("Labels: ", paste(params_bin$labels, collapse = ", "), "\n\n")
}

# Test 2: Duplicate breaks handling under Jenks/K-Means (Expect: Pass)
cat("--- Running Test 2: Jenks Duplicate Breaks ---\n")
vv_dup <- c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2)
params_dup <- classification_params_test("agro", 5, "jenks", vv_dup, "YlOrRd")
if (is.null(params_dup) || any(is.na(params_dup$rcl_mat[, 1:2]))) {
  cat("Test 2: FAILED\n\n")
} else {
  cat("Test 2: PASSED\n")
  cat("Actual Classes generated: ", params_dup$n_c, "\n")
  cat("Legend Labels: ", paste(params_dup$leg_labels, collapse = ", "), "\n\n")
}

# Test 3: Area calculation for binned styling (Expect: Pass)
cat("--- Running Test 3: Area Calculation for Binned ---\n")
r <- rast(nrows=10, ncols=10, xmin=0, xmax=10, ymin=0, ymax=10)
values(r) <- 1:100
crs(r) <- "EPSG:32635"

area_res <- calc_area_df_test(r, "bin", 5, "equal", values(r), "YlOrRd")
if (is.character(area_res) || !"Ha" %in% names(area_res)) {
  cat("Test 3: FAILED\n\n")
} else {
  cat("Test 3: PASSED\n")
  print(area_res)
  cat("\n")
}
