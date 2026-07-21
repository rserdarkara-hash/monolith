# ui_colors.R - palette definitions and colour-resolution helpers (pure
# functions + static constants; no reactivity). Sourced via ui_helpers.R.


agro_colors <- c("#E69F00", "#F0E442", "#009E73") # Orange, Yellow, Green

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

nutrient_palettes <- list(
  TN = "Greens", P = "Blues", K = "Oranges", Ca = "YlOrRd",
  Mg = "PuBuGn", Fe = "Purples", Mn = "GnBu", Cu = "YlGn", Zn = "YlOrBr"
)

# Diverging palette for residual/error maps in the Export Styler. High
# contrast only overrides the user's choice when that choice is not itself
# colorblind-safe (per RColorBrewer's colorblind-friendly list).
cb_safe_diverging <- c("RdBu", "RdYlBu", "PuOr", "BrBG", "PiYG", "PRGn")

resolve_resid_palette <- function(input) {
  pal <- input$styler_resid_palette %||% "RdBu"
  if (isTruthy(input$styler_high_contrast) && !pal %in% cb_safe_diverging) pal <- "PuOr"
  pal
}

# Descriptive-suite palette catalogue. Every option must yield usable colours
# for ANY group count: Brewer palettes are ramped past their native maximum
# instead of degrading to NA colours.
desc_palette_choices <- list(
  "Default" = c("Default (ggplot2)" = "default"),
  "Colorblind Safe" = c("Okabe-Ito" = "okabe", "Viridis" = "viridis",
                        "Cividis" = "cividis", "Plasma" = "plasma", "Turbo" = "turbo"),
  "Qualitative (Brewer)" = c("Set1" = "Set1", "Set2" = "Set2", "Set3" = "Set3",
                             "Dark2" = "Dark2", "Paired" = "Paired",
                             "Accent" = "Accent", "Pastel1" = "Pastel1")
)

desc_palette_colors <- function(pal, n) {
  if (pal %in% c("viridis", "cividis", "plasma", "magma", "inferno", "turbo")) {
    return(viridis::viridis(n, option = pal))
  }
  base <- if (identical(pal, "okabe")) {
    unname(grDevices::palette.colors(9, palette = "Okabe-Ito"))
  } else {
    RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info[pal, "maxcolors"], pal)
  }
  if (n <= length(base)) base[seq_len(n)] else grDevices::colorRampPalette(base)(n)
}

# `continuous` only for truly continuous fills (XYZ surface); discrete fills
# (including geom_density_2d_filled's ordered `level`) take a discrete scale,
# otherwise ggplot errors with "Discrete value supplied to continuous scale".
apply_desc_palette <- function(p, pal, continuous = FALSE) {
  if (is.null(pal) || identical(pal, "default")) return(p)
  if (continuous) {
    cols <- desc_palette_colors(pal, 256)
    return(p + scale_fill_gradientn(colours = cols, na.value = "grey85") +
             scale_color_gradientn(colours = cols, na.value = "grey85"))
  }
  pal_fn <- function(n) desc_palette_colors(pal, n)
  p + discrete_scale("fill", palette = pal_fn) +
    discrete_scale("colour", palette = pal_fn)
}

nutrient_limits <- list(
  TN = c(0.05, 0.10), P = c(8, 25), K = c(150, 300), Ca = c(1428, 2857),
  Mg = c(80, 160), Fe = c(4, 6), Mn = c(1.2, 3.5), Cu = c(0.3, 0.8), Zn = c(1, 3)
)

get_nut_key <- function(v) {
  v_up <- toupper(as.character(v))
  if (length(v_up) == 0 || is.na(v_up) || v_up == "") return(NULL)
  
  patterns <- c(
    TN = "\\bTN\\b|NITROGEN",
    P  = "\\bP\\b|PHOSPHORUS|OLSEN",
    K  = "\\bK\\b|POTASSIUM",
    Ca = "\\bCA\\b|CALCIUM",
    Mg = "\\bMG\\b|MAGNESIUM",
    Fe = "\\bFE\\b|IRON",
    Mn = "\\bMN\\b|MANGANESE",
    Cu = "\\bCU\\b|COPPER",
    Zn = "\\bZN\\b|ZINC"
  )
  
  matches <- sapply(patterns, function(pat) grepl(pat, v_up))
  if (any(matches)) return(names(patterns)[which(matches)[1]])
  return(NULL)
}

get_default_palette <- function(var_name, category = "Soil", label = NULL) {
  nut <- get_nut_key(var_name)
  if (is.null(nut) && !is.null(label)) nut <- get_nut_key(label)
  
  if (!is.null(nut)) return(nutrient_palettes[[nut]])
  
  if (is.null(category)) {
    return("YlOrRd")
  } else if (category == "Environmental Data") {
    "RdYlBu"
  } else if (category == "Landsat Data") {
    "viridis"
  } else if (category == "Sentinel Data") {
    "viridis"
  } else if (category == "Merged Data") {
    "viridis"
  } else if (category == "Terrain Data") {
    "BrBG"
  } else {
    "YlOrRd"
  }
}

TABLEAU10 <- c("#4e79a7","#f28e2b","#e15759","#76b7b2","#59a14f",
               "#edc948","#b07aa1","#ff9da7","#9c755f","#bab0ac")

generate_group_palette <- function(groups, palette_name = "Set1") {
  n <- length(groups)
  if (n == 0) return(character(0))

  if (palette_name == "Tableau10") {
    colors <- rep_len(TABLEAU10, n)
  } else {
    max_n <- RColorBrewer::brewer.pal.info[palette_name, "maxcolors"]
    colors <- RColorBrewer::brewer.pal(min(max(n, 3), max_n), palette_name)
    if (n > max_n) colors <- grDevices::colorRampPalette(colors)(n)
    colors <- colors[seq_len(n)]
  }
  stats::setNames(colors, groups)
}
