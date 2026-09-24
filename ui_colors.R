# ui_colors.R - palette definitions and colour-resolution helpers (pure
# functions + static constants; no reactivity). Sourced via ui_helpers.R.


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

# Palette for standard-error and variance maps. Uncertainty is a non-negative
# magnitude with no meaningful midpoint, and a diverging palette would draw
# one, so a diverging choice (RColorBrewer category "div") becomes viridis.
# Sequential choices pass through. Used by the Map Viewer and the Export Styler.
uncertainty_palette <- function(pal) {
  pal <- as.character(pal %||% "viridis")[1]
  info <- RColorBrewer::brewer.pal.info
  if (pal %in% rownames(info) && identical(as.character(info[pal, "category"]), "div")) "viridis" else pal
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

# Reference limits prefilled for Agronomical > Supervised styling with three
# classes: Low below `limits[1]`, Moderate from `limits[1]` up to `limits[2]`,
# High from `limits[2]` up, each class holding its lower limit (the map and
# the agreement table classify with right = FALSE). `pattern` recognises the
# nutrient in a column name, in list order. The DTPA micronutrient classes are
# local limits (Çokuysal & Erbaş 2004) on the DTPA soil test of Lindsay &
# Norvell (1978), who publish single critical levels rather than classes.
# Scientific Guide section 9.4.1 lists the full references.
DTPA_LOCAL_SOURCE <- "local limits of Çokuysal & Erbaş (2004) on the DTPA test of Lindsay & Norvell (1978)"
NUTRIENT_REFERENCE <- list(
  TN = list(pattern = "\\bTN\\b|NITROGEN", method = "Total N", unit = "%",
            limits = c(0.05, 0.10), source = "Çokuysal & Erbaş (2004)"),
  P  = list(pattern = "\\bP\\b|PHOSPHORUS|OLSEN", method = "Olsen P", unit = "mg kg⁻¹",
            limits = c(8, 25), source = "Yüksel & Ekinci (2019)"),
  K  = list(pattern = "\\bK\\b|POTASSIUM", method = "NH₄OAc K", unit = "mg kg⁻¹",
            limits = c(200, 300), source = "Çokuysal & Erbaş (2004)"),
  Ca = list(pattern = "\\bCA\\b|CALCIUM", method = "NH₄OAc Ca", unit = "mg kg⁻¹",
            limits = c(1428, 2857), source = "Çokuysal & Erbaş (2004)"),
  Mg = list(pattern = "\\bMG\\b|MAGNESIUM", method = "NH₄OAc Mg", unit = "mg kg⁻¹",
            limits = c(80, 160), source = "Çokuysal & Erbaş (2004)"),
  Fe = list(pattern = "\\bFE\\b|IRON", method = "DTPA Fe", unit = "mg kg⁻¹",
            limits = c(4, 6), source = DTPA_LOCAL_SOURCE),
  Mn = list(pattern = "\\bMN\\b|MANGANESE", method = "DTPA Mn", unit = "mg kg⁻¹",
            limits = c(1.2, 3.5), source = DTPA_LOCAL_SOURCE),
  Cu = list(pattern = "\\bCU\\b|COPPER", method = "DTPA Cu", unit = "mg kg⁻¹",
            limits = c(0.3, 0.8), source = DTPA_LOCAL_SOURCE),
  Zn = list(pattern = "\\bZN\\b|ZINC", method = "DTPA Zn", unit = "mg kg⁻¹",
            limits = c(1, 3), source = DTPA_LOCAL_SOURCE)
)

get_nut_key <- function(v) {
  v_up <- toupper(as.character(v))
  if (length(v_up) == 0 || is.na(v_up) || v_up == "") return(NULL)
  matches <- vapply(NUTRIENT_REFERENCE, function(r) grepl(r$pattern, v_up), logical(1))
  if (any(matches)) return(names(NUTRIENT_REFERENCE)[which(matches)[1]])
  NULL
}

# Units the reference limits accept as their own, compared after .unit_key():
# lower case, no spaces, dots, carets or middle dots, superscripts and the
# micro sign written out.
UNIT_EQUIVALENTS <- list(
  mg_per_kg = c("mg/kg", "mgkg-1", "ppm", "ug/g", "ugg-1"),
  percent = c("%", "percent", "pct")
)
.unit_key <- function(u) {
  u <- as.character(u %||% "")[1]
  if (is.na(u)) u <- ""
  u <- tolower(trimws(u))
  u <- gsub("⁻", "-", u)
  u <- gsub("¹", "1", u)
  u <- gsub("[µμ]", "u", u)
  gsub("[[:space:].·^]", "", u)
}

#' How a variable's recorded unit relates to a reference unit: "empty" (none
#' recorded), "equivalent" (the same unit under another spelling, e.g. ppm for
#' mg kg⁻¹) or "different".
reference_unit_status <- function(var_unit, ref_unit) {
  if (!nzchar(.unit_key(var_unit))) return("empty")
  family <- function(u) {
    hit <- names(UNIT_EQUIVALENTS)[vapply(UNIT_EQUIVALENTS, function(e) .unit_key(u) %in% e, logical(1))]
    if (length(hit)) hit[1] else paste0("other:", .unit_key(u))
  }
  if (identical(family(var_unit), family(ref_unit))) "equivalent" else "different"
}

#' The limits the Supervised boxes open with for one variable and class count.
#' The reference limits (NUTRIENT_REFERENCE) apply at three classes when the
#' variable's unit is theirs or not recorded; otherwise the k - 1
#' equal-probability quantiles of `values` (type 7) are offered, which describe
#' the data and carry no agronomic meaning. Returns `limits`, `source`
#' ("reference" or "quantile"), the registry entry `ref` (NULL without one) and
#' `unit_status`.
class_limit_defaults <- function(var_id, unit, n_classes, values) {
  key <- get_nut_key(var_id)
  ref <- if (!is.null(key)) NUTRIENT_REFERENCE[[key]]
  k <- as.integer(n_classes)
  status <- if (!is.null(ref)) reference_unit_status(unit, ref$unit) else NA_character_
  if (!is.null(ref) && isTRUE(k == 3L) && status %in% c("empty", "equivalent")) {
    return(list(limits = ref$limits, source = "reference", ref = ref, unit_status = status))
  }
  v <- values[is.finite(values)]
  q <- if (length(v) && isTRUE(k >= 2L)) {
    stats::quantile(v, probs = seq_len(k - 1L) / k, type = 7, names = FALSE)
  } else rep(NA_real_, max(k - 1L, 0L))
  list(limits = q, source = "quantile", ref = ref, unit_status = status)
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
