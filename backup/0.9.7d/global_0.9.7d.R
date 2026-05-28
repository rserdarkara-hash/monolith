# global_0.9.7d.R - Centralized Configuration & Dependencies for Monolith v0.9.7d
# Copyright (c) 2026 Recep Serdar Kara. All rights reserved.

# --- Required Package Suite ---
required_packages <- c(
  "shiny", "shinyjs", "shinyWidgets", "shinyFiles", "shinycssloaders", "DT",
  "sf", "terra", "tidyterra", "leaflet", "leaflet.extras", "ggspatial", "fields",
  "classInt", "gstat", "automap", "concaveman", "spdep", "FNN",
  "dplyr", "tidyr", "jsonlite", "readxl", "openxlsx", "officer", "zip",
  "ggplot2", "ggpubr", "plotly", "RColorBrewer", "viridis", "latticeExtra",
  "patchwork", "fresh", "showtext", "scales", "commonmark", "glue",
  "randomForest", "DALEX", "yardstick", "agricolae", "mgcv",
  "future", "furrr", "promises"
)

# --- Core R/Shiny Dependencies ---
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinyFiles)
library(shinycssloaders)
library(DT)

# --- Spatial & Mapping Dependencies ---
library(sf)
library(terra)
library(tidyterra)
library(leaflet)
library(leaflet.extras)
library(ggspatial)
library(fields)
library(classInt)
library(gstat)
library(automap)
library(concaveman)
library(spdep)
library(FNN)

# --- Data Manipulation & Formatting Dependencies ---
library(dplyr)
library(tidyr)
library(jsonlite)
library(readxl)
library(openxlsx)
library(officer)
library(zip)

# --- Visualization & Theme Dependencies ---
library(ggplot2)
library(ggpubr)
library(plotly)
library(RColorBrewer)
library(viridis)
library(latticeExtra)
library(patchwork)
library(fresh)
library(showtext)
library(scales)
library(commonmark)
library(glue)

# --- Machine Learning & Diagnostics Dependencies ---
library(randomForest)
library(DALEX)
library(yardstick)
library(agricolae)
library(mgcv)

# --- Async & Parallel Core Dependencies ---
library(future)
library(furrr)
library(promises)

# --- Enable Automatic Font Rendering ---
showtext_auto()

# --- Register Static Asset Path ---
addResourcePath("assets", file.path(getwd(), "assets"))

# --- Configure Safe Multisession Future Plan ---
# Prevents leaking socket connections during hot reloads or multiple user sessions
if (!inherits(future::plan(), "multisession")) {
  future::plan(future::multisession)
}

# --- Source v0.9.7d Helper Modules ---
source("ui_helpers_0.9.7d.R")
source("spatial_helpers_0.9.7d.R")
source("theme_helpers_0.9.7d.R")
source("gov_module_0.9.7d.R")
source("desc_exploratory_module_0.9.7d.R")

