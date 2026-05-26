# global_0.9.6c.R - Centralized Configuration & Dependencies for Monolith v0.9.6c
# Copyright (c) 2026 Recep Serdar Kara. All rights reserved.

# --- Smart Auto-Installation & Package Loading Hook ---
required_packages <- c(
  "shiny", "shinyjs", "shinyWidgets", "shinyFiles", "shinycssloaders", "DT",
  "sf", "terra", "tidyterra", "leaflet", "leaflet.extras", "ggspatial", "fields",
  "classInt", "gstat", "automap", "concaveman", "spdep", "FNN",
  "dplyr", "tidyr", "jsonlite", "readxl", "openxlsx", "officer", "zip",
  "ggplot2", "ggpubr", "plotly", "RColorBrewer", "viridis", "latticeExtra",
  "patchwork", "fresh", "showtext", "scales", "commonmark",
  "randomForest", "DALEX", "yardstick", "agricolae", "mgcv",
  "future", "furrr", "promises", "progressr"
)

# Identify missing packages
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]

# Install missing ones using a default cloud repository (non-interactively)
if (length(missing_packages) > 0) {
  message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

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
library(progressr)

# --- Enable Automatic Font Rendering ---
showtext_auto()

# --- Register Static Asset Path ---
addResourcePath("assets", file.path(getwd(), "assets"))

# --- Configure Safe Multisession Future Plan ---
# Prevents leaking socket connections during hot reloads or multiple user sessions
if (!inherits(future::plan(), "multisession")) {
  future::plan(future::multisession)
}

# --- Source v0.9.7 Helper Modules ---
source("ui_helpers_0.9.7.R")
source("spatial_helpers_0.9.7.R")
source("theme_helpers_0.9.7.R")
source("gov_module_0.9.7.R")
source("desc_exploratory_module_0.9.7.R")

