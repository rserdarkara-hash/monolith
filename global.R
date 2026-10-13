# Checked before anything is attached, so an older R fails here with the cause
# named instead of later inside whichever package breaks first.
if (getRversion() < "4.5.0") {
  stop("Monolith requires R 4.5.0 or higher; this session is R ", getRversion(),
       ". See README, System Prerequisites.", call. = FALSE)
}

required_packages <- c(
  "shiny", "shinyjs", "shinyWidgets", "shinyFiles", "shinycssloaders", "DT",
  "sf", "terra", "tidyterra", "leaflet", "leaflet.extras", "ggspatial", "fields",
  "classInt", "gstat", "concaveman", "spdep", "FNN",
  # Exact Jenks breaks (natural_breaks(), spatial_pipeline.R); called via ::.
  "Ckmeans.1d.dp",
  "dplyr", "tidyr", "jsonlite", "readxl", "openxlsx", "officer", "zip",
  "ggplot2", "plotly", "RColorBrewer", "viridis",
  "patchwork", "showtext", "scales", "commonmark",
  "randomForest", "DALEX", "yardstick", "agricolae", "mgcv",
  "parsnip", "recipes", "workflows", "tune", "rsample", "dials",
  "spatialsample", "hardhat", "ranger", "xgboost", "nnet",
  "future", "furrr", "promises", "nortest", "data.table", "fs",
  # Read PROJ's proj.db for the CRS candidate shortlist (crs_registry());
  # called via ::. Without them the shortlist falls back to crs_zone_candidates().
  "DBI", "RSQLite",
  # Installed by other packages but called directly via ::, so a missing copy
  # is named here: parallelly (nested clusters), units, rlang, htmltools,
  # tibble and viridisLite (Classification Suite).
  "parallelly", "units", "rlang", "htmltools", "tibble", "viridisLite"
)

missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages) > 0) {
  # Never installed silently: the recorded test baselines were measured under
  # the renv.lock versions, while renv::restore() rebuilds superseded versions
  # from source (needs a compiler). Both routes are named and nothing installs
  # without an explicit yes. MONOLITH_ALLOW_LATEST=true skips the question for
  # the upstream-drift CI job (.github/workflows/upstream.yaml).
  allow_latest <- isTRUE(as.logical(Sys.getenv("MONOLITH_ALLOW_LATEST", "false")))

  if (!allow_latest) {
    # Without renv.lock there is only one route, so no A/B lettering.
    has_lock <- file.exists("renv.lock")
    routes <- paste0(
      length(missing_packages), " package(s) are missing: ",
      paste(missing_packages, collapse = ", "),
      if (has_lock) "\n\nA. Current CRAN releases." else "\n\nCurrent CRAN releases.",
      " Pre-built binaries, a few minutes, no compiler:",
      "\n     install.packages(c(\"", paste(missing_packages, collapse = "\", \""), "\"))",
      if (has_lock) "\n   This is the route for using the app." else "")
    if (has_lock) {
      routes <- paste0(routes,
        "\n\nB. The versions renv.lock pins, which the recorded test baselines were",
        "\n   measured under:",
        "\n     install.packages(\"renv\")   # once",
        "\n     renv::restore()",
        "\n   Any that CRAN has since superseded build from source, which takes longer",
        "\n   and needs a C/C++/Fortran toolchain (Rtools on Windows). Needed only to",
        "\n   reproduce the recorded test values, not to analyse your own data.")
    }

    # Never prompt non-interactively: the test harness sources this file.
    if (!interactive()) {
      stop(routes, "\n\nSee README, section 3.", call. = FALSE)
    }

    message(routes, "\n")
    ans <- utils::menu(c("Install the current CRAN releases now", "Cancel"),
                       title = "Install the missing packages now?")
    if (!identical(ans, 1L)) {
      stop("Cancelled: ", length(missing_packages), " package(s) still missing.",
           call. = FALSE)
    }
  }

  message("Installing from CRAN (current releases): ",
          paste(missing_packages, collapse = ", "))
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinyFiles)
library(shinycssloaders)
library(DT)

library(sf)
library(terra)
library(tidyterra)
library(leaflet)
library(leaflet.extras)
library(ggspatial)
library(fields)
library(classInt)
library(gstat)
library(concaveman)
library(spdep)
library(FNN)

library(dplyr)
library(tidyr)
library(jsonlite)
library(readxl)
library(openxlsx)
library(officer)
library(zip)

library(ggplot2)
library(plotly)
library(RColorBrewer)
library(viridis)
library(patchwork)
library(showtext)
library(scales)
library(commonmark)

library(randomForest)
library(DALEX)
library(yardstick)
library(agricolae)
library(mgcv)
library(nortest)

# Classification backbone (tidymodels); parsnip loads the engine packages
# (ranger, xgboost, nnet) at fit time.
library(parsnip)
library(recipes)
library(workflows)
library(tune)
library(rsample)
library(dials)
library(spatialsample)
library(hardhat)

library(future)
library(furrr)
library(promises)

# App version shown in the UI; DESCRIPTION is the single authority.
app_version <- tryCatch(
  unname(read.dcf("DESCRIPTION", fields = "Version")[1, 1]),
  error = function(e) "unknown"
)
if (is.na(app_version)) app_version <- "unknown"

# The docs drawer's three guides, rendered once per app start.
DOCS_HTML <- lapply(c(user = "docs/user_guide.md", desc = "docs/desc_exploratory_guide.md",
                      sci = "docs/scientific_guide.md"), function(f) {
  HTML(commonmark::markdown_html(paste(readLines(f, warn = FALSE), collapse = "\n"),
                                 extensions = "table"))
})

showtext_auto()

addResourcePath("assets", file.path(getwd(), "assets"))

# Shiny's default 5 MB cap would refuse uploads before the handlers' own 30 MB
# table limits apply; 200 MB admits a whole boundary shapefile set.
options(shiny.maxRequestSize = 200 * 1024^2)

# The test helper opts in before sourcing, so no unused startup pool is spawned
# (its Windows socket handshake can fail).
if (isTRUE(getOption("monolith_test_sequential", FALSE))) {
  future::plan(future::sequential)
} else if (!inherits(future::plan(), "multisession")) {
  # One outer worker per CONCURRENT async task (run, optimizer, classification,
  # Governing Factors, CRS search); compute parallelism lives in the nested
  # PSOCK clusters. A used worker keeps ~470 MB (DLLs, GDAL/GEOS/PROJ arenas):
  # on 16 cores a per-core pool held 7.5 GB against 2.9 GB at six. Further
  # tasks queue in future_promise.
  future::plan(future::multisession,
               workers = min(6L, future::availableCores()))
}

source("ui_helpers.R")
source("spatial_helpers.R")
source("classif_helpers.R")
source("theme_helpers.R")
source("gov_module.R")
source("desc_exploratory_module.R")
source("classif_module.R")

