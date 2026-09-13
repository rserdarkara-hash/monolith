# Checked before anything is attached. The README states R >= 4.5.0 as a
# requirement, and without this the requirement is only documentation: an older
# session fails much later, with an error from whichever package breaks first
# rather than one naming the actual cause.
if (getRversion() < "4.5.0") {
  stop("Monolith requires R 4.5.0 or higher; this session is R ", getRversion(),
       ". See README, System Prerequisites.", call. = FALSE)
}

required_packages <- c(
  "shiny", "shinyjs", "shinyWidgets", "shinyFiles", "shinycssloaders", "DT",
  "sf", "terra", "tidyterra", "leaflet", "leaflet.extras", "ggspatial", "fields",
  "classInt", "gstat", "concaveman", "spdep", "FNN",
  "dplyr", "tidyr", "jsonlite", "readxl", "openxlsx", "officer", "zip",
  "ggplot2", "ggpubr", "plotly", "RColorBrewer", "viridis",
  "patchwork", "showtext", "scales", "commonmark", "glue",
  "randomForest", "DALEX", "yardstick", "agricolae", "mgcv",
  "parsnip", "recipes", "workflows", "tune", "rsample", "dials",
  "spatialsample", "hardhat", "ranger", "xgboost", "nnet",
  "future", "furrr", "promises", "nortest", "data.table", "fs",
  # DBI + RSQLite read PROJ's own proj.db catalogue (crs_registry(),
  # global_utils.R) to shortlist candidate CRS by area of use. Called through
  # ::, never attached; their absence degrades Tier 3 to the zone family
  # instead of failing.
  "DBI", "RSQLite"
)

missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages) > 0) {
  # WHY THIS DOES NOT SIMPLY INSTALL. install.packages() fetches whatever CRAN
  # publishes today, and the numeric test layer (tests/testthat/fixtures/) pins
  # values measured under the versions in renv.lock: a change in gstat's
  # variogram fit or fields' GCV moves them. Installing without asking is
  # therefore the one action that can invalidate a recorded baseline with
  # nothing in the repository having changed.
  #
  # It does NOT follow that everyone should be sent through renv::restore().
  # Every locked version CRAN has since superseded is restored from source, which
  # needs a C/C++/Fortran toolchain (Rtools on Windows), and that share grows
  # with the lockfile's age. Those pins
  # matter for reproducing the recorded test values, not for analysing your own
  # data. So both routes are named with their real cost, and nothing installs
  # without an explicit yes.
  #
  # MONOLITH_ALLOW_LATEST=true skips the question. It exists for the
  # upstream-drift CI job (.github/workflows/upstream.yaml), which runs against
  # newer packages on purpose; it is not a convenience switch.
  allow_latest <- isTRUE(as.logical(Sys.getenv("MONOLITH_ALLOW_LATEST", "false")))

  if (!allow_latest) {
    # The A/B lettering earns its place only when there is a B: a stripped copy
    # of the repository carries no lockfile, and there is then one route.
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

    # Never prompt when non-interactive: the test harness sources this file, and
    # a prompt there would hang the suite.
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
library(ggpubr)
library(plotly)
library(RColorBrewer)
library(viridis)
library(patchwork)
library(showtext)
library(scales)
library(commonmark)
library(glue)

library(randomForest)
library(DALEX)
library(yardstick)
library(agricolae)
library(mgcv)
library(nortest)

# Classification suite backbone (tidymodels). Engine packages (ranger, xgboost,
# nnet) are installed via required_packages and loaded on demand by parsnip at
# fit time, so they are not attached here.
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

# Single source of the app version for anything the UI displays. DESCRIPTION is
# the authority (README/CHANGELOG are release documents maintained by hand), so
# a release only has to touch the version in one place that the code reads.
app_version <- tryCatch(
  unname(read.dcf("DESCRIPTION", fields = "Version")[1, 1]),
  error = function(e) "unknown"
)
if (is.na(app_version)) app_version <- "unknown"

showtext_auto()

addResourcePath("assets", file.path(getwd(), "assets"))

# Shiny rejects any upload above its request cap (5 MB by default) before a
# handler runs, which made the 30 MB checks on the data and metadata tables in
# server_data_setup.R unreachable and refused ordinary boundary shapefile sets.
# The cap covers a whole .shp set in one request; tables keep their 30 MB limit.
options(shiny.maxRequestSize = 200 * 1024^2)

if (!inherits(future::plan(), "multisession")) {
  future::plan(future::multisession)
}

source("ui_helpers.R")
source("spatial_helpers.R")
source("classif_helpers.R")
source("theme_helpers.R")
source("gov_module.R")
source("desc_exploratory_module.R")
source("classif_module.R")

