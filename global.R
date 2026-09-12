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
  # WHY THIS STOPS INSTEAD OF INSTALLING. install.packages() fetches whatever
  # CRAN publishes today, and this app's numeric test layer pins recorded values
  # (tests/testthat/fixtures/) that only hold for the versions in renv.lock: a
  # change in gstat's variogram fit or fields' GCV moves them. Installing latest
  # is therefore the one action that can silently invalidate the baselines, and
  # it costs the user no less than restoring the validated set - the download is
  # the same size either way. So while a lockfile is present, the app names what
  # is missing and points at the one command that installs the right versions.
  #
  # MONOLITH_ALLOW_LATEST=true opts back into installing latest. It exists for
  # the upstream-drift CI job (.github/workflows/upstream.yaml), whose whole
  # purpose is to run against newer packages on purpose; it is not a convenience
  # switch for ordinary use.
  allow_latest <- isTRUE(as.logical(Sys.getenv("MONOLITH_ALLOW_LATEST", "false")))
  missing_txt <- paste(missing_packages, collapse = ", ")

  if (!allow_latest && file.exists("renv.lock")) {
    stop("Missing packages: ", missing_txt,
         "\n\nInstall the validated versions (renv.lock) with:",
         "\n    install.packages(\"renv\")   # once",
         "\n    renv::restore()",
         "\n\nThese are the versions the app's recorded test baselines were taken",
         " under. See README, section 3.", call. = FALSE)
  }

  # No lockfile (a stripped copy of the repository): offer the install, since
  # there is no validated set to point at. Never prompt when non-interactive -
  # the test harness sources this file, and a prompt there would hang the suite.
  if (!allow_latest && !interactive()) {
    stop("Missing packages: ", missing_txt,
         "\n\nInstall them with:",
         "\n    install.packages(c(\"", paste(missing_packages, collapse = "\", \""), "\"))",
         call. = FALSE)
  }
  if (!allow_latest) {
    ans <- utils::menu(c("Yes", "No"),
                       title = paste0(length(missing_packages),
                                      " package(s) are missing: ", missing_txt,
                                      "\nInstall them from CRAN now (latest versions)?"))
    if (!identical(ans, 1L)) {
      stop("Cancelled: ", length(missing_packages), " package(s) still missing.",
           call. = FALSE)
    }
  }

  message("Installing missing packages from CRAN (latest versions): ", missing_txt)
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

