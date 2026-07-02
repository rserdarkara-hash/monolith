
required_packages <- c(
  "shiny", "shinyjs", "shinyWidgets", "shinyFiles", "shinycssloaders", "DT",
  "sf", "terra", "tidyterra", "leaflet", "leaflet.extras", "ggspatial", "fields",
  "classInt", "gstat", "automap", "concaveman", "spdep", "FNN",
  "dplyr", "tidyr", "jsonlite", "readxl", "openxlsx", "officer", "zip",
  "ggplot2", "ggpubr", "plotly", "RColorBrewer", "viridis", "latticeExtra",
  "patchwork", "fresh", "showtext", "scales", "commonmark", "glue",
  "randomForest", "DALEX", "yardstick", "agricolae", "mgcv",
  "future", "furrr", "promises", "nortest"
)

missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages) > 0) {
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
library(automap)
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
library(latticeExtra)
library(patchwork)
library(fresh)
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

library(future)
library(furrr)
library(promises)

showtext_auto()

addResourcePath("assets", file.path(getwd(), "assets"))

if (!inherits(future::plan(), "multisession")) {
  future::plan(future::multisession)
}

source("ui_helpers.R")
source("spatial_helpers.R")
source("theme_helpers.R")
source("gov_module.R")
source("desc_exploratory_module.R")

