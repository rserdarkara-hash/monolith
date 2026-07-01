# Technology Stack: Monolith

## 1. Core Language & Runtime
* **R (v4.5.0+)**: Primary language for data handling, spatial calculations, geostatistical modeling, and web dashboard logic.

## 2. Core Framework
* **R Shiny**: Web application framework used to build interactive dashboards.
* **Modular Structure**: UI and Server logic are modularized into dedicated files (`gov_module_0.9.8b.R`, `desc_exploratory_module_0.9.8b.R`) and utility scripts (`spatial_helpers_0.9.8b.R`, `ui_helpers_0.9.8b.R`, `theme_helpers_0.9.8b.R`).

## 3. Libraries & Package Registry
* **Spatial & Geostatistics**:
  * `sf`: Simple features for vector data.
  * `terra`: Raster data processing.
  * `tidyterra`: ggplot2 integration for terra spatial objects.
  * `gstat`: Variogram modeling and Kriging (Ordinary, Co-Kriging, Regression).
  * `fields`: Thin Plate Splines (TPS) interpolation.
* **Machine Learning**:
  * `randomForest`: Random Forest classification and regression modeling.
  * `DALEX`: Model-agnostic explanations (ALE, PDP, SHAP).
* **Visualization & Table Display**:
  * `ggplot2` / `ggpubr`: Publication-ready static plots.
  * `plotly`: Interactive charts and exploration.
  * `leaflet`: Interactive web mapping.
  * `DT`: Interactive tables.
* **Performance**:
  * `future` / `furrr`: Parallel processing execution wrapper.
* **UI Themes**:
  * `fresh`: Theme customizer for Shiny dashboard aesthetics.

## 4. Development & Runtime Guidelines
* **Auto-Loader (`global_0.9.8b.R`)**: Auto-installation hook for resolving missing dependencies dynamically from CRAN.
* **Flexible Implementation**: While we use the existing R Shiny architecture, we are open to refactoring or utilizing alternative packages if a better solution is needed to fix performance or architectural issues identified in `issues.md`.
