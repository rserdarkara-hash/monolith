# Technology Stack

## 1. Primary Language & Extensibility
- **R:** The core language for the application, handling all statistical, spatial, and data processing logic.
- **Python (Optional Integration):** Python is authorized to be introduced into the stack at any time via tools like `reticulate` or as external microservices. This provides flexibility to leverage Python's vast ecosystem for enhanced machine learning, advanced visualizations, streamlined data pipelines, or performance improvements wherever practical or scientifically necessary.

## 2. Web & Interactive Framework
- **Shiny:** Framework for building the interactive web application.
- **ShinyJS / shinyjs:** For dynamic UI control (e.g., disabling buttons).
- **commonmark:** *[Added v2.6]* For fast, robust parsing of Markdown documentation into HTML without strict `xfun` or `knitr` dependencies.
- **fresh:** *[Added v2.2_theme]* For creating highly customized, "plug-and-play" aesthetic themes and integrating Google Fonts.
- **shinyWidgets:** For advanced UI components like pickerInput with visual palette swatches.
- **shinycssloaders:** For loading animations (spinners) during async operations.

## 3. Spatial Analysis & Geostatistics
- **sf (Simple Features):** Standard for vector spatial data handling.
- **terra:** High-performance replacement for the `raster` package.
- `gstat:` Core library for geostatistical modeling, including Variograms, Ordinary Kriging, and Co-Kriging.
- `agricolae:` *[Added v0.8.4]* For performing specialized post-hoc statistical significance tests (Duncan's, Tukey's HSD).
- `automap:` For automated Kriging parameter search and model selection.
- **tidymodels:** *[Added 2026-02-25]* Integrated for robust, modular machine learning pipelines (replacing raw `randomForest` and `lm` calls).
- **yardstick:** Component of tidymodels used for standardizing numeric and multi-class prediction performance metrics (Kappa, MCC, NSE, RPIQ, etc.).
- **DALEX:** *[Added v0.8.5]* Core engine for machine learning model explainability, providing SHAP profiles, ALE interactions, and robust permutation-based variable importance metrics.
- **randomForest:** For machine learning-based trend modeling in RF-Kriging (now wrapped via `parsnip`).  
- **fields:** Used for Thin Plate Spline (TPS) surface distribution modeling.
- **spdep:** *[Added v1.9]* Used for high-efficiency, sparse-matrix spatial weight calculations and Moran's I diagnostics.
- **FNN:** For fast nearest neighbor search (k=1) to optimize resolution heuristics.
- **concaveman:** For efficient generation of concave hulls.

## 4. Visualization & Cartography
- **leaflet:** For interactive web maps.
- **ggplot2:** Standard for high-quality static plotting.
- **plotly:** *[Added v0.8.1]* Utilized for creating highly interactive (hover, zoom, pan) expandable modals for all visualizations and generating 3D PCA Biplots.
- **tidyterra:** Integration between `terra` and `ggplot2`.
- **ggspatial:** For adding scale bars and north arrows to static ggplot2 figures.
- **leaflet.extras:** Extension for leaflet maps allowing advanced user drawing interactions.
- **patchwork:** For combining multiple ggplot2 objects into synchronized side-by-side figures.
- **RColorBrewer / viridis:** Standard scientific color palettes.
- **latticeExtra:** For advanced layering in variogram plots.
- **showtext:** *[Added v2.3]* For advanced Google Fonts support and high-fidelity typographical rendering across all export formats.

## 5. Optimization & Performance
- **future / promises:** For asynchronous, non-blocking background processing in Shiny.
- **future / furrr:** *[Updated v2.1]* Replaced `future.apply` with `furrr` for expressive functional parallel programming (using `future_map`).
- **multisession (future):** *[Updated v2.1]* Configured for nested parallelism (`plan(list(multisession, multisession))`) to resolve localities and parameter grids concurrently.
- **progressr:** *[Added v2.1]* Provides a unified progress reporting framework for parallel operations within the Shiny UI.

## 6. Data Wrangling & Integration
- **tidyverse (dplyr, tidyr, purrr):** For efficient data manipulation and functional programming (essential for batch automation).
- **readxl:** For importing spreadsheet-based soil data.
- **jsonlite:** For saving and loading user configurations and metadata.

## 7. Reporting & Exports
- **rmarkdown / knitr:** For generating automated HTML/PDF summary reports.
- **officer / flextable:** *[Added 2026-02-28]* For high-quality, scientifically structured Word document generation and tabular data formatting.
- **openxlsx:** *[Added v2.3]* For professional, multi-sheet Excel workbook generation (compiling all session statistics into a single file).
- **zip:** *[Added 2026-03-06]* For reliable generation of .zip archives to support batch export downloads over cloud deployment environments.
- **Unified Export Registry & Styler:** *[Added v2.3]* Centralized system for capturing all session assets (Maps, Plots, Tables). *[Updated v2.4]* Unified styling engine (`generate_styled_plot`) ensures 100% fidelity between UI preview and final exports by synchronizing aspect ratios (10:8), text orientation, and 2.5x DPI calibration.
- **Publication-Ready Figures:** Support for high-resolution exports using `grDevices`, `lattice`, and `ggplot2::ggsave()` in the following formats:
    - **TIFF:** Tagged Image File Format (standard for journals, supports LZW compression).
    - **PNG:** Portable Network Graphics (lossless web/presentation standard).
    - **JPEG:** Joint Photographic Experts Group (standard image format).
    - **PDF:** Portable Document Format (vector-based, infinitely scalable).
