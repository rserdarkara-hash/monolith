# Product Guide: Monolith Spatial Analysis Dashboard

## 1. Scientific Vision & Objective
The Monolith Spatial Analysis Dashboard is a robust, all-encompassing geostatistical application developed in R (Shiny). Its core objective is to serve as a comprehensive workbench for agronomists, soil scientists, and researchers to perform highly meticulous spatial mapping, interpolation, prediction performance evaluation, and field zoning. This setup serves as the foundational documentation to facilitate future iterative improvements, modularization, and feature enhancements through Conductor tracks while preserving the mathematical and functional integrity of the existing monolith.

## 2. Target Audience & Roles
- **Geostatistical Researchers & Scientists:** Users who require deep statistical validation, variogram modeling, and cross-validation metrics (RMSE, R2, Lin's CCC, Moran's I) to rigorously evaluate predictive spatial models.
- **Agronomists & Advisors:** Domain experts utilizing the tool to interpolate empirical soil properties (e.g., pH, TN, P, K, Ca, Mg) and analyze spatial variability to prescribe targeted agronomical treatments. 
- **Farmers & Agricultural Managers:** Stakeholders interpreting the final high-resolution maps and management zones to apply precision agriculture practices.

## 3. Core Capabilities & Scientific Rigor

### 3.1 Advanced Spatial Interpolation Engines
The dashboard integrates multiple state-of-the-art interpolation algorithms to convert discrete point sampling data into continuous predictive surfaces:
- **Ordinary Kriging (OK):** Utilizes spatial autocorrelation and robust semi-variogram fitting to provide best linear unbiased predictions. *[Updated v2.4]* Implements epsilon-nugget stability fixes and NaN protection for low-variance variables like Iron (Fe).
- **Regression Kriging (RK) & Co-Kriging (CK):** Incorporates auxiliary environmental covariates to improve prediction accuracy. *[Updated v2.0]* CK includes standardized covariate processing and transparent fallback protocols.
- **Random Forest Kriging (RFK):** Blends machine learning (Random Forest) with geostatistical methods to handle non-linear spatial relationships. *[Updated v2.0]* Utilizes rigorous Out-of-Bag (OOB) predictions for structural residual fitting.
- **Inverse Distance Weighting (IDW):** Deterministic interpolation with RMSE-based power optimization for tuning the distance-decay parameter.
- **Thin Plate Spline (TPS):** Smoothing splines with Generalized Cross-Validation (GCV) optimization to balance data fit and surface smoothness. *[Updated v2.2]* Restored robust internal GCV optimization with realistic LOOCV calculations to prevent overfitting metrics.

### 3.2 Automation & Parameter Optimization
- **Variogram Auto-Fit:** Automatically selects the optimal theoretical semi-variogram model (e.g., Spherical, Exponential, Gaussian) based on empirical data distribution.
- **Covariate Standardization:** *[Updated v2.0]* Unmeasured grid covariates are dynamically interpolated via Ordinary Kriging (OK) instead of IDW. *[Updated v2.1]* Implements a `tryCatch` safety wrapper with automatic IDW fallback for problematic covariates (e.g., singular matrices or pure nugget effects).
- **Dynamic Resolution Logic:** Implements the "Strasov Fix" to dynamically calculate grid resolution, ensuring high-fidelity rendering (minimum 300 cells on the longest axis) while remaining mathematically aware of coordinate reference systems (e.g., degrees vs. meters).
  - *[Updated v2.5]* Tracks, retains, and displays independent resolution calculations for each selected locality natively within the Map Viewer overlay and UI tables.
- **Concurrent Optimization:** *[Added v2.1]* Simultaneously evaluates parameter grids (e.g., variogram models, IDW factors, TPS lambdas) and processing localities using nested `future` sessions.

### 3.3 Diagnostic Metrics & Validation
Provides an exhaustive suite of cross-validation (CV) diagnostics to quantify model uncertainty and prediction accuracy:
- **Root Mean Square Error (RMSE), R2 (Correlation), R2 (NSE/Traditional), and Bias (ME):** Fundamental metrics for evaluating the magnitude of prediction error, variance explained, and model efficiency.
- **Lin's Concordance Correlation Coefficient (CCC):** Evaluates the degree of agreement between predicted and observed values.
- **High-Precision Geostatistical Metrics:** Expanded tracking for all models, including NSE, NRMSE (%), RPD (Ratio of Performance to Deviation), RPIQ (Ratio of Performance to Interquartile Range), and SMAPE for robust evaluation of highly skewed soil metrics.
- **Classification Performance Metrics:** Capabilities to map regression predictions onto user-defined Agronomical Classes or Quartiles, calculating robust multi-class diagnostics such as Cohen's Kappa, Weighted Kappa (Linear), Overall Accuracy, Balanced Accuracy, and Matthews Correlation Coefficient (MCC).
- **Moran's I:** Measures spatial autocorrelation of residuals to diagnose model bias. *[Updated v2.2]* Explicitly exposed in the main spatial interpolation statistics UI.
- **Dual Residual Mapping:** *[Added v2.4]* Distinct support for 'Interpolated Delta' (Surface Difference) and 'Interpolated Point Errors' (Model Deviation) with explicit mathematical definitions and scientific interpretation guides in the UI.
- **Full-Pipeline Cross-Validation:** *[Updated v1.9]* Regression Kriging (RK) and RF-Kriging (RFK) utilize scientifically rigorous LOOCV where the trend model is re-fitted per fold using known covariates at test locations to ensure unbiased error estimation.
- **Prediction Uncertainty Mapping:** *[Updated v2.0]* Rigorously quantifies prediction uncertainty for hybrid models by summing the trend model's variance with the residual kriging variance.
- **Structural Dependence Metric:** Quantifies the proportion of spatial variance explained by the structural model component rather than the nugget effect.
- **Residual Analysis & VIF:** Checks for variance inflation factors (strictly applied to linear RK models) and residual clustering.
## 4. UI Architecture & Functional Workflow

### Tab 1: Data Setup & Pre-processing
- **Ingestion:** Supports diverse data formats, including coordinate-mapped CSV/Excel files and standard vector formats (Shapefiles).
- **Branded Dashboard Header:** *[Added v0.8.7]* Replaces the traditional text title with a high-resolution, responsive banner (`banner.png`). The header is optimized for aesthetic consistency across all themes and includes an integrated 'About' module for instant version and project identification.
- **In-App Scientific Documentation:** *[Added v2.6]* A comprehensive, sliding-drawer 'Information & Documentation Module' that provides on-demand access to mathematical intuitions, agronomical examples, and rigorous UI workflows directly within the app, powered by dynamically loaded HTML/Markdown. Contextual popovers provide instant, dismissible explanations for complex UI parameters without cluttering the screen.
- **Spatial Alignment:** User-driven Coordinate Reference System (CRS) selection (defaulting to EPSG:4326/32635) to ensure accurate spatial projections.
- **Dynamic Theme Engine:** *[Added v2.2_theme]* Features a robust, plug-and-play theme switcher with 10 distinct aesthetic palettes (e.g., Deep Forest, Obsidian Night, Cyberpunk). 
  - **Aesthetic Consistency:** Automatically adjusts colors, typography (via Google Fonts), and UI element styles (e.g., sidebars, panels) to maintain visual harmony.
  - **Context-Aware Mapping:** Synchronizes map base tiles with the active theme. *[Updated v2.4]* Includes global scale synchronization ('Match Scales') across comparison views.
  - **Persistence:** Remembers the user's preferred theme across browser sessions using local storage.
- **Semantic Mapping:** Integrates external metadata configurations (e.g., 'variable list.xlsx') to dynamically assign scientific labels, categorizations, and agronomical color palettes to variables. *[Updated v2.2]* These labels are now propagated throughout the UI, including predictor selection and correlation rank modules, with new significance filters based on calculated P-values.

### Tab 2: Interactive Map Viewer
- **Dynamic Visualization:** Leverages Leaflet for responsive, multi-layered spatial visualization.
- **Enhanced UX:** *[Updated v0.8.4]* Implements a detailed real-time progress bar overlay reporting specific algorithmic steps during map generation, and a "Map Reveal" button to conceptualize model finalization. Includes manual map redraw controls to resolve UI rendering lag.
- **Interactive Map Panning:** *[Added v0.8.6]* Features a dynamic locality-based focus dropdown next to base map selection. Users can instantly pan the viewport to specific mapped localities or reset to a 'Global View' (synchronized across dual-view modes) without re-rendering the spatial models.
- **Dynamic Base Layers:** *[Updated v0.8.2]* Allows real-time switching of underlying Leaflet tile providers (Satellite, Topographic, Dark Matter, etc.) without reloading the map.
- **Comparative Views:** Allows toggling between 'Actual' (sampled), 'Predicted' (modeled), 'Comparison', and 'Residual' maps to visually assess interpolation accuracy.
- **Clean Titles:** *[Updated v2.5]* Cleaned, unified titles (removing raw category brackets) across all map viewers and exported views.
- **Pop-up Engine:** A highly customized interaction layer that reveals deep point-specific data, metadata, and coordinates upon user interaction.
- **Interactive Spatial Selection:** Integrated drawing tools (polygons, rectangles) allow users to select specific sample points, dynamically assign them to custom "Localities" or "Analysis Groups," and export the augmented dataset for immediate downstream analysis.

### Tab 3: Scientific Analysis & Metrics Panel
- **Geostatistical Plots:** Generates experimental and fitted variogram plots for visual inspection of spatial autocorrelation.
- **Quantitative Zoning:** Computes precise surface area coverage statistics (in Hectares) for different management zones or nutrient classes.

### Tab 5: Scientific Analytics Engine (Descriptive, Correlation, PCA)
*[Added v0.8.1]* An advanced statistical evaluation suite designed to operate pre- or post-interpolation:
- **Descriptive & Exploratory Suite:** Features multi-factor grouping and auto-discretization logic to generate rich data distributions (Histograms, Density, Box/Violin, Sina-plots, Ridge Plots, ECDF, Heatmaps, Radars, and multi-fit XYZ Surfaces).
  - **Significance Testing:** *[Updated v0.8.4]* Expanded with integrated ANOVA and post-hoc significance testing (Duncan’s, Tukey’s HSD). Significance letters are dynamically mapped to Box, Violin, and Sina plots. Variable selections now persist across different analysis styles within the session.
  - **Ghosting Overlay:** Dynamically overlays a selected local sub-population atop the global background dataset to instantly visually compare local traits vs. global distributions.
- **Correlation Analysis:** Provides interactive Hierarchical Clustering Heatmaps, Correlation Networks, Correlograms, Lagged CCFs, and exact Partial Correlation controls (via regression residual extraction), fully synced with the active categorical groupings.
- **Principal Component Analysis (PCA):** A dedicated high-dimensional module featuring interactive Scree, Biplot (2D/3D), Loadings, Contribution, and Mahalanobis Distance plots. 
  - **Automated Collinearity Filter:** Automatically intercepts near-perfect collinearity (r > 0.95) and prevents execution distortions, providing a warning UI and manual override.
- **Governing Factors:** *[Added v0.8.5]* A new multi-scale influence and causality diagnostics module. It leverages Random Forest variable importance alongside SHAP (SHapley Additive exPlanations) and ALE (Accumulated Local Effects) plots to reveal non-linear dependencies and feature interactions between target agronomical parameters and potential governing factors.
- **Expandable Plot Engine:** Integrates a toggleable interactive layer using `plotly` within full-screen modal overlays to allow deep inspection and hovering on all major statistical outputs.

### Tab 4: Unified Export & Reporting
- **Session Export Registry:** *[Added v2.3]* Centralized tracking of all plots, maps, and tables generated during a session.
- **Advanced Export Styler:** *[Added v2.3]* A high-fidelity modal interface for customizing typography (Google Fonts), granular font sizes, axis overrides, and output quality. 
  - *[Updated v2.5]* Features a tabbed UI (Basic vs. Advanced), rigorous plot margin controls, dynamic scale bar sizing, and new Publication Modifiers (e.g., High Contrast Colorblind Safe palettes).
  - *[Updated v2.5]* **Configuration Persistence:** Styler settings are natively saved/loaded across sessions via browser Local Storage, and can be manually exported/imported as JSON files.
- **Multi-Format Reporting:** *[Added v2.3]* Support for professional-grade exports including PNG, TIFF, PDF, and JPEG for plots, plus unified multi-sheet Excel workbooks for statistical data (Performance, Area, Variogram data).
- **Quick Export Integration:** *[Added v2.3]* Instant "one-click" styler access directly from the Map Viewer.

## 5. Architectural Roadmap (v2.6 & v0.8.7 Iterations)
The application has matured into a high-performance, publication-ready geostatistical workbench. Version 2.6 introduced a built-in educational layer via a dynamic sliding drawer. Version 0.8.7 formalizes the application's visual identity by transitioning from text-based titles to responsive branded components and dedicated project information modals. Previous versions formalized UI cleanliness and rigorous export customizations, introducing fully responsive styler components, JSON-based persistent UI configurations, and colorblind-safe publication modifiers. Future iterations will focus on further modularization and potential integration of Python-based machine learning modules.
