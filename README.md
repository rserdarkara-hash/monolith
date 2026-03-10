# Monolith 0.8.7: Advanced Spatial Analysis Dashboard
*Monolith* is a high-performance R Shiny application designed for proper (or a standardized, at least) spatial statistical analysis, geostatistical modeling, and mapping. It provides a comprehensive toolkit for exploring spatial variability, you may find it well-suited for research in soil science, life sciences, and agronomy.

Whether you are mapping soil physicochemistry, analyzing topographical interactions, or generating publication-ready **spatial, descriptive and multi-criteria explorative metrics**, Monolith provides a seamless, parallel-processed environment to ingest, interpolate, interpret and export the data.

# Key Features
* Diverse Spatial Engine: Implement deterministic and geostatistical interpolation models, including Inverse Distance Weighting (IDW), Thin Plate Splines (TPS), Ordinary Kriging (OK), Co-Kriging (CK), Regression Kriging (RK), and Random Forest Kriging (RFK).

  ![Alternative Text](assets/1.png)
* 
  
* Automated & Manual Optimization: Automated least-squares fitting for variograms for four different models, Generalized Cross-Validation (GCV) for TPS, and Leave-One-Out Cross-Validation (LOOCV) based power optimization for IDW. Dynamic variogram fitting and manual tuning overrides are available for expert calibration. When the interpolation run, every result related to that and many extra statisticaligure and table will be available for batch export.
* Comprehensive Diagnostics: Evaluate models through LOOCV. Generate advanced metrics including Concordance Correlation Coefficient (CCC), Nash-Sutcliffe Efficiency (NSE), RPD, RPIQ, and Moran's I for spatial autocorrelation.
* Descriptive & Exploratory Suite: Understand the nature and shaping factors of your dataset.
* Unified Export Registry: Compile session assets into a centralized registry. Use the integrated WYSIWYG Styler to customize typography, DPI, and layout for publication-ready outputs (.PNG, .TIFF, .PDF) or batch-export everything with statistical tabular data merged into a Excel file.
* Dynamic UI & Theming: Fully responsive interface with customizable, accessible themes (e.g., Deep Forest, Obsidian Night) and dual-map comparison modes for visual audits of actual vs. predicted surfaces.

## Mapping Predictions and Interpreting Spatial Resonance of Prediction Errors 

**1. Visual Validation**

Monolith generates side-by-side "Actual" and "Predicted" surfaces. By matching the color scales, you can instantly verify if the model captures the true variance of the field or just smooths the data.

**2. Residual Diagnostics**

To understand the spatial structure of model errors, Monolith provides two diagnostic maps:

*Surface Delta (Regional Bias):* Subtracts the predicted surface from the actual surface to reveal zones of consistent over- or under-prediction.

*Point Errors (Predictive Model Uncertainty)*: Interpolates prediction errors at exact sampling points to map zones where the model fails to capture local variation.

**3. Actionable Agronomical Output**

Monolith’s classification engine automatically translates continuous predictions (e.g., Nitrogen levels) into standard agronomical zones see information section for default limits that are valid for Eastern Mediterrenean Turkey. It outputs exact area coverages (in hectares).
