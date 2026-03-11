# Monolith 0.8.8: Advanced Spatial Analysis Dashboard
*Monolith* is a high-performance R Shiny application designed for proper (or a standardized, at least) spatial statistical analysis, geostatistical modeling, and mapping. It provides a comprehensive toolkit for exploring spatial variability, you may find it well-suited for research in soil science, life sciences, and agronomy.

Whether you are mapping soil physicochemistry, analyzing topographical interactions, or generating publication-ready **spatial, descriptive and multi-criteria explorative metrics**, Monolith provides a seamless, parallel-processed environment to ingest, interpolate, interpret and export the data for continuous and classified maps.

# Key Features
* **Diverse Spatial Engine**: Implement deterministic and geostatistical interpolation models for contunious and classified maps, including Inverse Distance Weighting (IDW), Thin Plate Splines (TPS), Ordinary Kriging (OK), Co-Kriging (CK), Regression Kriging (RK), and Random Forest Kriging (RFK).

  ![Alternative Text](assets/1.png) 
  ![Alternative Text](assets/2.png) 
  
  
  
* **Automated & Interactive Manual Optimization of Model Fittings**: Automated least-squares fitting for variograms for four different models, Generalized Cross-Validation (GCV) for TPS, and Leave-One-Out Cross-Validation (LOOCV) based power optimization for IDW. Dynamic variogram fitting and manual tuning overrides are available for expert calibration. When the interpolation run, each results of it will be instantly available  for batch export.

  ![Alternative Text](assets/3.png) 


* **Comprehensive Diagnostics**: Evaluate models through LOOCV. Generate advanced metrics including Concordance Correlation Coefficient (CCC), Nash-Sutcliffe Efficiency (NSE), RPD, RPIQ, and Moran's I for spatial autocorrelation.

  ![Alternative Text](assets/4.png) 


* **Descriptive & Exploratory Suite**: Understand your dataset: Simultaneous descriptive, correlation, principal component and factor analyses.

  ![Alternative Text](assets/5.png) 

* **Unified Export Registry**: Compile session assets into a centralized registry. Use the integrated WYSIWYG Styler to customize typography, DPI, and layout for publication-ready outputs (.PNG, .TIFF, .PDF) or batch-export everything with statistical tabular data merged into a Excel file.

  ![Alternative Text](assets/6.png) 


* **Dynamic UI & Theming**: Fully responsive interface with customizable, accessible themes (e.g., Deep Forest, Obsidian Night) and dual-map comparison modes for visual audits of actual vs. predicted surfaces.

  ![Alternative Text](assets/7.png) 


## Mapping Predictions and Interpreting Spatial Resonance of Prediction Errors 

**1. Visual Validation**

Monolith generates side-by-side "Actual" and "Predicted" surfaces. By matching the color scales, you can instantly verify if the model captures the true variance of the field or just smooths the data.

  ![Alternative Text](assets/8.png) 


**2. Residual Diagnostics**

To understand the spatial structure of model errors, Monolith provides two diagnostic maps:

*Surface Delta (Regional Bias):* Subtracts the predicted surface from the actual surface to reveal zones of consistent over- or under-prediction.

*Point Errors (Predictive Model Uncertainty)*: Interpolates prediction errors at exact sampling points to map zones where the model fails to capture local variation.

  ![Alternative Text](assets/9.png) 


**3. Actionable Agronomical Output**

Monolith’s classification engine automatically translates continuous predictions (e.g., Nitrogen levels) into standard agronomical zones see information section for default limits that are valid for Eastern Mediterrenean Turkey. It outputs exact area coverages (in hectares).


[def]: assets/2.png