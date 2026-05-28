# Monolith 0.9.8a: Advanced Spatial Analysis Dashboard
*Monolith* is a R Shiny application designed for proper (or a standardized, at least) spatial statistical analysis, geostatistical modeling, and mapping. It provides a comprehensive toolkit for exploring spatial variability, you may find it well-suited for research in soil science, life sciences, and agronomy.

Whether you are mapping soil physicochemistry, analyzing topographical interactions, or generating publication-ready **spatial, descriptive and multi-criteria explorative metrics**, Monolith provides a seamless, parallel-processed environment to ingest, interpolate, interpret and export the data for continuous and classified maps.


# Key Features
* **Diverse Spatial Engine**: Deterministic and geostatistical interpolation models for contunious and classified maps of your small fields or vast plains you study on. Monolith’s classification engine automatically translates continuous predictions (e.g., Nitrogen levels) into standard agronomical zones. It outputs exact area coverages (in hectares).


  - Inverse Distance Weighting (IDW), 
  
  - Thin Plate Splines (TPS), 

  - Ordinary Kriging (OK), 

  - Co-Kriging (CK), 

  - Regression Kriging (RK), 
  
  - Random Forest Kriging (RFK).

  ![Alternative Text](assets/1.png) 
  ![Alternative Text](assets/2.png) 
  
  
* **Automated & Manual Optimization of Model Fittings**: Automated least-squares fitting for variograms for four different models, Generalized Cross-Validation (GCV) for TPS, and Leave-One-Out Cross-Validation (LOOCV) based power optimization for IDW. Interactive variogram fitting and manual tuning overrides are available for expert calibration. When the interpolation run, each results of interpolation will be instantly available for batch export.

  ![Alternative Text](assets/3.png) 


* **Comprehensive Diagnostics**: Evaluate models through LOOCV. Generate advanced metrics including Nash-Sutcliffe Efficiency (NSE), CCC,  RPD, RPIQ, and Moran's I for spatial autocorrelation.

  ![Alternative Text](assets/4.png) 


* **Descriptive & Exploratory Suite**: Understand your dataset with simultaneous descriptive, correlation, and principal component analyses with results that can be instantly generated and observed by simultaneous categorization and data popularization of choice. An additional Governing Factors module computes variable importance and effects via Random Forest models with ALE, PDP, and SHAP analyses, decoupled as of v0.9.6 for advanced performance and modularity.

  ![Alternative Text](assets/5.png) 

* **Unified Export Registry**: Compile session assets into a centralized registry. Use the integrated WYSIWYG Styler to customize typography, DPI, and layout for publication-ready outputs (.PNG, .TIFF, .PDF) or batch-export everything with statistical tabular data merged into a Excel file.

  ![Alternative Text](assets/6.png) 


* **Dynamic UI & Theming**: Fully responsive interface with customizable themes and figures, accessible data details on maps/graphs for visual audits of hot-points.

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


# Installation & Setup Guide

## 1. System Prerequisites

Before installing the application, ensure you have the following software installed:

*   **R:** Version 4.5.0 or higher is highly recommended. You can download it from [CRAN](https://cran.r-project.org/).
*   **RStudio (Optional but recommended):** The easiest way to run and interact with Shiny applications. Download from [Posit](https://posit.co/download/rstudio-desktop/).
*   **System Dependencies for Spatial Packages:** 
    *   **Windows:** Generally, RTools handles most binary dependencies. Download [RTools](https://cran.r-project.org/bin/windows/Rtools/) matching your R version.
    *   **macOS:** You may need to install `gdal` and `proj` via Homebrew (`brew install gdal proj`).
    *   **Linux (Ubuntu/Debian):** Install spatial libraries using your package manager:
        ```bash
        sudo apt-get update
        sudo apt-get install libgdal-dev libproj-dev libgeos-dev libudunits2-dev
        ```

## 2. Package Dependencies

The Monolith dashboard relies on a comprehensive suite of R packages for its spatial engine, statistical analytics, and user interface. 

> [!IMPORTANT]
> **Automated Package Setup in v0.9.8a:**
> You **do not need** to execute any manual `install.packages(...)` console commands.
> Sourcing the dashboard or launching `monolith_ver_0.9.8a.R` automatically triggers a smart **Auto-Installation Hook** inside `global_0.9.8a.R`. This hook scans your environment, identifies any missing packages from the required suite, and downloads them non-interactively from the cloud CRAN repository.
> 
> *The dependencies managed automatically by the loader include: shiny, leaflet, sf, terra, tidyterra, gstat, fields, randomForest, DALEX, ggplot2, ggpubr, plotly, fresh, future, furrr, and DT.*

## 3. Application Structure

Ensure your project directory maintains the following structure. The application consists of a centralized package loader, a main runner file, and several helper scripts:

```
monolith/
│
├── global.R                          # Centralized package loader & environment configuration
├── monolith_ver_0.9.8a.R            # Main Application Runner (Sources global.R)
├── spatial_helpers_0.9.8a.R         # Spatial math, interpolation methods & CV logic
├── ui_helpers_0.9.8a.R              # Analytics, descriptive plots & fuzzy matching helpers
├── theme_helpers_0.9.8a.R           # Theming & export configurations
├── gov_module_0.9.8a.R              # Governing Factors UI & Server Modules
├── desc_exploratory_module_0.9.8a.R # Descriptive & Exploratory Suite (Tab 5 Module)
│
├── assets/                          # Screenshots & static assets
├── backup/                          # Backup copies of prior versions
├── docs/                            # Guides & documentation
├── sample_data/                     # Demo datasets (restricted license)
│
├── LICENSE                          # GPL-3.0 license
└── .gitignore
```

## 4. Running the Application

### Option A: Using RStudio (Recommended)
1. Open the `monolith_ver_0.9.8a.R` file in RStudio.
2. Ensure all required packages are installed and loaded without errors.
3. Click the **"Run App"** button located at the top right of the source editor.

### Option B: Using the R Console
1. Open your R console or terminal.
2. Set your working directory to the folder containing the app:
   ```R
   setwd("/path/to/your/monolith/directory")
   ```
3. Launch the Shiny app:
   ```R
   shiny::runApp("monolith_ver_0.9.8a.R")
   ```

## License

This project features a dual-licensing structure to protect both open-source contributions and private research datasets:

*   **Software & Source Code**: The core codebase of **Monolith** is licensed under the **GNU General Public License v3.0 (GPL-3.0)**. You are free to run, study, share, and modify the software, provided all derivative works remain open-source under the same terms. See the [LICENSE](LICENSE) file for the full legal text.
*   **Sample Datasets**: The files within the [sample_data/](file:///C:/Users/serda/Desktop/Data%20analysis%20tool%20creation/monolith/sample_data) directory are **not** open-source. They are provided solely for demonstrating and testing the application. You are strictly prohibited from publishing, redistributing, or using these datasets in any scientific publications or external projects. See the [sample_data/DATA_LICENSE](file:///C:/Users/serda/Desktop/Data%20analysis%20tool%20creation/monolith/sample_data/DATA_LICENSE) file for the full terms and restrictions.

[def]: assets/2.png
