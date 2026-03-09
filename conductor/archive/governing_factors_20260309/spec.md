# Specification: Governing Factors Analysis Tab

## 1. Overview
This track introduces a new "Governing Factors" analysis tab to the existing Descriptive and Exploratory Suite. Its primary goal is to provide multi-scale influence and causality diagnostics for target agronomical parameters against potential governing factors (predictors) using machine learning explainability techniques.

## 2. Functional Requirements
- **Backup Protocol:** As a mandatory first step, the current app and helper files (e.g., `monolith_0.8.4.R`) must be backed up to the `backups/` folder. The app must never be overwritten directly; instead, a new version (e.g., `monolith_0.8.5.R`) is created.
- **New Tab Creation:** Integrate a 4th tab named "Governing Factors" within the Scientific Analytics Engine module.
- **UI Layout - Sidebar (Analysis Configuration):**
    - `selectInput`: Target Parameter (e.g., pH, TN).
    - `multiInput` (or `pickerInput`): Potential Governing Factors (Predictors).
    - `sliderInput`: Number of Permutations (for RF importance).
    - `actionButton`: An explicit "Run Analysis" or "Update Plots" button to trigger the heavy computational rendering, preventing UI lag.
- **UI Layout - Main Panel (Multi-scale Influence & Causality Diagnostics):**
    - **4 Quads:**
        - **Top Left:** Global Importance (Bar Chart) derived from the model.
        - **Top Right:** Replaced with Causality/Interaction Plots (e.g., Feature Interaction Networks or 2D Partial Dependence) instead of a standard Correlation Heatmap.
        - **Bottom Left:** Functional Effect Plot. Must include a small toggle switch to alternate between ALE (Accumulated Local Effects) plots and SHAP (SHapley Additive exPlanations) values for the top identified factor.
        - **Bottom Right:** Replaced with Causality/Interaction Plots instead of a standard PCA Biplot.
    - **Tabular Data View:** Add a tabular data representation of the calculated importance metrics, SHAP values, and ALE statistics below the quadrant plot section, similar to the architecture of the other analysis tabs.
- **Computational Engine:**
    - Must utilize native R packages (e.g., `DALEX`, `iml`, or `fastshap`) integrated with the existing `tidymodels` workflow to calculate SHAP values and ALE/PDP plots.
    - All plots must be calculated and presented in a scientifically valid and reputable manner.
- **Grouping and Discretization Engine Integration:**
    - The entire fourth section must respect and be subjected to the existing grouping and discretization engine, consistent with the other three tabs in the suite.

## 3. Non-Functional Requirements
- **Performance:** Complex computations (permutations, SHAP) must be deferred until the user explicitly clicks the "Run Analysis" button to ensure the UI remains responsive.
- **Consistency:** The new tab must adhere to the existing `fresh` dynamic theme engine, semantic mapping, and rigorous export styler configurations.

## 4. Acceptance Criteria
- A user can navigate to the "Governing Factors" tab, select a Target Parameter and Governing Factors, define the number of permutations, and click "Run Analysis".
- The 4 quadrants successfully render the Global Importance Bar Chart, Causality/Interaction plots, and the Functional Effect plot.
- The user can toggle between ALE and SHAP plots.
- The analysis dynamically updates based on the global grouping/discretization engine selections.
- A functional data table displays the statistical metrics beneath the visual plots.
- The original app code is safely backed up and a new version is created.

## 5. Out of Scope
- Python integration via `reticulate` for SHAP/PDP calculations (we are strictly using Native R packages).
- Standard Correlation Heatmaps and PCA Biplots within this specific tab.