# Descriptive & Exploratory Suite: UI/UX Guide

The **Descriptive and Exploratory Suite**  provides a comprehensive set of statistical and visual tools to investigate your data either before interpolation or after generating parameter predictions.



## 1. Global Data Grouping & Discretization

At the top of the Analytics Engine, a master control panel dictates the data subset fed into all subsequent analysis tabs (Descriptive, Correlation, PCA, Governing Factors). Any filter or grouping applied persists across your entire analytical session through this tabs, the results will instantly updated if grouping modified.

*   **Grouping Variables (Max 5):** You can select up to 5 categorical or numerical variables to act as grouping factors.
*   **Auto-Discretization:** If a continuous numerical variable (like Elevation or pH) is selected as a grouping factor, the UI automatically applies discretization logic to bin it into logical categories (e.g., Low, Medium, High) so it can be used for grouping plots (like Boxplots) or Correlation network nodes.
*   **Active Group Filter:** Once groups are defined, you can isolate specific sub-populations using the filter dropdown.

---

## 2. Tab 1: Descriptive Suite

This tab focuses on univariate and bivariate distributions, how data varies across the groups defined in the global panel.

**2.1 Plot Type Selection**
A central dropdown allows you to switch between over a dozen high-fidelity visualization modes:
*   **Distribution:** Histogram, Density, ECDF, QQ Plot, Ridge/Joyplot.
*   **Categorical Variance:** Boxplot, Violin, Sina-style Plot.
*   **Multivariate/Spatial:** Scatterplot, 2D Density Heatmap, Parallel Coordinates, Radar/Spider Chart, Multi-fit XYZ Surface.

**2.2 Significance Testing (ANOVA & Post-Hoc)**
*   When utilizing categorical variance plots (Boxplot, Violin, Sina), the UI natively integrates ANOVA testing.
*   Users can select post-hoc methods (Duncan's or Tukey's HSD).
*   **UX Interaction:** The resulting statistical significance letters (e.g., 'a', 'b', 'ab') are dynamically rendered directly atop the individual plot geometries, allowing for immediate visual interpretation of statistical differences between soil or field groups.

**2.3 Ghosting Overlay**
*   **Functionality:** A toggleable feature that overlays the currently selected local sub-population (filtered group) over a faded, "ghosted" background representing the entire global dataset.
---

## 3. Tab 2: Correlation Analysis

This module evaluates the linear and monotonic relationships between all numeric variables in the dataset.

**3.1 Method Selection**
*   Choose between `Pearson` (linear), `Spearman` (rank/monotonic), or `Kendall` (tau).

**3.2 Plot Type Selection**
*   **Hierarchical Heatmap:** Automatically clusters highly correlated variables together.
*   **Correlation Network:** Visualizes relationships as a node-edge graph, where edge thickness dictates correlation strength.
*   **Partial Correlation:** Allows users to calculate correlations while mathematically controlling for the effect of a third variable (via regression residual extraction).
*   **Correlogram & Lagged CCF:** For spatial or sequential lag analysis.

**3.3 Data Table**
*   A reactive data table (`DT::dataTableOutput`) below the plot provides the exact numerical correlation matrix for rigorous inspection and export.

---

## 4. Tab 3: Principal Component Analysis (PCA)

A dedicated high-dimensional dimensionality reduction module.

**4.1 Automated Collinearity Filter (Critical UX Guardrail)**
*   Before PCA executes, the system scans the selected variables. If near-perfect collinearity is detected ($r > 0.95$), a prominent warning UI (`pca_collinearity_warning_ui`) intercepts the process.
*   It lists the exact conflicting pairs and prevents execution, offering an "Ignore Warning & Force PCA" red button for advanced users. This prevents the generation of heavily distorted loading vectors.

**4.2 Plot Settings**
*   **Types:** Scree Plot, Biplot (2D), Loadings, Contribution, Cumulative Variance, and Mahalanobis Distance.
*   **Controls:** Dynamic numeric inputs appear based on the plot type to select specific Principal Components (e.g., X-Axis PC 1, Y-Axis PC 2) or assess specific loading contributions.

---

## 5. Tab 4: Governing Factors

This module leverages machine learning explainability to discover non-linear relationships and feature interactions.

**5.1 Configuration**
*   **Target:** Select the primary soil parameter you wish to explain.
*   **Predictors:** Select the environmental or secondary variables acting as potential influences.
*   **Permutations:** A slider controls the robustness of the Random Forest variable importance calculation (default: 50).

**5.2 Functional Effect Plots**
Users can toggle between two advanced explainability frameworks:
*   **SHAP (SHapley Additive exPlanations):** Shows the marginal contribution of each feature across the dataset.
*   **ALE (Accumulated Local Effects):** A faster, unbiased alternative to Partial Dependence Plots that maps the main effect of a predictor on the target variable.

---

## 6. Expandable Plot Engine (Global Feature)

Across *most* tabs in the Analytics Engine (Descriptive, Correlation, PCA), the main plot area features an **"Expand / Interactive"** button in the top right corner.

*   **Interaction:** Clicking this button opens a large, full-screen modal.
*   **Modality:** The user can toggle between a "Static (High-Res)" view for clean screenshots or an "Interactive (Hover/Zoom)" mode.
*   **Interactive Engine:** The interactive mode converts the standard `ggplot2` object into a `plotly` object, granting the user pan, zoom, and deep point-specific hover capabilities natively within the browser.