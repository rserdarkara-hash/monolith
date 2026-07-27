# Descriptive & Exploratory Suite

The **Descriptive and Exploratory Suite**  provides a comprehensive set of statistical and visual tools to investigate your data either before interpolation or after generating parameter predictions.

> *Note: The statistical and geostatistical methods described in this document (e.g., kriging variants, IDW, TPS, cross-validation metrics, PCA, statistical tests) are established methods previously published in the literature; they are not original scientific contributions of this software or its author. This document explains how these methods are implemented within Monolith for practical and operational clarity, and is not designed to serve as a citable source for their theoretical origins. Users who wish to cite the original literature source for a given method should locate and review the relevant sources themselves, as this document does not aim to provide academic citations for each method.*

## 0. Variable Naming

A **Variable naming** radio at the top of the Analytics Engine switches every dropdown, plot axis, title, and table in the suite (including the Governing Factors tab) between the human-readable **variable labels** defined in your metadata mapping (default) and the raw **column names** of the uploaded file. When no metadata mapping was provided, both options show the column names. The toggle is purely cosmetic and never changes a computed value.

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
*   **Multivariate/Spatial:** Scatterplot, 2D Density Heatmap, Parallel Coordinates, Radar Chart, XYZ Surface.

**2.2 Significance Testing (ANOVA, Kruskal-Wallis & Post-Hoc)**
*   When utilizing categorical variance plots (Boxplot, Violin, Sina), the UI natively integrates ANOVA testing for normally distributed data.
*   **Non-Parametric Testing:** For data that violates normality assumptions, the UI includes a Kruskal-Wallis test option, complete with a contextual tooltip recommendation.
*   Users can select post-hoc methods (Duncan's or Tukey's HSD). For the Kruskal-Wallis option, the pairwise post-hoc comparisons behind the significance letters are Benjamini-Hochberg (BH) adjusted, consistent with the FDR policy used in the correlation table.
*   **UX Interaction:** The resulting statistical significance letters (e.g., 'a', 'b', 'ab') are dynamically rendered directly atop the individual plot geometries, allowing for immediate visual interpretation of statistical differences between soil or field groups.

**2.3 Ghosting Overlay**
*   **Functionality:** A toggleable feature that overlays the currently selected local sub-population (filtered group) over a faded, "ghosted" background representing the entire global dataset.

**2.4 Normality Testing**
*   The UI features an integrated normality test to assess distribution assumptions.
*   **Dynamic Methodology:** If the sample size is $n < 50$, the system automatically utilizes the Shapiro-Wilk test. For $n \ge 50$, it switches to the Lilliefors test (`nortest::lillie.test`).
*   **Group-Aware:** If a grouping variable is active (i.e. >1 group), the normality test is run on the group residuals to account for inter-group differences. Otherwise, it runs directly on the raw values.

---

## 3. Tab 2: Correlation Analysis

This module evaluates the linear and monotonic relationships between all numeric variables in the dataset.

**3.1 Method Selection**
*   Choose between `Pearson` (linear), `Spearman` (rank/monotonic), or `Kendall` (tau).

**3.2 Plot Type Selection**
*   **Hierarchical Heatmap:** Automatically clusters highly correlated variables together.
*   **Correlation Network:** Visualizes relationships as a node-edge graph, where edge thickness dictates correlation strength.
*   **Partial Correlation:** Allows users to calculate correlations while mathematically controlling for the effect of a third variable. The estimator follows the selected method (`ppcor` conventions, see Scientific Guide Section 8.3): **Pearson** residualizes the raw values on the controls, **Spearman** residualizes the *ranks* (a partial rank correlation is the product-moment partial correlation of ranks, not a rank correlation of raw-value residuals), and **Kendall** inverts the Kendall tau matrix. The accompanying p-values account for the controls: with *k* control variables partialled out, significance is computed on *n − 2 − k* degrees of freedom (Pearson/Spearman t-statistic; Kendall uses a normal approximation with effective sample size *n − k*), rather than the naive *n − 2* of a plain correlation test. If any variable cannot be partialled out against the controls, the table aborts with an explicit error instead of silently reporting raw correlations.
*   **Correlogram:** Sorted matrix view of the same coefficients, sized and coloured by strength.
*   **Spatial Cross-Correlogram:** Cross-correlation between two variables **as a function of the ground distance separating sample points** — the spatial analogue of a time-series cross-correlation. Requires the coordinate mapping to be confirmed on the Data Setup tab; coordinates are projected to metres before binning, so the x-axis is a real distance, never a table row offset. Point size shows how many pairs fall in each distance bin and bins with fewer than 30 pairs are greyed as unreliable; the dashed line marks the ordinary (non-spatial) correlation for reference. With `Spearman` or `Kendall` selected the curve is computed on ranks and labelled accordingly. Method and interpretation: Scientific Guide Section 8.4.

**3.3 Data Table**
*   A reactive data table (`DT::dataTableOutput`) below the plot provides the exact numerical correlation matrix for rigorous inspection and export. Pairwise tables report both the raw p-value and a Benjamini-Hochberg adjusted column computed across all pairs shown.

---

## 4. Tab 3: Principal Component Analysis (PCA)

A dedicated high-dimensional dimensionality reduction module.

**4.1 Automated Collinearity Filter (Critical UX Guardrail)**
*   Before PCA executes, the system scans the selected variables. If near-perfect collinearity is detected ($r > 0.95$), a prominent warning UI (`pca_collinearity_warning_ui`) intercepts the process.
*   The same scan also flags variables that are **constant** over the current selection (zero variance). These are listed as *Constant (no variance)* rather than *High VIF*, because a constant is not a collinearity problem: it carries no information at all and would make the correlation matrix singular on its own. Earlier versions reported both cases under the VIF label, sending the reader to look for a correlation that did not exist.
*   It lists the exact conflicting pairs and prevents execution, offering an "Ignore Warning & Force PCA" red button for advanced users. This prevents the generation of heavily distorted loading vectors.

**4.2 Plot Settings**
*   **Types:** Scree Plot, Biplot (2D), Biplot (3D), Loadings, Contribution, Cumulative Variance, and Mahalanobis Distance.
*   **Controls:** Dynamic numeric inputs appear based on the plot type to select specific Principal Components (e.g., X-Axis PC 1, Y-Axis PC 2) or assess specific loading contributions.
*   **Scaling Caveat (Contribution & cos2):** Both diagnostics implicitly assume the PCA was run on scaled, unit-variance inputs. If "Scale & Center Data" was unchecked when the PCA was executed, the values are dominated by the high-variance variables and are not comparable across variables measured on different scales; the module displays an inline note under the plot controls in that case.
*   **Mahalanobis Distance (Classical Estimator):** Distances are computed on the PC scores using their theoretical diagonal covariance (the squared component standard deviations). Near-zero-variance components, which arise when PCA is force-executed on collinear variables, are excluded from the distance (and from the chi-square threshold's degrees of freedom), so the outlier plot remains available instead of failing on a singular covariance matrix. The centre and scatter are the **classical** mean and covariance, which the suspected outliers themselves contribute to, so a cluster of extreme observations can inflate the covariance and hide inside the threshold (*masking*). Read the panel as a screening aid, not a decision rule — see Scientific Guide Section 8.2.

---

## 5. Tab 4: Governing Factors

This module leverages machine learning explainability to discover non-linear relationships and feature interactions.

**5.1 Configuration**
*   **Target:** Select the primary soil parameter you wish to explain.
*   **Predictors:** Select the environmental or secondary variables acting as potential influences.
*   **Permutations:** A slider controls the robustness of the Random Forest variable importance calculation (default: 50).
*   **Number of Trees (ntree):** Controls the depth and complexity of the underlying Random Forest model. Higher values (e.g., 500) ensure extreme stability in permutations but take longer.
*   **SHAP Sample Size (Max):** Since calculating SHAP explanations is computationally intense, this slider dictates the maximum random subsample size. Lower values execute faster for quick exploration; higher values provide more robust, comprehensive representations of the dataset.

**5.1.1 Cancelling a run**
A **Cancel Run** button sits in the running panel. Like the Classification Suite's, it takes effect at the next checkpoint rather than instantly: checkpoints sit before the Random Forest fit, before the permutation-importance pass, before the ALE/PDP profiles, and between individual SHAP observations. Because the SHAP loop is normally the longest stage and is checked per observation, a cancel there is usually picked up within a second or two. The exception is the permutation-importance step, which runs all of its passes inside a single uninterruptible call and must finish before the cancel is seen — lowering **Permutations** shortens that window. A cancelled run keeps no partial results; the previous run's plots (if any) stay on screen and the Run Analysis button becomes available again.

**5.2 Functional Effect Plots**
Users can toggle between two advanced explainability frameworks:
*   **ALE (Accumulated Local Effects):** A faster, unbiased alternative to Partial Dependence Plots that maps the main effect of a predictor on the target variable.
*   **PDP (Partial Dependence Plot):** Shows the marginal effect of a feature on the predicted outcome.

**5.3 SHAP Dependence Plot**
*   Each point is the per-observation SHAP attribution of the most important predictor: how much that predictor shifts the model's prediction for that sample away from the dataset-mean prediction, in the target variable's own units. Values therefore sum (across all predictors) to the deviation of the sample's prediction from the mean.

**5.4 Tabular Data Metrics**
*   The metrics table lists the permutation importance (dropout loss) of each governing factor and leads with an **RF model quality row, out-of-bag (OOB) % variance explained**, so the reliability of the Random Forest behind the importance and SHAP results can be judged directly. Low OOB values mean the explainability outputs describe a weak model and should be interpreted cautiously.

---

## 6. Expandable Plot Engine

Across *most* tabs in the Analytics Engine (Descriptive, Correlation, PCA), the main plot area features an **"Expand / Interactive"** button in the top right corner.

*   **Interaction:** Clicking this button opens a large, full-screen modal.
*   **Modality:** The user can toggle between a "Static (High-Res)" view for clean screenshots or an "Interactive (Hover/Zoom)" mode.
*   **Interactive Engine:** The interactive mode converts the standard `ggplot2` object into a `plotly` object, granting the user pan, zoom, and deep point-specific hover capabilities natively within the browser.
