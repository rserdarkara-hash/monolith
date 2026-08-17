# Descriptive & Exploratory Suite

The **Descriptive and Exploratory Suite** provides statistical and visual tools for investigating your data, either before interpolation or after generating parameter predictions.

> *Scope of this document: the statistical methods in this suite (normality tests, group-comparison tests, correlation and partial correlation, PCA, random-forest explainability) are established published methods and are not original contributions of this software or its author. This guide describes how they are implemented here. The mathematics and the choices behind them are in the Scientific Guide, whose Section 11 lists the works cited; for the theoretical origins of a method, consult the primary literature directly.*

## 0. Variable Naming

A **Variable naming** radio at the top of the Analytics Engine switches every dropdown, plot axis, title and table in the suite, the Governing Factors tab included, between the human-readable **variable labels** from your metadata mapping (default) and the raw **column names** of the uploaded file. Without a metadata mapping both options show the column names. The toggle is cosmetic and never changes a computed value.

## 1. Global Data Grouping & Discretization

A master control panel at the top of the Analytics Engine dictates the data subset fed into every analysis tab (Descriptive, Correlation, PCA, Governing Factors). Filters and groupings persist across the session, and results update immediately when a grouping changes.

*   **Grouping variables (max 5):** select up to five categorical or numerical variables as grouping factors.
*   **Auto-discretization:** a continuous variable selected as a grouping factor (elevation, pH) is binned automatically into ordered categories (Low, Medium, High) so it can drive boxplot groups or correlation-network nodes.
*   **Active group filter:** once groups are defined, the filter dropdown isolates specific sub-populations.

---

## 2. Tab 1: Descriptive Suite

Univariate and bivariate distributions, and how they vary across the groups defined in the global panel.

**2.1 Plot type selection**

A dropdown switches between thirteen visualization modes:
*   **Distribution:** Histogram, Density, ECDF, QQ Plot, Ridge/Joyplot.
*   **Categorical variance:** Boxplot, Violin, Sina-style Plot.
*   **Multivariate and spatial:** Scatterplot, 2D Density Heatmap, Parallel Coordinates, Radar Chart, XYZ Surface.

**2.2 Significance testing (ANOVA, Kruskal-Wallis and post-hoc)**
*   On the categorical-variance plots (Boxplot, Violin, Sina), the test control is a single-select radio group: **None** (the default), ANOVA, Duncan's, Tukey's HSD, and Kruskal-Wallis. Only one test is ever applied.
*   **Kruskal-Wallis** is the non-parametric route for data violating the normality assumptions, flagged by the normality indicator beside the control. Its pairwise post-hoc comparisons behind the significance letters are Benjamini-Hochberg adjusted, consistent with the FDR policy used in the correlation table.
*   **Duncan's is labelled *(liberal)* for a reason.** It controls only the comparison-wise error rate, so with *k* groups its effective family-wise error rate grows toward 1 and it separates more means than Tukey's HSD on identical data. It is provided for reproducing older agronomy literature that reports it; **Tukey's HSD is the conservative default** for new work. See Scientific Guide Section 8.6.
*   Significance letters ('a', 'b', 'ab') are rendered directly on the plot geometries, so differences between groups can be read straight off the figure.

**2.3 Ghosting overlay**
*   A toggle that overlays the currently filtered sub-population on a faded "ghosted" background representing the entire dataset.

**2.4 Normality testing**
*   An integrated normality test assesses the distribution assumptions behind the parametric options.
*   **Test selection by sample size:** Shapiro-Wilk below n = 5000, switching to the Lilliefors (Kolmogorov-Smirnov) test at or above it, which is where `shapiro.test` stops accepting input. Below n = 3, or on a constant variable, no test is reported.
*   **Group-aware:** with a grouping variable active, each group is tested separately and the overall test runs on the group residuals, so between-group differences do not register as non-normality. Without grouping it runs on the raw values. The readout states which was used.

---

## 3. Tab 2: Correlation Analysis

Linear and monotonic relationships between the numeric variables in the dataset.

**3.1 Method selection**
*   `Pearson` (linear), `Spearman` (rank/monotonic), or `Kendall` (tau).

**3.2 Plot type selection**
*   **Hierarchical Heatmap:** clusters highly correlated variables together.
*   **Correlation Network:** a node-edge graph where edge thickness carries correlation strength.
*   **Partial Correlation:** correlations computed while controlling for other variables. The estimator follows the selected method (`ppcor` conventions, Scientific Guide Section 8.3): **Pearson** residualizes the raw values on the controls, **Spearman** residualizes the *ranks* (a partial rank correlation is the product-moment partial correlation of ranks, not a rank correlation of raw-value residuals), and **Kendall** inverts the Kendall tau matrix. The p-values account for the controls: with *k* controls partialled out, significance is computed on *n − 2 − k* degrees of freedom (Pearson and Spearman t-statistic; Kendall uses a normal approximation with effective sample size *n − k*) rather than the naive *n − 2* of a plain correlation test. If a variable cannot be partialled out against the controls, the table aborts with an explicit error instead of silently reporting raw correlations.
*   **Correlogram:** a sorted matrix view of the same coefficients, sized and coloured by strength.
*   **Spatial Cross-Correlogram:** cross-correlation between two variables **as a function of the ground distance separating sample points**, the spatial analogue of a time-series cross-correlation. It requires the coordinate mapping to be confirmed on the Data Setup tab, and coordinates are projected to metres before binning, so the x-axis is a real distance and never a table row offset. Point size shows how many pairs fall in each distance bin, bins with fewer than 30 pairs are greyed as unreliable, and the dashed line marks the ordinary non-spatial correlation for reference. With `Spearman` or `Kendall` selected the curve is computed on ranks and labelled accordingly. Method and interpretation: Scientific Guide Section 8.4.

**3.3 Sample used**
*   The four matrix panels (heatmap, network, partial, correlogram) are one matrix, estimated on the rows with no missing value in **any** selected variable, control variables included. The figure and the table both state that sample: *"Complete cases: n = 87 of 132 rows (45 dropped for missing values)."* A large drop is a fact about the dataset, not a display artifact: narrow the variable selection, or impute deliberately before uploading. Why complete cases rather than pairwise: Scientific Guide Section 8.7.
*   The Spatial Cross-Correlogram works on the two chosen variables and the coordinates, so it uses the samples complete across those four columns.

**3.4 Data table**
*   A data table below the plot gives the exact numerical correlation matrix for inspection and export. Pairwise tables report both the raw p-value and a Benjamini-Hochberg adjusted column computed across all pairs shown.

---

## 4. Tab 3: Principal Component Analysis (PCA)

**4.1 Automated collinearity filter**
*   Before PCA executes, the selected variables are scanned. Near-perfect collinearity ($r > 0.95$) raises a warning panel that intercepts the process, lists the exact conflicting pairs, and prevents execution. An "Ignore Warning & Force PCA" button is available for advanced users. The guard exists because collinear inputs distort the loading vectors severely.
*   The same scan flags variables that are **constant** over the current selection (zero variance). These are listed as *Constant (no variance)* rather than *High VIF*, because a constant is not a collinearity problem: it carries no information at all and would make the correlation matrix singular on its own.

**4.2 Plot settings**
*   **Types:** Scree Plot, Biplot (2D), Biplot (3D), Loadings, Contribution, Cumulative Variance, and Mahalanobis Distance.
*   **Controls:** numeric inputs appear according to the plot type, to select specific principal components (X-axis PC 1, Y-axis PC 2) or assess specific loading contributions.
*   **Scaling caveat (Contribution):** contribution is the share of a *component* attributable to a variable, so it is scale-sensitive by definition. With "Scale & Center Data" unchecked, the values are dominated by the high-variance variables and are not comparable across variables measured on different scales; the module shows an inline note under the plot controls in that case.
*   **cos2 (quality of representation):** the share of a *variable's own* variance captured by the selected PCs, normalised by that variable's total variance across all components, so it always reads 0 to 1 whether or not the PCA was scaled (Scientific Guide Section 8.5). For an unscaled run the inline note points out what does still depend on scaling: the components themselves are driven by the high-variance variables.
*   **Mahalanobis distance (classical estimator):** distances are computed on the PC scores using their theoretical diagonal covariance, the squared component standard deviations. Near-zero-variance components, which arise when PCA is force-executed on collinear variables, are excluded from the distance and from the chi-square threshold's degrees of freedom, so the outlier plot stays available instead of failing on a singular covariance matrix. The centre and scatter are the **classical** mean and covariance, which the suspected outliers themselves contribute to, so a cluster of extreme observations can inflate the covariance and hide inside the threshold (*masking*). Read the panel as a screening aid, not a decision rule. See Scientific Guide Section 8.2.

---

## 5. Tab 4: Governing Factors

Machine-learning explainability applied to non-linear relationships and feature interactions.

**5.1 Configuration**
*   **Target:** the primary soil parameter to explain.
*   **Predictors:** the environmental or secondary variables acting as potential influences.
*   **Permutations:** controls the robustness of the random-forest variable importance calculation (10 to 100, default 50).
*   **Number of trees (ntree):** the size of the underlying random forest (50 to 500, default 100). Higher values stabilise the permutation results but take longer.
*   **SHAP sample size (max):** SHAP explanations are computationally intensive, so this caps the random subsample (50 to 1000, default 100). Lower values run faster for quick exploration; higher values represent the dataset better.

**5.1.1 Cancelling a run**

A **Cancel Run** button sits in the running panel and takes effect at the next checkpoint rather than instantly. Checkpoints sit before the random-forest fit, before the permutation-importance pass, before the ALE/PDP profiles, and between individual SHAP observations. The SHAP loop is normally the longest stage and is checked per observation, so a cancel there is usually picked up within a second or two. The exception is permutation importance, which runs all of its passes inside a single uninterruptible call and must finish before the cancel is seen; lowering **Permutations** shortens that window. A cancelled run keeps no partial results: the previous run's plots stay on screen and the Run Analysis button becomes available again.

**5.2 Functional effect plots**

Two explainability frameworks are available:
*   **ALE (Accumulated Local Effects):** a faster, unbiased alternative to partial dependence that maps the main effect of a predictor on the target.
*   **PDP (Partial Dependence Plot):** the marginal effect of a feature on the predicted outcome.

**5.3 SHAP dependence plot**
*   Each point is the per-observation SHAP attribution of the most important predictor: how much that predictor shifts the model's prediction for that sample away from the dataset-mean prediction, in the target variable's own units. Across all predictors the values sum to the deviation of the sample's prediction from the mean.

**5.4 Tabular data metrics**
*   The metrics table lists the permutation importance (dropout loss) of each governing factor and leads with an **RF model quality row, out-of-bag (OOB) % variance explained**, so the reliability of the random forest behind the importance and SHAP results can be judged directly. Low OOB values mean the explainability outputs describe a weak model and should be interpreted cautiously.

---

## 6. Expandable Plot Engine

Across the Descriptive, Correlation and PCA tabs, the main plot area carries an **Expand / Interactive** button in its top right corner.

*   Clicking it opens a full-screen modal.
*   The modal toggles between a **Static (High-Res)** view for clean screenshots and an **Interactive (Hover/Zoom)** mode.
*   Interactive mode converts the `ggplot2` object into a `plotly` object, adding pan, zoom and point-specific hover readouts in the browser.
