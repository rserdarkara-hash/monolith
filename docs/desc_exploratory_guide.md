# Descriptive & Exploratory Suite

The **Descriptive and Exploratory Suite** provides statistical and visual tools for investigating your data, either before interpolation or after generating parameter predictions.

> *Scope of this document: the statistical methods in this suite (normality tests, group-comparison tests, correlation and partial correlation, PCA, random-forest explainability) are established published methods and are not original contributions of this software or its author. This guide describes how they are implemented here; the mathematics and the choices behind them are in the Scientific Guide. Works cited below are listed in the References section at the end of this document.*

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
*   On the categorical-variance plots (Boxplot, Violin, Sina), the test control is a single-select radio group: **None** (the default), ANOVA, Duncan's multiple range test (Duncan 1955), Tukey's HSD (Tukey 1949), and Kruskal-Wallis (Kruskal & Wallis 1952). Only one test is ever applied.
*   **Kruskal-Wallis** is the non-parametric route for data violating the normality assumptions, flagged by the normality indicator beside the control. Its pairwise post-hoc comparisons behind the significance letters are Benjamini-Hochberg adjusted (Benjamini & Hochberg 1995), consistent with the FDR policy used in the correlation table.
*   **Duncan's is labelled *(liberal)* for a reason.** It controls only the comparison-wise error rate, so with *k* groups its effective family-wise error rate grows toward 1 and it separates more means than Tukey's HSD on identical data. It is provided for reproducing older agronomy literature that reports it; **Tukey's HSD is the conservative default** for new work. See Scientific Guide Section 8.6.
*   Significance letters ('a', 'b', 'ab') are rendered directly on the plot geometries, so differences between groups can be read straight off the figure.

**2.3 Ghosting overlay**
*   A toggle that overlays the currently filtered sub-population on a faded "ghosted" background representing the entire dataset.

**2.4 Normality testing**
*   An integrated normality test assesses the distribution assumptions behind the parametric options.
*   **Test selection by sample size:** Shapiro-Wilk (Shapiro & Wilk 1965) below n = 5000, switching to the Lilliefors (Kolmogorov-Smirnov) test (Lilliefors 1967) at or above it, which is where `shapiro.test` stops accepting input. Below n = 3, or on a constant variable, no test is reported.
*   **Group-aware:** with a grouping variable active, each group is tested separately and the overall test runs on the group residuals, so between-group differences do not register as non-normality. Without grouping it runs on the raw values. The readout states which was used.

---

## 3. Tab 2: Correlation Analysis

Linear and monotonic relationships between the numeric variables in the dataset.

**3.1 Method selection**
*   `Pearson` (linear), `Spearman` (rank/monotonic), or `Kendall` (tau).

**3.2 Plot type selection**
*   **Hierarchical Heatmap:** clusters highly correlated variables together.
*   **Correlation Network:** a node-edge graph where edge thickness carries correlation strength.
*   **Partial Correlation:** correlations computed while controlling for other variables. The estimator follows the selected method (`ppcor` conventions, Kim 2015; Scientific Guide Section 8.3): **Pearson** residualizes the raw values on the controls, **Spearman** residualizes the *ranks* (a partial rank correlation is the product-moment partial correlation of ranks, not a rank correlation of raw-value residuals), and **Kendall** inverts the Kendall tau matrix. The p-values account for the controls: with *k* controls partialled out, significance is computed on *n − 2 − k* degrees of freedom (Pearson and Spearman t-statistic; Kendall uses a normal approximation with effective sample size *n − k*) rather than the naive *n − 2* of a plain correlation test. If a variable cannot be partialled out against the controls, the table aborts with an explicit error instead of silently reporting raw correlations.
*   **Correlogram:** a sorted matrix view of the same coefficients, sized and coloured by strength.
*   **Spatial Cross-Correlogram:** cross-correlation between two variables **as a function of the ground distance separating sample points**, the spatial analogue of a time-series cross-correlation. It requires the coordinate mapping to be confirmed on the Data Setup tab, and coordinates are projected to metres before binning, so the x-axis is a real distance and never a table row offset. Point size shows how many pairs fall in each distance bin, bins with fewer than 30 pairs are greyed as unreliable, and the dashed line marks the ordinary non-spatial correlation for reference. With `Spearman` or `Kendall` selected the curve is computed on ranks and labelled accordingly. Method and interpretation: Scientific Guide Section 8.4 (Journel & Huijbregts 1978; Goovaerts 1997).

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
*   **cos2 (quality of representation):** the share of a *variable's own* variance captured by the selected PCs, normalised by that variable's total variance across all components, so it always reads 0 to 1 whether or not the PCA was scaled (Abdi & Williams 2010; Lê et al. 2008; Scientific Guide Section 8.5). For an unscaled run the inline note points out what does still depend on scaling: the components themselves are driven by the high-variance variables.
*   **Mahalanobis distance (classical estimator; Mahalanobis 1936):** distances are computed on the PC scores using their theoretical diagonal covariance, the squared component standard deviations. Near-zero-variance components, which arise when PCA is force-executed on collinear variables, are excluded from the distance and from the chi-square threshold's degrees of freedom, so the outlier plot stays available instead of failing on a singular covariance matrix. The centre and scatter are the **classical** mean and covariance, which the suspected outliers themselves contribute to, so a cluster of extreme observations can inflate the covariance and hide inside the threshold (*masking*); the high-breakdown alternative is the minimum covariance determinant estimator (Rousseeuw & Van Driessen 1999), which the panel deliberately does not use. Read the panel as a screening aid, not a decision rule. See Scientific Guide Section 8.2.

---

## 5. Tab 4: Governing Factors

Machine-learning explainability applied to non-linear relationships and feature interactions.

**5.1 Configuration**
*   **Target:** the primary soil parameter to explain.
*   **Predictors:** the environmental or secondary variables acting as potential influences.
*   **Permutations:** controls the robustness of the random-forest permutation variable-importance calculation (Breiman 2001; Fisher et al. 2019) (10 to 100, default 50).
*   **Number of trees (ntree):** the size of the underlying random forest (`randomForest`; Liaw & Wiener 2002) (50 to 500, default 100). Higher values stabilise the permutation results but take longer.
*   **SHAP sample size (max):** SHAP explanations are computationally intensive, so this caps the random subsample (50 to 1000, default 100). Lower values run faster for quick exploration; higher values represent the dataset better.

**5.1.1 Cancelling a run**

A **Cancel Run** button sits in the running panel and takes effect at the next checkpoint rather than instantly. Checkpoints sit before the random-forest fit, before the permutation-importance pass, before the ALE/PDP profiles, and between individual SHAP observations. The SHAP loop is normally the longest stage and is checked per observation, so a cancel there is usually picked up within a second or two. The exception is permutation importance, which runs all of its passes inside a single uninterruptible call and must finish before the cancel is seen; lowering **Permutations** shortens that window. A cancelled run keeps no partial results: the previous run's plots stay on screen and the Run Analysis button becomes available again.

**5.2 Functional effect plots**

Two explainability frameworks are available:
*   **ALE (Accumulated Local Effects; Apley & Zhu 2020):** a faster alternative to partial dependence, unbiased under correlated predictors, that maps the main effect of a predictor on the target.
*   **PDP (Partial Dependence Plot; Friedman 2001):** the marginal effect of a feature on the predicted outcome.

**5.3 SHAP dependence plot**
*   Each point is the per-observation SHAP attribution (Lundberg & Lee 2017) of the most important predictor: how much that predictor shifts the model's prediction for that sample away from the dataset-mean prediction, in the target variable's own units. Across all predictors the values sum to the deviation of the sample's prediction from the mean.

**5.4 Tabular data metrics**
*   The metrics table lists the permutation importance (dropout loss) of each governing factor and leads with an **RF model quality row, out-of-bag (OOB) % variance explained**, so the reliability of the random forest behind the importance and SHAP results can be judged directly. Low OOB values mean the explainability outputs describe a weak model and should be interpreted cautiously.

---

## 6. Expandable Plot Engine

Across the Descriptive, Correlation and PCA tabs, the main plot area carries an **Expand / Interactive** button in its top right corner.

*   Clicking it opens a full-screen modal.
*   The modal toggles between a **Static (High-Res)** view for clean screenshots and an **Interactive (Hover/Zoom)** mode.
*   Interactive mode converts the `ggplot2` object into a `plotly` object, adding pan, zoom and point-specific hover readouts in the browser.

---

## 7. References

Works cited in this guide. The Scientific Guide's Section 11 carries the full reference list for the mathematics behind these methods.

Abdi, H., & Williams, L. J. (2010). Principal component analysis. *WIREs Computational Statistics*, 2(4), 433-459. https://doi.org/10.1002/wics.101

Apley, D. W., & Zhu, J. (2020). Visualizing the effects of predictor variables in black box supervised learning models. *Journal of the Royal Statistical Society, Series B*, 82(4), 1059-1086. https://doi.org/10.1111/rssb.12377

Benjamini, Y., & Hochberg, Y. (1995). Controlling the false discovery rate: a practical and powerful approach to multiple testing. *Journal of the Royal Statistical Society, Series B*, 57(1), 289-300. https://doi.org/10.1111/j.2517-6161.1995.tb02031.x

Breiman, L. (2001). Random forests. *Machine Learning*, 45(1), 5-32. https://doi.org/10.1023/A:1010933404324

Duncan, D. B. (1955). Multiple range and multiple F tests. *Biometrics*, 11(1), 1-42. https://doi.org/10.2307/3001478

Fisher, A., Rudin, C., & Dominici, F. (2019). All models are wrong, but many are useful: learning a variable's importance by studying an entire class of prediction models simultaneously. *Journal of Machine Learning Research*, 20(177), 1-81.

Friedman, J. H. (2001). Greedy function approximation: a gradient boosting machine. *The Annals of Statistics*, 29(5), 1189-1232. https://doi.org/10.1214/aos/1013203451

Goovaerts, P. (1997). *Geostatistics for Natural Resources Evaluation*. Oxford University Press, New York. https://doi.org/10.1093/oso/9780195115383.001.0001

Journel, A. G., & Huijbregts, C. J. (1978). *Mining Geostatistics*. Academic Press, London. ISBN 978-0-12-391050-1.

Kim, S. (2015). ppcor: an R package for a fast calculation to semi-partial correlation coefficients. *Communications for Statistical Applications and Methods*, 22(6), 665-674. https://doi.org/10.5351/CSAM.2015.22.6.665

Kruskal, W. H., & Wallis, W. A. (1952). Use of ranks in one-criterion variance analysis. *Journal of the American Statistical Association*, 47(260), 583-621. https://doi.org/10.1080/01621459.1952.10483441

Lê, S., Josse, J., & Husson, F. (2008). FactoMineR: an R package for multivariate analysis. *Journal of Statistical Software*, 25(1), 1-18. https://doi.org/10.18637/jss.v025.i01

Liaw, A., & Wiener, M. (2002). Classification and regression by randomForest. *R News*, 2(3), 18-22.

Lilliefors, H. W. (1967). On the Kolmogorov-Smirnov test for normality with mean and variance unknown. *Journal of the American Statistical Association*, 62(318), 399-402. https://doi.org/10.1080/01621459.1967.10482916

Lundberg, S. M., & Lee, S.-I. (2017). A unified approach to interpreting model predictions. *Advances in Neural Information Processing Systems*, 30, 4765-4774.

Mahalanobis, P. C. (1936). On the generalised distance in statistics. *Proceedings of the National Institute of Sciences of India*, 2(1), 49-55. (Reprinted in *Sankhya A*, 80(S1), 1-7. https://doi.org/10.1007/s13171-019-00164-5)

Rousseeuw, P. J., & Van Driessen, K. (1999). A fast algorithm for the minimum covariance determinant estimator. *Technometrics*, 41(3), 212-223. https://doi.org/10.1080/00401706.1999.10485670

Shapiro, S. S., & Wilk, M. B. (1965). An analysis of variance test for normality (complete samples). *Biometrika*, 52(3/4), 591-611. https://doi.org/10.2307/2333709

Tukey, J. W. (1949). Comparing individual means in the analysis of variance. *Biometrics*, 5(2), 99-114. https://doi.org/10.2307/3001913
