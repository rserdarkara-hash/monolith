# Descriptive & Exploratory Suite

The **Descriptive and Exploratory Suite** provides statistical and visual tools for investigating your data, either before interpolation or after generating parameter predictions.

> *Scope of this document: the statistical methods in this suite (normality tests, group-comparison tests, correlation and partial correlation, PCA, random-forest explainability) are established published methods and are not original contributions of this software or its author. This guide describes how they are implemented here; the mathematics and the choices behind them are in the Scientific Guide. Works cited below are listed in the References section at the end of this document; please, run your own, secondary verification on the references before using them.*

## 0. Variable Naming

A **Variable naming** radio at the top of the Analytics Engine switches every dropdown, plot axis, title and table in the suite, the Governing Factors tab included, between the human-readable **variable labels** from your metadata mapping (default) and the raw **column names** of the uploaded file. Without a metadata mapping both options show the column names. The toggle changes presentation only; calculations and fitted models retain source column identifiers. Equal display labels include their source identifiers in brackets.

## 1. Global Data Grouping & Discretization

The **Data Grouping & Discretization** panel at the top of the Analytics Engine defines the groups and the sub-population that the Descriptive, Correlation and PCA tabs analyse. Groupings persist across the session, and results update immediately when a grouping changes. Governing Factors always fits its forest on every row of the dataset, whatever the grouping and the active groups.

*   **Grouping Variables (Max 5):** select categorical or numerical variables as grouping factors. Several variables combine into one group per observed combination of their levels.
*   **Discretization:** each grouping variable gets a *Type/Binning* selector: Categorical, Numeric: Median or Numeric: Mean (two classes, at or below and above), Numeric: Tertiles (Low, Medium, High) or Numeric: Quintiles (Q1 to Q5). A numeric variable with more than 10 distinct values starts on Median; any other variable starts on Categorical. Missing grouping measurements remain unassigned, including when tied tertile or quintile cut points leave one **Low Variation** group.
*   **Select Active Groups to Compare:** once groups are defined, this dropdown isolates specific sub-populations (all groups are selected at first).

---

## 2. Tab 1: Descriptive Suite

Univariate and bivariate distributions, and how they vary across the groups defined in the global panel.

**2.1 Plot type selection**

The **Plot Type** dropdown switches between thirteen visualization modes:
*   **Distribution:** Histogram, Density, ECDF, QQ Plot, Ridge/Joyplot.
*   **Categorical variance:** Boxplot, Violin, Sina-style Plot.
*   **Multivariate and spatial:** Scatterplot (with an optional **Add Trend Line**: Linear, Loess, Polynomial of degree 2 or GAM), 2D Density Heatmap, Parallel Coordinates (at least 2 variables), Radar Chart (at least 3), XYZ Surface (with a **Surface Fit Model**: Linear, Loess, Polynomial, GAM or Thin Plate Splines).

A **Color Palette** selector beside it sets the plot's colours. Plot axes and legends name the selected grouping context.

Parallel coordinates and radar normalize each variable using observed values in the active groups. Observed constants map to zero; missing measurements stay missing. Unavailable variables are named in the caption. Fewer than two observed dimensions for parallel coordinates, or three for radar, produces an explanation. Radar groups with missing dimensions show their available points without a polygon.

**2.2 Significance testing (ANOVA, Kruskal-Wallis and post-hoc)**
*   On the categorical-variance plots (Boxplot, Violin, Sina), the **Group comparison test** control is a single-select radio group: **None** (the default), ANOVA, Duncan's multiple range test (Duncan 1955), Tukey's HSD (Tukey 1949), and Kruskal-Wallis (Kruskal & Wallis 1952). Only one test is ever applied.
*   **Kruskal-Wallis** is the non-parametric route for data violating the normality assumptions, flagged by the normality indicator beside the control. Its pairwise post-hoc comparisons behind the significance letters are Benjamini-Hochberg adjusted (Benjamini & Hochberg 1995), consistent with the FDR policy used in the correlation table.
*   **Duncan's is labelled *(liberal)* for a reason.** It controls only the comparison-wise error rate, so with *k* groups its effective family-wise error rate grows toward 1 and it separates more means than Tukey's HSD on identical data. It is provided for reproducing older agronomy literature that reports it; **Tukey's HSD is the conservative default** for new work. See Scientific Guide Section 8.6.
*   Significance letters ('a', 'b', 'ab') are rendered directly on the plot geometries, so differences between groups can be read straight off the figure. ANOVA, and Tukey or Duncan with only two groups, print the *F* statistic, degrees of freedom and *p* instead of letters.

**2.3 Ghosting overlay**
*   **Enable Ghosting (Selected vs. Total)** overlays the active groups on a faded "ghosted" background representing the entire dataset. It applies to the Histogram, Density, Boxplot, Violin, Scatterplot and ECDF, and only while the active groups leave some rows out.

**2.4 Normality testing**
*   An integrated normality test assesses the distribution assumptions behind the parametric options.
*   **Test selection by sample size:** Shapiro-Wilk (Shapiro & Wilk 1965) below n = 5000, switching to the Lilliefors (Kolmogorov-Smirnov) test (Lilliefors 1967) at or above the app's n = 5000 cutoff. Below n = 3, or on a constant variable, no test is reported and the readout says which of the two applied.
*   **Group-aware:** with a grouping variable active, each group is tested separately and the overall test runs on the group residuals, so between-group differences do not register as non-normality. Without grouping it runs on the raw values. The readout states which was used.
*   **The verdict is text, not only a tooltip.** Beside the severity icon the panel prints one sentence naming the test, its statistic with that test's own symbol (*W* for Shapiro-Wilk, *D* for Lilliefors), the p-value, the significance stars, n, and the conclusion at alpha = 0.05, so it can be read without hovering and copied into a report. The icon's tooltip keeps the longer explanation and the per-group breakdown.

**2.5 Group statistics table**

Under the plot, one row per group plus a TOTAL row over every non-missing value. A real group called TOTAL keeps its own statistics; the pooled row receives a distinct label, such as **TOTAL (pooled)**, also when copied. Beside **Count**, **Mean** and **SD** the table reports the robust counterparts, because a few outliers move the moments a long way and leave these where they are:

*   **Median**, **Q1** and **Q3** from `stats::quantile(type = 7)`, R's default, so the quartiles agree with `summary()` elsewhere in the app.
*   **IQR** = Q3 - Q1.
*   **MAD**, `stats::mad()`, the median absolute deviation **scaled by 1.4826**. The constant makes it a consistent estimator of sigma for a normal sample, which is what lets it be read on the same scale as the SD beside it; an unscaled MAD would invite a wrong comparison.
*   **Min** and **Max** close the row.

Values are computed at full precision and displayed to four significant digits, so a small-unit variable keeps its digits and a near-constant column is never shown as a constant one. On a scatterplot with a trend line the table gains that fit's R² (or, for loess, its squared correlation) and p-value per group.

---

## 3. Tab 2: Correlation Analysis

Linear and monotonic relationships between the numeric variables in the dataset.

**3.1 Method selection**
*   `Pearson` (linear), `Spearman` (rank/monotonic), or `Kendall` (tau).

**3.2 Plot type selection**
*   **Hierarchical Heatmap:** clusters highly correlated variables together.
*   **Correlation Network:** a node-edge graph where edge thickness carries correlation strength; pairs weaker than the **Correlation Threshold** (default 0.3) are not drawn.
*   **Partial Correlation:** correlations computed while controlling for the **Control Variables (Partial Out)**. The estimator follows the selected method (`ppcor` conventions, Kim 2015; Scientific Guide Section 8.3): **Pearson** residualizes the raw values on the controls, **Spearman** residualizes the *ranks* (a partial rank correlation is the product-moment partial correlation of ranks, not a rank correlation of raw-value residuals), and **Kendall** inverts, for each pair, the Kendall tau matrix of that pair and the controls. Every method conditions each pair on the controls only, so adding a variable to the table does not change the other pairs' coefficients; all pairs are computed on the rows complete for every selected variable, so a variable with missing values does shrink the shared *n* printed under the table. The p-values account for the controls: with effective control degrees of freedom *k*, significance is computed on *n − 2 − k* degrees of freedom (Pearson and Spearman t-statistic; Kendall uses a normal approximation with effective sample size *n − k*) rather than the naive *n − 2* of a plain correlation test. For Pearson and Spearman, *k* is the fitted control design rank minus its intercept, so redundant or constant controls do not consume extra degrees of freedom. Targets with no residual variation, or nonpositive residual test degrees of freedom, produce an explanation in both plot and table. Kendall retains its explicit refusal of singular tau matrices.
*   **Correlogram:** a sorted matrix view of the same coefficients, sized and coloured by strength.
*   **Spatial Cross-Correlogram:** cross-correlation between two variables **as a function of the ground distance separating sample points**, the spatial analogue of a time-series cross-correlation. It requires the X/Y columns and the Input Data CRS to be set on the Data Setup tab, and coordinates are projected to metres before binning, so the x-axis is a real distance and never a table row offset. **Distance Bins** (3 to 50, default 15) sets how many distance classes the pairs are sorted into. Point size shows how many pairs fall in each distance bin, bins with fewer than 30 pairs are greyed as unreliable, and the dashed line marks the ordinary non-spatial correlation for reference. With `Spearman` or `Kendall` selected the curve is computed on ranks and labelled accordingly. Method and interpretation: Scientific Guide Section 8.4 (Journel & Huijbregts 1978; Goovaerts 1997).

**3.3 Sample used**
*   The four matrix panels (heatmap, network, partial, correlogram) are one matrix, estimated on the rows with no missing value in **any** selected variable, control variables included. The figure and the table both state that sample: *"Complete cases: n = 87 of 132 rows (45 dropped for missing values)."* A large drop is a fact about the dataset, not a display artifact: narrow the variable selection, or impute deliberately before uploading. Why complete cases rather than pairwise: Scientific Guide Section 8.7.
*   The Spatial Cross-Correlogram works on the two chosen variables and the coordinates, so it uses the samples complete across those four columns.

**3.4 Data table**
*   The **Correlation Matrix** table below the plot gives the exact numerical coefficients for inspection and copying. Pairwise tables report both the raw p-value and a Benjamini-Hochberg adjusted column computed across all pairs shown.

---

## 4. Tab 3: Principal Component Analysis (PCA)

**4.1 Variable screen before the PCA**
*   Choose the variables under **PCA Setup**, leave **Scale & Center Data (Recommended)** ticked unless the variables share one unit and scale, and press **Run PCA**. The PCA requires at least five rows complete across the selected variables; a smaller complete population produces a persistent explanation. Before the PCA executes, those same complete rows are scanned, and what the scan finds is reported in up to two sections, each with its own heading and sentence (a section with nothing to report is left out):
    *   **Near-duplicate pairs:** pairs with |*r*| > 0.95, each with its *r*. A near-duplicate pair measures one direction twice, so that direction is weighted double in the leading components and the biplot understates every other variable.
    *   **No variance:** variables constant across the selected rows. They carry no information and cannot be standardised.
*   There is no VIF screen: a variance inflation factor describes regression coefficients, while the shared variance it would flag is what the components summarise.
*   The near-duplicate pairs are **advisory**: they stop the PCA and ask you to remove one variable of each pair from the selection, or to press **Run PCA with these variables**, because keeping both can be a legitimate choice. A constant variable is not a judgement call, so it never stops anything and never comes with the button: a variable with exactly or effectively no variance (the same rule the interpolation engines apply to covariates) is excluded from the PCA automatically, and the remaining variables keep "Scale & Center Data". A PCA with excluded variables shows a standing note above the results naming them, and every PCA figure, including the downloaded PNG, names them in its caption.
*   **At least two variables with variance are needed.** When the exclusion leaves fewer, the PCA is refused with a persistent message naming the excluded variables, in place of the results.

**4.2 Plot settings**
*   **Types:** Scree Plot, Biplot (2D), 3D PCA Scores, Loadings, Contribution, Quality of Rep. (Cos2), Cumulative Variance, and Mahalanobis Distance.
*   **Controls:** inputs appear according to the plot type: the components on the axes (X-Axis PC 1 and Y-Axis PC 2, plus Z-Axis PC 3 for 3D PCA Scores), a single PC for the loadings and contributions, or the PCs whose cos2 is summed.
*   **3D PCA Scores** shows observations, without loading arrows, and requires at least three available components. Two-component fits remain available in the other views. Every component selection is checked against the current fit, including after rerunning with fewer variables.
*   The **PCA Results** table under the plot gives the numbers behind it.
*   **Scaling caveat (Contribution):** contribution is the share of a *component* attributable to a variable, so it is scale-sensitive by definition. With "Scale & Center Data" unchecked, the values are dominated by the high-variance variables and are not comparable across variables measured on different scales; the module shows an inline note under the plot controls in that case.
*   **cos2 (quality of representation):** the share of a *variable's own* variance captured by the selected PCs, normalised by that variable's total variance across all components, so it always reads 0 to 1 whether or not the PCA was scaled (Abdi & Williams 2010; Lê et al. 2008; Scientific Guide Section 8.5). For an unscaled run the inline note points out what does still depend on scaling: the components themselves are driven by the high-variance variables.
*   **Mahalanobis distance (classical estimator; Mahalanobis 1936):** distances are computed on the PC scores using their theoretical diagonal covariance, the squared component standard deviations. Near-zero-variance components, which arise when a PCA is run on near-duplicate variables, are excluded from the distance and from the chi-square threshold's degrees of freedom, so the outlier plot stays available instead of failing on a singular covariance matrix. The centre and scatter are the **classical** mean and covariance, which the suspected outliers themselves contribute to, so a cluster of extreme observations can inflate the covariance and hide inside the threshold (*masking*); the high-breakdown alternative is the minimum covariance determinant estimator (Rousseeuw & Van Driessen 1999), which the panel deliberately does not use. Read the panel as a screening aid, not a decision rule. See Scientific Guide Section 8.2.

---

## 5. Tab 4: Governing Factors

Machine-learning explainability applied to non-linear relationships and feature interactions.

**5.1 Configuration**
*   **Target Parameter:** the primary soil parameter to explain.
*   **Governing Factors:** the environmental or secondary variables acting as potential influences, picked from a list with a search box and Select All / Deselect All. The target is never used as its own factor. The analysis needs at least 50 rows that carry the target and every selected predictor; the message names how many rows qualify when fewer do.
*   **Number of trees (ntree):** the size of the underlying random forest (`randomForest`; Liaw & Wiener 2002) (50 to 500, default 100). The importance is averaged over the trees, so more trees give a more stable importance estimate, at a longer run.
*   **SHAP sample size (max):** SHAP explanations are computationally intensive, so this caps the random subsample (50 to 1000, default 100). Lower values run faster for quick exploration; higher values represent the dataset better.
*   Press **Run Analysis**. The results are four plots (Global Importance, SHAP Dependence, Functional Effect, and a Feature vs Target Scatterplot of the most important factor) above the Tabular Data Metrics table.

**5.1.1 Cancelling a run**

A **Cancel Run** button sits in the running panel and takes effect at the next checkpoint rather than instantly. Checkpoints sit before the random-forest fit, before the ALE/PDP profiles, and between individual SHAP observations. The SHAP loop is normally the longest stage and is checked per observation, so a cancel there is usually picked up within a second or two. The exception is the forest fit, which also computes the importance and runs as a single uninterruptible call; fewer trees shorten that window. A cancelled run keeps no partial results: the previous run's plots stay on screen and the Run Analysis button becomes available again.

**5.2 Functional effect plots**

The **Functional Effect Plot** switch under Plot Settings chooses between two explainability frameworks, both drawn for the most important factor:
*   **ALE (Accumulated Local Effects; Apley & Zhu 2020):** a faster alternative to partial dependence that maps the main effect of a predictor on the target. It accumulates prediction differences over small intervals of the predictor's observed values, so with correlated predictors it avoids partial dependence's extrapolation into combinations of values the data do not contain.
*   **PDP (Partial Dependence Plot; Friedman 2001):** the marginal effect of a feature on the predicted outcome.

**5.3 SHAP dependence plot**
*   Each point is the per-observation SHAP attribution (Lundberg & Lee 2017) of the most important predictor: how much that predictor shifts the model's prediction for that sample away from the dataset-mean prediction, in the target variable's own units. Across all predictors the values sum to the deviation of the sample's prediction from the mean.

**5.4 Tabular data metrics**
*   The importance of each governing factor is its **out-of-bag permutation importance** (Breiman 2001): for every tree, how much the mean squared error of its predictions on the samples that tree was not grown on increases when that factor's values are shuffled, averaged over the trees, in the target's squared units. Scoring on samples each tree never saw keeps the forest's memory of its own training data out of the ranking: a factor with no real relationship to the target scores near zero, where shuffling on the training samples would still credit it. Values near zero or below indicate the factor adds nothing the forest can use. Correlated factors distort the ranking: the forest divides their shared signal between them, and shuffling one of them alone creates combinations of values the data do not contain (Strobl et al. 2008). The table leads with an **RF model quality row, out-of-bag (OOB) % variance explained**, so the reliability of the random forest behind the importance and SHAP results can be judged directly. Low OOB values mean the explainability outputs describe a weak model and should be interpreted cautiously. A high one is not a map-accuracy figure: out-of-bag samples lie among the training samples, so with spatially autocorrelated samples the OOB variance explained overstates how well the forest predicts at new locations (Meyer et al. 2019; Ploton et al. 2020).
*   The table has the columns **Governing Factor / Metric**, **Value** and **Unit**, because the two kinds of number in it are not on one scale: the importance rows are an increase in out-of-bag MSE in the target's squared units, the model-quality row a percentage of variance. A fourth column, **Scaled (÷ SE)**, gives each factor's increase divided by its standard error across the trees, the form `randomForest` prints as %IncMSE and the one the Random Forest Kriging importance panel shows. It grows with the number of trees (roughly with its square root), so compare scaled values only between runs with the same ntree; the unscaled increase orders the factors. The importance plot shows both forms side by side. Values are displayed at four significant digits.

---

## 6. Expandable Plot Engine

On the Descriptive, Correlation and PCA tabs the main plot, and on the Governing Factors tab each of the four plots, carries an **expand** icon in its top right corner.

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

Meyer, H., Reudenbach, C., Wöllauer, S., & Nauss, T. (2019). Importance of spatial predictor variable selection in machine learning applications: moving from data reproduction to spatial prediction. *Ecological Modelling*, 411, 108815. https://doi.org/10.1016/j.ecolmodel.2019.108815

Ploton, P., Mortier, F., Réjou-Méchain, M., Barbier, N., Picard, N., Rossi, V., Dormann, C., Cornu, G., Viennois, G., Bayol, N., Lyapustin, A., Gourlet-Fleury, S., & Pélissier, R. (2020). Spatial validation reveals poor predictive performance of large-scale ecological mapping models. *Nature Communications*, 11, 4540. https://doi.org/10.1038/s41467-020-18321-y

Rousseeuw, P. J., & Van Driessen, K. (1999). A fast algorithm for the minimum covariance determinant estimator. *Technometrics*, 41(3), 212-223. https://doi.org/10.1080/00401706.1999.10485670

Shapiro, S. S., & Wilk, M. B. (1965). An analysis of variance test for normality (complete samples). *Biometrika*, 52(3/4), 591-611. https://doi.org/10.2307/2333709

Strobl, C., Boulesteix, A.-L., Kneib, T., Augustin, T., & Zeileis, A. (2008). Conditional variable importance for random forests. *BMC Bioinformatics*, 9, 307. https://doi.org/10.1186/1471-2105-9-307

Tukey, J. W. (1949). Comparing individual means in the analysis of variance. *Biometrics*, 5(2), 99-114. https://doi.org/10.2307/3001913
