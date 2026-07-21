# Scientific & Analytical Methodology Guide

The Monolith Spatial Analysis Dashboard provides agronomists, soil scientists, and geostatisticians with a toolkit for exploring spatial variability. This guide briefly elaborates on the mathematical intuition, structural assumptions, and practical applications of the underlying spatial methods and evaluation metrics.

> *Note: The statistical and geostatistical methods described in this document (e.g., kriging variants, IDW, TPS, cross-validation metrics, PCA, statistical tests) are established methods previously published in the literature; they are not original scientific contributions of this software or its author. This document explains how these methods are implemented within Monolith for practical and operational clarity, and is not designed to serve as a citable source for their theoretical origins. Users who wish to cite the original literature source for a given method should locate and review the relevant sources themselves, as this document does not aim to provide academic citations for each method.*

---

## 1. Spatial Interpolation Engines

Spatial interpolation creates continuous prediction surfaces from discrete point samples. The dashboard implements both deterministic methods (relying solely on geometric proximity) and geostatistical models (incorporating spatial autocorrelation and statistical uncertainty).

**NaN Protection:** Across all Kriging methods, the resulting predictions (`var1.pred`) and theoretical variances (`var1.var`) are systematically protected against NaN or Infinite outputs, explicitly converting them to `NA` for downstream mapping.

### 1.1 Ordinary Kriging (OK)

**Mathematical Intuition:** Ordinary Kriging assumes that the value at an unsampled location <i>Z<sup>*</sup>(x<sub>0</sub>)</i> is a linear combination of known surrounding values <i>Z(x<sub>i</sub>)</i>. The formula is:
<br><br>
<div style="text-align:center;"><i>Z<sup>*</sup>(x<sub>0</sub>) = &sum; &lambda;<sub>i</sub> Z(x<sub>i</sub>)</i></div>
<br>
Unlike simple kriging, OK assumes an unknown, constant global mean (<i>&mu;</i>). The weights <i>&lambda;<sub>i</sub></i> are determined by minimizing the estimation variance while ensuring the weights sum to 1 (<i>&sum; &lambda;<sub>i</sub> = 1</i>). The variance-covariance matrix used to solve for these weights is derived directly from the theoretical variogram.

**Agronomical Example:** Predicting soil pH across a relatively uniform field where variations are driven by natural soil-forming processes rather than abrupt topographical changes or human intervention.

**Algorithmic Stability (Epsilon-Nugget):** For variables with extremely low variance (e.g., specific micro-nutrients like Iron), the codebase strictly enforces a tiny nugget (`max(initial_sill * 1e-6, 1e-6)`) when the initial empirical nugget is exactly zero. This prevents singular matrix inversion failures during Kriging.

### 1.2 Regression Kriging (RK)

**Mathematical Intuition:** RK decomposes the spatial variable into a deterministic trend and a stochastic residual.
<br><br>
<div style="text-align:center;"><i>Z(x) = m(x) + e(x)</i></div>
<br>
First, a generalized linear model fits the trend <i>m(x)</i> using secondary covariates (e.g., elevation, NDVI). The residuals <i>e(x)</i> represent the spatially correlated variation not explained by the trend. Ordinary Kriging is then applied to the residuals. The final prediction sums the trend and kriged residuals.

**Agronomical Example:** Mapping Soil Organic Carbon (SOC). Elevation and soil moisture (derived from satellite imagery) are used to predict the baseline SOC trend. RK then kriges the residuals to adjust for localized organic matter accumulations missed by the remote sensing data.

**Reported Trend Diagnostics:** For each per-locality trend model, the Scientific Analysis tab reports R², adjusted R², the residual standard error with its degrees of freedom, the overall F test (with p-value), and a coefficient table with standard errors, t statistics, p-values, and 95% confidence intervals computed from the t distribution on the residual degrees of freedom. These describe the *trend component only*; the quality of the full RK prediction (trend + kriged residuals) is assessed by the cross-validation metrics in the Model Performance table.

### 1.3 Random Forest Kriging (RFK)

**Mathematical Intuition:** RFK mirrors the logic of Regression Kriging but replaces the linear trend model with a Random Forest ensemble learning algorithm. Random Forests build numerous decision trees and average their predictions. RFs are highly flexible and capable of capturing complex, non-linear interactions among covariates without strictly assuming a linear or parametric functional form. OK is then applied to the Random Forest residuals.

**Agronomical Example:** Predicting variable crop yield across a highly heterogeneous landscape where the relationship between yield, slope, aspect, and electrical conductivity is highly non-linear and interactive.

### 1.4 Co-Kriging (CK)

**Mathematical Intuition:** CK extends Ordinary Kriging by using one or more secondary variables to improve the prediction of a primary variable. It relies on the cross-variogram, which models how the two variables co-vary in space:
<br><br>
<div style="text-align:center;"><i>&gamma;<sub>12</sub>(h) = (1 / 2N(h)) &sum; [Z<sub>1</sub>(x) - Z<sub>1</sub>(x+h)][Z<sub>2</sub>(x) - Z<sub>2</sub>(x+h)]</i></div>
<br>
By incorporating the cross-covariance matrix, CK utilizes dense secondary sampling to inform the sparse primary data.

**Agronomical Example:** You have sparse, expensive laboratory soil tests for Nitrate (NO3), but dense, cheap sensor data for Soil Electrical Conductivity (EC). Since EC and Nitrate often co-vary, CK uses the dense EC points to dramatically improve the Nitrate interpolation surface.

**Covariate Kriging Fallback (CK/RK/RFK):** When interpolating covariates across the spatial grid, if covariate kriging fails (e.g., due to pure nugget effects or collinearity collapses), the pipeline implements an automatic and silent `tryCatch` fallback to IDW (p=2, nmax=12). Documenting this increases transparency on how the spatial engine ensures map generation succeeds.

**Covariate Standardization and a Known Cross-Validation Leak:** Before the LMC is fitted, every auxiliary variable is standardized to zero mean and unit variance (the target is **not** rescaled, so predictions, cross-validation metrics and the plotted variograms all remain in the variable's own units). The mean and standard deviation used are those of the **full data set**. Cross-validation then holds points out of an already-centred frame, so each held-out observation contributed — by a factor of 1/n — to the centring of its own predictors. This is a genuine but *affine, second-order, O(1/n)* optimism: it cannot change the rank ordering of predictions, and it touches the covariates only. Removing it entirely would require re-standardizing inside every fold, which means replacing `gstat.cv()` with a hand-rolled co-kriging cross-validation loop; that cost was reviewed and deliberately declined in favour of documenting the approximation here. CK metrics should therefore be read as marginally optimistic relative to OK metrics on the same data.

**A note on LMC starting values:** `fit.lmc()` receives one starting variogram model that gstat copies onto every direct and cross variogram, and that seed carries the target's raw variance even for the unit-variance covariates. This looks like a scaling bug and has been reported as one, but it is provably inert: the fit runs with `fit.ranges = FALSE`, and with the model type and range held fixed the variogram is *linear in its sill parameters*, so the weighted-least-squares solution is independent of the starting sill. This was verified empirically (starting sills spanning 1e-6 to 1e12 on one empirical variogram return a bit-identical fitted sill; seeding each variogram from its own empirical plateau reproduces the current LMC exactly for target variances up to 3.5e8). The starting values *would* matter if range fitting were ever enabled, and the code carries a comment to that effect.

**Local Search Neighborhood (nmax):** Co-Kriging solves the system using only the nearest `nmax` observations of every variable, exposed as the **CK Max Neighbors** slider (range 5–60, **default 15**) in the sidebar's Spatial Engine panel. This is a *modelling* choice, not merely a speed setting: `nmax` sets how local the stationarity assumption is. A small neighbourhood assumes the mean and covariance structure are constant only within a short radius (appropriate for non-stationary fields, and it avoids the matrix growth of Global Co-Kriging, which builds the cross-covariance over the whole dataset at once); a large neighbourhood approaches a global system and is preferable when the field is genuinely stationary and the sampling is sparse. Because the appropriate value depends on point density and the range of the fitted cross-variogram, no single number is defensible across every dataset — 15 is the default (and the historical hardcoded value), not a universal recommendation. OK/RK/RFK, by contrast, krige with a global neighbourhood; IDW has its own **Max Neighbors** control. The applied value is recorded in the run-configuration summary.

### 1.5 Inverse Distance Weighting (IDW)

**Mathematical Intuition:** A purely deterministic method. The estimated value is a weighted average of known points, where the weight is inversely proportional to the distance <i>d</i> raised to a or power <i>p</i> (usually <i>p=2</i>):
<br><br>
<div style="text-align:center;"><i>&lambda;<sub>i</sub> = (1 / d<sub>i</sub><sup>p</sup>) / &sum; (1 / d<sub>i</sub><sup>p</sup>)</i></div>
<br>
IDW assumes that points closer to the target are more similar. It does not account for data clustering (redundant sampling) or directional anisotropy.

**Agronomical Example:** Quick, computationally inexpensive mapping of recent, localized rainfall events from a scattered network of rain gauges where statistical assumptions of stationarity are not strictly necessary.

### 1.6 Thin Plate Spline (TPS)

**Mathematical Intuition:** TPS is a deterministic method akin to bending a sheet of metal to pass exactly through the sampled data points while minimizing the "bending energy" (the integral of the squared second derivatives of the surface). It yields highly smooth surfaces but is susceptible to severe overshooting or undershooting in areas devoid of data.

**Agronomical Example:** Generating smooth elevation contours or temperature gradients where abrupt discontinuities are physically implausible.

**Engine Fallback (TPS → IDW):** If the TPS fit fails entirely for a locality (e.g., a degenerate point configuration), the pipeline automatically substitutes an IDW interpolation using the active IDW parameters. The substitution is recorded in the run log (`TPS failed: … Falling back to IDW.`) and surfaced as a per-locality warning, so fallback outputs are never mistaken for genuine TPS results.

---

## 2. Grid Resolution Logic

Grid resolution (cell size) determines the size of each pixel in your final mapped surface. It represents the spatial support of your model. Selecting the correct resolution ensures your map captures the true scale of variation without overspecifying or losing local detail.

### 2.1 The Spatial Support Metric (FNN Nearest-Neighbor)
When resolution is determined dynamically, the engine utilizes a **Fast Nearest Neighbor (FNN)** algorithm to analyze the point density of the samples. The recommended spatial resolution ($R_{rec}$) is defined as exactly half the expected nearest-neighbor distance:
<br><br>
<div style="text-align:center;"><i>R<sub>rec</sub> = 0.5 &times; Expected Nearest-Neighbor Distance (h<sub>NN</sub>)</i></div>
<br>
This ensures that the mapped grid spacing is directly proportional to your physical sampling spacing, preventing the generation of artificial, unmeasured detail.

All resolution recommendations are computed and expressed in **metres**, regardless of the analysis CRS. If a geographic (degree-based) CRS is selected, nearest-neighbor distances and extents are measured via a Web Mercator (EPSG:3857) projection corrected by cos(latitude) (`calc_metric_spacing()` in `spatial_pipeline.R`). Interpolation itself always runs in a projected CRS, so metric pixel sizes remain valid.

### 2.2 Auto (Per Locality)
In this mode, the FNN point-density analysis is run **independently** for each locality. If Locality A is densely sampled, it receives a fine resolution (e.g. 50 m); if Locality B is sparsely sampled, it receives a coarse resolution (e.g. 150 m). This is the most geostatistically rigorous approach for multi-site studies.

### 2.3 Auto (Global)
In this mode, the FNN analysis evaluates the point density of the **entire dataset** as a single spatial unit. A uniform, global resolution value is calculated and applied to all selected localities, ensuring map pixels match in size across all generated outputs.

### 2.4 Fixed (Manual)
The user manually overrides the dynamic recommendations by adjusting a slider input (e.g. to 50 m or 20 m). This is useful when the researcher wants to force a specific, high-resolution pixel size for cartographic consistency, premium rendering, or printing.

---

## 3. Spatial Boundary & Dynamic Buffering Systems

Defining the boundary (or clipping mask) of your interpolation is critical to determine the physical extent of predictions. Unconstrained interpolation can lead to high-risk extrapolation. Monolith implements an automated **Dynamic Buffering** engine that scales the boundary padding of each locality based on the active resolution and the mathematical constraints of the selected interpolation method.

### 3.1 Boundary Types
* **Convex Hull / Concave Hull:** Mathematical envelopes drawn tightly around the outer limits of your data points. They connect the outermost sample coordinates like a shrink-wrap and **do not use or support buffer padding**.
* **Wrapped (Buffered):** Creates a concave hull wrapped around the points, but inflates it outwards by the buffer distance ($D_b$) to ensure the mapped area covers the fields' outer borders.
* **Strict Measured (Point Buffer):** Creates individual buffer circles around every point and unions them together.

### 3.2 Dynamic Buffering (Wrapped Mode)
Rather than applying a single universal buffer distance to all datasets, the dynamic buffering system scales the padding distance ($D_b$) relative to the active resolution. 
* **Calculation:** If resolution is set to *Auto*, the buffer scales with the FNN density resolution ($R_{local}$). If the user overrides to *Fixed* resolution, the buffer scales with the manual slider value ($R_{manual}$), providing an interactive, live-updating UI where adjusting the slider instantly updates the buffer column.
* **Method-Specific Buffer Ratios:**
  1. **Thin Plate Spline (TPS):**
     - **Ratio:** $D_b = 1.0 \times \text{Resolution}$
     - TPS is a global smoothing spline that minimizes bending energy. Outside the sample bounds, splines bend unconstrainedly, causing severe runaway edge effects ("spline explosions"). A very tight buffer is critical to crop the grid before these unconstrained spline edges ruin the scale.
  2. **Inverse Distance Weighting (IDW):**
     - **Ratio:** $D_b = 2.0 \times \text{Resolution}$
     - IDW is a local deterministic method where weights drop off exponentially with distance. Extrapolating beyond average sample spacing results in flat, artificial "halos" that converge to the global mean. A medium buffer crops the map within the logical region of neighbor decay.
  3. **Kriging Suite (OK, CK, RK, RFK):**
     - **Ratio:** $D_b = 3.0 \times \text{Resolution}$
     - Geostatistical models utilize the semivariogram to characterize spatial autocorrelation. Predictions naturally decay toward the global mean (or trend) as distance increases, and kriging variance is recorded. A wider buffer is safe and appropriate, matching the range of spatial correlation.

### 3.3 Strict Measured (Point Buffer) Manual Enforcement
In **Strict Measured** mode, the boundary is formed by drawing circular buffer zones around every individual sample point and unioning them.
- **The Extrapolation Challenge:** Dynamic buffering in this mode can lead to massive, exaggerated mapped areas that distort the actual point-level influence. For example, if a field has a coarse 200m resolution, an automated 3x multiplier creates 600m circles around each point, artificially creating a vast mapped footprint that overclaims coverage.
- To prevent this exaggeration and give users control, the **Strict Measured** boundary type strictly disables dynamic buffering and operates solely under **Fixed (Manual)** settings.

**Grid evaluation domain:** the prediction grid is generated over the boundary's bounding box, but grid nodes falling outside the boundary are excluded *before* the engines run rather than masked afterwards. All engines predict pointwise, so this changes no within-boundary value; it only removes computation the mask would have discarded — a substantial saving for concave or multi-part boundaries whose bounding box is much larger than their area.

---

## 4. Automated Optimizations

### 4.1 Variogram Optimization
The empirical semivariogram <i>&gamma;(h)</i> quantifies spatial dependence by calculating half the average squared difference between paired data values separated by a distance lag <i>h</i>:
<br><br>
<div style="text-align:center;"><i>&gamma;(h) = (1 / 2N(h)) &sum; [Z(x<sub>i</sub>) - Z(x<sub>i</sub> + h)]<sup>2</sup></i></div>
<br>
Geostatistical models require fitting a theoretical continuous curve (e.g., Spherical, Exponential, Gaussian) to this empirical scatterplot.

### Tuning Parameters
- **Nugget (<i>C<sub>0</sub></i>):** The y-intercept. In theory, <i>&gamma;(0) = 0</i>, but in practice, measurement error and micro-scale spatial variation cause a discontinuity at the origin. A high nugget implies a noisy dataset.
- **Partial Sill (<i>C</i>):** The structured spatial variance. The total sill (<i>C<sub>0</sub> + C</i>) represents the apriori variance of the data (where the variogram flattens out).
- **Range (<i>a</i>):** The maximum distance of spatial autocorrelation. Beyond this distance lag, points are statistically independent.

**Auto-Fit vs. Manual Tuning:**
The dashboard employs an automated least-squares fitting algorithm to establish a baseline. 
- **Robust Fallback Engine:** To prevent mathematical crashes (e.g., singular matrix inversion) on difficult datasets, the auto-fit engine features a dynamic multi-model fallback. It sequentially attempts to fit Ste (Stein's), Sph (Spherical), Exp (Exponential), Gau (Gaussian), and Mat (Matern, &nu; = 1.5) models. If standard range fitting fails, it recursively falls back to estimating ranges at `max_dist/3` and `max_dist/2` to ensure a stable curve is successfully mapped.
- **Matern smoothness is fixed, not estimated.** The candidate search fits nugget, partial sill and range by weighted least squares, but leaves the Matern smoothness parameter &nu; (kappa) at its starting value of **1.5** — the Matern 3/2 model — for every candidate. `gstat::fit.variogram` only estimates &nu; when explicitly asked to (`fit.kappa = TRUE`), and it is deliberately not asked here: a free smoothness parameter would give the Matern candidate one more degree of freedom than the other four and make the cross-candidate residual-sum-of-squares comparison that selects the winner an unequal contest. Read the "Mat" candidate as "Matern with &nu; = 1.5", not as a fitted-smoothness Matern. If the empirical variogram suggests a markedly smoother or rougher process near the origin, use **Manual Tuning** and compare the Ste (Stein's) parameterization, which behaves differently at short lags.
- **Manual Override:** However, automated fits can get trapped in local minima or overfit to outliers at high lag distances. You may switch to **Manual Tuning** to prioritize the fit at shorter lags, which have the greatest impact on kriging weights.

---

### 4.2 IDW Optimization
* **Logic**: The application performs an automated search to find the optimal **Distance Power** that minimizes the spatial interpolation error for each specific locality.
* **Optimization Engine**: The system executes a cross-validation loop to find the optimal power factor, testing values from **0.5 to 5.0**. If the dataset has 50 or fewer observations, it uses **Leave-One-Out Cross-Validation (LOOCV)**.
* **Large Dataset Handling**: For larger datasets (typically > 50 points), the engine automatically switches to **5-Fold Cross-Validation** to maintain computational efficiency without sacrificing statistical reliability.
* **Consistency & Reproducibility**: To ensure a fair comparison across all candidate power factors and maintain true reproducibility, the engine generates a single, deterministic fold-assignment vector (using a fixed random seed). This shared partition removes fold-induced evaluation noise, ensuring the optimal power is selected strictly based on its distance-decay performance.
* **Local Adaptation**: As the soil variability is site-specific, the "Optimize" button calculates a unique power factor for every selected locality. 
* **Projected Search**: The power search runs on the same projected (metric) coordinates the interpolation itself uses. A geographic (degree-based) upload is transformed to its local UTM zone before the search, so nearest-neighbor selection and distance decay use true ground distances (a degree of longitude is shorter than a degree of latitude away from the equator, so a degree-based search would distort both), and the stored power matches the run that consumes it.
* **Interpretation Caveat**: The power factor is selected and subsequently evaluated on the same cross-validation data (no nested/outer CV loop). The reported CV metrics for an optimized IDW model may therefore be mildly optimistic, since the power was tuned to minimize exactly that error. This is a deliberate, documented trade-off: a nested CV would multiply the computational cost for a single tuning parameter with a coarse candidate grid, where the selection-induced optimism is small. Compare methods with this in mind.

### 4.3 TPS Optimization
* **Logic**: The software optimizes the **Smoothing Parameter** to achieve the ideal mathematical balance between honoring every individual data point and creating a generalized regional trend. By default, the TPS lambda parameter is set to `< 0` (Auto GCV) to apply this natively during interpolation.
* **GCV Diagnostics**: The engine utilizes **Generalized Cross-Validation (GCV)** within the `fields::Tps` algorithm to automatically find the optimal lambda.
* **Interpretation**: The "Best Lambda" is defined as the value achieving the lowest GCV score. A lambda of 0 indicates an "Exact Interpolator" (zero error at sample points), while higher values indicate a "Smoothing Spline," which is often better for handling noisy sensor data.
* **Visualization**: If the user explicitly clicks the "Optimize TPS Lambda" button, the engine runs an explicit grid search to extract the optimal value, overriding the Auto mode. The resulting **GCV Curve** is then plotted in the Scientific Analysis tab, allowing the user to verify if the optimization process reached a clear mathematical minimum.
* **Projected Search**: As with IDW (Section 4.2), a geographic upload is projected to its local UTM zone before the coordinates are normalized to the unit square, so the spline geometry (and its GCV-optimal lambda) reflects true ground distances rather than a distorted degree-based aspect ratio, and agrees with the interpolation run.

---

## 5. Validation Diagnostics

The dashboard runs cross-validation for the selected spatial model to produce the predicted-versus-observed pairs behind every performance metric. The **Cross-Validation Strategy** control (directly beneath the Interpolation dropdown in the Spatial Engine panel) governs how the held-out folds are formed. It affects the reported metrics only, **never the interpolated surface**, and never the IDW power search (Section 4.2), which keeps its own dedicated fold scheme:

- **Auto (Default):** **Leave-One-Out Cross-Validation (LOOCV)** for datasets of 50 or fewer observations; a seeded, balanced **random 10-Fold Cross-Validation** above 50 for computational efficiency.
- **Standard LOOCV:** Full Leave-One-Out regardless of sample size, the most rigorous option, but computationally heavy beyond ~2000 points (the kriging engines re-fit the variogram on every fold).
- **Spatial Block CV:** Ten spatially contiguous folds formed by k-means clustering of the sample coordinates. Random folds place a test point's near-neighbours into the training set, so under spatial autocorrelation the model effectively interpolates from almost-collocated data and cross-validation over-states its skill. Holding out whole spatial blocks removes that leakage, giving a more honest estimate of prediction at genuinely unsampled locations (recommended for digital-soil-mapping validation, e.g. Roberts et al. 2017; Ploton et al. 2020). Below 30 observations the blocks become degenerate, so the engine automatically falls back to LOOCV.

All fold assignments use a fixed seed (`12345`) for reproducibility.

> **Rank requirement for Regression Kriging.** Every RK fold refits the linear trend on the fold's training rows, so each fold needs more training points than the model has coefficients (one per covariate, plus the intercept, plus one residual degree of freedom). When the largest fold leaves too few points — many covariates, few samples, or a coarse blocking scheme on a small locality — the fold's `lm` is rank-deficient and predicts `NA`, which propagates through the residual kriging and turns the reported metrics into `NA` with nothing indicating why. The engine now checks this up front and **reports an explicit error naming the shortfall** instead of returning silent `NA`s; the same guard applies to the main RK fit, where falling short routes the locality through the standard Ordinary Kriging fallback with the reason logged. The remedy is fewer covariates, more samples, or a strategy with smaller held-out folds.

By dropping points according to the chosen partition, we generate a dataset of predicted vs. actual values (<i>P<sub>i</sub></i> vs <i>O<sub>i</sub></i>). The cross-validation engine utilizes a centralized metric abstraction (`perform_cv`) to process an expanded suite of metrics natively.

- **RMSE (Root Mean Square Error):**
  <div style="text-align:center;"><i>RMSE = &radic;( &sum; (P<sub>i</sub> - O<sub>i</sub>)<sup>2</sup> / n )</i></div>
  The absolute measure of fit in the units of the variable. Smaller is better.

- **Traditional R&sup2; vs. Correlation R&sup2;:**
  - **Traditional R&sup2; (Nash-Sutcliffe Efficiency):** Defines how well the model predicts relative to simply using the global mean. 
    <div style="text-align:center;"><i>R&sup2; = 1 - &sum;(O<sub>i</sub> - P<sub>i</sub>)<sup>2</sup> / &sum;(O<sub>i</sub> - O<sub>mean</sub>)<sup>2</sup></i></div>
    It penalizes bias and can be negative if the model is worse than the mean.
  - **Correlation R&sup2; (Pearson's):** Only measures linear correlation. A model could consistently predict exactly double the actual value and have a Correlation R&sup2; of 1.0, but a Traditional R&sup2; &lt; 0. We prioritize Traditional R&sup2; for spatial accuracy.

- **MBE (Mean Bias Error):**
  <div style="text-align:center;"><i>MBE = &sum; (P<sub>i</sub> - O<sub>i</sub>) / n</i></div>
  Indicates systemic bias. Positive values indicate the model generally overestimates; negative implies underestimation.

  > **Sign convention — two different bias statistics are reported.** The tables that compare an **uploaded ML prediction column against the observed values** report this MBE, labelled **"MBE (ML pred - observed)"**, in the direction *predicted minus observed* shown above. The **Model Performance** table for the interpolation engines instead reports **"Bias (ME)"**, the mean cross-validation error, computed in the opposite direction, *observed minus predicted*. The two quantities describe different models (an external prediction column versus the spatial interpolator's own CV) and carry **opposite signs for the same over-prediction**, so never read one against the other; the labels are deliberately distinct for this reason.

- **CCC (Lin's Concordance Correlation Coefficient):** Evaluates the degree to which the paired data fall on the 45-degree line of perfect agreement. It combines precision (Pearson's r) with accuracy (bias shift). CCC is reported as **NA when either vector is constant** (zero variance): the correlation term does not exist there, and for two identical constant vectors the formula degenerates to 0/0 — the statistic is undefined, so no agreement value is asserted.

- **RPD (Ratio of Performance to Deviation):**
  <i>RPD = SD<sub>actual</sub> / RMSE</i>. A dimensionless metric. RPD &gt; 2.0 indicates an excellent predictive model. RPD &lt; 1.4 suggests the model has poor predictive capacity.

- **RPIQ (Ratio of Performance to InterQuartile Distance):**
  <i>RPIQ = (Q3 - Q1) / RMSE</i>. More robust than RPD when the original data is highly skewed (non-normal), which is common in soil properties like salinity.

- **SMAPE (Symmetric Mean Absolute Percentage Error):** Standardizes absolute errors as percentages, preventing extreme inflation when actual values are near zero. Where an observation and its prediction are *both* exactly zero the summand is 0/0; that term is defined as **0** (the usual convention) rather than dropped, so sMAPE is always averaged over the same sample count as every other metric in the table.

**Undefined metrics are reported as NA, never as infinity.** Every ratio metric above has an input configuration that zeroes its denominator, and in each case the quantity is genuinely undefined rather than infinitely good or bad:

| Metric | Undefined when | Interpretation |
|---|---|---|
| NSE | observations are constant (SST = 0) | there is no variance for the model to explain |
| NRMSE (mean) | mean of observations is 0 | a zero-mean (centred / anomaly) variable has no scale to normalise against |
| RPD, RPIQ | RMSE = 0 (perfect prediction) | spread-to-error ratios are undefined at zero error (Chang et al. 2001) |
| CCC | either vector is constant | the correlation term does not exist |

Reporting `Inf` or `-Inf` in these cells would propagate into the Model Performance table, the exported metrics CSV, and the pooled "Total (Combined)" diagnostics, where it reads as a real (and spectacular, or catastrophic) score. The tables show a blank/NA instead.

- **Moran's I (Spatial Autocorrelation of Residuals):**
  Evaluates whether the cross-validation errors are randomly distributed across the field. If Moran's I is significantly positive, errors are clustered (e.g., the model consistently underestimates in the north and overestimates in the south). This indicates the model failed to capture a macroscopic spatial trend, and an RK or RFK approach might be required. The neighbour structure is a **symmetric k-nearest-neighbour contiguity** (`k = 8`, a common default, capped at n − 1 for small samples), row-standardised (`spdep::nb2listw(style = "W")`). A kNN definition is scale-stable and avoids an arbitrary distance-band cutoff; because Moran's I is inherently sensitive to the neighbour definition, the reported value should be read as *the residual autocorrelation under this fixed 8-NN weighting*. Datasets containing duplicated coordinates are handled by separating the duplicates with a negligible, data-scaled jitter applied under a fixed internal seed, so Moran's I is exactly reproducible between runs and the global RNG state is left untouched (see Section 9.2 for why the displacement must scale with the coordinate magnitude rather than being a fixed constant). **Note:** The neighbour count `k` is hardcoded; see Section 9 for details on adjusting it.

- **Classification Performance (Accuracy, Cohen's Kappa, Weighted Kappa, MCC):** Observed and predicted values are binned into classes and compared as a confusion problem. For **agronomical classes**, the bin intervals are left-closed `[low, high)`, identical to the map classification (`terra::classify(..., right = FALSE)`), so a value lying exactly on a class boundary receives the same class in the performance tables and on the classified map. **Quartile** binning uses the conventional right-closed intervals on the observed quartiles.

**Residual semantics on CV failure:** All residual-based diagnostics (validation metrics, pooled CV residual variograms, Moran's I) are computed strictly from cross-validation residuals. If cross-validation fails for a locality, its residuals are left empty rather than silently substituted with model training residuals; CV and training residuals are never mixed.

### 5.1 Directional Variogram (Anisotropy Check)

Every variogram the prediction engines fit is **omnidirectional**: point pairs are binned by separation distance regardless of their orientation, which implicitly assumes the spatial structure is the same in every direction (geometric isotropy). Real fields often violate this — a floodplain, a prevailing wind, a tillage direction or a geological strike can give the variable a longer correlation range along one axis than across it.

The **Directional Variogram** panel in the Scientific Analysis tab tests that assumption. Semivariance is recomputed within four angular cones, at bearings of 0°, 45°, 90° and 135° measured *clockwise from north* (so N-S, NE-SW, E-W, NW-SE), each with a 22.5° half-angle tolerance. Those four cones tile the half-circle exactly once, so every point pair contributes to exactly one direction and the four curves are disjoint subsets of the omnidirectional one (their pair counts sum to it — an invariant pinned in the test suite). The panel can be computed on the **measured values** or on the run's **cross-validation residuals**; the residual view is the more relevant one for RK/RFK, where the residual — not the raw target — is what actually gets kriged.

*How to read it:* curves that rise to a common sill at clearly different distances indicate **geometric anisotropy** (direction-dependent range); curves that plateau at clearly different sills indicate **zonal anisotropy**. Roughly coincident curves support the isotropy assumption the engines make. Point coordinates are projected to a metric CRS before the cones are formed, because a bearing measured in degrees of longitude is not a bearing on the ground.

*Scope:* this panel is **strictly diagnostic**. No prediction path consumes it, and all interpolation engines remain omnidirectional, so nothing on any map changes as a result of what is displayed here. Where pronounced anisotropy is found, the honest interpretation is that the reported ranges are directional averages and the uncertainty surface is correspondingly smoothed across directions; acting on that finding requires an anisotropic model, which this version does not fit.

**Pooled "Total (Combined)" diagnostics CRS:** Each locality's CV object travels in its own local UTM zone, so pooling them for the combined residual variogram and pooled Moran's I requires one common metric CRS. The pooled set is reprojected to the **auto-UTM zone of the combined centroid** (the same zone rule used when projecting geographic input data). Web Mercator (EPSG:3857) is deliberately not used for this: its distances are inflated by 1/cos(latitude) (about 40% at 45° N), which would systematically stretch the pooled variogram's lag axis and distort the pooled Moran neighbour distances. Per-locality diagnostics are unaffected by this pooling step. If the per-locality CV objects cannot be combined (structural mismatch), the pooled diagnostics show their empty state rather than a partial pool that would misrepresent a subset as the combined result.

---

## 6. Residual Analysis

Quantitative metrics summarize global performance, but Residual Analysis visualizes localized model failures, helping identify spatial patterns in the error.

### 6.1 Interpolated Delta (Surface Diff)

This function interpolates the Actual measured data and the Predicted data (from your uploaded dataset) into two separate, continuous surfaces using your chosen geostatistical method, and then subtracts them: <i>Surface<sub>Actual</sub> - Surface<sub>Predicted</sub></i>.

**Use Case:** This maps the net difference between the two geostatistical surfaces. It reveals broader regional zones where your pre-calculated machine learning predictions consistently over-predict or under-predict the true spatial distribution of the target variable in the soil.

### 6.2 Point Errors and Interpolated Point Errors

The discrete error at each exact sampling location (<i>O<sub>i</sub> - P<sub>i</sub></i>, or Actual - Predicted) is displayed directly as coloured point markers (the Map Viewer's Point Residuals panel; exported as the Point Error Map). Additionally, an Inverse Distance Weighting (IDW) interpolation is run purely on those error values, producing the Interpolated Point Errors Map available in the Export Panel. The uploaded point coordinates are projected into the locality's working metric (UTM) CRS before this IDW step, so the error surface is valid for geographic (lat/lon) uploads exactly as for projected ones.

**Use Case:** This creates a map showing the spatial structure of local model failure (the model produced the uploaded parameter predictions, not the spatial interpolation model). Hotspots on this map indicate distinct zones in the field where the current prediction model cannot capture the true soil variability.

---

## 7. Uncertainty Analysis & Confidence Mapping

While interpolation provides the "best guess" for a soil property, Uncertainty Analysis quantifies the reliability of that guess at every pixel in the field. This feature is exclusively available for Kriging-based methods (OK, RK, RFK, CK), as they provide a formal statistical error model.

### 7.1 Theoretical Basis
In Kriging, the uncertainty is a function of the **Spatial Configuration** of your samples and the **Variogram Model**.
* **Geometric Influence**: Uncertainty is at its lowest at the exact location of a sample point and increases as you move into "unsampled" territory.
* **Variogram Influence**: A high **Nugget** or a short **Range** in the fitted model will result in higher overall uncertainty across the entire generated map.

### 7.2 Uncertainty Metrics
The application allows you to toggle between two primary metrics for visualizing spatial risk:
* **Kriging Variance**: Represents the theoretical mean squared error of the prediction, expressed in the *squared* units of the variable (e.g., (mg/kg)² for potassium). Because of the squaring, its legend values are much larger than the variable's typical range; an SE of 130 mg/kg corresponds to a variance of ~17,000 (mg/kg)². It is particularly useful for comparing the relative stability and fit of different variogram models.
* **Standard Error**: The square root of the variance, expressed in the same units as your primary soil parameter (e.g., %TN or pH units).
* **Use Case**: This is the most practical metric for agronomists. For example, if a point predicts **2.0% Nitrogen** with a **Standard Error of 0.2**, you can be approximately 95% confident the true value lies between 1.6% and 2.4%.
* **Display note**: Uncertainty layers are always rendered with a continuous color scale, and the map legend states the metric and its unit (e.g., "Variance: K (mg/kg)²"). Agronomic and Binned classification breaks are defined for concentrations and are therefore not applied to uncertainty surfaces.

### 7.3 Hybrid Model Uncertainty (RK & RFK)
For advanced models (Regression Kriging and Random Forest Kriging), the uncertainty is "Combined" to provide a rigorous error surface:
* **Trend Uncertainty**: Captures the error in the relationship between your soil target and environmental predictors, such as how well Elevation explains Nitrogen levels.
* **Residual Uncertainty**: Captures the Kriging error of the remaining unexplained variation.
* **Total Map**: The final uncertainty map for RK/RFK is the mathematical sum of both the trend variance and the residual kriging variance, providing a comprehensive "Full-Model" error surface.

**Approximation: the "Total" is an additive sum (zero trend–residual covariance).** For both RK and RFK the combined variance is `Var(trend) + Var(residual kriging)`, which assumes the trend-estimation error and the kriged-residual error are independent (zero cross-covariance). This is the conventional two-step regression-kriging approximation: because the trend is fitted first and its residuals are kriged separately, the two error terms are treated as additive. The exact joint treatment, in which the covariance between the mean-surface estimate and the residual prediction is carried explicitly, is Kriging with External Drift (KED) / Universal Kriging, which is not what the two-step engine does. In practice the additive form is standard and slightly **conservative-to-approximate** rather than exact; interpret the Total surface accordingly.

**The trend-uncertainty term differs between RK and RFK.** For RK, the trend component is the parametric standard error of the linear-model prediction (`se.fit²`), i.e. the variance of the estimated mean surface. For RFK, no closed-form standard error exists, so the trend component is user-selectable via the **RFK Uncertainty Method** control:
* **Infinitesimal Jackknife (default, calibrated)**, the estimator of Wager, Hastie & Efron (2014), with the Monte-Carlo bias correction. It is the random-forest analogue of RK's `se.fit²`: the sampling variance of the *ensemble-mean* prediction, and a better-calibrated trend-uncertainty term than the raw between-tree spread. This is the default.
* **Ensemble spread (fast)**, the **between-tree variance** of the Random Forest (the spread of the individual trees' predictions around the ensemble mean). This reflects model *instability* rather than a formal predictive variance and typically **understates** the true predictive uncertainty; treat it as a relative "where is the model least stable?" measure, not an absolute 95%-interval measure.

Switching methods changes **only** the RFK uncertainty surface; the RFK prediction map (`var1.pred`) and the reported cross-validation metrics are identical either way.

**Approximation: the kriged covariate surfaces are treated as error-free.** The trend term of an RK/RFK prediction is evaluated at grid cells using covariate values that are themselves *kriging estimates* (Section 8.1), produced by interpolating each auxiliary variable onto the prediction grid. Those covariate surfaces have their own kriging variance, but it is discarded: the trend variance carried into the Total is the linear model's `se.fit²` (or the random forest's jackknife variance) computed **as if the covariate values at each grid cell were measured without error**. The propagated term that a full error budget would add — the sensitivity of the trend to covariate error, roughly `Σ (∂trend/∂xⱼ)² · Var(x̂ⱼ)` plus the covariate cross-terms — is therefore missing, and the reported RK/RFK uncertainty is **optimistic away from the sample points**, where the covariate surfaces are themselves least certain. The effect is smallest near samples (covariate kriging variance approaches its nugget there) and grows into unsampled territory, i.e. exactly where the uncertainty map matters most. This is standard practice for two-step regression kriging with interpolated covariates, and it is the reason the Total surface should be read as a *relative* reliability map rather than as a calibrated absolute interval. It does not apply where covariates come from an exhaustive raster (e.g. a DEM), which carries no interpolation error of this kind.

**Covariate surfaces are fitted independently of target missingness.** The auxiliary covariate surfaces that RK/RFK evaluate their trend on are kriged from the **full covariate-complete point set** (co-located points deduplicated, samples with a missing target retained), and their variogram lag width and cutoff come from that same set. The target model, by contrast, is fitted on the point set for its own surface (target-`NA` rows removed first, then deduplicated), with lags derived from *that* set. The two lag definitions can therefore differ slightly. This is deliberate: a sample lacking a laboratory value for the target still carries valid covariate measurements, and discarding it would needlessly weaken the covariate surfaces. Because the bounding-box diagonal drives the cutoff, the difference is usually negligible; it is documented here because the covariate surfaces are not fitted on the same support as the target.

Both maps remain labeled "Variance"/"SE" for UI consistency, and RK vs RFK magnitudes are still not directly comparable. Quantile Regression Forests are deliberately **not** offered here: a QRF predictive interval already contains the irreducible residual scatter that this engine models separately via residual kriging, so summing the two under the additive trend + residual decomposition would double-count that variance.

---

## 8. Data Analytics & PCA Protocols

### 8.1 Multicollinearity Filter (PCA & Spatial Models)

The system actively guards against severe multicollinearity which can destabilize multivariate mathematical models. 
* **PCA Module:** Before executing standard PCA, it scans the numerical matrix for pairwise correlations > 0.95. If detected, it actively halts the execution and alerts the user, requiring a manual override or parameter drop. This is a critical statistical guardrail that prevents severe distortion of the loading vectors.
* **Geostatistical Engine (RK, RFK, CK):** Before launching multivariate spatial interpolations (Regression Kriging, Random Forest Kriging, or Co-Kriging), the system performs a Variance Inflation Factor (VIF) check on the selected auxiliary variables. If variables with a VIF > 10 are detected, an interactive modal halts the process, advising the user to "Auto-Drop and Continue" or force execution. If auto-dropped, the variables are strictly purged from both the interpolation algorithms and downstream diagnostic reports (e.g., Variable Importance plots). The modal's choice is honoured end-to-end: choosing to keep collinear covariates disables iterative pruning inside the engines entirely (the collinearity is still reported, just not acted upon), identically in the interpolation and classification paths. Independently of that choice, **numerically constant covariates are always excluded** before the VIF iteration — a constant carries no regression information and would make the correlation matrix singular. "Constant" is judged **relative to each covariate's own magnitude** (standard deviation below ~10⁻⁸ of the column's largest absolute value), never against an absolute variance floor: an absolute threshold silently discards legitimately small-unit covariates — clay expressed as a 0–1 fraction, normalized indices, ratios, or variables in km or Mg can all carry real signal at variances below 10⁻⁶ — and, because constants are dropped even under "Keep All", the user's override could not rescue them. The relative criterion drops only columns whose values differ in noise digits, which is the case the correlation matrix genuinely cannot support.
  * *Note on covariate surfaces:* the gate is resolved **before** the auxiliary covariates are interpolated onto the prediction grid, so a covariate the filter drops is never kriged. The gate is evaluated separately for the Actual and Predicted surfaces, on exactly the point sets each engine fits (target-`NA` rows removed, then co-located points deduplicated), because the two surfaces can retain different samples and therefore different covariate sets; the grid then carries the union of the two retained sets. This changes only which surfaces are built, never the fitted model or its predictions.
  * *Note on Co-Kriging:* CK applies the same gate as RK/RFK on the same point sets, but it does **not** consume kriged covariate grids — it predicts the target and its covariates jointly through the LMC — so no covariate surfaces are built for it. The gate matters most here: the LMC fits a direct variogram for every covariate **plus every cross-variogram**, and collinear covariates drive the coregionalization matrices toward singularity, which makes `fit.lmc()` fail and silently drops the run into the Ordinary Kriging fallback. A CK run whose covariates trip the VIF threshold will therefore give different (and better-conditioned) results depending on the Drop/Keep answer.
  * *Note on cross-validation:* the VIF/collinearity prune is run **once on the full auxiliary set before cross-validation**, not re-derived inside each CV fold. This is deliberate and does not inflate the reported CV skill: the filter is **unsupervised**: it inspects only the covariate–covariate correlation matrix and never sees the target variable, so, unlike supervised feature selection, it introduces no leakage or optimistic bias when performed outside the resampling loop. Keeping the retained variable set fixed across folds also makes the model definition and its diagnostics stable and interpretable.
* **Classification Suite:** the same iterative-VIF engine screens the selected numeric covariates before a classification run, with a live warning under the covariate picker and the same Drop/Keep modal at run time. The threshold is method-aware: 10 for most learners, tightening to 5 when Random Forest is selected (Section 10.8 explains why moderate collinearity already dilutes permutation importance).
* **Context sensitivity (both gates and the Predictor Ranks):** correlation structure is scope-dependent — two covariates can be nearly collinear across the whole dataset yet independent within one locality (or vice versa, a Simpson's-paradox-type reversal). All three screens therefore run on the data the model will actually fit: the interpolation gate and the sidebar **Predictor Ranks (Correlation)** list use the localities selected in the Context panel (the ranks are stamped with their scope and sample count), and the classification gate uses the module's resolved spatial scope (localities and/or polygons). A recorded Drop/Keep decision is reset whenever the method, the covariate set, or the locality selection changes.

---

## 9. Advanced Parameter Customization (Hardcoded Values)

Certain statistical parameters are hardcoded to ensure smooth automated execution but can be modified directly in the source code by advanced users:

### 9.1 Cross-Validation Random Seed
To ensure "scientific reproducibility" across identical runs, every cross-validation fold assignment, both the random k-fold partition and the Spatial Block k-means clustering, uses a fixed seed (`12345`).
* **Where to change:** `make_cv_folds` inside `spatial_metrics.R` is the single source of fold assignments for all engines; `perform_kriging_loocv` additionally seeds its per-fold Random Forest draws (RFK).
* **How to change:** Locate `set.seed(12345)` and either change the integer or comment out the line to allow fully randomized folds on every execution.

### 9.2 Moran's I Neighbour Count (k)
The Moran's I residual-autocorrelation test defines neighbours via a symmetric k-nearest-neighbour contiguity with a hardcoded `k = 8` (capped at n − 1 for small samples). This replaces the earlier arbitrary distance-band heuristic (`mean-NN × 5`), which, being a wide band, diluted local autocorrelation toward zero.
* **Where to change:** `calc_moran` function inside `spatial_metrics.R`.
* **How to change:** Locate `k_nn <- min(8L, nrow(coords) - 1L)` and adjust the `8L` if you expect the range of residual spatial autocorrelation to be captured by a smaller (more local) or larger neighbourhood.

**Duplicate-coordinate handling.** The k-nearest-neighbour search cannot accept exactly co-located points, so co-located coordinates are separated by a tiny seeded displacement before the neighbour graph is built (the RNG is sandboxed, so Moran's I stays bit-reproducible). The displacement is **scaled to the data**: 1e-9 of the coordinate span, with a floor of 1e-12 of the coordinate magnitude and an absolute floor of 1e-8 map units. A fixed absolute displacement is unsafe at projected coordinate magnitudes — at a UTM northing of ~4.5 × 10⁶ the spacing between representable doubles is ~1 × 10⁻⁹, so a 1e-8 nudge is only about ten representable steps and can round straight back onto the original value, silently leaving duplicates in the data. All three floors remain orders of magnitude below any real sample spacing, so the neighbour definition itself is unaffected.

### 9.3 Cross-Validation Strategy and Thresholds
The **Cross-Validation Strategy** (Auto / Standard LOOCV / Spatial Block CV) is selectable directly in the UI; no code edit is required to switch strategies (see Section 5). The associated thresholds are hardcoded in one place:
* **Where to change:** `resolve_cv_plan` inside `spatial_metrics.R`.
* **How to change:** Under **Auto**, the engine uses LOOCV when the number of observations is 50 or fewer and switches to random 10-Fold above 50; edit the `n > 50` test to move that boundary. **Spatial Block** degrades to LOOCV below the `CV_BLOCK_MIN_N` constant (default `30`). The number of spatial blocks / random folds is the `k = 10L` value; change it to use a different fold count.

### 9.4 Map-Styling Class Breaks (Jenks / K-Means)
Class limits for the Agronomical styling algorithms are computed by `calc_class_breaks` (`spatial_pipeline.R`) rather than a bare `classInt::classIntervals` call, for two statistical reasons:
* **Reproducibility.** Both `classInt` styles draw random numbers internally (K-Means starts; Jenks silently switches to an *unseeded* 3,000-value sample above n = 3,000), so the same map could legitimately produce different class limits on every restyle. `calc_class_breaks` runs under the app's standard two-sided seed sandbox (seed `12345`, caller RNG state restored), making the breaks bit-reproducible.
* **Tractability.** The exact Jenks algorithm is O(n²) and takes several blocking seconds on raster-sized vectors; breaks are therefore estimated on a seeded subsample capped at `max_n = 5000` cells. Estimating classification breaks from a sample is the standard practice in GIS software; the class *areas* reported in the Scientific Analysis tab are always computed by classifying the **full-resolution** raster with those breaks, never a resampled one. (The interactive Leaflet viewer may display a mean-aggregated preview above ~500,000 cells; this is display-only.)
* **Where to change:** the `max_n = 5000L` default of `calc_class_breaks`, and the `LEAFLET_DISPLAY_MAX_CELLS <- 5e5` viewer cap in `server_setup.R`.
* **Commit semantics.** Agronomical sub-settings (algorithm, class count, supervised limits) are staged in the sidebar and only take effect when **APPLY TO MAPS & STATS** is pressed; the break computation itself is unchanged, so an applied configuration yields bit-identical breaks, class areas, and kappa statistics to the former live-recompute behaviour — it just runs once per commit instead of once per input tick.

---

## 10. Supervised Classification Suite

The Classification Suite (Tab 6) is a separate modelling paradigm from the interpolation engines: instead of predicting a continuous surface and thresholding it into agronomic zones (Section 5), it trains a **predictive multiclass classifier** directly on a categorical target using co-sampled covariates, in the tradition of categorical Digital Soil Mapping. The engine lives in `classif_helpers.R` and is fully decoupled from the Shiny UI.

### 10.1 Targets and Learners

The target is either an existing categorical column (soil class, land-use label) or a continuous variable discretised into ordered classes with `classInt` break styles (quantile, equal-interval, or Jenks), using left-closed `[low, high)` intervals for consistency with the app's agronomic-class convention. **Numeric columns are never auto-detected as categorical**: coarse-resolution environmental covariates (e.g. climate-raster precipitation metrics) legitimately carry very few distinct values, and treating an ordered quantity as nominal both dummy-encodes it in the preprocessing recipe and switches its grid transfer from kriging to nearest-neighbour assignment. Numeric class codes should be recoded to text or run through the binned-target mode.

Three learners are offered behind a common tidymodels (`parsnip`/`workflows`) backbone:
* **Multinomial logistic regression** (`nnet`), the standard parametric baseline. For **two-class targets** the engine substitutes binomial logistic regression (`glm`): the multinomial model with K = 2 reduces exactly to it, and parsnip's `multinom_reg`/`nnet` wrapper produces malformed probability output in the binary case. The substitution is statistically equivalent at the default `penalty = 0`; penalty tuning is skipped for binary targets.
* **Random Forest** (`ranger`, probability forest), the de-facto standard in categorical DSM;
* **Extreme Gradient Boosting** (`xgboost`).

All three share one preprocessing recipe: novel-level absorption, median/mode imputation, dummy encoding of categorical covariates, zero-variance removal, and standardisation of numeric predictors (monotonic, so it aids multinomial convergence without affecting the tree learners). Hyperparameter tuning is optional (None / Light / Full); when enabled, a space-filling grid is evaluated over the same cross-validation folds and the best set (by accuracy) is refitted. Because the tuned parameters are then evaluated on the same folds (not nested CV), tuned-depth metrics are mildly optimistic; the default depth **None** fits fixed defaults and carries no such optimism.

### 10.2 Spatial Cross-Validation and Pooled Metrics

The default validation is **spatial blocked CV** (`spatialsample::spatial_clustering_cv`, k-means on the projected coordinates), for the same reason Spatial Block CV exists in Section 5: random folds place a test point's near-neighbours in the training set, so under spatial autocorrelation they over-state skill (Roberts et al. 2017; Ploton et al. 2020). Standard class-stratified random k-fold remains available for in-domain estimates. Fold assignment uses the app-wide fixed seed (`12345`) under the two-sided RNG sandbox.

Predictions are collected **out-of-fold and pooled** before any metric is computed: each fold's model predicts hard classes *and* full class-probability vectors on its held-out points, and the metrics are evaluated once on the pooled set. Pooling avoids the undefined per-fold macro-metrics that arise when a spatially contiguous fold happens to contain a single class, and guarantees the probability metrics always have every class present.

Reported metrics: overall accuracy, Cohen's kappa, balanced accuracy, macro-averaged precision/recall/F1 (so minority classes are not masked), multiclass ROC AUC (Hand & Till 2001), multiclass log-loss, and the Brier score. The confusion matrix is accompanied by per-class **producer accuracy** (recall; omission-error complement) and **user accuracy** (precision; commission-error complement), the standard per-class report in soil and land-cover classification.

### 10.3 Prediction Uncertainty: Normalised Shannon Entropy

The classifier analogue of the kriging-variance map is the normalised Shannon entropy of the predicted class probabilities at each grid cell:

<div style="text-align:center;"><i>H* = - &sum;<sub>k</sub> p<sub>k</sub> ln p<sub>k</sub> / ln K</i></div>

where *K* is the number of classes. *H\** = 0 means the model is certain of a single class; *H\** = 1 means the probabilities are uniform (maximal confusion). Entropy is a property of the model's probability output, not a formal error model: read it as a relative "where is the classifier least decided?" surface.

### 10.4 Covariate-Surface Approximations

The classifier is trained at sample locations, but map prediction requires covariate values at every grid cell. Monolith derives these from the samples themselves ("classify from co-sampled covariates"), not from exhaustive rasters:
* **Numeric covariates** are interpolated to the grid with the same ordinary-kriging covariate builder used by RK/RFK (`krige_covariates`, IDW fallback).
* **Categorical covariates** cannot be kriged; each grid cell inherits the class of its nearest training point (first-nearest-neighbour).

Both transfers are documented approximations whose smoothing/blocking error propagates into the classified map, the probability surfaces, and the entropy surface. Where wall-to-wall covariate rasters exist (DEM derivatives, satellite bands), sampling them at grid nodes outside the app remains the more rigorous DSM design; the cross-validated performance metrics, computed strictly at sample locations, are unaffected by this approximation.

### 10.5 Spatial Scope and the Prediction Domain

A classification run can be restricted to a **spatial scope** before anything is fitted:
* **Localities** — any subset of the mapped locality/grouping column (defaulting to the sidebar Context panel selection);
* **Polygons** — shapes drawn on the map and/or an uploaded shapefile, either intersected with the selected localities or used alone.

Scoping is applied *before* target construction, so binned-target break points (quantiles, Jenks) are derived from the scoped data only, and all cross-validation folds, metrics, and hyperparameter searches see exclusively in-scope points.

The prediction domain is built from the same scope resolution as a union of **per-locality boundaries** of the scoped points, intersected with or replaced by the polygon union when a polygon scope is active. Using per-locality boundaries rather than one hull around everything keeps the classifier from predicting into unsampled corridors between localities — regions where the covariate-surface approximations of Section 10.4 would be pure extrapolation.

The boundary construction follows the **same geometric conventions as the interpolation engines** (Section 3), but is configured by the suite's own Spatial Scope controls (Boundary Type: Concave Hull / Convex Hull / Wrapped / Strict Point Buffer; Buffer Logic; Grid Resolution) — independent of the interpolation sidebar, which is hidden while the Classification Suite tab is active. The settings apply per locality in scope. Wrapped mode buffers each locality's concave hull — with the fixed distance, or dynamically from that locality's mean nearest-neighbour spacing (× 0.5 × the generic 2.0 multiplier, clamped to [5, 2000] m; the *method-specific* multipliers of Section 3.2 are an interpolation-variance concept and do not apply to a classifier). Strict mode unions fixed-radius buffers around the sample points. Grid resolution is either **Fixed** (the manual value) or **Auto**, deriving a resolution targeting ≈50 000 cells inside the domain (clamped to [5, 1000] m, with an additional ≈4 M-cell bounding-box cap for far-apart localities). Covariate kriging (Section 10.4) fills the whole buffered domain, so buffered boundaries entail modest covariate extrapolation beyond the outermost samples — the same trade-off as the interpolation engines' buffered modes.

### 10.6 Hyperparameter Tuning

The **Hyperparameter tuning** selector (Tab 6, next to the learner) sets how hard Monolith searches each learner's hyperparameter space before the metrics of Section 10.2 are computed. Three depths ship:

* **None (fixed defaults)** — the default. Nothing is tuned; each learner is fit at the hardcoded defaults (multinomial: `penalty = 0`; Random Forest: 500 trees with `ranger`'s built-in `mtry`/`min_n`; XGBoost: 500 trees, `tree_depth = 6`, `learn_rate = 0.05`, `min_n = 2`). Fast, deterministic, and free of the optimism noted below.
* **Light** — a 10-point space-filling grid over a small set of high-leverage parameters: `penalty` (multinomial), `mtry` + `min_n` (Random Forest), `tree_depth` + `learn_rate` (XGBoost).
* **Full** — a 30-point grid over a wider set: the same parameters for the multinomial and Random Forest learners, and for XGBoost additionally `mtry`, `min_n`, `loss_reduction`, and `sample_size`.

When a depth other than None is chosen, the grid is by default evaluated over the *same* cross-validation folds and the best combination (by accuracy) is refitted; because tuning and evaluation share those folds (not nested CV), Light/Full metrics are mildly optimistic relative to None.

**The tuning search always uses the run's own resampling strategy.** Both the cross-validation loop and the final full-data fit build their tuning folds with the strategy selected for the run — spatial clustering under Spatial CV, class-stratified random folds under Standard CV. This matters because the final model's hyperparameters are what the class map, the entropy surface, the permutation importance ranking and the exported `.rds` bundle are all built from: tuning those under random folds while reporting spatial-CV metrics would select the model that performs best under exactly the near-neighbour leakage spatial CV exists to remove, and every downstream product would inherit that choice while the metrics table advertised a leakage-free estimate. Runs at depth **None** are unaffected (nothing is tuned, so the fold strategy never enters the final fit).

**Nested cross-validation** (the *Use nested CV (slower)* checkbox, visible only when a tuning depth is active) removes that optimism. Instead of one global selection, each **outer** fold re-runs the full grid search on **5 inner folds** built exclusively from that fold's analysis rows — fold construction, grid evaluation, and winner selection never see the outer held-out data — and the finalized workflow then predicts the held-out fold. The pooled metrics therefore estimate the generalisation error of the *entire procedure including the hyperparameter search* (Varma & Simon 2006; Cawley & Talbot 2010). Inner folds reuse the outer resampling strategy (spatial clustering inside spatial CV), so the inner selection faces the same leakage regime as the outer estimate — the spatial analogue of the recommendation in Schratz et al. (2019) for tuning under spatial autocorrelation. Consequences worth knowing:

* No single "best" hyperparameter set exists under nesting; the per-outer-fold winners are collected (`nested_params`, one row per fold) and their fold-to-fold agreement is itself a stability diagnostic.
* The **deployed/exported model** is still tuned once on all training data (standard practice: nested CV estimates the procedure's skill; the final model uses everything), and the model bundle records *that* model's own hyperparameters.
* Cost multiplies by roughly the number of inner folds (~5x the tuning work). Depth None is unaffected by the checkbox.

The depth/parameter/grid-size mapping lives in `.classif_tuning_registry()` in `classif_helpers.R` and is the single place to change tuning behaviour: adjust a grid size, add or drop a tuned parameter for a learner, or register a new depth key. The change is purely additive and flows automatically into both the UI selector and the fitting path.

### 10.7 Per-Area Performance

When the scope contains more than one locality (or polygon, in polygons-only mode), the **Performance by Area** table splits the pooled out-of-fold predictions by area and reports n, overall accuracy, Cohen's kappa, balanced accuracy, and macro F1 per area, plus a Total row that reproduces the pooled headline metrics. Two caveats apply:
* The fold structure is global: spatial clusters need not respect locality borders, so a per-area subset is an *evaluation* slice of one jointly trained model, not an independently validated per-area model (unlike the interpolation engines, which fit one model per locality).
* Small areas yield unstable estimates, and any metric undefined for an area (e.g. a class that never occurs there) is reported as NA rather than silently imputed. Probability metrics are deliberately omitted from the per-area table for the same instability reason.

### 10.8 Permutation Feature Importance

Every run reports **model-agnostic permutation importance** (Breiman 2001; Fisher, Rudin & Dominici 2019): one covariate at a time is randomly shuffled — severing its association with the target while preserving its marginal distribution — the final model re-predicts, and the increase in **multiclass log-loss** over the unpermuted baseline is recorded (mean of 5 shuffles under the fixed seed). Log-loss is used rather than accuracy because it consumes the full probability vector, so it registers importance even when a permutation rarely flips the arg-max class. Because the measure is computed on the raw covariates through the entire fitted workflow, it is directly comparable across all three learners, unlike engine-native measures (ranger impurity, xgboost gain, multinomial coefficients), which live on different scales.

**Evaluation design (selectable).** The permutation can be scored on either of two row sets, chosen with *Feature importance scored on* in the module sidebar:

* **Out-of-fold (default).** Each cross-validation fold's own fitted model permutes the predictors of the rows that fold never saw, and the per-fold log-loss increases are pooled by fold size — which reproduces exactly the increase that one evaluation over the whole out-of-fold set would give. This measures importance under the same honest design as the reported accuracy, and it costs no extra model fits: the fold models already exist inside the CV loop, and the total number of rows predicted is the same as in the training-row design. Caveat: a predictor is shuffled *within* each assessment set, so with very small folds the shuffle decorrelates it less thoroughly and importance is slightly understated.
* **Training rows.** The final model scored on the rows it was fitted on — the conventional default (cf. `vip::vi_permute`). Rankings are informative, but absolute magnitudes lean optimistic for flexible learners that partially memorise their training set.

Both designs report the same quantity (increase in multiclass log-loss) on different rows, and the design in force is written onto the plot axis, into the exported metrics CSV, and into the results table, because an out-of-fold and a training-row importance are **not comparable numbers**. Negative values indicate pure noise covariates under either design.

One further caveat:
* **Correlated covariates split their importance** between them (either can substitute for the other; Strobl et al. 2008), which is exactly why the multicollinearity gate of Section 8.1 runs first: on a VIF-cleaned covariate set, the reported shares are far more interpretable. The displayed *share (%)* renormalises the positive importances to 100. Because this dilution sets in well below the classical VIF > 10 collinearity cutoff, the classification module's screen is **method-aware**: when Random Forest is selected the advisory threshold tightens to **VIF > 5** (the conventional "moderate collinearity" bound, e.g. James et al. 2013; O'Brien 2007 discusses why such rules are advisory, not absolute). The gate remains a user decision — predictions are largely insensitive to keeping the covariates; only the importance interpretation suffers.

### 10.9 No-Covariate Baselines and Covariate Lift

To answer "did the covariates actually buy anything?", every run scores two reference models on the **same cross-validation folds** as the covariate model:
* **Majority-class baseline** — always predict the most frequent class; its accuracy is the no-information rate, and its kappa is 0 by construction.
* **Spatial 1-NN baseline** — each held-out point receives the class of its nearest analysis-set point (Euclidean distance, projected coordinates): the categorical analogue of nearest-neighbour/Thiessen interpolation, i.e. what pure spatial proximity achieves with no covariate information. Under spatial blocked CV this is a demanding, honest baseline: it must transfer across fold boundaries exactly like the model.

**Covariate lift** is the accuracy difference between the covariate model and the spatial baseline (reported in accuracy points on identical folds). Statistical significance of the paired improvement is assessed with **McNemar's test** (continuity-corrected) on the discordant pairs — the standard test for comparing two classifiers evaluated on the same samples (Dietterich 1998). A non-significant lift is a substantive scientific finding: the covariates add little beyond spatial position, and a simpler spatial model (or better covariates) should be considered. The test is reported as NA when no discordant pairs exist.

### 10.10 Confidence Thresholding (Abstention)

The predicted-class map supports a **confidence threshold** implementing the classical reject option (Chow 1970): grid cells whose maximum class probability falls below the threshold are assigned an explicit **Unclassified** category (rendered grey, with its own row in the area table) instead of a weak arg-max guess — e.g. a 34/33/33% cell is a statement of ignorance, not a prediction. Abstained regions are the natural candidates for additional field sampling.

The threshold is applied at rasterisation time in the main session: it re-classifies the existing probability surface without re-fitting anything, so it can be explored interactively, and the exported class GeoTIFF honours the current setting (probability and entropy rasters are never masked). Thresholds at or below 1/K cannot fire, since the maximum of a K-class probability vector is at least 1/K. Alongside the map, a CV-based readout reports the **coverage/selective-accuracy trade-off** at the chosen threshold — the fraction of pooled out-of-fold points that would be retained and the accuracy among them (selective risk; retained accuracy should sit at or above the overall accuracy). CV metrics themselves are always computed on all points, without abstention.

### 10.11 Class-Imbalance Weighting

The optional **Balance classes** switch applies inverse-frequency ("balanced") case weights, *w<sub>c</sub> = n / (K · n<sub>c</sub>)*, so each class contributes equally to the training loss regardless of its sample count (King & Zeng 2001; the `class_weight="balanced"` convention) — countering the tendency of accuracy-driven learners to ignore rare but important classes. Design decisions:
* Weights affect **fitting only**; all reported metrics remain unweighted (balanced accuracy and the macro averages already expose minority-class performance). Expect weighted runs to trade some overall accuracy for better minority-class recall.
* In the out-of-fold loop, weights are **recomputed from each fold's analysis rows**, so no held-out class-prevalence information enters a fold's fit; the hyperparameter-tuning pass (whose folds `tune_grid` controls) uses whole-training-set weights, a negligible deviation for a K-number summary.
* Weights apply to Random Forest and XGBoost. The multinomial learner's engine does not accept case weights; the run badge then reports "weights unsupported (unweighted)" instead of silently pretending.
* **SMOTE-style synthetic oversampling is deliberately not offered**: synthetic minority samples carry no valid spatial position, so they fabricate autocorrelation structure and break the leakage guarantees of spatial blocked CV (any resampling would also have to happen strictly inside each fold). Field-collecting more minority-class samples remains the only real remedy; weighting is the honest statistical stopgap.

### 10.12 Trained-Model Export

The **Download Model (.rds)** button saves the final fitted tidymodels workflow with its training metadata (method, covariates, class levels, weight usage, tuning depth and selected hyperparameters, projected CRS, n, timestamp, and the R/parsnip/engine package versions). The bundle can be loaded in any R session with compatible package versions and applied to new covariate data without retraining:

```r
b <- readRDS("classification_model_rf_20260710.rds")
predict(b$workflow, new_data, type = "prob")   # or type = "class"
```

`new_data` must contain the covariate columns listed in `b$predictors`; the preprocessing recipe (imputation, dummy encoding, normalisation) is embedded in the workflow and replays automatically. Predictions for the exported model are identical to the in-app surface predictions (same fitted object). Note that .rds serialisation is version-sensitive for the xgboost engine: reuse the bundle under the package versions recorded in `b$versions`.
