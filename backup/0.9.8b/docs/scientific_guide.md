# Scientific & Analytical Methodology Guide (v0.9.8b)

The Monolith Spatial Analysis Dashboard provides agronomists, soil scientists, and geostatisticians with a toolkit for exploring spatial variability. This guide briefly elaborates on the mathematical intuition, structural assumptions, and practical applications of the underlying spatial methods and evaluation metrics.

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

**Algorithmic Stability (Epsilon-Nugget):** For variables with extremely low variance (e.g., specific micro-nutrients like Iron), the codebase strictly enforces a tiny nugget (`1e-6 * initial_sill`) when the initial empirical nugget is exactly zero. This prevents singular matrix inversion failures during Kriging.

### 1.2 Regression Kriging (RK)

**Mathematical Intuition:** RK decomposes the spatial variable into a deterministic trend and a stochastic residual.
<br><br>
<div style="text-align:center;"><i>Z(x) = m(x) + e(x)</i></div>
<br>
First, a generalized linear model fits the trend <i>m(x)</i> using secondary covariates (e.g., elevation, NDVI). The residuals <i>e(x)</i> represent the spatially correlated variation not explained by the trend. Ordinary Kriging is then applied to the residuals. The final prediction sums the trend and kriged residuals.

**Agronomical Example:** Mapping Soil Organic Carbon (SOC). Elevation and soil moisture (derived from satellite imagery) are used to predict the baseline SOC trend. RK then kriges the residuals to adjust for localized organic matter accumulations missed by the remote sensing data.

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

---

## 2. Grid Resolution Logic

Grid resolution (cell size) determines the size of each pixel in your final mapped surface. It represents the spatial support of your model. Selecting the correct resolution ensures your map captures the true scale of variation without overspecifying or losing local detail.

### 2.1 The Spatial Support Metric (FNN Nearest-Neighbor)
When resolution is determined dynamically, the engine utilizes a **Fast Nearest Neighbor (FNN)** algorithm to analyze the point density of the samples. The recommended spatial resolution ($R_{rec}$) is defined as exactly half the expected nearest-neighbor distance:
<br><br>
<div style="text-align:center;"><i>R<sub>rec</sub> = 0.5 &times; Expected Nearest-Neighbor Distance (h<sub>NN</sub>)</i></div>
<br>
This ensures that the mapped grid spacing is directly proportional to your physical sampling spacing, preventing the generation of artificial, unmeasured detail.

### 2.2 Auto (Per Locality)
In this mode, the FNN point-density analysis is run **independently** for each locality. If Locality A is densely sampled, it receives a fine resolution (e.g. 50 m); if Locality B is sparsely sampled, it receives a coarse resolution (e.g. 150 m). This is the most geostatistically rigorous approach for multi-site studies.

### 2.3 Auto (Global)
In this mode, the FNN analysis evaluates the point density of the **entire dataset** as a single spatial unit. A uniform, global resolution value is calculated and applied to all selected localities, ensuring map pixels match in size across all generated outputs.

### 2.4 Fixed (Manual)
The user manually overrides the dynamic recommendations by adjusting a slider input (e.g. to 50 m or 20 m). This is useful when the researcher wants to force a specific, high-resolution pixel size for cartographic consistency, premium rendering, or printing.

---

## 3. Spatial Boundary & Dynamic Buffering Systems

Defining the boundary (or clipping mask) of your interpolation is critical to determine the physical extent of predictions. Unconstrained interpolation can lead to high-risk extrapolation. Monolith v0.9.6b implements an automated **Dynamic Buffering** engine that scales the boundary padding of each locality based on the active resolution and the mathematical constraints of the selected interpolation method.

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
The dashboard employs an automated least-squares fitting algorithm to establish a baseline. However, automated fits can get trapped in local minima or overfit to outliers at high lag distances. You may switch to **Manual Tuning** to prioritize the fit at shorter lags, which have the greatest impact on kriging weights.

---

### 4.2 IDW Optimization
* **Logic**: The application performs an automated search to find the optimal **Distance Power** that minimizes the spatial interpolation error for each specific locality.
* **Optimization Engine**: The system executes a Leave-One-Out Cross-Validation (LOOCV) loop, testing power factors ranging from **0.5 to 5.0**.
* **High-Precision Mode**: For larger datasets (typically > 50 points), the engine automatically switches to **5-fold Cross-Validation** to maintain computational efficiency without sacrificing statistical reliability.
* **Local Adaptation**: As the soil variability is site-specific, the "Optimize" button calculates a unique power factor for every selected locality. 

### 4.3 TPS Optimization
* **Logic**: The software optimizes the **Smoothing Parameter** to achieve the ideal mathematical balance between honoring every individual data point and creating a generalized regional trend.
* **GCV Diagnostics**: The engine utilizes **Generalized Cross-Validation (GCV)** to score a grid of 30 different lambda values distributed on a logarithmic scale from 0.00000001 to 10.
* **Interpretation**: The "Best Lambda" is defined as the value achieving the lowest GCV score. A lambda of 0 indicates an "Exact Interpolator" (zero error at sample points), while higher values indicate a "Smoothing Spline," which is often better for handling noisy sensor data.
* **Visualization**: The resulting **GCV Curve** is plotted in the Scientific Analysis tab, allowing the user to verify if the optimization process reached a clear mathematical minimum.

---

## 5. Validation Diagnostics

The dashboard automatically runs Leave-One-Out Cross-Validation (LOOCV) for the selected spatial model. By dropping one data point at a time and predicting its value using the remaining points, we generate a dataset of predicted vs. actual values (<i>P<sub>i</sub></i> vs <i>O<sub>i</sub></i>). The cross-validation engine utilizes a centralized metric abstraction (`perform_cv`) to process an expanded suite of metrics natively.

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

- **CCC (Lin's Concordance Correlation Coefficient):** Evaluates the degree to which the paired data fall on the 45-degree line of perfect agreement. It combines precision (Pearson's r) with accuracy (bias shift).

- **RPD (Ratio of Performance to Deviation):**
  <i>RPD = SD<sub>actual</sub> / RMSE</i>. A dimensionless metric. RPD &gt; 2.0 indicates an excellent predictive model. RPD &lt; 1.4 suggests the model has poor predictive capacity.

- **RPIQ (Ratio of Performance to InterQuartile Distance):**
  <i>RPIQ = (Q3 - Q1) / RMSE</i>. More robust than RPD when the original data is highly skewed (non-normal), which is common in soil properties like salinity.

- **SMAPE (Symmetric Mean Absolute Percentage Error):** Standardizes absolute errors as percentages, preventing extreme inflation when actual values are near zero.

- **Moran's I (Spatial Autocorrelation of Residuals):**
  Evaluates whether the LOOCV errors are randomly distributed across the field. If Moran's I is significantly positive, errors are clustered (e.g., the model consistently underestimates in the north and overestimates in the south). This indicates the model failed to capture a macroscopic spatial trend, and an RK or RFK approach might be required. The system uses FNN (k=1) and `spdep` for rapid spatial weights matrix construction when calculating the spatial autocorrelation of residuals.

---

## 6. Residual Analysis

Quantitative metrics summarize global performance, but Residual Analysis visualizes localized model failures, helping identify spatial patterns in the error.

### 6.1 Interpolated Delta (Surface Diff)

This function interpolates the Actual measured data and the Predicted data (from your uploaded dataset) into two separate, continuous surfaces using your chosen geostatistical method, and then subtracts them: <i>Surface<sub>Actual</sub> - Surface<sub>Predicted</sub></i>.

**Use Case:** This maps the net difference between the two geostatistical surfaces. It reveals broader regional zones where your pre-calculated machine learning predictions consistently over-predict or under-predict the true spatial distribution of the target variable in the soil.

### 6.2 Interpolated Point Errors

This calculates the discrete error at each exact sampling location (<i>O<sub>i</sub> - P<sub>i</sub></i>, or Actual - Predicted) and runs an Inverse Distance Weighting (IDW) interpolation purely on those error values.

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
* **Kriging Variance**: Represents the theoretical mean squared error of the prediction. It is particularly useful for comparing the relative stability and fit of different variogram models.
* **Standard Error**: The square root of the variance, expressed in the same units as your primary soil parameter (e.g., %TN or pH units).
* **Use Case**: This is the most practical metric for agronomists. For example, if a point predicts **2.0% Nitrogen** with a **Standard Error of 0.2**, you can be approximately 95% confident the true value lies between 1.6% and 2.4%.

### 7.3 Hybrid Model Uncertainty (RK & RFK)
For advanced models (Regression Kriging and Random Forest Kriging), the uncertainty is "Combined" to provide a rigorous error surface:
* **Trend Uncertainty**: Captures the error in the relationship between your soil target and environmental predictors, such as how well Elevation explains Nitrogen levels.
* **Residual Uncertainty**: Captures the Kriging error of the remaining unexplained variation.
* **Total Map**: The final uncertainty map for RK/RFK is the mathematical sum of both the trend variance and the residual kriging variance, providing a comprehensive "Full-Model" error surface.

---

## 8. Data Analytics & PCA Protocols

### 8.1 Multicollinearity Filter

The PCA module implements an automated strict collinearity check. Before executing standard PCA, it scans the numerical matrix for pairwise correlations > 0.95. If detected, it actively halts the execution and alerts the user, requiring a manual override or parameter drop. This is a critical statistical guardrail that prevents severe distortion of the loading vectors.
