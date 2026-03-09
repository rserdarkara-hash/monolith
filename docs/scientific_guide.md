# Scientific & Analytical Methodology Guide

The Monolith Spatial Analysis Dashboard provides agronomists, soil scientists, and geostatisticians with a toolkit for exploring spatial variability. This guide briefly elaborates on the mathematical intuition, structural assumptions, and practical applications of the underlying spatial methods and evaluation metrics.

---

## 1. Spatial Interpolation Engines

Spatial interpolation creates continuous prediction surfaces from discrete point samples. The dashboard implements both deterministic methods (relying solely on geometric proximity) and geostatistical models (incorporating spatial autocorrelation and statistical uncertainty).

### 1.1 Ordinary Kriging (OK)

**Mathematical Intuition:** Ordinary Kriging assumes that the value at an unsampled location <i>Z<sup>*</sup>(x<sub>0</sub>)</i> is a linear combination of known surrounding values <i>Z(x<sub>i</sub>)</i>. The formula is:
<br><br>
<div style="text-align:center;"><i>Z<sup>*</sup>(x<sub>0</sub>) = &sum; &lambda;<sub>i</sub> Z(x<sub>i</sub>)</i></div>
<br>
Unlike simple kriging, OK assumes an unknown, constant global mean (<i>&mu;</i>). The weights <i>&lambda;<sub>i</sub></i> are determined by minimizing the estimation variance while ensuring the weights sum to 1 (<i>&sum; &lambda;<sub>i</sub> = 1</i>). The variance-covariance matrix used to solve for these weights is derived directly from the theoretical variogram.

**Agronomical Example:** Predicting soil pH across a relatively uniform field where variations are driven by natural soil-forming processes rather than abrupt topographical changes or human intervention.

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

### 1.5 Inverse Distance Weighting (IDW)

**Mathematical Intuition:** A purely deterministic method. The estimated value is a weighted average of known points, where the weight is inversely proportional to the distance <i>d</i> raised to a power <i>p</i> (usually <i>p=2</i>):
<br><br>
<div style="text-align:center;"><i>&lambda;<sub>i</sub> = (1 / d<sub>i</sub><sup>p</sup>) / &sum; (1 / d<sub>i</sub><sup>p</sup>)</i></div>
<br>
IDW assumes that points closer to the target are more similar. It does not account for data clustering (redundant sampling) or directional anisotropy.

**Agronomical Example:** Quick, computationally inexpensive mapping of recent, localized rainfall events from a scattered network of rain gauges where statistical assumptions of stationarity are not strictly necessary.

### 1.6 Thin Plate Spline (TPS)

**Mathematical Intuition:** TPS is a deterministic method akin to bending a sheet of metal to pass exactly through the sampled data points while minimizing the "bending energy" (the integral of the squared second derivatives of the surface). It yields highly smooth surfaces but is susceptible to severe overshooting or undershooting in areas devoid of data.

**Agronomical Example:** Generating smooth elevation contours or temperature gradients where abrupt discontinuities are physically implausible.

---

## 2. Automated Optimizations

### 2.2 Variogram Optimization
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
### 2.2 IDW Optimization
* **Logic**: The application performs an automated search to find the optimal **Distance Power** that minimizes the spatial interpolation error for each specific locality.
* **Optimization Engine**: The system executes a Leave-One-Out Cross-Validation (LOOCV) loop, testing power factors ranging from **0.5 to 5.0**.
* **High-Precision Mode**: For larger datasets (typically > 50 points), the engine automatically switches to **5-fold Cross-Validation** to maintain computational efficiency without sacrificing statistical reliability.
* **Local Adaptation**: As the soil variability is site-specific, the "Optimize" button calculates a unique power factor for every selected locality. 

### 2.3 TPS Optimization
* **Logic**: The software optimizes the **Smoothing Parameter** to achieve the ideal mathematical balance between honoring every individual data point and creating a generalized regional trend.
* **GCV Diagnostics**: The engine utilizes **Generalized Cross-Validation (GCV)** to score a grid of 30 different lambda values distributed on a logarithmic scale from 0.00000001 to 10.
* **Interpretation**: The "Best Lambda" is defined as the value achieving the lowest GCV score. A lambda of 0 indicates an "Exact Interpolator" (zero error at sample points), while higher values indicate a "Smoothing Spline," which is often better for handling noisy sensor data.
* **Visualization**: The resulting **GCV Curve** is plotted in the Scientific Analysis tab, allowing the user to verify if the optimization process reached a clear mathematical minimum.

## 3. Validation Diagnostics

The dashboard automatically runs Leave-One-Out Cross-Validation (LOOCV) for the selected spatial model. By dropping one data point at a time and predicting its value using the remaining points, we generate a dataset of predicted vs. actual values (<i>P<sub>i</sub></i> vs <i>O<sub>i</sub></i>).

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
  Evaluates whether the LOOCV errors are randomly distributed across the field. If Moran's I is significantly positive, errors are clustered (e.g., the model consistently underestimates in the north and overestimates in the south). This indicates the model failed to capture a macroscopic spatial trend, and an RK or RFK approach might be required.

---

## 4. Residual Analysis - regarding parameter predictions

Quantitative metrics summarize global performance, but Residual Analysis visualizes localized model failures, helping identify spatial patterns in the error.

### 4.1 Interpolated Delta (Surface Diff)

This function interpolates the Actual measured data and the Predicted data (from your uploaded dataset) into two separate, continuous surfaces using your chosen geostatistical method, and then subtracts them: <i>Surface<sub>Actual</sub> - Surface<sub>Predicted</sub></i>.

**Use Case:** This maps the net difference between the two geostatistical surfaces. It reveals broader regional zones where your pre-calculated machine learning predictions consistently over-predict or under-predict the true spatial distribution of the target variable in the soil.

### 4.2 Interpolated Point Errors

This calculates the discrete error at each exact sampling location (<i>O<sub>i</sub> - P<sub>i</sub></i>, or Actual - Predicted) and runs an Inverse Distance Weighting (IDW) interpolation purely on those error values.

**Use Case:** This creates an "Uncertainty Map" showing the spatial structure of local model failure (the model produced the uploaded parameter predictions, not the spatial interpolation model). Hotspots on this map indicate distinct zones in the field where the current prediction model cannot capture the true soil variability.

## 5. Uncertainty Analysis & Confidence Mapping - regarding spatial interpolation

While interpolation provides the "best guess" for a soil property, Uncertainty Analysis quantifies the reliability of that guess at every pixel in the field. This feature is exclusively available for Kriging-based methods (OK, RK, RFK, CK), as they provide a formal statistical error model.

### 5.1 Theoretical Basis
In Kriging, the uncertainty is a function of the **Spatial Configuration** of your samples and the **Variogram Model**.
* **Geometric Influence**: Uncertainty is at its lowest at the exact location of a sample point and increases as you move into "unsampled" territory.
* **Variogram Influence**: A high **Nugget** or a short **Range** in the fitted model will result in higher overall uncertainty across the entire generated map.

### 5.2 Uncertainty Metrics
The application allows you to toggle between two primary metrics for visualizing spatial risk:
* **Kriging Variance**: Represents the theoretical mean squared error of the prediction. It is particularly useful for comparing the relative stability and fit of different variogram models.
* **Standard Error**: The square root of the variance, expressed in the same units as your primary soil parameter (e.g., %TN or pH units).
* **Use Case**: This is the most practical metric for agronomists. For example, if a point predicts **2.0% Nitrogen** with a **Standard Error of 0.2**, you can be approximately 95% confident the true value lies between 1.6% and 2.4%.

### 5.3 Hybrid Model Uncertainty (RK & RFK)
For advanced models (Regression Kriging and Random Forest Kriging), the uncertainty is "Combined" to provide a rigorous error surface:
* **Trend Uncertainty**: Captures the error in the relationship between your soil target and environmental predictors, such as how well Elevation explains Nitrogen levels.
* **Residual Uncertainty**: Captures the Kriging error of the remaining unexplained variation.
* **Total Map**: The final uncertainty map for RK/RFK is the mathematical sum of both the trend variance and the residual kriging variance, providing a comprehensive "Full-Model" error surface.