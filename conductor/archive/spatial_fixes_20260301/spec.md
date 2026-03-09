# Specification: Spatial Interpolation Optimization Processes (v2.0)

## 1. Overview
The goal of this track is to implement critical mathematical corrections and optimizations to the spatial interpolation pipeline, upgrading the monolithic application from v1.9 to v2.0. The core focus is to address theoretical inconsistencies in hybrid kriging (RFK, RK) and co-kriging models, ensuring rigorous geostatistical validity while retaining all existing styles, functionality, and scientific metrics.

## 2. Functional Requirements
### 2.1 Finalizing RFK Residuals (Issue A)
- **Requirement:** Update the `apply_interpolation` function for Random Forest Kriging (RFK).
- **Detail:** Use Out-of-Bag (OOB) predictions from `rf_mod$predicted` instead of in-bag predictions (`predict(rf_mod, data)`) to calculate residuals for final variogram fitting.

### 2.2 Co-Kriging (CK) Initialization (Issue B)
- **Requirement:** Fix the Linear Model of Coregionalization (LMC) initialization in CK.
- **Detail:** Standardize all covariates to a mean of 0 and variance of 1 prior to CK, OR dynamically inject each variable's specific variance into its respective starting variogram model to prevent positive-definite matrix failures.

### 2.3 Correcting VIF Thresholding (Issue C)
- **Requirement:** Isolate Variance Inflation Factor (VIF) filtering.
- **Detail:** Apply VIF thresholding (dropping covariates > 10) exclusively to Regression Kriging (RK). Ensure this filter is NOT applied to Random Forest Kriging (RFK).

### 2.4 Fixing Prediction Variance Estimation (Issue D)
- **Requirement:** Correct uncertainty mapping for hybrid models (RK & RFK).
- **Detail:** Total prediction variance must sum both the trend model's variance and the residual kriging variance (e.g., `total_var = trend_var + res_krig$var1.var`).

### 2.5 Mitigating Error-in-Variables Bias (Issue E)
- **Requirement:** Improve covariate interpolation prior to trend prediction.
- **Detail:** Replace the current IDW smoothing of covariates with Ordinary Kriging (OK) to respect spatial structure and reduce attenuation bias. (Explore Sequential Gaussian Simulation (SGS) if computationally feasible within the Shiny architecture).

### 2.6 Explicit Co-Kriging Fallback UI (Issue F)
- **Requirement:** Handle Co-Kriging convergence failures transparently.
- **Detail:** When LMC fitting fails, the application should fallback to Ordinary Kriging (OK), but it **must** display a prominent UI warning indicating the fallback occurred, so the user is not misled.

### 2.7 App Versioning and Backup
- **Requirement:** Copy existing `app_v1.9.R` and its helper files to `app_v2.0.R` (and `spatial_helpers_v2.0.R`, `ui_helpers_v2.0.R`).
- **Detail:** Never overwrite the v1.9 files. The previous files must remain fully functional and backed up.

## 3. Non-Functional Requirements
- **Performance:** Ensure that implementing OK for covariates does not cause unacceptable blocking in the Shiny app (leverage `future`/`promises` if necessary).
- **Consistency:** Maintain all styling, layouts, and data ingestion logic identical to v1.9.

## 4. Acceptance Criteria
- [ ] `testthat` scripts are implemented to validate RFK OOB residual calculation and CK matrix initialization.
- [ ] `testthat` scripts verify that the UI warning mechanism correctly triggers during an intentional CK convergence failure.
- [ ] Visual and quantitative outputs from v2.0 are systematically compared against v1.9 to confirm fixes while ensuring baseline continuity.
- [ ] The app runs from a new file `app_v2.0.R` with appropriate updated helpers, leaving v1.9 untouched.

## 5. Out of Scope
- Major UI redesigns or new plot types not related to the listed bugs.
- Modifications to purely non-spatial components like basic data table rendering.