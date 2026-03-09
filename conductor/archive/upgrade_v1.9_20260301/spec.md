# Specification: Upgrade to v1.9 - Spatial Interpolation & Scientific Metrics

## 1. Overview
This track focuses on critical optimizations and bug fixes for the Monolith Spatial Analysis Dashboard, transitioning from v1.8 to v1.9. The primary goals are to fix scientific errors in cross-validation (RK/RFK), improve the mathematical integrity of Thin Plate Splines (TPS), and enhance computational efficiency for IDW and Moran's I calculations.

## 2. Functional Requirements

### 2.1 RK/RFK Optimization
- **Fix Artificial Covariate Degradation (Error 1A):** Remove the IDW interpolation block inside `perform_rk_cv` and `perform_rfk_cv` cross-validation loops. Pass the test row directly into the trend prediction to use known auxiliary variables.
- **Fix Overfitted RF Residuals (Error 1B):** Change the residual calculation in `perform_rfk_cv` to use Out-Of-Bag (OOB) predictions (`train[[target_var]] - rf_mod$predicted`) instead of in-sample predictions.

### 2.2 TPS Optimization
- **Fix Anisotropic Geometric Distortion (Error 2A):** Scale both X and Y axes by the exact same global maximum range in `opt_tps` and `apply_interpolation` to preserve Euclidean distance geometry.
- **Remove Redundant Lambda Grid Search (Error 2B):** Replace the manual grid search loop in `opt_tps` with the internal optimization logic of `fields::Tps()`.

### 2.3 IDW & Moran's I Optimization
- **IDW Bottleneck Fix:** Increase the power parameter increment from 0.1 to 0.5 in `optimize_idw_p` for improved computational efficiency.
- **Moran's I Sparse Matrix Implementation:** Replace dense matrix allocation (`outer(diffs, diffs)` and `as.matrix(dist(coords))`) with a sparse matrix approach using appropriate R libraries (e.g., `spdep` or `Matrix`).

## 3. Non-Functional Requirements
- **Performance:** Significant reduction in computation time for IDW optimization and Moran's I on larger datasets.
- **Mathematical Integrity:** Preservation of physical aspect ratios in TPS and unbiased error estimation in RK/RFK.
- **Backwards Compatibility:** All existing v1.8 features, styles, and scientific functions must remain operational in v1.9.

## 4. Acceptance Criteria
- [ ] `app_v1.9.R` is created as a copy of `app_v1.8.R` with applied fixes.
- [ ] LOOCV for RK/RFK shows realistic (non-ruined) validation metrics when using known covariates.
- [ ] TPS surfaces do not exhibit geometric distortion when axes have different physical ranges.
- [ ] IDW optimization completes in less time without significant loss of accuracy.
- [ ] Moran's I calculation does not crash or exhaust RAM for datasets up to 2,000 points.
- [ ] All unit tests pass with >80% coverage.

## 5. Out of Scope
- Major UI redesign.
- Introduction of new interpolation methods (e.g., machine learning models other than RF).