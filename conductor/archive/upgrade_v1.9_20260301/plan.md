# Implementation Plan - Upgrade to v1.9: Optimize Spatial Interpolation & Scientific Metrics

## Phase 1: Environment Setup & Baseline [checkpoint: 0d4faf3]
- [x] Task: Create v1.9 Working Branch (reproduce v1.8 app to v1.9) 56bab2d
    - [x] Create a backup of `app_v1.8.R` in `backups/pre_v1.9_upgrade_20260301/`
    - [x] Copy `app_v1.8.R` to `app_v1.9.R`
- [x] Task: Initialize Testing Infrastructure e54b994
    - [x] Setup `tests/testthat/` directory structure
    - [x] Create `tests/testthat/test-spatial-logic.R` with baseline tests for current interpolation methods
- [x] Task: Conductor - User Manual Verification 'Phase 1: Environment Setup & Baseline' (Protocol in workflow.md) 0d4faf3

## Phase 2: RK & RFK Optimization (Errors 1A, 1B) [checkpoint: 0e9aed8]
- [x] Task: Fix Artificial Covariate Degradation in CV (Error 1A) cf10030
    - [x] Write failing test for `perform_rk_cv` that fails when covariates are interpolated unnecessarily
    - [x] Implement fix in `app_v1.9.R`: remove IDW block and pass test row directly
    - [x] Verify test passes
- [x] Task: Fix Overfitted RF Residuals (Error 1B) 280f5d3
    - [x] Write failing test for `perform_rfk_cv` that detects in-sample vs OOB residual difference
    - [x] Implement fix in `app_v1.9.R`: use `rf_mod$predicted` for training residuals
    - [x] Verify test passes
- [x] Task: Conductor - User Manual Verification 'Phase 2: RK & RFK Optimization' (Protocol in workflow.md) 0e9aed8

## Phase 3: TPS Optimization (Errors 2A, 2B) [checkpoint: 1fc9c95]
- [x] Task: Fix Anisotropic Geometric Distortion (Error 2A) 37fcf35
    - [x] Write failing test for `opt_tps` that checks for aspect ratio preservation in scaling
    - [x] Implement fix in `app_v1.9.R`: scale both axes by global max range
    - [x] Verify test passes
- [x] Task: Remove Redundant Lambda Grid Search (Error 2B) e0a86bf
    - [x] Write test to verify `opt_tps` returns optimal lambda from `fields::Tps` internal optimization
    - [x] Implement fix in `app_v1.9.R`: remove manual grid search loop
    - [x] Verify test passes
- [x] Task: Conductor - User Manual Verification 'Phase 3: TPS Optimization' (Protocol in workflow.md) 1fc9c95

## Phase 4: IDW & Moran's I Optimization [checkpoint: c8d7162]
- [x] Task: Optimize IDW Grid Search (Process Flaw) 92d9949
    - [x] Write performance test for `optimize_idw_p`
    - [x] Implement fix in `app_v1.9.R`: change power increment from 0.1 to 0.5
    - [x] Verify performance improvement and test passing
- [x] Task: Implement Sparse Matrix for Moran's I (Efficiency) f7afa3c
    - [x] Write failing test for `calc_moran` on large dataset (simulated)
    - [x] Implement fix in `app_v1.9.R`: use sparse matrix approach (e.g., `Matrix` package)
    - [x] Verify RAM usage reduction and test passing
- [x] Task: Conductor - User Manual Verification 'Phase 4: IDW & Moran's I Optimization' (Protocol in workflow.md) c8d7162

## Phase 5: Release & Verification [checkpoint: 233d6a6]
- [x] Task: Final Regression Testing ab8bc45
    - [x] Run full test suite against `app_v1.9.R`
    - [x] Verify all UI elements and legacy features still work as expected
- [x] Task: Documentation & Cleanup fd58110
    - [x] Update `GEMINI.md` or version notes if applicable
    - [x] Cleanup temporary test data
- [x] Task: Conductor - User Manual Verification 'Phase 5: Release & Verification' (Protocol in workflow.md) 233d6a6
