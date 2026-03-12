# Scientific Guide Additions & Clarifications

Based on the latest comprehensive review of the Monolith codebase against the current `scientific_guide.md`, the following points should be added or clarified to ensure 100% alignment between documented claims and codebase execution.

## 1. Algorithmic Stability & Math Enhancements
The codebase employs advanced stability checks that should be explicitly documented to enhance the scientific credibility of the tool:
- **Epsilon-Nugget Stability in OK:** For variables with extremely low variance (e.g., specific micro-nutrients like Iron), the codebase strictly enforces a tiny nugget (`1e-6 * initial_sill`) when the initial empirical nugget is exactly zero. This prevents singular matrix inversion failures during Kriging.
- **NaN Protection:** The resulting kriged predictions (`var1.pred`) and variances (`var1.var`) are systematically protected against NaN or Infinite outputs, explicitly converting them to `NA` for robust downstream mapping.

## 2. Dynamic Cross-Validation Protocols
- **IDW Optimization Detail:** The documentation states IDW optimizes a power parameter via LOOCV. The codebase reveals an adaptive threshold: it employs LOOCV for $n \le 50$, but intelligently switches to a **5-fold Cross-Validation** strategy for datasets where $n > 50$ to maintain UI responsiveness and computational efficiency without sacrificing statistical reliability. This should be explicitly stated in Section 2.2.
- **Covariate Kriging Fallback (CK/RK/RFK):** When interpolating covariates across the spatial grid, if covariate kriging fails (e.g., due to pure nugget effects or collinearity collapses), the pipeline implements an automatic and silent `tryCatch` fallback to IDW ($p=2$, $nmax=12$). Documenting this increases transparency on how the spatial engine ensures map generation succeeds.

## 3. High-Fidelity Validation Metrics
- **Centralized Metric Abstraction (`perform_cv`):** The cross-validation engine processes an expanded suite of robust metrics. While RMSE, CCC, and R2 are documented, the codebase natively calculates **NSE** (Nash-Sutcliffe Efficiency), **NRMSE_mean** (Normalized RMSE), **RPD**, **RPIQ**, and **SMAPE**. Section 3 should be expanded to mathematically define RPIQ and SMAPE, as they are crucial for evaluating skewed soil variables like Salinity.
- **Moran's I Execution:** The system uses FNN (k=1) and `spdep` for rapid spatial weights matrix construction when calculating the spatial autocorrelation of residuals.

## 4. PCA Collinearity Protocol
- **Multicollinearity Filter:** The PCA module implements an automated strict collinearity check. Before executing standard PCA, it scans the numerical matrix for pairwise correlations $> 0.95$. If detected, it actively halts the execution and alerts the user, requiring a manual override or parameter drop. This is a critical statistical guardrail that prevents severe distortion of the loading vectors and should be formally documented in the Data Analytics section.

---
*No existing files were overwritten. These additions act as structural proposals for the next documentation revision.*