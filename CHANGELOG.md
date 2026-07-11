# Changelog

All notable changes to Monolith are documented in this file.

## [1.0.2] - 2026-07-11

### Added
- **Classification Suite** (new Tab 6, `classif_helpers.R` + `classif_module.R`): true predictive multiclass classification from co-sampled covariates, distinct from the existing continuous-to-agro-zone binning. Built on a tidymodels backbone (`parsnip`, `recipes`, `workflows`, `tune`, `rsample`, `dials`, `spatialsample`, `hardhat`, `yardstick`) with three learners — multinomial (nnet), random forest (ranger, probability forest), and XGBoost.
  - Shared preprocessing recipe (novel level -> impute -> dummy -> zero-variance filter -> normalize) and an expandable tuning-depth registry (none / light / full).
  - Hand-rolled out-of-fold CV loop with spatial CV (k-means clustering via `spatialsample`) as the default strategy, matching the interpolation engines' spatial-autocorrelation-aware philosophy.
  - Optional **nested CV** (5 inner folds of the same strategy) for honest hyperparameter-selection performance estimates, alongside the plain (non-nested) path.
  - Per-class producer/user accuracy, macro metrics, probability metrics (ROC AUC, log-loss, Brier score), and a normalized Shannon-entropy uncertainty surface.
  - Raster prediction reuses the interpolation engine's covariate kriging for numeric covariates plus nearest-point assignment for categorical ones.
  - Spatial scope controls shared with the sidebar's Boundary Type/Buffer/Resolution, with live "in scope" point counts and a thin-scope adequacy warning that names every class below the minimum sample count with concrete remedies.
  - Per-area ("Performance by Area") metrics when multiple localities are in scope, method-aware VIF collinearity guardrails (threshold 5 for Random Forest, 10 otherwise), inverse-frequency class weighting, and an optional abstention (reject-option) threshold at rasterization.
  - Results panel: 2x2 map grid (variable importance, class map, entropy, class probability) with a click-to-expand modal (scale bar, north arrow, and sample-point overlay options), and a single results table behind a dropdown (metrics, per-class, per-area, as applicable).
  - Dedicated model bundle (.rds) export and publication-ready PNG export (projected axes, 300 dpi), separate from the interpolation module's Export Registry.

### Changed
- 11 tidymodels-ecosystem packages added to the dependency suite (60 total); `timechange` bumped to >= 0.4.0 (required by `recipes`). See README §3 for the full table.

### Testing
- Added `test-classification.R` covering the classification engine, spatial scope resolution, scope adequacy warnings, nested CV, and method-aware VIF.
- Full suite: 1095 passing, 0 failing.

## [1.0.1] - 2026-07-09

### Added
- **Map Viewer "View" dropdown** to switch instantly between the surfaces a completed run already computed (Actual, ML Predicted, Actual vs Predicted, Residuals) — no re-run needed.
- **Diverging color palette selector** for residual/error maps in the Export Styler (9 options, default Red-Blue), with automatic colorblind-safe substitution when High-Contrast Mode is on and the chosen palette isn't colorblind-safe.
- **Expanded descriptive-suite color palette catalogue** (Okabe-Ito, Viridis, Cividis, Plasma, Turbo, plus additional Brewer qualitative sets), correctly ramped past a palette's native color maximum instead of degrading to NA colors.
- **Point Error Map** is now exported as discrete point markers at the exact sample locations (matching the Map Viewer's Point Residuals panel), in addition to the existing IDW-interpolated **Interpolated Point Errors Map**.
- **TPS lambda preset buttons** ("Set Auto (GCV)" / "Set Exact (0)") under both the global and the per-locality manual lambda sliders; the 0.001-step slider made the special values -1 (Auto) and 0 (exact interpolation) hard to hit by dragging.

### Changed
- **Sidebar inputs now configure only the *next* run.** Changing Variable, Primary View, Interpolation Method, or Comparison Mode no longer alters the currently displayed map, titles, or Scientific Analysis tables — those keep describing the last completed run until **Run Interpolation** is pressed again. Styling controls (Continuous/Binned/Agronomical, class limits, palette) remain live and restyle the displayed map immediately. This closes a class of bugs where reconfiguring the sidebar mid-run or after a run desynced the display from the actual results.
- The "Map Uncertainty" checkbox and Scientific Analysis panels (variogram/RF/CK/TPS diagnostics) now follow the displayed run's method rather than the live sidebar selection, so they no longer flicker or vanish while choosing the next method.
- Residual and uncertainty map layers are now consistently excluded from Agronomical/Binned classification everywhere, including the Export Styler (previously only excluded in the live Map Viewer; concentration-based class limits are not meaningful on an error or variance scale).
- Governing Factors module: the "running" spinner now appears immediately on click instead of lagging ~10 seconds behind (spinner state now driven by `shinyjs` rather than a delayed reactive output), and only the target + predictor columns are shipped to the background worker.
- Scientific Analysis tables (variogram/regional parameters, model metrics, area coverage, descriptive stats) now keep computing while their tab is hidden, so opening the tab after a run shows current results immediately instead of stale numbers.
- The "Total (Combined)" Variogram Parameters and Regional Parameters tables, previously required to switch to a specific locality, now list every fitted locality.
- After pressing Run, the tab switch to the Map Viewer now happens instantly (almost) instead of lagging behind data validation and dispatch.

### Fixed
- `var_mapping_ui`'s error handler now logs via `warning()` instead of `print()`.
- Uploaded-prediction detection (`has_upl_pred`, palette picker, agronomical limit UI) now correctly treats the "no prediction column" sentinel (`NA`) as absent via a new `is_valid_col_ref()` helper — previously only a bare `is.null()` check was used, which could miss this case.
- The Export Styler could classify uncertainty map layers into Agronomical/Binned zones when "Binned" was selected, since the exclusion check only accounted for residual layers, not uncertainty layers.
- Removed premature/misleading captions on tables shown before any run existed; Area Coverage and Descriptive Statistics sections now render only once a run exists and only show the locality tables relevant to the current selection.
- **The Regional Parameters table (and the exported per-locality "Model Parameters" table) now report the lambda / IDW power the displayed run actually used.** Previously they read the live tuning store, which is only filled by the optimizer or a manual apply; runs that fell back to the global slider (e.g. a fixed Lambda of 0) were mislabeled "Auto (GCV)", and optimizing after a run could rewrite the table under an already-displayed result. The per-locality parameters consumed at dispatch are now committed with the run's display context, and the Predicted column shows "N/A" when the run computed no predicted surface.
- **TPS GCV Diagnostics panels no longer mislead when no GCV curves exist.** GCV curves are produced only by the "OPTIMIZE TPS LAMBDA" button, yet after a plain TPS run (Auto lambda = -1 default, or a fixed lambda such as 0) the "Total (Combined)" view still promised per-locality figures while the per-locality view stayed silently blank. Every empty state now shows an explicit placeholder: how to generate the curves (run the optimizer; fixed-lambda runs have no GCV search to plot), and a per-locality notice when the optimizer skipped or failed a locality (e.g. fewer than 5 valid points).

### Testing
- Added `test-base-plot-styler.R`, `test-fuzzy-matching.R`, and expanded `test-palettes.R` covering the export styler's classification rules, point-error rendering, the new palette helpers, and the `is_valid_col_ref` guard.
- Added `test-tps-gcv-plot.R` covering every state of the TPS GCV diagnostics builder (no curves at all, wrong target, Total view, missing locality, real curve) and of the run-committed Regional Parameters table builder (fixed lambda vs Auto labeling, Total view, missing predicted surface, IDW keys).
- Full suite: 916 passing, 0 failing.

## [1.0.0] - 2026-07-06

Initial public release.
