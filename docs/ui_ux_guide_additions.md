# UI/UX Guide Additions & Clarifications

Based on a systematic review of the UI components (`ui_helpers_0.8.9.R`) and the main mapping workflows (`monolith_ver_0.8.9.R`), the UI architecture closely mirrors the existing `ui_ux_guide.md`. The following minor additions are proposed to fully capture the latest UX refinements and pipeline logic.

## 1. Data Ingestion & Fallbacks
- **Smart Suffix Matching Details:** The documentation mentions suffix matching for Predictions (`_cve`, `_ss`). It should also be noted that the UI groups variables dynamically into structural folders (e.g., 'Soil Physicochemistry', 'Terrain') based on the uploaded metadata, allowing users to seamlessly navigate high-dimensional datasets.
- **Collinearity Warning UI:** Under the PCA analysis tab, if the user selects variables with a correlation $r > 0.95$, a dedicated warning UI (`pca_collinearity_warning_ui`) alerts them to the specific variable pair causing the multicollinearity. A "Force PCA" red button is provided. This safety mechanism enhances the analytical UX.

## 2. Optimization Panels & Manual Overrides
- **TPS and IDW Dedicated Tuning UIs:** The interface provides a dynamic slider panel that not only acts globally but calculates optimal parameters on a per-locality basis. When `OPTIMIZE IDW FACTORS` or `OPTIMIZE TPS LAMBDA` is clicked, the UI generates a reactive table detailing the exact Lambda/Power factor chosen for *each independent spatial domain* (e.g., Zone A vs. Zone B). This per-locality tracking should be explicitly detailed in the Optimization section of the guide.
- **Maximum Neighbors for IDW (`nmax`):** A specific tooltip in the UI advises users to restrict `idw_nmax` (default 12) before optimization to prevent distant, unrelated points from distorting local predictions. 

## 3. Responsive Diagnostic Views
- **Conditional Metric Rendering:** The UI employs `shinyjs` to dynamically toggle the visibility of complex validation diagnostics (like the RF Variable Importance Plot, Internal Residual Variogram, or TPS GCV Diagnostic Plot) based on the active Spatial Engine and whether the user is viewing `Actual` or `Predicted` data. This prevents empty plots from rendering and ensures visual cleanliness.

## 4. Export & Theming Persistence
- **JSON Configuration Saving:** The Export Styler includes a mechanism to save and load typographical/margin configurations natively as `.json` files. This allows users to establish a specific "Lab Standard" aesthetic and apply it across different sessions instantly.

---
*No existing files were overwritten. These additions act as structural proposals for the next documentation revision.*