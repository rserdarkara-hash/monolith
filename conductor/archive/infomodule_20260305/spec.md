# Specification: Information & Documentation Module (v2.6)

## 1. Overview
The goal is to design and seamlessly implement a comprehensive "Information & Documentation Module" into the Monolith Spatial Analysis Dashboard. This update (transitioning from v2.5 to v2.6) will provide users with rigorous scientific methodology guides and practical step-by-step UI/UX workflows directly within the application. The system will rely on external Markdown files for content management, presented via an accessible sliding drawer and contextual click-based popovers, while ensuring all styling inherits from `theme_helpers_v2.5.R` (now `_v2.6.R`).

## 2. Functional Requirements

### 2.1 UI Implementation Strategy
- **Sliding Drawer (Main Guide):** Implement a sliding sidebar/drawer that houses the full Scientific & Analytical Methodology Guide and the UI/UX User Guide. This allows users to read documentation while interacting with the app.
- **Contextual Help (Popovers):** Add small "i" icons next to complex inputs (e.g., Resolution Logic, Boundary Type, Comparison Mode). Clicking these icons will trigger rich-text popovers that stay open until dismissed.
- **Content Storage:** All extensive documentation text must be stored in external `.md` (Markdown) files and rendered dynamically within the Shiny UI.
- **Theme Inheritance:** Ensure all new UI elements (drawer, popovers, rendered Markdown) strictly inherit CSS styles from the theme helpers to support dynamic theme switching.
- **File Backups & Versioning:** Before any changes, back up `app_v2.5.R` and its helpers (`theme_helpers_v2.5.R`, `spatial_helpers_v2.5.R`, `ui_helpers_v2.5.R`) to the `backups/` folder. Create new v2.6 versions of these files (`app_v2.6.R`, `theme_helpers_v2.6.R`, etc.) for development.

### 2.2 Scientific & Analytical Methodology Guide (Objective 1)
Draft and display rigorous guides explaining the mathematical intuition, logic, and evaluation criteria for:
- **Spatial Interpolation Engines:** Ordinary Kriging (OK), Regression Kriging (RK), Random Forest Kriging (RFK), Co-Kriging (CK), Inverse Distance Weighting (IDW), Thin Plate Spline (TPS).
- **Variogram Optimization:** Auto-Fit vs. Manual tuning modes per method (nugget, partial sill, range).
- **Validation Diagnostics:** Explain metrics calculated in `spatial_helpers` (RMSE, Traditional vs. Correlation R², MBE, CCC, RPD, RPIQ, SMAPE, Moran's I). Includes mathematical definitions, practical agronomical examples, and scientific references.
- **Residual Analysis:** Explain the difference between "Interpolated Delta (Surface Diff)" and "Interpolated Point Errors".

### 2.3 Step-by-Step UI/UX User Guide (Objective 2)
Draft and display a practical workflow guide explaining the dashboard's interface:
- **Step 1: Data Setup:** Coordinate system (CRS) detection, variable mapping (Actual vs. Predicted), and metadata file upload.
- **Step 2: Spatial Engine & Tuning:** Interpolation method selection based on data density, applying/evaluating optimization results, and dynamic resolution logic.
- **Step 3: Borders & Styling:** Polygoning logics, "Continuous", "Binned", and "Agronomical" color styles (Jenks, K-means, supervised limits).
- **Step 4: Export Styler:** Typographical scaling, high-contrast modes, and DPI settings in the unified session export registry.

## 3. Non-Functional Requirements
- **Maintainability:** Externalizing documentation to Markdown files ensures easy updates without altering application logic.
- **Performance:** Dynamic loading of Markdown should not significantly impact the initial load time of the dashboard.
- **Aesthetics:** All new UI components must be fully responsive and visually consistent with the existing theme engine.

## 4. Acceptance Criteria
- [ ] Application files are correctly backed up and new v2.6 files are initialized.
- [ ] A sliding drawer is successfully implemented and togglable from the main UI, rendering the external Markdown documentation.
- [ ] Contextual "i" popovers are present next to critical UI elements and function correctly (click to toggle).
- [ ] Markdown content accurately covers all specified scientific methodologies, UI workflows, agronomical examples, and references.
- [ ] The newly added UI components seamlessly change styling when the user switches themes via the theme engine.

## 5. Out of Scope
- Adding new spatial interpolation methods.
- Modifying the underlying mathematical logic of existing functions.