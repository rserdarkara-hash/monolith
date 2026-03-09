# Specification: Export Panel Improvements (Track: export_panel_v2.3)

## 1. Overview
This track focuses on a comprehensive upgrade of the Monolith Spatial Analysis Dashboard's export capabilities. The goal is to provide a centralized, highly customizable interface (Export Panel) that allows users to review, configure, and export all plots, maps, and tabular data generated during a session.

## 2. Development Strategy (Safety & Integrity)
- **Mandatory Backup:** Before any implementation begins, all active version files (`app_v2.2_theme.R`, `spatial_helpers_v2.2.R`, `ui_helpers_v2.2.R`, and `theme_helpers.R`) must be backed up to the `backups/` directory.
- **Non-Destructive Upgrade:** `app_v2.2_theme.R` and its helpers must NEVER be overwritten. Implementation will occur on a newly created `app_v2.3.R` and corresponding helper copies.

## 3. Functional Requirements

### 3.1 Unified Export Registry
- **Session Tracking:** The application must maintain a registry of all "exportable" objects (plots, maps, tables) created during the current user session.
- **Object Types:**
  - **Plots/Maps:** Actual Maps, Predicted Maps, Comparison Maps, Residual Maps, Variograms, Cross-Validation Plots.
  - **Tables:** Cross-validation metrics, Surface area/Zoning statistics, Sample data, Correlation rankings.

### 3.2 Advanced Configuration Tools (The "Styler")
- **Typography:** Options to adjust font family (supporting Google Fonts), font size, and label orientation (horizontal/vertical/angled).
- **Layout & Spacing:** Real-time adjustment of plot margins, padding between elements (e.g., legend vs. plot area), and legend positioning (top/bottom/left/right).
- **Output Quality:** Controls for DPI settings (72 to 600 DPI), color profiles (RGB/CMYK), and background transparency for PNG/PDF.

### 3.3 Dynamic Preview & Modal Interface
- **Export Modal/Popup:** A dedicated UI layer (potentially a `shiny::modalDialog` or a custom CSS overlay) to house the "Styler" tools and a high-fidelity dynamic preview of the selected asset.
- **Live Updates:** Changes made in the "Styler" must reflect instantly in the preview window before final export.

### 3.4 Multi-Format Support
- **Plot Formats:** PNG, TIFF, PDF, and high-resolution JPEG.
- **Tabular Formats:** Excel (.xlsx - multi-sheet support), Word (.docx - with formatted tables), and CSV.
- **Combined Reports:** Ability to generate a single Word document containing selected plots and their corresponding statistical tables.

### 3.5 Batch Export Workflow
- **Multi-Selection:** A "Batch List" view allowing users to select multiple items from the session registry for simultaneous export.
- **Contextual Export:** Shortcut "Export" buttons integrated directly into analysis tabs for quick access to the "Styler".

## 4. Non-Functional Requirements
- **Performance:** Dynamic previews must be optimized to prevent UI lag during parameter adjustment (e.g., debouncing inputs).
- **Aesthetic Consistency:** The Export Panel UI must respect the active theme (Theme Engine) and match the "Monolith" aesthetic.

## 5. Acceptance Criteria
- [ ] User can see a list of all plots and tables generated in the current session.
- [ ] Changing a font size or margin in the "Styler" updates the preview image in real-time.
- [ ] Exporting to Word generates a document with correctly formatted tables and clear images.
- [ ] Exporting to Excel creates a workbook with multiple sheets for different analysis metrics.
- [ ] The "Export Panel" functions correctly under different themes (e.g., Obsidian Night, Deep Forest).

## 6. Out of Scope
- Permanent storage of exported files on the server (all exports are client-side downloads).
- Editing of raw data within the export panel (only styling/configuration).
