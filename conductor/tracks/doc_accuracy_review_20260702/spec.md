# Specification: Monolith Documentation Accuracy Review

## Overview
This track focuses on conducting a comprehensive review and update of Monolith's documentation (Scientific Guide + User Guide) to match the v0.9.8b codebase exactly, and refactoring the documentation drawer UI to introduce a third, dedicated tab for the Descriptive & Exploratory Suite.

## Scope of Work
### 1. Documentation Drawer UI Refactoring
- Rename the tab 'UI/UX Guide' to 'User Guide'.
- Add a new third tab 'Descriptive & Exploratory Suite'.
- Update the documentation drawer renderer `render_docs_drawer` in `ui_helpers_0.9.8b.R`.
- Update the Shiny server code in `monolith_ver_0.9.8b.R` to load and render the three guides.

### 2. File Organization
- Rename `docs/ui_ux_guide.md` to `docs/user_guide.md`.
- Extract the Descriptive & Exploratory Suite sections from `docs/user_guide.md` and place them in a new file `docs/desc_exploratory_guide.md`.
- Update all code references loading these markdown files.

### 3. Documentation Accuracy Review
- **Code Inventory**: Systematically list every geostatistical model, ML module, parameter, default value, and validation step implemented in the codebase (v0.9.8b).
- **Scientific Guide**: Verify all equations, parameters, edge-case fallbacks, and mathematical descriptions in `docs/scientific_guide.md` against actual code logic (e.g. OK epsilon-nugget, IDW optimization neighbors, CK IDW fallback, TPS GCV).
- **User Guide**: Verify all references to UI elements (buttons, menus, labels, and inputs) against the codebase, ensuring a one-to-one match.
- **Descriptive & Exploratory Guide**: Verify all statistical tests (ANOVA post-hoc HSD letters, PCA collinearity warnings, SHAP/ALE explainability settings) match the actual codebase implementation.

## Acceptance Criteria
- The Shiny app runs and displays the three-tab documentation drawer.
- All three guides render properly via standard commonmark HTML compilation with MathJax support.
- Every claim, default value, parameter limit, and UI description in the guides is 100% verified against the code.
- No obsolete references or placeholder text remain in the documentation.

## Out of Scope
- Implementing new geostatistical methods or analytical features.
- Modifying the app's styling themes, except for the drawer's tab structures.
