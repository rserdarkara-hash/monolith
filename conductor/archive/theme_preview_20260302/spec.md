# Track Specification: Dynamic Theme Preview & Implementation

## Overview
This track introduces a robust UI/UX enhancement to the Monolith Spatial Analysis Dashboard (moving from v2.2 to `app_v2.2_theme.R` as a new artifact). The objective is to implement a "plug-and-play" theme engine using the `fresh` package, generating 10 distinct, highly-customized aesthetic palettes. A dynamic dropdown will allow users to seamlessly preview and switch between these themes, including updating the base map tiles in real-time while preserving the user's session state.

## Functional Requirements
1. **Theme Engine Generation:**
    - Create 10 distinct `shinydashboard` theme objects using `fresh::create_theme()`.
    - Each theme must explicitly define `light_blue` (Primary), `dark_bg` (Sidebar), and `content_bg` (Body background).
    - Apply a unique Google Font to each theme using `fresh::use_googlefont()`.
    - Synchronize the `manual_style` variable (used for custom HTML/CSS tuning boxes) with the selected theme's color palette.
2. **Theme Switcher UI:**
    - Implement a Dropdown Menu in the header or sidebar for theme selection.
    - Ensure the selected theme is saved in the user's browser (e.g., via `shinyjs` cookies or `shinyStorePlus`) to persist across sessions.
3. **Dynamic Map Integration:**
    - Link the active theme to specific Leaflet base tiles (`providers$CartoDB.DarkMatter` for Dark themes, `providers$CartoDB.Positron` for Minimalist, and `providers$Esri.WorldImagery` for Earthy).
    - Update base tiles dynamically via `leafletProxy` upon theme switch to preserve map zoom and pan state.
4. **File Operations:**
    - Do NOT overwrite existing `app_v2.2.R` or its helpers.
    - Backup existing v2.2 files and create `app_v2.2_theme.R`, along with any new helper scripts (e.g., `theme_helpers.R`).

## Non-Functional Requirements
- **Performance:** Theme switching must be seamless without triggering a full page reload or map re-render.
- **Maintainability:** The 10 themes should be defined in a separate helper file to keep the main application file clean.
- **Resilience:** If the browser-stored theme is invalid or missing, default to a robust primary theme.

## Acceptance Criteria
- [ ] 10 distinct themes (Deep Forest, Obsidian Night, etc.) are available in the dropdown.
- [ ] Selecting a theme instantly updates the dashboard colors, fonts, and `manual_style` boxes.
- [ ] Leaflet base tiles update dynamically (DarkMatter, Positron, WorldImagery) based on the theme category without losing the current map view.
- [ ] Theme selection persists after refreshing the page.
- [ ] The original `app_v2.2.R` remains untouched, and the new version is built as `app_v2.2_theme.R`.

## Out of Scope
- Modifying the underlying spatial interpolation logic.
- Adding new analytical features or changing the existing data upload workflows.