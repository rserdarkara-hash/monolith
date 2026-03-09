# Specification: Banner Integration (v0.8.7)

## Overview
Add a branded banner (`banner.png`) to the header of the Monolith Spatial Soil Analysis Dashboard. This banner will replace the existing text-based title ("Monolith") and serve as the primary brand element across all application themes.

## Functional Requirements
- **Banner Loading:** Load the `banner.png` image and integrate it into the Shiny UI header.
- **Placement:** The banner MUST be left-aligned within the header container.
- **Title Replacement:** The existing "Monolith" text title must be removed.
- **Responsiveness:** The banner MUST scale proportionally when the window is resized or viewed on smaller screens to prevent layout breakage.
- **Theme Consistency:** The banner must be styled to fit seamlessly within all existing application themes (Dark, Light, etc.).
- **Styling Alternatives:** Provide at least two styling options (e.g., a subtle border or drop shadow) that adapt to the active theme's colors.

## Acceptance Criteria
- [ ] `banner.png` is displayed in the top-left corner of the application.
- [ ] The text "Monolith" is no longer visible in the main header.
- [ ] The banner scales down proportionally on mobile/narrow viewports.
- [ ] The banner looks integrated and professional in all 4+ themes.
- [ ] No regression in the existing "Governing Factors" or "Map Viewer" functionalities.

## Out of Scope
- Interactive banner elements (links, buttons).
- Multiple banner images for different themes (unless necessary for visibility).
