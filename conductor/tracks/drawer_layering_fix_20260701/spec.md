# Specification: Resolve Map Viewer Overlay and Documentation Drawer Layering Conflict

## Overview
The map viewer's processing/reveal overlay (`.map-processing-overlay`) currently has a `z-index` of `2000`. The documentation/guides drawer (`.docs-drawer`), which slides in from the right, has a `z-index` of `1050`. Consequently, when the map viewer cover is visible, it obscures the guides drawer. 

This track resolves this layout bug by ensuring that the documentation drawer is always layered at the top (above the map processing overlay) when active.

## Functional Requirements
1. Modify the CSS z-index of `.docs-drawer` to `2500` (which is greater than `.map-processing-overlay`'s `2000`, but lower than `#shiny-notification-container`'s `99999`).
2. Ensure the drawer sits on top of the cover overlay in all map viewer states:
   - **Awaiting Spatial Interpolation** phase (initial state before running models).
   - **Running/Processing** phase (while parallel interpolation is running, showing spinner/progress bar).
   - **Reveal Maps & Enable Analysis** phase (when the button to reveal maps is displayed).
3. The map processing overlay should remain visible in the background and is properly covered by the documentation drawer.
4. System notifications should remain layered on top of all elements.

## Non-Functional Requirements
- Maintain smooth transition animations (the 0.3s slide-in transition of `.docs-drawer` should remain unaffected).
- Keep CSS theme variables compatibility.

## Acceptance Criteria
- In all map viewer pre-reveal states (awaiting interpolation, running interpolation, and awaiting manual reveal click), opening the documentation/guides drawer shows the drawer fully visible on top of the cover.
- The documentation drawer has `z-index: 2500`.
- System notifications are still visible on top of everything.

## Out of Scope
- Modifying the styles, colors, or structure of the documentation drawer or map viewer.
- Altering the trigger logic for the map reveal overlay or the drawer.
- Adjusting z-index of other unrelated modal elements.
