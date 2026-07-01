# Specification: Resolve Map Viewer Overlay, Documentation Drawer, and About Modal Layering Conflict

## Overview
The map viewer's processing/reveal overlay (`.map-processing-overlay`) currently has a `z-index` of `2000`. The documentation/guides drawer (`.docs-drawer`), which slides in from the right, has a `z-index` of `1050`. The default Bootstrap modals (`.modal` and `.modal-backdrop`) also have z-index values (`1050` and `1040` respectively) that are lower than the map viewer overlay. Consequently, when the map viewer cover is active, it obscures both the documentation drawer and the "About" modal dialog.

This track resolves these layout conflicts by ensuring that the documentation drawer and modal dialogs are always layered at the top (above the map processing overlay) when active.

## Functional Requirements
1. Modify the CSS z-index of `.docs-drawer` to `2500` (which is greater than `.map-processing-overlay`'s `2000`, but lower than system notifications' `99999`).
2. Modify the CSS z-index of `.modal` to `2610` and `.modal-backdrop` to `2600` (so modals overlay the documentation drawer and map viewer cover correctly).
3. Ensure the drawer and modal sit on top of the cover overlay in all map viewer states:
   - **Awaiting Spatial Interpolation** phase (initial state).
   - **Running/Processing** phase (while parallel interpolation is running).
   - **Reveal Maps & Enable Analysis** phase (when the reveal button is displayed).
   - **Interpolation Cancelled** state.
4. The map processing overlay should remain visible in the background and be properly covered by the active drawer/modal.
5. System notifications should remain layered on top of all elements.

## Non-Functional Requirements
- Maintain smooth transition animations (the drawer's 0.3s slide-in transition and modal fade animations should remain unaffected).
- Keep CSS theme variables compatibility.

## Acceptance Criteria
- In all map viewer pre-reveal states, opening the documentation/guides drawer shows the drawer fully visible on top of the cover.
- In all map viewer pre-reveal states, clicking the "About" icon button displays the modal and its backdrop cleanly on top of the cover.
- The documentation drawer has `z-index: 2500`.
- The modal dialog has `z-index: 2610 !important;` and its backdrop has `z-index: 2600 !important;`.
- System notifications are still visible on top of everything.

## Out of Scope
- Modifying the styles, colors, or structure of the documentation drawer, modal layout, or map viewer.
- Altering the trigger logic for the map reveal overlay, drawer, or modal.
