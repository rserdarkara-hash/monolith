# Specification: Interactive Map Panning by Locality

## 1. Overview
Introduce a new interactive UI element—a dropdown menu located next to the Base Map dropdown within the interactive Leaflet Map Viewer. This dropdown will allow users to quickly pan the map's viewport to focus on specific mapped localities dynamically after initial map generation.

## 2. Functional Requirements
- **Locality Dropdown:** Add a new `selectInput` (or `pickerInput`) next to the Base Map dropdown in the Map Viewer tab.
- **Dynamic Options:** The dropdown must populate with all unique localities present in the active dataset.
- **Default Option:** The dropdown will feature a default "Global View" option, representing the full bounding box of all localities combined.
- **Panning Action:** Upon selecting a specific locality, the Leaflet map(s) must instantly jump (no animation) to the geographic bounding box of that locality.
- **Reset Action:** Selecting "Global View" must return the map to the initial state, where all localities are fully visible.
- **Dual View Synchronization:** In dual view mode, selecting a locality from the dropdown must synchronously pan both the primary and comparison Leaflet maps to the same bounding box simultaneously.
- **Conditional Rendering:** If the dataset contains only one locality, the dropdown must be completely hidden or disabled to avoid redundant UI clutter.

## 3. Non-Functional Requirements
- **Performance:** The panning operation should not trigger a full map redraw or recalculation of models; it must strictly manipulate the Leaflet viewport bounds using `leafletProxy`.
- **UI Consistency:** The new dropdown should match the existing aesthetic and sizing of the Base Map dropdown.

## 4. Acceptance Criteria
- [ ] The new dropdown appears next to the Base Map selection in the UI.
- [ ] The dropdown is hidden if only one locality is loaded.
- [ ] Selecting a specific locality instantly pans the map to fit that locality's bounds.
- [ ] Selecting "Global View" pans the map to the initial global bounds.
- [ ] In dual-view mode, both maps pan synchronously.
- [ ] The underlying data, markers, and layers remain intact during panning.

## 5. Out of Scope
- Implementing custom coordinate panning (user-typed lat/lon).
- Storing panning history.