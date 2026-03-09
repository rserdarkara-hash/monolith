# Implementation Plan - Export Panel Improvements (v2.3)

This plan outlines the steps to upgrade the Export Panel with advanced customization, dynamic previews, and multi-format support.

## Phase 1: Environment Setup & Safety [checkpoint: 4117bbc]
- [x] Task: Backup existing version (v2.2_theme) files to `backups/pre_v2.3_export_upgrade_20260302/` 6c09d2e
- [x] Task: Create `app_v2.3.R` and helper copies (`spatial_helpers_v2.3.R`, `ui_helpers_v2.3.R`) 8f10190
- [x] Task: Update `app_v2.3.R` to source the new v2.3 helpers 8f10190
- [x] Task: Conductor - User Manual Verification 'Phase 1: Environment Setup & Safety' (Protocol in workflow.md) 4117bbc

## Phase 2: Core Export Registry & UI Scaffold [checkpoint: 6044d7b]
- [x] Task: Implement a reactive value/list to track all exportable objects (plots and tables) across the session 6044d7b
- [x] Task: Create the basic UI for the new Export Panel (Sidebar or dedicated Tab) with a list of session assets 6044d7b
- [x] Task: Implement the "Styler" UI component (Typography, Spacing, Quality inputs) 6044d7b
- [x] Task: Conductor - User Manual Verification 'Phase 2: Core Export Registry & UI Scaffold' (Protocol in workflow.md) 6044d7b

## Phase 3: Dynamic Preview & Modal Logic [checkpoint: 6044d7b]
- [x] Task: Implement `shiny::modalDialog` for the detailed export view 6044d7b
- [x] Task: Build the dynamic preview engine (renders a temporary high-res image based on Styler inputs) 6044d7b
- [x] Task: Add debouncing to Styler inputs to optimize preview performance 6044d7b
- [x] Task: Conductor - User Manual Verification 'Phase 3: Dynamic Preview & Modal Logic' (Protocol in workflow.md) 6044d7b

## Phase 4: Multi-Format Export Engines [checkpoint: 6044d7b]
- [x] Task: Implement Plot Export (PNG, TIFF, PDF, JPEG) using `ggplot2::ggsave` with Styler parameters 6044d7b
- [x] Task: Implement Excel Export (.xlsx) using `openxlsx` (multi-sheet for different tables) 6044d7b
- [x] Task: Implement Word Export (.docx) using `officer` (formatted tables + plot embedding) 6044d7b
- [x] Task: Implement CSV Export for raw/processed tabular data 6044d7b
- [x] Task: Conductor - User Manual Verification 'Phase 4: Multi-Format Export Engines' (Protocol in workflow.md) 6044d7b

## Phase 5: Batch Operations & Integration [checkpoint: 6044d7b]
- [x] Task: Add "Select All" and "Export Selected" functionality to the asset list 6044d7b
- [x] Task: Integrate "Quick Export" buttons into Map and Analysis tabs 6044d7b
- [x] Task: Final theme synchronization (ensure Export Panel matches all 10 themes) 6044d7b
- [x] Task: Audit and Fix Contrast/Readability for all 10 themes (Address light-on-light/dark-on-dark issues) 6044d7b
- [x] Task: Register all Analysis Tables (Performance, Area, Classification, Stats) for export 6044d7b
- [x] Task: Register all Analysis Plots (Variograms, Scatterplots, Residual Variograms) for export 6044d7b
- [ ] Task: Fix hardcoded text colors (Resolution Note, etc.) to match theme body text
- [ ] Task: Implement "Combined Comparison Map" registration and Styler support
- [ ] Task: Refactor Batch Export to unify all selected tables into a single multi-sheet Excel file
- [ ] Task: Increase default font sizes for Batch Export plots
- [ ] Task: Conductor - User Manual Verification 'Phase 5: Batch Operations & Integration' (Protocol in workflow.md)
