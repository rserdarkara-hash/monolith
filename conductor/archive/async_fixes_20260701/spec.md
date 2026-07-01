# Track Specification: Resolve Parallel Worker Serialization and Async Race Conditions

## Overview
This track addresses three critical/high-severity issues identified in `issues.md` regarding Monolith's asynchronous processing and parallel execution environment. These issues cause serialization failures on parallel workers, potential data corruption from overlapping model runs, and stale results overwrite after cancellation.

## Key Requirements
1. **Parallel Worker Serialization**:
   * Drop the `get_regional_param` closure from the parallel worker globals export lists in `monolith_ver_0.9.8b.R` (specifically lines 3760, 3772, and 3802).
   * Verify that the worker does not require this closure since it is resolved on the main thread prior to dispatching the future.

2. **Parallel Worker Working Directory**:
   * Fix the dead variable `main_wd` (line 3746) by ensuring the worker's working directory is explicitly set to `main_wd` prior to sourcing `spatial_helpers_0.9.8b.R`, or use an absolute path.

3. **Overlapping Model Runs**:
   * Implement a re-entrancy check at the top of `observeEvent(rv$proceed_run, ...)` to ensure that no new run is dispatched if `rv$model_running` is `TRUE`.
   * Display a warning notification using `showNotification` when an overlap is blocked.

4. **Async Cancellation and Stale Results**:
   * Introduce a session-based run token (`rv$run_token`).
   * Increment the run token upon dispatching a new model run.
   * Capture the token locally in the reactive environment.
   * Check in the asynchronous `%...>%` success and `%...!%` error callbacks if the local token matches the active session token before updating reactive values or modifying the UI.
