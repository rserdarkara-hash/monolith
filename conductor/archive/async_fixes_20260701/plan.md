# Implementation Plan: Resolve Parallel Worker Serialization and Async Race Conditions

## Phase 1: Parallel Worker Environment & Serialization [checkpoint: d0b516d]

- [x] Task: Fix parallel worker serialization and working directory [360087b]
    - [x] Write verification script to simulate future/multisession serialization of helper globals
    - [x] Remove `get_regional_param` closure from exports in `monolith_ver_0_9_8b.R` (lines 3760, 3772, 3802)
    - [x] Resolve worker working directory issue by using absolute path or `setwd(main_wd)` before sourcing `spatial_helpers_0.9.8b.R`
    - [x] Run verification tests and confirm multisession workers initialize without errors

- [x] Task: Conductor - User Manual Verification 'Phase 1: Parallel Worker Environment & Serialization' (Protocol in workflow.md)

## Phase 2: Async Re-entrancy Guards and Cancellation [checkpoint: e6893fb]

- [x] Task: Implement re-entrancy guard for overlapping model runs [6861a0b]
    - [x] Write a verification plan to simulate rapid clicking on the interpolation run button
    - [x] Implement checks for `rv$model_running` at the top of `observeEvent(rv$proceed_run, ...)` and show warning notifications
    - [x] Verify that double clicks do not trigger overlapping runs

- [x] Task: Implement run token logic for async cancellation [6861a0b]
    - [x] Write a verification plan to simulate cancelling a model run while async tasks are active
    - [x] Implement `rv$run_token` session-level tracking and callback token matching in `future_promise` handlers
    - [x] Verify that stale results from cancelled runs do not write back to the active session state

- [x] Task: Conductor - User Manual Verification 'Phase 2: Async Re-entrancy Guards and Cancellation' (Protocol in workflow.md)
