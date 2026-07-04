# Design: robust_vgm_fit non-converged / singular candidate handling

**Date:** 2026-07-04
**Target:** `robust_vgm_fit` (monolith.R:199–257) + the fallback banner in `draw_map` (monolith.R:4317–4323)
**Status:** Approved design, pending implementation plan

## Problem

`robust_vgm_fit` screens 16 candidate variogram fits (4 models × 4 starting
ranges) per call and picks the lowest-SSErr candidate. Two defects:

1. **Flawed fits can win selection.** `gstat::fit.variogram` reports singular
   fits via `attr(f, "singular")` and non-convergence via a C-level warning
   ("No convergence after 200 iterations"). Neither signal is checked, so a
   singular or non-converged fit can be selected purely on SSErr.
2. **Warning flood.** The expected screening warnings escape uncaught. The
   function runs once per CV fold inside `perform_kriging_loocv`, so the test
   suite emits 301 of its 314 warnings from this one site, burying real
   signals.

## Decisions (user-approved)

- **Selection policy:** prefer clean fits; flawed fits win only when no clean
  candidate exists, and are tagged.
- **Warning policy:** catch and muffle the known screening warnings inside
  `robust_vgm_fit`; record diagnostics on the returned fit; let unrecognized
  warnings propagate.
- **UI surfacing:** extend the existing map banner to list flawed-winner
  localities with amber "interpret with caution" wording.
- **Approach:** harden `robust_vgm_fit` in place (no structural refactor).

## Design

### Candidate classification

Wrap each `gstat::fit.variogram` call in `withCallingHandlers`:

- Warnings whose message matches `"No convergence after"`, `"singular
  model"`, or `"singular covariance"` mark the candidate non-converged and
  are muffled with `invokeRestart("muffleWarning")`. (All three patterns
  were observed empirically from `fit.variogram` during fixture discovery;
  the third — "linear model has singular covariance matrix" — appears on
  some degenerate candidates.)
- Any other warning propagates unchanged.
- Singularity is read from `attr(f, "singular")` (authoritative), in addition
  to the warning pattern.

A candidate is **clean** when it converged, is non-singular, and passes the
existing sanity window (unchanged): `range[2]` in `(max_dist/100,
2 * max_dist)` and `psill[2] > 0`. A returned fit that fails convergence or
singularity but passes the sanity window is **flawed**. A fit that fails the
sanity window is **discarded** regardless of convergence — same as today —
because a pathological range/sill is not a usable model even as a last
resort. Candidates that error also remain discarded, as today.

### Selection

1. Lowest SSErr among **clean** candidates.
2. Else lowest SSErr among **flawed** candidates; returned fit gets
   `attr(fit, "flawed_winner") <- TRUE`.
3. Else the existing heuristic Spherical fallback, unchanged, with
   `attr(fit, "is_fallback") <- TRUE`.

Consistency fix included: the early heuristic return for tiny empirical
variograms (`is.null(v_emp) || nrow(v_emp) < 5`) is now also tagged
`is_fallback <- TRUE` (it currently returns the same heuristic untagged, so
the banner never fires for it).

This is a sanctioned behavior change: selection differs from today exactly
where a flawed fit previously beat a clean one on SSErr.

### Diagnostics contract

Every fit returned by `robust_vgm_fit` carries
`attr(fit, "vgm_diagnostics")`, a list:

- `n_tried` — candidates that returned a fit (errors excluded)
- `n_flawed` — of those, how many were non-converged or singular
- `flawed_winner` — logical, mirrors the top-level attribute

Top-level attrs `flawed_winner` / `is_fallback` remain the UI keys.
`clean_gstat_env` strips only call/formula environments, so all attributes
survive serialization through parallel workers.

### UI banner (draw_map)

The existing loop over `rv$v_fit_list` that collects `is_fallback` localities
gains a second collection for `flawed_winner`. Both render in the same
dismissible `addControl` block:

- red (existing wording): variogram fit failed, heuristic fallback applied
- amber (new): "Auto-fit selected a non-converged or singular variogram for:
  &lt;keys&gt;. Interpret interpolations with caution."

Keys keep the existing `<locality>_<act|pre>` format for consistency with the
current banner. No other UI changes.

### Error handling / edge cases

- All 16 candidates error → heuristic fallback (unchanged path).
- `v_emp` NULL / tiny → heuristic early return, now tagged `is_fallback`.
- Warning handler scope is exactly the `fit.variogram` call — warnings from
  surrounding code are untouched.
- No change to function signature; all callers (`apply_kriging_pipeline`,
  `perform_kriging_loocv`, `krige_covariates`, `apply_CK`, auto-fit observer,
  `render_resid_plot`) work unmodified.

## Testing (red-green, extends existing suite)

New `tests/testthat/test-robust-vgm-fit.R`, reusing `helper.R` fixtures:

1. **No-warning contract (red first):** a noisy small-n empirical variogram
   that today reliably triggers non-convergence must produce no R warnings.
2. **Diagnostics contract:** `vgm_diagnostics` present and well-formed on all
   three return paths (clean winner, flawed winner, heuristic fallback).
3. **Flawed winner:** fixture with no clean candidates yields
   `flawed_winner = TRUE` and a usable vgm object.
4. **Clean preference:** fixture where a flawed candidate has lower SSErr than
   a clean one; the clean fit must be selected (fixture located by seeded
   search during implementation).
5. **Tiny-variogram tagging:** `nrow(v_emp) < 5` return carries `is_fallback`.

Suite-level acceptance: full run passes with warnings reduced from 314 to
≈13 (the residual DALEX/qtukey/pipeline warnings belong to other backlog
targets). Snapshot diffs, if any, must be scientifically verified before
accepting — kriging surfaces may legitimately change where a flawed fit
previously won.

## Out of scope

- Shrinking the 4×4 candidate grid or caching fits (backlog #9, performance).
- The qtukey NaN guard in `get_stat_letters` (backlog #7).
- Any change to LOOCV fold structure or metrics.
