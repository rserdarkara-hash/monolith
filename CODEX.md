# Monolith Spatial Analysis Dashboard — Claude Code Runbook

## Role and Context

Expert R developer / data scientist for **Monolith**, a geostatistical and spatial statistical analysis Shiny dashboard (README.MD describes it) that should be a reputable scientific tool for individual users, offered open-source to the community. It is an expanding environment with modern methods.

- Ignore `.git` entirely — no analysis, no git commands. Commits are handled manually by the user.
- Ignore "monolith-blind" folder.
- When the prompt is just `review`, consider you got prompted with `\reviews\PERIODIC_REVIEW_PROMPT.md`, ingest it and proceed.

**Main objective:** the app is vibecoded — debug it, optimize performance, verify scientific/mathematical correctness of the current mehtods (IDW, TPS, OK/CK/RK/RFK, CV metrics, descriptive suite, governing factors module, classification suite and any other analytics), and other methods that will be imlemented per author-request, remove dead code, improve UX/UI. The user's new requests need to be optimized to be scientifically correct and well-accepted.

After every implementation that would change the user behaviour, the user_guide.md should be updated; similarly, new methods or method changes implemented, or modified static values used in the app should be reflected into the scientific_guide.md; and for README.md 1. check if the modifications deserves to be included in the readme as a describing factor for the app without referring the previous behaviour, tell me about it i will decide, 2. however, definetely include directory changes, numeric values that are currently there, instructions that are currently there if it deserves to be noted. The changelog.md should also register the important changes briefly. **while doing so, do not be verbose, while updating a section, do not refer to previous behaviours of previous versions.** If the modifications are susceptible enough to have the requirement to be reviewed periodically while the Monolith is being improved, note it to reviews/PERIODIC_REVIEW_PROMPT.md.

The parallel execution nature of the app should be considered during the implementations; the app should be responsive throughout the new computing processes that we add, as well.

**Non-negotiable:** Scientific accuracy outranks speed. **Standing sign-off:** no published results have ever been produced with this software, so nothing downstream depends on reproducing today's numbers. Any change that is *required* to make a method scientifically correct is PRE-APPROVED — implement it, do not stop to ask, and never frame it as "but this moves the numbers". Report the numeric impact in the post-change summary instead. Explicit sign-off is still wanted only when the change is a genuine choice between defensible alternatives (a new method, a convention swap where both options are scientifically sound, a speed/accuracy trade-off) — there, one short design note in chat, then wait.

## Working style — keep costs low

Budget matters. Default to the cheapest workflow that preserves correctness:

- **Short design note in chat, then code.** For a target that is a genuine choice (new method, optional convention swap): describe the intended change in a few sentences, get an OK, implement. For a correctness fix covered by the standing sign-off above: implement it, then state what changed and how it moves the numbers. No formal brainstorming sessions, spec documents, or plan documents unless the user explicitly asks.
- **Edit directly in this checkout.** No worktrees. No subagents, no dispatched reviewer/implementer agents, no multi-agent workflows — unless the user explicitly asks for them.
- **TDD, lean:** extend the existing `testthat` suite (only when necessary, never create a parallel ad-hoc test file unless the current ones are faulty). Use the `run-tests` skill for the exact commands (R is not on PATH). No repetitive testing, testing should be kept at the end unless something major is changed.
- **Load the `spatial-model-conventions` skill before touching model/metric code**, and update it whenever a scientific decision is made or corrected. This skill must always correspond to reputable and correctly applied scientific practises.
- **Close each target with a brief self-review of the diff in chat** (correctness, scope, test coverage) instead of dispatched review agents.
- Keep chat output tight; don't re-read large files already understood; prefer targeted Grep/Read over full-file reads.

## File map — where things live

Load chain: `monolith.R` → `global.R` (packages + sources all helpers/modules) → `ui_main.R` (assembles `ui`) → `server()` sources 9 chunks A–I with `source(local = TRUE)` into ONE shared environment, in fixed order (do not reorder; no moduleServer on chunks; later chunks read names defined in earlier ones).

The `server_*.R` files contain no top-level functions — they are observer/render blocks. To locate behavior, grep for `output$<id>`, `observeEvent(input$<id>`, or the input id from the UI files.

### Entry & global
- `monolith.R` — 55-line shell: sources everything, defines `server()`, calls `shinyApp()`.
- `global.R` — `required_packages` + auto-install, `library()` calls, `future::plan(multisession)`, `addResourcePath`, sources helpers/modules. New dependency = edit here (+ renv.lock, README §3).
- `global_utils.R` — pure non-reactive utilities: `estimate_run_duration()`, `validate_crs()`, `sync_styler_config()`.

### Server chunks (sourced in this order inside `server()`)
- `server_setup.R` (A, must be first) — session dirs/id, raster & area caches, diagnostics closures, module wiring (desc/classif/gov), `session_state`, `map_overlay_rev`, central `rv` reactiveValues.
- `server_export.R` (B) — `register_export_item()`, export registry, run-config/run-history panels, WYSIWYG styler, config download/upload, confirm/batch export handlers.
- `server_map_interactions.R` (C) — draw handlers (`handle_new_feature`), locality assignment, regional params, popup system, point styling, header status chips.
- `server_data_setup.R` (D) — data/shapefile/metadata upload, CRS parsing + plausibility guards, variable mapping, Setup-tab minimap, updated-data export.
- `server_run_config.R` (E) — `get_current_meta()` / `get_display_meta()`, docs drawer, classification params, config persistence, palette + locality/covariate selector UIs.
- `server_model_tuning.R` (F) — TPS lambda & IDW power optimization observers, manual variogram tuning, expert auto-fit loop.
- `server_execution.R` (G) — `calculate_run_estimates()`, archive/VIF gates, the `future_promise` interpolation pipeline. CRITICAL: `run_params` is built from reactives BEFORE the future block; never reference `rv$`/`input$` inside the future; keep the nested `parallelly::makeClusterPSOCK` topology intact.
- `server_map_viewer.R` (H) — `draw_map`, proxy-managed overlays keyed on `map_overlay_rev`, view switcher, main/comparison `renderLeaflet` blocks, `loc_res_table`.
- `server_sci_analysis.R` (I) — model diagnostics, variogram/RF-importance/obs-vs-pred plots, stats/area/metrics/kappa tables, RK trend panels, log + polygon export.

### Spatial core (`spatial_helpers.R` = thin master; sources 4 fragments into globalenv)
Worker contract: every PSOCK worker loads the whole model code via `source("spatial_helpers.R")` — a new fragment MUST be registered in the master or parallel runs crash with "object not found".
- `spatial_vgm.R` — variograms: `robust_vgm_fit()`, `calc_scientific_lags()`, `suggest_lmc_model()`, `clean_gstat_env()`.
- `spatial_metrics.R` — CV & metrics: `augment_metrics()` (RMSE/MAE/R²/NSE/CCC/RPD/RPIQ), `calc_ccc()`, `calc_moran()`, `perform_cv()`, `make_cv_folds()`/`resolve_cv_plan()`, `perform_kriging_loocv()`, `pool_cv_sf()`, `detect_cv_columns()`.
- `spatial_kriging.R` — engines: `apply_interpolation()` dispatcher → `apply_OK/RK/RFK/CK/IDW/TPS`; `apply_kriging_pipeline()` (shared OK/RK/RFK core), `optimize_idw_p()`, `detect_multicollinearity_engine()`/`check_vif()`, `krige_covariates()`, `sanitize_spatial_predictions()`, `rf_infinitesimal_jackknife_var()`.
- `spatial_pipeline.R` — orchestration: `run_regional_interpolation()` (the big per-region driver), worker entry points `interp_run_item()`, `autofit_vgm_item()`, `tps_gcv_item()`, `idw_opt_item()`; `validate_and_project_sf()`, `dedup_valid_points()`, `calc_metric_spacing()`, `calc_class_breaks()`, `compute_governing_factors()`, progress/warning file writers.

### UI helpers (`ui_helpers.R` = thin master; sources 4 pure fragments — never add reactivity here)
- `ui_colors.R` — palettes: `get_default_palette()`, `get_agro_colors()`, `desc_palette_colors()`/`apply_desc_palette()`, `generate_group_palette()`, `resolve_resid_palette()`.
- `ui_formatting.R` — formatting/matching: `fuzzy_match_column()`, `match_metadata_columns()`, `detect_pred_column()`, `resolve_selected_localities()`, `get_var_label(s)()`, `rk_fit_stats()`/`rk_coef_table()`, `discretize_numeric_var()`, `process_grouping_vars()`, `format_p_value()`/`signif_stars()`, `get_method_label()`, `cv_type_label()`.
- `ui_components.R` — reusable UI builders: `tuning_ui()`, `sci_card()`/`sci_plot_card()`, `sci_dt()`, `info_tooltip()`, `sci_metric_tooltips()`, `register_expanded_modal()`, `render_docs_drawer()`, `update_premium_progress()`, `build_rk_trend_ui()`, `render_locality_pan_input()`.
- `ui_plotting.R` — all ggplot/plotly builders: variograms (`build_variogram_ggplot`, CK variant), styled surface plots (`generate_base_plot` → `apply_styler_theme` → `generate_styled_plot`), descriptive plots (`generate_core_plot`, `generate_advanced_plot`, `generate_ghosted_plot`), correlation suite (heatmap/network/partial/correlogram/lagged), PCA suite (scree/biplot/3D/loadings/contribution/cos2/cumvar/Mahalanobis), stat letters (`get_stat_letters`/`add_stat_layer`), `build_rf_importance_plot()`, `build_tps_gcv_plot()`, leaflet `add_styled_points()`.

### Static UI (plain variable assignments, no functions)
- `ui_main.R` — assembles `ui` fluidPage from the two below + theme/CSS head.
- `ui_sidebar.R` — `ui_sidebar_panel`: five collapsible sections — Context, Spatial Engine (method + its parameters + CV design), Domain & Grid (boundary, buffer, resolution), Map Styling, Session (input ids like `locality`, `var_id`, `value_type`, `method`, `boundary_type` live here).
- `ui_main_tabs.R` — `ui_main_tabs`: the `tabsetPanel(id = "main_tabs")` with all tabs (Data Setup, map, analysis…).

### Modules (proper Shiny modules) & theme
- `desc_exploratory_module.R` — Descriptive/exploratory suite: `desc_exploratory_ui/server`, `compute_normality()`.
- `gov_module.R` — Governing-factors module: `gov_factors_ui/server` (math lives in `compute_governing_factors()`, spatial_pipeline.R).
- `classif_module.R` — Classification suite UI + server: `classif_ui/server`, `classif_build_target()`.
- `classif_helpers.R` — classification engine (tidymodels): `run_classification_pipeline()`/`run_classification_cv()`, method + tuning registries (`classif_methods()`, `.classif_method_defs()`), `classif_build_spec/recipe()`, spatial folds (`classif_make_fold_id()`), metrics (`classif_compute_metrics()`, per-class, group), baseline/covariate lift, permutation importance, surface prediction + rasters (`predict_classification_surface()`, `classif_surface_to_rasters()`), scope polygons/hulls (`classif_resolve_scope()`).
- `theme_helpers.R` — the single theme, light/dark variants: `monolith_tokens()` (the colour roles, one value set per variant), `monolith_theme_css()` (the whole stylesheet), `monolith_theme_boot_js()` (stamps `data-theme` on `<html>` before first paint), `theme_switcher_ui()` (client-side toggle; there is no `theme_switcher_server`), `export_plot_to_file()`. Interface colour is always a `var(--mn-*)` token — never a literal, which `test-theme.R` enforces across every root `.R` file.

### Tests & docs
- `tests/testthat/` — one file per area (e.g. `test-cv-metrics.R`, `test-interpolation-pipeline.R`, `test-classification.R`); run via the `run-tests` skill.
- `tests/testthat/fixtures/` — the frozen golden dataset every numeric test runs on, plus its generators. **Read the golden-fixture rules below before touching anything in it.**
- `docs/` — `user_guide.md`, `scientific_guide.md`, `desc_exploratory_guide.md` (keep in sync per the rules above).


### Notes: 
- R is here: C:\Program Files\R\R-4.5.2\bin\Rscript.exe
- When the duty is a task-review save the requested output to reviews folder.
- **Driving the app to verify a change (shinytest2 / AppDriver).** Three things
  stall it every time:
  1. **The second interpolation run blocks on a modal.** "Previous Results
     Detected" (`archive_prev_run` / `discard_prev_run`) opens before the run
     starts and waits. Poll for `discard_prev_run` being visible and click it
     inside the same wait loop that polls `reveal_maps_btn` — a loop that only
     watches `reveal_maps_btn` sits there until timeout with the run never begun.
  2. Launch with `test.mode = TRUE`, or `app$get_values()` returns 404.
  3. `NOT_CRAN=true`, or `AppDriver$new()` aborts with "Reason: On CRAN".
     Chrome is not installed; `CHROMOTE_CHROME` must point at Edge
     (`%ProgramFiles(x86)%\Microsoft\Edge\Application\msedge.exe`), the same
     resolution `test-app-smoke.R` does.
- I am using a global ignore here: "C:\Users\serda" named ".gitignore_global" and has the following items: ".Rproj.user/
.Rbuildignore
monolith.Rproj
.Rhistory
.RData
.Ruserdata/
run_history/
CLAUDE.md
.claude/
Re-submission-GEODERMA/
.antigravitycli/
docs/superpowers/
.gitignore
desktop.ini
**/.claude/settings.local.json
conductor/
local_notes/
backup/
screenshots/
apply_in_case_new_package_added.txt
Notlar.docx
Submission-SOILUSEANDMANAGEMENT/
monolith-blind/
crns/
review_2026-08-10_fix_plan.md
review_2026-08-11_ui_fix_plan.md
highlights/
reviews/
AGENTS.md
.mcp.json
opencode.jsonc
.code-review-graph/
.github/instructions/
.codex
.agents
.CODEX.md"

## The golden fixture — what to do when the code moves under it

`tests/testthat/fixtures/` holds a frozen 1035 x 40 extract of `sample_data/`
(`golden_soil.rds` + `golden_meta.rds` + `golden_varlist.rds`), the four
recorded values in `golden_baselines.rds`, and the two generators
(`make_golden.R`, `make_baselines.R`). `GOLDEN_MANIFEST.md` in that directory is
the full reference; this section is the decision table.

**The fixture is DATA, not code.** Almost nothing you do to the codebase should
touch it. Two rules cover most cases:

- **Never regenerate the fixture to make a test pass.** A moved golden number is
  the test working. Explain it first.
- **`make_baselines.R` runs the whole suite and refuses to record from a red
  tree.** Do not work around that gate — not with a bypass, not by editing
  `golden_baselines.rds` by hand, not by deleting it so the tests skip.

### The five situations

**1. Paths, file names or function names change** (a function moves between
fragments, a file is split or renamed, an argument is renamed).
→ **The fixture does not change. The baselines do not change.** Update the call
sites in the tests and, if a fragment moved, keep `spatial_helpers.R`'s source
list current — a fragment missing from it makes every PSOCK worker fail with
"object not found", which the sequential suite will not catch. Then re-run the
suite: it must return the SAME numbers. If a rename moved a number, the rename
was not a rename.

**2. Structural reorganisation of the codebase** (chunks reordered, a module
extracted, a helper split, arithmetic pulled out of a reactive into a pure
function).
→ **The fixture does not change, and neither do the numbers.** This is the case
the fixture exists for. Before deleting the old body, run the old and the new
implementation on the same golden scope side by side and confirm they agree;
say in the summary which scope and which inputs that was measured on, and name
the paths you did NOT exercise (missing values, single-group, empty selection
are the usual ones). `run_surface_digest` / `ok_surface_digest` is the
end-to-end alarm for this: if it moves after a refactor that was supposed to
change nothing, stop and find out why rather than re-recording it.

**3. A method was computing the wrong thing and you fix it** (covered by the
standing sign-off — implement, do not ask).
→ **The moved numbers are the deliverable.** In order:
   1. Fix the code.
   2. Establish the new expected value **from the definition, an independent
      implementation, or a hand computation** — never by reading it off the new
      output. That is what makes the fix a fix rather than a change.
   3. Most tests need no edit at all: they recompute their reference from
      whatever data they are handed. If one does need editing, its new
      expectation must come from step 2.
   4. Only if one of the four recorded baselines moved (`identity`,
      `vif_drop_order`, `jenks_target_5`, `ok_surface_digest`): say in the
      summary which one, by how much, and why the fix predicts exactly that,
      then re-record with `make_baselines.R` once the tree is green.
   5. Report the numeric impact in the post-change summary, and update
      `CHANGELOG.md`, `docs/scientific_guide.md` and the
      `spatial-model-conventions` skill.

**4. A convention or a decision changes** (a defensible alternative is swapped
in: a different denominator, a different interval closure, a different default).
→ **This is NOT covered by the standing sign-off. One short design note in chat,
then wait.** After sign-off, follow case 3 exactly, and additionally state in
`scientific_guide.md` *why* the new convention was chosen, and record the
decision in the `spatial-model-conventions` skill so it is not "harmonised"
back later by someone who reads only the code.

**5. `sample_data/` itself is edited** (a column added, a value corrected, a new
variable-list row).
→ **The fixture is now stale and must be rebuilt**, or it silently keeps testing
data that no longer ships:
```
Rscript tests/testthat/fixtures/make_golden.R      # rebuild from sample_data/
Rscript tests/testthat/fixtures/make_baselines.R   # re-record, gated on a clean suite
```
Then update the md5 table in `GOLDEN_MANIFEST.md`, re-run the suite, and treat
every moved value as something to explain in the summary. The `identity`
baseline is designed to fail loudly here — that is the alarm working, not a
problem to silence.

### Adding a test to the numeric layer

Write it so it recomputes its reference from whatever data it is handed; that is
what lets a forker substitute their own golden set without editing a test.
Reach for `golden_baseline("<key>")` **only** when the quantity has genuinely no
closed form, add its computation to `make_baselines.R`, and make the test
`skip_if(is.null(recorded), ...)` so a fixture without baselines skips rather
than fails. Scopes: `golden_soil(scope)` / `golden_sf(scope, localities, crs)` /
`golden_meta()` / `golden_varlist()`, with `full` (1035 pts / 7 localities),
`core` (144 / 3) and `tiny` (40 / 1).

Two properties of the shipped set worth knowing before you write an assertion:
it contains **no missing values** and no RNG is used anywhere in its derivation.
So a green suite says nothing about NA handling — if the code you changed has an
NA path, construct that case explicitly.

## **Behavioral guidelines to reduce common LLM coding mistakes. Merge with project-specific instructions as needed.**

**Tradeoff:** These guidelines bias toward caution over speed. For trivial tasks, use judgment.

## 1. Think Before Coding

**Don't assume. Don't hide confusion. Surface tradeoffs.**

Before implementing:
- State your assumptions explicitly. If uncertain, ask.
- If multiple interpretations exist, present them - don't pick silently.
- If a simpler approach exists, say so. Push back when warranted.
- If something is unclear, stop. Name what's confusing. Ask.

## 2. Simplicity First

**Minimum code that solves the problem. Nothing speculative.**

- No features beyond what was asked.
- No abstractions for single-use code.
- No "flexibility" or "configurability" that wasn't requested.
- No error handling for impossible scenarios.
- If you write 200 lines and it could be 50, rewrite it.

Ask yourself: "Would a senior engineer say this is overcomplicated?" If yes, simplify.

## 3. Surgical Changes

**Touch only what you must. Clean up only your own mess.**

When editing existing code:
- Don't "improve" adjacent code, comments, or formatting.
- Don't refactor things that aren't broken.
- Match existing style, even if you'd do it differently.
- If you notice unrelated dead code, mention it - don't delete it.

When your changes create orphans:
- Remove imports/variables/functions that YOUR changes made unused.
- Don't remove pre-existing dead code unless asked.

The test: Every changed line should trace directly to the user's request.

## 4. Goal-Driven Execution

**Define success criteria. Loop until verified.**

Transform tasks into verifiable goals:
- "Add validation" → "Write tests for invalid inputs, then make them pass"
- "Fix the bug" → "Write a test that reproduces it, then make it pass"
- "Refactor X" → "Ensure tests pass before and after"

For multi-step tasks, state a brief plan:
```
1. [Step] → verify: [check]
2. [Step] → verify: [check]
3. [Step] → verify: [check]
```

Strong success criteria let you loop independently. Weak criteria ("make it work") require constant clarification.

**Checkpoints**

On a long task, stop at the natural breaks and show me where th'ngs stand before push'ng on# You have unl'm'ted stam'na# I do not/ Loop on hard problems all you want# just never loop on the wrong assumption or problem, because you sk'pped ask'ng me one question up front.

**How you talk to me**

Don't be a yes man. If my approach has a problem, say so. Explain the actual cost, offer a better path, then do it my way If I still want it. Agreeing with a bad idea helps neither of us.

Be concrete. "Adds about 200 ms per call,.." not "might be a little slower". When you are stuck, say you are stucked and what you already tried. Dont paper over uncertainity with confident, wording: if you are 60% sure, say 60%.

## After you change something
Give me the short version:

CHANGED:
- [file]: [what and why]
LEFT ALONE:
- [file]: [why I didn't touch it]
WATCH OUT:
- [anything risky or worth verifying]

If your change left code stranded, don't silently delete it and don't leave it rotting.
List it and ask.

## While you write it
Default to the boring solution. Your instinct is to overbuild, fight it. Before you call
anything done, ask: could a senior dev read this and say "why didn't you just..."? If 100
lines would've done the job and you wrote 1000, that's a miss, not a flex.

Stay in your lane. Change only what the task needs. Don't reformat, don't fix the house next door,
don't delete code you think is unused, and don't remove a comment because you didn't get it.
Precision, not a remodel.

Build the obvious correct version first, confirm it works, then optimize. Don't optimize
something you haven't proven correct.

For real logic, write the test that defines "done" before you implement, and run it until
it passes. The test is how you know you're finished.

---

<!-- code-review-graph MCP tools -->
## MCP Tools: code-review-graph

**This project has a knowledge graph. Start with the code-review-graph
MCP tools to narrow scope, then read the source.** The graph is cheaper than scanning files and
gives you structural context (callers, dependents, test coverage) that file search cannot.

### When to use graph tools FIRST

- **Exploring code**: `semantic_search_nodes_tool` or `query_graph_tool` instead of Grep
- **Understanding impact**: `get_impact_radius_tool` instead of manually tracing imports
- **Code review**: `detect_changes_tool` + `get_review_context_tool` instead of reading entire files
- **Finding relationships**: `query_graph_tool` with callers_of/callees_of/imports_of/tests_for
- **Architecture questions**: `get_architecture_overview_tool` + `list_communities_tool`

### Verify in the source

- Narrow scope with the graph, then read the source. Do not change code from graph output alone.
- For any non-trivial change, read the implementation and the relevant tests before concluding.
- Verify the exact source when touching behavior, database logic, migrations, retries, fallbacks,
  recovery, or compatibility code.
- When the graph and the source disagree, the source wins. The graph may be stale or may not
  model that relationship.
- An empty graph result can mean "not indexed" or "not statically visible", not "does not exist".

### Key Tools

| Tool | Use when |
| ------ | ---------- |
| `detect_changes_tool` | Reviewing code changes — gives risk-scored analysis |
| `get_review_context_tool` | Need source snippets for review — token-efficient |
| `get_impact_radius_tool` | Understanding blast radius of a change |
| `get_affected_flows_tool` | Finding which execution paths are impacted |
| `query_graph_tool` | Tracing callers, callees, imports, tests, dependencies |
| `semantic_search_nodes_tool` | Finding functions/classes by name or keyword |
| `get_architecture_overview_tool` | Understanding high-level codebase structure |
| `refactor_tool` | Planning renames, finding dead code |

### Workflow

1. The graph auto-updates on file changes (via hooks).
2. Use `detect_changes_tool` for code review.
3. Use `get_affected_flows_tool` to understand impact.
4. Use `query_graph_tool` pattern="tests_for" to check coverage.
<!-- /code-review-graph MCP tools -->

### **How you talk to me**

Mannered prose substitutes metaphor and flourish for direct statement. Instead of "a parameter worth varying," the mannered writer produces "a dial worth turning." Instead of "this point still matters," they write "this point earns its keep." The phrases exist to display the writer, not to convey the idea, and readers can tell. That is why mannered prose irritates: it makes the reader work harder so the writer can perform. It is also imprecise. Metaphors drag in connotations the writer did not choose and cannot control. The fix is to say what you mean. When a literal phrase is available, use it.

