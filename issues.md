 Found 9 issues, ranked by severity. Code refs use the uploaded filenames.
Critical
1. get_regional_param gets exported into the parallel worker, but it's a closure over rv/session state
monolith_ver_0_9_8b.R:1787 — get_regional_param is defined inside server() and closes over rv (rv[[field]][[loc]][[target]]). It's included by bare reference in force_globals/force_globals_nested (lines 3760, 3772) and by name in furrr::furrr_options(globals = c(..., "get_regional_param", ...)) (line 3802), which are used inside promises::future_promise({...}) / future_map(...) to force the parallel worker to receive it.
The problem: exporting this closure means future/globals has to walk its enclosing environment — which is the entire server() execution frame, including rv (a reactiveValues object tied to the live session), input, output, session. These are not meaningfully serializable across a multisession process boundary. This risks either an outright globals-export failure on every model run, or unnecessary bloat/slowdown from attempting to snapshot the whole server environment.
It's also unneeded: get_regional_param() is only ever called on the main thread (I checked every call site — df_list <- lapply(locs, function(l) {...}) at line ~3708 resolves it to plain numbers before future_promise starts, storing them in m_params). It is never called inside spatial_helpers_0.9.8b.R or anywhere that executes on the worker.
Fix: drop get_regional_param from all three export lists (3760, 3772, 3802). Nothing on the worker needs it.
High
2. No guard against overlapping model runs
observeEvent(rv$proceed_run, {...}) (line 3535) and observeEvent(input$run, {...}) (line 3343) have no check on rv$model_running before dispatching a new async run. shinyjs::disable("run") is a client-side cue only — a rapid double-click on "Run"/"Start Interpolation" before the disable reaches the DOM, or two clicks landing in the same reactive flush, can trigger two overlapping future_promise jobs. Both write into the same rv$rast_list_act, rv$sf, sf_list/b_list, etc.; the "C1: Clear rast lists" reset at the top of one run can wipe results mid-write from the other, producing corrupted, mixed-locality output with no error shown.
Fix: at the top of observeEvent(rv$proceed_run, {...}):
rif (isTRUE(rv$model_running)) {
  showNotification("A model run is already in progress.", type = "warning")
  return()
}
3. Cancel doesn't stop in-flight work — stale results can silently land after "Cancelled"
observeEvent(input$cancel_model_btn, {...}) (line ~4183) immediately sets rv$model_running <- FALSE, re-enables "run", and hides the progress UI. But the actual work is cooperative-cancellation only, checked in run_regional_interpolation at 3 points (top, before "act", before "pre" — never mid-fit). If a region's model (e.g. RFK with ntree=200, or CK's fit.lmc) is already past its last checkpoint when cancel fires, it keeps running, and its %...>% success callback fires later regardless — silently populating rv$rast_list_act, re-showing reveal_maps_btn, etc., as if the run had completed normally, even though the UI already told the user it was cancelled.
Fix: introduce a run token, e.g. rv$run_token <- rv$run_token + 1L at dispatch, capture it locally (this_token <- rv$run_token), and in both %...>% and %...!% callbacks check if (this_token != rv$run_token) return() before touching rv or UI.
Medium
4. [RESOLVED] TPS cross-validation backfills failed folds with in-sample fitted values — inflates reported CV metrics
spatial_helpers_0_9_8b.R:814-816:
rif(any(is.na(cv_vals))) {
  cv_vals[is.na(cv_vals)] <- mod$fitted.values[is.na(cv_vals)]
}
When a fold's fields::Tps refit fails, the code substitutes the full-model (in-sample) fitted value for that point, rather than leaving it NA. perform_cv() already drops NA obs/pred pairs safely (valid <- !is.na(observed) & !is.na(predicted)), so this substitution is unnecessary — and actively wrong: an in-sample fit from a model trained on that exact point will fit far better than any genuine held-out prediction, quietly inflating TPS's reported RMSE/R²/CCC whenever any fold fails. Given these numbers can end up in manuscript tables, this is worth fixing.
Fix: delete lines 814-816; let the NAs flow through and get excluded by perform_cv's existing filter.
5. [RESOLVED] SHAP contribution matching uses an unanchored, unescaped regex instead of exact match
spatial_helpers_0_9_8b.R:942 (inside compute_governing_factors):
rsub <- shap_df[shap_df$obs_id == i & grepl(paste0("^", top_var), shap_df$variable_name), ]
Two problems: (a) top_var is spliced into a regex unescaped — a name containing . (e.g. P.Mehlich3, common soil-science naming) is a wildcard, not a literal dot, so it can false-match unrelated columns; (b) there's no end anchor, so top_var = "N" also matches "N_ratio", "Nitrogen", etc., silently summing unrelated contributions into the Causality/Interaction (A) scatterplot. variable_name in DALEX's shap output is the clean variable name, so exact match is correct and simpler:
rsub <- shap_df[shap_df$obs_id == i & shap_df$variable_name == top_var, ]
6. [RESOLVED] Governing Factors module has no seed — feature importance and top-variable selection are non-reproducible
compute_governing_factors() (spatial_helpers_0_9_8b.R:891-956) fits randomForest::randomForest(...), runs DALEX::model_parts(..., B = n_permutations), and does sample_idx <- sample(1:nrow(df_clean), ...) — none seeded. Compare this to the kriging CV code (lines 263, 790), which explicitly does set.seed(12345) # for scientific reproducibility. As written, clicking "Run Analysis" twice on identical inputs can produce a different RF fit, different permutation importances, and potentially a different top_var, which cascades into different ALE/PDP/interaction plots. Given this feeds manuscript-adjacent output, add set.seed(12345) (or a matching convention) before the RF fit and before the SHAP sampling call.
7. [RESOLVED] Governing Factors "Run Analysis" has no re-entrancy guard
gov_module_0_9_8b.R:126 — observeEvent(input$gov_run_btn, {...}) doesn't check gov_rv$ready == "running" or disable the button, unlike the main interpolation "run" button. Repeated clicks dispatch multiple concurrent future_promise RF/DALEX jobs; whichever resolves last wins, with no indication to the user that earlier clicks were wasted or that results might not correspond to the last click.
rif (identical(gov_rv$ready, "running")) return()
plus shinyjs::disable(ns("gov_run_btn")) / re-enable in the %...>%/%...!% callbacks.
Low
8. Dead variable suggests an incomplete fix for worker working directory
monolith_ver_0_9_8b.R:3746: main_wd <- getwd() is captured but never referenced again. Meanwhile source("spatial_helpers_0.9.8b.R", local = FALSE) inside the future_promise block (line ~3749) uses a relative path, relying on the multisession worker's cwd matching the main session's. If a worker's cwd ever diverges (pool created before a setwd(), different launch cwd under Rscript/deployment), this source() fails and the whole run errors. Looks like setwd(main_wd) was meant to be added right before the source() call and got dropped. Either add that line or source with an absolute path (file.path(main_wd, "spatial_helpers_0.9.8b.R")).
9. [RESOLVED] Theme localStorage sync won't survive a Shiny reconnect
theme_helpers_0_9_8b.R:396-402 uses observeEvent(input$saved_theme_js, {...}, ignoreInit = FALSE, once = TRUE). ignoreNULL defaults to TRUE, so the observer correctly waits for the real JS-sent value rather than firing on the initial NULL — that part's fine. But once = TRUE means it self-destructs after that first real firing. If the session later reconnects (dropped websocket, Shiny's auto-reconnect), the client's shiny:connected handler fires again and re-sends saved_theme_js, but there's no observer left to catch it — theme sync from localStorage silently stops working for the rest of that session. Minor; drop once = TRUE if reconnect-resilience matters, since the if (input$saved_theme_js != active_theme())-style idempotency isn't even needed here (it's harmless to re-run).