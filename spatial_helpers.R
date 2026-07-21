# spatial_helpers.R - thin master file for the geostatistical core.
#
# WORKER CONTRACT (critical for parallelism - do not break):
# The future_promise pipeline in server_execution.R, every nested PSOCK worker
# (inside interp_run_item, which sources this file via an ABSOLUTE
# run_params$main_wd path because nested workers do not inherit the project
# working directory), and the classification worker all load the model code
# with a single `source("spatial_helpers.R", local = FALSE)`. Sourcing this
# file must therefore define the COMPLETE spatial helper set in the caller's
# global environment: the fragment source() calls below use the default
# local = FALSE so everything lands in globalenv and the worker entry points
# (interp_run_item & co.) stay globalenv-enclosed.
# If you add a fragment, register it HERE - a worker missing one fragment
# (e.g. calc_ccc) crashes the whole parallel run with "object not found".
#
# Fragments are resolved relative to THIS file's own location (taken from the
# innermost source() frame), so the master works no matter which working
# directory the sourcing worker happens to have.
local({
  src_dir <- tryCatch({
    f <- NULL
    for (i in rev(seq_len(sys.nframe()))) {
      e <- sys.frame(i)
      if (exists("ofile", envir = e, inherits = FALSE)) {
        cand <- get("ofile", envir = e)
        if (is.character(cand) && length(cand) == 1L) { f <- cand; break }
      }
    }
    if (is.null(f)) "." else dirname(normalizePath(f))
  }, error = function(e) ".")
  source(file.path(src_dir, "spatial_vgm.R"))
  source(file.path(src_dir, "spatial_metrics.R"))
  source(file.path(src_dir, "spatial_kriging.R"))
  source(file.path(src_dir, "spatial_pipeline.R"))
})
