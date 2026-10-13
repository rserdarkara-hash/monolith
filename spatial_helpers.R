# spatial_helpers.R - master file for the geostatistical core.
#
# WORKER CONTRACT: every worker (interpolation, nested PSOCK, variogram
# optimizer, Governing Factors, classification) loads the model code with
# source("spatial_helpers.R"), so this file must define every fragment's
# functions in the global environment. A fragment not registered here crashes
# parallel runs with "object not found". Fragments resolve relative to this
# file, whatever the worker's working directory.
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
