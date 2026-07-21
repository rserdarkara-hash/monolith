# ui_helpers.R - thin master file. The former 2,200-line helper monolith is
# split into four cohesive fragments; sourcing THIS file loads all of them, so
# every existing call site (global.R, tests) keeps working unchanged.
# All fragments are pure R functions/constants - never introduce reactive()
# or observe() hooks inside them. Fragments are resolved relative to this
# file's own location (same self-locating pattern as spatial_helpers.R).
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
  source(file.path(src_dir, "ui_colors.R"))
  source(file.path(src_dir, "ui_formatting.R"))
  source(file.path(src_dir, "ui_components.R"))
  source(file.path(src_dir, "ui_plotting.R"))
})
