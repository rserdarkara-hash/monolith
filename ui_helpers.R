# ui_helpers.R - master file for the four pure UI fragments (no reactive() or
# observe() inside them). Fragments resolve relative to this file, as in
# spatial_helpers.R.
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
