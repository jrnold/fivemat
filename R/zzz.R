.onLoad <- function(libname, pkgname) {
  opt <- options()
  opt_fivemat <- list(
    fivemat.fmt_default_locale = fmt_default_locale()
  )
  to_set <- !(names(opt_fivemat) %in% names(opt))
  if (any(to_set)) options(opt_fivemat[to_set])
  invisible()
}
