#' Common Format Locales
#'
#' Predefined \code{\link{fmt_locale}} objects for
#' common locales.
#'
#' @details These locales are derived from the JSON
#' files provided by the \href{d3-format}{https://github.com/d3/d3-format/tree/master/locale}
#' Javascript library.
#'
#' @format A \code{list} of \code{\link{fmt_locale}} objects.
#'
#' @source \url{https://github.com/d3/d3-format}
#'
#' @examples
#' # Available locales
#' names(fmt_locales)
#' # English-US locale
#' fmt_locales[["en-US"]]
#' # French locale
#' fmt_locales[["fr-FR"]]
"fmt_locales"
