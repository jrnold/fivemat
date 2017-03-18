globalVariables("fmt_locales")

#' @importFrom stringr str_sub str_length str_c
#' @importFrom stringi stri_reverse
#' @importFrom purrr keep map_chr is_empty
group <- function(x, grouping = NULL, comma = ",") {
  if (is_empty(grouping)) return(x)
  intvls <- rep_len(grouping, max(str_length(x)))
  start <- cumsum(c(1L, intvls[-length(intvls)]))
  end <- start + intvls - 1L
  f <- function(x, start, end) {
    res <- keep(str_sub(stri_reverse(x), start, end), function(s) s != "")
    res <- stri_reverse(str_c(res, collapse = comma))
  }
  map_chr(x, f, start = start, end = end)
}

# Return base 10 exponent where mantissa * 10 ^ exponent
exponent <- function(x) {
  dplyr::if_else(is.finite(x), floor(log10(abs(x))), NA_real_)
}

#' Suggested decimal precisions
#'
#' \code{precision_prefix} returns a suggested decimal precision for fixed point
#' notation.
#'
#' \code{precision_round} Returns a suggested decimal precision for format types
#' that round to significant digits given the specified numeric \code{step} and
#' \code{max} values.
#'
#' \code{precision_fixed} returns a suggested decimal precision for a
#' format precision.
#'
#' @param x A numeric vector
#' @param step Minimum absolute difference between values that will be formatted
#' @param .max Largest absolute value to be formatted
#' @param value The SI prefix to be used
#' @return An integer vector of decimal precisions
#' @export
#' @importFrom assertthat assert_that
precision_fixed <- function(x) {
  assert_that(is.numeric(x))
  precision_fixed_(diff(sort(x)))
}

#' @rdname precision_fixed
#' @export
precision_fixed_ <- function(step) {
  assert_that(is.numeric(step))
  pmax(0, -exponent(abs(step)))
}

#' @rdname precision_fixed
#' @export
precision_round <- function(x) {
  assert_that(is.numeric(x))
  precision_prefix_(diff(sort(x)), max(abs(x)))
}

#' @rdname precision_fixed
#' @export
precision_round_ <- function(step, .max) {
  assert_that(is.numeric(step))
  assert_that(is.numeric(.max))
  pmax(0, exponent(abs(.max) - step) - exponent(step)) + 1
}

#' @rdname precision_fixed
#' @export
precision_prefix <- function(x, value) {
  assert_that(is.numeric(x))
  assert_that(is.numeric(value))
  precision_prefix_(diff(sort(x)), value)
}

#' @rdname precision_fixed
#' @export
precision_prefix_ <- function(step, value) {
  assert_that(is.numeric(step))
  assert_that(is.numeric(value))
  pmax(0, pmax(-8, pmin(8, floor(exponent(value) / 3))) * 3 -
         exponent(abs(step)))
}



replace_numerals <- function(x, numerals = NULL) {
  if (is.null(numerals)) return(x)
  names(numerals) <- as.character(0:9)
  stringr::str_replace_all(x, numerals)
}


na_else <- function(x, default) {
  dplyr::if_else(!is.na(x), x, default)
}
