#' Suggested precisions
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
