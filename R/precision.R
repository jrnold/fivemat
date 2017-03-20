#' Suggested precision for fixed decimal formats
#'
#' Take either a numeric vector or the maximum absolute \code{step} size
#' between values and and return a suggested decimal precision for fixed
#' point formats such as \code{"f"}.
#'
#' \code{precision_round} Returns a suggested decimal precision for format types
#' that round to significant digits, such as \code{"g"} and \code{"r"}.
#'
#' \code{precision_fixed} Returns a suggested SI prefix.
#'
#' @param x A numeric vector to be formatted.
#' @param step Minimum absolute difference between values that will be formatted.
#' @return An integer vector of suggest precisions. For \code{precision_fixed},
#'    this is length one, for \code{precision_fixed_} it is the same length
#'    as \code{step}.
#' @seealso \code{\link{precision_round}} for the suggested precision to
#'   use with formats that round to significant digits,
#'   and \code{\link{precision_prefix}} for the suggested SI prefix to use with
#'   formatting.
#' @source The \code{d3-format} function \href{https://github.com/d3/d3-format/blob/master/src/precisionFixed.js}{precisionFixed}.
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

#' Suggested precision for rounded significant digit formats
#'
#' The function \code{precision_round_} takes the maximum absolute distance
#' between values, (\code{step}), and the maximum absolute value, (\code{value}),
#' of the values to be formatted, and returns the suggested precision for
#' formats that round to significant digits (e.g. \code{"g"}).
#' \code{precision_round} calculates this precision given a numeric vector of
#' values to format, \code{x}.
#'
#' @param x A numeric vector of values to be formatted.
#' @param step Numeric: Minimum absolute difference between values that will be formatted.
#' @param xmax Numeric: Maximum absolute value of the values to be formatted.
#' @return An integer vector of suggest precisions. For \code{precision_fixed},
#'    this is length one, for \code{precision_fixed_} it is the same length
#'    as \code{step}.
#' @seealso \code{\link{precision_fixed}} for the suggested precision to
#'   use with formats that use a fixed number of digits after the decimal point,
#'   and \code{\link{precision_prefix}} for a suggested SI prefix to use.
#' @source The \code{d3-format} function \href{https://github.com/d3/d3-format/blob/master/src/precisionRound.js}{precisionRound}
#' @importFrom assertthat assert_that
#' @export
precision_round <- function(x) {
  assert_that(is.numeric(x))
  precision_prefix_(diff(sort(x)), max(abs(x)))
}

#' @rdname precision_round
#' @export
precision_round_ <- function(step, xmax) {
  assert_that(is.numeric(step))
  assert_that(is.numeric(xmax))
  pmax(0, exponent(abs(xmax) - step) - exponent(step)) + 1
}

#' Suggested precision for SI Prefix formatting
#'
#' The function \code{precision_prefix_} provides a suggested precision for
#' use with SI prefix to use,
#' given a \code{value} to format, and the difference between the values,
#' \code{step}, that will be formatted.
#' \code{precision_prefix} calculates the suggested SI Prefix given a
#' numeric vector of values to format, \code{x}.
#'
#' @param x A numeric vector of values to be formatted.
#' @param step Numeric: Minimum absolute difference between values that will be formatted.
#' @param prefix The SI prefix to use. This is any valid argument for
#'    \code{\link{si_prefix}}. If \code{NULL} in \code{precision_prefix},
#'    then the prefix is determined from the median value in \code{x}.
#' @return Named list of two elements:
#' \describe{
#' \item{precision}{Integer vector: The suggested precision}
#' \item{si_prefix}{An named integer vector of SI prefixes. The names are the
#'   SI prefix names, and the values are the exponents, as in
#'   \code{\link{SI_PREFIXES}}.}
#' }
#' @seealso \code{\link{precision_fixed}} for the suggested precision to
#'   use with formats that use a fixed number of digits after the decimal point,
#'   and \code{\link{precision_round}} for the suggested precision to use with
#'   formats that round to significant digits.
#' @export
#' @source The \code{d3-format} function \href{https://github.com/d3/d3-format/blob/master/src/precisionPrefix.js}{precisionPrefix}.
#' @importFrom assertthat assert_that
#' @export
precision_prefix <- function(x, prefix = NULL) {
  assert_that(is.numeric(x))
  # use mean of log10 values since the relevant exponents are on log10 scale.
  prefix <- prefix %||% (10 ^ mean(log10(abs(x))))
  precision_prefix_(diff(sort(x)), prefix)
}

#' @rdname precision_prefix
#' @export
precision_prefix_ <- function(step, prefix) {
  assert_that(is.numeric(step))
  prefix <- si_prefix(prefix)
  p <- as.integer(pmax(0, prefix  - exponent(abs(step))))
  list(precision = p, si_prefix = prefix)
}
