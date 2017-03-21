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
#' @details
#' The suggested precision is
#' \deqn{
#' p = \max \left(0, \lfloor \log_{10}|d| \rfloor \right)
#' }{max(0, floor(log10(d)))}
#' where \eqn{d} is the maximum absolute distance between values.
#' This assumes that the values to be formatted are multiples of the step
#' size \code{d}.
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
#' @examples
#' # step size is 1 and the suggested precision is 0
#' x <- c(1, 1.5, 2)
#' p <- precision_fixed(x)
#' p
#' fmt(x, paste0(".", p, "f"))
#' # alternatively calculate the step size
#' step <- max(diff(sort(x)))
#' precision_fixed_(step)
#' # For 1, 2, 3, the step size is 1 suggested precision is 0
#' x2 <- 1:3
#' p2 <- precision_fixed(x2)
#' p2
#' fmt(x2, paste0(".", p, "f"))
precision_fixed <- function(x) {
  assert_that(is.numeric(x))
  precision_fixed_(max(diff(sort(x))))
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
#' @details
#' The suggested precision, \eqn{p}, for values in a vector, \eqn{x}, is
#' \eqn{
#'   p = \max \left(0,\lfloor\log_{10}(|\max_{i} x|) \rfloor - \lfloor \log_{10} d \rfloor \right)
#' }{max(0, floor(log10(max(abs(x)) - d)) - floor(log10(d)))},
#' where \eqn{d} the maximum absolute distance between values of
#' \eqn{x}.
#'
#' For the exponential format, \code{"e"}, substract use \code{p - 1} for the
#' precision.
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
#' @examples
#' # For these, the step size is 0.01 and suggested precision is 3
#' x <- c(0.99, 1, 1.01)
#' p <- precision_round(x)
#' fmt(x, paste0(".", p, "r"))
#' # For these, the step size is 0.1 and suggested precision is 2
#' x2 <- c(0.9, 1.0, 1.1)
#' p2 <- precision_round(x2)
#' fmt(x2, paste0(".", p2, "r"))
#' # For the e format type subtract one
#' fmt(x, paste0(".", p - 1, "e"))
precision_round <- function(x) {
  assert_that(is.numeric(x))
  precision_round_(max(diff(sort(x))), max(abs(x)))
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
#' @details The suggested precision, \eqn{p}, when formatting a vector with
#' an SI Prefix of \eqn{10^k} is,
#' \deqn{
#' p = \max \left( 0, k - \lfloor \log_{10}d \rfloor \right)
#' ,}{p = max(0, floor(log10(d))),}
#' where \eqn{d} is the maximum absolute value between elements in the vector.
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
#' @examples
#' x <- c(1.1e6, 1.2e6, 1.3e6)
#' p <- precision_prefix(x, 6L)
#' p$precision
#' fmt(x, paste0(".", p$precision), si_prefix = 6L)
precision_prefix <- function(x, prefix = NULL) {
  assert_that(is.numeric(x))
  # use mean of log10 values since the relevant exponents are on log10 scale.
  prefix <- prefix %||% (10 ^ mean(log10(abs(x))))
  precision_prefix_(max(diff(sort(x))), prefix)
}

#' @rdname precision_prefix
#' @export
precision_prefix_ <- function(step, prefix) {
  assert_that(is.numeric(step))
  prefix <- si_prefix(prefix)
  p <- as.integer(pmax(0, prefix  - exponent(abs(step))))
  list(precision = p, si_prefix = prefix)
}
