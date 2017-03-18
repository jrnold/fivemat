#' @importFrom stringr str_sub str_length str_c
#' @importFrom stringi stri_reverse
#' @noRd
group <- function(x, grouping, comma) {
  intvls <- rep_len(grouping, max(str_length(x)))
  start <- c(1L, intvls[-length(intvls)])
  end <- start + intvls - 1L
  f <- function(x, start, end) {
    res <- purrr::keep(str_sub(stri_reverse(x), start, end), function(s) s != "")
    res <- str_c(rev(res), collapse = comma)
  }
  purrr::map_chr(x, f, start = start, end = end)
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

# split x into mantissa and exponent
fmt_decimal <- function(x, p) {
  # need to handle NA, NaN, Inf
  strx <- character(length(x))
  strx[is.finite(x)] <- formatC(abs(is.finite(x)), format = "e", digits = p)
  split <- str::str_split_fixed(strx, "e", 2)
  tibble::tibble(mantissa = str_replace(split[ , 1], "[^0-9]", ""),
                 exponent = as.integer(split[ , 2]))
}

fmt_rounded <- function(x, p) {
  d <- fmt_decimal(x, p)
  if_else(
    is.finite(x),
    if_else(
      d$mantissa < 0,
      str_c("0.", strrep("0", d$exponent), d$mantissa),
      if_else(
        str_length(d$mantissa) > d$exponent,
        str_c(str_sub(d$mantissa, 1L, d$exponent - 1L), ".",
              str_sub(d$mantissa, d$exponent)),
        str_c(d$mantissa, strrep("0", d$exponent))
      )
    ),
    base::format(x)
  )
}

replace_numerals <- function(x, numerals = NULL) {
  if (is.null(numerals)) return(x)
  names(numerals) <- as.character(0:9)
  stingr::str_replace_all(x, numerals)
}
