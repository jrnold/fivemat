#' Create a format locale
#'
#' Create a format locale. This object provides information on the
#' symbols for decimal points, group (thousands) seperator, group sizes, currency symbols,
#' and numerals to be used in formatting numbers.
#'
#' The function `fmt_default_locale` returns the default locale.
#' The defaults are chosen to match R (i.e. US English).
#'
#' The default locale is set with the option, `fivemat.fmt_default_locale`.
#'
#' @param decimal_mark string. the decimal point mark (e.g., `"."``).
#' @param grouping_mark string. the grouping mark (e.g., `","``).
#' @param grouping numeric vector group sizes (e.g., c(3L)). It is recycled as needed.
#' @param currency character vector of length two with the currency prefix and suffix (e.g., `c("$", "")`
#' @param numerals A character vector of length ten to replace the numerals 0-9.
#'    If named, the names must be `"0"`, ..., `"9"`. If not named, then the
#'    vector is assumed to be ordered 0 to 9; e.g. `numerals[1]` is the numeral
#'    for "0", `numerals[2]` is the numeral for "1" and so on.
#' @param inf_mark Character: Mark to use for infinity (\code{Inf}).
#' @param na_mark Character: Mark to use for missing values, (\code{NA}).
#' @param nan_mark Character: Mark to use for not-a-number values, (\code{NaN}).
#' @return  An object of class `"fmt_locale"`, which is a named list with elements: `decimal`, `thousands`, `grouping`, `currency`, and `numerals` (optional).
#' @export
fmt_locale <- function(decimal_mark = ".",
                       grouping_mark = ",",
                       grouping = 3,
                       currency = c("$", ""),
                       numerals = NULL,
                       inf_mark = "Inf",
                       na_mark = "NA",
                       nan_mark = "NaN") {
  assert_that(is.string(decimal_mark))
  assert_that(is.string(grouping_mark))
  assert_that(is.numeric(grouping))
  grouping <- as.integer(grouping)
  assert_that(is.character(currency) && length(currency) == 2)
  if (!is.null(numerals)) {
    assert_that(is.character(numerals) && length(numerals) == 10)
    if (!is.null(names(numerals))) {
      numerals <- numerals[order(names(numerals))]
      assert_that(all(names(numerals) == as.character(0:9)))
    } else {
      names(numerals) <- as.character(0:9)
    }
  }
  assert_that(is.string(inf_mark))
  assert_that(is.string(na_mark))
  assert_that(is.string(nan_mark))

  structure(
    list(
      decimal_mark = decimal_mark,
      grouping_mark = grouping_mark,
      grouping = grouping,
      currency = currency,
      numerals = numerals,
      inf_mark = inf_mark,
      na_mark = na_mark,
      nan_mark = nan_mark
    ),
    class = "fmt_locale"
  )
}

#' @export
#' @rdname fmt_locale
fmt_default_locale <- function() {
  loc <- getOption("fivemat.fmt_default_locale")
  if (is.null(loc)) {
    loc <- fmt_locale()
    options("fivemat.fmt_default_locale" = loc)
  }
  loc
}

#' @export
print.fmt_locale <- function(x, ...) { # nocov start
  cat("<fmt_locale>\n")
  cat("Grouping:       ", str_c(x$grouping, collapse = ","), "\n")
  cat("Grouping Mark:  ", x$grouping_mark, "\n")
  cat("Decimal Mark:   ", x$decimal_mark, "\n")
  cat("Currency:       ", str_c(x$currency[1], 1, x$currency[2]), "\n")
  if (!is.null(x$numerals)) {
    cat("Numerals:      ", str_c(x$numerals, collapse = ", "), "\n")
  } else {
    cat("Numerals:      ", str_c(0:9, collapse = ", "), "\n")
  }
  cat("Inf: ", x$inf_mark, ", NA: ", x$na_mark, ", NaN", x$nan_mark, "\n")
} # nocov end
