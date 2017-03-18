#' Create a format locale
#'
#' Create a format locale. This object provides information on the
#' symbols for decimal points, group (thousands) seperator, group sizes, currency symbols,
#' and numerals to be used in formatting numbers.
#'
#' The function `fmt_default_locale` gets the default locale.
#' The defaults are chosen to match R (i.e. US English), but without currency symbols.
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
#' @return  An object of class `"fmt_locale"`, which is a named list with elements: `decimal`, `thousands`, `grouping`, `currency`, and `numerals` (optional).
#' @export
fmt_locale <- function(decimal_mark = ".",
                       grouping_mark = ",",
                       grouping = 3,
                       currency = c("", ""),
                       numerals = NULL) {
  assert_that(is.string(decimal_mark))
  assert_that(is.string(grouping_mark))
  assert_that(is.numeric(grouping))
  grouping <- as.integer(grouping)
  assert_that(is.character(currency) && length(currency) == 2)
  if (!is.null(numerals)) {
    assert_that(is.character(numerals) && length(numerals) == 10)
    if (!is.null(names(numerals))) {
      assert_that(sort(names(numerals)) == as.character(0:9))
    } else {
      names(numerals) <- as.character(0:9)
    }
  }

  structure(
    list(
      decimal_mark = decimal_mark,
      grouping_mark = grouping_mark,
      grouping = grouping,
      currency = currency,
      numerals = numerals
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
print.fmt_locale <- function(x, ...) {
  cat("<fmt_locale>\n")
  cat("Grouping:  ", x$grouping, "\n")
  cat("Grouping Mark:  ", x$grouping_mark, "\n")
  cat("Decimal Mark:  ", x$decimal_mark, "\n")
  cat("Currency: ", str_c(x$currency[1], 1, x$currency[2]), "\n")
  if (!is.null(x$numerals)) {
    print("Numerals: ", x$numerals, "\n")
  }
}
