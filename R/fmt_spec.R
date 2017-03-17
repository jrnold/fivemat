# [[fill]align][sign][symbol][0][width][,][.precision][type]
RE = "^(?:(.)?([<>=^]))?([+\\-\\( ])?([$#])?(0)?(\\d+)?(,)?(\\.\\d+)?([a-z%])?$"

na_else <- function(x, default) {
  dplyr::if_else(!is.na(x), x, default)
}

fmt_types <- list(
  NULL = NULL, #formatDefault,
  "%" = function(x, p) sprintf(paste0("%f.", p), x * 100),
  "b" = function(x) as.character(R.utils::intToBin(round(x))),
  "c" = as.character,
  "d" = function(x) as.character(round(x)),
  "e" = function(x, p) sprintf(paste0("%e.", p), x),
  "f" = function(x, p) sprintf(paste0("%d.", p), x),
  "g" = function(x, p) sprintf(paste0("%g.", p), x),
  "o" = function(x) format(as.octmode(x)),
  "p" = NULL, # function(x, p) { return formatRounded(x * 100, p); },
  "r" = NULL, # formatRounded,
  "s" = NULL, # formatPrefixAuto,
  "X" = function(x) format(as.hexmode(round(x)), upper.case = TRUE),
  "x" = function(x) format(as.hexmode(round(x)), upper.case = FALSE)
)

#' @rdname fmt_spec
#' @importFrom assertthat is.string
as_fmt_spec <- function(x) {
  if (inherits("fmt_spec")) return(x)
  spec <- as.character(x)
  assert_that(is.string(spec))
  m <- stringr::str_match(x, RE)
  if (any(is.na(m))) {
    stop("\"", spec, "\" is an invalid format.", call. = FALSE)
  }

  res <- list()
  res$fill <- na_else(match[1], " ")
  res$align <- na_else(match[2], ">")
  res$sign <- na_else(match[3], "-")
  res$symbol <- na_else(match[4], "")
  res$zero <- !is.na(match[5])
  res$width <- if (is.na(match[6])) NA_integer_ else as.integer(match[6])
  res$comma <- !is.na(match[7])
  res$precision <- if (is.na(match[8])) {
    NA_integer_
  } else {
    as.integer(stringr::str_sub(match[8], 2))
  }
  res$type <- na_else(match[9], ".")
  purrr::invoke(fmt_spec, res)
}

#' Format Spec
#'
#' @param x An object to convert to a format spec: a string representation
#'   of the spec.
#' @param fill A string. See details.
#' @param align A string. See Details.
#' @param sign A string. See Details.
#' @param symbol A string. See Details.
#'
#' @export
#' @importFrom assertthat is.flag is.number
fmt_spec <- function(fill = NULL,
                     align = c(">", "<", "^", "="),
                     sign = c("-", "+", "(", " "),
                     symbol = NULL,
                     zero = FALSE,
                     width = NA_integer_,
                     comma = FALSE,
                     precision = NA_integer_,
                     type = NULL) {
  assert_that(is.string(fill))
  align <- match.arg(align)
  sign <- match.arg(sign)
  if (!is.null(symbol)) {
    assert_that(is.string(symbol) && (symbol %in% c("$", "#")))
  }
  assert_that(is.flag(zero))
  assert_that(is.flag(comma))
  assert_that(is.number(width))
  assert_that(is.number(precision))
  assert_that(is.null(type) || is.string(type))

  res <- list(fill = fill, align = align, sign = sign,
              symbol = symbol, width = width,
              comma = comma, precision = precision, type = type)
  # The "n" type is an alias for ",g".
  if (!is.null(type)) {
    if (res$type == "n") {
      res$comma <- TRUE
      res$type <- "g"
    } else if (!type %in% names(fmt_types)) {
      # Map invalid types to the default format.
      # Use something else for default?
      res[["type"]] <- NULL
    }
  }

  # If zero fill is specified, padding goes after sign and before digits.
  if (res$zero) {
    res$fill <- "0"
    res$align <- "="
  }
  structure(res, class = "fmt_spec")
}

#' @export
print.fmt_spec <- function(x, ...) {
  stringr::str_c(x$fill,
                  x$align,
                  x$sign,
                  x$symbol,
                  if (is.na(x$width)) "" else max(1, x$width),
                  if (x$comma) "," else "",
                  if (is.na(x$precision)) "" else "." + max(0, x$precision),
                  if (x$type == "NULL") "" else x$type)
}

#' Format locale
#'
#' Create a a format locale. This object provides information on the symbols for decimal points, group (thousands) seperator, group sizes, currency symbols, and numerals.
#'
#' @param decimal_mark string. the decimal point mark (e.g., `"."``).
#' @param grouping_mark string. the grouping mark (e.g., `","``).
#' @param grouping numeric vector group sizes (e.g., c(3L)). It is recycled as needed.
#' @param currency character vector of length two with the currency prefix and suffix (e.g., `c("$", "")`
#' @param numerals A character vector of length ten to replace the numerals 0-9.
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
