# split x into mantissa and exponent
fmt_decimal <- function(x, p) {
  if (is_empty(x)) return(character())
  # need to handle NA, NaN, Inf
  strx <- character(length(x))
  strx[is.finite(x)] <- sprintf(paste0("%.", p - 1L, "e"),
                                abs(x[is.finite(x)]), p - 1L)
  split <- stringr::str_split_fixed(strx, "e", 2)
  tibble::tibble(mantissa = str_replace(split[, 1], "[^0-9]", ""),
                 exponent = as.integer(split[, 2]))
}

#' @importFrom stringi stri_dup
#' @importFrom dplyr case_when
#' @noRd
fmt_rounded <- function(x, p) {
  if (is_empty(x)) return(character())
  x <- signif(x, p)
  k <- -exponent(x) + p - 1L
  f <- function(i, j) sprintf(str_c("%.", j, "f"), i)
  map2_chr(x, k, f)
}

# like precision, but drops insignificant trailing 0's
fmt_default <- function(x, ...) {
  UseMethod("fmt_default")
}

fmt_default.numeric <- function(x, p, ...) {
  sprintf_("g")(x, p)
}

fmt_default.integer <- function(x, ...) {
  sprintf_("d")(x)
}

#' @importFrom stringi stri_dup
#' @importFrom dplyr case_when
#' @noRd
fmt_prefix_auto <- function(x, p) {
  fin <- is.finite(x)
  out <- vector("character", length(x))
  out[!fin] <- format(x[!fin])
  d <- fmt_decimal(x[fin], p)
  prefix_exponent <- si_prefix(as.integer(d[["exponent"]]))
  i <- d[["exponent"]] - prefix_exponent + 1L # nolint
  n <- str_length(d$mantissa) # nolint
  out[fin] <-
    case_when(
      i == n ~ d$mantissa,
      i > n  ~ str_c(d$mantissa, stri_dup("0", i - n)),
      i > 0L ~ str_c(str_sub(d$mantissa, 1L, i), ".",
                     str_sub(d$mantissa, i + 1L)),
      TRUE   ~ str_c("0.", stri_dup("0", -i),
                     fmt_decimal(x[fin], pmax(0L, p + i - 1L))$mantissa)
    )
  attr(out, "si_prefix") <- names(prefix_exponent)
  out
}

sprintf_ <- function(format) {
  force(format)
  function(x, p = NULL) {
    if (is.null(p)) {
      sprintf(paste0("%", format), x)
    } else {
      sprintf(paste0("%.", p, format), x)
    }
  }
}

# integer to binary
# @param x single integer value
# @return binary representation as character string of abs(x)
#' @importFrom purrr map2
#' @importFrom stringi stri_flatten
int2bin <- function(x) {
  x <- abs(as.integer(x))
  out <- vector("list", length(x))
  is_zero <- x == 0L
  out[is_zero] <- list("0")
  k <- floor(log2(x[!is_zero])) + 1L
  f <- function(i, j) {
    idx <- seq(from = j, to = 1L, by = -1L)
    stri_flatten(as.integer(intToBits(i))[idx])
  }
  out[!is_zero] <- map2(x[!is_zero], k, f)
  out
}

fmt_g <- function(x, p, upper = FALSE) {
  x <- signif(x, p)
  k <- exponent(x)
  if_else(k < -4 | k >= p,
          sprintf_(if (upper) "E" else "e")(x, p - 1L),
          sprintf_("f")(x, p - k - 1L))
}

# integer types
fmt_types_int <- c("b", "d", "o", "x", "X")
fmt_types_dblf <- c("%", "f", "F", "s")
fmt_types_dblp <- c("a", "A", "e", "E", "g", "G", "r")
fmt_types_chr <- c("c")
# Derived types
# s -> transform by SI, f
# p,% -> transform by % then f or e
# uses both significant digits and decimal
# g not derived since formatting is conditional

fmt_types <- list(
  "%" = function(x, p) sprintf_("f")(x * 100, p),
  # a and A are from R sprintf
  "a" = function(x, p) str_sub(sprintf_("a")(x, p), 3),
  "A" = function(x, p) str_sub(sprintf_("A")(x, p), 3),
  "b" = function(x, p) map_chr(round(x), int2bin),
  "c" = function(x, p) as.character(x),
  "d" = function(x, p) sprintf("%d", as.integer(round(x))),
  "e" = sprintf_("e"),
  "E" = sprintf_("E"),
  "f" = sprintf_("f"),
  "g" = function(x, p) fmt_g(x, p, FALSE),
  "G" = function(x, p) fmt_g(x, p, TRUE),
  "o" = function(x, p) sprintf_("o")(as.integer(round(x))),
  "p" = function(x, p) fmt_rounded(x * 100, p),
  "r" = function(x, p) fmt_rounded(x, p),
  "s" = function(x, p) fmt_prefix_auto(x, p),
  "u" = function(x, p) intToUtf8(round(x), multiple = TRUE),
  "X" = function(x, p) sprintf_("X")(as.integer(round(x))),
  "x" = function(x, p) sprintf_("x")(as.integer(round(x)))
)

# [[fill]align][sign][symbol][0][width][,][.precision][type]
#' @importFrom stringr regex
RE <- regex(str_c("^",
                  "(?:(.)?([<>=^]))?", # [fill]align
                  "([+\\-\\( ])?",     # sign
                  "([$#])?",           # symbol
                  "(0)?",              # zero
                  "(\\d+)?",           # width
                  "(,)?",              # comma
                  "(\\.\\d+)?",        # precision
                  "([a-z%])?",         # type
                  "$"), ignore_case = TRUE)

#' @rdname fmt_spec
#' @importFrom assertthat is.string
#' @importFrom stringr str_length str_detect regex str_match str_sub
#' @export
as_fmt_spec <- function(x = character()) {
  if (inherits(x, "fmt_spec")) return(x)
  if (purrr::is_empty(x) || x == "") return(fmt_spec())
  spec <- as.character(x)
  assert_that(is.string(spec))
  # drop 1st element because that is the full pattern
  m <- str_match(spec, RE)[1, -1]
  if (all(is.na(m))) {
    stop("\"", spec, "\" is an invalid format.", call. = FALSE)
  }
  res <- list()
  res$fill <- na_else(m[1], " ")
  res$align <- na_else(m[2], ">")
  res$sign <- na_else(m[3], "-")
  res$symbol <- if (is.na(m[4])) NULL else m[4]
  res$zero <- !is.na(m[5])
  res$width <- if (is.na(m[6])) NULL else as.integer(m[6])
  res$comma <- !is.na(m[7])
  res$precision <- if (is.na(m[8])) {
    NULL
  } else {
    as.integer(str_sub(m[8], 2))
  }
  res$type <- if (is.na(m[9])) NULL else m[9]
  purrr::invoke(fmt_spec, res)
}

Format <- R6Class("Format", {
  public = list(
    format = function(x, spec) {
      base::format(x)
    }
  )
})

#' Format Specification
#'
#' The function \code{fmt_spec} is used to create a format specification object
#' which describes how a number is to be formatted.
#' The function \code{as_fmt_spec} parses a string with a concise representation of
#' the specification into a \code{fmt_spec} object.
#'
#' @param x A string representation of the spec. See Details.
#' @param fill A string. See Details.
#' @param align A string. See Details.
#' @param sign A string. See Details.
#' @param symbol A string. See Details.
#' @param zero A logical vector of length one. See Details.
#' @param width An integer vector of length one. See Details.
#' @param comma An logical vector of length one. See Details.
#' @param precision An integer vector of length one. See Details.
#' @param type A character vector of length one. See Details.
#' @return An object of class \code{"fmt_spec"}. This is a list with elements:
#' \describe{
#' \item{fill}{Character vector of length one. See Details.}
#' \item{align}{Character vector of length one. See Details.}
#' \item{sign}{Character vector of length one. See Details.}
#' \item{symbol}{Character vector of length one. See Details.}
#' \item{width}{Integer vector of length one. See Details.}
#' \item{comma}{Logical vector of length one. See Details.}
#' \item{precision}{Integer vector of length one.}
#' \item{type}{Character vector of length one, or \code{NULL} if the
#'             default type}
#' }
#'
#' @details
#' The returned function takes a number as the only argument, and returns a string representing the formatted number.
#' The general form of a specifier is:
#' \verb{
#'  [[fill]align][sign][symbol][0][width][,][.precision][type]
#' }
#'
#' The \code{fill} can be any character. The presence of a fill character is signaled by the *align* character following it, which must be one of the following:
#' \itemize{
#' \item{\code{>}: Forces the field to be right-aligned within the available space.}
#' \item{\code{<}: Forces the field to be left-aligned within the available space.}
#' \item{\code{^}: Forces the field to be centered within the available space.}
#' \item{\code{=}: like \code{>}, but with any sign and symbol to the left of any padding.}
#' }
#'
#' The \code{sign} can be:
#' \itemize{
#' \item{\code{"-"} : nothing for positive and a minus sign for negative. (Default behavior.)}
#' \item{\code{"+"} : a plus sign for positive and a minus sign for negative.}
#' \code{{"("} : nothing for positive and parentheses for negative.}
#' \code{{" "} (space) : a space for positive and a minus sign for negative.}
#' }
#'
#' The \code{symbol} can be:
#' \itemize{
#' \item{\code{"$"}: Apply currency symbols as per the \code{locale}.}
#' \item{\code{"#"}: The behavior epends on the \code{type}:
#'   \itemize{
#'   \item{\code{"b"}: Prefix with \code{"0b"}.}
#'   \item{\code{"o"}: Prefix with \code{"0o"}.}
#'   \item{\code{"x"}, \code{"X"}, code{"a"}, \code{"A"}: Prefix with \code{"0x"}.}
#'   }}
#' }
#'
#' The \code{zero} (\code{0}) option enables zero-padding; this implicitly sets \code{fill} to \code{"0"} and \code{align} to \code{"="}.
#'
#' The \code{width} defines the minimum field width; if not specified, then the width will be determined by the content.
#'
#' The \code{comma} (\code{","}) option enables the use of a group separator,
#' such as a comma for thousands. The grouping mark and intervals are specified
#' by the \code{locale}.
#'
#' Depending on the \code{type}, the \code{precision} either indicates the number of digits that follow the decimal point (\code{"f"}, \code{"\%"}), or the number of significant digits (\code{NULL}, \code{"a"}, \code{"e"}, \code{"g"}, \code{"r"}, \code{"s"}, \code{"p"}).
#' If the precision is not specified, it defaults to 6 for all types except \code{NULL}, which defaults to 12.
#' The recision is ignored for integer formats (types \code{"b"}, \code{"o"}, \code{"d"}, \code{"x"}, \code{"X"} and \code{"c"}).
#' See \code{\link{precision_fixed}} and \code{\link{precision_round}} for help picking an appropriate precision.
#'
#' The available \code{type} values are:
#' \itemize{
#' \item{\code{NULL}: The default format. It is similar to \code{"g"}, but insignificant trailing zeros are trimmed.}
#' \item{\code{"a"},\code{"A"}: Double precision values in binary notation of the form \code{h.hhhp[+-]d}. This is a binary fraction expressed in hex and multiplied by a decimal power of 2. The number of hex digits after the  decimal point is specified by the precision.}
#' \item{\code{"b"}: Integer values in binary notation.}
#' \item{\code{"c"}: Convert to character using \code{as.character}.}
#' \item{\code{"d"}: Integer values in decimal notation.}
#' \item{\code{"e"},\code{"E"}: Double precision value in exponential notation of the form \code{m.dde[+-]xx} or \code{m.ddE[+-]xx}.}
#' \item{\code{"f"}: Double precision value in fixed point decimal notation in the form \code{mmm.ddd}. The number of decimal palcaes is specified by the precision.}
#' \item{\code{"g"},\code{"G"}: Double precision value in the either fixed point (\code{"f"}) or exponential (\code{"e"}) notation, rounded to significant digits, specified by the precision. Fixed point is used if precision is less than 4, or the exopnent is greater than or equal to the precision. Trailing zeros are not dropped.}
#' \item{\code{"n"}: Shorthand for \code{",g"}.}
#' \item{\code{"o"}: Integer values in octal notation.}
#' \item{\code{"p"}: Multiplied by 100, rounded to significant digits, and then formatted with \code{"f"} and a percent sign (\code{"\%"}) suffix.}
#' \item{\code{"r"}:  Decimal notation, but rounded to significant digits.}
#' \item{\code{"s"}:  Decimal notation (\code{"f"}) with an [SI prefix](#locale_formatPrefix), rounded to significant digits.}
#' \item{\code{"u"}: Integer values converted to its unicode character.}
#' \item{\code{"\%"}: Multiplied by 100, and then formatted with decimal notation (\code{"f"}) and a percent sign (\code{"\%"}) suffix.}
#' \item{\code{"x"}, \code{"X"}: Hexadecimal notation, rounded to integer. \code{"x"} uses lower-case letters, and \code{"X"}, upper-case letters.}
#' }
#'
#' Note that these formats are not the same as those in \code{\link[base]{sprintf}}.
#'
#' @export
#' @importFrom stringr str_to_lower
#' @importFrom assertthat is.flag is.number
fmt_spec <- function(type = "*",
                     fill = " ",
                     align = c(">", "<", "^", "="),
                     sign = c("-", "+", "(", " "),
                     symbol = NULL,
                     zero = FALSE,
                     width = NULL,
                     comma = FALSE,
                     precision = NULL) {
  assert_that(is.string(fill))
  align <- match.arg(align)
  sign <- match.arg(sign)
  if (!is.null(symbol)) {
    assert_that(is.string(symbol) && (symbol %in% c("$", "#")))
  }
  assert_that(is.flag(zero))
  assert_that(is.flag(comma))
  assert_that(is.null(width) || is.number(width))
  assert_that(is.null(precision) || is.number(precision))
  assert_that(is.null(type) || is.string(type))

  res <- list(fill = fill, align = align, sign = sign,
              symbol = symbol, width = width,
              comma = comma, precision = precision, type = type)
  # The "n" type is an alias for ",g".
  if (res$type == "n") {
    res$comma <- TRUE
    res$type <- "g"
  } else if (type == "N") {
    res$comma <- TRUE
    res$type <- "G"
  } else if (type == "i") {
    res$type <- "d"
  } else if (!type %in% names(fmt_types)) {
    stop("Type `\"", type, "\" is not recognized.", call. = FALSE)
  }
  precision <- if (is.null(precision)) {
    # defaults from d3-format
    # perhaps use 4 instead as in R
    if (type == "*") 12L else 6L
  } else {
    # clamp precision to: [0, 20] for fixed, [1, 21] for precision
    max(0L, min(20L, spec$precision)) + (out$type %in% fmt_types_dblp)
  }
  # If zero fill is specified, padding goes after sign and before digits.
  if (zero) {
    res$fill <- "0"
    res$align <- "="
  }
  structure(res, class = "fmt_spec")
}

#' @export
format.fmt_spec <- function(x, ...) {
  stringr::str_c(x$fill,
                  x$align,
                  x$sign,
                  x$symbol,
                  if (is.null(x$width)) "" else max(1, x$width),
                  if (x$comma) "," else "",
                  if (is.null(x$precision)) ""
                  else str_c(".", max(0, x$precision)),
                  if (is.null(x$type)) "" else x$type)
}

#' @export
as.character.fmt_spec <- format.fmt_spec

# nocov start
#' @export
print.fmt_spec <- function(x, ...) {
  cat("<fmt_spec>: ", format(x, ...))
  invisible(x)
}
# nocov end
