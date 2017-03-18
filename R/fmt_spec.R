# split x into mantissa and exponent
fmt_decimal <- function(x, p) {
  # need to handle NA, NaN, Inf
  strx <- character(length(x))
  strx[is.finite(x)] <- formatC(abs(x[is.finite(x)]), format = "e",
                                # since n.nn, needs precision - 1L
                                digits = p - 1L)
  split <- stringr::str_split_fixed(strx, "e", 2)
  tibble::tibble(mantissa = str_replace(split[, 1], "[^0-9]", ""),
                 exponent = as.integer(split[, 2]))
}

#' @importFrom dplyr case_when
#' @noRd
fmt_rounded <- function(x, p) {
  d <- fmt_decimal(x, p)
  case_when(
    !is.finite(x) ~ base::format(x),
    d$exponent < 0 ~ str_c("0.", strrep("0", d$exponent), d$mantissa),
    str_length(d$mantissa) > (d$exponent + 1L) ~
      str_c(str_sub(d$mantissa, 1L, d$exponent), ".",
            str_sub(d$mantissa, d$exponent + 1L)),
    TRUE ~ str_c(d$mantissa,
                 strrep("0", pmax(0L,
                                  d$exponent - str_length(d$mantissa) + 1L)))
  )
}

# like precision, but drops insignificant trailing 0's
fmt_default <- function(x, p) {
  formatC(x, format = "f", digits = p, drop0trailing = TRUE)
}

#' @importFrom dplyr case_when
#' @noRd
fmt_prefix_auto <- function(x, p) {
  fin <- is.finite(x)
  out <- vector("character", length(x))
  out[!fin] <- format(x[!fin])
  d <- fmt_decimal(x[fin], p)
  i <- d$exponent - prefix_exponent(d$exponent) + 1L
  n <- str_length(d$mantissa)
  out[fin] <-
    case_when(
      i == n ~ d$mantissa,
      i > n ~ str_c(d$mantissa, strrep("0", pmax(0L, i - n + 1L))),
      i > 0L ~  str_c(str_sub(d$mantissa, 1L, i), ".",
                      str_sub(d$mantissa, i + 1L)),
      TRUE ~ str_c("0.", strrep("0", pmax(0L, 1L - i)),
                   fmt_decimal(x[fin],
                               pmax(0L, p + i - 1L))$mantissa)
    )
  out
}

fmt_types <- list(
  "%" = function(x, p) formatC(x * 100, format = "f", digits = p),
  # a and A are from R sprintf
  "a" = function(x, p) str_sub(sprintf(paste0("%.", p, "a"), x), 3),
  "A" = function(x, p) str_sub(sprintf(paste0("%.", p, "A"), x), 3),
  "b" = function(x, p) as.character(R.utils::intToBin(round(x))),
  "c" = function(x, p) base::as.character(x),
  "d" = function(x) as.character(round(x)),
  "e" = function(x, p) formatC(x, format = "e", digits = p),
  "f" = function(x, p) formatC(x, format = "f", digits = p),
  "g" = function(x, p) formatC(x, format = "g", digits = p),
  "o" = function(x, p) format(as.octmode(x)),
  "p" = function(x, p) fmt_rounded(x * 100, p),
  "r" = fmt_rounded,
  "s" = fmt_prefix_auto,
  "X" = function(x, p) format(as.hexmode(round(x)), upper.case = TRUE),
  "x" = function(x, p) format(as.hexmode(round(x)), upper.case = FALSE)
)

# [[fill]align][sign][symbol][0][width][,][.precision][type]
RE <- stringr::regex(str_c("^",
                          "(?:(.)?([<>=^]))?",
                          "([+\\-\\( ])?",
                          "([$#])?",
                          "(0)?",
                          "(\\d+)?",
                          "(,)?",
                          "(\\.\\d+)?",
                          "([a-z%])?",
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

#' Format Specification
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
#'
#' @details
#'
#' \code{as_fmt_spec()} returns a \code{fmt_spec} object for the given string \code{specifier}.
#' The returned function takes a number as the only argument, and returns a string representing the formatted number.
#' The general form of a specifier is:
#' \verb{[[fill]align][sign][symbol][0][width][,][.precision][type]}
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
#' \item{\code{"-} : nothing for positive and a minus sign for negative. (Default behavior.)}
#' \item{\code{"+"} : a plus sign for positive and a minus sign for negative.}
#' \code{{"("} : nothing for positive and parentheses for negative.}
#' \code{{" "} (space) : a space for positive and a minus sign for negative.}
#' }
#'
#' The \code{symbol} can be:
#' \itemize{
#' \item{\code{"$"}: apply currency symbols per the locale definition.}
#' \item{\code{"#"}: for binary, octal, or hexadecimal notation, prefix by \code{"0b"}, \code{"0o"}, or \code{"0x"}, respectively.}
#' }
#'
#' The \code{zero} (\code{0}) option enables zero-padding; this implicitly sets \code{fill} to \code{"0"} and \code{align} to \code{"="}.
#' The \code{width} defines the minimum field width; if not specified, then the width will be determined by the content.
#' The \code{comma} (\code{,}) option enables the use of a group separator, such as a comma for thousands.
#'
#' Depending on the \code{type}, the \code{precision} either indicates the number of digits that follow the decimal point (\code{"f"}, \code{"\%"}), or the number of significant digits (\code{NULL}, "e"`, `"g"`, `"r"`, `"s"`, `"p"`).
#' If the precision is not specified, it defaults to 6 for all types except \code{NULL}, which defaults to 12.
#' Precision is ignored for integer formats (types \code{"b"}, \code{"o"}, \code{"d"}, \code{"x"}, \code{"X"} and \code{"c"}).
#' See \code{\link{precision_fixed}} and \code{\link{precision_round}} for help picking an appropriate precision.
#'
#' The available \code{type} values are:
#' \itemize{
#' \item{ \code{"e"} - exponent notation.}
#' \item{ \code{"f"} - fixed point notation.}
#' \item{ \code{"g"} - either decimal or exponent notation, rounded to significant digits.}
#' \item{ \code{"r"} - decimal notation, rounded to significant digits.}
#' \item{ \code{"s"} - decimal notation with an [SI prefix](#locale_formatPrefix), rounded to significant digits.}
#' \item{ \code{"\%"} - multiply by 100, and then decimal notation with a percent sign.}
#' \item{ \code{"p"} - multiply by 100, round to significant digits, and then decimal notation with a percent sign.}
#' \item{ \code{"b"} - binary notation, rounded to integer.}
#' \item{ \code{"o"} - octal notation, rounded to integer.}
#' \item{ \code{"d"} - decimal notation, rounded to integer.}
#' \item{ \code{"x"} - hexadecimal notation, using lower-case letters, rounded to integer.}
#' \item{ \code{"X"} - hexadecimal notation, using upper-case letters, rounded to integer.}
#' \item{ \code{"c"} - converts the integer to the corresponding unicode character before printing.}
#' \item{ \code{NULL} - like \code{"g"}, but trim insignificant trailing zeros.}
#' }
#' The type \code{n} is also supported as shorthand for \code{,g}.
#'
#' For the \code{"g"}, \code{"n"} and \code{NULL} (none) types, decimal notation is used if the resulting string would
#' have \code{precision} or fewer digits; otherwise,
#' an exponent notation is used.
#'
#' @export
#' @importFrom stringr str_to_lower
#' @importFrom assertthat is.flag is.number
fmt_spec <- function(fill = " ",
                     align = c(">", "<", "^", "="),
                     sign = c("-", "+", "(", " "),
                     symbol = NULL,
                     zero = FALSE,
                     width = NULL,
                     comma = FALSE,
                     precision = NULL,
                     type = NULL) {
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
  if (!is.null(type)) {
    if (res$type == "n") {
      res$comma <- TRUE
      res$type <- "g"
    } else if (type %in% c("e", "g")) {
      res$type <- str_to_lower(type)
    } else if (!type %in% names(fmt_types)) {
      # Map invalid types to the default format.
      # Use something else for default?
      res[["type"]] <- NULL
    }
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

#' @export
print.fmt_spec <- function(x, ...) {
  message("<fmt_spec>: ", format(x, ...))
  invisible(x)
}
