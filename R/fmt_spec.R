# [[fill]align][sign][symbol][0][width][,][.precision][type]
RE = "^(?:(.)?([<>=^]))?([+\\-\\( ])?([$#])?(0)?(\\d+)?(,)?(\\.\\d+)?([a-z%])?$"

na_else <- function(x, default) {
  dplyr::if_else(!is.na(x), x, default)
}

fmt_types <- list(
  "%" = NULL,
  "b" = NULL,
  "c" = NULL,
  "d" = NULL,
  "e" = NULL,
  "f" = NULL,
  "g" = NULL,
  "o" = NULL,
  "p" = NULL,
  "r" = NULL,
  "s" = NULL,
  "X" = NULL,
  "x" = NULL,
  # default
  NULL = NULL
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
