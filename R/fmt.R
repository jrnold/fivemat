#' @importFrom tibble tibble
fmt_init <- function(x) {
  tibble(value = x,
         prefix = "",
         postfix = "",
         string = "")
}

# split x into mantissa and exponent
fmt_decimal <- function(x, p) {
  value <- NULL
  out <- tibble::tibble(
    value = x,
    exponent = exponent(value),
    mantissa = as.integer(round(value * 10 ^ (-exponent - 1 + p)))
  )
  out
}

fmt_inf <- function(x, locale) {
  x[["string"]][x[["is_inf"]]] <- locale$inf_mark
  x
}

fmt_na <- function(x, locale) {
  x[["string"]][x[["is_na"]]] <- locale$na_mark
  x
}


fmt_nan <- function(x, locale) {
  x[["string"]][x[["is_nan"]]] <- locale$nan_mark
  x
}

fmt_pad_zero <- function(x, spec, locale) {
  if (spec$align == "=" && spec$fill == "0") {
    width <- spec$width
    fin <- x$not_na
    if (is.null(width)) {
      lens <- rowSums(cbind(str_length(x$string[fin]),
                            str_length(x$prefix[fin]),
                            str_length(x$postfix[fin])))
      width <- max(lens)
    }
    x$string[fin] <- str_pad(x$string[fin], width = width, side = "left",
                             pad = "0")
    x
  } else x
}

fmt_init <- function(x) {
  tibble(value = x,
         string = "",
         prefix = "",
         postfix = "")
}

fmt_init_int <- function(x, locale) {
  out <- fmt_init(x)
  out[["value"]] <- as.integer(x)
  out[["not_na"]] <- !is.na(out[["value"]])
  out[["na"]] <- is.na(out[["value"]])
  out[["negative"]] <- !out[["na"]] & out[["value"]] < 0
  out <- fmt_na(out, locale)
  out
}

fmt_init_dbl <- function(x, p, locale, fixed = FALSE) {
  out <- fmt_init(x)
  # by rounding before formatting, numbers rounede to 0 will
  # be formatted correctly without minus signs.
  f <- if (fixed) base::round else base::signif
  out[["value"]] <- f(as.numeric(x), p)
  out[["not_na"]] <- !is.finite(out[["value"]])
  out[["nan"]] <- is.nan(out[["nan"]])
  out[["na"]] <- is.na(out[["value"]]) & !out[["nan"]]
  out[["inf"]] <- is.infinite(out[["value"]])
  out[["negative"]] <- !out[["na"]] & !out[["nan"]] & out[["value"]] < 0
  out <- fmt_na(out, locale)
  out <- fmt_nan(out, locale)
  out <- fmt_inf(out, locale)
  out
}

fmt_init_chr <- function(x, locale) {
  out <- fmt_init(x)
  out[["value"]] <- as.character(x)
  out <- fmt_na(out, locale)
  out
}

#' @importFrom purrr map2_chr
fmt_rounded <- function(x, p, locale) {
  p <- min(max(1L, p), 21L)
  out <- fmt_init_dbl(x, p, locale, fixed = FALSE)
  value <- out$value[out$not_na]
  k <- -exponent(value) + p - 1L
  f <- function(i, j) sprintf(str_c("%.", j, "f"), i)
  out$string[out$not_na] <- map2_chr(value, k, f)
  out
}

fmt_init_si <- function(x, p, locale, si_prefix = NULL) {
  p <- min(max(0L, p), 20L)
  if (is.null(si_prefix)) {
    si_prefix <- exponent(x)
  }
  prefix <- si_prefix(si_prefix)
  out <- fmt_init_dbl(x / 10 ^ prefix, p, locale, fixed = TRUE)
  out[["postfix"]] <- names(si_prefix)
  out
}

#' Format Types
fmt_types <- list()

fmt_types[["b"]] <- function(x, locale) {
  out <- fmt_init_int(x, locale)
  out[["string"]][out[["not_na"]]] <-
    int2bin(abs(out[["value"]][out[["not_na"]]]))
  out
}

fmt_type_e <- function(x, p, locale, capitalize = FALSE) {
  p <- min(max(1L, p), 21L)
  out <- fmt_init_dbl(x, p, locale, fixed = FALSE)
  pat <- str_c("%", if (!is.null(p)) "" else str_c(".", p),
               if (capitalize) "e" else "E")
  out[["string"]][out[["not_na"]]] <-
    sprintf(pat, abs(out[["value"]][out[["not_na"]]]))
  out
}

fmt_types[["e"]] <- function(x, spec, locale) {
  fmt_type_e(x, spec$precision, locale, capitalize = FALSE)
}

fmt_types[["E"]] <- function(x, spec, locale) {
  fmt_type_e(x, spec$precision, locale, capitalize = TRUE)
}

fmt_types[["d"]] <- function(x, spec, locale) {
  out <- fmt_init_int(x, locale)
  out[["string"]][out[["not_na"]]] <-
    str_c(sprintf("%d", abs(out[["value"]][out[["not_na"]]])))
  out
}

fmt_types[["f"]] <- function(x, spec, locale, capitalize = FALSE) {
  p <- min(max(0L, spec$precision), 20L)
  out <- fmt_init_dbl(x, p, locale, fixed = TRUE)
  pat <- str_c("%", if (!is.null(p)) "" else str_c(".", p), "f")
  out[["string"]][out[["not_na"]]] <-
    sprintf(pat, abs(out[["value"]][out[["not_na"]]]))
  out
}

fmt_type_g <- function(x, spec, locale, capitalize = FALSE) {
  p <- min(max(1L, spec$precision), 21L)
  out <- fmt_init_dbl(x, spec$precision, locale, fixed = TRUE)
  pat <- str_c("%", if (!is.null(p)) "" else str_c(".", p),
               if (capitalize) "g" else "G")
  fin <- out[["not_na"]]
  out[["string"]][fin] <- sprintf(pat, abs(out[["value"]][fin]))
  out
}

fmt_types[["g"]] <- function(x, spec, locale) {
  fmt_type_g(x, spec, locale, capitalize = FALSE)
}

fmt_types[["G"]] <- function(x, spec, locale) {
  fmt_type_g(x, spec, locale, capitalize = TRUE)
}

fmt_types[["o"]] <- function(x, spec, locale, capitalize = FALSE) {
  out <- fmt_init_int(x, locale)
  out[["string"]][out[["not_na"]]] <-
    sprintf("%o", abs(out[["value"]][out[["not_na"]]]))
  out
}

fmt_types[["p"]] <- function(x, spec, locale) {
  out <- fmt_rounded(x, spec$precision, locale)
  out[["postfix"]] <- str_c(spec$percent_mark, out[["postfix"]])
  out
}

fmt_types[["r"]] <- function(x, spec, locale) {
  fmt_rounded(x, spec$precision, locale)
}

fmt_type_x <- function(x, spec, locale, capitalize = FALSE) {
  out <- fmt_init_int(x)
  pat <- str_c("%", if (capitalize) "x" else "X")
  out[["string"]][out[["not_na"]]] <-
    sprintf(pat, abs(out[["value"]][out[["not_na"]]]))
  out
}

fmt_types[["x"]] <- function(x, spec, locale) {
  fmt_type_x(x, spec, locale, capitalize = FALSE)
}

fmt_types[["X"]] <- function(x, spec, locale) {
  fmt_type_x(x, spec, locale, capitalize = TRUE)
}

fmt_types[["%"]] <- function(x, spec, locale, capitalize = FALSE) {
  out <- fmt_types[["f"]](x * 100)
  out[["postfix"]] <- str_c(spec$percent_mark, out[["postfix"]])
  out
}

fmt_symbols <- list(
  "#" = function(x, spec, locale) {
    if (spec$type %in% c("a", "A", "x", "X")) {
      x[["prefix"]] <- str_c("0x", x[["prefix"]])
    } else if (spec$type %in% c("o")) {
      x[["prefix"]] <- str_c("0o", x[["prefix"]])
    } else if (spec$type %in% c("b")) {
      x[["prefix"]] <- str_c("0b", x[["prefix"]])
    }
    x
  },
  "$" = function(x, spec, locale) {
    x[["prefix"]] <- str_c(locale[["currency"]][1], x[["prefix"]])
    x[["prefix"]] <- str_c(locale[["currency"]][2], x[["postfix"]])
    x
  }
)

fmt_negative <- function(x, spec, locale) {
  minus <- spec$sign
  if (minus == "-") {
    x[["prefix"]] <- str_c(x[["prefix"]], if_else(x[["negative"]], "-", ""))
  } else if (minus == "+") {
    x[["prefix"]] <- str_c(x[["prefix"]], if_else(x[["negative"]], "-", "+"))
  } else if (minus == " ") {
    x[["prefix"]] <- str_c(x[["prefix"]], if_else(x[["negative"]], "-", " "))
  } else if (minus == "(") {
    x[["prefix"]] <- str_c(x[["prefix"]], if_else(x[["negative"]], "(", ""))
    x[["postfix"]] <- str_c(x[["prefix"]], if_else(x[["negative"]], ")", ""))
  }
  x
}


#' @importFrom stringr str_sub str_length str_c str_split_fixed
#' @importFrom stringi stri_reverse
#' @importFrom purrr keep map_chr is_empty
#' @noRd
fmt_group <- function(x, spec, locale) {
  grouping <- spec$grouping
  sep <- locale$grouping_mark
  if (is_empty(grouping)) {
    return(x)
  }
  if (is_empty(x)) {
    return(character())
  }
  # split integer from digits or exponent
  # use ?= so that the splitting part is kept
  split_pattern <- "(?=\\.|[eE][+-])"
  x_split <- str_split_fixed(x, split_pattern, 2L)
  intvls <- rep_len(grouping, max(str_length(x_split[, 1])))
  start <- cumsum(c(1L, intvls[-length(intvls)]))
  end <- start + intvls - 1L
  f <- function(x, start, end) {
    res <- keep(str_sub(stri_reverse(x), start, end), function(s) s != "")
    res <- stri_reverse(str_c(res, collapse = sep))
    res
  }
  str_c(map_chr(x_split[ , 1], f, start = start, end = end),
        x_split[ , 2])
}

# Truncate leading zeros
trunc_zeros <- function(x, width = NULL) {
  if (is.null(width)) {
    out <- str_replace(x, "^[0,]*", "")
  } else {
    out <- str_replace(x, str_c("^([0,]*)(.{", width, ",})"), "\\2")
  }
  str_replace(out, "^,", " ")
}

fmt_pad <- function(x, spec, locale) {
  align <- spec$align
  fill <- spec$fill
  if (is.null(spec$align) || (spec$fill == "0" && spec$align == "=")) {
    return(x)
  }
  width <- spec$width
  # null width uses longest string
  if (is.null(width)) {
    lens <- rowSums(cbind(str_length(x$string),
                          str_length(x$prefix),
                          str_length(x$postfix)))
    width <- max(lens)
  }
  if (align == "=") {
    str_c(x$prefix,
          str_pad(str_c(x$string, x$postfix),
                        width = width - str_length(x$prefix),
                        pad = fill))
  } else {
    side <- switch(align,
                   "<" = "left",
                   "^" = "both",
                   ">" = "right")
    str_pad(str_c(x$prefix, x$string, x$postfix), width = width,
            side = side, pad = fill)
  }
}


#' Format numbers
#'
#' The function \code{fmt_new} creates a function to format numbers using
#' a given \code{locale} and \code{spec}. The function \code{fmt} formats
#' numbers, and is a convenience function for \code{fmt_new(...)(x)}.
#'
#'
#' @param x A numeric or integer vector
#' @param si_prefix If non-\code{NULL}, then use
#'   an \href{https://en.wikipedia.org/wiki/Metric_prefix#List_of_SI_prefixes}{SI prefix} to format \code{x}.
#'   If \code{TRUE}, then use \code{precision_prefix} to automatically determine
#'   the SI prefix to use. Otherwise, it can be any of the valid arguments
#'   for \code{\link{si_prefix}}: a string with the prefix name, an integer with the
#'   SI prefix exponent, or a numeric value.
#'   Unlike the \code{"s"} format type, this applies the same
#'   prefix to all values in \code{x}, rather than determining the SI-prefix
#'   on a value by value basis. Additionally, a non-\code{NULL} \code{si} implies
#'   a \code{"f"} format type, in which \code{precision} represents the number of
#'   digits past the decimal point.
#' @param spec A \code{\link{fmt_spec}} object, or a string coerced to a
#'    \code{fmt_spec} object using \code{\link{as_fmt_spec}}, or a list of
#'    arguments passed to \code{fmt_spec}.
#' @param locale A \code{\link{fmt_locale}} object.
#' @param ... Arguments passed to \code{fmt_new}.
#' @return \code{fmt_new} returns a function with a single argument.
#'    \code{fmt} returns a function of the same length as \code{x} of
#'    formatted numbers.
#'
#' @export
#' @importFrom dplyr if_else
#' @importFrom purrr invoke %||% is_empty
#' @importFrom stringr str_c str_pad str_match str_replace str_replace_all
#' @importFrom stringr str_trim
#' @examples
#' fmt(c(0.00042, 0.0042), spec = ",.0", si_prefix = 1e-6)
fmt_new <- function(spec = NULL, locale = NULL, si_prefix = NULL) {
  locale <- locale %||% fmt_default_locale()
  spec <- spec %||% fmt_spec()
  if (!inherits(spec, "fmt_spec")) {
    if (is.character(spec)) {
      assert_that(is.string(spec))
      spec <- as_fmt_spec(spec)
    } else if (is.list(spec)) {
      spec <- invoke(fmt_spec, spec)
    } else {
      stop("Class ", str_c(class(spec), collapse = ", "),
           " for `spec` is not supported.",
           call. = FALSE)
    }
  }
  if (!inherits(locale, "fmt_locale")) {
    if (is.character(locale)) {
      if (length(locale) != 1) {
        stop("If character, `locale` must have a length of one.",
             call. = FALSE)
      }
      # en_US -> en-US
      locale <- str_replace(locale, "_", "-")
      locale <- fmt_locales[[locale]]
      if (is.null(locale)) {
        stop(str_c("Locale `", locale, "` not found in `fmt_locales`.\n",
             "Available locales: ", str_c(names(fmt_locales), collapse = ", ")),
             call. = FALSE)
      }
    } else {
      stop("Class ", str_c(class(locale), collapse = ", "),
           " for `locale` is not supported.",
           call. = FALSE)
    }
  }
  f <- function(x) {
    # initial formatting
    out <- fmt_types[[spec$type]](x, spec, locale)
    # process symbols
    out <- fmt_symbols[["$"]](out, spec, locale)
    out <- fmt_symbols[["#"]](out, spec, locale)
    # process negative (-)
    out <- fmt_negative(out, spec, locale)
    # zero padding
    out <- fmt_pad_zero(out, spec, locale)
    # grouping
    out <- fmt_group(out, spec, locale)
    # alignment and padding - output to string
    fmt_pad(out, spec, locale)
  }
  structure(f, class = c("fmt", "function"))
}

# nocov start
#' @export
print.fmt <- function(x, ...) {
  cat("<fmt>\n")
  print(environment(x)$spec)
  print(environment(x)$locale)
  invisible(x)
}
# nocov end

#' @rdname fmt_new
#' @export
fmt <- function(x, spec = NULL, locale = NULL, ...) {
  fmt_new(spec = spec, locale = locale, ...)(x)
}
