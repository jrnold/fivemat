# split x into mantissa and exponent
fmt_decimal <- function(x, p) {
  out <- tibble::tibble(
    value = x,
    exponent = exponent(value),
    mantissa = as.integer(round(value * 10 ^ (-exponent - 1 + p)))
  )
  out
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

#' @importFrom stringr str_sub str_length str_c
#' @importFrom stringi stri_reverse
#' @importFrom purrr keep map_chr is_empty
#' @noRd
fmt_group <- function(x, grouping = NULL, sep = ",") {
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

fmt_pad <- function(string, left, right, align = "<",
                    fill = NULL, width = NULL) {
  # null width uses longest string
  if (is.null(width)) {
    lens <- rowsums(cbind(str_length(string),
                          str_length(left),
                          str_length(right)))
    width <- max(lens)
  }
  if (align == "=") {
    str_c(left, str_pad(str_c(string, right),
                        width = width - str_length(left),
                        pad = fill))
  } else {
    side <- switch(align,
                   "<" = "right",
                   "^" = "both",
                   ">" = "left")
    str_pad(str_c(left, string, right), width = width,
            side = side, pad = fill)
  }
}

fmt_pad_zero <- function(x, width = NULL) {
  if (is.null(width)) {
    lens <- rowsums(cbind(str_length(string),
                          str_length(left),
                          str_length(right)))
    width <- max(lens)
  }
  x$string <- str_pad(x$string, width = width, side = "left", pad = "0")
  x
}

fmt_init <- function(x) {
  tibble(value = x,
         left = "",
         right = "",
         center = "")
}

fmt_init_int <- function(x) {
  out <- fmt_init(x)
  out[["value"]] <- as.integer(x)
  out[["not_na"]] <- !is.finite(out[["value"]])
  out[["na"]] <- is.na(out[["value"]])
  out[["negative"]] <- !out[["na"]] & out[["value"]] < 0
  out
}

fmt_init_dbl <- function(x, p = 6, fixed = FALSE) {
  out <- fmt_init(x)
  f <- if (fixed) base::round else base::signif
  out[["value"]] <- f(as.numeric(x), p)
  out[["not_na"]] <- !is.finite(out[["value"]])
  out[["nan"]] <- is.nan(out[["nan"]])
  out[["na"]] <- is.na(out[["value"]]) & !out[["nan"]]
  out[["inf"]] <- is.infinite(out[["value"]])
  out[["negative"]] <- !out[["na"]] & !out[["nan"]] & out[["value"]] < 0
  out
}

fmt_init_chr <- function(x) {
  out <- fmt_init(x)
  out[["value"]] <- as.character(x)
  out
}

fmt_init_si <- function(x, si_prefix) {
  prefix <- si_prefix(si_prefix)
  out <- fmt_init_dbl(x / 10 ^ prefix)
  out[["right"]] <- names(si_prefix)
  out
}

fmt_inf <- function(x, spec, locale) {
  x[["center"]][x[["is_inf"]]] <- locale$inf_mark
  x
}

fmt_na <- function(x, spec, locale) {
  x[["center"]][x[["is_na"]]] <- locale$na_mark
  x
}

fmt_nan <- function(x, spec, locale) {
  x[["center"]][x[["is_nan"]]] <- locale$nan_mark
  x
}

fmt_minus <- function(x, minus, locale) {
  if (minus == "-") {
    out[["left"]] <- str_c(out[["left"]], if_else(out[["negative"]], "-", ""))
  } else if (minus == "+") {
    out[["left"]] <- str_c(out[["left"]], if_else(out[["negative"]], "-", "+"))
  } else if (minus == " ") {
    out[["left"]] <- str_c(out[["left"]], if_else(out[["negative"]], "-", " "))
  } else if (minus == "(") {
    out[["left"]] <- str_c(out[["left"]], if_else(out[["negative"]], "(", ""))
    out[["right"]] <- str_c(out[["left"]], if_else(out[["negative"]], ")", ""))
  }
  out
}

fmt_currency <- function(x, locale) {
  out[["left"]] <- str_c(locale[["currency"]][1], out[["left"]])
  out[["left"]] <- str_c(locale[["currency"]][2], out[["right"]])
  out
}

fmt_type_x <- function(x, spec, locale, capitalize = FALSE) {
  out <- fmt_init_int(x)
  pat <- str_c("%", if (spec$symbol == "#") "#" else "",
               if (capitalize) "x" else "X")
  out[["center"]][out[["not_na"]]] <-
    sprintf(pat, abs(out[["value"]][out[["not_na"]]]))
  out
}

fmt_type_o <- function(x, spec, locale, capitalize = FALSE) {
  out <- fmt_init_int(x)
  out[["center"]][out[["not_na"]]] <-
    str_c(if (spec$symbol == "#") "0o" else "",
          sprintf("%o", abs(out[["value"]][out[["not_na"]]])))
  out
}

fmt_type_b <- function(x, spec, locale) {
  out <- fmt_init_int(x)
  out[["center"]][out[["not_na"]]] <-
    str_c(if (spec$symbol == "#") "0b" else "",
          int2bin(abs(out[["value"]][out[["not_na"]]])))
  out
}

fmt_type_d <- function(x, spec, locale) {
  out <- fmt_init_int(x)
  out[["center"]][out[["not_na"]]] <-
    str_c(sprintf("%d", abs(out[["value"]][out[["not_na"]]])))
  out
}

fmt_type_e <- function(x, spec, locale, capitalize = FALSE) {
  out <- fmt_init_dbl(x)
  p <- spec$precision
  pat <- str_c("%", if (!is.null(p)) "" else str_c(".", p),
               if (capitalize) "e" else "E")
  out[["center"]][out[["not_na"]]] <-
    sprintf(pat, abs(out[["value"]][out[["not_na"]]]))
  out
}

fmt_type_f <- function(x, spec, locale, capitalize = FALSE) {
  out <- fmt_init_dbl(x)
  p <- spec$precision
  pat <- str_c("%", if (!is.null(p)) "" else str_c(".", p), "f")
  out[["center"]][out[["not_na"]]] <-
    sprintf(pat, abs(out[["value"]][out[["not_na"]]]))
  out
}

fmt_type_g <- function(x, spec, locale, capitalize = FALSE) {
  out <- fmt_init_dbl(x)
  p <- spec$precision
  pat <- str_c("%", if (!is.null(p)) "" else str_c(".", p),
               if (capitalize) "g" else "G")
  out[["center"]][out[["not_na"]]] <-
    sprintf(pat, abs(out[["value"]][out[["not_na"]]]))
  out
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

  # If predefined SI Prefix
  if (!is.null(si_prefix)) {
    # si_prefix() will throw an error
    si_prefix <- si_prefix(si_prefix)
    spec$type <- "f"
  }

  # Indicator if 0 fill
  zero <- spec$fill %==% "0" && spec$align %==% "="

  # What format function should we use?
  # Is this an integer type?
  # Can this type generate exponential notation?
  if (is_empty(spec$type)) {
    format_type <- fmt_default
  } else {
    format_type <- fmt_types[[spec$type]]
  }

  # or clamp the specified precision to the supported range.
  # For significant precision, it must be in [1, 21].
  # For fixed precision, it must be in [0, 20].

  precision <- spec$precision


    string[fin] <- replace_numerals(string[fin], numerals = locale$numerals)
    if (!is.null(si_prefix)) {
      string <- str_c(string, str_trim(names(si_prefix)))
    }
    string
  },
  class = c("fmt", "function"))
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
