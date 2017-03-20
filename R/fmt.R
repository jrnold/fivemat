#' @importFrom stringr str_sub str_length str_c
#' @importFrom stringi stri_reverse
#' @importFrom purrr keep map_chr is_empty
#' @noRd
fmt_group <- function(x, grouping = NULL, sep = ",") {
  if (is_empty(grouping)) return(x)
  if (is_empty(x)) return(character())
  intvls <- rep_len(grouping, max(str_length(x)))
  start <- cumsum(c(1L, intvls[-length(intvls)]))
  end <- start + intvls - 1L
  f <- function(x, start, end) {
    res <- keep(str_sub(stri_reverse(x), start, end), function(s) s != "")
    res <- stri_reverse(str_c(res, collapse = sep))
    res
  }
  map_chr(x, f, start = start, end = end)
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
#' @param inf_mark String used for \code{Inf} values.
#' @param nan_mark String used for \code{NaN} values.
#' @param na_mark String used for \code{NA} values.
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
fmt_new <- function(spec = NULL, locale = NULL, si_prefix = NULL,
                    inf_mark = "Inf", na_mark = "NA",
                    nan_mark = "NaN") {
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
      assert_that(is.string(locale))
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
  assert_that(is.string(inf_mark))
  assert_that(is.string(na_mark))
  assert_that(is.string(nan_mark))

  # If predefined SI Prefix, then type is always f
  if (!is.null(si_prefix)) {
    assert_that(is.number(si_prefix) |
                  is.string(si_prefix) |
                  is.flag(si_prefix))
  }
  if (!is.null(si_prefix)) {
    spec$type <- "f"
    if (is.logical(si_prefix)) {
      si_prefix <- if (si_prefix) precision_prefix(si_prefix) else NULL
    } else {
      si_prefix <- si_prefix(si_prefix)
      if (is.na(si_prefix)) {
        stop("`", si_prefix, "` is an invalid SI prefix", call. = FALSE)
      }
    }
  }

  prefix <- if (spec$symbol %==% "$") {
    locale$currency[1]
  } else if (spec$symbol %==% "#") {
    if (spec$type %==% c("b", "o", "x", "X")) {
      str_c("0", str_to_lower(spec$type))
    } else if (spec$type %==% c("a", "A")) {
      str_c("0x")
    }
  } else ""

  suffix <- if (spec$symbol %==% "$") {
    locale$currency[2]
  } else if (spec$type %==% c("%", "p")) {
    "%"
  } else ""
  zero <- spec$fill %==% "0" && spec$align %==% "="

  # What format function should we use?
  # Is this an integer type?
  # Can this type generate exponential notation?
  if (is_empty(spec$type)) {
    format_type <- fmt_default
  } else {
    format_type <- fmt_types[[spec$type]]
  }
  maybe_suffix <- {is_empty(spec$type) || # nolint
      {spec$type %==% c("d", "e", "f", "g", "p", "r", "s", "%")}} # nolint

  # or clamp the specified precision to the supported range.
  # For significant precision, it must be in [1, 21].
  # For fixed precision, it must be in [0, 20].
  spec$precision <- if (is.null(spec$precision)) {
    if (is_empty(spec$type)) 12L else 6L
  } else if (spec$type %==% c("g", "p", "r", "s")) {
    max(1L, min(21L, spec$precision))
  } else {
    max(0L, min(20L, spec$precision))
  }
  precision <- spec$precision

  group <- function(x, width = 0L) {
    if (zero) {
      # for sanity in regexes, replace , afterwardsb
      x <- fmt_group(x, locale$grouping, ",")
      # remove leading 0's and , above the width
      x <- str_replace(x, paste0("^[0,]+((?:0,|[^,]).{", width - 1L, "})$"),
                       "\\1")
      x <- str_replace_all(x, ",", locale$grouping_mark)
      x
    } else {
      fmt_group(x, locale$grouping, locale$grouping_mark)
    }
  }

  structure(function(x) {
    finite_x <- is.finite(x)
    n <- length(x)
    x_prefix <- rep(prefix, n)
    x_suffix <- rep(suffix, n)

    # predefined SI prefix, then divide number by that amount
    if (!is.null(si_prefix)) {
      x <- x * 10 ^ -si_prefix
    }

    if (spec$type %==% c("c", "u")) {
      x_suffix <- str_c(format_type(x), x_suffix)
      s <- rep("", n)
    } else {
      # Perform the initial formatting.
      s <- character(length(x))
      s[is.nan(x)] <- nan_mark
      s[is.na(x) & !is.nan(x)] <- na_mark
      s[is.infinite(x)] <- inf_mark
      s[finite_x] <- format_type(abs(x[finite_x]), precision)

      # negative values
      neg_x <- !is.na(x) & x < 0

      # If a negative value rounds to zero during formatting, treat as positive.
      if (is_empty(spec$type) ||
          spec$type %==% c("f", "g", "d", "e", "r", "s", "%", "a", "A")) {
        is_zero <- as.numeric(s[finite_x]) == 0
      } else if (spec$type %in% c("x", "X")) {
        is_zero <- strtoi(s, base = 16L) == 0
      } else if (spec$type %in% c("o")) {
        is_zero <- strtoi(s[finite_x], base = 8L) == 0
      } else if (spec$type %in% c("b")) {
        is_zero <- strtoi(s[finite_x], base = 2L) == 0
      } else {
        is_zero <- FALSE
      }
      neg_x[is_zero] <- FALSE

      # Compute the prefix and suffix.
      x_prefix <- str_c(if_else(neg_x,
                                if_else(spec$sign == "(", "(", "-"),
                                if_else(spec$sign %in% c("-", "("),
                                        "", spec$sign)),
                        x_prefix)
      x_suffix <- str_c(suffix,
                        if_else(rep(spec$type %==% "s", n),
                                names(si_prefix.numeric(x)), ""),
                        if_else(neg_x & spec$sign == "(", ")", ""))

      # Break the formatted value into the integer “value” part that can be
      # grouped, and fractional or exponential “suffix” part that is not.
      if (maybe_suffix) {
        tmp <- str_match(s, "^([0-9]+)(\\.)?(.*)")
        s <- case_when(
          !finite_x       ~ s,
          is.na(tmp[, 2]) ~ "",
          TRUE            ~ tmp[, 2]
        )
        x_suffix <- str_c(if_else(is.na(tmp[ , 3]), "", locale$decimal_mark),
                          if_else(is.na(tmp[ , 4]), "", tmp[ , 4]),
                          x_suffix)
      }
    }

    # If the fill character is not "0", grouping is applied before padding.
    # if (spec$comma && !(spec$fill != "0" && spec$align != "=")) {
    if (zero) {
      if (!is_empty(spec$width)) {
        w <- spec$width - str_length(x_prefix) - str_length(x_suffix)
        # TODO: How should NaN, NA, ... be handled?
        # pad them with spaces
        if (spec$type %==% "c") {
          s <- str_pad(s, width = w, side = "left", pad = "0")
        } else {
          s[!finite_x] <- str_pad(s[!finite_x], width = w, side = "left",
                                  pad = "0")
          s[finite_x] <- str_pad(s[finite_x], width = w, side = "left",
                                  pad = "0")
        }
        if (spec$comma) {
          s[finite_x] <- group(s[finite_x], width = w + str_length(x_prefix))
        }
      }
      s <- str_c(x_prefix, s, x_suffix)
    } else {
      if (spec$comma) {
        s[finite_x] <- group(s[finite_x])
      }
      if (!is_empty(spec$width) && !is_empty(spec$fill)) {
        if (spec$align == "<") {
          s <- str_pad(str_c(x_prefix, s, x_suffix), width = spec$width,
                             side = "right", pad = spec$fill)
        } else if (spec$align == "=") {
          s <- str_c(x_prefix,
                     str_pad(str_c(s, x_suffix),
                             width = (spec$width - str_length(x_prefix)),
                             side = "left", pad = spec$fill))
        } else if (spec$align == "^") {
          s <- str_pad(str_c(x_prefix, s, x_suffix), width = spec$width,
                       side = "both", pad = spec$fill)
        } else {
          s <- str_pad(str_c(x_prefix, s, x_suffix), width = spec$width,
                       side = "left", pad = spec$fill)
        }
      } else {
        # needs to be here because = doesn't paste these before.
        s <- str_c(x_prefix, s, x_suffix)
      }
    }
    s <- replace_numerals(s, numerals = locale$numerals)
    if (!is.null(si_prefix)) {
      s <- str_c(s, str_trim(names(si_prefix)))
    }
    s
  }, class = c("fmt", "function"))
}

#' @export
print.fmt <- function(x, ...) {
  cat("<fmt>\n")
  print(environment(x)$spec)
  print(environment(x)$locale)
  cat("Inf = ", environment(x)$inf_mark,
      ", NA = ", environment(x)$na_mark,
      ", NaN = ", environment(x)$nan_mark, "\n")
  invisible(x)
}

#' @rdname fmt_new
#' @export
fmt <- function(x, spec = NULL, locale = NULL, ...) {
  fmt_new(spec = spec, locale = locale, ...)(x)
}
