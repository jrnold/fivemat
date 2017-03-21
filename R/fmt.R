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
  maybe_suffix <- is_empty(spec$type) || # nolint
      {spec$type %==% c("%", "d", "e", "E", "f", "g", "G", "p", "r", "s")} # nolint

  # or clamp the specified precision to the supported range.
  # For significant precision, it must be in [1, 21].
  # For fixed precision, it must be in [0, 20].
  spec$precision <- if (is.null(spec$precision)) {
    if (test_types(spec$type, default = TRUE)) 12L else 6L
  } else if (test_types(spec$type, c("g", "G", "p", "r", "s"))) {
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
    n <- length(x)
    prefix <- rep("", n)
    suffix <- rep("", n)
    string <- rep("", n)
    si_prefix_str <- rep("", n)

    if (test_types(spec$type, "c")) {
      is_na <- is.na(x)
      fin <- !is_na
      pos_inf <- neg_inf <- is_nan <- rep(FALSE, n)
    } else {
      fin <- is.finite(x)
      pos_inf <- is.infinite(x) & x > 0
      neg_inf <- is.infinite(x) & x < 0
      is_nan <- is.nan(x)
      is_na <- is.na(x) & !is.nan(x)
      # Handle special values: NA, NaN, +/-Inf
      string[is_nan] <- locale$nan_mark %||% "NaN"
      string[pos_inf | neg_inf] <- locale$inf_mark %||% "Inf"
    }
    string[is_na] <- locale$na_mark %||% "NA"

    # need to ensure that prefixes aren't put before special values
    if (spec$symbol %==% "$") {
      prefix[fin] <- locale$currency[1]
    } else if (spec$symbol %==% "#") {
      if (test_types(spec$type, c("b", "o", "x", "X"))) {
        prefix[fin] <- str_c("0", str_to_lower(spec$type))
      } else if (test_types(spec$type, c("a", "A"))) {
        prefix[fin] <- str_c("0x")
      }
    }

    if (spec$symbol %==% "$") {
      suffix[fin] <- locale$currency[2]
    } else if (test_types(spec$type, c("%", "p"))) {
      suffix[fin] <- "%"
    }

    if (test_types(spec$type, c("c", "u"))) {
      suffix[fin] <- str_c(format_type(x[fin]), suffix[fin])
      string[fin] <- ""
    } else {
      # if predefined SI prefix, then divide number by that amount
      if (!is.null(si_prefix)) {
        x[fin] <- x[fin] * 10 ^ -si_prefix
      }
      # Perform the initial formatting.
      xfmt <- format_type(abs(x[fin]), precision)
      string[fin] <- xfmt
      if (!is.null(attr(xfmt, "si_prefix"))) {
        si_prefix_str[fin] <- attr(xfmt, "si_prefix")
      }
    }

    # find negative values
    neg_x <- neg_inf
    if (!test_types(spec$type, c("c", "u"))) {
      neg_x <- neg_x | (fin & x < 0)
      # negative values
      # If a negative value rounds to zero during formatting, treat as positive.
      is_zero <- rep(FALSE, n)
      is_zero[fin] <- if (test_types(spec$type,
                                     c("%", "d", "e", "E",
                                       "f", "g", "G", "p", "r", "s"), TRUE)) {
        as.numeric(string[fin]) == 0
      } else if (test_types(spec$type, c("x", "X"))) {
        strtoi(string[fin], base = 16L) == 0
      } else if (test_types(spec$type, c("o"))) {
        strtoi(string[fin], base = 8L) == 0
      } else if (test_types(spec$type, c("b"))) {
        strtoi(string[fin], base = 2L) == 0
      } else if (test_types(spec$type, c("a", "A"))) {
        as.numeric(str_c("0x", string[fin])) == 0
      }
      neg_x <- neg_x & !is_zero
    }

    # Compute Signs
    if (test_types(spec$type, c("c", "u"))) {
      # only do signs for +/-Inf
      # I'm not sure whether this is worth it. Maybe handle this by
      # having separate marks for -Inf and +Inf
      prefix[neg_inf] <- str_c(if_else(spec$sign == "(", "(", "-"),
                                prefix[neg_inf])
      prefix[pos_inf] <- str_c(if_else(spec$sign %in% c("-", "("),
                                        "", spec$sign), prefix[pos_inf])
      suffix[neg_inf] <- str_c(suffix[neg_inf],
                                if_else(spec$sign == "(", ")", ""))
    } else {
      prefix <-
        str_c(case_when(
          neg_x ~ if_else(spec$sign == "(", "(", "-"),
          fin | (!is.na(x) & x == Inf) ~
          if_else(spec$sign %in% c("-", "("), "", spec$sign),
          TRUE ~ ""
        ), prefix)
      suffix <-
        str_c(suffix, si_prefix_str,
              if_else(neg_x & spec$sign == "(", ")", ""))
    }

    # Break the formatted value into the integer “value” part that can be
    # grouped, and fractional or exponential “suffix” part that is not.
    if (maybe_suffix & any(fin)) {
      tmp <- str_match(string[fin], "^([0-9]+)(\\.)?(.*)")
      string[fin] <- if_else(is.na(tmp[, 2]), "", tmp[, 2])
      suffix[fin] <- str_c(if_else(is.na(tmp[, 3]), "", locale$decimal_mark),
                            if_else(is.na(tmp[, 4]), "", tmp[, 4]),
                            suffix[fin])
    }

    # If the fill character is not "0", grouping is applied before padding.
    # if (spec$comma && !(spec$fill != "0" && spec$align != "=")) {
    if (zero) {
      if (!is_empty(spec$width)) {
        w <- spec$width - str_length(prefix) - str_length(suffix)
        string[fin] <- str_pad(string[fin], width = w, side = "left",
                               pad = "0")
        # Does it make sense to pad Inf, NaN, NA with 0?
        # Not really, but it doesn't make sense to pad then in many other cases
        string[!fin] <- str_pad(string[!fin], width = w,
                                side = "left", pad = " ")
        if (spec$comma & !test_types(spec$type, "c")) {
          # Only group finite values
          string[fin] <-
            group(string[fin], width = w + str_length(prefix[fin]))
        }
      }
      string <- str_c(prefix, string, suffix)
    } else {
      if (spec$comma & !test_types(spec$type, "c")) {
        string[fin] <- group(string[fin])
      }
      if (!is_empty(spec$width) && !is_empty(spec$fill)) {
        if (spec$align == "<") {
          string <- str_pad(str_c(prefix, string, suffix), width = spec$width,
                             side = "right", pad = spec$fill)
        } else if (spec$align == "=") {
          string <- str_c(prefix,
                     str_pad(str_c(string, suffix),
                             width = (spec$width - str_length(prefix)),
                             side = "left", pad = spec$fill))
        } else if (spec$align == "^") {
          string <- str_pad(str_c(prefix, string, suffix), width = spec$width,
                            side = "both", pad = spec$fill)
        } else {
          string <- str_pad(str_c(prefix, string, suffix), width = spec$width,
                            side = "left", pad = spec$fill)
        }
      } else {
        # needs to be here because = doesn't paste these before.
        string <- str_c(prefix, string, suffix)
      }
    }
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
