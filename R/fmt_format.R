PREFIXES <- c("y", "z", "a", "f", "p", "n", "\u03BC", "m", "", "k", "M", "G",
              "T", "P", "E", "Z", "Y")

prefix_exponent <- function(x) pmax(-8L, pmin(8L, floor(x %/% 3L))) * 3L

get_prefix <- function(x, p) {
  out <- rep("", length(x))
  fin <- is.finite(x)
  exponent <- fmt_decimal(x[fin], p)$exponent
  out[fin] <- PREFIXES[8L + prefix_exponent(exponent) %/% 3 + 1L]
  out
}

#' Format numbers
#'
#' The function \code{fmt_new} creates a function to format numbers using
#' a given \code{locale} and \code{spec}. The function \code{fmt} formats
#' numbers, and is a convenience function for \code{fmt_new(...)(x)}.
#'
#' @param x A numeric or integer vector
#' @param si \code{NULL} or a numeric vector with length one.
#'   The \href{SI-prefix}{https://en.wikipedia.org/wiki/Metric_prefix#List_of_SI_prefixes}
#'   to use \code{x}. Unlike the \code{"s"} format type, this applies the same
#'   prefix to all values in \code{x}, rather than determining the SI-prefix
#'   on a value by value basis. Additionally, a non-\code{NULL} \code{si} implies
#'   a \code{"f"} format type, in which \code{precision} represents the number of
#'   digits past the decimal point.
#' @param spec A \code{\link{fmt_spec}} object, or a string coerced to a
#'    \code{fmt_spec} object using \code{\link{as_fmt_spec}}, or a list of
#'    arguments passed to \code{fmt_spec}.
#' @param locale A \code{\link{fmt_locale}} object.
#' @return \code{fmt_new} returns a function with a single argument.
#'    \code{fmt} returns a function of the same length as \code{x} of
#'    formatted numbers.
#'
#' @details
#' The supported SI-prefixes are:
#' \itemize{
#' \item{\code{"y"} - yocto, \eqn{10^{-24}}}
#' \item{\code{"z"} - zepto, \eqn{10^{-21}}}
#' \item{\code{"a"} - atto, \eqn{10^{-18}}}
#' \item{\code{"f"} - femto, \eqn{10^{-15}}}
#' \item{\code{"p"} - pico, \eqn{10^{-12}}}
#' \item{\code{"n"} - nano, \eqn{10^{-9}}}
#' \item{\code{"µ"} - micro, \eqn{10^{-6}}}
#' \item{\code{"m"} - milli, \eqn{10^{-3}}}
#' \item{\code{" "} (none) - \eqn{10^0}}
#' \item{\code{"k"} - kilo, \eqn{10^3}}
#' \item{\code{"M"} - mega, \eqn{10^{6}}}
#' \item{\code{"G"} - giga, \eqn{10^{9}}}
#' \item{\code{"T"} - tera, \eqn{10^{12}}}
#' \item{\code{"P"} - peta, \eqn{10^{15}}}
#' \item{\code{"E"} - exa, \eqn{10^{18}}}
#' \item{\code{"Z"} - zetta, \eqn{10^{21}}}
#' \item{\code{"Y"} - yotta, \eqn{10^{24}}}
#' }
#'
#' @export
#' @importFrom dplyr if_else
#' @importFrom purrr invoke %||% is_empty
#' @importFrom stringr str_replace str_pad str_c str_match
#' @examples
#' fmt(c(0.00042, 0.0042), spec = ",.0", si = 1e-6)
fmt_new <- function(spec = NULL, locale = NULL, si = NULL) {
  locale <- locale %||% fmt_default_locale()
  spec <- spec %||% fmt_spec()
  if (!is.null(si)) {
    assert_that(is.number(si))
  }
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

  # If predefined SI Prefix, then type is always f
  if (!is.null(si)) {
    spec$type <- "f"
    si_exp <- max(-8, min(8, floor(exponent(si) / 3))) * 3
    si_k <- 10 ^ -si_exp
    si_prefix <- PREFIXES[8L + prefix_exponent(si_exp) %/% 3 + 1L]
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
  precision <- if (is.null(spec$precision)) {
    if (is_empty(spec$type)) 6L else 12L
  } else if (spec$type %==% c("g", "p", "r", "s")) {
    max(1L, min(21L, spec$precision))
  } else {
    max(0L, min(20L, spec$precision))
  }

  group <- function(x) {
    fmt_group(x, locale$grouping, locale$grouping_mark)
  }

  function(x) {
    n <- length(x)
    x_prefix <- rep(prefix, n)
    x_suffix <- rep(suffix, n)

    # predefined SI prefix, then adjust the prefix
    if (!is.null(si)) {
      x <- si_k * x
    }

    if (spec$type %==% "c") {
      x_suffix <- format_type(x)
      s <- rep("", n)
    } else {
      # Perform the initial formatting.
      neg_x  <- x < 0;
      s <- format_type(abs(x), precision)

      # If a negative value rounds to zero during formatting, treat as positive.
      if (is_empty(spec$type) || spec$type %==% c("f", "g", "d", "e",
                                                  "r", "s", "%")) {
        is_zero <- as.numeric(x) == 0
      } else if (spec$type %in% c("x", "X")) {
        is_zero <- strtoi(x, base = 16L) == 0
      } else if (spec$type %in% c("o")) {
        is_zero <- strtoi(x, base = 8L) == 0
      } else if (spec$type %in% c("b")) {
        is_zero <- strtoi(x, base = 2L) == 0
      } else {
        is_zero <- FALSE
      }
      neg_x[is_zero] <- FALSE

      # Compute the prefix and suffix.
      x_prefix <- str_c(if_else(neg_x,
                                if_else(spec$sign == "(", "(", "-"),
                                if_else(spec$sign %in% c("-", "("), "", spec$sign)),
                        x_prefix)

      x_suffix <- str_c(suffix,
                        if_else(rep(spec$type %==% "s", n),
                                get_prefix(x, precision), ""),
                        if_else(neg_x & spec$sign == "(", ")", ""))

      # Break the formatted value into the integer “value” part that can be
      # grouped, and fractional or exponential “suffix” part that is not.
      if (maybe_suffix) {
        tmp <- str_match(s, "^([0-9]+)(\\.)?(.*)")
        s <- if_else(is.na(tmp[ , 2]), "", tmp[ , 2])
        x_suffix <- str_c(if_else(is.na(tmp[ , 3]), "", locale$decimal_mark),
                          if_else(is.na(tmp[ , 4]), "", tmp[ , 4]),
                          x_suffix)
      }
    }

    # If the fill character is not "0", grouping is applied before padding.
    # if (spec$comma && !(spec$fill != "0" && spec$align != "=")) {
    if (zero) {
      w <- if (!is_empty(spec$width)) spec$width else 0L
      s <- str_c(x_prefix,
                 str_pad(str_c(s, x_suffix),
                         width = w, side = "left", pad = spec$fill))
      if (spec$comma) s <- group(x)
    } else {
      if (spec$comma) s <- group(x)
      if (!is_empty(spec$width) && !is_empty(spec$fill)) {
        s <- switch(spec$align,
                    "<" = str_pad(str_c(x_prefix, s, x_suffix),
                                  width = spec$width,
                                  side = "right", pad = spec$fill),
                    "=" = str_c(x_prefix,
                                str_pad(str_c(s, x_suffix),
                                        width = spec$width - str_length(x_prefix),
                                        side = "left", pad = spec$fill)),
                    "^" = str_pad(str_c(x_prefix, s, x_suffix),
                                  width = spec$width,
                                  side = "both", pad = spec$fill),
                    ">" = str_pad(str_c(x_prefix, s, x_suffix),
                                  width = spec$width,
                                  side = "left", pad = spec$fill))
      } else {
        s <- str_c(x_prefix, s, x_suffix)
      }
    }
    s <- replace_numerals(s, numerals = locale$numerals)
    if (!is.null(si)) {
      s <- str_c(s, si_prefix)
    }
    s
  }
}

#' @rdname fmt_new
#' @export
fmt <- function(x, spec = NULL, locale = NULL, si = NULL) {
  fmt_new(spec = spec, locale = locale, si = si)(x)
}
