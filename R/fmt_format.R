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

"%==%" <- function(x, y) !is_empty(x) && x %in% y

#' Format numbers
#'
#' @param x A numeric or integer vector
#' @param spec A \code{\link{fmt_spec}} object, or a string coerced to a
#'    \code{fmt_spec} object using \code{\link{as_fmt_spec}}, or a list of
#'    arguments passed to \code{fmt_spec}.
#' @param locale A \code{\link{fmt_locale}} object.
#' @return A character vector the same length as \code{x} of formatted numbers.
#'
#' @export
#' @importFrom dplyr if_else
#' @importFrom purrr invoke %||% is_empty
#' @importFrom stringr str_replace str_pad str_c str_match
fmt_format <- function(x, spec = NULL, locale = NULL) {
  locale <- locale %||% fmt_default_locale()
  spec <- spec %||% fmt_spec()

  if (!inherits(spec, "fmt_spec")) {
    if (is.character(spec)) {
      spec <- as_fmt_spec(spec)
    } else if (is.list(spec)) {
      spec <- invoke(fmt_spec, spec)
    } else {
      stop("Class ", str_c(class(spec)), " for `spec` is not supported.",
           call. = FALSE)
    }
  }

  prefix <- if (!is_empty(spec$symbol) && spec$symbol == "$") {
    locale$currency[1]
  } else if (!is_empty(spec$symbol) && spec$symbol == "#" &&
             !is_empty(spec$type) && spec$type %in% c("b", "o", "x", "X")) {

    str_c("0", str_to_lower(spec$type))
  } else ""
  suffix <- if (!is_empty(spec$symbol) && spec$symbol == "$") {
    locale$currency[2]
  } else if (!is_empty(spec$symbol) && spec$type %in% c("%", "p")) {
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

  # part related to the data
  n <- length(x)
  x_prefix <- rep(prefix, n)
  x_suffix <- rep(suffix, n)

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
  } else if (!is_empty(spec$width) && !is_empty(spec$fill)) {
    s <- switch(spec$align,
                "<" = str_pad(str_c(x_prefix, s, x_suffix),
                              width = spec$width,
                              side = "right", pad = spec$fill),
                "=" = str_c(x_prefix,
                            str_pad(str_c(s, x_suffix),
                                    width = spec$width - str_len(x_prefix),
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
  s
}
