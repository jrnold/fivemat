PREFIXES <- c("y", "z", "a", "f", "p", "n", "\u03BC", "m", "", "k", "M", "G",
              "T", "P", "E", "Z", "Y")

prefix_exponent <- function(x) pmax(-8L, pmin(8L, floor(x %/% 3L))) * 3L

get_prefix <- function(x) {
  out <- rep("", length(x))
  fin <- is.finite(x)
  exponent <- fmt_decimal(x[fin])$exponent
  out[fin] <- PREFIXES[8L + prefix_exponent(exponent) %/% 3 + 1L]
  out
}


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
#' @importFrom purrr invoke %||%
#' @importFrom stringr str_replace
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

  prefix <- if (spec$symbol == "$") {
    locale$currency[1]
  } else if (spec$symbol == "#" && spec$type %in% c("b", "o", "x", "X")) {
    str_c("0", str_to_lower(spec$type))
  } else ""
  suffix <- if (spec$symbol == "$") {
    locale$currency[2]
  } else if (spec$type %in% c("%", "p")) {
    "%"
  } else ""

  # What format function should we use?
  # Is this an integer type?
  # Can this type generate exponential notation?
  if (is.null(spec$type)) {
    format_type <- fmt_default
  } else {
    format_type <- fmt_types[spec$type]
  }
  maybe_suffix <- {is.null(spec$type) || # nolint
      {spec$type %in% c("d", "e", "f", "g", "p", "r", "s", "%")}} # nolint

  # or clamp the specified precision to the supported range.
  # For significant precision, it must be in [1, 21].
  # For fixed precision, it must be in [0, 20].
  precision <- if (is.null(spec$precision)) {
    if (is.null(spec$type)) 6L else 12L
  } else if (spec$type %in% c("g", "p", "r", "s")) {
    max(1, min(21, spec$precision))
  } else {
    max(0, min(20, spec$precision))
  }

  # part related to the data
  if (spec$type == "c") {
    suffix <- format_type(x)
    s <- ""
  } else {
    # Perform the initial formatting.
    neg_x  <- x < 0;
    s <- format_type(abs(x), precision)

    # If a negative value rounds to zero during formatting, treat as positive.
    neg_x[as.numeric(s) == 0] <- FALSE

    # Compute the prefix and suffix.
    prefix <- str_c(if_else(neg_x,
                            if_else(spec$sign == "(", "(", "-"),
                            if_else(spec$sign %in% c("-", "("), "", spec$sign)),
                    prefix)

    suffix <- str_c(suffix,
                    if_else(spec$type == "s", get_prefix(x), ""),
                    if_else(neg_x & spec$sign == "(", ")", ""))

    # Break the formatted value into the integer “value” part that can be
    # grouped, and fractional or exponential “suffix” part that is not.
    if (maybe_suffix) {
    }
  }

  # If the fill character is not "0", grouping is applied before padding.
  if (spec$comma && !(spec$fill != "0" && spec$align != "=")) {
    NULL
  }

  s
}
