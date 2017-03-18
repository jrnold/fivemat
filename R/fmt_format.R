fmt_format <- function(x, spec = NULL, locale = NULL) {
  locale <- locale %||% fmt_default_locale()
  spec <- spec %||% fmt_spec()

  if (!is.spec(spec)) {
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
    str_c("0", str_to_lower(type))
  } else ""
  suffix <- if (spec$symbol == "$") {
    locale$currency[2]
  } else if (spec$type %in% c("%", "p")) {
    "%"
  } else ""

  # What format function should we use?
  # Is this an integer type?
  # Can this type generate exponential notation?
  if (is.null(fmt_type)) {
    fmt_type <- fmt_default
  } else {
    fmt_type <- fmt_types[spec$type]
  }
  maybe_suffix <- (is.null(spec$type) ||
    (spec$type %in% c("d", "e", "f", "g", "p", "r", "s", "%")))

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
    suffix <- format_type(x) + x
    s <- ""
  } else {
    # Perform the initial formatting.
    neg_x  <- value < 0;
    s <- format_type(abs(x), precision)

    # If a negative value rounds to zero during formatting, treat as positive.
    neg_x[as.numeric(s) == 0] <- FALSE

    # Compute the prefix and suffix.
    prefix <- str_c(if_else(neg_x,
                            if_else(spec$sign == "(", "(", "-"),
                            if_else(spec$sign %in% c("-", "("), "", spec$sign)),
                    prefix)

    suffix <- str_c(suffix,
                    if_else(spec$type == "s",
                            prefixes[8L + prefix_exponent / 3 + 1L],
                            ""),
                    if_else(neg_x & spec$sign == "(", ")", ""))


    # Break the formatted value into the integer “value” part that can be
    # grouped, and fractional or exponential “suffix” part that is not.
    if (maybe_suffix) {
    }
  }

  # If the fill character is not "0", grouping is applied before padding.
  if (spec$comma && !(spec$fill != "0" && spec$align != "=")) {
    value <- group(s, Inf)
  }

  s
}
