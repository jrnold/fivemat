re = "^(?:(.)?([<>=^]))?([+\\-\\( ])?([$#])?(0)?(\\d+)?(,)?(\\.\\d+)?([a-z%])?$"

to_fixed <- function(x, p) sprintf(paste0("%e.", p), x)

to_exponential <- function(x, p)

to_precision <- function(x, p)

fmt_types <- list(
  "" = NULL, #formatDefault,
  "%" = function(x, p) sprintf(paste0("%f.", p), x * 100),
  "b" = function(x) as.character(R.utils::intToBin(round(x))),
  "c" = as.character,
  "d" = function(x) as.character(round(x)),
  "e" = function(x, p) sprintf(paste0("%e.", p), x),
  "f" = function(x, p) sprintf(paste0("%d.", p), x),
  "g" = function(x, p) sprintf(paste0("%g.", p), x),
  "o" = function(x) format(as.octmode(x)),
  "p" = NULL, # function(x, p) { return formatRounded(x * 100, p); },
  "r" = NULL, # formatRounded,
  "s" = NULL, # formatPrefixAuto,
  "X" = function(x) format(as.hexmode(round(x)), upper.case = TRUE),
  "x" = function(x) format(as.hexmode(round(x)), upper.case = FALSE)
)

fmt_spec <- function(x) {
  match <- str_match(x)

  res <- list(
    fill = match[1] %||% " ",
    align = match[2] %||% ">",
    sign = match[3] %||% "-",
    symbol = match[4] %||% "",
    zero = !is.na(match[5]),
    width = as.numeric(match[6]),
    comma = !is.na(match[7]),
    precision = match[8], # && +match[8].slice(1),
    type = match[9] %||% ""
  )

  # The "n" type is an alias for ",g".
  if (res$type == "n") {
    comma <- TRUE
    type <- "g"
  } else if (!type %in% names(format_types)) {
    type <- ""
  }

  # If zero fill is specified, padding goes after sign and before digits.
  if (res$zero %||% (res$fill == "0" && res$align == "=")) {
    res$zero <- TRUE
    res$fill <- "0"
    res$align <- "="
  }
  structure(res, class = "fmt_spec")
}

print.fmt_spec(x) {
  mst <- paste0(x$fill,
                x$align,
                x$sign,
                x$symbol,
                if (x$zero)  "0" else "",
                if (is.null(x$width) "" else max(1, width),
                if (x$comma) "," else "",
                if (is.null(x$precision)) ""
                else paste0(".", max(0, x$precision)),
                x$type)
}

prettify <- function(x, locale = NULL) {
  specifier <- format_specifier(x)

  prefix <- if (x$symbol == "$") {
    locale$currency[1]
  } else if (x$symbol == "#" && (x$type %in% c("b", "o", "x", "X")) {
    paste0("0", str_to_lower(x$type))
  } else {
    ""
  }
  suffix <- if (x$symbol == "$") {
    locale$currency[2]
  } else if (x$type %in% c("%", "p")) {
    "%"
  } else {
    ""
  }

  format_type <- fmt_types[type]

  maybe_suffix <- is.null(type) || type %in% c("d", "e", "f", "g", "p", "r", "s", "%")

  precision <- if (is.null(precision)) {
    if (is.null(type)) 6
    else 12
  } else if (type %in% c("g", "p", "r", "s")) {
    max(1, min(21, precision))
  } else {
    max(0, min(20, precision))
  }

}

exponent <- function(x) {
  floor(log10(abs(x))) + (x < 0)
}

precision_fixed <- function(step) {
  max(0, -exponent(abs(step)))
}

precision_prefix <- function(step, value) {
  pmax(0, pmax(-8, pmin(8, floor(exponent(value) / 3))) * 3 -
         exponent(abs(step)))
}

precision_round <- function(step, max) {
  step <- abs(step)
  max <- abs(max) - step
  max(0, exponent(max) - exponent(step)) + 1
}