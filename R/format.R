library("stringr")

re = "^(?:(.)?([<>=^]))?([+\\-\\( ])?([$#])?(0)?(\\d+)?(,)?(\\.\\d+)?([a-z%])?$"

formatt_types <- list(
  "" = NULL, #formatDefault,
  "%" = NULL, # function(x, p) { return (x * 100).toFixed(p); },
  "b" = NULL, # function(x) { return Math.round(x).toString(2); },
  "c" = NULL, # function(x) { return x + ""; },
  "d" = NULL, # function(x) { return Math.round(x).toString(10); },
  "e" = NULL, # function(x, p) { return x.toExponential(p); },
  "f" = NULL, # function(x, p) { return x.toFixed(p); },
  "g" = NULL, # function(x, p) { return x.toPrecision(p); },
  "o" = NULL, # function(x) { return Math.round(x).toString(8); },
  "p" = NULL, # function(x, p) { return formatRounded(x * 100, p); },
  "r" = NULL, # formatRounded,
  "s" = NULL, # formatPrefixAuto,
  "X" = NULL, # function(x) { return Math.round(x).toString(16).toUpperCase(); },
  "x" = NULL # function(x) { return Math.round(x).toString(16); }
)

formatt_spec <- function(x) {
  match <- str_match(x)

  res <- list(
    fill = match[1] %||% " ",
    align = match[2] %||% ">",
    sign = match[3] %||% "-",
    symbol = match[4] %||% "",
    zero = !is.na(match[5]),
    width = match[6] && +match[6],
    comma = !!match[7],
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
  structure(res, class = "format_specifier")
}

print.formatt_spec(x) {
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


}
