globalVariables("fmt_locales")

"%==%" <- function(x, y) !is_empty(x) && x %in% y

# Return base 10 exponent where mantissa * 10 ^ exponent
exponent <- function(x) {
  dplyr::if_else(is.finite(x), floor(log10(abs(x))), NA_real_)
}

replace_numerals <- function(x, numerals = NULL) {
  if (is.null(numerals)) return(x)
  names(numerals) <- as.character(0:9)
  stringr::str_replace_all(x, numerals)
}

na_else <- function(x, default) {
  dplyr::if_else(!is.na(x), x, default)
}
