globalVariables("fmt_locales")

#' @importFrom purrr is_empty
"%==%" <- function(x, y) !is_empty(x) && x %in% y

# Return base 10 exponent where mantissa * 10 ^ exponent
#' @importFrom dplyr if_else
exponent <- function(x) {
  out <- rep(NA_integer_, length(x))
  out[is.finite(x) & x == 0] <- 0L
  pos_x <- is.finite(x) & x > 0
  out[pos_x] <- as.integer(floor(log10(abs(x[pos_x]))))
  out
}

#' @importFrom stringr str_replace_all
replace_numerals <- function(x, numerals = NULL) {
  if (is.null(numerals)) return(x)
  names(numerals) <- as.character(0:9)
  str_replace_all(x, numerals)
}

#' @importFrom dplyr if_else
na_else <- function(x, default) {
  if_else(!is.na(x), x, default)
}

# make it easier to test types
test_types <- function(x, types = character(), default = FALSE) {
  (default & is_empty(x)) | (x %==% types)
}

remove_leading_zeros <- function(x) {
  # cases: \d+, \d+.\d+(e[+-]\d+), \d+e\+\d+
  str_replace(x, "^0+", "")
}

drop_trailing_zeros <- function(x) {
  # cases: \d+, \d+.\d+(e[+-]\d+), \d+e\+\d+
  # remove e+00 or e-00
  x <- str_replace(x, regex("e[+-]0+$", ignore_case = TRUE))
  # remove 9.9000 or 9.9000e+01
  # don't remove 9000
  x <- str_replace(x, regex("(\\.)(\\d*)0+($|[e])", ignore_case = TRUE),
                   "\\1\\2\\3")

}
