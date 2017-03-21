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

# since precision is just rounding, which is just binning
# calculate precision with histogram methods
# auto_precision <- function(x) {
#   x <- x[!is.na(x)]
#   floor(log10(diff(range(x)) / nclass.FD(x)))
# }

# drop_trailing_zeros <- function(x) {
#   notna <- !is.na(x)
#   x[notna] <- str_replace(x[notna], "(e[+-]|\\.)?0+$", "")
#   x
# }

# make it easier to test types
test_types <- function(x, types = character(), default = FALSE) {
  (default & is_empty(x)) | (x %==% types)
}
