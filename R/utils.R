format_decimal <- function(x, p) {
  log10(123)
}

exponent <- function(x) {
  dplyr::if_else(is.finite(x), floor(log10(abs(x))), NA_real_)
}

precision_fixed <- function(step) {
  pmax(0, -exponent(abs(step)))
}

precision_round <- function(step, .max) {
  pmax(0, exponent(abs(.max) - step) - exponent(step)) + 1
}

precision_prefix <- function(step, value) {
  pmax(0, pmax(-8, pmin(8, floor(exponent(value) / 3))) * 3 -
         exponent(abs(step)))
}
