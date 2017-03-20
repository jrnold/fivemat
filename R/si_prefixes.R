#' SI Prefixes
#'
#' @format A named numeric vector of SI prefixes.
#' The names are the SI prefixes, the values are the exponents,
#' \eqn{-24, -21, \dots, 0, \dots, 21, 24}{-24, -21, ..., 0, ..., 21, 24}.
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
#' \item{\code{" "} - (none) \eqn{10^0}}
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
#' @source \href{https://en.wikipedia.org/wiki/Metric_prefix\#List_of_SI_prefixes}{Table of SI prefixes}
#' @export
#' @importFrom purrr set_names
#' @examples
#' SI_PREFIXES
SI_PREFIXES <-
  set_names(
    seq(-24, 24, 3),
    c("y", "z", "a", "f", "p", "n", "\u03BC", "m", " ", "k", "M", "G",
      "T", "P", "E", "Z", "Y")
  )

#' Lookup SI prefix
#'
#' Lookup an SI prefix by name, by exponent (power of 10), or by value.
#' For exponents or values, the best SI prefix is returned.
#'
#' @param x A vector
#' @return A named character vector where values are the SI prefix exponents
#'   and names are the SI prefix names, from \code{\link{SI_PREFIXES}}.
#' @export
#' @examples
#' # lookup by name
#' si_prefix(c("K", "T", "mu", "\u03BC", "", NA))
#' # lookup by exponent with integers
#' si_prefix(c(-2L, -1L, 0L, 5L))
#' # lookup by value with numeric vectors
#' si_prefix(c(-1100, 5, 1.5e8))
si_prefix <- function(x) {
  UseMethod("si_prefix")
}

#' @describeIn si_prefix \code{x} are the names of the prefixes. The prefix
#'   mu can be referred to with its unicode value, \code{"\u03BC"}, or \code{"mu"}.
#'   Empty or missing values map to no strings. Invalid values return \code{NA}.
#' @export
si_prefix.character <- function(x) {
  x[is.na(x) | x == ""] <- " "
  # allow mu to be referred to by name
  x[x == "mu"] <- "\u03BC"
  SI_PREFIXES[x]
}

#' @describeIn si_prefix \code{x} are the exponents of the SI prefixes,
#'   \eqn{-24, -21, ..., -3, 0, 3, ..., 21, 24}.
#' @export
si_prefix.integer <- function(x) {
  k <- x %/% 3L
  k[k > 8L] <- 8L
  k[k < -8L] <- -8L
  SI_PREFIXES[8L + k + 1L]
}

#' @describeIn si_prefix \code{x} are numeric values.
#' @export
si_prefix.numeric <- function(x) {
  si_prefix.integer(as.integer(exponent(abs(x))))
}
