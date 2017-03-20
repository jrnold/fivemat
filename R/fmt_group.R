#' @importFrom stringr str_sub str_length str_c
#' @importFrom stringi stri_reverse
#' @importFrom purrr keep map_chr is_empty
#' @noRd
fmt_group <- function(x, grouping = NULL, sep = ",") {
  if (is_empty(grouping)) return(x)
  if (is_empty(x)) return(character())
  intvls <- rep_len(grouping, max(str_length(x)))
  start <- cumsum(c(1L, intvls[-length(intvls)]))
  end <- start + intvls - 1L
  f <- function(x, start, end) {
    res <- keep(str_sub(stri_reverse(x), start, end), function(s) s != "")
    res <- stri_reverse(str_c(res, collapse = sep))
    res
  }
  map_chr(x, f, start = start, end = end)
}
