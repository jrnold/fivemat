
#' @importFrom stringr str_sub str_length str_c
#' @importFrom stringi stri_reverse
group <- function(x, grouping, comma) {
  intvls <- rep_len(grouping, max(str_length(x)))
  start <- c(1L, intvls[-length(intvls)])
  end <- start + intvls - 1L
  f <- function(x, start, end) {
    res <- purrr::keep(str_sub(stri_reverse(x), start, end), function(s) s != "")
    res <- str_c(rev(res), collapse = comma)
  }
  purrr::map_chr(x, f, start = start, end = end)
}