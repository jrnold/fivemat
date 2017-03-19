#' @importFrom stringr str_sub str_length str_c
#' @importFrom stringi stri_reverse
#' @importFrom purrr keep map_chr is_empty
#' @noRd
fmt_group <- function(x, grouping = NULL, sep = ",", width = Inf) {
  if (is_empty(grouping)) return(x)
  if (is_empty(x)) return(character())
  intvls <- rep_len(grouping, max(str_length(x)))
  start <- cumsum(c(1L, intvls[-length(intvls)]))
  end <- start + intvls - 1L
  f <- function(x, start, end) {
    if (x %in% c("Inf", "NA", "NaN", "")) return(x)
    res <- keep(str_sub(stri_reverse(x), start, end), function(s) s != "")
    res <- stri_reverse(str_c(res, collapse = sep))
    if (width < Inf) {
      res <- str_sub(res, 1, width)
    }
    res
  }
  map_chr(x, f, start = start, end = end)
}
