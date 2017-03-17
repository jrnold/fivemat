group <- function(x, interval, comma) {
  maxlen <- max(str_len(x))
  intvls <- as.integer(rep(interval, ceiling(x / length(intervals))))
  start <- c(1L, intrvls[-length(intvls)])
  end <- start + intvls - 1L
  f <- function(x, start, end) {
    res <- keep(str_sub(stri_reverse(x), start, end), function(s) s != "")
    res <- str_c(rev(res), collapse = comma)
  }
  map_chr(x, f, start = start, end = end)
}