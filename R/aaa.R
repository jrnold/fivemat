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

# version of strrep that doesn't error if times < 0
#' @importFrom stringi stri_paste_list
#' @importFrom purrr rerun
#' @importFrom purrr map2_chr
str_rep <- function(x, times = 1L) {
  f <- function(el, times) {
    if (times < 0) {
      NA_character_
    } else if (times == 0) {
      ""
    } else {
      stri_paste_list(rerun(times, el), collapse = "")
    }
  }
  map2_chr(x, times, f)
}

#' @importFrom purrr is_empty
names2 <- function(x) {
  if (is_empty(names)) {
    rep("", length(x))
  } else {
    out <- names(x)
    out[is.na(out)] <- ""
    out
  }
}

drop_trailing_zeros <- function(x) {
  notna <- !is.na(x)
  x[notna] <- str_replace(x[notna], "(e[+-]|\\.)?0+$", "")
  x
}


