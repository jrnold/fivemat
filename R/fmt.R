# split x into mantissa and exponent
fmt_decimal <- function(x, p) {
  value <- NULL
  out <- tibble(
    value = x,
    exponent = exponent(value),
    mantissa = as.integer(round(value * 10 ^ (-exponent - 1 + p)))
  )
  out
}

pad_zero <- function(x, spec, locale) {

}

#' @importFrom purrr map2_chr
fmt_rounded <- function(x, p) {
  p <- min(max(1L, p), 21L)
  k <- -exponent(x) + p - 1L
  f <- function(i, j) sprintf(str_c("%.", j, "f"), i)
  map2_chr(x, k, f)
}

# fmt_init_si <- function(x, p, locale, si_prefix = NULL) {
#   p <- min(max(0L, p), 20L)
#   if (is.null(si_prefix)) {
#     si_prefix <- exponent(x)
#   }
#   prefix <- si_prefix(si_prefix)
#   out <- fmt_init_dbl(x / 10 ^ prefix, p, locale, fixed = TRUE)
#   out[["postfix"]] <- names(si_prefix)
#   out
# }

# split number into integer, decimal, and exponent parts
#' @importFrom tibble tibble
#' @importFrom stringr str_match_all
split_number <- function(x) {
  pattern <- "([-+]?[0-9]*)?(?:\\.([0-9]*))?(?:[eE]([+-][0-9]+))?"
  xsplit <- str_match_all(x, pattern)
  tibble(
    integer = as.integer(xsplit[ , 2]),
    decimal = as.integer(xsplit[ , 3]),
    exponent = if_else(is.na(xsplit[ , 4]), 0, as.integer(xsplit[ , 4]))
  )
}

#' @importFrom stringr str_sub str_length str_c str_split_fixed
#' @importFrom stringi stri_reverse
#' @importFrom purrr keep map_chr is_empty
#' @noRd
fmt_group <- function(x, spec, locale) {
  grouping <- locale$grouping
  sep <- locale$grouping_mark
  if (is_empty(grouping)) {
    return(x)
  }
  if (is_empty(x)) {
    return(character())
  }
  # split integer from digits or exponent
  # use ?= so that the splitting part is kept
  split_pattern <- "(?=\\.|[eE][+-])"
  x_split <- str_split_fixed(x, split_pattern, 2L)
  intvls <- rep_len(grouping, max(str_length(x_split[, 1])))
  start <- cumsum(c(1L, intvls[-length(intvls)]))
  end <- start + intvls - 1L
  f <- function(x, start, end) {
    res <- keep(str_sub(stri_reverse(x), start, end), function(s) s != "")
    res <- stri_reverse(str_c(res, collapse = sep))
    res
  }
  str_c(map_chr(x_split[ , 1], f, start = start, end = end),
        x_split[ , 2])
}

# Truncate leading zeros
trunc_zeros <- function(x, width = NULL) {
  if (is.null(width)) {
    out <- str_replace(x, "^[0,]*", "")
  } else {
    out <- str_replace(x, str_c("^([0,]*)(.{", width, ",})"), "\\2")
  }
  str_replace(out, "^,", " ")
}

fmt_pad <- function(x, spec, locale) {
  if (is.null(spec$align) || (spec$fill == "0" && spec$align == "=") ||
      is.null(spec$width)) {
      return(str_c(x$prefix, x$string, x$postfix))
  }
  align <- spec$align
  fill <- spec$fill
  width <- spec$width
  # null width uses longest string
  # if (is.null(width)) {
  #   lens <- rowSums(cbind(str_length(x$string),
  #                         str_length(x$prefix),
  #                         str_length(x$postfix)))
  #   width <- max(lens)
  # }
  if (align == "=") {
    str_c(x$prefix,
          str_pad(str_c(x$string, x$postfix),
                        width = width - str_length(x$prefix),
                        pad = fill))
  } else {
    side <- switch(align,
                   "<" = "right",
                   "^" = "both",
                   ">" = "left")
    str_pad(str_c(x$prefix, x$string, x$postfix), width = width,
            side = side, pad = fill)
  }
}

#' @importFrom R6 R6Class
#' @importFrom tibble tibble
Formatter <- R6Class("Formatter",
  public = list(
    spec = NULL,
    locale = NULL,
    initialize = function(spec = fmt_spec(), locale = fmt_locale()) {
      self$spec <- spec
      self$locale <- locale
    },
    render = function(x) {
      # be able to handle NULL values gracefully
      if (is.null(x)) {
        return(self$format_null())
      }
      out <- self$preprocess(x)
      out <- self$setup(out)
      if (any(out$is_na)) {
        out$string[out$is_na] <- self$format_na()
      }
      if (!is.null(out[["is_nan"]])) {
        out$string[out$is_nan] <- self$format_nan()
      }
      if (!is.null(out[["is_inf"]])) {
        out$string[out$is_inf] <- self$format_inf()
      }
      if (any(out$is_value)) {
        out$string[out$is_value] <- self$format_values(out$value[out$is_value])
      }
      if ("#" %in% self$spec$symbol) {
        out <- self$format_pound(out)
      }
      if ("$" %in% self$spec$symbol) {
        out <- self$format_dollar(out)
      }
      if (self$spec$zero) {
        out <- self$format_zero(out)
      }
      if (self$spec$comma) {
        out <- self$group(out)
      }
      out <- self$pad(out)
      out <- self$postprocess(out)
      out
    },
    preprocess = identity,
    setup = function(x) {
      n <- length(x)
      out <- tibble(string = rep("", n),
                    prefix = rep("", n),
                    postfix = rep("", n))
      if (purrr::is_integer(x)) {
        out[["is_na"]] <- is.na(x)
        out[["is_value"]] <- !out$is_na
        out[["negative"]] <- out$is_value & x < 0
        out[["value"]] <- abs(x)
      } else if (purrr::is_double(x)) {
        out[["is_nan"]] <- is.nan(x)
        out[["is_na"]] <- is.na(x) & !out$is_nan
        out[["is_inf"]] <- is.infinite(x)
        out[["is_value"]] <- !(out$is_nan | out$is_na | out$is_inf)
        out[["negative"]] <- (out$is_value | out$is_inf) & x < 0
        out[["value"]] <- abs(x)
      } else if (purrr::is_character(x)) {
        out[["is_na"]] <- is.na(out)
        out[["is_value"]] <- !out$is_na
        out[["value"]] <- x
      } else {
        stop("Don't know how to handle objects of class: ",
             str_c(class(x), collapse = ","))
      }
      out
    },
    format_null = function() "",
    format_na = function() self$locale$na_mark,
    format_nan = function() self$locale$nan_mark,
    format_inf = function() self$locale$inf_mark,
    format_values = function(x) as.character(x),
    format_pound = identity,
    format_dollar = function(x) {
      x[["prefix"]] <- str_c(self$locale[["currency"]][1], x[["prefix"]])
      x[["postfix"]] <- str_c(x[["postfix"]], self$locale[["currency"]][2])
      x
    },
    format_zero = function(x) {
      width <- self$spec$width
      fin <- x$is_value
      if (is.null(width)) {
        lens <- rowSums(cbind(str_length(x$string[fin]),
                              str_length(x$prefix[fin]),
                              str_length(x$postfix[fin])))
        width <- max(lens)
      } else {
        width <- width - str_length(x$prefix[fin]) - str_length(x$postfix[fin])
      }
      x$string[fin] <- str_pad(x$string[fin], width = width, side = "left",
                               pad = "0")
      x
    },
    format_minus = function(x) {
      if (is.null(x$negative)) return(x)
      minus <- self$spec$sign
      if (minus == "-") {
        x[["prefix"]] <- str_c(if_else(x[["negative"]], self$locale$minus, ""),
                               x[["prefix"]])
      } else if (minus == "+") {
        x[["prefix"]] <- str_c(if_else(x[["negative"]], self$locale$minus,
                                       self$locale$plus), x[["prefix"]])
      } else if (minus == " ") {
        x[["prefix"]] <- str_c(if_else(x[["negative"]],
                                       self$locale$minus, " "), x[["prefix"]])
      } else if (minus == "(") {
        x[["prefix"]] <- str_c(if_else(x[["negative"]],
                                       self$locale$left_paren, ""),
                               x[["prefix"]])
        x[["postfix"]] <- str_c(x[["postfix"]],
                                if_else(x[["negative"]],
                                        self$locale$right_paren, ""))
      }
      x
    },
    group = function(x) {
      grouping <- self$locale$grouping
      sep <- self$locale$grouping_mark
      if (is_empty(grouping) || nrow(x) == 0) {
        return(x)
      }
      # split integer from digits or exponent
      # use ?= so that the splitting part is kept
      split_pattern <- "(?=\\.|[eE][+-])"
      x_split <- str_split_fixed(x$string[x$is_value], split_pattern, 2L)
      intvls <- rep_len(grouping, max(str_length(x_split[, 1])))
      start <- cumsum(c(1L, intvls[-length(intvls)]))
      end <- start + intvls - 1L
      f <- function(x, start, end) {
        res <- keep(str_sub(stri_reverse(x), start, end), function(s) s != "")
        res <- stri_reverse(str_c(res, collapse = sep))
        res
      }
      x$string[x$is_value] <-
        str_c(map_chr(x_split[ , 1], f, start = start, end = end), x_split[ , 2])
      x
    },
    pad = function(x) {
      align <- self$spec$align
      fill <- self$spec$fill
      width <- self$spec$width
      if (is.null(align) || is.null(width)) {
        return(str_c(x$prefix, x$string, x$postfix))
      }
      # null width uses longest string
      # if (is.null(width)) {
      #   lens <- rowSums(cbind(str_length(x$string),
      #                         str_length(x$prefix),
      #                         str_length(x$postfix)))
      #   width <- max(lens)
      # }
      if (align == "=") {
        str_c(x$prefix,
              str_pad(str_c(x$string, x$postfix),
                      width = width - str_length(x$prefix),
                      pad = fill))
      } else {
        side <- switch(align,
                       "<" = "right",
                       "^" = "both",
                       ">" = "left")
        str_pad(str_c(x$prefix, x$string, x$postfix), width = width,
                side = side, pad = fill)
      }
    },
    postprocess = identity
  )
)


FormatterDblFixed <- R6Class("FormatterDblFixed",
    inherit = Formatter,
    public = list(
      preprocess = function(x) {
        round(as.numeric(x), self$spec$precision)
      }
    )
)

FormatterDblSignif <- R6Class("FormatterDblSig",
  inherit = Formatter,
  public = list(
   preprocess = function(x) {
     signif(as.numeric(x), self$spec$precision)
   }
  )
)

FormatterInt <- R6Class("FormatterChr",
  inherit = Formatter,
  public = list(
    preprocess = as.integer
  )
)

FormatterChr <- R6Class("FormatterChr",
  inherit = Formatter,
  public = list(
    preprocess = as.character
  )
)

#' Format Types
fmt_types <- new.env()

FormatterType_a <-
  R6Class("FormatterType_a",
          inherit = FormatterDblSignif,
          public = list(
            format_pound = function(x) {
              x$prefix[x$is_value] <- "0x"
              x
            },
            format_values = function(x) sprintf("%a", x)
          ))

fmt_types$a <- FormatterType_a

FormatterType_A <-
  R6Class("FormatterType_A",
          inherit = FormatterDblSignif,
          public = list(
            format_pound = function(x) {
              x$prefix[x$is_value] <- "0x"
              x
            },
            format_values = function(x) sprintf("%A", x)
          ))

fmt_types$A <- FormatterType_A

FormatterType_b <- R6Class("FormatterType_b",
  inherit = FormatterInt,
  public = list(
    format_values = function(x) int2bin(x),
    format_pound = function(x) {
      x$prefix[x$is_value] <- "0b"
      x
    }
  ))


fmt_types$b <- FormatterType_b

FormatterType_u <-
  R6Class("FormatterType_u",
          inherit = FormatterInt,
          public = list(
            format_na = function() "",
            format_values = function(x) intToUtf8(x, multiple = TRUE)
          ))

fmt_types$u <- FormatterType_u

FormatterType_c <-
  R6Class("FormatterType_c",
          inherit = FormatterChr,
          public = list(
            format_values = as.character
          ))

fmt_types$c <- FormatterType_c

FormatterType_d <- R6Class("FormatterType_d",
  inherit = FormatterInt,
  public = list(
    format_values = function(x) {
      sprintf("%d", x)
    }
  )
)

fmt_types$d <- FormatterType_d

FormatterType_e <- R6Class("FormatterType_e",
  inherit = FormatterDblSignif,
  public = list(
    format_values = function(x) {
      p <- min(max(0L, self$spec$precision), 21L)
      sprintf(str_c("%", if (is.null(p)) "" else str_c(".", p), "e"), x)
    }
  )
)
fmt_types$e <- FormatterType_e

FormatterType_E <- R6Class("FormatterType_E",
  inherit = FormatterDblSignif,
  public = list(
   format_values = function(x) {
     p <- min(max(0L, self$spec$precision), 21L)
     sprintf(str_c("%", if (is.null(p)) "" else str_c(".", p), "E"), x)
   }
  )
)
fmt_types$E <- FormatterType_E

FormatterType_f <- R6Class("FormatterType_f",
  inherit = FormatterDblFixed,
  public = list(
   format_values = function(x) {
     p <- min(max(0L, self$spec$precision), 20L)
     pattern <- str_c("%", if (is.null(p)) "" else str_c(".", p), "f")
     sprintf(pattern, abs(x))
   }
  )
)

fmt_types$f <- FormatterType_f

FormatterType_g <-
  R6Class("FormatterType_g",
    inherit = FormatterDblSignif,
    public = list(
     format_values = function(x) {
       p <- min(max(0L, self$spec$precision), 21L)
       pattern <- str_c("%", if (is.null(p)) "" else str_c(".", p), "g")
       sprintf(pattern, abs(x))
      }
    )
  )

fmt_types$g <- FormatterType_g

FormatterType_G <-
  R6Class("FormatterType_G",
          inherit = FormatterDblSignif,
          public = list(
            format_values = function(x) {
              p <- min(max(0L, self$spec$precision), 21L)
              pattern <- str_c("%", if (is.null(p)) "" else str_c(".", p), "G")
              sprintf(pattern, abs(x))
            }
          )
  )

fmt_types$G <- FormatterType_G

FormatterType_o <-
  R6Class("FormatterType_o",
          inherit = FormatterInt,
          public = list(
            format_values = function(x) {
              sprintf("%o", x)
            },
            format_pound = function(x) {
              x$prefix[x$is_value] <- "0o"
              x
            }
          ))

fmt_types$o <- FormatterType_o

FormatterType_p <-
  R6Class("FormatterType_p",
          inherit = FormatterType_r,
          public = list(
            setup = function(x) {
              out <- super$setup(x)
              out$postfix[out$is_value] <- self$locale$percent_mark
              out
            },
            preprocess = function(x) {
              signif(as.numeric(x) * 100, self$spec$precision)
            }
          ))

fmt_types$p <- FormatterType_p

FormatterType_P <-
  R6Class("FormatterType_P",
          inherit = FormatterType_f,
          public = list(
            setup = function(x) {
              out <- super$setup(x)
              out$postfix[out$is_value] <- self$locale$percent_mark
              out
            },
            preprocess = function(x) {
              super$preprocess(as.numeric(x) * 100)
            }
          ))

fmt_types$P <- FormatterType_P

FormatterType_r <-
  R6Class("FormatterType_r",
        inherit = FormatterDblSignif,
        public = list(
          format_values = function(x) {
            fmt_rounded(x, self$spec$precision)
          }
        ))

fmt_types$r <- FormatterType_r

FormatterType_x <-
  R6Class("FormatterType_x",
          inherit = FormatterInt,
          public = list(
            format_values = function(x) {
              sprintf("%x", x)
            },
            format_pound = function(x) {
              x$prefix[x$is_value] <- "0x"
              x
            }
          ))

fmt_types$x <- FormatterType_x

FormatterType_X <-
  R6Class("FormatterType_X",
          inherit = FormatterInt,
          public = list(
            format_values = function(x) {
              sprintf("%X", x)
            },
            format_pound = function(x) {
              x$prefix[x$is_value] <- "0x"
              x
            }
          ))

fmt_types$X <- FormatterType_X

#
# fmt_types[["%"]] <- function(x, spec, locale, capitalize = FALSE) {
#   out <- fmt_types[["f"]](x * 100)
#   out[["postfix"]] <- str_c(spec$percent_mark, out[["postfix"]])
#   out
# }



#' Format numbers
#'
#' The function \code{fmt_new} creates a function to format numbers using
#' a given \code{locale} and \code{spec}. The function \code{fmt} formats
#' numbers, and is a convenience function for \code{fmt_new(...)(x)}.
#'
#'
#' @param x A numeric or integer vector
#' @param si_prefix If non-\code{NULL}, then use
#'   an \href{https://en.wikipedia.org/wiki/Metric_prefix#List_of_SI_prefixes}{SI prefix} to format \code{x}.
#'   If \code{TRUE}, then use \code{precision_prefix} to automatically determine
#'   the SI prefix to use. Otherwise, it can be any of the valid arguments
#'   for \code{\link{si_prefix}}: a string with the prefix name, an integer with the
#'   SI prefix exponent, or a numeric value.
#'   Unlike the \code{"s"} format type, this applies the same
#'   prefix to all values in \code{x}, rather than determining the SI-prefix
#'   on a value by value basis. Additionally, a non-\code{NULL} \code{si} implies
#'   a \code{"f"} format type, in which \code{precision} represents the number of
#'   digits past the decimal point.
#' @param spec A \code{\link{fmt_spec}} object, or a string coerced to a
#'    \code{fmt_spec} object using \code{\link{as_fmt_spec}}, or a list of
#'    arguments passed to \code{fmt_spec}.
#' @param locale A \code{\link{fmt_locale}} object.
#' @param ... Arguments passed to \code{fmt_new}.
#' @return \code{fmt_new} returns a function with a single argument.
#'    \code{fmt} returns a function of the same length as \code{x} of
#'    formatted numbers.
#'
#' @export
#' @importFrom dplyr if_else
#' @importFrom purrr invoke %||% is_empty
#' @importFrom stringr str_c str_pad str_match str_replace str_replace_all
#' @importFrom stringr str_trim
#' @examples
#' fmt(c(0.00042, 0.0042), spec = ",.0", si_prefix = 1e-6)
fmt_new <- function(spec = NULL, locale = NULL, si_prefix = NULL) {
  locale <- locale %||% fmt_default_locale()
  spec <- spec %||% fmt_spec()
  if (!inherits(spec, "fmt_spec")) {
    if (is.character(spec)) {
      assert_that(is.string(spec))
      spec <- as_fmt_spec(spec)
    } else if (is.list(spec)) {
      spec <- invoke(fmt_spec, spec)
    } else {
      stop("Class ", str_c(class(spec), collapse = ", "),
           " for `spec` is not supported.",
           call. = FALSE)
    }
  }
  if (!inherits(locale, "fmt_locale")) {
    if (is.character(locale)) {
      if (length(locale) != 1) {
        stop("If character, `locale` must have a length of one.",
             call. = FALSE)
      }
      # en_US -> en-US
      locale <- str_replace(locale, "_", "-")
      locale <- fmt_locales[[locale]]
      if (is.null(locale)) {
        stop(str_c("Locale `", locale, "` not found in `fmt_locales`.\n",
             "Available locales: ", str_c(names(fmt_locales), collapse = ", ")),
             call. = FALSE)
      }
    } else {
      stop("Class ", str_c(class(locale), collapse = ", "),
           " for `locale` is not supported.",
           call. = FALSE)
    }
  }
  fmt_types[[spec$type]]$new(spec, locale)
}

# nocov start
# print.fmt <- function(x, ...) {
#   cat("<fmt>\n")
#   print(environment(x)$spec)
#   print(environment(x)$locale)
#   invisible(x)
# }
# # nocov end

#' @rdname fmt_new
#' @export
fmt <- function(x, spec = NULL, locale = NULL, ...) {
  fmt_new(spec = spec, locale = locale, ...)$render(x)
}
