# split number into integer, decimal, and exponent parts
#' @importFrom tibble tibble as_tibble
#' @importFrom stringr str_match_all
#' @importFrom dplyr mutate_all funs
#'
parse_number <- function(x, fill_na = TRUE) {
  pattern <- "^([-+])?([0-9]+)?(?:(\\.)([0-9]*))?(?:([eE])([+-])([0-9]+))?$"
  out <- str_match(x, pattern)[ , -1L, drop = FALSE]
  colnames(out) <- c("sign", "integer",
                     "decimal_point", "decimal",
                     "exponent_mark", "exponent_sign", "exponent")
  out <- as_tibble(out)
  if (fill_na) {
    for (i in names(out)) {
      out[[i]] <- if_else(is.na(out[[i]]), "", out[[i]])
    }
  }
  out
}

#' @importFrom purrr map_chr keep
commas <- function(x, grouping, sep = ",") {
  intvls <- rep_len(grouping, max(str_length(x), na.rm = TRUE))
  start <- cumsum(c(1L, intvls[-length(intvls)]))
  end <- start + intvls - 1L
  f <- function(x, start, end) {
    if (is.na(x)) {
      ""
    } else {
      res <- keep(str_sub(stri_reverse(x), start, end), function(s) s != "")
      stri_reverse(str_c(res, collapse = sep))
    }
  }
  map_chr(x, f, start = start, end = end)
}

# split x into mantissa and exponent
mantissa <- function(x, p) {
  value <- NULL
  out <- tibble(
    value = x,
    exponent = exponent(value),
    mantissa = as.integer(round(value * 10 ^ (-exponent - 1 + p)))
  )
  out
}

#' @importFrom purrr map2_chr
fmt_rounded <- function(x, p) {
  p <- min(max(1L, p), 21L)
  k <- -exponent(x) + p - 1L
  f <- function(i, j) sprintf(str_c("%.", j, "f"), i)
  map2_chr(x, k, f)
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
#' @importFrom stringi stri_reverse
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
      out <- self$format_minus(out)
      if (self$spec$zero) {
        out <- self$format_zero(out)
      }
      out <- self$group(out)
      out <- self$pad(out)
      out <- self$postprocess(out)
      out
    },
    preprocess = identity,
    setup = function(x) {
      n <- length(x)
      out <- tibble(string = rep("", n),
                    prefix = rep("", n),
                    postfix = rep("", n),
                    value = x)
      if (purrr::is_integer(x)) {
        out[["is_na"]] <- is.na(x)
        out[["is_value"]] <- !out$is_na
      } else if (purrr::is_double(x)) {
        out[["is_nan"]] <- is.nan(x)
        out[["is_na"]] <- is.na(x) & !out$is_nan
        out[["is_inf"]] <- is.infinite(x)
        out[["is_value"]] <- !(out$is_nan | out$is_na | out$is_inf)
      } else if (purrr::is_character(x)) {
        out[["is_na"]] <- is.na(x)
        out[["is_value"]] <- !out$is_na
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
      # only prefix and postfix currency if
      x[["prefix"]] <- str_c(if_else(x[["is_value"]],
                                     self$locale[["currency"]][1], ""),
                             x[["prefix"]])
      x[["postfix"]] <- str_c(x[["postfix"]],
                              if_else(x[["is_value"]],
                                      self$locale[["currency"]][2], ""))
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
      if (!is.numeric(x$value)) {
        return(x)
      } else if (is.integer(x$value)) {
        negative <- x[["is_value"]] & x[["value"]] < 0
      } else {
        negative <- (x[["is_value"]] & x[["value"]] < 0) |
          (x[["is_inf"]] & x[["value"]] < 0)
      }
      minus <- self$spec$sign
      if (minus == "-") {
        x[["prefix"]] <- str_c(if_else(negative, self$locale$minus_mark, ""),
                               x[["prefix"]])
      } else if (minus == "+") {
        x[["prefix"]] <- str_c(if_else(negative, self$locale$minus_mark,
                                       self$locale$plus_mark), x[["prefix"]])
      } else if (minus == " ") {
        x[["prefix"]] <- str_c(if_else(negative, self$locale$minus_mark, " "),
                               x[["prefix"]])

      } else if (minus == "(") {
        x[["prefix"]] <- str_c(if_else(negative, self$locale$left_paren, ""),
                               x[["prefix"]])
        x[["postfix"]] <- str_c(x[["postfix"]],
                                if_else(negative, self$locale$right_paren, ""))
      }
      x
    },
    group = function(x) {
      # group all non-na values
      # double number formats will overwrite this to only group the integer parts
      grouping <- self$locale$grouping
      if (self$spec$comma & !is_empty(grouping)) {
        isval <- x$is_value
        x$string[isval] <- commas(x$string[isval], grouping,
                                  self$locale$grouping_mark)
      }
      x
    },
    pad = function(x) {
      align <- self$spec$align
      fill <- self$spec$fill
      width <- self$spec$width
      if (is.null(align)) {
        return(str_c(x$prefix, x$string, x$postfix))
      }
      # null width uses longest string
      if (is.null(width)) {
        lens <- rowSums(cbind(str_length(x$string),
                              str_length(x$prefix),
                              str_length(x$postfix)))
        width <- max(lens)
      }
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


#' @importFrom purrr invoke_rows
FormatterDblSignif <- R6Class("FormatterDblSignif",
  inherit = Formatter,
  public = list(
    preprocess = function(x) {
      signif(as.numeric(x), self$spec$precision)
    },
    group = function(x) {
      # split integer from digits or exponent
      # use ?= so that the splitting part is kept
      nums <- x$is_value
      parsed <- parse_number(x$string[nums])
      # replace 0-9 with locale numerals
      if (!is.null(self$locale$numerals)) {
        parsed$integer <- str_replace_all(parsed$integer, self$locale$numerals)
        parsed$decimal <- str_replace_all(parsed$exponent, self$locale$numerals)
        parsed$exponent <- str_replace_all(parsed$exponent,
                                           self$locale$numerals)
      }
      # replace decimal mark with locale
      if (self$locale$decimal_mark != ".") {
        parsed$decimal_point <- str_replace(parsed$decimal_point, "[.]",
                                           self$locale$decimal_mark)
      }
      # group integer part if comma is specified
      grouping <- self$locale$grouping
      if (self$spec$comma & !is_empty(grouping)) {
        parsed$intger <- commas(parsed$integer, grouping,
                                self$locale$grouping_mark)
      }
      x$string[nums] <- invoke_rows(str_c, parsed, .collate = "rows")[[".out"]]
      x
    }
  )
)

FormatterDblFixed <- R6Class("FormatterDblFixed",
   inherit = FormatterDblSignif,
   public = list(
     preprocess = function(x) {
       round(as.numeric(x), self$spec$precision)
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
            format_values = function(x) {
              p <- self$spec$precision
              str_replace(sprintf(str_c("%#", if (is.null(p)) ""
                                        else str_c(".", p), "a"), abs(x)),
                          "^0[Xx]", "")
            },
            group = function(x) {
              # group all non-na values
              # double number formats will overwrite this to only group the integer parts
              grouping <- self$locale$grouping
              if (self$spec$comma & !is_empty(grouping)) {
                isval <- x$is_value
                x$string[isval] <- commas(x$string[isval], grouping,
                                          self$locale$grouping_mark)
              }
              x
            }
          ))

fmt_types$a <- FormatterType_a

FormatterType_A <-
  R6Class("FormatterType_A",
          inherit = FormatterType_a,
          public = list(
            format_pound = function(x) {
              x$prefix[x$is_value] <- "0X"
              x
            },
            format_values = function(x) {
              p <- self$spec$precision
              str_replace(sprintf(str_c("%#", if (is.null(p)) ""
                                        else str_c(".", p), "A"), abs(x)),
                          "^0[Xx]", "")
            }
          ))

fmt_types$A <- FormatterType_A

FormatterType_b <- R6Class("FormatterType_b",
  inherit = FormatterInt,
  public = list(
    format_values = function(x) int2bin(abs(x)),
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
            format_values = function(x) intToUtf8(abs(x), multiple = TRUE)
          ))

fmt_types$u <- FormatterType_u

FormatterType_c <-
  R6Class("FormatterType_c",
          inherit = FormatterChr,
          public = list(
            format_values = as.character,
            group = identity
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
      sprintf(str_c("%", if (is.null(p)) "" else str_c(".", p), "e"), abs(x))
    }
  )
)
fmt_types$e <- FormatterType_e

FormatterType_E <- R6Class("FormatterType_E",
  inherit = FormatterDblSignif,
  public = list(
   format_values = function(x) {
     p <- min(max(0L, self$spec$precision), 21L)
     sprintf(str_c("%", if (is.null(p)) "" else str_c(".", p), "E"), abs(x))
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

FormatterType_s <- R6Class("FormatterType_s",
   inherit = Formatter,
   public = list(
     preprocess = as.numeric,
     setup = function(x) {
       out = super$setup(x)
       p <- min(max(0L, self$spec$precision), 20L)
       # singe SI prefix this alters both string and postfix, it needs to
       # work on both
       if (any(out[["is_value"]])) {
         vals <- out[["value"]][out[["is_value"]]]
         if (is.null(self$spec$subtype)) {
           si_prefix <- exponent(x)
         } else {
           si_prefix <- self$spec$substype
         }
         vals <- signif(vals / 10 ^ si_prefix, p)
         out$prefix[out$is_value] <-
           c(out$postfix, names(si_prefix))
         out$string[out$is_value] <-
           fmt_rounded(abs(vals), self$spec$precision)
       }
       out
     },
     format_values = identity
   ))



fmt_types$s <- FormatterType_s

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
              sprintf("%o", abs(x))
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
              super$preprocess(as.numeric(abs(x)) * 100)
            }
          ))

fmt_types$P <- FormatterType_P

FormatterType_r <-
  R6Class("FormatterType_r",
        inherit = FormatterDblSignif,
        public = list(
          format_values = function(x) {
            fmt_rounded(abs(x), self$spec$precision)
          }
        ))

fmt_types$r <- FormatterType_r

FormatterType_x <-
  R6Class("FormatterType_x",
          inherit = FormatterInt,
          public = list(
            format_values = function(x) {
              sprintf("%x", abs(x))
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
              sprintf("%X", abs(x))
            },
            format_pound = function(x) {
              x$prefix[x$is_value] <- "0x"
              x
            }
          ))

fmt_types$X <- FormatterType_X


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
