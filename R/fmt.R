# split x into mantissa and exponent
fmt_decimal <- function(x, p) {
  value <- NULL
  out <- tibble::tibble(
    value = x,
    exponent = exponent(value),
    mantissa = as.integer(round(value * 10 ^ (-exponent - 1 + p)))
  )
  out
}

pad_zero <- function(x, spec, locale) {
  if (spec$align == "=" && spec$fill == "0") {
    width <- spec$width
    fin <- x$not_na
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
  } else x
}

# #' @importFrom purrr map2_chr
# fmt_rounded <- function(x, p, locale) {
#   p <- min(max(1L, p), 21L)
#   out <- fmt_init_dbl(x, p, locale, fixed = FALSE)
#   value <- out$value[out$not_na]
#   k <- -exponent(value) + p - 1L
#   f <- function(i, j) sprintf(str_c("%.", j, "f"), i)
#   out$string[out$not_na] <- map2_chr(value, k, f)
#   out
# }

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

# split number into integer, decimal, and exponent part
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
Formatter <- R6Class("Formatter",
  public = list(
    initialize = function(spec = fmt_spec(), locale = fmt_locale()) {
      self$spec <- spec
      self$locale <- locale
    },
    format = function(x) {
      # be able to handle NULL values gracefully
      if (is.null(x)) return(self$format_null())
      out <- self$preprocess(x)
      out <- self$setup(out)
      out <- self$format_na(out)
      out <- self$format_nan(out)
      out <- self$format_inf(out)
      out <- self$format_values(out)
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
      if (!is.null(self$align)) {
        out <- self$pad(out)
      }
      out <- self$postprocess(out)
      out
    },
    preprocess = identity,
    setup = function(x) {
      out <- tibble(value = x,
                    string = "",
                    prefix = "",
                    postfix = "")
      if (purrr::is_integer(x)) {
        out[["is_na"]] <- is.na(x)
        out[["is_value"]] <- !out$is_na
        out[["negative"]] <- out$is_value & out$value < 0
      } else if (purrr::is_double(x)) {
        out[["is_nan"]] <- is.nan(x)
        out[["is_na"]] <- is.na(x) & !out$is_nan
        out[["is_inf"]] <- is.infinite(x)
        out[["is_value"]] <- !(out$is_nan | !out$is_na | !out$is_inf)
        out[["negative"]] <- (out$is_value | out$is_inf) & out$value < 0
      } else if (purrr::is_character(x)) {
        out[["is_na"]] <- is.na(out)
        out[["is_value"]] <- !out$is_na
      } else {
        stop("Don't know how to handle objects of class: ",
             str_c(class(x), collapse = ","))
      }
    },
    format_null = function() "",
    format_na = function(x) {
      if (!is.null(x$is_na)) {
        x[["string"]][x[["is_na"]]] <- locale$na_mark
      }
      x
    },
    format_nan = function(x) {
      if (!is.null(x$is_nan)) {
        x[["string"]][x[["is_nan"]]] <- locale$nan_mark
      }
      x
    },
    format_inf = function(x) {
      if (!is.null(x$is_inf)) {
        x[["string"]][x[["is_inf"]]] <- locale$inf_mark
      }
      x
    },
    format_value = function(x) {
      x$string[x[["is_value"]]] <- as.character(x[["is_value"]])
      x
    },
    format_pound = identity,
    format_dollar = function(x) {
      x[["prefix"]] <- str_c(self$locale[["currency"]][1], x[["prefix"]])
      x[["postfix"]] <- str_c(x[["postfix"]], self$locale[["currency"]][2])
      x
    },
    format_zero = function(x) {
      x$value[x$is_value] <- pad_zero(x$value[x$is_value], self$spec, self$locale)
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
      if (is_empty(grouping) || len(x) == 0) {
        return(x)
      }
      # split integer from digits or exponent
      # use ?= so that the splitting part is kept
      split_pattern <- "(?=\\.|[eE][+-])"
      x_split <- str_split_fixed(x$string, split_pattern, 2L)
      intvls <- rep_len(grouping, max(str_length(x_split[, 1])))
      start <- cumsum(c(1L, intvls[-length(intvls)]))
      end <- start + intvls - 1L
      f <- function(x, start, end) {
        res <- keep(str_sub(stri_reverse(x), start, end), function(s) s != "")
        res <- stri_reverse(str_c(res, collapse = sep))
        res
      }
      x$string <- str_c(map_chr(x_split[ , 1], f, start = start, end = end),
                        x_split[ , 2])
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
    inherit = FormatterDbl,
    public = list(
      preprocess = function(x) {
        round(as.numeric(x), self$spec$precision)
      }
    )
)

FormatterDblSignif <- R6Class("FormatterDblSig",
  inherit = FormatterDbl,
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

FormatterType_f <- R6Class("FormatterType_f",
  inherit = FormatterDblFixed
)

fmt_types$f <- FormatterType_f

# fmt_types[["b"]] <- function(x, spec, locale) {
#   out <- fmt_init_int(x, locale)
#   out[["string"]][out[["not_na"]]] <-
#     int2bin(abs(out[["value"]][out[["not_na"]]]))
#   out
# }
#
# fmt_type_e <- function(x, p, locale, capitalize = FALSE) {
#   p <- min(max(0L, p), 21L)
#   out <- fmt_init_dbl(x, p, locale, fixed = FALSE)
#   pat <- str_c("%", if (is.null(p)) "" else str_c(".", p),
#                if (capitalize) "E" else "e")
#   out[["string"]][out[["not_na"]]] <-
#     sprintf(pat, abs(out[["value"]][out[["not_na"]]]))
#   out
# }
#
# fmt_types[["e"]] <- function(x, spec, locale) {
#   fmt_type_e(x, spec$precision, locale, capitalize = FALSE)
# }
#
# fmt_types[["E"]] <- function(x, spec, locale) {
#   fmt_type_e(x, spec$precision, locale, capitalize = TRUE)
# }
#
# fmt_types[["d"]] <- function(x, spec, locale) {
#   out <- fmt_init_int(x, locale)
#   fin <- out[["not_na"]]
#   out[["string"]][fin] <- sprintf("%d", abs(out[["value"]][fin]))
#   out
# }
#
# fmt_types[["f"]] <- function(x, spec, locale, capitalize = FALSE) {
#   p <- min(max(0L, spec$precision), 20L)
#   out <- fmt_init_dbl(x, p, locale, fixed = TRUE)
#   pat <- str_c("%", if (is.null(p)) "" else str_c(".", p), "f")
#   fin <- out[["not_na"]]
#   out[["string"]][fin] <- sprintf(pat, abs(out[["value"]][fin]))
#   out
# }
#
# fmt_type_g <- function(x, spec, locale, capitalize = FALSE) {
#   p <- min(max(0L, spec$precision), 21L)
#   out <- fmt_init_dbl(x, spec$precision, locale, fixed = TRUE)
#   pat <- str_c("%", if (is.null(p)) "" else str_c(".", p),
#                if (capitalize) "G" else "g")
#   fin <- out[["not_na"]]
#   out[["string"]][fin] <- sprintf(pat, abs(out[["value"]][fin]))
#   out
# }
#
# fmt_types[["g"]] <- function(x, spec, locale) {
#   fmt_type_g(x, spec, locale, capitalize = FALSE)
# }
#
# fmt_types[["G"]] <- function(x, spec, locale) {
#   fmt_type_g(x, spec, locale, capitalize = TRUE)
# }
#
# fmt_types[["o"]] <- function(x, spec, locale, capitalize = FALSE) {
#   out <- fmt_init_int(x, locale)
#   fin <- out[["not_na"]]
#   out[["string"]][fin] <- sprintf("%o", abs(out[["value"]][fin]))
#   out
# }
#
# fmt_types[["p"]] <- function(x, spec, locale) {
#   out <- fmt_rounded(x, spec$precision, locale)
#   out[["postfix"]] <- str_c(spec$percent_mark, out[["postfix"]])
#   out
# }
#
# fmt_types[["r"]] <- function(x, spec, locale) {
#   fmt_rounded(x, spec$precision, locale)
# }
#
# fmt_type_x <- function(x, spec, locale, capitalize = FALSE) {
#   out <- fmt_init_int(x, locale)
#   pat <- str_c("%", if (capitalize) "x" else "X")
#   out[["string"]][out[["not_na"]]] <-
#     sprintf(pat, abs(out[["value"]][out[["not_na"]]]))
#   out
# }
#
# fmt_types[["x"]] <- function(x, spec, locale) {
#   fmt_type_x(x, spec, locale, capitalize = FALSE)
# }
#
# fmt_types[["X"]] <- function(x, spec, locale) {
#   fmt_type_x(x, spec, locale, capitalize = TRUE)
# }
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
#' @export
print.fmt <- function(x, ...) {
  cat("<fmt>\n")
  print(environment(x)$spec)
  print(environment(x)$locale)
  invisible(x)
}
# nocov end

#' @rdname fmt_new
#' @export
fmt <- function(x, spec = NULL, locale = NULL, ...) {
  fmt_new(spec = spec, locale = locale, ...)(x)
}
