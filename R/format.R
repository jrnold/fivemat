fmt_types <- list(
  "" = NULL, #formatDefault,
  "%" = function(x, p) sprintf(paste0("%f.", p), x * 100),
  "b" = function(x) as.character(R.utils::intToBin(round(x))),
  "c" = as.character,
  "d" = function(x) as.character(round(x)),
  "e" = function(x, p) sprintf(paste0("%e.", p), x),
  "f" = function(x, p) sprintf(paste0("%d.", p), x),
  "g" = function(x, p) sprintf(paste0("%g.", p), x),
  "o" = function(x) format(as.octmode(x)),
  "p" = NULL, # function(x, p) { return formatRounded(x * 100, p); },
  "r" = NULL, # formatRounded,
  "s" = NULL, # formatPrefixAuto,
  "X" = function(x) format(as.hexmode(round(x)), upper.case = TRUE),
  "x" = function(x) format(as.hexmode(round(x)), upper.case = FALSE)
)

# prettify <- function(x, locale = NULL) {
#   specifier <- format_specifier(x)
#
#   prefix <- if (x$symbol == "$") {
#     locale$currency[1]
#   } else if (x$symbol == "#" && (x$type %in% c("b", "o", "x", "X")) {
#     paste0("0", str_to_lower(x$type))
#   } else {
#     ""
#   }
#   suffix <- if (x$symbol == "$") {
#     locale$currency[2]
#   } else if (x$type %in% c("%", "p")) {
#     "%"
#   } else {
#     ""
#   }
#
#   format_type <- fmt_types[type]
#
#   maybe_suffix <- is.null(type) || type %in% c("d", "e", "f", "g", "p", "r", "s", "%")
#
#   precision <- if (is.null(precision)) {
#     if (is.null(type)) 6
#     else 12
#   } else if (type %in% c("g", "p", "r", "s")) {
#     max(1, min(21, precision))
#   } else {
#     max(0, min(20, precision))
#   }
#
# }
