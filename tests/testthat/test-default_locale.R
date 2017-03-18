# en_Us <- fmt_locale(
#   decimal = ".",
#   thousands = ",",
#   grouping = c(3),
#   currency = c("$", "")
# )
#
# fr_Fr = fmt_locale(
#   decimal = ",",
#   thousands =  ".",
#   grouping = c(3),
#   currency = c("", "\u00a0€")
# )
#
# test_that("fmt_default(definition) returns the new default locale", {
#   locale <- fmt_default(frFr)
#   try {
#     test.equal(locale.format("$,.2f")(12345678.90), "12.345.678,90 €")
#     test.end()
#   } finally {
#     fmt_default(enUs)
#   }
# })
#
# test_that("fmt_default(definition) affects d3.format", {
#   locale <- fmt_default(frFr)
#   try {
#     test.equal(d3.format, locale.format)
#     test.equal(d3.format("$,.2f")(12345678.90), "12.345.678,90 €")
#     test.end()
#   } finally {
#     fmt_default(enUs)
#   }
# })
#
# test_that("fmt_default(definition) affects d3.formatPrefix", {
#   locale <- fmt_default(frFr)
#   try {
#     test.equal(d3.formatPrefix, locale.formatPrefix)
#     test.equal(d3.formatPrefix(",.2", 1e3)(12345678.90), "12.345,68k")
#     test.end()
#   } finally {
#     fmt_default(enUs)
#   }
# })
