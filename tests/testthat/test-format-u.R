context("format u")

test_that("fmt_new(\"u\") unicode character", {
  expect_equal(fmt_new("u")(0x2603), "\u2603")
  expect_equal(fmt_new("020u")(0x2603),  "0000000000000000000\u2603")
  expect_equal(fmt_new(" ^20u")(0x2603), "         \u2603          ")
  expect_equal(fmt_new("$u")(0x2603), "$\u2603")
})

test_that("fmt_new(\"u\") does not localize a decimal point", {
  expect_equal(fmt(46, spec = "u", locale = fmt_locale(decimal_mark = "/")),
               ".")
})

test_that("fmt_new(\"u\") works with special values", {
  expect_equal(fmt(c(-Inf, Inf, NA, NaN), spec = "u"),
                   c("-Inf", "Inf", "NA", "NaN"))
})
