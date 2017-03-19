context("format c")

test_that("format(\"c\") unicode character", {
  expect_equal(fmt_new("c")("☃"), "☃")
  expect_equal(fmt_new("020c")("☃"),  "0000000000000000000☃")
  expect_equal(fmt_new(" ^20c")("☃"), "         ☃          ")
  expect_equal(fmt_new("$c")("☃"), "$☃")
})

test_that("format(\"c\") does not localize a decimal point", {
  expect_equal(fmt(".", spec = "c", locale = fmt_locale(decimal_mark = "/")),
               ".")
})
