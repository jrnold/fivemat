context("fmt_new")

test_that("fmt returns a function", {
  expect_is(fmt_new("d"), "function")
  expect_is(fmt_new("d"), "fmt")
})

test_that("fmt_new(spec)(number) returns a string", {
  expect_is(fmt_new("d")(0), "character")
})

test_that("fmt_new(spec)(x) works with a list", {
  expect_equal(fmt_new(list(type = "f", precision = 2))(c(1.234, 123)),
               c("1.23", "123.00"))
})

test_that("fmt_new(spec)(x) throws error with bad spec", {
  expect_error(fmt_new(123), regexp = "Class .* for `spec` is not supported")
})

test_that("fmt_new(spec) throws an error for invalid formats", {
  expect_error(fmt_new("foo"), regexp = "is an invalid format")
  expect_error(fmt_new(".-2s"), regexp = "is an invalid format")
  expect_error(fmt_new(".f"), regexp = "is an invalid format")
})

test_that(paste("fmt_new(\",.\") unreasonable precision values are clamped ",
                "to reasonable values"), {
  expect_equal(fmt_new(".30f")(0), "0.00000000000000000000")
  expect_equal(fmt_new(".0g")(1), "1")
})

#test_that("fmt_new(\"s\") handles very small and very large values", {
  #small <- "0.00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002y" # nolint
  #expect_equal(fmt_new("s")(2.2250738585072014e-308), small)
  #big <- "179769000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000Y" # nolint
  #expect_equal(fmt_new("s")(1.7976931348623157e+308), big)
#})

test_that("fmt_new() raises error with bad locales", {
  expect_error(fmt_new(locale = c("en-US", "en-GB")),
               regexp = "If character, `locale` must have a length of one")
  expect_error(fmt_new(locale = c("foo")),
               regexp = "not found in `fmt_locales`")
  expect_error(fmt_new(locale = 1L),
               regexp = "or `locale` is not supported")
})
