context("fmt_spec")

test_that("as_fmt_spec(specifier) throws an error for invalid formats", {
  expect_error(as_fmt_spec("foo"), regexp = "\"foo\" is an invalid format.")
  expect_error(as_fmt_spec(".-2s"), regexp = "\"\\.-2s\" is an invalid format")
  expect_error(as_fmt_spec(".f"), regexp = "\"\\.f\" is an invalid format")
})

test_that("as_fmt_spec(specifier) returns a as_fmt_spec object", {
  s <- as_fmt_spec("")
  expect_is(s, "fmt_spec")
})

test_that("as_fmt_spec(\"\") has the expected defaults", {
  s <- as_fmt_spec()
  expect_equal(s$fill, " ")
  expect_equal(s$align, ">")
  expect_equal(s$sign, "-")
  expect_equal(s$symbol, NULL)
  expect_equal(s$width, NULL)
  expect_equal(s$comma, FALSE)
  expect_equal(s$precision, NULL)
  expect_equal(s$type, NULL)
})

test_that("as_fmt_spec(specifier) uses the none type for unknown types", {
  expect_equal(as_fmt_spec("q")$type, NULL)
  expect_equal(as_fmt_spec("S")$type, NULL)
})

test_that("as_fmt_spec(\"n\") is an alias for \",g\"", {
  s <- as_fmt_spec("n")
  expect_equal(s$comma, TRUE)
  expect_equal(s$type, "g")
})

test_that("as_fmt_spec(\"0\") is an alias for \"0=\"", {
  s <- as_fmt_spec("0")
  expect_equal(s$fill, "0")
  expect_equal(s$align, "=")
})

test_that("format(as_fmt_spec(specifier)) reflects current field values", {
  s <- as_fmt_spec()
  s$fill <- "_"
  expect_equal(format(s), "_>-")
  s$align <- "^"
  expect_equal(format(s), "_^-")
  s$sign <- "+"
  expect_equal(format(s), "_^+")
  s$symbol <- "$"
  expect_equal(format(s), "_^+$")
  s$align <- "="
  s$fill <- "0"
  expect_equal(format(s), "0=+$")
  s$width <- 12
  expect_equal(format(s), "0=+$12")
  s$comma <- TRUE
  expect_equal(format(s), "0=+$12,")
  s$precision <- 2
  expect_equal(format(s), "0=+$012,.2")
  s$type <- "f"
  expect_equal(format(s), "0=+$12,.2f")
  expect_equal(fmt_new(s)(42), "+$0,000,042.00")
})

test_that("format(as_fmt_spec(specifier)) clamps precision to zero", {
  s <- as_fmt_spec()
  s$precision <- -1
  expect_equal(format(s), " >-.0")
})

test_that("as_fmt_spec(specifier).toString() clamps width to one", {
  s <- as_fmt_spec()
  s$width <- -1
  expect_equal(format(s), " >-1")
})
