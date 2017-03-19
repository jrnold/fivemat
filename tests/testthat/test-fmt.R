context("fmt_new")

test_that("fmt_new(spec)(number) returns a string", {
  expect_is(fmt_new("d")(0), "character")
})

test_that("fmt_new(spec) throws an error for invalid formats", {
  expect_error(fmt_new("foo"), regexp = "is an invalid format")
  expect_error(fmt_new(".-2s"), regexp = "is an invalid format")
  expect_error(fmt_new(".f"), regexp = "is an invalid format")
})

test_that("format(\",.\") unreasonable precision values are clamped to reasonable values", {
  expect_equal(fmt_new(".30f")(0), "0.00000000000000000000")
  expect_equal(fmt_new(".0g")(1), "1")
})

# test_that("format(\"s\") handles very small and very large values", {
#   #small <- "0.00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002y"
#   #expect_equal(fmt_new("s")(.Machine$double.xmin), small)
#   #big <- "1797693134860000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000Y"
#   #expect_equal(fmt_new("s")(.Machine$double.xmax), big)
# })