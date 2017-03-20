context("format o")

test_that("format(\"o\") octal", {
  expect_equal(fmt_new("o")(10), "12")
})

test_that("format(\"#o\") octal with prefix", {
  expect_equal(fmt_new("#o")(10), "0o12")
})
