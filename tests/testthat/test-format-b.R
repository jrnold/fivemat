context("format b")

test_that("format(\"b\") binary", {
  expect_equal(fmt_new("b")(10), "1010")
})

test_that("format(\"#b\") binary with prefix", {
  expect_equal(fmt_new("#b")(10), "0b1010")
})
