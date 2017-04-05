context("format n")

test_that("fmt_new(\"n\") is an alias for \",g\"", {
  f <- fmt_new(".12n")$render
  expect_equal(f(0), "0.00000000000")
  expect_equal(f(42), "42.0000000000")
  expect_equal(f(42000000), "42,000,000.0000")
  expect_equal(f(420000000), "420,000,000.000")
  expect_equal(f(-4), "-4.00000000000")
  expect_equal(f(-42), "-42.0000000000")
  expect_equal(f(-4200000), "-4,200,000.00000")
  expect_equal(f(-42000000), "-42,000,000.0000")
  expect_equal(f(.0042), "0.00420000000000")
  expect_equal(f(.42), "0.420000000000")
  expect_equal(f(1e21), "1.00000000000e+21")
})

test_that("fmt_new(\"n\") uses zero padding", {
  expect_equal(fmt_new("01.0n")$render(0), "0")
  expect_equal(fmt_new("02.0n")$render(0), "00")
  expect_equal(fmt_new("03.0n")$render(0), "000")
  expect_equal(fmt_new("05.0n")$render(0), "0,000")
  expect_equal(fmt_new("08.0n")$render(0), "0,000,000")
  expect_equal(fmt_new("013.0n")$render(0), "0,000,000,000")
  expect_equal(fmt_new("021.0n")$render(0), "0,000,000,000,000,000")
  expect_equal(fmt_new("013.8n")$render(-42000000), "-0,042,000,000")
})

test_that("fmt_new(\"n\") works with special values", {
  expect_equal(fmt_new("g")$render(c(-Inf, Inf, NA, NaN)),
               c("-Inf", "Inf", "NA", "NaN"))
})
