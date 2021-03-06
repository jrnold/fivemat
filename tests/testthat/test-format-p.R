context("format p")

test_that("fmt_new(\"p\") can output a percentage", {
  f <- fmt_new("p")
  expect_equal(f(.00123), "0.123000%")
  expect_equal(f(.0123), "1.23000%")
  expect_equal(f(.123), "12.3000%")
  expect_equal(f(.234), "23.4000%")
  expect_equal(f(1.23), "123.000%")
  expect_equal(f(-.00123), "-0.123000%")
  expect_equal(f(-.0123), "-1.23000%")
  expect_equal(f(-.123), "-12.3000%")
  expect_equal(f(-1.23), "-123.000%")
})

test_that("fmt_new(\"+p\") can output a percentage with rounding and sign", {
  f <- fmt_new("+.2p")
  expect_equal(f(.00123), "+0.12%")
  expect_equal(f(.0123), "+1.2%")
  expect_equal(f(.123), "+12%")
  expect_equal(f(1.23), "+120%")
  expect_equal(f(-.00123), "-0.12%")
  expect_equal(f(-.0123), "-1.2%")
  expect_equal(f(-.123), "-12%")
  expect_equal(f(-1.23), "-120%")
})

test_that("fmt_new(\"p\") works with special values", {
  f <- fmt_new("p")
  expect_equal(f(c(Inf, -Inf, NA, NaN)), c("Inf", "-Inf", "NA", "NaN"))
})
