context("format p")

test_that("fmt_new(\"p\") can output a percentage", {
  f <- fmt_new("p")$render
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
  f <- fmt_new("+.2p")$render
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
  f <- fmt_new("p")$render
  expect_equal(f(c(Inf, -Inf, NA, NaN)), c("Inf", "-Inf", "NA", "NaN"))
})

context("fmt_new P")

test_that("fmt_new(\"P\") can output a whole percentage", {
  f <- fmt_new(".0P")
  expect_equal(f$render(0), "0%")
  expect_equal(f$render(.042), "4%")
  expect_equal(f$render(.42), "42%")
  expect_equal(f$render(4.2), "420%")
  expect_equal(f$render(-.042), "-4%")
  expect_equal(f$render(-.42), "-42%")
  expect_equal(f$render(-4.2), "-420%")
})

test_that("fmt_new(\".%\") can output a percentage with precision", {
  expect_equal(fmt(.234, ".1%"), "23.4%")
  expect_equal(fmt(.234, ".2%"), "23.40%")
})

test_that("fmt_new(\"%\") fill respects suffix",  {
  expect_equal(fmt_new("020.0%")$render(42), "0000000000000004200%")
  expect_equal(fmt_new("20.0%")$render(42), "               4200%")
})

test_that("fmt_new(\"^%\") align center puts suffix adjacent to number", {
  expect_equal(fmt_new("^21.0P")$render(.42),    "         42%         ")
  expect_equal(fmt_new("^21,.0P")$render(422),   "       42,200%       ")
  expect_equal(fmt_new("^21,.0P")$render(-422),  "      -42,200%       ")
})

test_that("fmt_new(\"P\") works with special values", {
  f <- fmt_new(".0P")$render
  expect_equal(f(c(NA, NaN, Inf, -Inf)), c("NA", "NaN", "Inf", "-Inf"))
})
