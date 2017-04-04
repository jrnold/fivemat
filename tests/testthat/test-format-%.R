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
