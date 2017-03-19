context("fmt_new %")

test_that("fmt_new(\"%\") can output a whole percentage", {
  f <- fmt_new(".0%")
  expect_equal(f(0), "0%")
  expect_equal(f(.042), "4%")
  expect_equal(f(.42), "42%")
  expect_equal(f(4.2), "420%")
  expect_equal(f(-.042), "-4%")
  expect_equal(f(-.42), "-42%")
  expect_equal(f(-4.2), "-420%")
})

test_that("fmt_new(\".%\") can output a percentage with precision", {
  expect_equal(fmt(.234, ".1%"), "23.4%")
  expect_equal(fmt(.234, ".2%"), "23.40%")
})

test_that("fmt_new(\"%\") fill respects suffix",  {
  expect_equal(fmt_new("020.0%")(42), "0000000000000004200%")
  expect_equal(fmt_new("20.0%")(42), "               4200%")
})

test_that("fmt_new(\"^%\") align center puts suffix adjacent to number", {
  expect_equal(fmt_new("^21.0%")(.42),    "         42%         ")
  expect_equal(fmt_new("^21,.0%")(422),   "       42,200%       ")
  expect_equal(fmt_new("^21,.0%")(-422),  "      -42,200%       ")
})