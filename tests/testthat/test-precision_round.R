context("utils")

test_that("precision_round(step, max) returns the expected value", {
  expect_equal(fivemat:::precision_round(0.1, 1.1), 2)
  expect_equal(fivemat:::precision_round(0.01, 0.99), 2)
  expect_equal(fivemat:::precision_round(0.01, 1.00), 2)
  expect_equal(fivemat:::precision_round(0.01, 1.01), 3)
})

test_that("precision_prefix(step, value) returns zero if step has the same units as value", {
  for(i in seq(-24, 24, by = 3)) {
    for(j in (i + 0:2)) {
      expect_equal(fivemat:::precision_prefix(10 ^ i, 10 ^ j), 0)
    }
  }
})

test_that("precision_prefix(step, value) returns greater than zero if fractional digits are needed", {
  for (i in seq(-24, 24, by = 3)) {
    for (j in (i - c(4, 3, 2, 1))) {
      expect_equal(fivemat:::precision_prefix(10 ^ j, 10 ^ i), i - j)
    }
  }
})

test_that("precision_prefix(step, value) returns the expected precision when value is less than one yocto", {
  expect_equal(fivemat:::precision_prefix(1e-24, 1e-24), 0) # 1y
  expect_equal(fivemat:::precision_prefix(1e-25, 1e-25), 1) # 0.1y
  expect_equal(fivemat:::precision_prefix(1e-26, 1e-26), 2) # 0.01y
  expect_equal(fivemat:::precision_prefix(1e-27, 1e-27), 3) # 0.001y
  expect_equal(fivemat:::precision_prefix(1e-28, 1e-28), 4) # 0.0001y
})

test_that("precision_prefix(step, value) returns the expected precision when value is greater than than one yotta", {
  expect_equal(fivemat:::precision_prefix(1e24, 1e24), 0) # 1Y
  expect_equal(fivemat:::precision_prefix(1e24, 1e25), 0) # 10Y
  expect_equal(fivemat:::precision_prefix(1e24, 1e26), 0) # 100Y
  expect_equal(fivemat:::precision_prefix(1e24, 1e27), 0) # 1000Y
  expect_equal(fivemat:::precision_prefix(1e23, 1e27), 1) # 1000.0Y
})

test_that("precision_fixed(number) returns the expected value", {
  expect_equal(fivemat:::precision_fixed(8.9), 0)
  expect_equal(fivemat:::precision_fixed(1.1), 0)
  expect_equal(fivemat:::precision_fixed(0.89), 1)
  expect_equal(fivemat:::precision_fixed(0.11), 1)
  expect_equal(fivemat:::precision_fixed(0.089), 2)
  expect_equal(fivemat:::precision_fixed(0.011), 2)
})
