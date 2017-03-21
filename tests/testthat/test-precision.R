context("utils")

test_that("precision_round_(step, max) returns the expected value", {
  expect_equal(precision_round_(0.1, 1.1), 2)
  expect_equal(precision_round_(0.01, 0.99), 2)
  expect_equal(precision_round_(0.01, 1.00), 2)
  expect_equal(precision_round_(0.01, 1.01), 3)
})

test_that("precision_round_(step, max) returns the expected value", {
  expect_equal(precision_round(c(0.99, 1.0, 1.01)), 3)
  expect_equal(precision_round(c(0.9, 1.0, 1.1)), 2)
})

test_that(str_c("precision_prefix_(step, value) returns zero if step has ",
                "the same units as value"), {
  for (i in seq(-24, 24, by = 3)) {
    for (j in (i + 0:2)) {
      expect_equivalent(precision_prefix_(10 ^ i, 10 ^ j)$precision, 0)
    }
  }
})

test_that(str_c("precision_prefix_(step, value) returns greater than zero ",
                "if fractional digits are needed"), {
  for (i in seq(-24, 24, by = 3)) {
    for (j in (i - c(4, 3, 2, 1))) {
      expect_equivalent(precision_prefix_(10 ^ j, 10 ^ i)$precision, i - j)
    }
  }
})

test_that(str_c("precision_prefix_(step, value) returns the expected ",
                "precision when value is less than one yocto"), {
  expect_equivalent(precision_prefix_(1e-24, 1e-24)$precision, 0) # 1y
  expect_equivalent(precision_prefix_(1e-25, 1e-25)$precision, 1) # 0.1y
  expect_equivalent(precision_prefix_(1e-26, 1e-26)$precision, 2) # 0.01y
  expect_equivalent(precision_prefix_(1e-27, 1e-27)$precision, 3) # 0.001y
  expect_equivalent(precision_prefix_(1e-28, 1e-28)$precision, 4) # 0.0001y
})

test_that(str_c("precision_prefix_(step, value) returns the expected ",
                "precision when value is greater than than one yotta"), {
  expect_equivalent(precision_prefix_(1e24, 1e24)$precision, 0) # 1Y
  expect_equivalent(precision_prefix_(1e24, 1e25)$precision, 0) # 10Y
  expect_equivalent(precision_prefix_(1e24, 1e26)$precision, 0) # 100Y
  expect_equivalent(precision_prefix_(1e24, 1e27)$precision, 0) # 1000Y
  expect_equivalent(precision_prefix_(1e23, 1e27)$precision, 1) # 1000.0Y
})

test_that("precision_prefix(x) returns the expected precision", {
  expect_identical(precision_prefix(c(1.1e6, 1.2e6, 1.3e6)),
                   list(precision = 1L, si_prefix = c("M" = 6L)))
})

test_that("precision_fixed_(step) returns the expected value", {
  expect_equal(precision_fixed_(8.9), 0)
  expect_equal(precision_fixed_(1.1), 0)
  expect_equal(precision_fixed_(0.89), 1)
  expect_equal(precision_fixed_(0.11), 1)
  expect_equal(precision_fixed_(0.089), 2)
  expect_equal(precision_fixed_(0.011), 2)
})

test_that("precision_fixed(x) returns the expected value", {
  expect_equal(precision_fixed(c(1, 1.5, 2)), 1)
  expect_equal(precision_fixed(c(1, 2, 3)), 0)
})
