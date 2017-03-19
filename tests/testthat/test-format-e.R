context("format e")

test_that("format(\"e\") can output exponent notation", {
  f <- fmt_new("e")
  expect_equal(f(0), "0.000000e+00")
  expect_equal(f(42), "4.200000e+01")
  expect_equal(f(42000000), "4.200000e+07")
  expect_equal(f(420000000), "4.200000e+08")
  expect_equal(f(-4), "-4.000000e+00")
  expect_equal(f(-42), "-4.200000e+01")
  expect_equal(f(-4200000), "-4.200000e+06")
  expect_equal(f(-42000000), "-4.200000e+07")
  expect_equal(fmt_new(".0e")(42), "4e+01")
  expect_equal(fmt_new(".3e")(42), "4.200e+01")
})

test_that("format(\"e\") can format negative zero as zero", {
  expect_equal(fmt_new("1e")(-0), "0.000000e+00")
  expect_equal(fmt_new("1e")(-1e-12), "-1.000000e-12")
})

test_that("format(\",e\") does not group Infinity", {
  expect_equal(fmt_new(",e")(Inf), "Inf")
})
