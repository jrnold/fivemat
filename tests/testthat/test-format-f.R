context("format f")

test_that("fmt_new(\"f\") can output fixed-point notation", {
  expect_equal(fmt_new(".1f")(0.49), "0.5")
  expect_equal(fmt_new(".2f")(0.449), "0.45")
  expect_equal(fmt_new(".3f")(0.4449), "0.445")
  expect_equal(fmt_new(".5f")(0.444449), "0.44445")
  expect_equal(fmt_new(".1f")(100), "100.0")
  expect_equal(fmt_new(".2f")(100), "100.00")
  expect_equal(fmt_new(".3f")(100), "100.000")
  expect_equal(fmt_new(".5f")(100), "100.00000")
})

test_that("fmt_new(\"+$,f\") can output a currency with comma-grouping and sign", {
  f <- fmt_new("+$,.2f")
  expect_equal(f(0), "+$0.00")
  expect_equal(f(0.429), "+$0.43")
  expect_equal(f(-0.429), "-$0.43")
  expect_equal(f(-1), "-$1.00")
  expect_equal(f(1e4), "+$10,000.00")
})

test_that("fmt_new(\",.f\") can group thousands, space fill, and round to significant digits", {
  expect_equal(fmt_new("10,.1f")(123456.49), " 123,456.5")
  expect_equal(fmt_new("10,.2f")(1234567.449), "1,234,567.45")
  expect_equal(fmt_new("10,.3f")(12345678.4449), "12,345,678.445")
  expect_equal(fmt_new("10,.5f")(123456789.444449), "123,456,789.44445")
  expect_equal(fmt_new("10,.1f")(123456), " 123,456.0")
  expect_equal(fmt_new("10,.2f")(1234567), "1,234,567.00")
  expect_equal(fmt_new("10,.3f")(12345678), "12,345,678.000")
  expect_equal(fmt_new("10,.5f")(123456789), "123,456,789.00000")
})

test_that("fmt_new(\"f\") can display integers in fixed-point notation", {
  expect_equal(fmt_new("f")(42), "42.000000")
})

test_that("fmt_new(\"f\") can format negative zero as zero", {
  expect_equal(fmt_new("f")(-0), "0.000000")
  expect_equal(fmt_new("f")(-1e-12), "0.000000")
})

test_that("fmt_new(\"f\") can format negative infinity", {
  expect_equal(fmt_new("f")(-Inf), "-Inf")
})

test_that("fmt_new(\",f\") does not group Infinity", {
  expect_equal(fmt_new(",f")(Inf), "Inf")
})