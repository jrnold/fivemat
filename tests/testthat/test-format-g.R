context("formats g and G")

test_that("fmt_new(\"g\") can output general notation", {
  expect_equal(fmt_new(".1g")(0.049), "0.05")
  expect_equal(fmt_new(".1g")(0.49), "0.5")
  expect_equal(fmt_new(".2g")(0.449), "0.45")
  expect_equal(fmt_new(".3g")(0.4449), "0.445")
  expect_equal(fmt_new(".5g")(0.444449), "0.44445")
  expect_equal(fmt_new(".1g")(100), "1e+02")
  expect_equal(fmt_new(".2g")(100), "1.0e+02")
  expect_equal(fmt_new(".3g")(100), "100")
  expect_equal(fmt_new(".5g")(100), "100.00")
  expect_equal(fmt_new(".5g")(100.2), "100.20")
  expect_equal(fmt_new(".2g")(0.002), "0.0020")
})

test_that("fmt_new(\"G\") can output general notation", {
  expect_equal(fmt_new(".1G")(0.049), "0.05")
  expect_equal(fmt_new(".1G")(0.49), "0.5")
  expect_equal(fmt_new(".2G")(0.449), "0.45")
  expect_equal(fmt_new(".3G")(0.4449), "0.445")
  expect_equal(fmt_new(".5G")(0.444449), "0.44445")
  expect_equal(fmt_new(".1G")(100), "1E+02")
  expect_equal(fmt_new(".2G")(100), "1.0E+02")
  expect_equal(fmt_new(".3G")(100), "100")
  expect_equal(fmt_new(".5G")(100), "100.00")
  expect_equal(fmt_new(".5G")(100.2), "100.20")
  expect_equal(fmt_new(".2G")(0.002), "0.0020")
})

test_that("fmt_new(\",g\") can group thousands with general notation", {
  f <- fmt_new(",.12g")
  expect_equal(f(0), "0.00000000000")
  expect_equal(f(42), "42.0000000000")
  expect_equal(f(42000000), "42,000,000.0000")
  expect_equal(f(420000000), "420,000,000.000")
  expect_equal(f(-4), "-4.00000000000")
  expect_equal(f(-42), "-42.0000000000")
  expect_equal(f(-4200000), "-4,200,000.00000")
  expect_equal(f(-42000000), "-42,000,000.0000")
})

test_that("fmt_new(\",G\") can group thousands with general notation", {
  f <- fmt_new(",.12G")
  expect_equal(f(0), "0.00000000000")
  expect_equal(f(42), "42.0000000000")
  expect_equal(f(42000000), "42,000,000.0000")
  expect_equal(f(420000000), "420,000,000.000")
  expect_equal(f(-4), "-4.00000000000")
  expect_equal(f(-42), "-42.0000000000")
  expect_equal(f(-4200000), "-4,200,000.00000")
  expect_equal(f(-42000000), "-42,000,000.0000")
})

test_that("fmt_new(\"g\") works with special values", {
  expect_equal(fmt_new("g")(c(-Inf, Inf, NA, NaN)),
               c("-Inf", "Inf", "NA", "NaN"))
})

test_that("fmt_new(\"G\") works with special values", {
  expect_equal(fmt_new("G")(c(-Inf, Inf, NA, NaN)),
               c("-Inf", "Inf", "NA", "NaN"))
})
