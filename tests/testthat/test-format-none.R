context("format default")

test_that(paste("fmt_new(\".[precision]\") uses significant precision",
                "and trims insignificant zeros"), {
  expect_equal(fmt_new(".1")(4.9), "5")
  expect_equal(fmt_new(".1")(0.49), "0.5")
  expect_equal(fmt_new(".2")(4.9), "4.9")
  expect_equal(fmt_new(".2")(0.49), "0.49")
  expect_equal(fmt_new(".2")(0.449), "0.45")
  expect_equal(fmt_new(".3")(4.9), "4.9")
  expect_equal(fmt_new(".3")(0.49), "0.49")
  expect_equal(fmt_new(".3")(0.449), "0.449")
  expect_equal(fmt_new(".3")(0.4449), "0.445")
  expect_equal(fmt_new(".5")(0.444449), "0.44445")

})

test_that("fmt_new(\".[precision]\") does not trim significant zeros", {
  expect_equal(fmt_new(".5")(10), "10")
  expect_equal(fmt_new(".5")(100), "100")
  expect_equal(fmt_new(".5")(1000), "1000")
  expect_equal(fmt_new(".5")(21010), "21010")
  expect_equal(fmt_new(".5")(1.10001), "1.1")
  expect_equal(fmt_new(".5")(1.10001e6), "1.1e+06")
  expect_equal(fmt_new(".6")(1.10001), "1.10001")
  expect_equal(fmt_new(".6")(1.10001e6), "1.10001e+06")

})

test_that(paste("fmt_new(\".[precision]\") also trims the decimal point",
                "if there are only insignificant zeros"), {
  expect_equal(fmt_new(".5")(1.00001), "1")
  expect_equal(fmt_new(".5")(1.00001e6), "1e+06")
  expect_equal(fmt_new(".6")(1.00001), "1.00001")
  expect_equal(fmt_new(".6")(1.00001e6), "1.00001e+06")

})

test_that("fmt_new(\"$\") can output a currency", {
  f <- fmt_new("$")
  expect_equal(f(0), "$0")
  expect_equal(f(.042), "$0.042")
  expect_equal(f(.42), "$0.42")
  expect_equal(f(4.2), "$4.2")
  expect_equal(f(-.042), "-$0.042")
  expect_equal(f(-.42), "-$0.42")
  expect_equal(f(-4.2), "-$4.2")

})

test_that(paste("fmt_new(\"($\") can output a currency with parentheses",
                "for negative values"), {
  f <- fmt_new("($")
  expect_equal(f(0), "$0")
  expect_equal(f(.042), "$0.042")
  expect_equal(f(.42), "$0.42")
  expect_equal(f(4.2), "$4.2")
  expect_equal(f(-.042), "($0.042)")
  expect_equal(f(-.42), "($0.42)")
  expect_equal(f(-4.2), "($4.2)")

})

test_that("fmt_new(\"\") can format negative zero as zero", {
  expect_equal(fmt_new("")(-0), "0")
})

test_that("fmt_new() formats integers with the d format", {
  expect_equal(fmt_new(".3")(c(420000000, -4200000)),
               c("4.2e+08", "-4.2e+06"))
  expect_equal(fmt_new(".3")(as.integer(c(420000000, -4200000))),
               c("420000000", "-4200000"))
})

test_that("fmt_new(\"\") can format special values", {
  expect_equal(fmt_new("")(c(-Inf, Inf, NA, NaN, 4200)),
               c("-Inf", "Inf", "NA", "NaN", "4200"))
  expect_equal(fmt_new(",")(c(-Inf, Inf, NA, NaN, 4200)),
               c("-Inf", "Inf", "NA", "NaN", "4,200"))
  expect_equal(fmt_new("$,")(c(-Inf, Inf, NA, NaN, 4200)),
               c("-Inf", "Inf", "NA", "NaN", "$4,200"))
})
