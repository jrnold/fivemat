context("fmt_new with si_prefix")

test_that(paste("fmt_new(\"s\", si_prefix = value)(number) formats with the SI",
                "prefix appropriate to the specified value"), {
  expect_equal(fmt_new(",.0s", si_prefix = 1e-6)(.00042), "420\u03BC")
  expect_equal(fmt_new(",.0s", si_prefix = 1e-6)(.0042), "4,200\u03BC")
  expect_equal(fmt_new(",.3s", si_prefix = 1e-3)(.00042), "0.420m")
})

test_that(paste("fmt_new raises error with bad value of si_prefix"), {
  expect_error(fmt_new("", si_prefix = list()))
})

test_that(paste("fmt_new(\"s\", si_prefix = value)(number) uses yocto",
                "for very small reference values"), {
  expect_equal(fmt_new(",.0s", si_prefix = 1e-27)(1e-24), "1y")
})

test_that(paste("fmt_new(\"s\", value)(number) uses yotta for very small",
                "reference values"), {
  expect_equal(fmt_new(",.0s", si_prefix = 1e27)(1e24), "1Y")
})

test_that(paste("fmt_new(\"$,s\", si_prefix = value)(number) formats with ",
                "the specified SI prefix"), {
  f <- fmt_new(" $12,.1s", si_prefix = 1e6)
  expect_equal(f(-42e6),  "      -$42.0M")
  expect_equal(f(+4.2e6), "        $4.2M")
})

test_that("fmt_new(..., si_prefix = value) works with character input", {
  f <- fmt_new(".6f", si_prefix = "k")
  expect_equal(
    f(c(0.001, 1, 1000)),
    c("0.000001k", "0.001000k", "1.000000k"))
})

test_that("fmt_new(..., si_prefix = value) works with integer input", {
  f <- fmt_new(".6f", si_prefix = 3L)
  expect_equal(f(c(0.001, 1, 1000)),
               c("0.000001k", "0.001000k", "1.000000k"))
})
