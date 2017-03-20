context("format o")

test_that("fmt_new(\"o\") octal", {
  expect_equal(fmt_new("o")(c(0, 10, 10.2, NA, Inf, -Inf, NaN)),
               c("0", "12", "12", "NA", "Inf", "-Inf", "NaN"))
})

test_that("fmt_new(\"#o\") octal with prefix", {
  expect_equal(fmt_new("#o")(c(0, 10, 10.2, NA, Inf, -Inf, NaN)),
               c(paste0("0o", c("0", "12", "12")), "NA", "Inf", "-Inf", "NaN"))
})
