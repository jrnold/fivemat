context("format b")

test_that("fmt_new(\"b\") binary", {
  expect_equal(fmt_new("b")(c(0, 10, 10.1, NA, Inf, -Inf, NaN)),
               c("0", "1010", "1010", "NA", "Inf", "-Inf", "NaN"))
})

test_that("fmt_new(\"#b\") binary with prefix", {
  expect_equal(fmt_new("#b")(c(0, 10, 10.1, NA, Inf, -Inf, NaN)),
               c(paste0("0b", c("0", "1010", "1010")),
                 c("NA", "Inf", "-Inf", "NaN")))
})
