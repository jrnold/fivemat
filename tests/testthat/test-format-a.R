context("format a and A")

test_that("fmt_new(\"a\") works as expected", {
  expect_equal(fmt_new(".2a")$render(c(1, 2.5, 10, 16, NA, NaN, Inf, -Inf)),
               c("1.00p+0", "1.40p+1", "1.40p+3", "1.00p+4",
                 "NA", "NaN", "Inf", "-Inf"))
})

test_that("fmt_new(\"A\") works as expected", {
  expect_equal(fmt_new(".2A")$render(c(1, 2.5, 10, 16, NA, NaN, Inf, -Inf)),
               c("1.00P+0", "1.40P+1", "1.40P+3", "1.00P+4",
                 "NA", "NaN", "Inf", "-Inf"))
})

test_that("fmt_new(\"#.2a\") works as expected", {
  expect_equal(fmt_new("#.2a")$render(c(1, 2.5, 10, 16, NA, NaN, Inf, -Inf)),
               c("0x1.00p+0", "0x1.40p+1", "0x1.40p+3", "0x1.00p+4",
                 "NA", "NaN", "Inf", "-Inf"))
})

test_that("fmt_new(\"#.2A\") works as expected", {
  expect_equal(fmt_new("#.2A")$render(c(1, 2.5, 10, 16, NA, NaN, Inf, -Inf)),
               c("0x1.00P+0", "0x1.40P+1", "0x1.40P+3", "0x1.00P+4",
                 "NA", "NaN", "Inf", "-Inf"))
})
