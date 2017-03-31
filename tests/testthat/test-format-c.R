context("format c")

test_that("fmt_new(\"c\") works", {
  expect_equal(fmt_new("c")(c("abc", 123, NA)), c("abc", "123", "NA"))
})

test_that("fmt_new(\".>c\") works", {
  expect_equal(fmt_new(".>5c")(c("abc", 123, NA)), c("..abc", "..123", "...NA"))
})

test_that("fmt_new(\"$c\") works", {
  expect_equal(fmt_new("$c")(c("abc", 123, NA)), c("$abc", "$123", "NA"))
})
