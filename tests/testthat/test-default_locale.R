context("fmt_locale")

library("purrr")
test_that("fmt_locale numerals argument works", {
  expect_equal(fmt_locale(numerals = LETTERS[1:10])$numerals,
               set_names(LETTERS[1:10], as.character(0:9)))
  expect_equal(fmt_locale(numerals = set_names(LETTERS[1:10], 9:0))$numerals,
               set_names(LETTERS[10:1], as.character(0:9)))
})

test_that("fmt_locale numerals raises error if wrong names", {
  expect_error(fmt_locale(numerals = set_names(as.character(0:9),
                                               LETTERS[1:10]))$numerals,
               regexp = "names\\(numerals\\) == as.character\\(0:9\\)")
  expect_error(fmt_locale(numerals = as.character(0:10))$numerals,
               regexp = "length\\(numerals\\) not equal to 10")
})
