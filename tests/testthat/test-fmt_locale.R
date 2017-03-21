context("fmt_locale")

test_that(paste("fmt_new(decimal = decimal) observes the specified",
                "decimal point"), {
  expect_equal(fmt_new(locale = fmt_locale(decimal_mark = "|"),
                     spec = "06.2f")(2), "002|00")
  expect_equal(fmt_new(locale = fmt_locale(decimal_mark = "/"),
                     spec = "06.2f")(2), "002/00")
})

test_that(paste("fmt_locale(currency= c(prefix, suffix))",
                "observes the specified currency prefix and suffix"), {
  expect_equal(fmt_new(locale = fmt_locale(decimal_mark = ".",
                                           currency = c("\u0e3f", "")),
                       spec = "$06.2f")(2), "\u0e3f02.00")
  expect_equal(fmt_new(locale = fmt_locale(decimal_mark = ".",
                                           currency = c("", "\u0e3f")),
                       spec = "$06.2f")(2), "02.00\u0e3f")
})

test_that(paste("fmt_locale(grouping = null) does",
                "not perform any grouping"), {
  expect_equal(fmt_new(locale = fmt_locale(decimal_mark = ".",
                                           grouping = integer()),
                       spec = "012,.2f")(2), "000000002.00")
})

test_that(paste("fmt_locale(grouping = c(sizes)) observes",
                "the specified group sizes"), {
  expect_equal(fmt_new(locale = fmt_locale(decimal_mark = ".",
                                           grouping = c(3),
                                           grouping_mark = ","),
                       spec = "012,.2f")(2), "0,000,002.00")
  expect_equal(fmt_new(locale = fmt_locale(decimal_mark = ".",
                                           grouping = c(2),
                                           grouping_mark = ","),
                       spec = "012,.2f")(2), "0,00,00,02.00")
  expect_equal(fmt_new(locale = fmt_locale(decimal_mark = ".",
                                           grouping = c(2, 3),
                                           grouping_mark = ","),
                       spec = "012,.2f")(2), "00,000,02.00")
  expect_equal(fmt_new(locale = fmt_locale(decimal_mark = ".",
                                           grouping = c(3, 2, 2, 2, 2, 2, 2),
                                           grouping_mark = ","),
                       spec = ",.0f")(1e12), "10,00,00,00,00,000")
})

test_that(paste("fmt_locale(...) can format numbers using the",
                "Indian numbering system."), {
  format <- fmt_new(locale = "en-IN", spec = ",")
  expect_equal(format(10), "10")
  expect_equal(format(100), "100")
  expect_equal(format(1000), "1,000")
  expect_equal(format(10000), "10,000")
  expect_equal(format(100000), "1,00,000")
  expect_equal(format(1000000), "10,00,000")
  expect_equal(format(10000000), "1,00,00,000")
  expect_equal(format(10000000.4543), "1,00,00,000.4543")
  expect_equal(format(1000.321), "1,000.321")
  expect_equal(format(10.5), "10.5")
  expect_equal(format(-10), "-10")
  expect_equal(format(-100), "-100")
  expect_equal(format(-1000), "-1,000")
  expect_equal(format(-10000), "-10,000")
  expect_equal(format(-100000), "-1,00,000")
  expect_equal(format(-1000000), "-10,00,000")
  expect_equal(format(-10000000), "-1,00,00,000")
  expect_equal(format(-10000000.4543), "-1,00,00,000.4543")
  expect_equal(format(-1000.321), "-1,000.321")
  expect_equal(format(-10.5), "-10.5")
})

test_that(paste("fmt_locale(thousands = separator) observes",
                "the specified group separator"), {
  expect_equal(fmt_new(locale = fmt_locale(decimal_mark = ".",
                                           grouping = c(3),
                                           grouping_mark = " "),
                       spec = "012,.2f")(2), "0 000 002.00")
  expect_equal(fmt_new(locale = fmt_locale(decimal_mark = ".",
                                           grouping = c(3),
                                           grouping_mark = "/"),
                       spec = "012,.2f")(2), "0/000/002.00")
})

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
