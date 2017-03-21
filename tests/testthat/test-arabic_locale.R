context("arabic locales")

test_that("fmt(…) can format numbers using ar-001 locale.", {
  locale <- fmt_locales[["ar-001"]]
  expect_equal(fmt(-1234.56, spec = "$,.2f", locale = locale), "-١٬٢٣٤٫٥٦")
})

test_that("fmt(…) can format numbers using ar-AE locale.", {
  locale <- fmt_locales[["ar-AE"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "١٬٢٣٤٫٥٦ د.إ.")
})

test_that("fmt(…) can format numbers using ar-BH locale.", {
  locale <- fmt_locales[["ar-BH"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "١٬٢٣٤٫٥٦ د.ب.")
})

test_that("fmt(…) can format numbers using ar-DJ locale.", {
  locale <- fmt_locales[["ar-DJ"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "\u200fFdj ١٬٢٣٤٫٥٦")
})

test_that("fmt(…) can format numbers using ar-DZ locale.", {
  locale <- fmt_locales[["ar-DZ"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale), "د.ج. 1.234,56")

})

test_that("fmt(…) can format numbers using ar-EG locale.", {
  locale <- fmt_locales[["ar-EG"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale), "١٬٢٣٤٫٥٦ ج.م.")

})

test_that("fmt(…) can format numbers using ar-EH locale.", {
  locale <- fmt_locales[["ar-EH"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale), "د.م. 1,234.56")

})

test_that("fmt(…) can format numbers using ar-ER locale.", {
  locale <- fmt_locales[["ar-ER"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale), "Nfk ١٬٢٣٤٫٥٦")

})

test_that("fmt(…) can format numbers using ar-IL locale.", {
  locale <- fmt_locales[["ar-IL"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale), "₪ ١٬٢٣٤٫٥٦")

})

test_that("fmt(…) can format numbers using ar-IQ locale.", {
  locale <- fmt_locales[["ar-IQ"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale), "١٬٢٣٤٫٥٦ د.ع.")

})

test_that("fmt(…) can format numbers using ar-JO locale.", {
  locale <- fmt_locales[["ar-JO"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale), "١٬٢٣٤٫٥٦ د.أ.")

})

test_that("fmt(…) can format numbers using ar-KM locale.", {
  locale <- fmt_locales[["ar-KM"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "١٬٢٣٤٫٥٦ ف.ج.ق.")

})

test_that("fmt(…) can format numbers using ar-KW locale.", {
  locale <- fmt_locales[["ar-KW"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale), "١٬٢٣٤٫٥٦ د.ك.")

})

test_that("fmt(…) can format numbers using ar-LB locale.", {
  locale <- fmt_locales[["ar-LB"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale), "١٬٢٣٤٫٥٦ ل.ل.")

})

test_that("fmt(…) can format numbers using ar-MA locale.", {
  locale <- fmt_locales[["ar-MA"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale), "د.م. 1.234,56")

})

test_that("fmt(…) can format numbers using ar-MR locale.", {
  locale <- fmt_locales[["ar-MR"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale), "١٬٢٣٤٫٥٦ أ.م.")

})

test_that("fmt(…) can format numbers using ar-OM locale.", {
  locale <- fmt_locales[["ar-OM"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale), "١٬٢٣٤٫٥٦ ر.ع.")

})

test_that("fmt(…) can format numbers using ar-PS locale.", {
  locale <- fmt_locales[["ar-PS"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale), "₪ ١٬٢٣٤٫٥٦")

})

test_that("fmt(…) can format numbers using ar-QA locale.", {
  locale <- fmt_locales[["ar-QA"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale), "١٬٢٣٤٫٥٦ ر.ق.")

})

test_that("fmt(…) can format numbers using ar-SA locale.", {
  locale <- fmt_locales[["ar-SA"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale), "١٬٢٣٤٫٥٦ ر.س.")

})

test_that("fmt(…) can format numbers using ar-SD locale.", {
  locale <- fmt_locales[["ar-SD"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale), "١٬٢٣٤٫٥٦ ج.س.")

})

test_that("fmt(…) can format numbers using ar-SO locale.", {
  locale <- fmt_locales[["ar-SO"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "\u200fS \u0661\u066c\u0662\u0663\u0664\u066b\u0665\u0666")
})

test_that("fmt(…) can format numbers using ar-SS locale.", {
  locale <- fmt_locales[["ar-SS"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale), "£ ١٬٢٣٤٫٥٦")

})

test_that("fmt(…) can format numbers using ar-SY locale.", {
  locale <- fmt_locales[["ar-SY"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale), "١٬٢٣٤٫٥٦ ل.س.")

})

test_that("fmt(…) can format numbers using ar-TD locale.", {
  locale <- fmt_locales[["ar-TD"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "\u200fFCFA ١٬٢٣٤٫٥٦")

})

test_that("fmt(…) can format numbers using ar-TN locale.", {
  locale <- fmt_locales[["ar-TN"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "د.ت. 1.234,56")
})

test_that("fmt(…) can format numbers using ar-YE locale.", {
  locale <- fmt_locales[["ar-YE"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "١٬٢٣٤٫٥٦ ر.ى.")
})
