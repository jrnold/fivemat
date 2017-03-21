context("arabic locales")

test_that("fmt(…) can format numbers using ar-001 locale.", {
  locale <- fmt_locales[["ar-001"]]
  expect_equal(fmt(-1234.56, spec = "$,.2f", locale = locale),
               "-\u0661\u066c\u0662\u0663\u0664\u066b\u0665\u0666")
})

test_that("fmt(…) can format numbers using ar-AE locale.", {
  locale <- fmt_locales[["ar-AE"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "\u0661\u066c\u0662\u0663\u0664\u066b\u0665\u0666 \u062f.\u0625.") # nolint
})

test_that("fmt(…) can format numbers using ar-BH locale.", {
  locale <- fmt_locales[["ar-BH"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "\u0661\u066c\u0662\u0663\u0664\u066b\u0665\u0666 \u062f.\u0628.") # nolint
})

test_that("fmt(…) can format numbers using ar-DJ locale.", {
  locale <- fmt_locales[["ar-DJ"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "\u200fFdj \u0661\u066c\u0662\u0663\u0664\u066b\u0665\u0666")
})

test_that("fmt(…) can format numbers using ar-DZ locale.", {
  locale <- fmt_locales[["ar-DZ"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "\u062f.\u062c. 1.234,56")
})

test_that("fmt(…) can format numbers using ar-EG locale.", {
  locale <- fmt_locales[["ar-EG"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "\u0661\u066c\u0662\u0663\u0664\u066b\u0665\u0666 \u062c.\u0645.") # nolint
})

test_that("fmt(…) can format numbers using ar-EH locale.", {
  locale <- fmt_locales[["ar-EH"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "\u062f.\u0645. 1,234.56")
})

test_that("fmt(…) can format numbers using ar-ER locale.", {
  locale <- fmt_locales[["ar-ER"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "Nfk \u0661\u066c\u0662\u0663\u0664\u066b\u0665\u0666")

})

test_that("fmt(…) can format numbers using ar-IL locale.", {
  locale <- fmt_locales[["ar-IL"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "\u20aa \u0661\u066c\u0662\u0663\u0664\u066b\u0665\u0666")

})

test_that("fmt(…) can format numbers using ar-IQ locale.", {
  locale <- fmt_locales[["ar-IQ"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "\u0661\u066c\u0662\u0663\u0664\u066b\u0665\u0666 \u062f.\u0639.") # nolint

})

test_that("fmt(…) can format numbers using ar-JO locale.", {
  locale <- fmt_locales[["ar-JO"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "\u0661\u066c\u0662\u0663\u0664\u066b\u0665\u0666 \u062f.\u0623.") # nolint

})

test_that("fmt(…) can format numbers using ar-KM locale.", {
  locale <- fmt_locales[["ar-KM"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "\u0661\u066c\u0662\u0663\u0664\u066b\u0665\u0666 \u0641.\u062c.\u0642.") # nolint

})

test_that("fmt(…) can format numbers using ar-KW locale.", {
  locale <- fmt_locales[["ar-KW"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "\u0661\u066c\u0662\u0663\u0664\u066b\u0665\u0666 \u062f.\u0643.") # nolint
})

test_that("fmt(…) can format numbers using ar-LB locale.", {
  locale <- fmt_locales[["ar-LB"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "\u0661\u066c\u0662\u0663\u0664\u066b\u0665\u0666 \u0644.\u0644.") # nolint

})

test_that("fmt(…) can format numbers using ar-MA locale.", {
  locale <- fmt_locales[["ar-MA"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "\u062f.\u0645. 1.234,56")

})

test_that("fmt(…) can format numbers using ar-MR locale.", {
  locale <- fmt_locales[["ar-MR"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "\u0661\u066c\u0662\u0663\u0664\u066b\u0665\u0666 \u0623.\u0645.") # nolint
})

test_that("fmt(…) can format numbers using ar-OM locale.", {
  locale <- fmt_locales[["ar-OM"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "\u0661\u066c\u0662\u0663\u0664\u066b\u0665\u0666 \u0631.\u0639.") # nolint
})

test_that("fmt(…) can format numbers using ar-PS locale.", {
  locale <- fmt_locales[["ar-PS"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "\u20aa \u0661\u066c\u0662\u0663\u0664\u066b\u0665\u0666")
})

test_that("fmt(…) can format numbers using ar-QA locale.", {
  locale <- fmt_locales[["ar-QA"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "\u0661\u066c\u0662\u0663\u0664\u066b\u0665\u0666 \u0631.\u0642.") # nolint
})

test_that("fmt(…) can format numbers using ar-SA locale.", {
  locale <- fmt_locales[["ar-SA"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "\u0661\u066c\u0662\u0663\u0664\u066b\u0665\u0666 \u0631.\u0633.") # nolint

})

test_that("fmt(…) can format numbers using ar-SD locale.", {
  locale <- fmt_locales[["ar-SD"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "\u0661\u066c\u0662\u0663\u0664\u066b\u0665\u0666 \u062c.\u0633.") # nolint

})

test_that("fmt(…) can format numbers using ar-SO locale.", {
  locale <- fmt_locales[["ar-SO"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "\u200fS \u0661\u066c\u0662\u0663\u0664\u066b\u0665\u0666")
})

test_that("fmt(…) can format numbers using ar-SS locale.", {
  locale <- fmt_locales[["ar-SS"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "\u00a3 \u0661\u066c\u0662\u0663\u0664\u066b\u0665\u0666")

})

test_that("fmt(…) can format numbers using ar-SY locale.", {
  locale <- fmt_locales[["ar-SY"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "\u0661\u066c\u0662\u0663\u0664\u066b\u0665\u0666 \u0644.\u0633.") # nolint

})

test_that("fmt(…) can format numbers using ar-TD locale.", {
  locale <- fmt_locales[["ar-TD"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "\u200fFCFA \u0661\u066c\u0662\u0663\u0664\u066b\u0665\u0666")

})

test_that("fmt(…) can format numbers using ar-TN locale.", {
  locale <- fmt_locales[["ar-TN"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "\u062f.\u062a. 1.234,56")
})

test_that("fmt(…) can format numbers using ar-YE locale.", {
  locale <- fmt_locales[["ar-YE"]]
  expect_equal(fmt(1234.56, spec = "$,.2f", locale = locale),
               "\u0661\u066c\u0662\u0663\u0664\u066b\u0665\u0666 \u0631.\u0649.") # nolint
})
