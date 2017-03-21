context("SI prefixes")

test_that("si_prefix.character works as expected", {
  expect_identical(si_prefix(names(SI_PREFIXES)), SI_PREFIXES)
  expect_identical(si_prefix(c("mu", "", NA, " ", "k")),
                   set_names(as.integer(c(-6, 0, 0, 0, 3)),
                             c("\u03BC", "", "", "", "k")))
})

test_that("si_prefix.integer works as expected", {
  expect_identical(si_prefix(as.integer(seq(-30, 30))),
                   c(rep(SI_PREFIXES[1], 9),
                     rep(SI_PREFIXES[2:16], each = 3),
                     rep(SI_PREFIXES[17], 7)))
})

test_that("si_prefix.integer works as expected", {
  expect_identical(si_prefix(1.1 * 10 ^ seq(-24, 26)),
                   rep(SI_PREFIXES, each = 3))
})
