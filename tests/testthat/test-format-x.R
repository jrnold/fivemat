context("fmt_new x")

test_that(paste("fmt_new(\"x\") returns the expected hexadecimal (lowercase)",
                "string"), {
  expect_equal(fmt_new("x")(0xabef970), "abef970")
})

test_that(paste("fmt_new(\"#x\") returns the expected hexadecimal (lowercase)",
                "string with prefix"), {
  expect_equal(fmt_new("#x")(0xabef970), "0xabef970")
})

test_that("fmt_new(\",x\") groups thousands", {
  expect_equal(fmt_new(",x")(0xabef970), "a,bef,970")
})

test_that("fmt_new(\",x\") groups thousands", {
  expect_equal(fmt_new(",x")(0xabef970), "a,bef,970")
})

test_that("fmt_new(\"#,x\") does not group the prefix", {
  expect_equal(fmt_new("#,x")(0xabef970), "0xa,bef,970")

})

test_that("fmt_new(\"+#x\") puts the sign before the prefix", {
  expect_equal(fmt_new("+#x")(0xabef970),  "+0xabef970")
  expect_equal(fmt_new("+#x")(-0xabef970), "-0xabef970")
  expect_equal(fmt_new(" #x")(0xabef970),  " 0xabef970")
  expect_equal(fmt_new(" #x")(-0xabef970), "-0xabef970")

})

test_that("fmt_new(\"$,x\") fmt_news hexadecimal currency", {
  expect_equal(fmt_new("$,x")(0xabef970), "$a,bef,970")

})

test_that("fmt_new(\"[.precision]x\") always has precision zero", {
  expect_equal(fmt_new(".2x")(0xabef970), "abef970")
  expect_equal(fmt_new(".2x")(-4.2), "-4")

})

test_that("fmt_new(\"x\") rounds non-integers", {
  expect_equal(fmt_new("x")(2.4), "2")

})

test_that("fmt_new(\"x\") can fmt_new negative zero as zero", {
  expect_equal(fmt_new("x")(-0), "0")
  expect_equal(fmt_new("x")(-1e-12), "0")
})

test_that("fmt_new(\"x\") does not consider -0xeee to be positive", {
  expect_equal(fmt_new("x")(-0xeee), "-eee")

})

test_that(paste("fmt_new(\"X\") returns the expected",
                "hexadecimal (uppercase) string"), {
  expect_equal(fmt_new("X")(0xabef970), "ABEF970")

})

test_that(paste("fmt_new(\"#X\") returns the expected",
                "hexadecimal (uppercase) string with prefix"), {
  expect_equal(fmt_new("#X")(0xabef970), "0xABEF970")
})

test_that("fmt_new(\"X\") can fmt_new negative zero as zero", {
  expect_equal(fmt_new("X")(-0), "0")
  expect_equal(fmt_new("X")(-1e-12), "0")
})

test_that("fmt_new(\"X\") does not consider -0xeee to be positive", {
  expect_equal(fmt_new("X")(-0xeee), "-EEE")

})

test_that("fmt_new(\"#[width]x\") considers the prefix", {
  expect_equal(fmt_new("20x")(0xabef970),   "             abef970")
  expect_equal(fmt_new("#20x")(0xabef970),  "           0xabef970")
  expect_equal(fmt_new("020x")(0xabef970),  "0000000000000abef970")
  expect_equal(fmt_new("#020x")(0xabef970), "0x00000000000abef970")
})

test_that("fmt_new(x) works with special values", {
  expect_equal(fmt_new("x")(NA), "NA")
  expect_equal(fmt_new("#x")(NA), "NA")
  expect_equal(fmt_new("X")(NA), "NA")
  expect_equal(fmt_new("#X")(NA), "NA")
})
