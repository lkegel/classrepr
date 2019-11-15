context("dwt")

test_that("dec_rec", {
 method <- mgr_init("dwt")
 x <- as.numeric(AirPassengers)[1:128]
 dec <- mgr_dec(method, x)
 expect_equal(dec, x)
 repr <- mgr_red(method, dec)
 expect_equal(length(repr), 127)
})

test_that("is_vectorized", {
  method <- mgr_init("dwt")
  expect_true(!mgr_is_vectorized(method))
})
