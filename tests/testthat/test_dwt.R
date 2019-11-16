context("dwt")

test_that("dec_rec", {
 method <- mgr_init("dwt")
 x <- as.numeric(AirPassengers)[1:128]
 dec <- mgr_dec(method, x)
 expect_equal(dec, x)
 repr <- mgr_red(method, dec)
 expect_equal(length(repr), 127)
})

test_that("select_features", {
  method <- mgr_init("dwt")
  X <- matrix(c(1, 1, 2, 2, 1, 1, 4, 4), ncol = 4, nrow = 2,
              dimnames = list(NULL, c("F1", "F2", "F3", "F4")))
  feat <- mgr_select_features(method, X, NA, 2, NA)

  expect_equal(feat, c("F4", "F2"))
})

test_that("is_vectorized", {
  method <- mgr_init("dwt")
  expect_true(!mgr_is_vectorized(method))
})
