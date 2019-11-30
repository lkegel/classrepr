context("fbr")

test_that("dec_rec", {
  method <- mgr_init("raw")
  x <- c(1, 2, 3)
  dec <- mgr_dec(method, x)
  expect_equal(dec, x)
  red <- mgr_red(method, x)
  expect_equal(red, x)
})

test_that("distance", {
  method <- mgr_init("raw")
  x <- c(1, 2, 3)
  y <- c(2, 4, 6)
  expect_equal(mgr_distance(method, x, y, bsf = Inf), 14)
  expect_equal(mgr_distance(method, x, y, bsf = 5), 5)
})

test_that("is_vectorized", {
  method <- mgr_init("raw")
  expect_false(mgr_is_vectorized(method))
})
