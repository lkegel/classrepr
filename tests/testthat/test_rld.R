context("rld")

test_that("sgn0", {
  expect_equal(sgn0(0), 1)
  expect_equal(sgn0(1), 1)
  expect_equal(sgn0(-1), -1)
})

test_that("red_rld", {
  expect_equal(red_rld(c(0)), matrix(c(1, 1), nrow = 2, ncol = 1))
  expect_equal(red_rld(c(0, 0)), matrix(c(2, 1), nrow = 2, ncol = 1))
  expect_equal(red_rld(c(1, 1)), matrix(c(2, 1), nrow = 2, ncol = 1))
  expect_equal(red_rld(c(0, 1)), matrix(c(2, 1), nrow = 2, ncol = 1))
  expect_equal(red_rld(c(-1, -1)), matrix(c(2, 1), nrow = 2, ncol = 1))
  expect_equal(red_rld(c(-1, 0)), matrix(c(1, 2), nrow = 2, ncol = 1))

  expect_equal(red_rld(c( 1,  1,  1)), matrix(c(3, 1), nrow = 2, ncol = 1))
  expect_equal(red_rld(c( 1,  1, -1)), matrix(c(1, 1, 2, 1), nrow = 2, ncol = 2))
  expect_equal(red_rld(c( 1, -1,  1)), matrix(c(1, 3), nrow = 2, ncol = 1))
  expect_equal(red_rld(c( 1, -1, -1)), matrix(c(1, 1, 2, 1), nrow = 2, ncol = 2))
  expect_equal(red_rld(c(-1,  1,  1)), matrix(c(1, 1, 2, 1), nrow = 2, ncol = 2))
  expect_equal(red_rld(c(-1,  1, -1)), matrix(c(1, 3), nrow = 2, ncol = 1))
  expect_equal(red_rld(c(-1, -1,  1)), matrix(c(1, 1, 2, 1), nrow = 2, ncol = 2))
  expect_equal(red_rld(c(-1, -1, -1)), matrix(c(3, 1), nrow = 2, ncol = 1))

  x <- as.numeric(AirPassengers)
  x <- (x - mean(x)) / sd(x)
  res <- red_rld(x)
  expect_equal(sum(res[1, ] * res[2, ]), length(x))
})

test_that("dec_rec", {
 method <- mgr_init("rld")

 x <- as.numeric(AirPassengers)
 dec <- mgr_dec(method, x)
 expect_equal(dec, x)

 repr <- mgr_red(method, dec)
 expect_equal(repr, c(V1 = 1))
})

test_that("is_vectorized", {
  method <- mgr_init("rld")
  expect_true(!mgr_is_vectorized(method))
})
