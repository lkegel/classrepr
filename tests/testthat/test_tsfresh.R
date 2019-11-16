context("fbr")

test_that("dec_rec", {
  x <- matrix(c(AirPassengers, AirPassengers), ncol = length(AirPassengers),
              nrow = 2, byrow = T)

  method <- mgr_init("tsfresh")
  ts_start <- as.POSIXct("2010-01-01 01:23:54", tz = "UTC")
  ts_end <- as.POSIXct("2010-05-01 01:23:54", tz = "UTC")
  method$ti <- ts_start + seq(0, 143) * 3600

  # 1 Row
  dec <- mgr_dec(method, x[1, , drop = F])
  expect_equal(x[1, , drop = F], dec)
  repr <- mgr_red(method, dec)
  expect_equal(nrow(repr), 1)

  # 2 Rows
  dec <- mgr_dec(method, x)
  expect_equal(x, dec)
  repr <- mgr_red(method, dec)
  expect_equal(nrow(repr), 2)

  # 2 Rows
  dec <- mgr_dec(method, x, 2)
  expect_equal(x, dec)
  repr <- mgr_red(method, dec, 2)
  expect_equal(nrow(repr), 2)
})

test_that("select_features", {
  method <- mgr_init("tsfresh")
  y <- sample(2, 100, replace = T)
  X <- matrix(c(y, sample(2, 100, replace = T)), ncol = 2, nrow = 100,
              dimnames = list(NULL, c("F1", "F2")))

  feat <- mgr_select_features(method, X, y, NA, 1)
  expect_equal(feat, "F1")
})

test_that("is_vectorized", {
  method <- mgr_init("tsfresh")
  expect_true(mgr_is_vectorized(method))
})
