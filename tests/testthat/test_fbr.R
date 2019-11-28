context("fbr")

test_that("dec_rec", {
  t1 <- as.POSIXct("2010-01-01", tz = "GMT")
  tT <- as.POSIXct("2012-01-01", tz = "GMT")
  ts <- seq(t1, tT, by = "hour")
  TT <- length(ts)

  season_day <- rep(rnorm(24), ceiling(TT / 24))[1:TT]
  season_week <- rep(rep(rnorm(7), each = 24), ceiling(TT / 24 / 7)) [1:TT]
  season_year <- rep(rep(rnorm(12), each = 730), 3) [1:TT]
  x <- season_day + season_week + season_year

  method <- mgr_init("fbr")
  method$frequency <- 24

  ymd <- strftime(ts, "%Y-%m-%d", tz = "GMT")
  method$ymdf <- as.factor(ymd)
  method$ymdc <- unname(table(ymd))

  ym <- strftime(ts, "%Y-%m", tz = "GMT")
  method$ymf <- as.factor(ym)
  method$ymc <- unname(table(ym))

  method$w_year <- T

  dec <- mgr_dec(method, x)

  expect_equal(length(dec$season_day), TT)
  expect_equal(length(dec$season_week),  TT)
  expect_equal(length(dec$season_year), TT)
  expect_equal(length(dec$trend), TT)
  expect_equal(length(dec$residuals), TT)

  expect_equal(length(dec$season_1), 24)
  expect_equal(length(dec$season_2), 7)
  expect_equal(length(dec$season_3), 12)

  expect_equal(length(dec$phi_1), 1)
  expect_equal(length(dec$phi_2), 1)

  red <- mgr_red(method, dec)
  expect_equal(length(red), 49)

  method$w_mask <- F
  red <- mgr_red(method, dec)
  expect_equal(length(red), 4)

  method$w_strength <- T
  red <- mgr_red(method, dec)
  expect_equal(length(red), 8)

  method$w_mask <- T
  red <- mgr_red(method, dec)
  expect_equal(length(red), 53)

})

test_that("dec_rec_wo_year", {
  t1 <- as.POSIXct("2010-01-01", tz = "GMT")
  tT <- as.POSIXct("2011-01-01", tz = "GMT")
  ts <- seq(t1, tT, by = "hour")
  TT <- length(ts)

  season_day <- rep(rnorm(24), ceiling(TT / 24))[1:TT]
  season_week <- rep(rep(rnorm(7), each = 24), ceiling(TT / 24 / 7)) [1:TT]
  season_year <- rep(rep(rnorm(12), each = 730), 3) [1:TT]
  x <- season_day + season_week + season_year

  method <- mgr_init("fbr")
  method$frequency <- 24

  ymd <- strftime(ts, "%Y-%m-%d", tz = "GMT")
  method$ymdf <- as.factor(ymd)
  method$ymdc <- unname(table(ymd))

  ym <- strftime(ts, "%Y-%m", tz = "GMT")
  method$ymf <- as.factor(ym)
  method$ymc <- unname(table(ym))

  method$w_year <- F

  dec <- mgr_dec(method, x)

  expect_equal(length(dec$season_day), TT)
  expect_equal(length(dec$season_week),  TT)
  expect_true(!"season_year" %in% names(dec))
  expect_equal(length(dec$trend), TT)
  expect_equal(length(dec$residuals), TT)

  expect_equal(length(dec$season_1), 24)
  expect_equal(length(dec$season_2), 7)
  expect_true(!"season_3" %in% names(dec))

  expect_equal(length(dec$phi_1), 1)
  expect_equal(length(dec$phi_2), 1)

  red <- mgr_red(method, dec)
  expect_equal(length(red), 37)

  method$w_mask <- F
  red <- mgr_red(method, dec)
  expect_equal(length(red), 4)

  method$w_strength <- T
  red <- mgr_red(method, dec)
  expect_equal(length(red), 7)

  method$w_mask <- T
  red <- mgr_red(method, dec)
  expect_equal(length(red), 40)
})

test_that("distance", {
  method <- mgr_init("fbr")
  x <- sample(5, 5)
  y <- sample(5, 5)
  d <- mgr_distance(method, x, y)

  expect_lt(d - sqrt(sum((x - y)^2)), .Machine$double.eps)
})

test_that("select_features", {
  method <- mgr_init("fbr")
  y <- sample(c(2, 3, 4), 1000, replace = T)
  foo <- sample(c(2, 3, 4), 1000, replace = T)
  sq <- sqrt(y)
  sqf <- sq * 2
  X <- data.frame(sq = sq, sqf = sqf, foo = foo)

  feat <- mgr_select_features(method, X, y, 1, NA)

  expect_equal(feat, "sqf")
})

test_that("select_features cfs", {
  method <- mgr_init("fbr")
  method$fs <- "cfs"
  df <- iris
  y <- as.factor(df[, ncol(df)])
  X <- as.matrix(df[, -ncol(df)])

  feat <- mgr_select_features(method, X, y, 5, NA)
  expect_true(all(feat %in% names(iris)[1:4]))

  feat <- mgr_select_features(method, X, y, 1, NA)
  expect_true(all(feat %in% names(iris)[1:4]))
  expect_equal(length(feat), 1)
})

test_that("is_vectorized", {
  method <- mgr_init("fbr")
  expect_true(!mgr_is_vectorized(method))
})
