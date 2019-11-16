init.fbr <- function(method) {
  method$w_strength <- F
  method$w_mask <- T

  method$frequency <- NA
  method$ymdf <- NA
  method$ymdc <- NA
  method$ymf <- NA
  method$ymc <- NA
  method$w_year <- NA

  return(method)
}

dec.fbr <- function(method, x, num_cores) {
  TT <- length(x)

  # Day
  ts <- ts(x, frequency = method$frequency)
  fit_day <- stl(ts, s.window = "per")
  season_1 <- fit_day$time.series[, "seasonal"][1:method$frequency]
  names(season_1) <- paste0("season_1_", 1:method$frequency)
  season_day <- as.numeric(fit_day$time.series[, "seasonal"])
  remainder <- x - season_day

  # Week
  means <- unlist(lapply(unname(split(remainder, method$ymdf)), mean))
  ts <- ts(means, frequency = 7)
  fit_last <- fit_week <- stl(ts, s.window = "per")
  season_2 <- fit_week$time.series[, "seasonal"][1:7]
  names(season_2) <- paste0("season_2_", 1:7)
  season_week <- unlist(mapply(rep, fit_week$time.series[, "seasonal"],
                               method$ymdc, SIMPLIFY = F))
  remainder <- remainder - season_week
  residuals <- x - season_week - season_day
  counts <- method$ymdc

  # Year
  if (method$w_year) {
    means <- unlist(lapply(unname(split(remainder, method$ymf)), mean))
    ts <- ts(means, frequency = 12)
    fit_last <- fit_year <- stl(ts, s.window = "per")
    season_3 <- fit_week$time.series[, "seasonal"][1:12]
    names(season_3) <- paste0("season_3_", 1:12)
    season_year <- unlist(mapply(rep, fit_year$time.series[, "seasonal"],
                                 method$ymc, SIMPLIFY = F))
    counts <- method$ymc

    residuals <- residuals - season_year
  } else {
    season_3 <- NA
    season_year <- NA
  }

  # Trend
  trend <- unlist(mapply(rep, fit_last$time.series[, "trend"], counts,
                         SIMPLIFY = F))
  times <- seq(0, length(trend) - 1)
  m <- matrix(c(times, rep(1, TT)), ncol = 2, nrow = TT, byrow = F)
  fit2 <- lm.fit(m, trend)
  # fit <- lm(trend ~ times)
  phi_1 <- unname(fit2$coefficients[2])
  phi_2 <- unname(fit2$coefficients[1])

  # Residuals
  residuals <- residuals - trend

  if (method$w_year) {
    result <- list(phi_1 = phi_1,
                   phi_2 = phi_2,
                   season_1 = season_1,
                   season_2 = season_2,
                   season_3 = season_3,
                   trend = trend,
                   season_day = season_day,
                   season_week = season_week,
                   season_year = season_year,
                   residuals = residuals)
  } else {
    result <- list(phi_1 = phi_1,
                   phi_2 = phi_2,
                   season_1 = season_1,
                   season_2 = season_2,
                   trend = trend,
                   season_day = season_day,
                   season_week = season_week,
                   residuals = residuals)
  }

  return(result)
}

strength <- function(res, comp) {
  1 - var(res) / var(res + comp)
}

acf1 <- function(x) {
  return(acf(x, plot = F)$acf[,,1][2])
}

#' @import moments
red.fbr <- function(method, x, num_cores) {
  result <- list()

  if (method$w_strength) {
    result[["tr"]] <- strength(x$residuals, x$trend)
    result[["day"]] <- strength(x$residuals, x$season_day)
    result[["week"]] <- strength(x$residuals, x$season_week)
    if (method$w_year) result[["year"]] <- strength(x$residuals, x$season_year)
  }

  result[["sd"]] <- var(x$residuals)
  result[["skew"]] <- moments::skewness(x$residuals)
  result[["kurt"]] <- moments::kurtosis(x$residuals)
  result[["acf1"]] <- acf1(x$residuals)

  if (method$w_mask) {
    result[["theta_1"]] = x$phi_1
    result[["theta_2"]] = x$phi_2
    result <- c(result, as.list(x$season_1))
    result <- c(result, as.list(x$season_2))
    if (method$w_year) result <- c(result, as.list(x$season_3))
  }

  return(unlist(result))
}

#' @import caret
select_features.fbr <- function(method, X, y, k, num_cores) {
  # Correlation-based Feature Selection
  corr_matrix <- cor(X)
  high_corr <- caret::findCorrelation(corr_matrix, cutoff = 0.75)
  X <- X[, -high_corr, drop = F]

  # Random Forest
  sizes <- seq(min(ncol(X), k))
  control <- caret::rfeControl(functions = caret::rfFuncs, method = "cv", number = 10)
  results <- caret::rfe(X, as.factor(y), sizes = sizes, rfeControl=control, metric = "Accuracy")

  result <- predictors(results)

  return(result)
}

distance.fbr <- function(method, x, y) {
  return(d_ed(x, y, length(x)))
}

is_vectorized.fbr <- function(method) return(F)

