init.rld <- function(method) {
  return(method)
}

dec.rld <- function(method, x, num_cores) {
  return(x)
}

red.rld <- function(method, x, num_cores) {
  rld <- red_rld(x)[2, ]
  names(rld) <- paste0("V", seq_along(rld))
  return(rld)
}

#' @import dtw
distance.rld <- function(method, x, y) {
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]

  return(dtw::dtw(y, x)$distance)
}

is_vectorized.rld <- function(method) return(F)
