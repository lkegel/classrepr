init.dwt <- function(method) {
  method$filter <- "haar"

  return(method)
}

dec.dwt <- function(method, x, num_cores) {
  return(x)
}

#' @import wavelets
red.dwt <- function(method, x, num_cores) {
  repr <- rev(unlist(wavelets::dwt(x, filter = method$filter)@W))

  return(repr)
}

select_features.dwt <- function(method, X, y, k, num_cores) {
  means <- apply(X, 2, function(x) mean(x^2))
  means <- sort(means, decreasing = T)
  result <- names(means)
  result <- result[1:min(k, length(result))]

  return(result)
}

distance.dwt <- function(method, x, y, bsf) {
  return(d_qed(x, y, length(x), bsf))
}

is_vectorized.dwt <- function(method) return(F)
