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

is_vectorized.dwt <- function(method) return(F)
