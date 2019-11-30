init.raw <- function(method) {
  return(method)
}

dec.raw <- function(method, x, num_cores) {
  return(x)
}

red.raw <- function(method, x, num_cores) {
  return(x)
}

distance.raw <- function(method, x, y, bsf) {
  return(d_qed(x, y, length(x), bsf))
}

is_vectorized.raw <- function(method) return(F)
