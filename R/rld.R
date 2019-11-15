init.rld <- function(method) {
  return(method)
}

dec.rld <- function(method, x, num_cores) {
  return(x)
}

red.rld <- function(method, x, num_cores) {
  return(red_rld(x)[2, ])
}

is_vectorized.rld <- function(method) return(F)
