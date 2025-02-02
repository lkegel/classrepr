#' @export
mgr_init <- function(method_name) {
  method <- structure(list(), class = method_name)

  init(method)
}

#' @export
mgr_get_config <- function(method) {
  get_config(method)
}

#' @export
mgr_set_config <- function(method, config) {
  if (!is.list(config)) {
    stop("Configuration have to be a list.")
  }

  set_config(method, config)
}

#' @export
mgr_dec <- function(method, x, num_cores = 1) {
  dec(method, x, num_cores)
}

#' @export
mgr_red <- function(method, x, num_cores = 1) {
  red(method, x, num_cores)
}

#' @export
mgr_select_features <- function(method, X, y, k, num_cores = 1) {
  select_features(method, X, y, k, num_cores)
}

#' @export
mgr_distance <- function(method, x, y, ...) {
  distance(method, x, y, ...)
}

#' @export
mgr_is_vectorized <- function(method) {
  is_vectorized(method)
}
