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
mgr_dec <- function(method, x) {
  dec(method, x)
}

#' @export
mgr_red <- function(method, x) {
  red(method, x)
}

#' @export
mgr_distance <- function(method, x, y, ...) {
  stopifnot(length(x) == length(y))

  distance(method, x, y, ...)
}

#' @export
mgr_is_vectorized <- function(method) {
  is_vectorized(method)
}
