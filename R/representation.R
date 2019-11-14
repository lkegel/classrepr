init <- function(method) UseMethod("init")
init.default <- function(method) stop("Unknown class")

get_config <- function(method) UseMethod("get_config")
get_config.default <- function(method) {
  unclass(method)
}

set_config <- function(method, config) UseMethod("set_config")
set_config.default <- function(method, config) {
  stopifnot(is.config(config))

  stopifnot(all(names(config) %in% names(method)))

  for (name in names(config)) {
    value <- config[[name]]
    if (is.null(value)) {
      stop()
    } else if (is.list(value)) {
      # do nothing
    } else if (any(is.na(value)) |  any(is.nan(value))) {
      stop()
    }
    method[[name]] <- config[[name]]
  }

  method
}

dec <- function(method, x, num_cores) UseMethod("dec")
dec.default <- function(method, x, num_cores) stop("Unknown class")

red <- function(method, x, num_cores) UseMethod("red")
red.default <- function(method, x, num_cores) stop("Unknown class")

distance <- function(method, x, y, ...) UseMethod("distance")
distance.default <- function(method, x, y, ...) stop("Unknown class")

is_vectorized <- function(method) UseMethod("is_vectorized")
is_vectorized.default <- function(method) stop("Unknown class")
