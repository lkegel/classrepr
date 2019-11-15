init.tsfresh <- function(method) {
  method$ti <- NA

  return(method)
}

dec.tsfresh <- function(method, x, num_cores) {
  return(x)
}

#' @import jsonlite
red.tsfresh <- function(method, x, num_cores) {
  I <- nrow(x)
  TT <- ncol(x)

  df <- data.frame(id = rep(seq(I), each = TT),
                   time = rep(as.character(method$ti), I),
                   value = as.numeric(t(x)))
  js <- jsonlite::toJSON(df)

  fp <- file.path(find.package("classrepr"), "exec", "tsfresh-red.py")
  fp_res <- tempfile(fileext = ".csv")
  fp_out <- tempfile(fileext = ".txt")
  if (.Platform$OS.type == "unix") {
    cmd <- "python3"
    args <- paste(fp, fp_res, num_cores)
  } else if (.Platform$OS.type == "windows") {
    cmd <- "py"
    args <- paste("-3", fp, fp_res, num_cores)
  }
  print(paste("Temp file tsfresh:", fp_out))
  res <- system2(cmd, args, input = js)
  repr <- read.csv(fp_res, colClasses = "numeric")
  stopifnot(unname(repr[, 1]) == seq(I))
  repr <- repr[, -1]

  unlink(fp_out)
  unlink(fp_res)

  return(repr)
}

is_vectorized.tsfresh <- function(method) return(T)
