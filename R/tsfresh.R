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

select_features.tsfresh <- function(method, X, y, k, num_cores) {
  I <- nrow(X)
  df <- as.data.frame(X)
  df <- cbind(df, y = y)

  fp <- file.path(find.package("classrepr"), "exec", "tsfresh-select.py")
  fp_out <- tempfile(fileext = ".csv")
  fp_in <- tempfile(fileext = ".csv")
  write.table(df, fp_in, row.names = F, sep = ",", quote = FALSE)
  if (.Platform$OS.type == "unix") {
    cmd <- "python3"
    args <- paste(fp, fp_in, fp_out, num_cores)
  } else if (.Platform$OS.type == "windows") {
    cmd <- "py"
    args <- paste("-3", fp, fp_in, fp_out, num_cores)
  }
  res <- system2(cmd, args)
  feat <- read.csv(fp_out, colClasses = "numeric", row.names = NULL)
  feat <- feat[, -1, drop = FALSE]
  unlink(fp_in)
  unlink(fp_out)

  return(names(feat))
}

distance.tsfresh <- function(method, x, y) {
  return(d_ed(x, y, length(x)))
}

is_vectorized.tsfresh <- function(method) return(T)
