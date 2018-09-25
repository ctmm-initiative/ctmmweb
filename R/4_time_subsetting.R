# divide x into interval_count intervals ----
# needed in time subsetting
# Taken from https://github.com/wch/r-source/blob/trunk/src/library/base/R/cut.R
divide <-
  function(x, interval_count)
  {
    if (is.na(interval_count) || interval_count < 2L)
      stop("invalid number of intervals")
    nb <- as.integer(interval_count + 1) # one more than #{intervals}
    dx <- diff(rx <- range(x, na.rm = TRUE))
    if (dx == 0) {
      dx <- abs(rx[1L])
      breaks <- seq.int(rx[1L] - dx/1000, rx[2L] + dx/1000,
                        length.out = nb)
    } else {
      breaks <- seq.int(rx[1L], rx[2L], length.out = nb)
      breaks[c(1L, nb)] <- c(rx[1L] - dx/1000, rx[2L] + dx/1000)
    }
    return(breaks)
  }
cut_date_time <- function(x, interval_count) {
  brks <- divide(as.numeric(x), interval_count)
  return(cut(x, lubridate::as_datetime(brks)))
}
divide_date_time <- function(x, interval_count) {
  return(lubridate::as_datetime(divide(as.numeric(x), interval_count)))
}
