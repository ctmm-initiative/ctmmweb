# unit formatting ----
# function with _f postfix generate a unit_format function, which can be used in ggplot scales. To generate formated values call function on input again
# generate a unit_format function with picked unit. This only take single value, the wrappers will take a vector, pick test value and concise parameter according to data type then apply to whole vector
# scales::unit_format is just this. we can have better control when we can round numbers first.
# we can transfer format parameters in ..., however nsmall is for minimum, this will make 60 to 60.00. combining digits and nsmall can make 23.98798 to 23.98. it's easier just to use round instead of format to always give 2 digits after decimal place. use 2 digits now.
# this create a text string, mainly used in ggplot code. table format use pick_unit instead, and rounding happened in format_dt. use different name other than scale to separate with ctmm:::unit scale value which is reversed
unit_format_round <- function(unit = "m", upscale = 1, sep = " ", ...){
  function(x){
    # comma use accuracy=1 (used to be digits) to round. we may not need comma in big numbers anyway. no longer use digits/accuracy in calling this function, do round afterwards by need
    # paste(scales::comma(round(x * scale, 2), ...), unit, sep = sep)
    paste(round(x * upscale, 2), unit, sep = sep)
  }
}
# almost all format is applied to a vector so they will have same unit
# given a vector of values in SI unit (also work with single value), pick the best unit, return unit name and scale. SI / scale get the new value. note the offical name of dimension: length for distance
# we almost always want concise except seconds was changed to sec, which are not what I wanted. use fixed concise setting for now
# pick the right represental value to pick unit. There could be large difference in ML/low/high value, previous code took median may get 0 and use smallest unit, then high value become too big in small unit(issue 78). Now using this code to pick represental value specifically for CI cols. Because of our function structure, this need to be put in pick_unit.
# 0 bring no information in unit detection as 0 can be in any unit, and lots of 0 skew the median value, if median became 0, units in ctmm will think it as a small value and choose smallest unit. Just exclude 0 in vec before median should be better.
pick_unit <- function(vec, dimension){
  # exclude 0 from vec. not just strictly 0, also very small values. It isn't really better to use 1e-10 ms compare to 1e-13 s.
  # length can be negative with x in other side. take abs value for all. otherwise it can mess up the not_too_small comparison, the median calculation.
  vec <- abs(vec)
  not_too_small <- (vec > 1e-9)
  # sometimes there is no value left, and na.rm removed NA, so max working on empty. let's remove na first, if nothing left, use default standard unit, which can be done by assign 1 as test value
  vec <- vec[not_too_small]
  vec <- vec[!is.na(vec)]
  if (length(vec) == 0) {
    test_value <- 1
  } else {
      test_value <- stats::median(vec)
  }
  # using this because some dimension may need different treatment on concise.
  switch(dimension,
         length = unit(test_value, dimension = "length", concise = TRUE),
         time =   unit(test_value, dimension = "time", concise = FALSE),
         speed =  unit(test_value, dimension = "speed", concise = TRUE),
         area =   unit(test_value, dimension = "area", concise = TRUE),
         diffusion =   unit(test_value, dimension = "diffusion", concise = TRUE)
         )
}
# given a vector of values (or single value) and dimension, return a formatting function. many plot or render code need a function
format_unit_f <- function(vec, dimension) {
  best_unit <- pick_unit(vec, dimension)
  unit_format_round(unit = best_unit$name, upscale = 1 / best_unit$scale)
}
# we have general functions now, but it's very cumbersome to replace all old usage with new function with additional parameter (simple replace will not work), and we sometimes need a specific function as parameter color_bin_break. so just write wrappers here. this is currying but we don't want additional dependency for functional for this simple usage. the defintion looks too simple but we need function in some places, just write pick_unit with a parameter is not enough, will need curry again.
# when using pick unit do the convert in dt, need to round digits, this was taken care of by digits parameter in format functions
# pick_unit calculate the name and scale. format function can convert values to strings.
pick_unit_distance <- function(vec) { pick_unit(vec, "length") }
pick_unit_seconds  <- function(vec) { pick_unit(vec, "time") }
pick_unit_speed    <- function(vec) { pick_unit(vec, "speed") }
pick_unit_speed_kmh <- function(vec) { list(scale = 1000/3600, name = "km/hour") }
pick_unit_speed_ms <- function(vec) { list(scale = 1, name = "m/s") }
pick_unit_area     <- function(vec) { pick_unit(vec, "area") }
pick_unit_diffusion     <- function(vec) { pick_unit(vec, "diffusion") }
format_distance_f <- function(vec) { format_unit_f(vec, "length")}
format_seconds_f  <- function(vec) { format_unit_f(vec, "time") }
format_speed_f    <- function(vec) { format_unit_f(vec, "speed") }
# speed outlier page need fixed unit for easier relating to reality. define a fixed unit speed funciton. ctmm:::unit only defined distance per day, no m/s or km/hour. we replace it with unit name and scale
format_speed_kmh   <- function(vec) {
  # note upscale is reversed with unit scale value
  unit_format_round(unit = "km/hour", upscale = 3600/1000)
}
format_speed_ms   <- function(vec) {
  unit_format_round(unit = "m/s", upscale = 1)
}
format_area_f     <- function(vec) { format_unit_f(vec, "area") }
format_diffusion_f     <- function(vec) { format_unit_f(vec, "diffusion") }
# note we cannot use format_speed_f(vec)(vec) for value in data.table, which may call the function for single value and lost context in whole vector.
# special formats for time ----
# intended for single input
format_diff_time <- function(diff_t) {
  diff_t_secs <- as.numeric(diff_t, units = "secs")
  format_unit_f(diff_t_secs, "time")(diff_t_secs)
}
# this can take vector input
format_datetime <- function(datetime) {
  format(datetime, "%Y-%m-%d %H:%M")
}
