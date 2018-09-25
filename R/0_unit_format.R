# unit formatting ----
# function with _f postfix generate a unit_format function, which can be used in ggplot scales. To generate formated values call function on input again
# generate a unit_format function with picked unit. This only take single value, the wrappers will take a vector, pick test value and concise parameter according to data type then apply to whole vector
# scales::unit_format is just this. we can have better control when we can round numbers first.
# we can transfer format parameters in ..., however nsmall is for minimum, this will make 60 to 60.00. combining digits and nsmall can make 23.98798 to 23.98. it's easier just to use round instead of format to always give 2 digits after decimal place. use 2 digits now.
# this create a text string, mainly used in ggplot code. table format use pick_unit instead, and rounding happened in format_dt
unit_format_round <- function(unit = "m", scale = 1, sep = " ", ...){
  function(x){
    # comma use accuracy=1 (used to be digits) to round. we may not need comma in big numbers anyway. no longer use digits/accuracy in calling this function, do round afterwards by need
    # paste(scales::comma(round(x * scale, 2), ...), unit, sep = sep)
    paste(round(x * scale, 2), unit, sep = sep)
  }
}
# almost all format is applied to a vector so they will have same unit
# given a vector of values in SI unit (also work with single value), pick the best unit, return unit name and scale. SI / scale get the new value. note the offical name of dimension: length for distance
# we almost always want concise except seconds was changed to sec, which are not what I wanted. use fixed concise setting for now
pick_unit <- function(vec, dimension){
  switch(dimension,
         # didn't use median because it could be near zero with positive and negative values
         length = ctmm:::unit(max(abs(vec), na.rm = TRUE)/2,
                              dimension = "length", concise = TRUE),
         time = ctmm:::unit(median(vec, na.rm = TRUE), dimension = "time",
                             concise = FALSE),
         speed = ctmm:::unit(median(vec, na.rm = TRUE), dimension = "speed",
                             concise = TRUE),
         area = ctmm:::unit(median(vec, na.rm = TRUE), dimension = "area",
                            concise = TRUE)
         )
}
# given a vector of values (or single value) and dimension, return a formatting function. many plot or render code need a function
format_unit_f <- function(vec, dimension) {
  best_unit <- pick_unit(vec, dimension)
  unit_format_round(unit = best_unit$name, scale = 1 / best_unit$scale)
}
# we have general functions now, but it's very cumbersome to replace all old usage with new function with additional parameter (simple replace will not work), and we sometimes need a specific function as parameter color_bin_break. so just write wrappers here. this is currying but we don't want additional dependency for functional for this simple usage
# when using pick unit do the convert in dt, need to round digits, this was taken care of by digits parameter in format functions
# pick_unit calculate the name and scale. format function can convert values to strings.
pick_unit_distance <- function(vec) { pick_unit(vec, "length") }
pick_unit_seconds  <- function(vec) { pick_unit(vec, "time") }
pick_unit_speed    <- function(vec) { pick_unit(vec, "speed") }
pick_unit_area     <- function(vec) { pick_unit(vec, "area") }
format_distance_f <- function(vec) { format_unit_f(vec, "length")}
format_seconds_f  <- function(vec) { format_unit_f(vec, "time") }
format_speed_f    <- function(vec) { format_unit_f(vec, "speed") }
format_area_f     <- function(vec) { format_unit_f(vec, "area") }
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
