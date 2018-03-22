# unit formatting ----
# function with _f postfix generate a unit_format function, which can be used in ggplot scales. To generate formated values call function on input again
# generate a unit_format function with picked unit. This only take single value, the wrappers will take a vector, pick test value and concise parameter according to data type then apply to whole vector
# need to round up digits otherwise DT is showing too many digits
# hardcoded values are round to 4 digits, print 2 digits.
# scales::unit_format is just this. we can have better control when we can round numbers first.
unit_format_round <- function(unit = "m", scale = 1, sep = " ", ...){
  function(x){
    paste(scales::comma(round(x * scale, 4), ...), unit, sep = sep)
  }
}
# almost all format is applied to a vector so they will have same unit
# given a vector of values in SI unit (also work with single value), pick the best unit, return unit name and scale. SI / scale get the new value. note the offical name of dimension: length for distance
# we almost always want concise except seconds was changed to sec, which are not what I wanted. use fixed concise setting for now
pick_unit <- function(vec, dimension){
  switch(dimension,
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
  unit_format_round(unit = best_unit$name, scale = 1 / best_unit$scale,
                    digits = 2)
}
# we have general functions now, but it's very cumbersome to replace all old usage with new function with additional parameter (simple replace will not work), and we sometimes need a specific function as parameter color_bin_break. so just write wrappers here. this is currying but we don't want additional dependency for functional for this simple usage
# when using pick unit do the convert in dt, need to round digits, this was taken care of by digits parameter in format functions
pick_unit_distance <- function(vec) { pick_unit(vec, "length") }
pick_unit_seconds  <- function(vec) { pick_unit(vec, "time") }
pick_unit_speed    <- function(vec) { pick_unit(vec, "speed") }
pick_unit_area     <- function(vec) { pick_unit(vec, "area") }
format_distance_f <- function(vec) { format_unit_f(vec, "length")}
format_seconds_f  <- function(vec) { format_unit_f(vec, "time") }
format_speed_f    <- function(vec) { format_unit_f(vec, "speed") }
format_area_f     <- function(vec) { format_unit_f(vec, "area") }
# given a resprentative value and unit dimension, get the best unit then generate a format function to be used on a vector of similar values. other derived functions pick certain value from a vector and choose a dimension.
# nsmall = 2 will cause 60 mins become 60.00
# pick_best_unit_f <- function(test_value, dimension, concise) {
#   best_unit <- ctmm:::unit(test_value, dimension, thresh = 1, concise = concise)
#   unit_format_round(unit = best_unit$name, scale = 1 / best_unit$scale,
#                     digits = 2)
# }
# function will take vector as input, but only return a format function which is good for scales in ggplot. will need to apply to vector again if need the formated result.
# format_distance_f <- function(v){
#   # didn't use median because it could be near zero with positive and negative values
#   pick_best_unit_f(max(abs(v), na.rm = TRUE)/2, dimension = "length",
#                    concise = TRUE)
# }
# given a test value, pick unit, return scale and name. SI / scale get the new value
# pick_unit_distance <- function(v) {
#   ctmm:::unit(max(abs(v), na.rm = TRUE)/2, dimension = "length",
#               concise = TRUE)
# }
# format_seconds_f <- function(secs) {
#   pick_best_unit_f(median(secs, na.rm = TRUE), dimension = "time",
#                    concise = FALSE)
# }
# note we cannot use format_speed_f(vec)(vec) for value in data.table, which may call the function for single value and lost context in whole vector.
# format_speed_f <- function(speed) {
#   pick_best_unit_f(median(speed, na.rm = TRUE), dimension = "speed",
#                    concise = TRUE)
# }
# pick_unit_speed <- function(speed) {
#   ctmm:::unit(median(speed, na.rm = TRUE), dimension = "speed",
#               concise = TRUE)
# }
# format_area_f <- function(area) {
#   pick_best_unit_f(median(area, na.rm = TRUE), dimension = "area",
#                    concise = TRUE)
# }
# special formats for time ----
# intended for single input
format_diff_time <- function(diff_t) {
  diff_t_secs <- as.numeric(diff_t, units = "secs")
  format_unit_f(diff_t_secs, "time")(diff_t_secs)
  # u_fun <- pick_best_unit_f(diff_t_secs,
  #                           dimension = "time", concise = FALSE)
  # u_fun(diff_t_secs)
}
# this can take vector input
format_datetime <- function(datetime) {
  format(datetime, "%Y-%m-%d %H:%M")
}
