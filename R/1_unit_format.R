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
# given a resprentative value and unit dimension, get the best unit then generate a format function to be used on a vector of similar values. other derived functions pick certain value from a vector and choose a dimension.
# nsmall = 2 will cause 60 mins become 60.00
pick_best_unit_f <- function(test_value, dimension, concise) {
  best_unit <- ctmm:::unit(test_value, dimension, thresh = 1, concise = concise)
  unit_format_round(unit = best_unit$name, scale = 1 / best_unit$scale,
                    digits = 2)
}
# function will take vector as input, but only return a format function which is good for scales in ggplot. will need to apply to vector again if need the formated result.
format_distance_f <- function(v){
  # didn't use median because it could be near zero with positive and negative values
  pick_best_unit_f(max(abs(v), na.rm = TRUE)/2, dimension = "length",
                   concise = TRUE)
}
# given a test value, pick unit, return scale and name. SI / scale get the new value
pick_unit_distance <- function(v) {
  ctmm:::unit(max(abs(v), na.rm = TRUE)/2, dimension = "length",
              concise = TRUE)
}
format_seconds_f <- function(secs) {
  pick_best_unit_f(median(secs, na.rm = TRUE), dimension = "time",
                   concise = FALSE)
}
# note we cannot use format_speed_f(vec)(vec) for value in data.table, which may call the function for single value and lost context in whole vector.
format_speed_f <- function(speed) {
  pick_best_unit_f(median(speed, na.rm = TRUE), dimension = "speed",
                   concise = TRUE)
}
pick_unit_speed <- function(speed) {
  ctmm:::unit(median(speed, na.rm = TRUE), dimension = "speed",
              concise = TRUE)
}
format_area_f <- function(area) {
  pick_best_unit_f(median(area, na.rm = TRUE), dimension = "area",
                   concise = TRUE)
}
# intended for single input
format_diff_time <- function(diff_t) {
  diff_t_secs <- as.numeric(diff_t, units = "secs")
  u_fun <- pick_best_unit_f(diff_t_secs,
                            dimension = "time", concise = FALSE)
  u_fun(diff_t_secs)
}
# this can take vector input
format_datetime <- function(datetime) {
  format(datetime, "%Y-%m-%d %H:%M")
}
