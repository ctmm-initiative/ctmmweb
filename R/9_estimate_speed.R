# simpler version of converting column with unit
format_dt_cols <- function(dt, col_name_vec, unit_picked,
                          digits = 3, unit_in_col_name = TRUE) {
  lapply(col_name_vec, function(col_name) {
    dt[, (col_name) := round(dt[[col_name]] / unit_picked$scale,
                             digits = digits)]
    if (unit_in_col_name) {
      setnames(dt, col_name, paste0(col_name, " (", unit_picked$name, ")"))
    }
  })
  return(dt)
}
# convert result list of speed calculation to dt, also calculate distance traveled. need to use the SI units, so calculate before unit conversion. since res is a list with different structure for error, it's easier to use separate vector input instead of a data.table.
speed_res_to_dt <- function(res, durations) {
  res_cleaned <- lapply(res, function(x) {
    if (class(x) == "try-error") {
      data.frame(rn = "error", low = Inf, ML = Inf, high = Inf)
    } else {
      data.table(x, keep.rownames = TRUE)
    }
  })
  dt <- rbindlist(res_cleaned)
  setnames(dt, "ML", "speed")
  dt[, duration := durations]
  dt[, distance_traveled := speed * duration]
  dt[, distance_traveled_low := low * duration]
  dt[, distance_traveled_high := high * duration]
  # format by unit, round values --
  # speed --
  # get unit name and ratio, update table manually. the existing format_dt_unit apply to each vector, that could result in different units for low and high.
  # pick unit apply to median, so we only need to look at speed column
  speed_unit_picked <- pick_unit_speed(dt$speed)
  format_dt_cols(dt, "speed", speed_unit_picked)
  format_dt_cols(dt, c("low", "high"), speed_unit_picked,
                 unit_in_col_name = FALSE)
  # combined CI column
  dt[, (paste0("speed CI (", speed_unit_picked$name, ")")) :=
       paste0("(", paste0(c(low, high), collapse = " – "), ")"),
     by = 1:nrow(dt)]
  # keep the low high column for plot
  dt[, c("rn") := NULL]
  # duration and distance --
  format_dt_cols(dt, "duration", pick_unit_seconds(dt$duration))
  # need to calculate them first otherwise column name changed
  distance_unit_picked <- pick_unit_distance(dt$distance_traveled)
  format_dt_cols(dt, c("distance_traveled_low", "distance_traveled_high"),
                 distance_unit_picked, unit_in_col_name = FALSE)
  dt[, (paste0("distance_traveled CI (", distance_unit_picked$name, ")")) :=
       paste0("(", paste0(c(distance_traveled_low,
                            distance_traveled_high), collapse = " – "), ")"),
     by = 1:nrow(dt)]
  format_dt_cols(dt, "distance_traveled", distance_unit_picked)
}
# find col name by pattern, add backtick to quote for future unquote. this works for ggplot aes_string, but dt don't need backtick
get_ticked_col_name <- function(names, pattern) {
  paste0("`", stringr::str_subset(names, pattern), "`")
}
