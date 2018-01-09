# ctmm data processing ----

# get single animal info in one row data frame
report_tele <- function(object) {
  # some data have one record for some individual, diff will return numeric(0), then median got NULL
  diffs <- diff(object$t)
  sampling_interval <- ifelse(length(diffs) == 0,
                              0,
                              stats::median(diffs))
  sampling_range <- max(object$t) - min(object$t)
  # above work on t which is cleaned by ctmm. original timestamp could have missing values
  t_start <- min(object$timestamp, na.rm = TRUE)
  t_end <- max(object$timestamp, na.rm = TRUE)
  data.table(identity = object@info$identity,
             interval_s = sampling_interval,
             interval = format_seconds_f(sampling_interval)(sampling_interval),
             duration_s = sampling_range,
             duration = format_seconds_f(sampling_range)(sampling_range),
             sampling_start = t_start,
             sampling_end = t_end,
             start = format_datetime(t_start),
             end = format_datetime(t_end),
             points = nrow(object))
}
wrap_single_telemetry <- function(tele_obj){
  if (class(tele_obj) != "list") {
    # use same name so we can return same name if no change made
    tele_obj <- list(tele_obj)
    names(tele_obj) <- attr(tele_obj[[1]],"info")$identity
  }
  return(tele_obj)
}
# sort tele list by identity, ggplot always sort by id. ctmm keep same order in csv, but this should not create problem. actually I found the table is sorted from ctmm for old buffalo data 1764627, which is unsorted in csv.
# we should keep the list sorted, not the info table. info table order match original list because we need to use table index.
sort_tele_list <- function(tele_list) {
  tele_list[stringr::str_sort(names(tele_list))]
}
#' Report data summary on telemetry object/list
#'
#' @param tele_obj_list telemetry object or list
#'
#' @return An information `data.table` for input
#' @export
#'
report <- function(tele_obj_list){
  tele_list <- wrap_single_telemetry(tele_obj_list)
  animal_info_list <- lapply(tele_list, report_tele)
  rbindlist(animal_info_list)
}
#' Calculate distance to median center for each animal location
#'
#' If there are big gaps in sampling time, median center for each time group is
#' used. To reduce duplicate calculation, speed calculation will use some
#' columns created in distance calculation. Always update `data.table` with `calc_distance` first then use [calc_speed()].
#'
#' @param animals_dt location `data.table` from [merge_tele()]
#'
#' @return `data.table` with distance columns added.
#' @export
#'
calc_distance <- function(animals_dt) {
  find_boundary <- function(data) {
    time_gap_threshold <- stats::quantile((diff(data$t)), 0.8) * 100
    # increase 1 sec because interval boundry is right open [ )
    data[inc_t > time_gap_threshold, t + 1]
  }
  # function above need inc_t, speed calculation need inc_x, inc_y, inc_t. It's easier add these column in one pass here, that means we need to always calculate distance then speed.
  # x[i] + inc[i] = x[i+1], note by id
  animals_dt[, `:=`(inc_x = shift(x, 1L, type = "lead") - x,
                    inc_y = shift(y, 1L, type = "lead") - y,
                    inc_t = shift(t, 1L, type = "lead") - t), by = id]
  animals_dt[, group_index := paste0(identity, "_",
                                     findInterval(t, find_boundary(.SD))),
             by = identity]
  animals_dt[, `:=`(median_x = median(x), median_y = median(y)),
             by = group_index]
  animals_dt[, distance_center := sqrt((x - median_x) ** 2 +
                                         (y - median_y) ** 2)]
  return(animals_dt)
}
# the naive definition of leaving speed. the NA cleaning is not ideal
calc_speed_leaving <- function(animals_dt, device_error) {
  animals_dt[, speed := sqrt(inc_x ^ 2 + inc_y ^ 2) / inc_t]
  animals_dt[is.infinite(speed), speed := NaN]
  # if last point n is outlier, n-1 will have high speed according to our definition, and n have no speed definition. assign n-1 speed to it. Then we don't need to clean up NA in speed too
  # this removed NaN too. The NA values caused speed outlier plot default subset to NULL. should not keep NA, NaN in speed, will cause too many troubles. could use negative value to mark
  for (i in animals_dt[is.na(speed), which = TRUE]) {
    animals_dt[i, speed := animals_dt[i - 1, speed]]
  }
  return(animals_dt)
}
# the pmin method
calc_speed_pmin <- function(animals_dt, device_error) {
  # TODO deal with dt==0 cases
  # dt == 0, use the sampling resolution to estimate the time difference
  # animals_dt[inc_t == 0]
  # sampling_resolution <- gcd_vec(diff(animals_dt$t))
  animals_dt[, speed := sqrt(inc_x ^ 2 + inc_y ^ 2) / inc_t]
  # TODO these NA cleaning are temporary. not sure which is optimal. histogram will remove infinite value but we may want to filter these points. since this is just fallback temp method and mainly rely on ctmm function, stop here now. probably should clean dup time first in separate method, maybe remove them to list with a warning. in my leaving speed just assign to point before it, which is not ideal but hide all problems.
  # animals_dt[is.na(speed)]
  animals_dt[is.infinite(speed), speed := NaN]
  animals_dt[, speed_min := pmin(speed, shift(speed, 1L), na.rm = TRUE), by = id]
  # the extended definition of pmin, take min(1->3, 1->2), min(N-2 ->N, N-1 ->N) speed for 1 and N. 1->2 and N-1 ->N are the previous defined value, the leaving speed of 1 and N-1. using x,y,t of next point - current point to get speed for current point.
  point_1 <- animals_dt[, .I[1], by = id]
  point_N <- animals_dt[, .I[.N], by = id]
  # animals_dt[c(point_1$V1, point_1$V1 + 2), shift(x, 1L, type = "lead") - x, by = id]
  animals_dt[c(point_1$V1, point_1$V1 + 2), `:=`(
    x_3_1 = shift(x, 1L, type = "lead") - x,
    y_3_1 = shift(y, 1L, type = "lead") - y,
    t_3_1 = shift(t, 1L, type = "lead") - t), by = id]
  animals_dt[point_1$V1, speed_min_n :=
               pmin(speed, sqrt(x_3_1 ^ 2 + y_3_1 ^ 2) / t_3_1,
                    na.rm = TRUE),
             by = id]
  # for N we are using N, N-2 order so still using lead, note order of t
  animals_dt[c(point_N$V1, point_N$V1 - 2), `:=`(
    x_N_2 = x - shift(x, 1L, type = "lead"),
    y_N_2 = y - shift(y, 1L, type = "lead"),
    t_N_2 = t - shift(t, 1L, type = "lead")), by = id]
  animals_dt[point_N$V1, speed_min_n :=
               pmin(speed, sqrt(x_N_2 ^ 2 + y_N_2 ^ 2) / t_N_2,
                    na.rm = TRUE),
             by = id]
  # View(animals_dt[c(point_1$V1, point_1$V1 + 2, point_N$V1, point_N$V1 - 2), c(1:13, 17:25), with = TRUE][order(id)])
  # to accompany extended definition, use speed_min first, now replace the speed column.
  animals_dt[, speed := speed_min]
  animals_dt[c(point_1$V1, point_N$V1), speed := speed_min_n]
  animals_dt[, c("speed_min", "x_3_1", "y_3_1", "t_3_1", "speed_min_n",
                 "x_N_2", "y_N_2", "t_N_2") := NULL]
  return(animals_dt)
}
# using ctmm util functions
calc_speed_ctmm <- function(animals_dt, device_error) {
  setkey(animals_dt, row_no)
  animals_dt[, speed := ctmm:::assign_speeds(.SD,
                                             dt = ctmm:::time_res(.SD),
                                             UERE = device_error, method = "max"),
             by = identity]
  return(animals_dt)
}
#' Calculate speed for each animal location
#'
#' It's difficult to get a simple speed definition yet robust to all kinds of
#' dirty data cases. A sophisticated method is attempt first which should cover
#' most common edge cases reasonably well. When it fails for extreme situations,
#' the function will fall back to simpler method which is more naive but robust.
#'
#' To reduce duplicate calculation, speed calculation will use some columns
#' created in distance calculation. Always update `data.table` with `calc_distance` first then use [calc_speed()].
#'
#' @param animals_dt telemetry data in merged data.table
#' @param device_error device error if available
#'
#' @return data.table with speed columns added.
#' @export
#'
calc_speed <- function(animals_dt, device_error = 0) {
  setkey(animals_dt, row_no)
  # my speed calculation need distance columns
  test_calc <- function(data, device_error, fun, fun_bak) {
    res <- tryCatch(fun(data, device_error), error = function(e) "error")
    if (identical(res, "error")) {
      res <- fun_bak(data, device_error)
      cat(crayon::red("Had error with first speed definition, use alternative instead\n"))
    }
    return(res)
  }
  # we didn't use the third fallback, pmin should be robust enough.
  animals_dt <- test_calc(animals_dt, device_error,
                          calc_speed_ctmm, calc_speed_pmin)
  # animals_dt <- calc_speed_ctmm(animals_dt)
  return(animals_dt)
}
# merge tele obj/list into data.table with identity column, easier for ggplot and summary. go through every obj to get data frame and metadata, then combine the data frame into data, metadata into info.
# assuming row order by timestamp and identity in same order with tele obj.
tele_list_to_dt <- function(tele_obj_list) {
  tele_list <- wrap_single_telemetry(tele_obj_list)
  animal_count <- length(tele_list)
  animal_data_list <- vector(mode = "list", length = animal_count)
  for (i in 1:animal_count) {
    animal_data_list[[i]] <- data.table(data.frame(tele_list[[i]]))
    animal_data_list[[i]][, identity := tele_list[[i]]@info$identity]
    animal_data_list[[i]][, row_name := row.names(tele_list[[i]])]
  }
  # some animals could have different extra columns. need fill to maintain the data frame, but the telelist should be clean objs.
  animals_data_dt <- rbindlist(animal_data_list, fill = TRUE)
  # ggplot color need a factor column. if do factor in place, legend will have factor in name
  animals_data_dt[, id := factor(identity)]
  animals_data_dt[, row_no := .I]
  any_dup <- anyDuplicated(animals_data_dt, by = "row_name")
  if (any_dup != 0) {
    message("duplicated row name found:\n", animals_data_dt[any_dup])
  }
  # animals_data_dt <- calc_distance(animals_data_dt)
  # animals_data_dt <- calc_speed(animals_data_dt)
  return(animals_data_dt)
}
#' Generate merged location and info `data.table` from telemetry object/list
#'
#' A Telemetry list hold mutiple animal data in separate list items, each item
#' have the animal location data in a data frame, and other information in
#' various slots. This structure supports flexible S3 methods for telemetry
#' object. However to plot multiple animals location together with `ggplot2` we
#' need to merge all location data into a single `data.frame` with an animal id
#' column.
#'
#' This function merge any input telemetry object/List into a `data.table` of
#' location data, and another information `data.table` for animals. `data.table`
#' is chosen over `data.frame` for much better performance. This data structure
#' is also used in a lot of places in app, which works on any selected subset of
#' full data in almost all steps.
#'
#' @param tele_obj_list telemetry object/list
#'
#' @return list of
#' - `data`: all animals merged in one data.table
#' - `info`: animal information table
#' @export
merge_tele <- function(tele_obj_list) {
  return(list(data = tele_list_to_dt(tele_obj_list),
              info = report(tele_obj_list)))
}
# to test if tele_list is in sync with merged data.
# this will not work when there are NA cols introduced by merge with different cols. need to clean those cols first
match_tele_merged <- function(tele_list, merged) {
  req(!is.null(tele_list) | (!is.null(merged)))
  # verify info match dt, this is not garranted since sometimes they are processed separately
  stopifnot(all.equal(merged$info, report(tele_list)))
  # verify data part matches, also the name must match
  dt_list <- split(merged$data, by = "identity")
  lapply(names(tele_list), function(x) {
    stopifnot(all.equal(dt_list[[x]][, timestamp:y],
                        data.table(data.frame(tele_list[[x]]))))
  })
  print("data consistency verified")
}
# ctmm:::extent take telemetry objects. previously I just use the original object in input together with the data frame version. Because we may apply filter/subset (remove outliers) on the data frame, then the input version become outdated. maintaining a matching telemetry object become cubersome. Since the only dependency on telemetry obj is here, I just modify the extent.telemetry function to take the data frame. need to make sure it follow the changes in ctmm::extent. https://github.com/ctmm-initiative/ctmm/blob/master/R/extent.R#L22
extent_dt <- function(animals_dt, level = 1, ...) {
  alpha <- (1-level)/2
  ranges <- animals_dt[, as.list(c(
    stats::quantile(x,probs=c(alpha,1-alpha)),
    stats::quantile(y,probs=c(alpha,1-alpha)))),
    by = "identity"]
  setnames(ranges, c("identity", "min_x", "max_x", "min_y", "max_y"))
  return(ranges)
}
get_ranges_quantile_dt <- function(animals_dt, level) {
  # no padding to avoid points already filtered by quantile appear in plot when the axes expanded, since we are only "filter" points by changing x y limit instead of removing points.
  ranges <- extent_dt(animals_dt, level)
  x_max_diff_half <- max(ranges[, max_x - min_x]) / 2
  y_max_diff_half <- max(ranges[, max_y - min_y]) / 2
  # need to filter data frame too otherwise the middle point is off. merging limits to every row make the filtering is easier.
  animals_dt_with_range <- merge(animals_dt, ranges, by = "identity")
  animals_updated <- animals_dt_with_range[x >= min_x & x <= max_x &
                                             y >= min_y & y <= max_y]
  dt <- animals_updated[, .(middle_x = (max(x) + min(x)) / 2,
                            middle_y = (max(y) + min(y)) / 2),
                        by = identity]
  dt[, x_start := middle_x - x_max_diff_half]
  dt[, x_end := middle_x + x_max_diff_half]
  dt[, y_start := middle_y - y_max_diff_half]
  dt[, y_end := middle_y + y_max_diff_half]
  return(dt)
}
