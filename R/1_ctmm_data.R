# ctmm data processing ----

# check tele obj or list, give proper message
assert_tele_list <- function(tele) {
  if (class(tele) != "list") {
    stop(crayon::bgRed$white(
      "Telemetry list is expected, see ?as_tele_list for details\n"))
  }
}

#' Coerce telemetry object to list
#'
#' [ctmm::as.telemetry()] will return will return single telemetry object
#' instead of a list when there is only one animal in data. To make code
#' consistent we always work with a list of telemetry objects. Use this function
#' over [ctmm::as.telemetry()] to make sure result is a proper list.
#'
#' @param tele result from [ctmm::as.telemetry()]
#'
#' @return a list of telemetry objects, each named by animal name
#' @export
#'
as_tele_list <- function(tele){
  if (class(tele) != "list") {
    # use same name so we can return same name if no change made
    tele_list <- list(tele)
    names(tele_list) <- attr(tele_list[[1]],"info")$identity
    return(tele_list)
  } else {
    return(tele)
  }
}
# update a list of telemetry obj identity slot with new names, also update item name with new names
update_tele_list_ids <- function(tele_list, new_name_vec){
  for (i in seq_along(tele_list)) {
    tele_list[[i]]@info$identity <- new_name_vec[i]
  }
  names(tele_list) <- new_name_vec
  return(tele_list)
}
# import multiple files, also work with single file
import_tele_files <- function(files, remove_marked_outliers = TRUE) {
  tele_list_list <- lapply(files, function(x) {
    as_tele_list(as.telemetry(x, mark.rm = remove_marked_outliers))
  })
  tele_list <- unlist(tele_list_list, recursive = FALSE)
  animal_names <- names(tele_list)
  dupe_index <- duplicated(animal_names)
  if (any(dupe_index)) {
    new_names <- make.unique(animal_names)
    warning("  Duplicate individual names found and changed:\n",
            paste0("\t", animal_names[dupe_index], "\t->\t",
                   new_names[dupe_index], "\n")
    )
    # change the identity slot in telemetry obj, and the item name in list
    # this applied to all names, not just dup names. however this is clear in concept, not likely to have error
    tele_list <- update_tele_list_ids(tele_list, new_names)
  }
  ctmm::projection(tele_list) <- ctmm::median(tele_list, k = 2)
  return(tele_list)
}
# is_calibrated <- function(tele_obj) {
#   # make sure it's logical otherwise it may return a numerical value
#   isTRUE(ctmm::uere(tele_obj)["horizontal"])
# }
is_calibrated <- function(tele_obj) {
  # integer will become index in switch, not working with 0
  switch(as.character(ctmm:::is.calibrated(tele_obj)),
         "1" = "yes",
         "0" = "no",
         NA_character_)
}
# uere_calibrated <- function(tele_obj) {
#   if (ctmm:::is.calibrated(tele_obj) != 1) {
#     return(NA_real_)
#   } else {
#     round(ctmm::uere(tele_obj)@.Data[["all", "horizontal"]], 3)
#   }
# }
# get single animal info in one row data frame
info_tele <- function(object) {
  # sometimes the data is anonymized and don't have timestamp column. It has happened several times so we need to have proper error message.
  stopifnot("timestamp" %in% names(object))
  # some data have one record for some individual, diff will return numeric(0), then median got NULL
  diffs <- diff(object$t)
  # the median of diff
  sampling_interval <- ifelse(length(diffs) == 0,
                              0,
                              stats::median(diffs))
  sampling_range <- max(object$t) - min(object$t)
  # above work on t which is cleaned by ctmm. original timestamp could have missing values
  t_start <- min(object$timestamp, na.rm = TRUE)
  t_end <- max(object$timestamp, na.rm = TRUE)
  calibrated <- is_calibrated(object)
  # uere_value <- uere_calibrated(object)
  # format the duration/interval units in list to make them use same unit
  data.table(identity = object@info$identity,
             start = format_datetime(t_start),
             end = format_datetime(t_end),
             interval = sampling_interval,
             duration = sampling_range,
             points = nrow(object),
             calibrated = calibrated
             # uere = uere_value
             )
}

# sort tele list by identity, ggplot always sort by id. ctmm keep same order in csv, but this should not create problem. actually I found the table is sorted from ctmm for old buffalo data 1764627, which is unsorted in csv.
# we should keep the list sorted, not the info table. info table order match original list because we need to use table index.
sort_tele_list <- function(tele_list) {
  tele_list[stringr::str_sort(names(tele_list))]
}
#' Report data summary on telemetry list
#'
#' @param tele_list [ctmm::as.telemetry()] telemetry list. Use [as_tele_list()]
#'   over [ctmm::as.telemetry()] to ensure a proper list.
#'
#' @return A summary `data.table`
#' @export
#'
report <- info_tele_list <- function(tele_list){
  # previously this work on either obj or list. but this may hide the problem to user. only work on list and give error here is better
  # stopifnot(class(tele_list) == "list")
  assert_tele_list(tele_list)
  info_list <- lapply(tele_list, info_tele)
  dt <- rbindlist(info_list)
  name_unit_list <- list("interval" = pick_unit_seconds,
                         "duration" = pick_unit_seconds)
  format_dt_unit(dt, name_unit_list)
}
#' Calculate distance to median center for each animal location
#'
#' If there are big gaps in sampling time, median center for each time group is
#' used. To reduce duplicate calculation, speed calculation will use some
#' columns created in distance calculation. Always update `data.table` with
#' [assign_distance()] first then use [assign_speed()].
#'
#' @param animals_dt location `data.table` from [combine()]. The original input
#'   `data.table` will be modified in place by reference after calculation.
#' @param tele_list the [ctmm::as.telemetry()] telemetry obj list. Calculation
#'   need error information from it.
#' @param device_error standardized device error in meter. Example: GPS: 10,
#'   VHF: 100
#'
#' @return The input `data.table` with distance columns added. The name `assign`
#'   hint on this nature.
#' @export
#'
assign_distance <- function(animals_dt, tele_list, device_error = 10) {
  # modify in place could be tricky in debuging. but it should not create problem in app usage, also making copy could take too much memory with big data set.
  # animals_dt <- copy(data_dt)
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
  # instead of calculating manually, use ctmm function because the complexity of error values. or use error vector and distanceMLE
  animals_dt[, `:=`(median_x = median(x), median_y = median(y)),
             by = group_index]
  animals_dt[, distance_center_raw := sqrt((x - median_x) ** 2 +
                                         (y - median_y) ** 2)]
  # calibrate error, see issue #5
  # animals_dt[, distance_center := ctmm:::distanceMLE(distance_center,
  #                                                    device_error)]
  # unique: identity is a column in .SD, dt will take the whole column to index tele_list, create a copy for every row.
  animals_dt[, error := ctmm:::get.error(
                          tele_list[[unique(identity)]][row_name,],
                          ctmm(error = device_error,axes = c("x","y")),
                          circle = TRUE),
             by = group_index]
  animals_dt[, distance_center := ctmm:::distanceMLE(distance_center_raw,
                                                     error),
             by = group_index]
  return(animals_dt)
}
# coati data has speed column, may have estimated speed later. change speed column name to assigned_speed
# the naive definition of leaving speed. the NA cleaning is not ideal
assign_speed_leaving <- function(animals_dt, tele_list, device_error) {
  animals_dt[, assigned_speed := sqrt(inc_x ^ 2 + inc_y ^ 2) / inc_t]
  animals_dt[is.infinite(assigned_speed), assigned_speed := NaN]
  # if last point n is outlier, n-1 will have high speed according to our definition, and n have no speed definition. assign n-1 speed to it. Then we don't need to clean up NA in speed too
  # this removed NaN too. The NA values caused speed outlier plot default subset to NULL. should not keep NA, NaN in speed, will cause too many troubles. could use negative value to mark
  for (i in animals_dt[is.na(assigned_speed), which = TRUE]) {
    animals_dt[i, assigned_speed := animals_dt[i - 1, assigned_speed]]
  }
  return(animals_dt)
}
# the pmin method
assign_speed_pmin <- function(animals_dt, tele_list, device_error) {
  # TODO deal with dt==0 cases
  # dt == 0, use the sampling resolution to estimate the time difference
  # animals_dt[inc_t == 0]
  # sampling_resolution <- gcd_vec(diff(animals_dt$t))
  animals_dt[, assigned_speed := sqrt(inc_x ^ 2 + inc_y ^ 2) / inc_t]
  # TODO these NA cleaning are temporary. not sure which is optimal. histogram will remove infinite value but we may want to filter these points. since this is just fallback temp method and mainly rely on ctmm function, stop here now. probably should clean dup time first in separate method, maybe remove them to list with a warning. in my leaving speed just assign to point before it, which is not ideal but hide all problems.
  # animals_dt[is.na(speed)]
  animals_dt[is.infinite(assigned_speed), assigned_speed := NaN]
  animals_dt[, speed_min := pmin(assigned_speed, shift(assigned_speed, 1L), na.rm = TRUE), by = id]
  # the extended definition of pmin, take min(1->3, 1->2), min(N-2 ->N, N-1 ->N) speed for 1 and N. 1->2 and N-1 ->N are the previous defined value, the leaving speed of 1 and N-1. using x,y,t of next point - current point to get speed for current point.
  point_1 <- animals_dt[, .I[1], by = id]
  point_N <- animals_dt[, .I[.N], by = id]
  # animals_dt[c(point_1$V1, point_1$V1 + 2), shift(x, 1L, type = "lead") - x, by = id]
  animals_dt[c(point_1$V1, point_1$V1 + 2), `:=`(
    x_3_1 = shift(x, 1L, type = "lead") - x,
    y_3_1 = shift(y, 1L, type = "lead") - y,
    t_3_1 = shift(t, 1L, type = "lead") - t), by = id]
  animals_dt[point_1$V1, speed_min_n :=
               pmin(assigned_speed, sqrt(x_3_1 ^ 2 + y_3_1 ^ 2) / t_3_1,
                    na.rm = TRUE),
             by = id]
  # for N we are using N, N-2 order so still using lead, note order of t
  animals_dt[c(point_N$V1, point_N$V1 - 2), `:=`(
    x_N_2 = x - shift(x, 1L, type = "lead"),
    y_N_2 = y - shift(y, 1L, type = "lead"),
    t_N_2 = t - shift(t, 1L, type = "lead")), by = id]
  animals_dt[point_N$V1, speed_min_n :=
               pmin(assigned_speed, sqrt(x_N_2 ^ 2 + y_N_2 ^ 2) / t_N_2,
                    na.rm = TRUE),
             by = id]
  # View(animals_dt[c(point_1$V1, point_1$V1 + 2, point_N$V1, point_N$V1 - 2), c(1:13, 17:25), with = TRUE][order(id)])
  # to accompany extended definition, use speed_min first, now replace the assigned_speed column.
  animals_dt[, assigned_speed := speed_min]
  animals_dt[c(point_1$V1, point_N$V1), assigned_speed := speed_min_n]
  animals_dt[, c("speed_min", "x_3_1", "y_3_1", "t_3_1", "speed_min_n",
                 "x_N_2", "y_N_2", "t_N_2") := NULL]
  return(animals_dt)
}
# using ctmm util functions
assign_speed_ctmm <- function(animals_dt, tele_list, device_error) {
  # assign_speeds expect telemetry obj and will use error info from it. Previously only data frame part is used. Now I need to get the telemetry obj for each animal. will use time_res by itself so no need for dt, also method default to max so no need for that. return a list, we need v.t since it match original row count. v.dt is for in-between.
  # animals_dt[, speed := ctmm:::assign_speeds(.SD,
  #                                            dt = ctmm:::time_res(.SD),
  #                                            UERE = device_error, method = "max"),
  #            by = identity]
  # when using by = identity, each .SD don't have identity column, it's outside.
  # follow usage in ctmm::outlie, error is calculated in distance function
  # row_name is characters, and we are using data.frame row.names to index them.
  animals_dt[, assigned_speed := ctmm:::assign_speeds(
                          tele_list[[identity]][row_name,],
                          UERE = error)$v.t,
             by = identity]
  return(animals_dt)
}
# general fall back function. wait data for speed error then replace with this too. right now just for akde memory error. this only handle one level fall back. for 2 levels fall back, right now just add it manually. currently the 2nd fall back in trymodels actually just print msg, didn't calculate it again so not really fallback again. note arg must be list even for single arg.
# to be more general, could move msg to first, use ... for all f and args. use do while inside. but don't need that so far.
fall_back <- function(f1, f1_args_list, f2, f2_args_list, msg) {
  res <- try(do.call(f1, f1_args_list))
  if (inherits(res, "try-error")) {
    # the right hand of $ is style name parameter instead of pkg function, no need to add pkg prefix
    cat(crayon::white$bgMagenta(msg), "\n")
    res <- do.call(f2, f2_args_list)
  }
  return(res)
}
#' Calculate speed for each animal location
#'
#' It's difficult to get a simple speed definition yet robust to all kinds of
#' dirty data cases. A sophisticated method is attempt first which should cover
#' most common edge cases reasonably well. When it fails for extreme situations,
#' the function will fall back to simpler method which is more naive but robust.
#'
#' To reduce duplicate calculation, speed calculation will use some columns
#' created in distance calculation. Always update `data.table` with
#' [assign_distance()] first then use [assign_speed()].
#'
#' @inheritParams assign_distance
#'
#' @return The input `data.table` with `assigned_speed` columns added. The name
#'  `assign` hint on this nature.
#' @export
#'
assign_speed <- function(animals_dt, tele_list, device_error = 10) {
  # note every parameter changes need to be present in every data call, several places
  # my speed calculation need distance columns
  stopifnot(c("error", "distance_center") %in% names(animals_dt))
  # we didn't use the third fallback, pmin should be robust enough.
  animals_dt <- fall_back(assign_speed_ctmm,
                          list(animals_dt, tele_list, device_error),
                          assign_speed_pmin,
                          list(animals_dt, tele_list, device_error),
                          "Had error with first speed definition, use alternative instead")
  return(animals_dt)
}
# we should coerice tele obj to list from very beginning once, and only work with list later. converting it in middle can hide the problem when some code works but some stil have problems.
# merge tele list into data.table with identity column, easier for ggplot and summary. go through every obj to get data frame and metadata, then combine the data frame into data, metadata into info.
# assuming row order by timestamp and identity in same order with tele obj.
# always use row_no for dt identifying records, use (identity, row_name) for tele_list (always need to use identity get item first). need to locate a record in tele_list from a row in dt(many operations only work with ctmm functions on telemetry, so need to convert and calc on that part), so need to maintain row_name same in dt and tele_list. never change them after initialization, also setkey on row_no, also need to make sure no dupe row_name within same individual(otherwise cause problem in dt). maintain row_no, so only add new in new subset added, keep original same.
# if multiple files are uploaded and holding same individual in different files with duplicated row_name, this could cause problem. there could also be problem with as_telemetry in this case. wait until reported by user.
# do need to reassign row_no when new subset added.
tele_list_to_dt <- function(tele_list) {
  assert_tele_list(tele_list)
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
  # should only initialize once and do not change them.
  animals_data_dt[, row_no := .I]
  setkey(animals_data_dt, row_no)
  any_dup <- anyDuplicated(animals_data_dt, by = c("identity", "row_name"))
  if (any_dup != 0) {
    message("duplicated row name found within same individual:\n")
    print(animals_data_dt[any_dup, .(identity, row_name)])
  }
  return(animals_data_dt)
}
#' Collect location and info `data.table` from telemetry list
#'
#' A [ctmm::as.telemetry()] telemetry list hold mutiple animal data in separate
#' list items, each item have the animal location data in a data frame, and
#' other information in various slots. This structure supports flexible S3
#' methods for telemetry object. However to plot multiple animals location
#' together with `ggplot2` we need to collect all location data as a single
#' `data.frame` with an animal id column.
#'
#' This function convert any input telemetry List into a list of 1. `data.table`
#' of location data, and 2. animal information `data.table`. `data.table` is
#' chosen over `data.frame` for much better performance. This data structure is
#' also used in a lot of places in app, which works on any selected subset of
#' full data in almost all steps.
#'
#' @param tele_list [ctmm::as.telemetry()] telemetry list. Use [as_tele_list()]
#'   over [ctmm::as.telemetry()] to ensure a proper list.
#'
#' @return list of - `data_dt`: all animals collected in one data.table -
#'   `info`: animal information table
#' @export
collect <- combine_tele_list <- function(tele_obj_list) {
  return(list(data_dt = tele_list_to_dt(tele_obj_list),
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
  cat(crayon::white$bgGreen("data consistency verified"))
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
# build id_pal from info ----
# this is needed in importing tele data, and restoring session data.
build_id_pal <- function(info) {
  leaflet::colorFactor(
    scales::hue_pal()(nrow(info)), info$identity, ordered = TRUE
  )
}
