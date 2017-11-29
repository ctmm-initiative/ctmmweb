# helper functions that useful to shiny app. Some are exported for individual uses. All functions are placed in one file for easier search for now.
# JS for log slider ----

#' JS Function To Logify A `sliderInput`
#'
#' To be added in UI code of Shiny. The server code of Shiny may also use it
#' when building UI dynamically. Search the usage in `/inst/app/ui.R` for
#' examples.
#'
#' @param digits digits after numerical point
#'
#' @return JS function code
#' @export
#'
JS.logify <- function(digits = 2) {
  paste0(  "
           // function to logify a sliderInput
           function logifySlider (sliderId, sci = false) {
           if (sci) {
           // scientific style
           $('#'+sliderId).data('ionRangeSlider').update({
           'prettify': function (num) { return ('10<sup>'+num+'</sup>'); }
           })
           } else {
           // regular number style
           $('#'+sliderId).data('ionRangeSlider').update({
           'prettify': function (num) { return (Math.pow(10, num).toFixed(",
           digits, ")); }
           })
           }
           }")
}
#' Register JS Logify Function For Each `sliderInput`
#'
#' @param slider_id_vec slider id vector
#' @param sci use scientific notation
#'
#' @return JS functions code
#' @export
#'
JS.onload <- function(slider_id_vec, sci = FALSE) {
  slider_call <- function(slider_id) {
    paste0("logifySlider('", slider_id,
           "', sci = ", ifelse(sci, "true", "false") , ")")
  }
  return(paste0("
                // execute upon document loading
                $(document).ready(function() {
                // wait a few ms to allow other scripts to execute
                setTimeout(function() {",
                paste0(lapply(slider_id_vec, slider_call), collapse = "\n"),
                "}, 5)})"
                ))
}
# UI style constants ----
# some are used in server call, so both ui and server need them
# box, plotOutput, renderPlot, no need to set all three if need adjustment.
# box height will expand by content, just set plotOutput width and height to percentages (99% width, need to keep it inside the box), then also need to set fixed value in renderPlot (otherwise it didn't show). We set height on histogram to make it shorter, setting box height is easier (no need to set in server part).
STYLES <- list(
  height_hist = 280,
  # outliers
  height_outlier_hist = "180px",
  # time subsetting
  # not setting the box height make arrange multiple items easier.
  # height_hist_subset_box = "380px",
  height_hist_subset_output = "150px",
  # height_selected_loc_box = "480px"
  # height_selected_loc = 480
  page_action = "background-color: #FFEB3B;font-weight: 500;width:100%;",
  # using similar color with first box in each page.
  page_switch = "background-color: #7ad0f7;font-weight: 500;width:100%;",
  external_link = "background-color: #a7c1fc;font-weight: 500;width:100%;",
  download_button = "color: #2196F3;width:100%;",
  help_button = "background-color: #8bc34a;width:100%;"
  # info box blue #00c0ef
)
# unit formatting ----
# function with _f postfix generate a unit_format function, which can be used in ggplot scales. To generate formated values call function on input again
# generate a unit_format function with picked unit. This only take single value, the wrappers will take a vector, pick test value and concise parameter according to data type then apply to whole vector
# need to round up digits otherwise DT is showing too many digits
# hardcoded values are round to 4 digits, print 2 digits.
unit_format_round <- function(unit = "m", scale = 1, sep = " ", ...){
  function(x){
    paste(scales::comma(round(x * scale, 4), ...), unit, sep = sep)
  }
}
# given a resprentative value and unit dimension, get the best unit then generate a format function to be used on a vector of similar values. other derived functions pick certain value from a vector and choose a dimension.
# extra round option when really needed
pick_best_unit_f <- function(test_value, dimension, concise, round = FALSE) {
  best_unit <- ctmm:::unit(test_value, dimension, thresh = 1, concise = concise)
  if (round) {
    unit_format_round(unit = best_unit$name, scale = 1 / best_unit$scale,
                      digits = 2)
  } else {
    scales::unit_format(unit = best_unit$name, scale = 1 / best_unit$scale,
                        digits = 2
                        # , nsmall = 2  # using this will cause 60 mins become 60.00
    )
  }
}
# function will take vector as input, but only return a format function which is good for scales in ggplot. will need to apply to vector again if need the formated result.
format_distance_f <- function(v, round = FALSE){
  # didn't use median because it could be near zero with positive and negative values
  pick_best_unit_f(max(abs(v), na.rm = TRUE)/2, dimension = "length",
                   concise = TRUE, round = round)
}
# given a test value, pick unit, return scale and name. SI / scale get the new value
pick_unit_distance <- function(v) {
  ctmm:::unit(max(abs(v), na.rm = TRUE)/2, dimension = "length",
              concise = TRUE)
}
format_seconds_f <- function(secs, round = FALSE) {
  pick_best_unit_f(median(secs, na.rm = TRUE), dimension = "time",
                   concise = FALSE, round = round)
}
# note we cannot use format_speed_f(vec)(vec) for value in data.table, which may call the function for single value and lost context in whole vector.
format_speed_f <- function(speed, round = FALSE) {
  pick_best_unit_f(median(speed, na.rm = TRUE), dimension = "speed",
                   concise = TRUE, round = round)
}
pick_unit_speed <- function(speed) {
  ctmm:::unit(median(speed, na.rm = TRUE), dimension = "speed",
                   concise = TRUE)
}
format_area_f <- function(area, round = FALSE) {
  pick_best_unit_f(median(area, na.rm = TRUE), dimension = "area",
                   concise = TRUE, round = round)
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
# ctmm data processing ----
# get single animal info in one row data frame
single_tele_info <- function(object) {
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
  tele_list[sort(names(tele_list))]
}
#' Get Information Table For Telemetry Object Or List
#'
#' @param tele_obj_list telemetry object or list
#'
#' @return An information `data.table` for input
#' @export
#'
tele_list_info <- function(tele_obj_list){
  tele_list <- wrap_single_telemetry(tele_obj_list)
  animal_info_list <- lapply(tele_list, single_tele_info)
  rbindlist(animal_info_list)
}
#' Calculate Distance To Median Center For Each Animal Location
#'
#' If there are big gaps in sampling time, median center for each time group is
#' used. To reduce duplicate calculation, speed calculation will use some
#' columns created in distance calculation. Always `calculate_distance` first
#' then [calculate_speed()].
#'
#' @param animals_dt telemetry data in merged data.table. See [merge_animals()]
#'
#' @return data.table with distance columns added. Note the input parameter is
#'   modified in place.
#' @export
#'
calculate_distance <- function(animals_dt) {
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
calculate_speed_leaving <- function(animals_dt, device_error) {
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
calculate_speed_pmin <- function(animals_dt, device_error) {
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
calculate_speed_ctmm <- function(animals_dt, device_error) {
  setkey(animals_dt, row_no)
  animals_dt[, speed := ctmm:::assign_speeds(.SD,
                                             dt = ctmm:::time_res(.SD),
                                             UERE = device_error, method = "max"),
             by = identity]
  return(animals_dt)
}
#' Calculate Speed For Each Animal Location
#'
#' It's difficult to get a simple speed definition yet robust to all kinds of
#' dirty data cases. A sophisticated method is attempt first which should cover
#' most common edge cases reasonably well. When it fails for extreme situations,
#' the function will fall back to simpler method which is more naive but robust.
#'
#' To reduce duplicate calculation, speed calculation will use some columns
#' created in distance calculation. Always [calculate_distance()] first then
#' [calculate_speed()].
#'
#' @param animals_dt telemetry data in merged data.table
#' @param device_error device error if available
#'
#' @return data.table with speed columns added. Note the input parameter is
#'   modified in place.
#' @export
#'
calculate_speed <- function(animals_dt, device_error) {
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
                          calculate_speed_ctmm, calculate_speed_pmin)
  # animals_dt <- calculate_speed_ctmm(animals_dt)
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
  # animals_data_dt <- calculate_distance(animals_data_dt)
  # animals_data_dt <- calculate_speed(animals_data_dt)
  return(animals_data_dt)
}
#' Generate Merged Location And Info `data.table` From Telemetry Object/List
#'
#' A Telemetry list hold mutiple animal data in separate list items, each item
#' have the animal location data in a data frame, and other information in
#' various slots. This structure supports flexible S3 methods for telemetry
#' object. However to plot multiple animals location together with `ggplot2`
#' it's better to merge all location data into single data frame with an animal
#' id column.
#'
#' We chose this data structure to be the main structure and made the app to
#' work on a set of animals at the same time in all steps. Thus any input
#' telemetry object/List need to be merged into a `data.table` of location data,
#' and another information `data.table` for animals. `data.table` is chosen over
#' `data.frame` for much better performance.
#'
#' @param tele_obj_list telemetry object/list
#'
#' @return list of - `data`: all animals merged in one data.table - `info`:
#'   animal information table
#' @export
#'
#' @examples merge_animals(buffalo)
merge_animals <- function(tele_obj_list) {
  return(list(data = tele_list_to_dt(tele_obj_list),
              info = tele_list_info(tele_obj_list)))
}
# to test if tele_list is in sync with merged data.
# this will not work when there are NA cols introduced by merge with different cols. need to clean those cols first
match_tele_merged <- function(tele_list, merged) {
  req(!is.null(tele_list) | (!is.null(merged)))
  # verify info match dt, this is not garranted since sometimes they are processed separately
  stopifnot(all.equal(merged$info, tele_list_info(tele_list)))
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
# ggplot ----
BIGGER_THEME <- ggplot2::theme(legend.key.size = grid::unit(8, "mm"),
                      legend.key.height = grid::unit(8, "mm"),
                      legend.text = ggplot2::element_text(size = 12),
                      axis.title = ggplot2::element_text(size = 14),
                      axis.text = ggplot2::element_text(size = 12))
BIGGER_KEY <- ggplot2::guides(colour = ggplot2::guide_legend(
  override.aes = list(size = 4)))
CENTER_TITLE <- ggplot2::theme(plot.title = ggplot2::element_text(
  hjust = 0.5, face = "bold"))
# map color to a factor with unused levels included, but don't show them in legend.
# note need to use dt$id format. note the mapping is provided in aes(color/fill = xx) already, this is to override some options.
factor_mapper <- function(fac, FUN) {
  FUN(drop = FALSE, breaks = levels(droplevels(fac)))
}
factor_color <- function(fac) {
  # scale_colour_hue(drop = FALSE, breaks = levels(droplevels(fac)))
  factor_mapper(fac, ggplot2::scale_colour_hue)
}
# note for fill colors we need different function
factor_fill <- function(fac) {
  # scale_fill_hue(drop = FALSE, breaks = levels(droplevels(fac)))
  factor_mapper(fac, ggplot2::scale_fill_hue)
}
factor_alpha <- function(fac) {
  # scale_alpha_discrete(drop = FALSE, breaks = levels(droplevels(fac)))
  factor_mapper(fac, ggplot2::scale_alpha_discrete)
}
# movebank download ----
# always need the response content in text, also need response status
request <- function(entity_type, user, pass){
  base_url <- "https://www.movebank.org/movebank/service/direct-read?entity_type="
  url <- paste0(base_url, entity_type)
  res <- httr::GET(url, config = httr::add_headers(user = user, password = pass))
  status <- httr::http_status(res)$category
  if (status != "Success") {
    shiny::showNotification(paste0(httr::http_status(res)$message,
                                   "\nCheck console for more information"),
                            duration = 6, type = "error")
    # will use xml2::read_html
    res_cont <- httr::content(res, type = 'text/html', encoding = "UTF-8")
    txt <- html_to_text(res_cont)
    formated_txt <- gsub("^ $", ": ", txt)
    warning(formated_txt)
  }
  res_cont <- httr::content(res, as = 'text', encoding = "UTF-8")
  return(list(status = status, res_cont = res_cont))
}
get_all_studies <- function(user, pass) {
  return(request("study", user, pass))
}
# [blog post](https://tonybreyal.wordpress.com/2011/11/18/htmltotext-extracting-text-from-html-via-xpath/), [code](https://github.com/tonybreyal/Blog-Reference-Functions/blob/master/R/htmlToText/htmlToText.R)
html_to_text <- function(html) {
  doc <- XML::htmlParse(html, asText = TRUE)
  text <- XML::xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
  return(text)
}
get_study_detail <- function(mb_id, user, pass) {
  request(paste0("study&study_id=", mb_id), user, pass)
}
get_study_data <- function(mb_id, user, pass){
  request(paste0("event&study_id=", mb_id), user, pass)
}
# divide x into interval_count intervals ----
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
# outlier ----
color_break <- function(bin_count, animals_dt, col_name, unit_formatter) {
  color_bin_breaks <- divide(animals_dt[[col_name]], bin_count)
  # cut generate roughly equal length breaks (with adjustment to include extremes, no need to change this) which sometimes have a negative first value, not good for distanc/speed. shift the breaks so that the first value is 0.
  if (color_bin_breaks[1] < 0) {
    color_bin_breaks <- color_bin_breaks + abs(color_bin_breaks[1])
  }
  # need to change the default of (b1, b2] to [b1, b2), otherwise 0 will not be included.
  # format label to include unit. difficult to separate unit with value, could only include left side though.
  vec_formatter <- unit_formatter(color_bin_breaks)
  color_bin_breaks_units <- vec_formatter(color_bin_breaks)
  color_bin_labels <- paste0(">= ", utils::head(color_bin_breaks_units, -1L))
  animals_dt[, paste0(col_name, "_color_factor") :=
               cut(animals_dt[[col_name]], breaks = color_bin_breaks,
                   labels = color_bin_labels, right = FALSE)]  # closed on left to include 0
  # remove empty bins in labels
  his <- graphics::hist(animals_dt[[col_name]], breaks = color_bin_breaks, plot = FALSE)
  # with n+1 breaks for n interval/bin count, using count index on breaks will get the left side for each break
  non_empty_indice <- which(his$counts != 0)
  # need both side to label the plot properly. this only works when 2 vectors have same length. but I don't like the other method of recyling index new_vec[c(TRUE, FALSE)]
  non_empty_breaks <- c(rbind(color_bin_breaks[non_empty_indice],
                              color_bin_breaks[non_empty_indice + 1]))
  # data.table is modified by reference, but we don't have global dt in app, often in reactive, so need to return it.
  return(list(animals_dt = animals_dt,
              color_bin_breaks = color_bin_breaks,
              non_empty_breaks = non_empty_breaks,
              vec_formatter = vec_formatter))
}
# parallel ----
# need to prepare exp_init for windows version ----
# try to include all variables in the lapply list so fun only take one parameter. that means exp_init usually only init libraries.
# export take from global env, so need to assign in global

#' Expression To Be Initialized In Windows For `ctmm` Related Parallel
#' Operations
#'
#' Parallel cluter in Windows is a socket cluster, which need to initialize each
#' session manually. For ctmm related parallel operations, `ctmm` package need
#' to be loaded. Instead of `library(ctmm)`, the expression of
#' `requireNamespace("ctmm", quietly = TRUE)` is more appropriate inside a
#' package.
WIN_INIT_ctmm <- expression({
  # library(ctmm)
  requireNamespace("ctmm", quietly = TRUE)
})

#' Combine Two Lists Into One List By Aligning Each Item
#'
#' `list_a` and `list_b` need to have same length.
#'
#' @param list_a list_a
#' @param list_b list_b
#'
#' @return A list of same length of input list. Each item is a list of
#' \itemize{
#'  \item \code{a: list_a[[i]]}
#'  \item \code{b: list_b[[i]]}
#' }
#'
#' @export
#'
align_list <- function(list_a, list_b) {
  stopifnot(length(list_a) == length(list_b))
  # use lapply instead of for only because we can get a list without initialization
  lapply(seq_along(list_a), function(i) {
    list(a = list_a[[i]], b = list_b[[i]])
  })
}
#' Parallel Apply Function To List In All Platforms
#'
#' This is a generic parallel lapply that work across all major platforms.
#'
#' In Windows `parallel::parLapplyLB` is used, which is a socket cluster and
#' need to initialize each session manually. In Linux/Mac `parallel::mclapply`
#' is used, where each worker will inherit the current environment through
#' forking, so no additional setup is required.
#'
#' @param ll Input list.
#' @param fun Function to be applied on `ll`. Note only single parameter
#'   function is accepted, otherwise it's difficult to determine how to assign
#'   input parameters to each list item and worker. You need to convert multiple
#'   parameter function into a function take single list parameter, and assign
#'   parameters in that list accordingly. [align_list()] is a helper function to
#'   align two lists.
#' @param win_init Expression to be initialized in Windows. Because all
#'   parameters should be included in the input list already, this usually means
#'   library calls, like `{library(ctmm)}` for ctmm related operations, which
#'   has been taken care of with the default value `ctmmweb:::WIN_INIT_ctmm`.
#'
#' @return List of applied results
#' @export
#'
para_ll <- function(ll, fun, win_init = ctmmweb:::WIN_INIT_ctmm) {
  sysinfo <- Sys.info()
  if (sysinfo["sysname"] == "Windows")  {  # Darwin / Windows
    win_cluster_size <- min(length(ll), parallel::detectCores())
    cat(crayon::inverse("running parallel in SOCKET cluster of", win_cluster_size, "\n"))
    cl <- parallel::makeCluster(win_cluster_size, outfile = "")
    # have to export parameter too because it's not available in remote
    parallel::clusterExport(cl, c("win_init"), envir = environment())
    parallel::clusterEvalQ(cl, eval(win_init))
    res <- parallel::parLapplyLB(cl, ll, fun)
    parallel::stopCluster(cl)
  } else {
    cluster_size <- min(length(ll), parallel::detectCores(logical = FALSE) * 4)
    cat(crayon::inverse("running parallel with mclapply in cluster of", cluster_size, "\n"))
    res <- parallel::mclapply(ll, fun, mc.cores = cluster_size)
  }
  return(res)
}
# app need this since we may want adjusted guess list instead of automatic guess
#' Parallel Fit Models For List Of Telemetry List And Guess List
#'
#' @param tele_guess_list aligned list of telemetry list and guess list
#'
#' @return list of model fitting results on each telemetry object
#'
para_ll_fit_tele_guess <- function(tele_guess_list) {
  # cannot use select_models name since that was a reactive expression to select model results by rows. use internal function for better locality, less name conflict
  fit_models <- function(tele_guess) {
    ctmm::ctmm.select(tele_guess$a, CTMM = tele_guess$b,
                      trace = TRUE, verbose = TRUE)
  }
  para_ll(tele_guess_list, fit_models)
}
# convenience wrapped to take telemetry list, guess them, fit models. In app we want more control and didn't use this.
#' Parallel Fit Models On Telemetry List
#'
#' @param tele_list telemetry list
#'
#' @return list of model fitting results on each telemetry object, named by
#'   telemetry object names
#' @export
#'
#' @examples para_ll_fit_tele(buffalo)
para_ll_fit_tele <- function(tele_list) {
  tele_guess_list <- align_list(tele_list,
                                lapply(tele_list, function(x) {
                                  ctmm.guess(x, interactive = FALSE)
                                }))
  print(system.time(model_select_res <-
                      para_ll_fit_tele_guess(tele_guess_list)))
  names(model_select_res) <- names(tele_list)
  return(model_select_res)
}
#' Parallel Calculate Occurrence From Telemetry And Model List
#'
#' @param tele_model_list Aligned list of telemetry list and model list
#'
#' @return occurrence results list
#' @export
#'
#' @examples para_ll_ud(align_list(buffalo, models_list))
para_ll_ud <- function(tele_model_list) {
  ud_calc <- function(tele_model_list) {
    ctmm::occurrence(tele_model_list$a, tele_model_list$b)
  }
  para_ll(tele_model_list, ud_calc)
}
# sample telemetry data ----

#' Sample From Telemetry Object
#'
#' Take even spaced `m` points. Rely on ctmm S3 method to treat telemetry object
#' as a data.frame, thus ctmm need to be imported in NAMESPACE.
#'
#' @param tele telemetry object
#' @param m sample size
#'
#'
#' @return sampled telemetry object
#' @export
#' @import ctmm
#'
#' @examples sample_tele(buffalo[[1]], 100)
sample_tele <- function(tele, m) {
  tele[floor(seq(from = 1, to = nrow(tele), length.out = m)), ]
}
#' Sample Each Telemetry Object In List
#'
#' @param tele_list telemetry list
#' @param m sample size
#'
#' @return sampled telemetry list
#' @export
#'
#' @examples sample_tele_list(buffalo, 100)
sample_tele_list <- function(tele_list, m) {
  lapply(tele_list, function(x) {
    sample_tele(x, m)
  })
}
# build ctmm model summary table ----
ctmm_obj_to_summary_dt <- function(model) {
  # convert the named vectors into data table, keep relevant info
  model_summary_list <- lapply(summary(model, units = FALSE), function(item) {
    data.table(t(data.frame(item)), keep.rownames = TRUE)
  })
  # any modification to dof_dt actually changed the parameter. this cause problems in rerun the function second time.
  dof_dt <- copy(model_summary_list[["DOF"]])
  ci_dt <- copy(model_summary_list[["CI"]])
  # need literal treatment by item name because every case is different
  # we need row name of CI, but not dof
  dof_dt[, rn := NULL]
  setnames(dof_dt, names(dof_dt), stringr::str_c("DOF ", names(dof_dt)))
  setnames(ci_dt, "rn", "estimate")
  ci_dt[estimate %in% c("low", "high"), estimate := stringr::str_c("CI ", estimate)]
  # with SI units, we only need to know which type of unit. the actual format need to happen after the whole table is built. so we only need a mapping from col name to proper unit function later.
  setnames(ci_dt, names(ci_dt), stringr::str_replace_all(names(ci_dt), "\\s\\(.*", ""))
  # two part need to bind together, add model name col, then add animal name col in last step. because of row number difference, it's easier to use merge, add common col first.
  dof_dt[, item := 1]
  ci_dt[, item := 1]
  res_dt <- merge(dof_dt, ci_dt, by = "item")
  res_dt[, item := NULL]
}
# from ctmm.fit result model list to data table with models in list column
model_fit_res_to_model_list_dt <- function(model_fit_res) {
  animal_names_dt <- data.table(identity = names(model_fit_res))
  model_name_list <- lapply(model_fit_res, names)
  # must use per row by to create list column, otherwise dt try to apply whole column to function
  animal_names_dt[, model_name_list := list(list(model_name_list[[identity]])),
                  by = 1:nrow(animal_names_dt)]
  models_dt <- animal_names_dt[, .(model_name = unlist(model_name_list)),
                               by = identity]
  models_dt[, model := list(list(model_fit_res[[identity]][[model_name]])),
            by = 1:nrow(models_dt)]
  models_dt[, model_no := .I]
  # also add the AICc col
  get_aicc_col <- function(model_list) {
    res <- summary(model_list, units = FALSE)
    data.frame(res)$dAICc
  }
  models_dt[, dAICc := get_aicc_col(model), by = identity]
}
model_list_dt_to_model_summary_dt <- function(models_dt, hrange = FALSE) {
  # make copy first because we will remove column later
  # a list of converted summary on each model
  model_summary_dt_list <- lapply(1:nrow(models_dt), function(i) {
    summary_dt <- ctmm_obj_to_summary_dt(models_dt$model[[i]])
    summary_dt[, model_no := i]
  })
  model_summary_dt <- rbindlist(model_summary_dt_list, fill = TRUE)
  # home range result also used this function, but there is no dAICc column from summary of list of home range.
  if (hrange) {
    res_dt <- merge(models_dt[, .(identity, model_name, model_no)],
                    model_summary_dt,
                    by = "model_no")
  } else {
    res_dt <- merge(models_dt[, .(identity, model_name, model_no, dAICc)],
                    model_summary_dt,
                    by = "model_no")
  }
  # res_dt[, color_target := stringr::str_c(identity, " - " , estimate)]
}
# apply units format functions list to columns
apply_format_f_list <- function(dt, format_f_list) {
  # it's easier to use a for loop since we can use i. with lapply and .SD we don't have col name available
  for (i in seq_along(format_f_list)) {
    # tried to use identity for cols don't need change, but we cannot update existing cols because col type changed
    if (!is.null(format_f_list[[i]])) {
      # the data table in shiny printed too many digits.
      dt[, paste0(names(dt)[i], "_units") := format_f_list[[i]](dt[[names(dt)[i]]])]
    }
  }
  new_cols <- names(dt)[stringr::str_detect(names(dt), "_units")]
  old_cols <- stringr::str_replace_all(new_cols, "_units", "")
  dt[, (old_cols) := NULL]
  setnames(dt, new_cols, old_cols)
}
format_model_summary_dt <- function(model_summary_dt) {
  # data.table modify reference, use copy so we can rerun same line again
  dt <- copy(model_summary_dt)
  # speed is m/day, need manual adjust before ctmm update on this
  # dt[, speed := speed / (24 * 3600)]
  # round up dof mean, area
  dt[, `DOF mean` := round(`DOF mean`, 3)]
  dt[, `DOF area` := round(`DOF area`, 3)]
  dt[, dAICc := round(dAICc, 3)]
  format_f_list <- lapply(names(dt), function(col_name) {
    switch(col_name,
       area = format_area_f(dt[[col_name]], round = TRUE),
       `tau position` = format_seconds_f(dt[[col_name]], round = TRUE),
       `tau velocity` = format_seconds_f(dt[[col_name]], round = TRUE),
       speed = format_speed_f(dt[[col_name]], round = TRUE),
       error = format_distance_f(dt[[col_name]], round = TRUE)
       )
  })
  # not really used, but easier to debug
  names(format_f_list) <- names(dt)
  res_dt <- apply_format_f_list(dt, format_f_list)
  # NA cells should have units removed or just empty values
  res_dt[stringr::str_detect(`tau velocity`, "^NA "),
         c("tau velocity", "speed") := ""]
  res_dt[stringr::str_detect(estimate, "CI"),
         c("DOF mean", "DOF area") := NA_real_]
  # add model full name col so it can be used to create model color palette
  res_dt[, full_name := stringr::str_c(identity, " - ", model_name)]
}
# combined steps of generating model summary and format it
#' Title
#'
#' @param models_dt model list dt
#'
#' @return formated model summary table
#' @export
#'
model_list_dt_to_formated_model_summary_dt <- function(models_dt) {
  model_summary_dt <- model_list_dt_to_model_summary_dt(models_dt,
                                                        hrange = FALSE)
  format_model_summary_dt(model_summary_dt)
}
# from akde result model list to data table with models in list column
#' Title
#'
#' @param selected_dt model names dt
#' @param selected_hrange_list home range list
#'
#' @return a data.table holding model info and home range
#' @export
#'
build_hrange_list_dt <- function(selected_model_names_dt, selected_hrange_list) {
  dt <- copy(selected_model_names_dt)
  dt[, model := list(selected_hrange_list)]
  dt[, model_no := .I]
}
format_hrange_summary_dt <- function(hrange_summary_dt) {
  # data.table modify reference, use copy so we can rerun same line again
  dt <- copy(hrange_summary_dt)
  dt[, `DOF area` := round(`DOF area`, 3)]
  dt[, `DOF bandwidth` := round(`DOF bandwidth`, 3)]
  format_f_list <- lapply(names(dt), function(col_name) {
    switch(col_name,
           area = format_area_f(dt[[col_name]], round = TRUE)
    )
  })
  # not really used, but easier to debug
  # names(format_f_list) <- names(dt)
  res_dt <- apply_format_f_list(dt, format_f_list)
  res_dt[stringr::str_detect(estimate, "CI"),
         c("DOF area", "DOF bandwidth") := NA_real_]
}
#' Title
#'
#' @param hrange_list_dt a data.table holding model info and home range
#'
#' @return formated home range summary table
#' @export
#'
hrange_list_dt_to_formated_range_summary_dt <- function(hrange_list_dt) {
  hrange_summary_dt <- model_list_dt_to_model_summary_dt(hrange_list_dt,
                                                        hrange = TRUE)
  format_hrange_summary_dt(hrange_summary_dt)
}
# folder and timestamp ----
# used in build_zip, creating temp folder for log
current_timestamp <- function() {
  # format(Sys.time(), "%Y-%m-%d_%H-%M-%S_UTC",
  #        tz = "UTC")
  format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
}
create_folder <- function(folder_path) {
  dir.create(folder_path, recursive = TRUE)
  return(folder_path)
}
# zip will be saved to one level up folder_path.
#' Title
#'
#' @param folder_path The folder to be compressed
#' @param zip_name The name of zip
#'
#' @return The absolute path of result zip file
#' @export
#'
compress_folder <- function(folder_path, zip_name) {
  previous_wd <- getwd()
  # one level up folder, so we can use relative path in zip
  setwd(dirname(folder_path))
  # zip example show that zip can take a folder as file list, it just need to be relative paths
  # relative_paths_under_folder <- list.files(folder_path, recursive = TRUE)
  # # construct the relative path inside zip
  # relative_paths <- file.path(basename(folder_path),
  #                             relative_paths_under_folder)
  zip_path <- file.path(dirname(folder_path), zip_name)
  zip::zip(zip_path, basename(folder_path),
           compression_level = 5, recurse = TRUE)
  setwd(previous_wd)
  return(zip_path)
}
# compress select files under one folder with relative path. zip will be put in same folder
compress_relative_files <- function(base_folder, relative_paths, zip_name) {
  previous_wd <- getwd()
  # one level up folder, so we can use relative path in zip
  setwd(base_folder)
  zip_path <- file.path(base_folder, zip_name)
  zip::zip(zip_path, relative_paths,
           compression_level = 5)
  setwd(previous_wd)
  return(zip_path)
}
# home range ----
parse_CI_levels <- function(levels_text) {
  if (stringr::str_trim(levels_text) == "") {
    return(0.95)
  } else {
    items <- stringr::str_trim(stringr::str_split(levels_text, ",")[[1]])
    as.numeric(items[items != ""]) / 100
  }
}
# file is the user chosen file name determined in download, need to prepare a file, copy to that path. write_f is a function that write files, take folder_path determined in build_zip as parameter.
build_shapefile_zip <- function(file, write_f, session_tmpdir) {
  # use time till min in zip name, use second in folder name, otherwise this function got run twice, will have error running 2nd time writing to same folder.
  current_time <- current_timestamp()  # need this in zip name so save it
  folder_path <- file.path(session_tmpdir, stringr::str_c("Range_", current_time))
  create_folder(folder_path)
  write_f(folder_path)
  zip_path <- compress_folder(folder_path,
                         paste0("Home Range ", current_time, ".zip"))
  file.copy(zip_path, file)
}
# map ----
GRID_GROUP <- "_graticule_"
# draw_group <- "_draw with measure_"
init_base_maps <- function(tiles_info) {
  leaf <- leaflet::leaflet(options = leaflet::leafletOptions(attributionControl = FALSE))
  for (prov in tiles_info$here) {
    leaf <- leaf %>% leaflet::addProviderTiles(leaflet::providers[[prov]], group = prov,
                                      options = leaflet::providerTileOptions(
                                        detectRetina = TRUE,
                                        app_id = tiles_info$here_app_id,
                                        app_code = tiles_info$here_app_code))
  }
  for (prov in tiles_info$open) {
    leaf <- leaf %>% leaflet::addProviderTiles(leaflet::providers[[prov]], group = prov)
  }
  return(leaf)
}
add_measure <- function(leaf) {
  leaf %>%
    leaflet::addMeasure(
      position = "bottomright",
      primaryLengthUnit = "meters",
      secondaryLengthUnit = "kilometers",
      primaryAreaUnit = "sqmeters",
      secondaryAreaUnit = "hectares",
      activeColor = "#3D535D",
      completedColor = "#e74c3c")
}
# the layer control need to wait home range, so not added here.
add_points <- function(leaf, dt, info, id_pal) {
  leaf <- leaf %>%
    leaflet::addSimpleGraticule(interval = 1, showOriginLabel = FALSE,
                       redraw = "moveend", group = GRID_GROUP)
  # add each individual as a layer
  # for loop is better than lapply since we don't need to use <<-
  for (current_id in info$identity) {
    leaf <- leaf %>%
      leaflet::addCircles(data = dt[identity == current_id], group = current_id,
                 lng = ~longitude, lat = ~latitude, radius = 0.3, weight = 2,
                 color = ~id_pal(id), opacity = 0.4, fillOpacity = 0.05)
  }
  leaf %>%
    leaflet::addLegend(pal = id_pal, values = info$identity,
                     position = "topleft") %>%
    leaflet::addScaleBar(position = "bottomleft") %>%
    # draw with measure, but it show measure on markers
    # addDrawToolbar(targetGroup = draw_group,
    #                editOptions = editToolbarOptions(
    #                  selectedPathOptions = selectedPathOptions())) %>%
    # addMeasurePathToolbar(options =
    #                         measurePathOptions(showOnHover = FALSE,
    #                                            minPixelDistance = 100))
    # simple measure
    add_measure()
}
reactive_validated <- function(reactive_value) {
  res <- try(reactive_value, silent = TRUE)
  return(!("try-error" %in% class(res)))
}
add_home_range <- function(leaf, hrange, hr_levels, hr_color, group_name){
  hrange_spdf <- sp::spTransform(ctmm::SpatialPolygonsDataFrame.UD(hrange,
                                                         level.UD = hr_levels),
                             sp::CRS("+proj=longlat +datum=WGS84"))
  ML_indice <- seq(2, length(hrange_spdf), by = 3)
  hrange_spdf_ML <- hrange_spdf[ML_indice, ]
  hrange_spdf_other <- hrange_spdf[-ML_indice, ]
  leaf %>%
    leaflet::addPolygons(data = hrange_spdf_ML, weight = 2.2, opacity = 0.7,
                fillOpacity = 0.05, color = hr_color, group = group_name) %>%
    leaflet::addPolygons(data = hrange_spdf_other, weight = 1.2, opacity = 0.4,
                fillOpacity = 0.05, color = hr_color, group = group_name)
}
# given a map object, add layers and return the map object
add_home_range_list <- function(leaf, hrange_list, hr_levels,
                                color_list, group_vec) {
  for (i in seq_along(hrange_list)) {
    leaf <- leaf %>% add_home_range(hrange_list[[i]], hr_levels,
                                    color_list[[i]], group_vec[i])
  }
  return(leaf)
}
# take and return rgb strings
vary_color <- function(base_color, count) {
  if (count == 1) {
    return(base_color)
  } else {
    hsv_vec <- grDevices::rgb2hsv(grDevices::col2rgb(base_color))[, 1]
    return(grDevices::hsv(hsv_vec[1], hsv_vec[2], seq(1, 0.5, length.out = count)))
  }
}
# added base map layer control
add_heat <- function(leaf, dt, tiles_info) {
  leaf %>%
    leaflet::addSimpleGraticule(interval = 1, showOriginLabel = FALSE,
                       redraw = "moveend", group = GRID_GROUP) %>%
    leaflet.extras::addHeatmap(data = dt, lng = ~longitude, lat = ~latitude,
               blur = 8, max = 1, radius = 5, group = "Heatmap") %>%
    leaflet::addScaleBar(position = "bottomleft") %>%
    add_measure() %>%
    leaflet::addLayersControl(
      baseGroups = c(tiles_info$here, tiles_info$open),
      overlayGroups = c(GRID_GROUP, "Heatmap"),
      options = leaflet::layersControlOptions(collapsed = FALSE))
}
get_bounds <- function(dt) {
  return(list(lng1 = min(dt$longitude), lat1 = min(dt$latitude),
              lng2 = max(dt$longitude), lat2 = max(dt$latitude)))
}
apply_bounds <- function(leaf, bounds) {
  leaflet::fitBounds(leaf, bounds$east, bounds$north, bounds$west, bounds$south)
}
