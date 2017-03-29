# helper functions that useful to shiny app, only need to run once
# to be placed in same directory of app.r/server.r
# function with _f postfix generate a unit_format function, which can be used in ggplot scales. To generate formated values call function on input again
# generate a unit_format function with picked unit. This only take single value, the wrappers will take a vector, pick test value and concise parameter according to data type then apply to whole vector
pick_best_unit_f <- function(test_value, dimension, concise) {
  # best_unit <- by_best_unit(test_value, dimension, concise = TRUE)
  best_unit <- ctmm:::unit(test_value, dimension, thresh = 1, concise = concise)
  unit_format(unit = best_unit$name, scale = 1 / best_unit$scale, digits = 2)
}
# function will take vector as input, but only return a format function which is good for scales in ggplot. will need to apply to vector again if need the formated result.
format_unit_distance_f <- function(v){
  # didn't use median because it could be near zero with positive and negative values
  pick_best_unit_f(max(abs(v))/2, dimension = "length", concise = TRUE)
}
format_seconds_f <- function(secs) {
  pick_best_unit_f(median(secs), dimension = "time", concise = FALSE)
}
format_speed_f <- function(speed) {
  pick_best_unit_f(median(speed), dimension = "speed", concise = TRUE)
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
# get single animal info in one row data frame
animal_info <- function(object) {
  # some data have one record for some individual, diff will return numeric(0), then median got NULL
  diffs <- diff(object$t)
  sampling_interval <- ifelse(length(diffs) == 0,
                   0,
                   stats::median(diffs))
  sampling_range <- max(object$t) - min(object$t)
  # above work on t which is cleaned by ctmm. original timestamp could have missing values
  t_start <- min(object$timestamp, na.rm = TRUE)
  t_end <- max(object$timestamp, na.rm = TRUE)
  dt <- data.table(identity = object@info$identity,
                   interval = sampling_interval,
                   # interval = format_seconds_f(sampling_interval)(sampling_interval),
                   duration = sampling_range,
                   # duration = format_seconds_f(sampling_range)(sampling_range),
                   sampling_start = t_start,
                   sampling_end = t_end,
                   start = format_datetime(t_start),
                   end = format_datetime(t_end))
  return(dt)
}
# merge list of telemetry obj into data frame with identity column, works with single tele obj
wrap_single_telemetry <- function(tele_obj){
  if (class(tele_obj) != "list") {
    tele_obj <- list(tele_obj)
    names(tele_obj) <- attr(tele_obj[[1]],"info")$identity
  }
  return(tele_obj)
}
# now both merge data and merge summary need to go through each individual, combine into one function. put info_selected in return list too. otherwise need to check null for null input in app init
merge_animals <- function(tele_objs) {
  tele_objs <- wrap_single_telemetry(tele_objs)
  animal_count <- length(tele_objs)
  animal_data_list <- vector(mode = "list", length = animal_count)
  animal_info_list <- vector(mode = "list", length = animal_count)
  for (i in 1:animal_count) {
    animal_data_list[[i]] <- data.table(data.frame(tele_objs[[i]]))
    animal_data_list[[i]][, identity := tele_objs[[i]]@info$identity]
    # print(i)
    animal_info_list[[i]] <- animal_info(tele_objs[[i]])
  }
  animals_data_dt <- rbindlist(animal_data_list)
  # ggplot color need a factor column. if do factor in place, legend will have factor in name
  animals_data_dt[, id := factor(identity)]
  # animals_data_dt[, timestamp := with_tz(timestamp, "UTC")]
  animals_info_dt <- rbindlist(animal_info_list)
  return(list(data = animals_data_dt, info = animals_info_dt))
}
# need the obj format, merged data frame format, level value
get_ranges_quantile <- function(tele_objs, animals, level) {
  tele_objs <- wrap_single_telemetry(tele_objs)
  ext_list <- lapply(tele_objs, extent, level = level)
  # no padding to avoid points filtered by quantile appear in plot
  x_diff_half <- max(unlist(lapply(ext_list, function(ext) { diff(ext$x) }))) / 2L
  y_diff_half <- max(unlist(lapply(ext_list, function(ext) { diff(ext$y) }))) / 2L
  # need to filter data frame too otherwise the middle point is off
  animal_list <- vector("list", length = length(tele_objs))
  for (i in seq_along(tele_objs)) {
    animal_list[[i]] <- animals[identity == names(ext_list)[i] &
                                  x >= ext_list[[i]]["min", "x"] &
                                  x <= ext_list[[i]]["max", "x"] &
                                  y >= ext_list[[i]]["min", "y"] &
                                  y <= ext_list[[i]]["max", "y"]]
  }
  animals_updated <- rbindlist(animal_list)
  dt <- animals_updated[, .(middle_x = (max(x) + min(x)) / 2,
                            middle_y = (max(y) + min(y)) / 2),
                        by = identity]
  dt[, x_start := middle_x - x_diff_half]
  dt[, x_end := middle_x + x_diff_half]
  dt[, y_start := middle_y - y_diff_half]
  dt[, y_end := middle_y + y_diff_half]
  return(dt)
}
# ggplot theme ----
bigger_theme <- theme(legend.key.size = unit(8, "mm"),
                      legend.key.height = unit(8, "mm"),
                      legend.text = element_text(size = 12),
                      axis.title = element_text(size = 14),
                      axis.text = element_text(size = 12))
bigger_key <- guides(colour = guide_legend(override.aes = list(size = 4)))
center_title <- theme(plot.title = element_text(hjust = 0.5, face = "bold"))
# movebank download ----
# always need the response content in text, also need response status
request <- function(entity_type, user, pass){
  base_url <- "https://www.movebank.org/movebank/service/direct-read?entity_type="
  url <- paste0(base_url, entity_type)
  res <- httr::GET(url, config = add_headers(user = user, password = pass))
  status <- http_status(res)$category
  if (status != "Success") {
    showNotification(paste0(http_status(res)$message, "\nCheck console for more information"),
                     duration = 6, type = "error")
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
  doc <- htmlParse(html, asText = TRUE)
  text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
  return(text)
}
get_study_detail <- function(mb_id, user, pass) {
  request(paste0("study&study_id=", mb_id), user, pass)
}
get_study_data <- function(mb_id, user, pass){
  request(paste0("event&study_id=", mb_id), user, pass)
}
