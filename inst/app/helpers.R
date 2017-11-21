# helper functions that useful to shiny app. fold the functions in script so it's easier to copy. keep comments in code so it's easy to update code later as two versions are identical.
# to be placed in same directory of app.r/server.r
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
# km <- unit_format(unit = "km", scale = 1e-3, digits = 2)
# km()
# given a resprentative value and unit dimension, get the best unit then generate a format function to be used on a vector of similar values. other derived functions pick certain value from a vector and choose a dimension.
# extra round option when really needed
pick_best_unit_f <- function(test_value, dimension, concise, round = FALSE) {
  # best_unit <- by_best_unit(test_value, dimension, concise = TRUE)
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
  dt <- data.table(identity = object@info$identity,
                   interval_s = sampling_interval,
                   interval = format_seconds_f(sampling_interval)(sampling_interval),
                   duration_s = sampling_range,
                   duration = format_seconds_f(sampling_range)(sampling_range),
                   sampling_start = t_start,
                   sampling_end = t_end,
                   start = format_datetime(t_start),
                   end = format_datetime(t_end),
                   points = nrow(object))
  return(dt)
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
# taking input directly, which could be tele obj or a list of tele obj. in server.R input afte wrap named as tele_list
tele_list_info <- function(tele_objs){
  tele_list <- wrap_single_telemetry(tele_objs)
  animal_info_list <- lapply(tele_list, single_tele_info)
  rbindlist(animal_info_list)
}

# distance and speed values need to be updated after outlier removal
# calculate_distance_all <- function(animals_dt) {
#   animals_dt[, `:=`(median_x = median(x), median_y = median(y)),
#              by = identity]
#   animals_dt[, distance_center := sqrt((x - median_x) ** 2 +
#                                          (y - median_y) ** 2)]
#   return(animals_dt)
# }
# calculate median center based on time clusters
calculate_distance <- function(animals_dt) {
  find_boundary <- function(data) {
    time_gap_threshold <- quantile((diff(data$t)), 0.8) * 100
    # increase 1 sec because interval boundry is right open [ )
    data[inc_t > time_gap_threshold, t + 1]
  }
  # need inc_t, so get these column first. always calculate distance then speed
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
  # x[i] + inc[i] = x[i+1], note by id
  # animals_dt[, `:=`(inc_x = shift(x, 1L, type = "lead") - x,
  #                   inc_y = shift(y, 1L, type = "lead") - y,
  #                   inc_t = shift(t, 1L, type = "lead") - t), by = id]
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
  # animals_dt[, `:=`(inc_x = shift(x, 1L, type = "lead") - x,
  #                   inc_y = shift(y, 1L, type = "lead") - y,
  #                   inc_t = shift(t, 1L, type = "lead") - t), by = id]  # note by id
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
# all speed calculation except ctmm assume distance have been calculated. Since we always update two together, this is not problem.
calculate_speed <- function(animals_dt, device_error) {
  setkey(animals_dt, row_no)
  # animals_dt <- calculate_speed_ctmm(animals_dt)
  # animals_dt <- calculate_speed_pmin(animals_dt)
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
# merge obj list into data frame with identity column, easier for ggplot and summary. go through every obj to get data frame and metadata, then combine the data frame into data, metadata into info.
# tele objs to data.table
# assuming row order by timestamp and identity in same order with tele obj.
tele_list_to_dt <- function(tele_objs) {
  tele_list <- wrap_single_telemetry(tele_objs)
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
# tele objs to data.table and info
merge_animals <- function(tele_objs) {
  return(list(data = tele_list_to_dt(tele_objs),
              info = tele_list_info(tele_objs)))
}
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
bigger_theme <- theme(legend.key.size = unit(8, "mm"),
                      legend.key.height = unit(8, "mm"),
                      legend.text = element_text(size = 12),
                      axis.title = element_text(size = 14),
                      axis.text = element_text(size = 12))
bigger_key <- guides(colour = guide_legend(override.aes = list(size = 4)))
center_title <- theme(plot.title = element_text(hjust = 0.5, face = "bold"))
# map color to a factor with unused levels included, but don't show them in legend.
# note need to use dt$id format. note the mapping is provided in aes(color/fill = xx) already, this is to override some options.
factor_mapper <- function(fac, FUN) {
  FUN(drop = FALSE, breaks = levels(droplevels(fac)))
}
factor_color <- function(fac) {
  # scale_colour_hue(drop = FALSE, breaks = levels(droplevels(fac)))
  factor_mapper(fac, scale_colour_hue)
}
# note for fill colors we need different function
factor_fill <- function(fac) {
  # scale_fill_hue(drop = FALSE, breaks = levels(droplevels(fac)))
  factor_mapper(fac, scale_fill_hue)
}
factor_alpha <- function(fac) {
  # scale_alpha_discrete(drop = FALSE, breaks = levels(droplevels(fac)))
  factor_mapper(fac, scale_alpha_discrete)
}
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
# divide x into interval_count intervals ----
# Taken from https://github.com/wch/r-source/blob/trunk/src/library/base/R/cut.R
divide <-
  function (x, interval_count)
  {
    if (is.na(interval_count) || interval_count < 2L)
      stop("invalid number of intervals")
    nb <- as.integer(interval_count + 1) # one more than #{intervals}
    dx <- diff(rx <- range(x, na.rm = TRUE))
    if(dx == 0) {
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
  return(cut(x, as_datetime(brks)))
}
divide_date_time <- function(x, interval_count) {
  return(as_datetime(divide(as.numeric(x), interval_count)))
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
  color_bin_labels <- paste0(">= ", head(color_bin_breaks_units, -1L))
  animals_dt[, paste0(col_name, "_color_factor") :=
               cut(animals_dt[[col_name]], breaks = color_bin_breaks,
                   labels = color_bin_labels, right = FALSE)]  # closed on left to include 0
  # remove empty bins in labels
  his <- hist(animals_dt[[col_name]], breaks = color_bin_breaks, plot = FALSE)
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
exp_init <<- expression({
  library(ctmm)
})
# generate a new list, each item have two item from list a and b. use a b because we want to name the item, but difficult to use original name of input
align_list <- function(list_a, list_b) {
  stopifnot(length(list_a) == length(list_b))
  # use lapply instead of for only because we can get a list without initialization
  lapply(seq_along(list_a), function(i) {
    list(a = list_a[[i]], b = list_b[[i]])
  })
}
# cannot transfer cluster size as parameter, because of environment?
para_ll <- function(ll, fun) {
  sysinfo <- Sys.info()
  if (sysinfo["sysname"] == "Windows")  {  # Darwin / Windows
    win_cluster_size <- min(length(ll), detectCores())
    cat(crayon::inverse("running parallel in SOCKET cluster of", win_cluster_size, "\n"))
    cl <- parallel::makeCluster(win_cluster_size, outfile = "")
    # have to export parameter too because it's not available in remote
    clusterExport(cl, c("exp_init"))
    clusterEvalQ(cl, eval(exp_init))
    res <- parLapplyLB(cl, ll, fun)
    stopCluster(cl)
  } else {
    cluster_size <- min(length(ll), detectCores(logical = FALSE) * 4)
    cat(crayon::inverse("running parallel with mclapply in cluster of", cluster_size, "\n"))
    res <- parallel::mclapply(ll, fun, mc.cores = cluster_size)
  }
  return(res)
}
# cannot use select_models since that was a reactive expression to select model results
fit_models <- function(tele_guess) {
  ctmm.select(tele_guess$a, CTMM = tele_guess$b,
              trace = TRUE, verbose = TRUE)
}
# wrapper to avoid function object as parameter
para_ll_fit <- function(tele_list) {
  para_ll(tele_list, fit_models)
}
# occurrence
ud_calc <- function(ud_para_list) {
  occurrence(ud_para_list$a, ud_para_list$b)
}
para_ll_ud <- function(ud_para_list) {
  para_ll(ud_para_list, ud_calc)
}
# sample buffalo data ----
pick_m_tele <- function(tele, m) {
  tele[floor(seq(from = 1, to = nrow(tele), length.out = m)), ]
}
pick_m_tele_list <- function(tele_list, m) {
  lapply(buffalo, function(x) {
    pick_m_tele(x, m)
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
  setnames(dof_dt, names(dof_dt), str_c("DOF ", names(dof_dt)))
  setnames(ci_dt, "rn", "estimate")
  ci_dt[estimate %in% c("low", "high"), estimate := str_c("CI ", estimate)]
  # with SI units, we only need to know which type of unit. the actual format need to happen after the whole table is built. so we only need a mapping from col name to proper unit function later.
  setnames(ci_dt, names(ci_dt), str_replace_all(names(ci_dt), "\\s\\(.*", ""))
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
model_list_dt_to_model_summary_dt <- function(models_dt) {
  # make copy first because we will remove column later
  # a list of converted summary on each model
  model_summary_dt_list <- lapply(1:nrow(models_dt), function(i) {
    summary_dt <- ctmm_obj_to_summary_dt(models_dt$model[[i]])
    summary_dt[, model_no := i]
  })
  model_summary_dt <- rbindlist(model_summary_dt_list, fill = TRUE)
  res_dt <- merge(models_dt[, .(identity, model_name, model_no, dAICc)],
                  model_summary_dt,
                  by = "model_no")
  # res_dt[, color_target := str_c(identity, " - " , estimate)]
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
  new_cols <- names(dt)[str_detect(names(dt), "_units")]
  old_cols <- str_replace_all(new_cols, "_units", "")
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
  res_dt[str_detect(`tau velocity`, "^NA "),
         c("tau velocity", "speed") := ""]
  res_dt[str_detect(estimate, "CI"),
         c("DOF mean", "DOF area") := NA_real_]
  # add model full name col so it can be used to create model color palette
  res_dt[, full_name := str_c(identity, " - ", model_name)]
}
# from akde result model list to data table with models in list column
build_hrange_list_dt <- function(selected_dt, selected_hrange_list) {
  dt <- copy(selected_dt)
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
  res_dt[str_detect(estimate, "CI"),
         c("DOF area", "DOF bandwidth") := NA_real_]
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
  if (str_trim(levels_text) == "") {
    return(0.95)
  } else {
    items <- str_trim(str_split(levels_text, ",")[[1]])
    as.numeric(items[items != ""]) / 100
  }
}
# file is the user chosen file name determined in download, need to prepare a file, copy to that path. write_f is a function that write files, take folder_path determined in build_zip as parameter.
build_shapefile_zip <- function(file, write_f, session_tmpdir) {
  # use time till min in zip name, use second in folder name, otherwise this function got run twice, will have error running 2nd time writing to same folder.
  current_time <- current_timestamp()  # need this in zip name so save it
  folder_path <- file.path(session_tmpdir, str_c("Range_", current_time))
  create_folder(folder_path)
  write_f(folder_path)
  zip_path <- compress_folder(folder_path,
                         paste0("Home Range ", current_time, ".zip"))
  file.copy(zip_path, file)
}
# map ----
init_base_maps <- function(tiles_info) {
  leaf <- leaflet(options = leafletOptions(attributionControl = FALSE))
  for (prov in tiles_info$here) {
    leaf <- leaf %>% addProviderTiles(providers[[prov]], group = prov,
                                      options = providerTileOptions(
                                        detectRetina = TRUE,
                                        app_id = tiles_info$here_app_id,
                                        app_code = tiles_info$here_app_code))
  }
  for (prov in tiles_info$open) {
    leaf <- leaf %>% addProviderTiles(providers[[prov]], group = prov)
  }
  return(leaf)
}
add_measure <- function(leaf) {
  leaf %>%
    addMeasure(
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
    addSimpleGraticule(interval = 1, showOriginLabel = FALSE,
                       redraw = "moveend", group = grid_group)
  # add each individual as a layer
  # for loop is better than lapply since we don't need to use <<-
  for (current_id in info$identity) {
    leaf <- leaf %>%
      addCircles(data = dt[identity == current_id], group = current_id,
                 lng = ~longitude, lat = ~latitude, radius = 0.3, weight = 2,
                 color = ~id_pal(id), opacity = 0.4, fillOpacity = 0.05)
  }
  leaf %>%
    addLegend(pal = id_pal, values = info$identity,
                     position = "topleft") %>%
    addScaleBar(position = "bottomleft") %>%
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
  hrange_spdf <- spTransform(SpatialPolygonsDataFrame.UD(hrange,
                                                         level.UD = hr_levels),
                             CRS("+proj=longlat +datum=WGS84"))
  ML_indice <- seq(2, length(hrange_spdf), by = 3)
  hrange_spdf_ML <- hrange_spdf[ML_indice, ]
  hrange_spdf_other <- hrange_spdf[-ML_indice, ]
  leaf %>%
    addPolygons(data = hrange_spdf_ML, weight = 2.2, opacity = 0.7,
                fillOpacity = 0.05, color = hr_color, group = group_name) %>%
    addPolygons(data = hrange_spdf_other, weight = 1.2, opacity = 0.4,
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
    hsv_vec <- rgb2hsv(col2rgb(base_color))[, 1]
    return(hsv(hsv_vec[1], hsv_vec[2], seq(1, 0.5, length.out = count)))
  }
}
# added base map layer control
add_heat <- function(leaf, dt, tiles_info) {
  leaf %>%
    addSimpleGraticule(interval = 1, showOriginLabel = FALSE,
                       redraw = "moveend", group = grid_group) %>%
    addHeatmap(data = dt, lng = ~longitude, lat = ~latitude,
               blur = 8, max = 1, radius = 5, group = "Heatmap") %>%
    addScaleBar(position = "bottomleft") %>%
    add_measure() %>%
    addLayersControl(
      baseGroups = c(tiles_info$here, tiles_info$open),
      overlayGroups = c(grid_group, "Heatmap"),
      options = layersControlOptions(collapsed = FALSE))
}
get_bounds <- function(dt) {
  return(list(lng1 = min(dt$longitude), lat1 = min(dt$latitude),
              lng2 = max(dt$longitude), lat2 = max(dt$latitude)))
}
apply_bounds <- function(leaf, bounds) {
  fitBounds(leaf, bounds$east, bounds$north, bounds$west, bounds$south)
}
