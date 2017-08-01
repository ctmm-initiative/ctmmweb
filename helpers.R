# helper functions that useful to shiny app, only need to run once
# to be placed in same directory of app.r/server.r
# function with _f postfix generate a unit_format function, which can be used in ggplot scales. To generate formated values call function on input again
# generate a unit_format function with picked unit. This only take single value, the wrappers will take a vector, pick test value and concise parameter according to data type then apply to whole vector
pick_best_unit_f <- function(test_value, dimension, concise) {
  # best_unit <- by_best_unit(test_value, dimension, concise = TRUE)
  best_unit <- ctmm:::unit(test_value, dimension, thresh = 1, concise = concise)
  scales::unit_format(unit = best_unit$name, scale = 1 / best_unit$scale, digits = 2)
}
# function will take vector as input, but only return a format function which is good for scales in ggplot. will need to apply to vector again if need the formated result.
format_distance_f <- function(v){
  # didn't use median because it could be near zero with positive and negative values
  pick_best_unit_f(max(abs(v), na.rm = TRUE)/2, dimension = "length", concise = TRUE)
}
format_seconds_f <- function(secs) {
  pick_best_unit_f(median(secs, na.rm = TRUE), dimension = "time", concise = FALSE)
}
format_speed_f <- function(speed) {
  pick_best_unit_f(median(speed, na.rm = TRUE), dimension = "speed", concise = TRUE)
}
# note we cannot use format_speed_f(vec)(vec) for value in data.table, which may call the function for single value and lost context in whole vector.
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
calculate_speed_leaving <- function(animals_dt) {
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
calculate_speed_pmin <- function(animals_dt) {
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
calculate_speed_ctmm <- function(animals_dt) {
  setkey(animals_dt, row_no)
  animals_dt[, speed := ctmm:::assign_speeds(.SD,
                                             dt = ctmm:::time_res(.SD),
                                             UERE = 0, method = "max"),
             by = identity]
  return(animals_dt)
}
# all speed calculation except ctmm assume distance have been calculated. Since we always update two together, this is not problem.
calculate_speed <- function(animals_dt) {
  setkey(animals_dt, row_no)
  # animals_dt <- calculate_speed_ctmm(animals_dt)
  # animals_dt <- calculate_speed_pmin(animals_dt)
  # my speed calculation need distance columns
  test_calc <- function(data, fun, fun_bak) {
    res <- tryCatch(fun(data), error = function(e) "error")
    if (identical(res, "error")) {
      res <- fun_bak(data)
      cat("Had error with first speed definition, use alternative instead\n")
    }
    return(res)
  }
  # we didn't use the third fallback, pmin should be robust enough.
  animals_dt <- test_calc(animals_dt,
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
  animals_data_dt <- rbindlist(animal_data_list)
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
    win_cluster_size <- detectCores(logical = FALSE)
    cat("running parallel in SOCKET cluster of", win_cluster_size, "\n")
    cl <- parallel::makeCluster(win_cluster_size, outfile = "")
    # have to export parameter too because it's not available in remote
    clusterExport(cl, c("exp_init"))
    clusterEvalQ(cl, eval(exp_init))
    res <- parLapplyLB(cl, ll, fun)
    stopCluster(cl)
  } else {
    cluster_size <- min(length(ll), detectCores(logical = FALSE) * 4)
    cat("running parallel with mclapply in cluster of", cluster_size, "\n")
    res <- parallel::mclapply(ll, fun, mc.cores = cluster_size)
  }
  return(res)
}
# sample buffalo data ----
pick_m <- function(n, m) {
  floor(seq(from = 1, to = n, length.out = m))
}
pick_m_tele <- function(tele, m) {
  # dt <- data.table(data.frame(tele[pick_m(nrow(tele), m), ]))
  # dt[, timestamp := as.character(timestamp)]
  # as.telemetry(dt)
  tele[pick_m(nrow(tele), m), ]
}
pick_m_tele_list <- function(tele_list, m) {
  lapply(buffalo, function(x) {
    pick_m_tele(x, m)
  })
}
