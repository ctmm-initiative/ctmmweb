# helper functions that useful to shiny app, only need to run once
# to be placed in same directory of app.r/server.r
# CHOOSE BEST UNITS FOR A LIST OF DATA. based on ctmm:unit, changed if else to switch
natural_unit <- function(data, dimension, thresh = 1, concise = FALSE) {
  switch(
    dimension,
    length = {
      name.list <- c("meters", "kilometers")
      abrv.list <- c("m", "km")
      scale.list <- c(1, 1000)
    },
    area = {
      name.list <- c("square meters", "hectares", "square kilometers")
      abrv.list <- c("m^2", "hm^2", "km^2")
      scale.list <- c(1, 100 ^ 2, 1000 ^ 2)
    },
    time = {
      name.list <- c("seconds", "minutes", "hours", "days", "months", "years")
      abrv.list <- c("sec", "min", "hr", "day", "mon", "yr")
      scale.list <- c(1, 60 * c(1, 60 * c(1, 24 * c(1, 29.53059, 365.24))))
    },
    speed = {
      name.list <- c("meters/day", "kilometers/day")
      abrv.list <- c("m/day", "km/day")
      scale.list <- c(1, 1000) / (60 * 60 * 24)
    },
    diffusion = {
      name.list <-
        c("square meters/day", "hectares/day", "square kilometers/day")
      abrv.list <- c("m^2/day", "hm^2/day", "km^2/day")
      scale.list <- c(1, 100 ^ 2, 1000 ^ 2) / (60 * 60 * 24)
    }
  )
  max.data <- max(abs(data))
  if(concise) { name.list <- abrv.list }
  # choose most parsimonious units
  matches <- max.data > thresh * scale.list
  if(any(matches)){
    matches <- (1:length(matches))[matches]
    matches <- last(matches)
  } else { matches <- 1 }
  name <- name.list[matches]
  scale <- scale.list[matches]
  return(list(scale=scale,name=name))
}
# return a list with data value and natural unit
# tried data frame but then data frame don't have data metadata in column names
by_natural_unit <- function(data, dimension, thresh = 1, concise = FALSE) {
  test <- natural_unit(data, dimension, thresh = 1, concise = FALSE)
  return(list(value = data / test$scale, unit = test$name))
}

# get single animal info in one row data frame
animal_info <- function(object) {
  t_diff <- stats::median(diff(object$t))
  sampling_interval <- by_natural_unit(t_diff, "time")
  t_range <- max(object$t) - min(object$t)
  sampling_range <- by_natural_unit(t_range, "time")
  # above work on t which is cleaned by ctmm. original timestamp could have missing values
  t_start <- min(object$timestamp, na.rm = TRUE)
  t_end <- max(object$timestamp, na.rm = TRUE)
  dt <- data.table(identity = object@info$identity,
                   sampling_interval_value = sampling_interval$value,
                   sampling_interval_unit = sampling_interval$unit,
                   sampling_range_value = sampling_range$value,
                   sampling_range_unit = sampling_range$unit,
                   sampling_start = t_start,
                   sampling_end = t_end)
  return(dt)
}
# pretty print summary table. to avoid duplicate computation, use info slot or animal summary table as input, and merge function will return this by default
# shiny DT will print numbers with lots of digits. there is formating function but we need a number and unit combination which doesn't work with the formating function. Have to format the number here.
pretty_info <- function(info) {
  dt <- info
  dt[, Identity := identity]
  dt[, Interval := paste(round(sampling_interval_value, 1),
                                  sampling_interval_unit)]
  dt[, Time_range := paste(round(sampling_range_value, 1), 
                               sampling_range_unit)]
  dt[, Start := format(sampling_start, "%Y-%m-%d %H:%M")]
  dt[, End := format(sampling_end, "%Y-%m-%d %H:%M")]
  return(dt[, .(Identity, Start, End, Interval, Time_range)])
}
# merge list of telemetry obj into data frame with identity column, works with single tele obj
# now both merge data and merge summary need to go through each individual, combine into one function. put info_selected in return list too. otherwise need to check null for null input in app init
merge_animals <- function(tele_objs) {
  # if we check null input here, no need to check it in shiny.
  if (is.null(tele_objs)) {
    return(NULL)
  }
  if (class(tele_objs) != "list") {
    return(merge_animals(list(tele_objs)))
  } else {
    animal_count <- length(tele_objs)
    animal_data_list <- vector(mode = "list", length = animal_count)
    animal_info_list <- vector(mode = "list", length = animal_count)
    for (i in 1:animal_count) {
      animal_data_list[[i]] <- data.table(data.frame(tele_objs[[i]]))
      animal_data_list[[i]][, identity := tele_objs[[i]]@info$identity]
      animal_info_list[[i]] <- animal_info(tele_objs[[i]])
    }
    animals_data_dt <- rbindlist(animal_data_list)
    # ggplot color need a factor column. if do factor in place, legend will have factor in name
    animals_data_dt[, id := factor(identity)]
    animals_info_dt <- rbindlist(animal_info_list)
  }
  return(list(data = animals_data_dt, info = animals_info_dt, 
              info_print = pretty_info(animals_info_dt)))
}
# ggplot theme
bigger_theme <- theme(legend.key.size = unit(8, "mm"),
                      legend.key.height = unit(8, "mm"),
                      legend.text = element_text(size = 12),
                      axis.title = element_text(size = 14),
                      axis.text = element_text(size = 12))
bigger_key <- guides(colour = guide_legend(override.aes = list(size = 4)))
# # given a vector, get new limit to make centroid at center
# expand_1D_center <- function(vec){
#   center <- median(vec)
#   new_diff <- max(center - min(vec), 
#                   max(vec) - center)
#   return(c(new_min = center - new_diff, 
#               new_max = center + new_diff))
# }
# # given x y vectors, get new x y lim to make centroid center
# # using x y to make it flexible for different data framing column names
# expand_2D_center <- function(x_vec, y_vec){
#   return(list(xlim = expand_1D_center(x_vec),
#               ylim = expand_1D_center(y_vec)))
# }
# get new expand ranges for all individual plots. all should have same range but different start and end
get_ranges <- function(animals) {
  dt <- animals[, .(max_x = max(x), 
                    min_x = min(x), 
                    max_y = max(y), 
                    min_y = min(y), 
                    median_x = median(x), 
                    median_y = median(y)),
                    by = identity]
  dt[, range_x := max_x - min_x]
  dt[, range_y := max_y - min_y]
  # note the max here is across all range_x since no by clause. added 1.05 for padding to use expand = FALSE without overlap and keep axes size
  dt[, new_diff_x := max(median_x - min_x, max_x - median_x) * 1.05]
  dt[, new_diff_y := max(median_y - min_y, max_y - median_y) * 1.05]
  dt[, x_start := median_x - new_diff_x]
  dt[, x_end := median_x + new_diff_x]
  dt[, y_start := median_y - new_diff_y]
  dt[, y_end := median_y + new_diff_y]
  return(dt)
}
# given a zooming mutipliers in (1, 100), scale one axis range
# zoom_in_range <- function(left, right, times) {
#   new_range <- (right - left) / times
#   # note mean take a vector, not list of items in parameters
#   center <- (right + left) / 2 
#   return(c(new_left = center - new_range / 2,
#            new_right = center + new_range / 2))
# }
# given a zooming ratio in (0.01, 1), scale portion of one axis range
zoom_in_range <- function(left, right, ratio) {
  new_range <- (right - left) * ratio
  # note mean take a vector, not list of items in parameters
  center <- (right + left) / 2 
  return(c(new_left = center - new_range / 2,
           new_right = center + new_range / 2))
}