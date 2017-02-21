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
    # animals_data_dt[, timestamp := with_tz(timestamp, "UTC")]
    animals_info_dt <- rbindlist(animal_info_list)
  }
  return(list(data = animals_data_dt, info = animals_info_dt, 
              info_print = pretty_info(animals_info_dt)))
}

# need the obj format, merged data frame format, level value
get_ranges_quantile <- function(tele_objs, animals, level) {
  if (is.null(tele_objs)) {
    return(NULL)
  }
  if (class(tele_objs) != "list") {
    # return(merge_animals(list(tele_objs)))
  } else {
    # ext_list <- vector("list", length = length(tele_obj))
    # for (i in seq_along(tele_obj)) {
    #   ext_list[[i]] <- extent(tele_obj[[i]], level = level)
    # }
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
}

# ggplot theme ----
bigger_theme <- theme(legend.key.size = unit(8, "mm"),
                      legend.key.height = unit(8, "mm"),
                      legend.text = element_text(size = 12),
                      axis.title = element_text(size = 14),
                      axis.text = element_text(size = 12))
bigger_key <- guides(colour = guide_legend(override.aes = list(size = 4)))
center_title <- theme(plot.title = element_text(hjust = 0.5, face = "bold"))