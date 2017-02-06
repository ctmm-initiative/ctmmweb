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
  df <- data.frame(identity = object@info$identity,
                   sampling_interval_value = sampling_interval$value,
                   sampling_interval_unit = sampling_interval$unit,
                   sampling_range_value = sampling_range$value,
                   sampling_range_unit = sampling_range$unit,
                   sampling_start = t_start,
                   sampling_end = t_end)
  return(df)
}
# merge list of telemetry obj into data frame with identity column, works with single tele obj
# now both merge data and merge summary need to go through each individual, combine into one function
merge_animals <- function(tele_obj) {
  if (class(tele_obj) != "list") {
    return(merge_animals(list(tele_obj)))
  } else {
    animal_count <- length(tele_obj)
    animal_data_list <- vector(mode = "list", length = animal_count)
    animal_info_list <- vector(mode = "list", length = animal_count)
    for (i in 1:animal_count) {
      animal_data_list[[i]] <- data.table(data.frame(tele_obj[[i]]))
      animal_data_list[[i]][, identity := tele_obj[[i]]@info$identity]
      animal_info_list[[i]] <- animal_info(tele_obj[[i]])
    }
    animals_data_dt <- rbindlist(animal_data_list)
    # ggplot color need a factor column. if do factor in place, legend will have factor in name
    animals_data_dt[, id := factor(identity)]
    animals_info_dt <- rbindlist(animal_info_list)
  }
  return(list(data = animals_data_dt, info = animals_info_dt))
}