# helper functions that useful to shiny app, only need to run once
# to be placed in same directory of app.r/server.r
# merge list of telemetry obj into data frame with identity column
merge_animals_data <- function(tele_obj) {
  if (class(tele_obj) != "list") {
    animals_dt <- data.table(data.frame(tele_obj))
    animals_dt[, identity := tele_obj@info$identity]
  } else {
    animal_count <- length(tele_obj)
    animal_list <- vector(mode = "list", length = animal_count)
    for (i in 1:animal_count) {
      animal_list[[i]] <- data.table(data.frame(tele_obj[[i]]))
      animal_list[[i]][, identity := tele_obj[[i]]@info$identity]
    }
    animals_dt <- rbindlist(animal_list)
  }
  animals_dt[, id := factor(identity)]
  return(animals_dt)
}
# merge data, create summaries
merge_animals <- function(tele_obj) {
  animals_dt <- merge_animals_data(tele_obj)
  summaries_dt <- animals_dt[, list(sampling_internval = .(dseconds(median(diff(t)))),
                                    sampling_start = min(timestamp),
                                    sampling_end = max(timestamp)
  ),
  by = identity]
  summaries_dt[, sampling_duration := as.duration(
    sampling_end - sampling_start), by = identity]
  return(list("animals" = animals_dt, "summaries" = summaries_dt))
}
