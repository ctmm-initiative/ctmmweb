# helper functions that too specific to shiny app and no need to be put inside package. Some are exported for individual uses. All functions are placed in one file for easier search for now.
# build id_pal from info. this is needed in importing tele data, and restoring session data.
build_id_pal <- function(info) {
  leaflet::colorFactor(
    scales::hue_pal()(nrow(info)), info$identity, ordered = TRUE
  )
}
# parse text input of numerical values
parse_num_text_input <- function(num_text) {
  parsed_values <- as.numeric(num_text)
  # non valid input is checked, rejected, show message
  if ((length(parsed_values) == 0) || (is.na(parsed_values))) {
    shiny::showNotification("Not a valid number",
                            duration = 5, type = "error")
    # return false to trigger req
    return(FALSE)
  } else {
    return(parsed_values)
  }
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
# home range ----
# parse text input of comma separated values
parse_comma_text_input <- function(comma_text, default_value) {
  items <- stringr::str_trim(stringr::str_split(comma_text, ",")[[1]])
  parsed_values <- as.numeric(items[items != ""])
  # non valid input is checked, rejected, show message
  if ((length(parsed_values) == 0) || (is.na(parsed_values))) {
    shiny::showNotification("Only number or comma separated numbers are accepted",
                            duration = 5, type = "error")
    return(default_value)
  } else {
    return(parsed_values)
  }
}
# for home range/occur level input, divid by 100, take default value when no valid input
parse_levels.UD <- function(levels_text) {
  parse_comma_text_input(levels_text, default_value = 95) / 100
}
# package installation time in current time zone. This is used in ui (app info dialog) and server (start info).
# get_pkg_installation_time <- function(){
#   format(file.mtime(system.file("app", package = "ctmmweb")), usetz = TRUE)
# }
# debugging helper ----
# print variable information. it's difficult to get expression name after transfered as a function parameter. so need to use name string as parameter
# not working because of environment. tricky with get.
# show_by_name <- function(var_name_string) {
#   cat(crayon::white$bgYellow$black(var_name_string), ":",
#       get(var_name_string, envir = parent.env(environment())), "\n")
# }
# catn <- function(marker, value) {
#   cat(crayon::white$bgYellow$black(marker), ":", value, "\n")
# }
