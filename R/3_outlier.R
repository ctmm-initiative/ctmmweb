# outlier ----
color_break <- function(bin_count, animals_dt, col_name, unit_formatter) {
  dt <- copy(animals_dt)
  # should not modify input dt
  color_bin_breaks <- divide(dt[[col_name]], bin_count)
  # cut generate roughly equal length breaks (with adjustment to include extremes, no need to change this) which sometimes have a negative first value, not good for distanc/speed. shift the breaks so that the first value is 0.
  if (color_bin_breaks[1] < 0) {
    color_bin_breaks <- color_bin_breaks + abs(color_bin_breaks[1])
  }
  # need to change the default of (b1, b2] to [b1, b2), otherwise 0 will not be included.
  # format label to include unit. difficult to separate unit with value, could only include left side though.
  vec_formatter <- unit_formatter(color_bin_breaks)
  color_bin_breaks_units <- vec_formatter(color_bin_breaks)
  color_bin_labels <- paste0(">= ", utils::head(color_bin_breaks_units, -1L))
  dt[, paste0(col_name, "_color_factor") :=
       cut(dt[[col_name]], breaks = color_bin_breaks,
           labels = color_bin_labels, right = FALSE)]  # closed on left to include 0
  # remove empty bins in labels
  his <- graphics::hist(dt[[col_name]], breaks = color_bin_breaks, plot = FALSE)
  # with n+1 breaks for n interval/bin count, using count index on breaks will get the left side for each break
  non_empty_indice <- which(his$counts != 0)
  # need both side to label the plot properly. this only works when 2 vectors have same length. but I don't like the other method of recyling index new_vec[c(TRUE, FALSE)]
  non_empty_breaks <- c(rbind(color_bin_breaks[non_empty_indice],
                              color_bin_breaks[non_empty_indice + 1]))
  # still use the animals_dt name because too many plot code is using that name, too much hassle to change them. just need to know this animals_dt is a local version with added columns
  return(list(animals_dt = dt,
              color_bin_breaks = color_bin_breaks,
              non_empty_breaks = non_empty_breaks,
              vec_formatter = vec_formatter))
}
# add back before export and saving to be movebank compatible
add_outliers_back <- function(dt, ids, outliers) {
  # if outliers is empty
  if (is.null(outliers)) {
    return(dt)
  }
  cols <- names(dt)  # cannot use names call with .. directly
  removed_outliers <- outliers[identity %in% ids, ..cols]
  removed_outliers[, manually_marked_outlier := TRUE]
  new_dt <- rbindlist(list(dt, removed_outliers), fill = TRUE)
  # cannot be NA, which will cause problem in as.telemetry import
  new_dt[is.na(manually_marked_outlier), manually_marked_outlier := FALSE]
  # need to sort otherwise outliers will be at bottom
  setkey(new_dt, row_no)
}
