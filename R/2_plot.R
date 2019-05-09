# ggplot ----
# customize theme ----
# BIGGER_THEME is used in main plots, where the legend is for id, title can be removed. outlier and time subsetting plot need legend, which didn't use this.

#' Override some ggplot2 default theme settings
#'
#' - `ctmmweb:::BIGGER_THEME` increase the default ggplot2 legend, axis size
#' - `ctmmweb:::BIGGER_KEY` increase default legend key size
#' - `ctmmweb:::CENTER_TITLE` place title in center and bold
#'
#' @format A `ggplot2` function call of `ggplot2::theme` or `ggplot2::guides`
#' @rdname ggplot_customization
BIGGER_THEME <- ggplot2::theme(legend.key.size = grid::unit(8, "mm"),
                               legend.key.height = grid::unit(8, "mm"),
                               legend.text = ggplot2::element_text(size = 15),
                               legend.title = ggplot2::element_blank(),
                               axis.title = ggplot2::element_text(size = 22),
                               axis.text = ggplot2::element_text(size = 15))
#' @rdname ggplot_customization
BIGGER_KEY <- ggplot2::guides(colour = ggplot2::guide_legend(
  override.aes = list(size = 4)))
#' @rdname ggplot_customization
CENTER_TITLE <- ggplot2::theme(plot.title = ggplot2::element_text(
  hjust = 0.5, face = "bold"))
# coord fixed ----
# need this to hide the warning https://github.com/tidyverse/ggplot2/issues/2799
# this is only needed when we added coord_fixed in function, then added another call in app to zoom. other single use in regular plot don't need this.
coord_fixed_no_warning <- ggplot2::coord_fixed()
coord_fixed_no_warning$default <- TRUE
# color mapper ----
# map color to a factor with unused levels included, but don't show them in legend.
# note need to use loc_data$id format. note the mapping is provided in aes(color/fill = xx) already, this is to override some options.
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
  # ggplot2 3.0 give warnings, have to suppress
  suppressWarnings(factor_mapper(fac, ggplot2::scale_alpha_discrete))
}
# plot 2 overview

#' Plot animal locations subset with all as background
#'
#' Draw a scatter plot of some animals' location with `ggplot2` (like plot 2
#' Overview in app). To add all animals as background, the full data set of
#' animals is also needed.
#'
#' @param loc_data_subset A subset of animals. A `data.table` subset taken from
#'   full data set of all animals location `loc_data`. Note the `id` column is
#'   factor with all animals identity as levels. So this subset still have all
#'   identities information, which is needed to color each animal consistently.
#' @param loc_data The full data set of all animals location in combined
#'   `data.table` format, usually came from [combine()]. If background is not
#'   needed, this parameter can be skipped and take the default `NULL` value. In
#'   this case better use named parameter for `point_size` if needed, like
#'   `plot_loc(loc_data_subset, point_size = 0.2)` instead of
#'   `plot_loc(loc_data_subset, 0.2)` otherwise the second parameter will be
#'   interpreted as `loc_data`.
#' @param point_size The size of point in plot. Denser plot may need smaller
#'   point size.
#' @return A ggplot object.
#' @export
plot_loc <- function(loc_data_subset, loc_data = NULL, point_size = 0.1) {
  ggplot2::ggplot() +
  {if (!is.null(loc_data)) {
    ggplot2::geom_point(data = loc_data, ggplot2::aes(x, y),
                        size = point_size, alpha = 0.6,
                        colour = "gray")
  }} +
    ggplot2::geom_point(data = loc_data_subset, ggplot2::aes(x, y, colour = id),
                        size = point_size, alpha = 0.7) +
    coord_fixed_no_warning +
    # ggplot2::coord_fixed(xlim = location_plot_gg_range$x,
    #                      ylim = location_plot_gg_range$y) +
    ctmmweb:::factor_color(loc_data_subset$id) +  # the color is right because id is factor, its levels included all values from full dataset ids.
    ggplot2::scale_x_continuous(labels =
                    ctmmweb:::format_distance_f(loc_data_subset$x)) +
    ggplot2::scale_y_continuous(labels =
                    ctmmweb:::format_distance_f(loc_data_subset$y)) +
    ggplot2::theme(legend.position = "top",
                   legend.direction = "horizontal") +
    ctmmweb:::BIGGER_THEME + ctmmweb:::BIGGER_KEY
}

#' Plot animal locations in facet
#'
#' Each animal is plotted separately (facet). The axis are aligned so it's easy
#' to compare them.
#'
#' @inheritParams plot_loc
#'
#' @return A ggplot object.
#' @export
plot_loc_facet <- function(loc_data_subset) {
  ggplot2::ggplot(data = loc_data_subset, ggplot2::aes(x, y)) +
    ggplot2::geom_point(size = 0.1, ggplot2::aes(colour = id)) +
    ggplot2::scale_x_continuous(labels =
                    ctmmweb:::format_distance_f(loc_data_subset$x)) +
    ggplot2::scale_y_continuous(labels =
                    ctmmweb:::format_distance_f(loc_data_subset$y)) +
    ctmmweb:::factor_color(loc_data_subset$id) +
    ggplot2::facet_grid(id ~ .) +
    coord_fixed_no_warning +
    ggplot2::theme(strip.text.y = ggplot2::element_text(size = 12)) +
    ctmmweb:::BIGGER_THEME + ctmmweb:::BIGGER_KEY
}

#' Plot animal sampling time histogram in facet
#'
#' @inheritParams plot_loc
#'
#' @return A ggplot object.
#' @export
plot_time <- function(loc_data_subset) {
  ggplot2::ggplot(data = loc_data_subset,
                  ggplot2::aes(x = timestamp, fill = id)) +
    ggplot2::geom_histogram(bins = 60) +
    ctmmweb:::factor_fill(loc_data_subset$id) +
    ggplot2::facet_grid(id ~ .) +
    ggplot2::theme(strip.text.y = ggplot2::element_text(size = 12)) +
    ctmmweb:::BIGGER_THEME + ctmmweb:::BIGGER_KEY
}
