# ggplot ----
BIGGER_THEME <- ggplot2::theme(legend.key.size = grid::unit(8, "mm"),
                               legend.key.height = grid::unit(8, "mm"),
                               legend.text = ggplot2::element_text(size = 12),
                               axis.title = ggplot2::element_text(size = 14),
                               axis.text = ggplot2::element_text(size = 12))
BIGGER_KEY <- ggplot2::guides(colour = ggplot2::guide_legend(
  override.aes = list(size = 4)))
CENTER_TITLE <- ggplot2::theme(plot.title = ggplot2::element_text(
  hjust = 0.5, face = "bold"))
# map color to a factor with unused levels included, but don't show them in legend.
# note need to use dt$id format. note the mapping is provided in aes(color/fill = xx) already, this is to override some options.
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
  factor_mapper(fac, ggplot2::scale_alpha_discrete)
}
# plot 2 overview

#' Plot Animal Locations Subset With Background
#'
#' Draw a scatter plot of some animals' location with `ggplot2`. To color each
#' individual consistently and add all animals as background, the full data set
#' of animals is also needed.
#'
#' @param selected_dt A subset of animals. A `data.table` subset taken from
#'   `dt`.
#' @param dt The full data set of all animals location in merged `data.table`
#'   format. Usually came from [merge_animals].
#' @param point_size The size of point in plot. Denser plot may need smaller
#'   point size.
#' @param overlay_all Draw full data set as background
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' library(ctmm)
#' library(ctmmweb)
#' data("buffalo")
#' dt <- merge_animals(buffalo)$data
#' selected_dt <- dt[identity %in% c("Gabs", "Queen")]
#' plot_loc(selected_dt, dt)
#'
plot_loc <- function(selected_dt, dt, point_size = 0.1, overlay_all = TRUE) {
  g <- ggplot2::ggplot() +
  {if (overlay_all) {
    ggplot2::geom_point(data = dt, ggplot2::aes(x, y),
                        size = point_size, alpha = 0.6,
                        colour = "gray")
  }} +
    ggplot2::geom_point(data = selected_dt, ggplot2::aes(x, y, colour = id),
                        size = point_size, alpha = 0.7) +
    ggplot2::coord_fixed() +
    # ggplot2::coord_fixed(xlim = location_plot_gg_range$x,
    #                      ylim = location_plot_gg_range$y) +
    ctmmweb:::factor_color(selected_dt$id) +  # the color is right because id is factor, its levels included all values from full dataset ids.
    ggplot2::scale_x_continuous(labels =
                                  ctmmweb:::format_distance_f(selected_dt$x)) +
    ggplot2::scale_y_continuous(labels =
                                  ctmmweb:::format_distance_f(selected_dt$y)) +
    ggplot2::theme(legend.position = "top",
                   legend.direction = "horizontal") +
    ctmmweb:::BIGGER_THEME + ctmmweb:::BIGGER_KEY
  g
}