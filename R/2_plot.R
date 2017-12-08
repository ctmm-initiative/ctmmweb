# ggplot ----
# customize theme ----

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
                               legend.text = ggplot2::element_text(size = 12),
                               axis.title = ggplot2::element_text(size = 14),
                               axis.text = ggplot2::element_text(size = 12))
#' @rdname ggplot_customization
BIGGER_KEY <- ggplot2::guides(colour = ggplot2::guide_legend(
  override.aes = list(size = 4)))
#' @rdname ggplot_customization
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

#' Plot animal locations subset with all as background
#'
#' Draw a scatter plot of some animals' location with `ggplot2` (like plot 2
#' Overview in app). To add all animals as background, the full data set of
#' animals is also needed.
#'
#' @param selected_dt A subset of animals. A `data.table` subset taken from full
#'   data set of all animals location. Note the `id` column is factor with all
#'   animals identity as levels. So this subset still have all identities
#'   information, which is needed to color each animal consistently.
#' @param dt The full data set of all animals location in merged `data.table`
#'   format. Usually came from [merge_animals]. Default `NULL` means no full
#'   data as background.
#' @param point_size The size of point in plot. Denser plot may need smaller
#'   point size.
#' @return A ggplot object.
#' @export
#'
#' @examples
#' library(ctmm)
#' library(ctmmweb)
#' data("buffalo")
#' dt <- merge_animals(buffalo)$data
#' selected_dt <- dt[identity %in% c("Gabs", "Queen")]
#' # you can take the ggplot2 object to further customize it
#' plot_loc(selected_dt, dt) +
#'   ggplot2::ggtitle("Locations of Buffalos") +
#'   # override the default left alignment of title and make it bigger
#'   ctmmweb:::CENTER_TITLE
#' # or export plot as png
#' g <- plot_loc(selected_dt, dt)
#' # ggplot2::ggsave("test.png", g)
plot_loc <- function(selected_dt, dt = NULL, point_size = 0.1) {
  ggplot2::ggplot() +
  {if (!is.null(dt)) {
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
#'
#' @examples
#' library(ctmm)
#' library(ctmmweb)
#' data("buffalo")
#' dt <- merge_animals(buffalo)$data
#' selected_dt <- dt[identity %in% c("Gabs", "Queen")]
#' plot_loc_facet(selected_dt)
plot_loc_facet <- function(selected_dt) {
  ggplot2::ggplot(data = selected_dt, ggplot2::aes(x, y)) +
    ggplot2::geom_point(size = 0.1, ggplot2::aes(colour = id)) +
    ggplot2::scale_x_continuous(labels =
                                  ctmmweb:::format_distance_f(selected_dt$x)) +
    ggplot2::scale_y_continuous(labels =
                                  ctmmweb:::format_distance_f(selected_dt$y)) +
    ctmmweb:::factor_color(selected_dt$id) +
    ggplot2::facet_grid(id ~ .) +
    ggplot2::coord_fixed() +
    ggplot2::theme(strip.text.y = ggplot2::element_text(size = 12)) +
    ctmmweb:::BIGGER_THEME + ctmmweb:::BIGGER_KEY
}

#' Plot animal sampling time histogram in facet
#'
#' @inheritParams plot_loc
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
#' plot_time(selected_dt)
plot_time <- function(selected_dt) {
  ggplot2::ggplot(data = selected_dt,
                       ggplot2::aes(x = timestamp, fill = id)) +
    ggplot2::geom_histogram(bins = 60) +
    ctmmweb:::factor_fill(selected_dt$id) +
    ggplot2::facet_grid(id ~ .) +
    ggplot2::theme(strip.text.y = ggplot2::element_text(size = 12)) +
    ctmmweb:::BIGGER_THEME + ctmmweb:::BIGGER_KEY
}
