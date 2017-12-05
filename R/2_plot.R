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