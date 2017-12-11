
#' Plot a group of empirical or modeled variograms.
#'
#' @param vario_list list of `ctmm::variogram`. The `names` of list will be used
#'   as figure title if in empirical variogram mode.
#' @param model_list `CTMM` models list. The `names` of list are needed for
#'   figure titles. Draw modeled variogram if provided. The models list should
#'   match `vario_list` in length and animal, so that `i`th model is for `i`th
#'   variogram.
#'   Possible values:
#'   - `NULL`, only draw empirical variogram. `vario_list` names will
#'   be used as figure title.
#'   - list of guesstimated models from `ctmm::ctmm.guess` on `vario_list`,
#'   overlay guesstimate variogram.
#'   - list of fitted models from `ctmm::ctmm.select` on `vario_list`, overlay
#'   modeled variogram.
#' @param fraction Fraction of time-lag range, 0 ~ 1.
#' @param relative_zoom
#' - Relative mode zoom every plot by fraction of their own Time-lag range. The
#' X, Y axes are not synced.
#' - Absolute mode operate on the max Time-lag range individual in group, and
#' all others scaled with same X, Y axes for easier comparison.
#' @param model_color The color of model variogram
#' @param columns The columns of the group plot layout.
#'
#' @export
plot_vario <- function(vario_list, model_list = NULL,
                       fraction = 0.5, relative_zoom = TRUE,
                       model_color = "blue", columns = 2){
  title_vec <- if (is.null(model_list)) names(vario_list)
                   else names(model_list)
  if (is.null(title_vec)) {
    stop("names of input list is needed for figure title")
  }
  row_count <- ceiling(length(vario_list) / columns)
  def.par <- graphics::par(no.readonly = TRUE)
  graphics::par(mfrow = c(row_count, columns),
                mar = c(5, 5, 4, 1), ps = 18, cex = 0.72, cex.main = 0.9)
  if (!relative_zoom) {
    max.lag <- max(sapply(vario_list, function(v){ last(v$lag) } ))
    xlim <- max.lag * fraction
    vario_zoomed_list <- lapply(vario_list,
                                function(vario) vario[vario$lag <= xlim, ])
    extent_tele <- ctmm::extent(vario_zoomed_list)
    for (i in seq_along(vario_zoomed_list)) {
      plot(vario_zoomed_list[[i]], CTMM = model_list[[i]],
           col.CTMM = model_color, fraction = 1,
           xlim = c(0, extent_tele["max", "x"]),
           ylim = c(0, extent_tele["max", "y"]))
      graphics::title(title_vec[i])
      # if (!is.null(model_list[[i]]) && model_list[[i]]$error) {
      #   title(vario_zoomed_list[[i]]@info$identity, sub = "Error on",
      #         cex.sub = 0.85, col.sub = "red")
      # } else {
      #   title(title_vec[i])
      # }
    }
  } else {
    for (i in seq_along(vario_list)) {
      plot(vario_list[[i]], CTMM = model_list[[i]],
           col.CTMM = model_color,
           fraction = fraction)
      # browser()
      graphics::title(title_vec[i])
      # if (!is.null(model_list[[i]]) && model_list[[i]]$error) {
      #   title(vario_list[[i]]@info$identity, sub = "Error on",
      #         cex.sub = 0.85, col.sub = "red")
      # } else {
      #   title(vario_list[[i]]@info$identity)
      # }
      # if (model_list[[i]]$error) {
      #   title(sub = "Error on", cex.sub = 0.85, col.sub = "red")
      # }
    }
  }
  graphics::par(def.par)
}
