# the app can use a 0.72 cex but package functions may need a smaller default.
# S3 generic on list https://github.com/ctmm-initiative/ctmm/blob/master/R/generic.R#L109
# plot.list <- ctmm:::plot.list
# S3 example, https://github.com/ctmm-initiative/ctmm/blob/master/R/1.R#L34
# plot <- function(object,...) UseMethod("plot")

#' Plot a group of empirical or modeled variograms.
#'
#' This is a wrapper over [ctmm::plot.variogram()] to make group plot easier.
#'
#' @param vario_list list of [ctmm::variogram()]. The `names` of list will be
#'  used as figure title in empirical variogram mode.
#' @param model_list `CTMM` models list. The `names` of list are needed for
#'   figure titles. Draw modeled variogram if provided. The models list should
#'   match `vario_list` in length and animal, so that `i`th model is for `i`th
#'   variogram.
#'   Possible values:
#'   - It can be skipped to use default value `NULL`, and only empirical
#'   variograms are drawn. `vario_list` names will be used as figure title.
#'   Better use named parameters for others in this case.
#'   - list of guesstimated models from [ctmm::ctmm.guess()] on telemetry data,
#'   overlay guesstimate variogram.
#'   - list of fitted models from [ctmm::ctmm.select()] on `vario_list`, overlay
#'   modeled variogram.
#' @param fraction Fraction of time-lag range, 0 ~ 1.
#' @param relative_zoom
#' - Relative mode zoom every plot by fraction of their own Time-lag range. The
#' X, Y axes are not synced.
#' - Absolute mode operate on the max Time-lag range individual in group, and
#' all others scaled with same X, Y axes for easier comparison.
#' @param cex The magnification factor of plot text and symbols. See [par()].
#' @param model_color The color of model variogram
#' @param columns The columns of the group plot layout.
#'
#' @export
plot_vario <- function(vario_list, model_list = NULL,
                       fraction = 0.5, relative_zoom = TRUE, cex = 0.65,
                       model_color = "blue", columns = 2){
  title_vec <- if (is.null(model_list)) names(vario_list)
                   else names(model_list)
  if (is.null(title_vec)) {
    stop("names of input list is needed for figure title")
  }
  row_count <- ceiling(length(vario_list) / columns)
  # the shared group code is not much, and it involves env setup and restoration, would need on.exit if abstracted to function. just copy, basically 3 lines.
  def.par <- graphics::par(no.readonly = TRUE)
  graphics::par(mfrow = c(row_count, columns),
                mar = c(5, 5, 4, 1), ps = 18, cex = cex, cex.main = 0.9)
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

# S3 generic, https://github.com/ctmm-initiative/ctmm/blob/master/R/emulate.R
# plot.variogram

#' Plot a group of home ranges or occurrences
#'
#' This is a wrapper over [ctmm::plot.telemetry()] to make group plot easier.
#'
#' @param UD_list `ctmm` `UD` object list, which can be home range from
#'   [ctmm::akde()] or occurrences from [ctmm::occurrence()]. The names of list
#'   are needed for title of figures.
#' @param level_vec The vector of `level.UD` in [ctmm::plot.telemetry()]. To be
#'   consistent with `ctmm` they are values 0 ~ 1 (for example 0.95). Note the
#'   app UI take values 0 ~ 100 (for example 95) for easier input.
#' @inheritParams plot_vario
#' @param tele_list [ctmm::as.telemetry] telemetry list. When provided, animal
#'   locations are overlayed in plot. This should only be used for home range
#'   since it can interfere with occurrence plot.
#'
#' @export
plot_ud <- function(UD_list, level_vec = 0.95, columns = 2, cex = 0.65,
                    tele_list = NULL) {
  row_count <- ceiling(length(UD_list) / columns)
  def.par <- graphics::par(no.readonly = TRUE)
  graphics::par(mfrow = c(row_count, columns),
                mar = c(5, 5, 4, 1), ps = 18, cex = cex, cex.main = 0.9)
  lapply(seq_along(UD_list), function(i) {
    tryCatch({
      # plot(select_models_occurrences()[[i]], level.UD = input$ud_level)
      if (is.null(tele_list)) {
        plot(UD_list[[i]], level.UD = level_vec)
      } else {
        # must use named parameter of UD here, since the 2nd parameter by position is for CTMM
        plot(tele_list[[i]], UD = UD_list[[i]], level.UD = level_vec)
      }
    }, error = function(e) {
      warning(names(UD_list)[i], ": ", e)
      plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 10), ylim = c(0, 10))
    })
    graphics::title(names(UD_list)[i])
  })
  graphics::par(def.par)
}
