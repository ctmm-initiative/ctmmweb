
#' Title
#'
#' @param vg_list list of `ctmm::variogram`
#' @param guess_list list of guesstimated models from `ctmm::ctmm.guess` on `vg_list`.
#' @param model_list `CTMM` models list. Draw modeled variogram if provided. The models list should match `vg_list` in length and animal, so that `i`th model is for `i`th variogram.
#' @param fraction
#' @param relative_zoom
#' @param figure_height
#' @param columns
#'
#' @return
#' @export
#'
#' @examples
plot_vario <- function(vg_list, guess_list = NULL, model_list = NULL,
                       fraction = 0.5, relative_zoom = TRUE,
                       figure_height = 250, columns = 2){

}
