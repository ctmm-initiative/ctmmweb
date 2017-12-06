# JS for log slider ----

#' JS Function To Logify A `sliderInput`
#'
#' Make Shiny `sliderInput` [support logarithmic
#' scale](https://stackoverflow.com/a/39028280/3718827). `JS.logify` create a
#' Javascript function in string, and [JS.onload] register the function to
#' `sliderInput`. Search the usage in `/inst/app/ui.R` for examples.
#'
#' @param digits digits after numerical point
#'
#' @return JS code
#' @export
#'
JS.logify <- function(digits = 2) {
  paste0(  "
           // function to logify a sliderInput
           function logifySlider (sliderId, sci = false) {
           if (sci) {
           // scientific style
           $('#'+sliderId).data('ionRangeSlider').update({
           'prettify': function (num) { return ('10<sup>'+num+'</sup>'); }
           })
           } else {
           // regular number style
           $('#'+sliderId).data('ionRangeSlider').update({
           'prettify': function (num) { return (Math.pow(10, num).toFixed(",
           digits, ")); }
           })
           }
           }")
}
#' Register JS Logify Function For Each `sliderInput`
#'
#' Make Shiny `sliderInput` [support logarithmic
#' scale](https://stackoverflow.com/a/39028280/3718827). `JS.logify` create a
#' Javascript function in string, and [JS.onload] register the function to
#' `sliderInput`. Search the usage in `/inst/app/ui.R` for examples.
#'
#' @param slider_id_vec slider id vector
#' @param sci use scientific notation
#'
#' @return JS code
#' @export
#'
JS.onload <- function(slider_id_vec, sci = FALSE) {
  slider_call <- function(slider_id) {
    paste0("logifySlider('", slider_id,
           "', sci = ", ifelse(sci, "true", "false") , ")")
  }
  return(paste0("
                // execute upon document loading
                $(document).ready(function() {
                // wait a few ms to allow other scripts to execute
                setTimeout(function() {",
                paste0(lapply(slider_id_vec, slider_call), collapse = "\n"),
                "}, 5)})"
                ))
                }
# UI style constants ----
# some are used in server call, so both ui and server need them
# box, plotOutput, renderPlot, no need to set all three if need adjustment.
# box height will expand by content, just set plotOutput width and height to percentages (99% width, need to keep it inside the box), then also need to set fixed value in renderPlot (otherwise it didn't show). We set height on histogram to make it shorter, setting box height is easier (no need to set in server part).
STYLES <- list(
  height_hist = 280,
  # outliers
  height_outlier_hist = "180px",
  # time subsetting
  # not setting the box height make arrange multiple items easier.
  # height_hist_subset_box = "380px",
  height_hist_subset_output = "150px",
  # height_selected_loc_box = "480px"
  # height_selected_loc = 480
  page_action = "background-color: #FFEB3B;font-weight: 500;width:100%;",
  # using similar color with first box in each page.
  page_switch = "background-color: #7ad0f7;font-weight: 500;width:100%;",
  external_link = "background-color: #a7c1fc;font-weight: 500;width:100%;",
  download_button = "color: #2196F3;width:100%;",
  help_button = "background-color: #8bc34a;width:100%;"
  # info box blue #00c0ef
)