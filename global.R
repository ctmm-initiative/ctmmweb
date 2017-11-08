# global environment setup. work report and app state may need some global varibles here too.
# other than package loading, all expressions are just constants. They are not expected to be modified in code even its possible. This make it safer to have a global shared environment for all app sessions.
if (!require("pacman")) install.packages("pacman")
# load_gh may fail if existing ctmm didn't get uninstalled properly in one run.
# pacman::p_load_gh("ctmm-initiative/ctmm@a24eeab591c7b00a28406a9972a26878507a43a1")
# packrat doesn't recognize p_load_gh so need to put ctmm in p_load again. because deployment try to reproduce current environment, it will install ctmm github version even I only used p_load here since I have github version installed. and the run script will install github version first so it also work.
pacman::p_load(shiny, shinydashboard, DT,
               markdown, crayon, rmarkdown, knitr,
               ctmm, data.table, parallel, memoise,
               ggplot2, scales, grid, gridExtra, lubridate,
               leaflet, sp, rgdal, leaflet.extras, htmlwidgets,
               httr, stringr, XML, xml2, zip)
# increase the uploading file size limit to 200M
options(shiny.maxRequestSize = 200*1024^2)

# UI style constants ----
# some are used in server call, so put them all here instead of ui.R
# box, plotOutput, renderPlot, no need to set all three if need adjustment.
# box height will expand by content, just set plotOutput width and height to percentages (99% width, need to keep it inside the box), then also need to set fixed value in renderPlot (otherwise it didn't show). We set height on histogram to make it shorter, setting box height is easier (no need to set in server part).
styles <- list(
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
grid_group <- "_graticule_"
draw_group <- "_draw with measure_"
map_name_by_tab <- list(Point = "point_map", Heatmap = "heat_map")
# UI modules ----
help_button <- function(module_id) {
  prefix <- NS(module_id)
  actionButton(prefix("help"),
               "Help",
               icon = icon("question"),
               style = styles$help_button
  )
}
click_help <- function(input, output, session, title, size, file){
  observeEvent(input$help, {
    showModal(modalDialog(
      title = title, size = size,
      fluidPage(includeMarkdown(file)),
      easyClose = TRUE, fade = FALSE
    ))
  })
}
# log slider ----
# need to be here because it will be used in ui.R
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

# call logifySlider for each relevant sliderInput
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
