# global environment setup. work report and app state may need some global varibles here too.
# other than package loading, all expressions are just constants. They are not expected to be modified in code even its possible. This make it safer to have a global shared environment for all app sessions.
if (!require("pacman")) install.packages("pacman")
# load_gh may fail if existing ctmm didn't get uninstalled properly in one run.
# pacman::p_load_gh("ctmm-initiative/ctmm@a24eeab591c7b00a28406a9972a26878507a43a1")
# packrat doesn't recognize p_load_gh so need to put ctmm in p_load again.
pacman::p_load(shiny, shinydashboard, DT, markdown,
               ctmm, data.table,
               ggplot2, scales, grid, gridExtra, lubridate,
               httr, stringr, XML)
# increase the uploading file size limit to 200M
options(shiny.maxRequestSize = 200*1024^2)

# UI style constants ----
# some are used in server call, so put them all here instead of ui.R
height_data_import_box <- "244px"
height_location_box <- "900px"
height_plot_loc <- 730
height_plot_3 <- 640
# sampling time
height_hist_box <- "350px"
height_hist <- 280
# time subsetting
# not setting the box height make arrange multiple items easier.
height_hist_subset_box <- "310px"
height_hist_subset_output <- "150px"
# height_selected_loc_box <- "480px"
# height_selected_loc <- 480
page_action_style <- "background-color: #FFEB3B;font-weight: 600;width:100%;"
external_link_style <- "background-color: #a7c1fc;font-weight: 600;width:100%;"
help_button_style <- "background-color: #8bc34a;width:100%;"
# info box blue #00c0ef

# UI modules ----
help_button <- function(module_id) {
  prefix <- NS(module_id)
  actionButton(prefix("help"),
               "Help",
               icon = icon("question"),
               style = help_button_style
  )
}
click_help <- function(input, output, session, title, file){
  observeEvent(input$help, {
    showModal(modalDialog(
      title = title, size = "l",
      fluidPage(includeMarkdown(file)),
      easyClose = TRUE, fade = FALSE
    ))
  })
}
