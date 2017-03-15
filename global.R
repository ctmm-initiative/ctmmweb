# global environment setup. work report and app state may need some global varibles here too.
if (!require("pacman")) install.packages("pacman")
# load_gh may fail if existing ctmm didn't get uninstalled properly in one run.
# pacman::p_load_gh("ctmm-initiative/ctmm@a24eeab591c7b00a28406a9972a26878507a43a1")
# packrat doesn't recognize p_load_gh so need to put ctmm in p_load again.
pacman::p_load(shiny, shinydashboard, DT, ctmm, ggplot2, scales, gridExtra, data.table, lubridate, markdown, httr, stringr, XML)
# increase the uploading file size limit to 200M
options(shiny.maxRequestSize = 200*1024^2)

# UI style constants ----
# some are used in server call, so put them all here instead of ui.R
height_data_import_box <- "244px"
height_location_box <- "800px"
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
page_action_style <- "background-color: #FFEB3B;font-weight: 600;"
external_link_style <- "background-color: #4eebff;font-weight: 600;"
help_button_style <- "background-color: #8bc34a;"
# info box blue #00c0ef
