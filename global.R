# global environment setup. work report and app state may need some global varibles here too.
if (!require("pacman")) install.packages("pacman")
# load_gh may fail if existing ctmm didn't get uninstalled properly in one run.
# pacman::p_load_gh("ctmm-initiative/ctmm@a24eeab591c7b00a28406a9972a26878507a43a1")
# packrat doesn't recognize p_load_gh so need to put ctmm in p_load again.
pacman::p_load(shiny, shinydashboard, DT, ctmm, ggplot2, scales, gridExtra, data.table, lubridate, markdown, httr, stringr)
# increase the uploading file size limit to 200M
options(shiny.maxRequestSize = 200*1024^2)
# options(shiny.trace = TRUE)
# options(shiny.trace = FALSE)
