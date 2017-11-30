# global environment setup. work report and app state may need some global varibles here too.
# other than package loading, all expressions are just constants. They are not expected to be modified in code even its possible. This make it safer to have a global shared environment for all app sessions.
# need pacman to install packages not listed in package dependencies.
if (!require("pacman")) install.packages("pacman")
# load_gh may fail if existing ctmm didn't get uninstalled properly in one run.
# pacman::p_load_gh("ctmm-initiative/ctmm@a24eeab591c7b00a28406a9972a26878507a43a1")
# packrat doesn't recognize p_load_gh so need to put ctmm in p_load again. because deployment try to reproduce current environment, it will install ctmm github version even I only used p_load here since I have github version installed. and the run script will install github version first so it also work.
pacman::p_load(shiny, shinydashboard, DT,
               ctmm,data.table)
# in app debug, need this in both ui.R and server.R
DEBUG_MODE <- FALSE
