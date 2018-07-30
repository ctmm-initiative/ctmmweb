# global environment setup. work report and app state may need some global varibles here too.
# other than package loading, all expressions are just constants. They are not expected to be modified in code even its possible. This make it safer to have a global shared environment for all app sessions.
# need pacman to install packages not listed in package dependencies.
if (!require("pacman")) install.packages("pacman")
# load_gh may fail if existing ctmm didn't get uninstalled properly in one run.
# pacman::p_load_gh("ctmm-initiative/ctmm@a24eeab591c7b00a28406a9972a26878507a43a1")
# packrat doesn't recognize p_load_gh so need to put ctmm in p_load again. because deployment try to reproduce current environment, it will install ctmm github version even I only used p_load here since I have github version installed. and the run script will install github version first so it also work.
# pipe operator is imported by DT, stringr, leaflet, etc. loading DT enable using of pipe
# the reason to load first line of packages is that they are not really needed for package functions, so are not included in package dependencies. we still need p_load to install them in first run. since they are loaded, no need to use full qualified name in app code.
pacman::p_load(shiny, shinydashboard, DT,
               ctmm, data.table, ctmmweb)
# package installation time in current time zone. This is used in ui (app info dialog) and server (start info). previously in here to save 2 calls, which cost 500 ms. but this often lead to error when app is stopped in debugging, which caused many re-launch, way more than the 500 ms saved. instead, write message menu in server side dynamically once.
# PKG_INSTALLATION_TIME <- format(file.mtime(system.file("app", package = "ctmmweb")), usetz = TRUE)
# switch to turn on debug button
# DEBUG_BUTTON <- FALSE
# have to source module here to make them both available to ui and server. tried to put ui in ui.R, source server in server.R, but server code need ui function (the simple help module don't need that) because it's renderUI which is not available.
source("module_fine_tune.R")
