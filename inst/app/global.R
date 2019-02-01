# global environment setup. work report and app state may need some global varibles here too.
# other than package loading, all expressions are just constants. They are not expected to be modified in code even its possible. This make it safer to have a global shared environment for all app sessions.
# the original need of pacman for installing them is taken by package dependency management, now move to that approach completely to simplify dependency management. thus we don't need pacman here, nor the special packrat fork.
# still need to load these package because some are too frequently used to use qualifier, some need to be loaded to perform properly (ctmm, data.table)
library(shiny)
library(shinydashboard)
library(ctmm)
library(data.table)
library(magrittr)
# package installation time in current time zone. This is used in ui (app info dialog) and server (start info). previously in here to save 2 calls, which cost 500 ms. but this often lead to error when app is stopped in debugging, which caused many re-launch, way more than the 500 ms saved. instead, write message menu in server side dynamically once.
# PKG_INSTALLATION_TIME <- format(file.mtime(system.file("app", package = "ctmmweb")), usetz = TRUE)
# switch to turn on debug button
# DEBUG_BUTTON <- FALSE
# have to source module here to make them both available to ui and server. tried to put ui in ui.R, source server in server.R, but server code need ui function (the simple help module don't need that) because it's renderUI which is not available.
# source("module_fine_tune.R")
