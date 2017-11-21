#' Start ctmm web app
#'
#' @param data Data to be loaded with app. Could be path to csv file, `ctmm` telemetry object/list. Default `NULL` will not load any data.
#'
#' @export
#' @import data.table
#'
# app <- function(data = NULL) {
#   appDir <- system.file("app", package = "ctmmweb")
#   if (appDir == "") {
#     stop("Could not find app directory. Try re-installing `ctmmweb`.", call. = FALSE)
#   }
#   shiny::runApp(appDir, display.mode = "normal")
# }

app <- function(shiny_app_data = NULL) {
  appDir <- system.file("app", package = "ctmmweb")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `ctmmweb`.", call. = FALSE)
  }
  # evaluate them inside function environment, also change working directory temporarily
  source(file.path(appDir, "global.R"), local = TRUE, chdir = TRUE)
  source(file.path(appDir, "ui.R"), local = TRUE, chdir = TRUE)
  source(file.path(appDir, "server.R"), local = TRUE, chdir = TRUE)
  shiny_app <- shiny::shinyApp(ui = ui, server = server)
  shiny::runApp(shiny_app, display.mode = "normal")
}
