#' Start ctmm web app
#'
#' The app is started with system default browser, while usually Shiny app is
#' launched with RStudio builtin browser. The builtin browser has some
#' compatibility problems so Chrome is recommended.
#'
#' @param data Data to be loaded with app. It could be anything can be taken by [ctmm::as.telemetry()]:
#' - csv file Path of [MoveBank format](https://www.movebank.org/node/13)
#' - `data.frame` of [MoveBank format](https://www.movebank.org/node/13)
#' - [move::Move-class] object
#' - [ctmm::as.telemetry()] telemetry object/list.
#' - Default `NULL` will not load any data.
#'
#' @export
#' @import data.table
#'
app <- function(shiny_app_data = NULL) {
  app_DIR <- system.file("app", package = "ctmmweb")
  if (app_DIR == "") {
    stop("Could not find app directory. Try re-installing `ctmmweb`.", call. = FALSE)
  }
  # evaluate them inside function environment, also change working directory temporarily
  source(file.path(app_DIR, "global.R"), local = TRUE, chdir = TRUE)
  source(file.path(app_DIR, "ui.R"), local = TRUE, chdir = TRUE)
  source(file.path(app_DIR, "server.R"), local = TRUE, chdir = TRUE)
  shiny_app <- shiny::shinyApp(ui = ui, server = server)
  shiny::runApp(shiny_app, launch.browser = TRUE, display.mode = "normal")
}
