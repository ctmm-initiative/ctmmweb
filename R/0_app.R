#' Start ctmm web app
#'
#' The app is started with system default browser, while usually Shiny app is
#' launched with RStudio builtin browser. The builtin browser has some
#' compatibility problems so Chrome is recommended.
#'
#' @param shiny_app_data Data to be loaded with app. It could be anything can be taken by [ctmm::as.telemetry()]:
#' - csv file Path of [MoveBank format](https://www.movebank.org/node/13)
#' - `data.frame` of [MoveBank format](https://www.movebank.org/node/13)
#' - [move::Move-class](https://www.rdocumentation.org/packages/move/versions/3.2.2/topics/Move-class) object
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
# check new release version of package
check_update <- function(installed_pkg_time) {
  installed_pkg_date <- lubridate::date(installed_pkg_time)
  # https://developer.github.com/v3/repos/commits/#get-a-single-commit
  base_url <- "https://api.github.com/repos/ctmm-initiative/ctmm_repo/commits"
  # for test, use an older since_date otherwise no result found
  # url <- paste0(base_url, "?since=" , "2019-01-01")
  url <- paste0(base_url, "?since=" , installed_pkg_date)
  # 600 ms, max 3.3 s.
  res <- try(httr::GET(url, httr::timeout(3)))
  if (inherits(res, "try-error")) {
    # cat("Update check failed\n")
    shiny::showNotification("Update check failed, check network connection",
                            duration = 6, type = "error")
    return(FALSE)
  }
  status <- httr::http_status(res)$category
  if (status != "Success") {
    shiny::showNotification(paste0("Update check failed:\n",
                                   httr::http_status(res)$message),
                            duration = 6, type = "error")
    return(FALSE)
  }
  content <- httr::content(res)
  if (length(content) != 0) {
    latest_commit_time <- lubridate::ymd_hms(
      content[[1]][["commit"]][["author"]][["date"]])
    # reverse condition for test
    if (installed_pkg_time < latest_commit_time) {
      shiny::showNotification("New release found, please update the app. Windows user can run the Update app link from start menu.",
                              duration = 9, type = "warning")
      return(TRUE)
    }
  }
  # all other cases return FALSE, like the empty list
  return(FALSE)
}
