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
#' @import data.table shinyWidgets
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
# check installed package version
get_build_info <- function() {
  desc_file <- system.file("DESCRIPTION", package = "ctmmweb")
  # desc_dt <- data.table(t(read.dcf(desc_file, all = TRUE)), keep.rownames = TRUE)
  # if installed with devtools/remotes install_github, will have RemoteType rows. Otherwise (with devtools install package in developtment mode) only has Built time
  # Remote section is a little bit verbose but it's better just use a pattern in case devtools changed their names
  # build_info <- desc_dt[stringr::str_detect(rn, "Remote") | rn == "Packaged"| rn == "Built"]
  # build_info_list <- list(build_info$V1)
  # names(build_info_list) <- build_info$rn
  desc_list <- as.list(read.dcf(desc_file, all = TRUE))
  build_info_list <- desc_list[stringr::str_subset(names(desc_list), "Remote|Packaged|Built")]
  # note Remotes is the item added in description for specifying install dependency from github, not the other Remote_ items. thus we just remove it
  build_info_list$Remotes <- NULL
  return(build_info_list)
  # version <- desc_dt[1, Version]
  # build_info <- desc_dt[1, LastCommit]
  # pattern is for 20xx-xx-xx
  # build_date <- stringr::str_extract(build_info, "20\\d\\d-\\d\\d-\\d\\d")
  # return(list(version = version, build_date = build_date,
  #             commit_message = build_info))
}
# given a build info list, print it nicely in log message. each item have a name and value.
print_build_info <- function(build_info) {
  # the 2nd part of log message have a leading \t. we need to make first item have one less \t, thus just add \t in end. collapse is the right way to add stuff in between but not in end.
  items <- purrr::map(names(build_info), ~ {
    stringr::str_c("- ", ., ": ", build_info[[.]])
  }) %>% stringr::str_c(collapse = "\n\t")
}
# check new release version of package
check_update <- function(installed_pkg_time) {
  installed_pkg_build_date <- lubridate::date(installed_pkg_time)
  # https://developer.github.com/v3/repos/commits/#get-a-single-commit
  base_url <- "https://api.github.com/repos/ctmm-initiative/ctmmweb/commits"
  # for test, use an older since_date otherwise no result found
  # url <- paste0(base_url, "?since=" , "2019-01-01")
  url <- paste0(base_url, "?since=" , installed_pkg_build_date)
  # 600 ms, max 3.3 s. so there will be one time delay if there is no internet.
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
    # reverse condition for test. Note in local development we could have local build newer than last commit in github. so nothing will happen.
    if (lubridate::ymd(installed_pkg_build_date) < latest_commit_time) {
      shiny::showNotification("New release found, please update the app. Windows user can run the Update app link from start menu.",
                              duration = 9, type = "warning")
      return(TRUE)
    }
  }
  # all other cases return FALSE, like the empty list
  return(FALSE)
}
