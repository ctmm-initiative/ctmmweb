# folder and timestamp ----
# used in build_zip, creating temp folder for log
current_timestamp <- function() {
  # format(Sys.time(), "%Y-%m-%d_%H-%M-%S_UTC",
  #        tz = "UTC")
  format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
}
create_folder <- function(folder_path) {
  dir.create(folder_path, recursive = TRUE)
  return(folder_path)
}
#' Compress a folder into zip
#'
#' [`zip`](https://github.com/r-lib/zip) package is used so this is platform
#' independent. It will keep the relative path structure in zip, include
#' `folder_path` itself so the zip can be extracted directly without need of
#' creating a folder to hold contents. The zip file will be saved to the parent
#' folder of `folder_path`.
#'
#' @param folder_path The folder to be compressed
#' @param zip_name The name of zip
#'
#' @return The absolute path of result zip file
#' @export
#'
zip_folder <- function(folder_path, zip_name) {
  previous_wd <- getwd()
  # one level up folder, so we can use relative path in zip
  setwd(dirname(folder_path))
  # zip example show that zip can take a folder as file list, it just need to be relative paths
  # relative_paths_under_folder <- list.files(folder_path, recursive = TRUE)
  # # construct the relative path inside zip
  # relative_paths <- file.path(basename(folder_path),
  #                             relative_paths_under_folder)
  zip_path <- file.path(dirname(folder_path), zip_name)
  zip::zip(zip_path, basename(folder_path),
           compression_level = 5, recurse = TRUE)
  setwd(previous_wd)
  return(zip_path)
}
# compress select files under one folder with relative path. zip will be put in same folder
zip_relative_files <- function(base_folder, relative_paths, zip_name) {
  previous_wd <- getwd()
  # one level up folder, so we can use relative path in zip
  setwd(base_folder)
  zip_path <- file.path(base_folder, zip_name)
  zip::zip(zip_path, relative_paths,
           compression_level = 5)
  setwd(previous_wd)
  return(zip_path)
}
# file is the user chosen file name determined in download, need to prepare a file, copy to that path. write_f is a function that write files, take folder_path determined in build_zip as parameter.
build_shapefile_zip <- function(file, write_f, session_tmpdir) {
  # use time till min in zip name, use second in folder name, otherwise this function got run twice, will have error running 2nd time writing to same folder.
  current_time <- current_timestamp()  # need this in zip name so save it
  folder_path <- file.path(session_tmpdir,
                           stringr::str_c("Range_", current_time))
  create_folder(folder_path)
  write_f(folder_path)
  zip_path <- zip_folder(folder_path,
                              paste0("Home Range ", current_time, ".zip"))
  file.copy(zip_path, file)
}
