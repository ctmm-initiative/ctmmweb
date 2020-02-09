# folder and timestamp ----
# used in build_zip, creating temp folder for log
current_timestamp <- function() {
  # format(Sys.time(), "%Y-%m-%d_%H-%M-%S_UTC",
  #        tz = "UTC")
  format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
}
get_build_info <- function(pkg = "ctmmweb") {
  desc_file <- system.file("DESCRIPTION", package = pkg)
  desc_dt <- data.table(read.dcf(desc_file, all = TRUE))
  version <- desc_dt[1, Version]
  build_info <- desc_dt[1, LastCommit]
  # pattern is for 20xx-xx-xx
  build_date <- stringr::str_extract(build_info, "20\\d\\d-\\d\\d-\\d\\d")
  return(list(version = version, build_date = build_date,
              commit_message = build_info))
}
# given a build info list, print it nicely
print_build_info <- function(build_info) {
  purrr::map(names(build_info), ~ {
    stringr::str_c("\n\t- ", ., ": ", build_info[[.]])
  }) %>% stringr::str_c(collapse = "")
}
# write content in utf-8, open connection with native encoding to avoid extra translation.
# https://kevinushey.github.io/blog/2018/02/21/string-encoding-and-r/
write_utf8 <- function(chara_vec, f) {
  # step 1: ensure our text is utf8 encoded
  chara_vec_utf8 <- enc2utf8(chara_vec)

  # step 2: create a connection with 'native' encoding
  # this signals to R that translation before writing
  # to the connection should be skipped
  con <- file(f, open = "w+", encoding = "native.enc")

  # step 3: write to the connection with 'useBytes = TRUE',
  # telling R to skip translation to the native encoding
  writeLines(chara_vec_utf8, con = con, useBytes = TRUE)

  # close our connection
  close(con)
}
# will not have warning if folder already exist; can create a path chain directly
create_folder <- function(folder_path) {
  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
  }
  return(folder_path)
}
#' Compress a folder into zip
#'
#' [zip::zip()] is used so this is platform independent. It will keep the
#' relative path structure in zip, include `folder_path` itself so the zip can
#' be extracted directly without need of creating a folder to hold contents. The
#' zip file will be saved to the parent folder of `folder_path`.
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
  suppressMessages(
    zip::zip(zip_path, basename(folder_path),
             compression_level = 5, recurse = TRUE)
  )
  setwd(previous_wd)
  return(zip_path)
}
# compress select files under one folder with relative path. zip will be put in same folder
# target zip file can be single file name or a partial path relative to base_folder
zip_relative_files <- function(base_folder, relative_paths, zip_relative_path) {
  previous_wd <- getwd()
  # one level up folder, so we can use relative path in zip
  setwd(base_folder)
  zip_path <- file.path(base_folder, zip_relative_path)
  suppressMessages(
    zip::zip(zip_path, relative_paths,
             compression_level = 5)
  )
  setwd(previous_wd)
  return(zip_path)
}
# compress multiple files into a zip, copy to target path
# file is the user chosen file name determined in download, need to prepare a file, copy to that path. write_f is a function that write files, take folder_path determined in build_zip as parameter. prefix is used to separate folder and file names, with "-" to be consistent with other usage.
build_zip <- function(file, write_f, session_tmpdir, prefix) {
  # use time till min in zip name, use second in folder name, otherwise this function got run twice, will have error running 2nd time writing to same folder.
  current_time <- current_timestamp()  # need this in zip name so save it
  folder_path <- file.path(session_tmpdir,
                           stringr::str_c(prefix, current_time))
  create_folder(folder_path)
  write_f(folder_path)
  zip_path <- zip_folder(folder_path,
                              paste0(prefix, current_time, ".zip"))
  file.copy(zip_path, file)
}
