# movebank download
request <- function(entity_type, user, pass){
  base_url <- "https://www.movebank.org/movebank/service/direct-read?entity_type="
  url <- paste0(base_url, entity_type)
  res <- httr::GET(url, config = add_headers(user = user, password = pass))
  return(httr::content(res, as = 'text', encoding = "UTF-8"))
}
downloaded_cols <- c("id", "name", "study_objective",
                   "number_of_deployments", "number_of_events",
                   "number_of_individuals",
                   "i_am_owner", "i_can_see_data", "license_terms"
)
get_all_studies <- function(user, pass) {
  cont <- request("study", user, pass)
  studies <- fread(cont, select = downloaded_cols)
  return(studies)
}
get_valid_studies <- function(user, pass){
  studies <- get_all_studies(user, pass)
  studies[, i_am_owner := ifelse(i_am_owner == "true", TRUE, FALSE)]
  studies[, i_can_see_data := ifelse(i_can_see_data == "true", TRUE, FALSE)]
  valid_studies <- studies[(i_can_see_data)]
  new_names <- sub(".*_", "", downloaded_cols)
  setnames(valid_studies, downloaded_cols, new_names)
  return(valid_studies)
}
