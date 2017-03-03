# movebank download
request <- function(entity_type, user, pass){
  base_url <- "https://www.movebank.org/movebank/service/direct-read?entity_type="
  url <- paste0(base_url, entity_type)
  res <- httr::GET(url, config = add_headers(user = user, password = pass))
  return(httr::content(res, as = 'text', encoding = "UTF-8"))
}
studies_cols <- c("id", "name",
                   "number_of_deployments", "number_of_events",
                   "number_of_individuals",
                   "i_am_owner", "i_can_see_data"
)
get_all_studies <- function(user, pass) {
  cont <- request("study", user, pass)
  studies <- fread(cont, select = studies_cols)
  return(studies)
}
get_valid_studies <- function(user, pass){
  studies <- get_all_studies(user, pass)
  studies[, i_am_owner := ifelse(i_am_owner == "true", TRUE, FALSE)]
  studies[, i_can_see_data := ifelse(i_can_see_data == "true", TRUE, FALSE)]
  valid_studies <- studies[number_of_events > 0 & i_can_see_data]
  new_names <- sub(".*_", "", studies_cols)
  setnames(valid_studies, studies_cols, new_names)
  valid_studies[, c("data", "owner") := NULL]
  return(valid_studies)
}
