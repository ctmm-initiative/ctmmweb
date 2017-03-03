# movebank download
request <- function(entity_type, user, pass){
  base_url <- "https://www.movebank.org/movebank/service/direct-read?entity_type="
  url <- paste0(base_url, entity_type)
  res <- httr::GET(url, config = add_headers(user = user, password = pass))
  return(httr::content(res, as = 'text', encoding = "UTF-8"))
}

get_all_studies <- function(user, pass) {
  res_cont <- request("study", user, pass)
  return(res_cont)
}
