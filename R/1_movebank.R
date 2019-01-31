# movebank download ----
# this function need shiny. it's internal and only used inside app so no need to check.
# always need the response content in text, also need response status
request <- function(entity_type, user, pass){
  base_url <- "https://www.movebank.org/movebank/service/direct-read?entity_type="
  url <- paste0(base_url, entity_type)
  res <- httr::GET(url, config = httr::add_headers(user = user, password = pass))
  status <- httr::http_status(res)$category
  if (status != "Success") {
    # need shiny here. no need to check since it's only used inside shiny app
    shiny::showNotification(paste0(httr::http_status(res)$message,
                                   "\nCheck console for more information"),
                            duration = 6, type = "error")
    # will use xml2::read_html
    res_cont <- httr::content(res, type = 'text/html', encoding = "UTF-8")
    txt <- html_to_text(res_cont)
    formated_txt <- gsub("^ $", ": ", txt)
    warning(formated_txt)
  }
  res_cont <- httr::content(res, as = 'text', encoding = "UTF-8")
  return(list(status = status, res_cont = res_cont))
}
get_all_studies <- function(user, pass) {
  return(request("study", user, pass))
}
# [blog post](https://tonybreyal.wordpress.com/2011/11/18/htmltotext-extracting-text-from-html-via-xpath/), [code](https://github.com/tonybreyal/Blog-Reference-Functions/blob/master/R/htmlToText/htmlToText.R)
html_to_text <- function(html) {
  doc <- XML::htmlParse(html, asText = TRUE)
  text <- XML::xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", XML::xmlValue)
  return(text)
}
get_study_detail <- function(mb_id, user, pass) {
  request(paste0("study&study_id=", mb_id), user, pass)
}
get_study_data <- function(mb_id, user, pass){
  request(paste0("event&study_id=", mb_id, "&attributes=all"), user, pass)
}
# check return type of movebank data download by counting , in first line. no data for download: 0; license agreement: 1; normal data: >1
header_comma_count <- function(chars) {
  line1 <- str_split(chars, "\\n")[[1]][[1]]
  str_count(line1, ",")
}
