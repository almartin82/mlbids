
cbs <- function() {
  stop('no need to scrape cbs player ids one by one.  see `cbs_bulk()`.')
}

cbs_bulk <- function() {

  url <-'http://api.cbssports.com/fantasy/players/list?version=3.0'
  r <- httr::GET(
    url = url,
    query = list(SPORT = "baseball", response_format = 'JSON')
  )

  cbs_text <- content(r, as = "text")
  cbs_json <- jsonlite::fromJSON(cbs_text, simplifyVector = FALSE)

  list2df(cbs_json$body$players)
}



