#' CBS scrape
#'
#' @description gets the name, dob, etc for a player, given
#' a cbsid value (an integer, generally between 1263000 and 2218000)
#' @param id an integer
#'
#' @return if given a valid id, a data frame with player name, dob, id, etc.
#' @export

cbs <- function(id) {
  url <- paste0('http://www.cbssports.com/mlb/players/playerpage/', id)
  resp <- httr::GET(url)
  if (!resp$status_code == 200) {
    stop('Bad response from cbssports.com')
  }

  h <- read_html(resp$content)
  cbs_title <- h %>% html_nodes('title') %>% html_text()
  if (cbs_title == ' , , , MLB Baseball - CBSSports.com ') {
    stop('No such playerid at cbssports.com')
  }

  demog_xml <- h %>%
    html_nodes("#gridContainer .featureComponent") %>%
    extract(1)

  demog_keys <- demog_xml %>%
    html_nodes('dt') %>%
    xml_text() %>%
    as.list() %>%
    unlist()

  demog_values <- demog_xml %>%
    html_nodes('dd') %>%
    xml_text() %>%
    as.list() %>%
    unlist()

  demog_df <- data.frame(
    keys = demog_keys,
    values = demog_values,
    stringsAsFactors = FALSE
  )

  fullname <- cbs_title %>%
    strsplit(split = ',')  %>%
    extract2(1) %>% extract(1)

  pos <- h %>%
    html_nodes(".playerPosition") %>%
    extract(1) %>%
    html_text()

  player <- data.frame(
    'cbsid' = id,
    'name' = fullname,
    'position' = pos,
    'dob' = demog_df[demog_df$keys == 'Birthdate:', ]$values %>% mdy(),
    'hometown' = demog_df[demog_df$keys == 'Hometown:', ]$values,
    stringsAsFactors = FALSE
  )

  player
}


#' Bulk CBS player download
#'
#' @return a data frame with all of the MLB players returned by the `/fantasy/player`
#' CBS API endpoint
#' @export

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
