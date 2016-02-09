#' yahoo
#'
#' @description gets the name and other demographic data for a player, given
#' a mlbid value (an integer between 1 and roughly 10K)
#' @param id
#'
#' @return a data frame
#' @export

yahoo <- function(id) {
  url <- paste0('http://sports.yahoo.com/mlb/players/', id)
  resp <- httr::GET(url)
  if (!resp$status_code == 200) {
    stop('Bad response from yahoo.com')
  }
  h <- read_html(resp$content)

  name <- h %>% html_nodes(".player-info h1") %>% html_text()
  pos <- h %>% html_nodes('.player-info span') %>% html_text() %>%
    gsub('\n', ' ', .) %>% stringr::str_trim(., side = 'both')
  pos <- strsplit(pos, ',', fixed = TRUE) %>% unlist() %>%
    stringr::str_trim(., side = 'both') %>% extract(2)
  dob <- h %>% html_nodes(".born") %>% html_text() %>%
    gsub('\n', ' ', .) %>% gsub('Born: ', '', .) %>%
    stringr::str_trim(., side = 'both') %>%
    lubridate::parse_date_time(., orders = 'mdy') %>% as.Date(.) %>%
    as.character(.)

  player <- data.frame(
    'yahooid' = id,
    'name' = name,
    'position' = pos,
    'full_name' = NA,
    'nickname' = NA,
    'dob' = dob,
    stringsAsFactors = FALSE
  )

  player
}
