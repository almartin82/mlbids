#' MLB
#'
#' @description gets the name and other demographic data for a player, given
#' a mlbid value (an integer between 110001 and something roughly in the ~ 650000s)
#' @param id an integer
#'
#' @return if given a valid id, a data frame with player name, dob, id, etc.
#' @export
#'
#' @examples
#' ex <- mlb(110001)

mlb <- function(id) {
  url <- paste0('http://m.mlb.com/player/', id)
  resp <- httr::GET(url)
  if (!resp$status_code == 200) {
    stop('Bad response from mlb.com.')
  }
  h <- read_html(resp$content)

  name <- h %>% html_nodes(".player-name") %>% html_text()
  pos <- h %>% html_nodes('.player-vitals ul li') %>% extract(1) %>% html_text()
  full_name <- h %>% html_nodes(".full-name") %>% html_text() %>%
    gsub('Full Name: ', '', .) %>% gsub('\n', ' ', .)


  bio <- h %>% html_nodes('.player-bio') %>% extract(1) %>%
    html_nodes('ul') %>% extract(1)

  bio_data <- bio %>%
    html_nodes('li') %>%
    xml_text() %>%
    lapply(., function(x) gsub('\n', ' ', x)) %>%
    unlist() %>%
    strsplit(., ': ')

  bio_keys <- lapply(bio_data, function(x) extract(x, 1)) %>%
    unlist() %>% trim_whitespace()
  bio_values <- lapply(bio_data, function(x) extract(x, 2)) %>%
    unlist() %>% trim_whitespace()

  bio_df <- data.frame(
    keys = bio_keys,
    values = bio_values,
    stringsAsFactors = FALSE
  )

  nickname <- bio_df[bio_df$keys == 'Nickname', 'values']
  dob <- bio_df[bio_df$keys == 'Born', 'values']

  player <- data.frame(
    'mlbid' = id,
    'name' = name,
    'position' = pos,
    'full_name' = full_name,
    'nickname' = ifelse(length(nickname) == 0, NA, nickname),
    'dob' = ifelse(length(dob) == 0, NA, dob),
    stringsAsFactors = FALSE
  )

  player
}
