#' MLB
#'
#' @description gets the name and other demographic data for a player, given
#' a mlbid value (an integer between 110001 and something roughly in the ~ 700000s)
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
  bio1 <- h %>% html_nodes('.full-name+ li') %>% html_text() %>%
    gsub('\n', ' ', .)
  bio2 <- h %>% html_nodes('.full-name+ li+ li') %>% html_text() %>%
    gsub('\n', ' ', .)

  nickname <- ifelse(
    grepl('Nickname:', bio1, fixed = TRUE),
    bio1 %>% gsub('Nickname: ', '', .), NA
  )

  dob <- ifelse(
    grepl('Born:', bio1, fixed = TRUE),
    bio1 %>% gsub('Born: ', '', .),
    ifelse(
      grepl('Born:', bio2, fixed = TRUE),
      bio2 %>% gsub('Born: ', '', .),
      NA
    )
  )

  player <- data.frame(
    'mlbid' = id,
    'name' = name,
    'position' = pos,
    'full_name' = full_name,
    'nickname' = nickname,
    'dob' = dob,
    stringsAsFactors = FALSE
  )

  player
}


scrape_new_mlbids <- function() {
  #read in the data file

  #
}
