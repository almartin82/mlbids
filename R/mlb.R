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


scrape_new_mlbids <- function(exclude_known = TRUE, verbose = TRUE) {
  #read in the data file
  extant_mlbids <- read.csv('data-raw/mlbids.csv', stringsAsFactors = FALSE)

  #read in the range of ids to search
  load(file = 'data-raw/search_range.Rda')
  search_seq <- seq(search_range[1], search_range[2], 1)

  #exclude known players from the search
  if (exclude_known) {
    mask <- search_seq %in% extant_mlbids$mlbid
    search_seq <- search_seq[!mask]
  }

  #iterate over the search range and look up
  sprintf('looking up ids from %s... to ...%s', head(search_seq), tail(search_seq))

  players <- list()
  for (i in search_seq) {
    if (verbose) cat(i); cat('...')

    tryCatch({
      m <- mlb(i)
      players[[paste0('p', i)]] <- m
      if (verbose) cat(m$Name); cat('\n')
    }, error = function(e) {
      cat("failed on ", i, ': ', conditionMessage(e), "\n")
    })
  }

  #bind rows
  sprintf('read %s new players from mlb.com!', length(players))
  new_mlbids <- dplyr::bind_rows(players)

  #bind extant and new
  final <- rbind(extant_mlbids, new_mlbids)

  #add 500 new ids to the original space and save
  search_range <- c(min(search_range), max(search_range) + 100)
  save(search_range, file = 'data-raw/search_range.Rda')

  #write the search space and data file
  write.csv(final, file = 'data-raw/mlbids.csv', row_names = FALSE)
}
