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


#' Scrape new mlbids
#'
#' @description a utility function that looks for newly added players to mlb.com,
#' or regenerates the master list of players in data-raw/mlb.csv
#' @param exclude_known should we ignore the players who have already have
#' records in mlb.csv?  default is TRUE.
#' @param verbose output current player name to the console.  default is FALSE
#' @param expand_range at the close, we'll expand the number of ids to check
#' by some fixed number, to make sure that we are looking high enough
#' to get new ones.  default is 200.
#'
#' @return 'OK' if function completes
#' @export

scrape_new_mlbids <- function(
  exclude_known = TRUE,
  verbose = TRUE,
  expand_range = 200,
  backtrack_range = 0
  ) {

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
  if (verbose) {
    work <- sprintf(
      'looking up ids from %s... to ...%s',
      head(search_seq) %>% unlist() %>% paste(., collapse = ', '),
      tail(search_seq) %>% unlist() %>% paste(., collapse = ', ')
    )
    cat(work); cat('\n\n')
  }

  players <- list()
  for (i in search_seq) {
    if (verbose) cat(paste0(i,'...'))
    tryCatch({
      m <- mlb(i)
      players[[paste0('p', i)]] <- m
      if (verbose) cat(paste(m$name), m$dob, '\n')
    }, error = function(e) {
      cat("failed on", i, '|', conditionMessage(e), "\n")
    })
  }

  #bind rows
  sprintf('read %s new players from mlb.com!', length(players))
  new_mlbids <- dplyr::bind_rows(players)

  #remove any rows that were brought down fresh
  extant_mlbids <- anti_join(extant_mlbids, new_mlbids, by = 'mlbid')
  #bind extant and new
  final <- rbind(extant_mlbids, new_mlbids)

  #add new ids to the original space and save
  search_range <- c(min(search_range), max(final$mlbid) + expand_range)
  save(search_range, file = 'data-raw/search_range.Rda')

  #write the search space and data file
  write.csv(final, file = 'data-raw/mlbids.csv', row.names = FALSE)

  'OK'
}
