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
  cbs_list <- jsonlite::fromJSON(cbs_text, simplifyVector = FALSE)
  players_json <- jsonlite::toJSON(cbs_list$body$players)
  players <- jsonlite::fromJSON(players_json, flatten = TRUE)

  clean_players <- list()

  for (i in 1:ncol(players)) {
    this_col <- players[, i]
    this_col <- lapply(X = this_col, function(x) {
      x <- unlist(x)
      ifelse(is.null(x), NA, x)
    })

    this_df <- data.frame(
      foo = this_col %>% unlist(),
      stringsAsFactors = FALSE
    )
    names(this_df)[1] <- names(players)[i]
    clean_players[[i]] <- this_df
  }

  clean_players_df <- dplyr::bind_cols(clean_players)

  clean_players_df %>%
    dplyr::filter(!is.na(pro_status)) %>%
    dplyr::select(
      -icons.video, -icons.injury, -icons.cold,
      -icons.hot, -icons.suspension, -icons.headline)
}


#' Scrape new ids for CBS
#'
#' @description hits the cbs player endpoint, then calls each playerid
#' to bring down additional detail (DOB, etc) from the cbs player pagers
#' @return writes data to data-raw/cbsids.csv. returns 'OK' if function completes
#' @export

cbs_scrape <- function() {
  p <- cbs_bulk()

  player_detail <- list()

  for (i in p$id) {
    print(i)

    tryCatch({
      this_player <- cbs(i)
      player_detail[[i]] <- this_player
    }, error = function(e) {
      cat("failed on", i, '|', conditionMessage(e), "\n")
    })

  }

  scraped <- dplyr::bind_rows(player_detail)

  final <- p %>%
    dplyr::left_join(
      scraped %>% dplyr::select(-position, -name),
      by = c('id' = 'cbsid')
  )

  write.csv(final, file = 'data-raw/cbsids.csv', row.names = FALSE)

  'OK'
}
