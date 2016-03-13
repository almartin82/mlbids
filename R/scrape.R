#' Scrape new ids for MLB and Yahoo
#'
#' @description a utility function that looks for newly added players from
#' one of the fantasy providers
#' @param id_type c('mlb', 'yahoo')
#' @param exclude_known should we ignore the players who have already have
#' records?  default is TRUE.
#' @param verbose output current player name to the console.  default is TRUE
#' @param expand_range at the close, we'll expand the number of ids to check
#' by some fixed number, to make sure that we are looking high enough
#' to get new ones.  default is 200.
#'
#' @return writes data to data-raw/whatever.csv. returns 'OK' if function completes
#' @export

scrape_new_ids <- function(
  id_type = 'mlb',
  exclude_known = TRUE,
  verbose = TRUE,
  expand_range = 500,
  backtrack_range = -500
) {

  #read in the data file
  extant_ids <- read.csv(
    file.path('data-raw', sprintf('%sids.csv', id_type)),
    stringsAsFactors = FALSE
  )

  #read in the range of ids to search
  load(file = file.path('data-raw', sprintf('%s_search_range.Rda', id_type)))
  search_seq <- seq(search_range[1], search_range[2], 1)

  #exclude known players from the search
  if (exclude_known) {
    mask <- search_seq %in% extant_ids[, sprintf('%sid', id_type)]
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
      m <- do.call(id_type, list('id' = i))
      players[[paste0('p', i)]] <- m
      if (verbose) cat(paste(m$name), m$dob, '\n')
    }, error = function(e) {
      cat("failed on", i, '|', conditionMessage(e), "\n")
    })
  }

  #bind rows
  sprintf('read %s new players from %s.com!', length(players), id_type)
  new_ids <- dplyr::bind_rows(players)

  #remove any rows that were brought down fresh
  extant_ids <- anti_join(extant_ids, new_ids, by = paste0(id_type, 'id'))
  #bind extant and new
  final <- rbind(extant_ids, new_ids)

  #add new ids to the original space and save
  search_range <- c(
    min(search_range),
    max(final[, sprintf('%sid', id_type)]) + expand_range
  )
  save(
    search_range,
    file = file.path('data-raw', sprintf('%s_search_range.Rda', id_type))
  )

  #write the search space and data file
  write.csv(
    final,
    file = file.path('data-raw', sprintf('%sids.csv', id_type)),
    row.names = FALSE
  )

  'OK'
}
