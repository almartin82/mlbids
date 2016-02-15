#' list to matrix
#'
#' @param inList list
#'
#' @return matrix
#' @references http://stackoverflow.com/a/24609337/561698
#' @export

list2mat <- function(inList) {
  UL <- unlist(inList)
  Nam <- unique(names(UL))
  M <- matrix(NA_character_,
              nrow = length(inList), ncol = length(Nam),
              dimnames = list(NULL, Nam))
  Row <- rep(seq_along(inList), sapply(inList, length))
  Col <- match(names(UL), Nam)
  M[cbind(Row, Col)] <- UL
  M
}


#' list to data frame
#'
#' @param inList list
#'
#' @return data frame
#' @export

list2df <- function(inList) {
  mat <- list2mat(inList)
  as.data.frame(mat, stringsAsFactors = FALSE)
}
