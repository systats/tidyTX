#' tx_get_dtm
#'
#' @param text A string vector
#' @param id a unique identifier
#' @param vec a pre built word vectorizer
#' @return a dtm
#'
#' @export
tx_get_dtm <- function(text, id, vec){
  dtm <- text %>%
    itoken(ids = id, progressbar = FALSE) %>%
    create_dtm(vec)

  return(dtm)
}
