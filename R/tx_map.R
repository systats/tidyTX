
#' tx_map_dict
#'
#' @param x A string vector
#' @return mapped dictionary on string for replacement\code{x}
#'
#' @export
tx_map_dict <- function(string, dict, iter_by = "for", key1 = 1, key2 = 2) {

  if(FALSE){ #iter_by == "map"

  } else if(iter_by == "for"){
    ### loop over same string length(dict) times
    for(jj in seq_along(dict)){
      string <- string %>%
        as.character() %>%
        stringr::str_replace_all(
          pattern = dict[[jj]][key1],
          replacement = dict[[jj]][key2]
        )
    }
    # cat("if not mactched catch")
  }

  return(string)
}

# test <- c("Hello Simon Roth. welcome to mapping a dictionary onto a character string.", "Well if it is successful, this part should by replaced.")
#
# dict <- list(
#   "Simon Roth" = c("Simon Roth", "PERSON"),
#   "this part" = c("this part", "PEEEP")
# )
#
# tx_map_dict(test, dict, iter_by = "for")
