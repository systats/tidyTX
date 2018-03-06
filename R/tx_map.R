
#' tx_map_dict
#'
#' @param x A string vector
#' @return mapped dictionary on string for replacement\code{x}
#'
#' @export
# tx_map_dict <- function(string, dict, iter_by = "for", key1 = 1, key2 = 2) {
#
#   if(FALSE){ #iter_by == "map"
#
#   } else if(iter_by == "for"){
#     ### loop over same string length(dict) times
#     for(jj in seq_along(dict)){
#       string <- string %>%
#         as.character() %>%
#         stringr::str_replace_all(
#           pattern = dict[[jj]][key1],
#           replacement = dict[[jj]][key2]
#         )
#     }
#     # cat("if not mactched catch")
#   }
#
#   return(string)
# }
tx_map_dict <- function (string, dict = json, iter_by = "for", key1 = 0, key2 = 1) {

  if (iter_by == "for") {
    for (jj in seq_along(dict)) {
      if(key1 == 0){
        string <- string %>%
          stringr::str_replace_all(pattern = names(dict[jj]),
                                   replacement = dict[[jj]][key2])
      } else {
        string <- string %>%
          stringr::str_replace_all(pattern = dict[[jj]][key1],
          replacement = dict[[jj]][key2])
      }
    }
  }
  return(string)
}

#' tx_discard_tokens
#' @export
tx_discard_tokens <- function(data, text, dict, purrr = T){

  if(purrr){
    string <- data %>%
      tidytext::unnest_tokens_("words", text, to_lower = T) %>%
      split(.$id) %>%
      purrr::map(.f = ~ dplyr::anti_join(.x, dict, by = "words")) %>%
      purrr::map_chr(.f = ~ paste(.x$words, collapse = " "))

    data %>%
      dplyr::mutate(ctext = string)

    # data %>%
    #   mutate(id = 1:n()) %>%
    #   tidytext::unnest_tokens_("words", text, to_lower = T) %>%
    #   group_by(id) %>%
    #   tidyr::nest(.key = "words") %>%
    #   dplyr::mutate(
    #     text = purrr::map_chr(words, ~ paste(.x$words, collapse = " ")),
    #     cwords = purrr::map(words, ~ dplyr::anti_join(.x, dict, by = "words")),
    #     ctext = purrr::map_chr(cwords, ~ paste(.x$words, collapse = " "))
    #  )
  } else {
    data %>%
      tidytext::unnest_tokens(words, text) %>%
      dplyr::anti_join(st_de) %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(ctext = paste(words, collapse = " ")) %>%
      dplyr::right_join(data)
  }
}
