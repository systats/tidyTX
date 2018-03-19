#' tx_sent
#'
#' @param x A string vector
#' @return seq of vectorized character vectors as list\code{x}
#'
#' @export
#'
#string <- c("hey how are you? are you fine?", "no im very sad an ill")
#dt <- tibble(text = string)
tx_sent <- function(data, string, lexica = c("afinn")){

  ### unnest tokens
  tokens <- data %>%
    #data.frame(x = x, stringsAsFactors = F) %>%
    mutate(id = 1:n()) %>%
    tidytext::unnest_tokens_("word", "text") %>%
    group_by(id) %>%
    mutate(idw = 1:n()) %>%
    ungroup() %>%
    select(id, idw, word)

  sent_dicts <- list(
    tidyTX::sent_afinn,
    tidyTX::sent_vader,
    tidyTX::sent_wordnet
  )

  sent_ind <- sent_dicts %>%
    purrr::reduce(dplyr::full_join, by = "word") %>%
    group_by(word) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(word = word %>% stringr::str_replace("[[:punct:]]", "")) %>%
    select(word, contains("_score")) %>%
    mutate_if(is.numeric, scale)


  fun_average <- function(x){
    m <- mean(x, na.rm = T)
    n <- ifelse(is.nan(m), NA, m)
    return(n)
  }

  sent <- tokens %>%
    left_join(sent_ind, by = "word") %>%
    group_by(id) %>%
    select(contains("_score")) %>%
    summarise_if(is.double, fun_average) %>%
    ungroup()

  sent$sent_pool <- sent %>%
    select(contains("score")) %>%
    rowMeans(na.rm = T) %>%
    ifelse(is.nan(.), NA, .)

  return(cbind(data, sent))
}
