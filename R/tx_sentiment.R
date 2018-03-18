#' tx_sent
#'
#' @param x A string vector
#' @return seq of vectorized character vectors as list\code{x}
#'
#' @export
#'
string <- c("hey how are you? are you fine?", "no im very sad an ill")
dt <- tibble(text = string)

tx_sent <- function(data, string, lexica = c("afinn")){

  ### unnest tokens
  tokens <- dt %>%
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
    purrr::reduce(dplyr::left_join, by = "word") %>%
    group_by(id, idw) %>%
    slice(1) %>%
    ungroup()

  sent <- tokens %>%
    left_join(tidyTX::sent_afinn, by = "word") %>%
    left_join(tidyTX::sent_vader, by = "word") %>%
    left_join(tidyTX::sent_wordnet, by = "word") %>%
    group_by(idw) %>%
    select(contains("_score"))
    summarise_all(mean, na.rm = T)

  # sent_ind <- c(list(tokens), dict_all) %>%
  #   purrr::reduce(dplyr::left_join, by = "word") %>%
  #   group_by(id, idw) %>%
  #   slice(1) %>%
  #   ungroup()
  #
  # sent_scale <- sent_ind %>%
  #   select(-id, -idw, -word) %>%
  #   purrr::map_df(scale)


  return(sent)
}

dt %>%
  tx_sent("text")


str_sentiment <- function(x){



  av <- function(x){
    ifelse(is.na(mean(x, na.rm = T)), NA, mean(x, na.rm = T))
  }

  sdv <- function(x){
    ifelse(is.na(stats::sd(x, na.rm = T)), NA, stats::sd(x, na.rm = T))
  }

  # num <- function(x){
  #   ss <- sent_dat$nrc %>%
  #     is.na() %>%
  #     table() %>%
  #     as.data.frame()
  #   ss$. ==F
  # }
  #
  #library(plyr)
  #join_all(sent_ind, by='word', type='left', match = "first")

  sent_ind <- c(list(tokens), dict_all) %>%
    purrr::reduce(dplyr::left_join, by = "word") %>%
    group_by(id, idw) %>%
    slice(1) %>%
    ungroup()

  sent_scale <- sent_ind %>%
    select(-id, -idw, -word) %>%
    purrr::map_df(scale)

  sent_dat <- cbind(sent_ind[,1:3], sent_scale) %>%
    select(-idw, -word) %>%
    group_by(id) %>%
    summarise_if(
      .predicate = is.double,
      .funs = funs(av, sdv)
    )

  colnames(sent_dat)[-1] <- paste0("sent_", colnames(sent_dat)[-1])

  var_order <- sent_dat[-1] %>%
    colnames() %>%
    sort()

  sent_dat <- sent_dat %>%
    select(id, var_order)

  sent_dat$sent_pool <- sent_dat %>%
    select(contains("_av")) %>%
    mutate_all(scale) %>%
    rowMeans(., na.rm = T) %>%
    ifelse(is.na(.), NA, .)

  sent_dat$sent_sdv <- sent_dat %>%
    select(contains("_sdv")) %>%
    mutate_all(scale) %>%
    rowMeans(., na.rm = T) %>%
    ifelse(is.na(.), NA, .)

  return(sent_dat)
  cat("\n", "next", "\n")
}
