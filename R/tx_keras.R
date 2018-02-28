#' tx_keras_text_seq
#'
#' @param x A string vector
#' @return seq of vectorized character vectors as list\code{x}
#'
#' @export
tx_text_to_seq <- function(token_fun, string, maxlen){
  text_seq <- token_fun %>%
    texts_to_sequences(., array(string, 1)) %>%
    keras::pad_sequences(., maxlen = maxlen, value = 0)
  return(text_seq)
}


#' tx_k_onehot
#'
#' @param x A string vector
#' @return transforms a k-level factor vector into a n*k matrix (one-hote encoding)\code{x}
#'
#' @export
tx_onehot <- function(x){
  return(dummies::dummy(dat$valid$party_id) %>% as.matrix())
}


#' tx_keras_predict
#'
#' @param data a list of data objects
#' @param model a pretrained keras model
#' @param index if index + 1?
#' @return a target vector \code{x}
#'
#' @export
tx_keras_predict <- function(seq, model, index = 1){
  preds <- predict_classes(model, x = seq) + index  %>% as.vector()
  return(preds)
}

#' tx_keras_predict_one
#'
#' @param string a one element string
#' @param token_fun a pre-constructed tokenizer
#' @param model a pre-trained model
#' @return a \code{x}
#'
#' @export
tx_keras_predict_one <- function(string, token_fun, model){
  new_seq <- token_fun %>%
    texts_to_sequences(., array(string, 1)) %>%
    pad_sequences(., maxlen = maxlen, value = 0)

  preds <- predict(model, x = new_seq) %>%
    as.data.frame()

  return(preds)
}


#' tx_keras_predict_str
#'
#' @param string a one element string
#' @param str a list structure of parts brought by different models
#' @return data frame of predictions \code{x}
#'
#' @export
tx_keras_predict_str <- function(string, str){
  new_list <- list()
  for(jj in seq_along(str)){
    new_list[[jj]] <- tidyTX::tx_keras_predict_one(string, token_fun = str[[jj]][[2]] , model = str[[jj]][[3]]) %>% mutate(model = str[[jj]][[1]])
  }
  dat <- purrr::reduce(new_list, rbind)
  return(dat)
}


#' tx_keras_plot
#'
#' @param history object
#' @return a ggplot2 object \code{x}
#'
#' @export
tx_keras_plot <- function(history) {
  data.frame(metric = history$metrics, stringsAsFactors = F) %>%
    mutate(epoch = 1:n()) %>%
    tidyr::gather("metric", "value", - epoch) %>%
    mutate(col = str_detect(metric, "val_")) %>%
    mutate(kind = str_replace_all(metric, "val_", "")) %>%
    ggplot(aes(epoch, value, colour = col, alpha = col)) +
    geom_point() +
    geom_line() +
    scale_alpha_discrete(range = c(.3, .7)) +
    facet_wrap(~kind, ncol = 1)
}

#' tx_keras_plot
#'
#' @param history object
#' @return a ggplot2 object \code{x}
#'
#' @export
tx_hc_confusion <- function(pred, real){
  table(pred, real) %>%
    prop.table() %>%
    as.data.frame() %>%
    dplyr::rename(preds = Var1, true = Var2, perc = Freq) %>%
    highcharter::hchart(type = "heatmap", highcharter::hcaes(x = true, y = preds, value = perc)) %>%
    highcharter::hc_legend(layout = "vertical", verticalAlign = "top", align = "right")
}

#' tx_gg_pred
#'
#' @param a string
#' @return a ggplot2 object \code{x}
#'
#' @export
tx_gg_pred <- function(string){
  tidyTX::tx_keras_predict_one(string) %>%
    tidyr::gather("party", "prob") %>%
    dplyr::mutate(party= factor(party, levels = unique(party))) %>%
    ggplot2::ggplot(ggplot2::aes(party, prob, fill = party)) +
    ggplot2::geom_bar(stat = "identity")
}
