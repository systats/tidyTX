#' tx_keras_text_seq
#'
#' @param x A string vector
#' @return seq of vectorized character vectors as list\code{x}
#'
#' @export
tx_text_to_seq <- function(token_fun, string, maxlen){
  text_seq <- token_fun %>%
    texts_to_sequences(., string) %>%
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
  return(dummies::dummy(x) %>% as.matrix())
}


#' tx_keras_predict
#'
#' @param data a list of data objects
#' @param model a pretrained keras model
#' @param index if index + 1?
#' @return a target vector
#'
#' @export
tx_keras_predict <- function(model, seq, index = 1){
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
    new_list[[jj]] <- tidyTX::tx_keras_predict_one(string, token_fun = str[[jj]][[2]] , model = str[[jj]][[3]]) %>% mutate(model = str[[jj]]$id)
  }
  dat <- purrr::reduce(new_list, rbind)
  return(dat)
}


#' tx_plot_party_str
#' @export
tx_plot_party_str <- function(x, plotly = F){
  if(!plotly){
    x %>%
      tidyr::gather("party", "prob", -model) %>%
      dplyr::mutate(party = factor(party, levels = unique(party))) %>%
      ggplot2::ggplot(ggplot2::aes(party, prob, alpha = model, fill = party)) +
      ggplot2::geom_bar(stat = "identity", position = 'dodge') +
      ggplot2::geom_text(
        ggplot2::aes(label = model),
        position = ggplot2::position_dodge(width = 0.9),
        vjust = -.25) +
      ggplot2::scale_alpha_discrete(range = c(.5,.9)) +
      ggplot2::scale_fill_manual("", values = c("#46962b",
                                                "#8B1A1A", "#E2001A", "#ffed00", "black")) +
      ggthemes::theme_hc()
  } else {
    gg <-   x %>%
      tidyr::gather("party", "prob", -model) %>%
      dplyr::mutate(party = factor(party, levels = unique(party))) %>%
      ggplot2::ggplot(ggplot2::aes(party, prob, alpha = model, fill = party)) +
      ggplot2::geom_bar(stat = "identity", position = 'dodge') +
      ggplot2::scale_alpha_discrete(range = c(.5,.9)) +
      ggplot2::scale_fill_manual("", values = c("#46962b",
                                                "#8B1A1A", "#E2001A", "#ffed00", "black")) +
      ggthemes::theme_hc()

    plotly::ggplotly(gg)
  }
}

#' tx_keras_plot
#'
#' @param history object
#' @return a ggplot2 object \code{x}
#'
#' @export
tx_keras_plot <- function(history) {
  data.frame(metric = history$metrics, stringsAsFactors = F) %>%
    dplyr::mutate(epoch = 1:n()) %>%
    tidyr::gather("metric", "value", - epoch) %>%
    dplyr::mutate(col = stringr::str_detect(metric, "val_")) %>%
    dplyr::mutate(kind = stringr::str_replace_all(metric, "val_", "")) %>%
    ggplot2::ggplot(ggplot2::aes(epoch, value, colour = col, alpha = col)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_alpha_discrete(range = c(.3, .7)) +
    ggplot2::facet_wrap(~kind, ncol = 1)
}

#' tx_confusion
#' @export
tx_confusion <- function(x, y, lib = "hchart", ...){

  if (lib == "gg") {
    gg <- data.frame(preds = x, real = y) %>%
      dplyr::count(preds, real) %>%
      ggplot2::ggplot(ggplot2::aes(real, preds, fill = Freq, label = n)) +
      ggplot2::geom_tile() +
      ggplot2::geom_text() +
      viridis::scale_fill_viridis(direction = -1)
  } else if (lib == "hchart") {
    gg <- data.frame(preds = x, real = y) %>%
      dplyr::count(preds, real) %>%
      tidyr::spread(key = "real", value = "n") %>%
      as.matrix() %>%
      highcharter::hchart(mat, type = "heatmap", ...)
  } else {
    gg <- data.frame(preds = x, real = y) %>%
      dplyr::count(preds, real) %>%
      tidyr::spread(key = "real", value = "n") %>%
      as.matrix() %>%
      d3heatmap::d3heatmap(mat, colors = "Spectral", ...)
  }
  return(gg)
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
