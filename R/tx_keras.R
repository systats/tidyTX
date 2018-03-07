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
tx_keras_predict_one <- function(model, string, token_fun, maxlen){
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
tx_keras_predict_str <- function(string, str, maxlen){
  new_list <- list()
  for(jj in seq_along(str)){
    new_list[[jj]] <- tidyTX::tx_keras_predict_one(model = str[[jj]][[3]], string = string, token_fun = str[[jj]][[2]], maxlen = maxlen) %>% mutate(model = str[[jj]]$id)
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
tx_confusion <- function(x, y, lib = "hchart", text_resize = F, info = F, info_list = F, ... ){

  mat <- data.frame(preds = x, real = y) %>%
    dplyr::count(preds, real) %>%
    tidyr::spread(key = "real", value = "n") %>%
    select(-preds) %>%
    as.matrix()

  ### prep matrix
  n <- sum(mat) # number of instances
  n_class <- nrow(mat) # number of classes
  diag <- diag(mat) # number of correctly classified instances per class
  rowsums <- apply(mat, 1, sum) # number of instances per class
  colsums <- apply(mat, 2, sum) # number of predictions per class
  p <- rowsums / n # distribution of instances over the actual classes
  q <- colsums / n # distribution of instances over the predicted classes

  ### custome error metrics
  # overall classification accuracy (% correctly classified)
  acc <- round(sum(diag) / n, 3)
  # per class
  precision <- diag / colsums
  recall <- diag / rowsums
  f1 <- 2 * precision * recall / (precision + recall) # harmonic mean (or a weighted average) of precision and recall
  eval <- data.frame(level = 1:length(precision), precision, recall, f1)

  if (lib == "gg") {
    gg <- data.frame(preds = x, real = y) %>%
      dplyr::count(preds, real) %>%
      dplyr::group_by(real) %>%
      dplyr::mutate(n_real = sum(n)) %>%
      ungroup() %>%
      dplyr::mutate(perc_real = round(n/n_real*100, 1)) %>%
      dplyr::mutate(label = paste0(n, "\n", perc_real, "%")) %>%
      mutate(preds = factor(preds, levels = sort(unique(preds), decreasing = T))) %>%
      mutate(real = factor(real)) %>%
      ggplot2::ggplot(ggplot2::aes(real, preds, fill = n)) +
      ggplot2::geom_tile(alpha = .8) +
      viridis::scale_fill_viridis(direction = -1) +
      scale_x_discrete(position = "top") +
      ggthemes::theme_few() +
      theme(legend.position = "none") +
      coord_equal() +
      labs(x = "Real value y", y = "Predicted value y hat")

    if(text_resize){
      gg <- gg + ggplot2::geom_text(aes(label = label, size = n))
    } else {
      gg <- gg + ggplot2::geom_text(aes(label = label))
    }

    if(info){
      gg_info <- eval %>%
        dplyr::mutate_all(function(x) round(x, 3)) %>%
        tidyr::gather("metric", "value", -level) %>%
        dplyr::mutate(level = as.factor(level)) %>%
        ggplot2::ggplot(aes(level, value, fill = level)) +
        ggplot2::geom_bar(stat = "identity", alpha = .7) +
        ggplot2::facet_wrap(~ metric, ncol = 2) +
        ggthemes::theme_few() +
        ggplot2::labs(x = "", y = "", caption = paste0("Accuracy: ", acc)) +
        ggplot2::theme(legend.position = "none")

      if(!info_list){
        scale_fill_party <- function(){
          ggplot2::scale_fill_manual("", values = c("#46962b",
                                                    "#8B1A1A", "#E2001A", "#ffed00", "black"))
        }
        gg_grid <- gridExtra::grid.arrange(
          gg,
          gg_info + scale_fill_party(),
          ncol = 2
        )
        return(gg_grid)
      } else {
        # as list for customizing
        return(list(gg, gg_info))
      }
    }

  } else if(lib == "plotly") {
    gg <- data.frame(preds = x, real = y) %>%
      dplyr::count(preds, real) %>%
      dplyr::group_by(real) %>%
      dplyr::mutate(n_real = sum(n)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(perc_real = round(n/n_real*100, 1)) %>%
      dplyr::mutate(label = paste0(n, "\n", perc_real, "%")) %>%
      dplyr::mutate(preds = factor(preds, levels = sort(unique(preds), decreasing = T))) %>%
      dplyr::mutate(real = factor(real)) %>%
      ggplot2::ggplot(ggplot2::aes(real, preds, fill = n, text = paste("percent:", perc_real))) +
      ggplot2::geom_tile() +
      viridis::scale_fill_viridis(direction = -1) +
      ggplot2::scale_x_discrete(position = "top") +
      ggthemes::theme_few() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::labs(x = "Real value y", y = "Predicted value y hat")

    gg <- plotly::ggplotly(gg)

  } else if (lib == "hchart") {
    gg <- mat %>%
      highcharter::hchart(mat, type = "heatmap", ...)
  } else {
    gg <- mat %>%
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
