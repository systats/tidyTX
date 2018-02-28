#' tx_replace_url
#'
#' @param x A string vector
#' @return cleaned \code{x}
#'
#' @examples
#' "Hi http://google.com my god" %>%
#'    tx_replace_url()
#' @export

tx_replace_url <- function(x) {
  out <- stringr::str_replace_all(x, "https?[:]//[[:graph:]]+", "URL")
  return(out)
}

#' tx_replace_emoji
#'
#' @param x A string vector
#' @return emoji corrected \code{x}
#'
#' @examples
#' "Hi :D my god. My brain hurts :/" %>%
#'    tx_replace_emoji()
#' @export

tx_replace_emoji <- function(x){

  rgx_smile <- ":-\\)|\\(-:|:\\)|\\(:|=\\)|\\(=|=D|=\\]|\\[=|:\\]|\\[:|\\^_\\^|\\(\\^_\\^\\)"
  rgx_laugh <- ":-D|:D|X-D|XD|xD|Xd|:d|:P|:-P|:p"
  rgx_love <- "<3|:\\*"
  rgx_wink <- ";-\\)|;\\)|;-D|;D|\\(;|\\(-;"
  rgx_frown <- ":-\\(|:-/|:\\(|\\):|\\)-:|:\\/|=\\/|-_-"
  rgx_cry <- ':,\\(|:"\\(|:\\(\\('

  out <- x %>%
    str_replace_all(rgx_smile, " EMO_SMILEY ") %>%
    str_replace_all(rgx_laugh, " EMO_LAUGH ") %>%
    str_replace_all(rgx_love, " EMO_LOVE" ) %>%
    str_replace_all(rgx_wink, " EMO_WINK" ) %>%
    str_replace_all(rgx_frown, " EMO_FROWN ") %>%
    str_replace_all(rgx_cry, " EMO_CRY ")

  return(out)
}

#' tx_replace_twitter
#'
#' @param x A string vector
#' @return emoji corrected \code{x}
#'
#' @examples
#' "RT @meee: Oh my god. #RealDonaldTrump is a moron :D" %>%
#'    tx_replace_twitter()
#' @export

tx_replace_twitter <- function(x, rm_hash = T, replace_hash =F, replace_hndl = T, rm_retweet = T){
  if(rm_retweet){
    x <- x %>%
      stringr::str_replace_all('^RT.*?: ', '')
  }
  if(replace_hash & replace_hndl){
    x <- tw$text[1:5] %>%
      stringr::str_replace_all('#(\\w+)', 'HASH') %>%
      stringr::str_replace_all('@(\\w+)', 'HNDL')
  } else {
    x <- x %>%
      stringr::str_replace_all('@(\\w+)', 'HNDL')
  }
  if(rm_hash){
    x <- x %>%
      stringr::str_replace_all('#', '')
  }
  return(x)
}


#' tx_replace_punc
#'
#' @param x A string vector
#' @return punctuations replaced \code{x}
#'
#' @examples
#' "Oh my god? #RealDonaldTrump is a moron!" %>%
#'    tx_replace_punc()
#' @export

tx_replace_punc <- function(x){
  out <- x %>%
    stringr::str_replace_all("\n", " ") %>%
    stringr::str_replace_all("\\.|\\:|\\;", " PUNC_DOT ") %>%
    stringr::str_replace_all("\\!", " PUNC_EXCL ") %>%
    stringr::str_replace_all("\\?", " PUNC_QUES ") %>%
    stringr::str_replace_all("\\.\\.\\.|…", " PUNC_DOTS ") %>%
    stringr::str_replace_all("[[:punct:]]^[_]", " ")# %>%
    #stringr::str_replace_all("[[:...:]]", " ")

  return(out)
}

#' tx_spacing
#'
#' @param x A string vector
#' @return optimized white space in \code{x}
#'
#' @examples
#' "  Oh my god?   #RealDonaldTrump    is a moron!" %>%
#'    tx_spacing()
#' @export

tx_spacing <- function(x){
  out <- x %>%
    stringr::str_replace_all("\\/t", " ") %>%
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_trim() %>%
    stringr::str_to_lower()
  return(out)
}

#' tx_replace_currency
#'
#' @param x A string vector
#' @return currency symbols replaced in \code{x}
#'
#' @examples
#' "Oh my god? the product costs ¥30, £11, €10, $5 ..." %>%
#'    tx_replace_currency()
#' @export

tx_replace_currency <- function(x){
  out <- x %>%
    str_replace_all("\\$", " USD ") %>%
    str_replace_all("\\€", " EUR ") %>%
    str_replace_all("\\¥", " JPY ") %>%
    str_replace_all("\\£", " GBP ")
  return(out)
}


#' tx_replace_str
#'
#' @param x A string vector
#' @return `\n` `\t` replaced as well as to_lower in \code{x}
#'
#' @examples
#' "Oh my god \n the product costs \t ..." %>%
#'    tx_replace_str()
#' @export

tx_replace_str <- function(x){
  out <- x %>%
    str_replace_all("\n|\t", " ") %>%
    str_replace_all("\\s+", " ")
  return(out)
}

#' tx_n_tokens
#'
#' @param x A string vector
#' @return counts tokens of string\code{x}
#'
#' @examples
#' "Oh my god? #RealDonaldTrump is a moron!" %>%
#'    tx_n_tokens()
#' @export
tx_n_tokens <- function(string, token = "word"){

  if(token == "word"){
    n_tokens <- str_count(string, "\\W+")
  } else if(token == "char"){
    n_tokens <- nchar(string)
  } else if(token == "space"){
    pattern <- "\\S+"
    pattern <- "[[:alpha:]]+"
    n_tokens <- str_count(string, pattern)
  }

  return(n_tokens)
}


#' tx_party_de
#'
#' @param x a ggplot2 plot
#' @return a color palette\code{x}
#'
#' @examples
#' "Oh my god? #RealDonaldTrump is a moron!" %>%
#'    tx_n_tokens()
#' @export
tx_party_de <- function(x, ...) {
  x + ggplot2::scale_fill_manual(...,
        values = c("#46962b", "#8B1A1A", "#E2001A", "#ffed00", "black")
      )
}
