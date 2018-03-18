# Data and dictionaries for text normalization

#' German stopwords
#'
#' @format A data frame with 358473 rows and 2 variables:
#' \describe{
#'   \item{word}{a key word}
#'   \item{lemma}{its mapped lemma.}
#'   ...
#' }
#' @source \url{http://www.lexiconista.com/datasets/lemmatization/}
"stop_words_de"

#' English stopwords
#'
#' @format A data frame with 358473 rows and 2 variables:
#' \describe{
#'   \item{word}{a key word}
#'   \item{lemma}{its mapped lemma.}
#'   ...
#' }
#' @source \url{http://www.lexiconista.com/datasets/lemmatization/}
"stop_words_en"


#' German Lemma Dictionary
#'
#' @format A data frame with 358473 rows and 2 variables:
#' \describe{
#'   \item{word}{a key word}
#'   \item{lemma}{its mapped lemma.}
#'   ...
#' }
#' @source \url{http://www.lexiconista.com/datasets/lemmatization/}
"hash_lemma_de"

#' English Lemma Dictionary
#'
#' @format A data frame with 41759 rows and 2 variables:
#' \describe{
#'   \item{word}{a key word}
#'   \item{lemma}{its mapped lemma.}
#'   ...
#' }
#' @source \url{http://www.lexiconista.com/datasets/lemmatization/}
"hash_lemma_en"

#' French Lemma Dictionary
#'
#' @format A data frame with 224001 rows and 2 variables:
#' \describe{
#'   \item{word}{a key word}
#'   \item{lemma}{its mapped lemma.}
#'   ...
#' }
#' @source \url{http://www.lexiconista.com/datasets/lemmatization/}
"hash_lemma_fr"



#' Wordnet Sentiment Dictionary
#'
#' @format A data frame with 24,428 rows and 3 variables:
#' \describe{
#'   \item{word}{a key word}
#'   \item{wordnet_score}{ sentiment number }
#'   \item{wordnet_class}{ sentiment class }
#'   ...
#' }
#' @source \url{http://www.lexiconista.com/datasets/lemmatization/}
"sent_wordnet"


#' Vader Sentiment Dictionary
#'
#' @format A data frame with 7,516 rows and 6 variables:
#' \describe{
#'   \item{word}{a key word}
#'   \item{vader_score}{ sentiment number }
#'   \item{vader_intens}{ sentiment number }
#'   \item{vader_seq}{ sentiment number }
#'   \item{vader_med}{ sentiment number }
#'   \item{vader_class}{ sentiment class }
#'   ...
#' }
#' @source \url{http://www.lexiconista.com/datasets/lemmatization/}
"sent_vader"


#' AFINN Sentiment Dictionary
#'
#' @format A data frame with 2,476 rows and 2 variables:
#' \describe{
#'   \item{word}{a key word}
#'   \item{afinn_score}{ sentiment number }
#'   ...
#' }
#' @source \url{http://www.lexiconista.com/datasets/lemmatization/}
"sent_afinn"

