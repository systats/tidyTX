% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tx_replace.R
\name{tx_replace_punc}
\alias{tx_replace_punc}
\title{tx_replace_punc}
\usage{
tx_replace_punc(x)
}
\arguments{
\item{x}{A string vector}
}
\value{
punctuations replaced \code{x}
}
\description{
tx_replace_punc
}
\examples{
"Oh my god? #RealDonaldTrump is a moron!" \%>\%
   tx_replace_punc()
}
