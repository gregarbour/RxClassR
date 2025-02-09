% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apis.R
\name{similar_by_rxcuis}
\alias{similar_by_rxcuis}
\title{Find similar classes for a list of drugs specified by RxNorm identifiers.}
\usage{
similar_by_rxcuis(rxcuis, relaSource = NULL, rela = NULL,
  scoreType = 0, top = 10, equivalenceThreshold = NULL,
  inclusionThreshold = NULL, trunc = T)
}
\arguments{
\item{rxcuis}{a list of RxNorm drug identifiers (RxCUIs). A maximum of 500 are permitted.}
}
\description{
Find similar classes for a list of drugs specified by RxNorm identifiers.
}
\examples{
similar_by_rxcuis(rxcuis = 7052, top = 10)
similar_by_rxcuis(rxcuis = c(7052, 7676, 7804, 23088), top = 3)
similar_by_rxcuis(rxcuis = c(7052, 7676, 7804, 23088), top = 3, relaSource = 'DAILYMED')
}
