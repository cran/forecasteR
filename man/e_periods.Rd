% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_periodograma_utils_.R
\name{e_periods}
\alias{e_periods}
\title{Periodogram Plot}
\usage{
e_periods(x, p = NULL, noms = NULL)
}
\arguments{
\item{x}{a ts object.}

\item{p}{which important period to plot.}

\item{noms}{vector of lenght 3 to indicate the text to use.}
}
\value{
echarts4r plot
}
\description{
Periodogram Plot
}
\examples{
e_periods(AirPassengers)

}
\author{
Diego Jimenez <diego.jimenez@promidat.com>
}
