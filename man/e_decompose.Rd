% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_descom_utils.R
\name{e_decompose}
\alias{e_decompose}
\title{Decompose plot}
\usage{
e_decompose(serie, f = NULL, noms = NULL)
}
\arguments{
\item{serie}{a ts object.}

\item{f}{vector of dates for the time series.}

\item{noms}{vector of names for y axis.}
}
\value{
echarts4r plot
}
\description{
Decompose plot
}
\examples{
e_decompose(AirPassengers)

}
\author{
Diego Jimenez <diego.jimenez@promidat.com>
}
