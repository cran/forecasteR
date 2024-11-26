#' @name forecasteR
#' @aliases forecasteR
#' @docType package
#' @title Time Series Forecast System
#' @author
#' Maintainer: Oldemar Rodriguez Rojas <oldemar.rodriguez@ucr.ac.cr>\cr
#' \itemize{
#'   \item Oldemar Rodriguez Rojas <oldemar.rodriguez@ucr.ac.cr>
#'   \item Diego Jiménez Alvarado
#' }
#' @description
#' A web application for displaying, analysing and forecasting univariate time series. Includes basic methods such as mean, naïve, seasonal naïve and drift, as well as more complex methods such as Holt-Winters Box,G and Jenkins, G (1976) <doi:10.1111/jtsa.12194> and ARIMA Brockwell, P.J. and R.A.Davis (1991) <doi:10.1007/978-1-4419-0320-4>.
#' @details
#' \tabular{ll}{
#' Package: \tab forecasteR\cr
#' Type: \tab Package\cr
#' Version: \tab 3.0.2\cr
#' Date: \tab 2024-11-25\cr
#' License: \tab GPL (>=2)\cr
#' }
#' @keywords package
"_PACKAGE"

NULL
utils::globalVariables(c(
  "n", "z", "vars", "for", "Lag", "ACF", "tail", "ts.union", "fecha",
  "duration", "runjs", "ts", "head", "auto.arima", "valor", "name", "linf",
  "lsup", "x", "w", "y", "noms", "colors", "table_m", "formula"
))
