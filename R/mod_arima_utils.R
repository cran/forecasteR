#' Best parameters arima model
#'
#' @param train a ts object (train of a time series).
#' @param test a ts object (test of a time series).
#' @param period value indicate the period to use.
#' @param ar vector of values to test p, d, q of arima model.
#' @param es vector of values to test P, D, Q of arima model.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return arima model
#' @export calibrar.arima
#' @importFrom forecast forecast
#' @importFrom stats arima
#' @examples
#' calibrar.arima(AirPassengers[1:132], AirPassengers[133:144], 12, 0:1)
#' 
calibrar.arima <- function(train, test, period, ar = 0:2, es = 0:1) {
  error.c <- Inf
  model.c <- NULL
  for (p in ar) {
    for (d in ar) {
      for (q in ar) {
        for (P in es) {
          for (D in es) {
            for (Q in es) {
              tryCatch({
                model.i <- arima(train, order = c(p, d, q), seasonal = list(
                  order = c(P, D, Q), period = period))
                pred.i  <- forecast(model.i, h = length(test))
                error.i <- RMSE(pred.i$mean, test)
                if(error.i < error.c) {
                  error.c <- error.i
                  model.c <- model.i         
                }
              }, error = function(e) {}, warning = function(w) {})
            }
          }
        }
      }
    }
  }
  
  return(model.c)
}

#' Best parameters arima model
#'
#' @param x a ts object.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_acf
#' @importFrom stats acf qnorm
#' @examples
#' e_acf(AirPassengers)
#' 
e_acf <- function(x) {
  res  <- acf(x, plot = F)
  clim <- qnorm((1 + 0.95)/2)/sqrt(res$n.used)
  res  <- data.frame(Lag = res$lag[, , 1], ACF = res$acf[, , 1])
  res$Lag <- res$Lag * frequency(x)
  lim  <- round(min(c(res$ACF, -clim)) - 0.06, 1)
  
  res |> e_charts(Lag) |> e_bar(ACF, barMaxWidth = 3) |> 
    e_mark_line(data = list(yAxis = clim)) |> 
    e_mark_line(data = list(yAxis = -clim)) |> 
    e_legend(show = F) |> e_y_axis(min = lim, name = 'ACF') |>
    e_x_axis(name = 'Lag') |> e_show_loading()
}

#' Best parameters arima model
#'
#' @param x a ts object.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_pacf
#' @importFrom stats pacf qnorm
#' @examples
#' e_pacf(AirPassengers)
#' 
e_pacf <- function(x) {
  res  <- pacf(x, plot = F)
  clim <- qnorm((1 + 0.95)/2)/sqrt(res$n.used)
  res  <- data.frame(Lag = res$lag[, , 1], ACF = res$acf[, , 1])
  res$Lag <- res$Lag * frequency(x)
  lim  <- round(min(c(res$ACF, -clim)) - 0.06, 1)
  
  res |> e_charts(Lag) |> e_bar(ACF, barMaxWidth = 3) |> 
    e_mark_line(data = list(yAxis = clim)) |> 
    e_mark_line(data = list(yAxis = -clim)) |> 
    e_legend(show = F) |> e_y_axis(min = lim, name = 'PACF') |>
    e_x_axis(name = 'Lag') |> e_show_loading()
}

