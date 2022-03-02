#' Tendencia y Estacionalidad
#'
#' @param x a ts object.
#' @param d a vector of dates to use on axis x (Optional).
#' @param noms a vector of 3 to indicate the names to use on legend.
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return data.frame
#' @export e_tc
#' @import echarts4r
#' @importFrom stats lm
#' @examples
#' e_tc(AirPassengers)
#' 
e_tc <- function(x, d = NULL, noms = c("Time Series", "Trend", "Cyclicality")) {
  if(is.null(d)) {
    d <- 1:(length(x))
  }
  
  t     <- 1:(length(x))
  t2    <- t^2
  sin.t <- sin(2 * pi * t)
  cos.t <- cos(2 * pi * t)
  
  if(min(x) < 0) {
    aux    <- abs(min(x)) + 1
    x      <- x + aux
    lserie <- log(x)
    
    regresion <- lm(lserie ~ t + t2)$fit
    fourier   <- lm(lserie ~ t + t2 + sin.t + cos.t)$fit
    
    lserie    <- lserie - log(aux)
    regresion <- regresion - log(aux)
    fourier   <- fourier - log(aux)
  } else {
    lserie <- log(x)
    
    regresion <- lm(lserie ~ t + t2)$fit
    fourier   <- lm(lserie ~ t + t2 + sin.t + cos.t)$fit
    
    lserie    <- lserie
    regresion <- regresion
    fourier   <- fourier
  }
  
  df <- data.frame(x = d, w = lserie, y = regresion, z = fourier)
  
  df |> e_charts(x) |> e_line(w, name = noms[1]) |> 
    e_line(y, name = noms[2], showSymbol = F) |> 
    e_line(z, name = noms[3], showSymbol = F) |>
    e_datazoom(type = "slider") |> e_x_axis(scale = T) |>
    e_y_axis(scale = T) |> e_tooltip() |> e_show_loading()
}
