#' #' Torch model for time series.
#' #'
#' #' @param x a ts object.
#' #' @param model a keras model.
#' #' @param lag indicates by value to test alpha, beta and gamma.
#' #' @param batch_size indicates by value to test alpha, beta and gamma.
#' #' @param epochs indicates by value to test alpha, beta and gamma.
#' #' @param verbose indicates by value to test alpha, beta and gamma.
#' #'
#' #' @author Diego Jimenez <diego.jimenez@promidat.com>
#' #' @return keras model
#' #' @export tskeras
#' #' @import keras
#' #' @importFrom stats end
#' #' @examples
#' #' \donttest{
#' #'   library(keras)
#' #'   modelo.deep <- keras_model_sequential() %>% 
#' #'   layer_lstm(
#' #'     units = 10, activation = 'tanh', batch_input_shape = c(1, 12, 1),
#' #'     return_sequences = TRUE, stateful = TRUE) %>%
#' #'   layer_dense(units = 1) %>%
#' #'   compile(loss = 'mse', optimizer = 'adam', metrics = 'mse')
#' #'   tskeras(AirPassengers, modelo.deep, lag = 12, epochs = 1)
#' #' }
#' #' 
#' tskeras <- function(x, model, lag = 1, batch_size = 1, epochs = 20, verbose = 0) {
#'   factores_escala <- c(mean(x), sd(x))
#'   
#'   x_scale <- (x - factores_escala[1]) / factores_escala[2]
#'   
#'   matrix_x_scale <- as.matrix(x_scale)
#'   
#'   x_data <- t(sapply(
#'     1:(length(matrix_x_scale) - lag - lag),
#'     function(x) matrix_x_scale[x:(x + lag - 1), 1]
#'   ))
#'   
#'   x_arr <- array(
#'     data = as.numeric(unlist(x_data)),
#'     dim = c(nrow(x_data), lag, 1)
#'   )
#'   
#'   y_data <- t(sapply(
#'     (1 + lag):(length(matrix_x_scale) - lag),
#'     function(x) matrix_x_scale[x:(x + lag - 1)]
#'   ))
#'   
#'   y_arr <- array(
#'     data = as.numeric(unlist(y_data)),
#'     dim = c(nrow(y_data), lag, 1)
#'   )
#'   
#'   model %>% fit(
#'     x = x_arr, y = y_arr,
#'     batch_size = batch_size, epochs = epochs,
#'     verbose = verbose, shuffle = FALSE
#'   )
#'   
#'   x_pred <- array(
#'     data = as.vector(tail(matrix_x_scale, lag)),
#'     dim = c(1, lag, 1)
#'   )
#'   
#'   return(list(
#'     m = model, fmean = factores_escala[1], fsd = factores_escala[2],
#'     lag = lag, vars = x_pred, start = end(x), frequency = frequency(x)
#'   ))
#' }
#' 
#' #' Time series forecasts for a Torch model.
#' #'
#' #' @param object An object from keras.
#' #' @param h Number of periods for forecasting.
#' #'
#' #' @author Diego Jimenez <diego.jimenez@promidat.com>
#' #' @return Point forecasts as a time series.
#' #' @export
#' #' @import keras
#' #' @importFrom stats predict
#' #' @examples
#' #' \donttest{
#' #'   library(keras)
#' #'   modelo.deep <- keras_model_sequential() %>% 
#' #'   layer_lstm(
#' #'     units = 10, activation = 'tanh', batch_input_shape = c(1, 12, 1),
#' #'     return_sequences = TRUE, stateful = TRUE) %>%
#' #'   layer_dense(units = 1) %>%
#' #'   compile(loss = 'mse', optimizer = 'adam', metrics = 'mse')
#' #'   modelo.deep <- tskeras(AirPassengers, modelo.deep, lag = 12, epochs = 1)
#' #'   pred.tskeras(modelo.deep, h = 12)
#' #' }
#' #' 
#' pred.tskeras <- function(object, h = 1) {
#'   pred <- c()
#'   x_vars <- object$vars
#'   
#'   while (length(pred) < h) {
#'     lstm_forecast <- object$m %>% predict(x_vars, batch_size = 1)
#'     if((h - length(pred)) < object$lag) {
#'       pred <- c(pred, lstm_forecast[, , 1][1:(h - length(pred))])
#'     } else {
#'       pred <- c(pred, lstm_forecast[, , 1])
#'     }
#'     x_vars <- lstm_forecast
#'   }
#'   
#'   pred <- c(NA, pred)
#'   pred <- pred * object$fsd + object$fmean
#'   pred <- ts(pred, start = object$start, frequency = object$frequency)
#'   return(tail(pred, -1))
#' }
