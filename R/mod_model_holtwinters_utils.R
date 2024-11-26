#' Best parameters HoltWinters model
#'
#' @param train a ts object (train of a time series).
#' @param test a ts object (test of a time series).
#' @param paso indicates by value to test alpha, beta and gamma.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return HoltWinters model
#' @export calibrar.HW
#' @importFrom forecast forecast
#' @importFrom stats HoltWinters
#' @examples
#' calibrar.HW(window(AirPassengers, end = c(1959, 12)), window(AirPassengers, start = 1960), 0.5)
#' 
calibrar.HW <- function(train, test, paso = 0.1) {
  error.c <- Inf
  alpha.i <- paso  # alpha cant be 0
  mod.c <- NULL
  while(alpha.i <= 1) {
    beta.i <- 0
    while(beta.i <= 1) {
      gamma.i <- 0
      while(gamma.i <= 1) {
        tryCatch({
          mod.i <- HoltWinters(train, alpha = alpha.i, 
                               beta = beta.i, gamma = gamma.i)
          res.i <- forecast(mod.i, h = length(test))
          error.i <- RMSE(res.i$mean, test)
          if(error.i < error.c) {
            error.c <- error.i
            mod.c <- mod.i         
          }
        }, error = function(e) {}, warning = function(w) {})
        gamma.i <- gamma.i + paso
      }
      beta.i <- beta.i + paso
    }
    alpha.i <- alpha.i + paso
  }  
  return(mod.c)
}
