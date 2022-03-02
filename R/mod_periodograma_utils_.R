#' Periodogram Data.frame
#'
#' @param x a ts object.
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return data.frame
#' @export df_periods
#' @importFrom stats spec.pgram frequency
#' @examples
#' df_periods(AirPassengers)
#' 
df_periods <- function(x) {
  res <- spec.pgram(x, log = "no", plot = F)
  res <- data.frame(spec = res$spec, freq = res$freq)
  res$per <- frequency(x) / res$freq
  res$pos.max <- order(res$spec, res$freq, decreasing = TRUE)
  res
}

#' Periodogram Plot
#'
#' @param x a ts object.
#' @param p which important period to plot.
#' @param noms vector of lenght 3 to indicate the text to use.
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_periods
#' @import echarts4r
#' @examples
#' e_periods(AirPassengers)
#' 
e_periods <- function(x, p = NULL, noms = NULL) {
  df    <- df_periods(x)
  mejor <- df[df$pos.max[df$pos.max != 1][p], ]
  
  if(is.null(noms)) {
    noms <- c(" most important period is ", "Frecuency", "Period")
  }
  
  opts <- list(
    tooltip = list(trigger = 'none', axisPointer = list(type = 'cross')),
    xAxis = list(
      list(type = "category", data = round(df$freq, 3),
           axisPointer = list(label = list(formatter = htmlwidgets::JS(paste0(
             "function(params){return('", noms[2], ": ' + params.value)}"))))
      ),
      list(type = "category", data = round(df$per, 3),
           axisPointer = list(label = list(formatter = htmlwidgets::JS(paste0(
             "function(params){return('", noms[3], ": ' + params.value)}"))))
      )
    ),
    yAxis = list(type = "value"),
    series = list(list(type = "line", data = df$spec))
  )
  
  res <- e_charts() |> e_list(opts) |> e_datazoom(type = "slider") |> 
    e_x_axis(scale = T) |> e_y_axis(scale = T) |> e_show_loading()
  
  if(!is.null(p)) {
    if(p != 0) {
      res$x$opts$series[[3]] <- list(
        data = list(list(value = c(which(df$freq == mejor$freq) - 1, mejor$spec))),
        type = "scatter", symbolSize = 15,
        labelLayout = list(x = '70%', moveOverlap = 'shiftY'),
        labelLine = list(
          show = T, length2 = 5, lineStyle = list(width = 3, color = '#bbb')
        ),
        label = list(
          formatter = htmlwidgets::JS(paste0(
            "function(params){\n", 
            "  return '", p, noms[1], round(mejor$per, 2),
            "'\n}"
          )), show = T, minMargin = 10, position = 'top'
        )
      )
    }
  }
  
  return(res)
}
