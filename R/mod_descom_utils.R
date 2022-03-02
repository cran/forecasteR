#' Decompose plot
#'
#' @param serie a ts object.
#' @param f vector of dates for the time series.
#' @param noms vector of names for y axis.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_decompose
#' @import echarts4r
#' @importFrom stats decompose
#' @examples
#' e_decompose(AirPassengers)
#' 
e_decompose <- function(serie, f = NULL, noms = NULL) {
  if(is.null(f)) {
    f <- 1:length(serie)
  }
  if(is.null(noms)) {
    noms <- c("History", "Trend", "Seasonal", "Residual")
  }
  d <- decompose(serie)
  d <- data.frame(f, v = d$x, t = d$trend, s = d$seasonal, r = d$random)
  
  opts <- list(
    xAxis = list(
      list(data = f, gridIndex = 0, show = F),
      list(data = f, gridIndex = 1, show = F),
      list(data = f, gridIndex = 2, show = F),
      list(data = f, gridIndex = 3)
    ),
    yAxis = list(
      list(name = noms[1], nameTextStyle = list(fontWeight = 'bold')),
      list(gridIndex = 1, name = noms[2], 
           nameTextStyle = list(fontWeight = 'bold')),
      list(gridIndex = 2, name = noms[3], 
           nameTextStyle = list(fontWeight = 'bold')),
      list(gridIndex = 3, name = noms[4], 
           nameTextStyle = list(fontWeight = 'bold'))
    ),
    grid = list(
      list(bottom = '78%'),
      list(top = '30%', bottom = '54%'),
      list(top = '54%', bottom = '32%'),
      list(top = '76%')
    ),
    series = list(
      list(type = "line", data = d$v),
      list(type = "line", data = d$t, xAxisIndex = 1, yAxisIndex = 1),
      list(type = "line", data = d$s, xAxisIndex = 2, yAxisIndex = 2),
      list(type = "line", data = d$r, xAxisIndex = 3, yAxisIndex = 3)
    ),
    tooltip = list(
      trigger = 'axis',
      formatter = htmlwidgets::JS(paste0(
        "function(params) {\n",
        "  return(`${params[0].name}<br>${params[0].marker} ", noms[1], ": ${params[0].value}<br>",
        "${params[1].marker} ", noms[2], ": ${params[1].value}<br>",
        "${params[2].marker} ", noms[3], ": ${params[2].value}<br>",
        "${params[3].marker} ", noms[4], ": ${params[3].value}`)\n", 
        "}"))
    ),
    dataZoom = list(
      xAxisIndex = c(0, 1, 2, 3)
    ),
    axisPointer = list(
      link = list(xAxisIndex = 'all')
    )
  )
  
  res <- e_charts() |> e_list(opts) |> e_show_loading()
  return(res)
}