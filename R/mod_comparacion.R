#' comparacion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_comparacion_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tabBoxPrmdt(
      id = ns("BoxPreds"), #opciones = opc_disp, title = titulo_disp,
      tabPanel(
        title = labelInput("text_m"), value = "tabEtable",
        withLoader(DT::dataTableOutput(ns('table_error')), 
                   type = "html", loader = "loader4")
      ),
      tabPanel(
        title = labelInput("txterror"), value = "tabEplot",
        echarts4rOutput(ns("plot_error"), height = "70vh")
      ),
      tabPanel(
        title = labelInput("plot_m"), value = "tabEpred",
        echarts4rOutput(ns("pred_error"), height = "70vh")
      )
    )
  )
}
    
#' comparacion Server Function
#'
#' @noRd 
mod_comparacion_server <- function(input, output, session, updateData, rvmodelo) {
  ns <- session$ns
  
  output$table_error <- DT::renderDataTable({
    ms  <- rvmodelo
    lg  <- updateData$idioma
    res <- data.frame()
    
    for (x in names(ms)) {
      if(!is.null(ms[[x]])) {
        res <- rbind(res, ms[[x]]$error)
      }
    }
    res <- round(res, 3)
    noms <- row.names(res)
    row.names(res) <- tr(row.names(res), lg)
    noms2 <- row.names(res)
    
    cod <- paste0("tabla.errores(list(",
                  paste(paste0("pred.", noms), collapse = ","),
                  "), test, c('", paste(noms2, collapse = "','"), "'))")
    isolate(updateData$code[['comp']][['doccompt']] <- cod)
    
    DT::datatable(res, options = list(dom = 'frtip', scrollY = "50vh"))
  }, server = T)
  
  output$plot_error <- renderEcharts4r({
    ms <- rvmodelo
    lg <- updateData$idioma
    
    tryCatch({
      res <- data.frame()
      for (x in names(ms)) {
        if(!is.null(ms[[x]])) {
          res <- rbind(res, ms[[x]]$error)
        }
      }
      res <- round(res, 3)
      noms <- row.names(res)
      row.names(res) <- tr(row.names(res), lg)
      noms2 <- row.names(res)
      
      cod <- paste0("res <- tabla.errores(list(",
                    paste(paste0("pred.", noms), collapse = ","),
                    "), test, c('", paste(noms2, collapse = "','"), "'))\n",
                    "grafico.errores(res)")
      isolate(updateData$code[['comp']][['doccompe']] <- cod)
      
      grafico.errores(res)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  output$pred_error <- renderEcharts4r({
    ms <- rvmodelo
    lg <- updateData$idioma
    fecha <- isolate(updateData$seriedf[[1]])
    train <- isolate(updateData$train)
    test  <- isolate(updateData$test)
    
    tryCatch({
      series.tiempo <- list(
        list(type = "line", data = train, name = tr("train", lg)),
        list(type = "line", data = c(rep(NA, length(train)), test), 
             name = tr("test", lg))
      )
      nombres <- c()
      for (i in 1:length(names(ms))) {
        if(!is.null(ms[[names(ms)[i]]])) {
          pred <- c(rep(NA, length(train)), ms[[names(ms)[i]]]$pred)
          series.tiempo[[(i+2)]] <- list(
            type = "line", data = pred, name = tr(names(ms)[i], lg)
          )
          nombres <- c(nombres, names(ms)[i])
        }
      }
      
      noms <- paste0("pred.", nombres)
      cod <- paste0(
        "res <- ts.union(train,test, ", paste(noms, collapse = ","), ")\n",
        "res <- data.frame(res)\n",
        "res$d <- seriedf[[1]]\n",
        "res |> e_charts(x = d) |> e_datazoom() |> e_tooltip(trigger = 'axis') |>\n",
        paste(sapply(c("train", "test", noms), function(x) {
          paste0("  e_line(serie = ", x, ", name = '", tr(x), "')")
        }), collapse = " |>\n")
      )
      isolate(updateData$code[['comp']][['doccompp']] <- cod)
      
      opts <- list(
        xAxis = list(
          type = "category", data = format(fecha, "%Y-%m-%d %H:%M:%S")),
        yAxis = list(show = TRUE, scale = T),
        series = series.tiempo
      )
      
      e_charts() |> e_list(opts) |> e_legend() |> e_datazoom() |> 
        e_tooltip(trigger = 'axis') |> e_show_loading()
    }, error = function(e) {
      return(NULL)
    })
  })
}
    
## To be copied in the UI
# mod_comparacion_ui("comparacion_ui_1")
    
## To be copied in the server
# callModule(mod_comparacion_server, "comparacion_ui_1")
 
