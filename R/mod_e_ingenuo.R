#' e_ingenuo UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom forecast forecast snaive
mod_e_ingenuo_ui <- function(id){
  ns <- NS(id)
  
  opc_snaive <- div(
    conditionalPanel(
      condition = "input.BoxSnaive == 'tabText' | input.BoxSnaive == 'tabPlot'", ns = ns,
      tabsOptions(list(icon("cog")), 100, 70, tabs.content = list(
        list(
          conditionalPanel(
            condition = "input.BoxSnaive == 'tabText'", ns = ns,
            options.run(ns("run_snaive")), tags$hr(style = "margin-top: 0px;")
          ),
          conditionalPanel(
            condition = "input.BoxSnaive == 'tabPlot'", ns = ns,
            options.run(NULL), tags$hr(style = "margin-top: 0px;"),
            fluidRow(
              col_4(
                colourpicker::colourInput(
                  ns("col_train_snaive"), labelInput("coltrain"), "#5470c6",
                  allowTransparent = T)
              ),
              col_4(
                colourpicker::colourInput(
                  ns("col_test_snaive"), labelInput("coltest"), "#91cc75", 
                  allowTransparent = T)
              ),
              col_4(
                colourpicker::colourInput(
                  ns("col_p_snaive"), labelInput("colpred"), "#fac858", 
                  allowTransparent = T)
              )
            )
          )
        )
      ))
    )
  )
  
  tagList(
    tabBoxPrmdt(
      id = ns("BoxSnaive"), opciones = opc_snaive, #title = titulo_disp,
      tabPanel(
        title = labelInput("text_m"), value = "tabText",
        div(style = "height: 70vh; overflow: scroll;", 
            withLoader(verbatimTextOutput(ns("text_snaive")),
                       type = "html", loader = "loader4"))
      ),
      tabPanel(
        title = labelInput("table_m"), value = "tabTable",
        withLoader(DT::dataTableOutput(ns('table_snaive'), height = "70vh"),
                   type = "html", loader = "loader4")
      ),
      tabPanel(
        title = labelInput("plot_m"), value = "tabPlot",
        echarts4rOutput(ns("plot_snaive"), height = "70vh")
      ),
      tabPanel(
        title = labelInput("error_m"), value = "tabError",
        uiOutput(ns("error_snaive"))
      )
    )
  )
}
    
#' e_ingenuo Server Function
#'
#' @noRd 
mod_e_ingenuo_server <- function(input, output, session, updateData, rvmodelo) {
  ns <- session$ns
  
  observeEvent(input$BoxSnaive, {
    if(input$BoxSnaive == "tabText") {
      shinyjs::show('run_snaive')
    } else {
      shinyjs::hide('run_snaive')
    }
  })
  
  observeEvent(c(updateData$train, updateData$test), {
    updateTabsetPanel(session, "BoxSnaive", selected = "tabText")
  })
  
  output$text_snaive <- renderPrint({
    input$run_snaive
    train <- updateData$train
    test  <- updateData$test
    
    tryCatch({
      modelo <- snaive(train, h = length(test))
      pred   <- modelo$mean
      rvmodelo$snai$model <- modelo
      rvmodelo$snai$pred  <- pred
      rvmodelo$snai$error <- tabla.errores(list(pred), test, c("snai"))
      
      cod <- paste0(
        "modelo.snai <- meanf(train, h = length(test))\n", 
        "pred.snai   <- modelo.snai$mean\n",
        "error.snai  <- tabla.errores(list(pred.snai), test, 'Snaive')")
      isolate(updateData$code[['snai']] <- list(docsnaim = cod))
      
      modelo
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
  
  output$table_snaive <- DT::renderDataTable({
    lg <- updateData$idioma
    test <- isolate(updateData$test)
    seriedf <- tail(isolate(updateData$seriedf), length(test))
    seriedf[[1]] <- format(seriedf[[1]], '%Y-%m-%d %H:%M:%S')
    
    tryCatch({
      res <- data.frame(seriedf[[1]], seriedf[[2]], rvmodelo$snai$pred, 
                        abs(seriedf[[2]] - rvmodelo$snai$pred))
      colnames(res) <- tr(c('date', 'Real', 'table_m', 'diff'), lg)
      res[, 2:4] <- round(res[, 2:4], 3)
      
      cod <- paste0(
        "s <- tail(seriedf, length(test))\n",
        "res <- data.frame(s[[1]], s[[2]], pred.snai, abs(s[[2]] - pred.snai))\n",
        "colnames(res) <- c('", paste(colnames(res), collapse = "','"), "')\n",
        "res[, 2:4] <- round(res[, 2:4], 3)\nres")
      isolate(updateData$code[['snai']][['docsnait']] <- cod)
      
      DT::datatable(res, selection = 'none', editable = F, rownames = F, 
                    options = list(dom = 'frtp', scrollY = "50vh"))
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
  
  output$plot_snaive <- renderEcharts4r({
    train <- isolate(updateData$train)
    test  <- isolate(updateData$test)
    lg    <- updateData$idioma
    pred  <- rvmodelo$snai$pred
    serie <- data.frame(ts.union(train, test, pred))
    serie$date <- isolate(updateData$seriedf)[[1]]
    colnames(serie) <- c("train", "test", "pred", "date")
    colors <- c(input$col_train_snaive, input$col_test_snaive, input$col_p_snaive)
    
    tryCatch({
      noms <- c(tr(c('train', 'test', 'table_m'), lg), 'pred.snai')
      isolate(updateData$code[['snai']][['docsnaip']] <- code.plots(noms, colors))
      
      opts <- list(
        xAxis = list(
          type = "category", data = format(serie$date, "%Y-%m-%d %H:%M:%S")),
        yAxis = list(show = TRUE, scale = T),
        series = list(
          list(type = "line", data = serie$train, name = noms[1]),
          list(type = "line", data = serie$test,  name = noms[2]),
          list(type = "line", data = serie$pred,  name = noms[3])
        )
      )
      
      e_charts() |> e_list(opts) |> e_legend() |> e_datazoom() |> 
        e_tooltip(trigger = 'axis') |> e_show_loading() |> e_color(colors)
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
  
  output$error_snaive <- renderUI({
    lg <- updateData$idioma
    
    tryCatch({
      res <- div(
        style = "display: table; width: 100%; height: 70vh; overflow: scroll;",
        infoBox2(tr("mse", lg), rvmodelo$snai$error$MSE, NULL, 
                 tags$img(src = 'img/ECM.svg', style = "max-width: 90%;"), "red", 6, fill = T),
        infoBox2(tr("rmse", lg), rvmodelo$snai$error$RMSE, NULL, 
                 tags$img(src = 'img/RECM.svg', style = "max-width: 90%;"), "yellow", 6, fill = T),
        infoBox2(tr("re", lg), rvmodelo$snai$error$RE, NULL, 
                 tags$img(src = 'img/ER.svg', style = "max-width: 90%;"), "green", 6, fill = T),
        infoBox2(tr("cor", lg), rvmodelo$snai$error$CORR, NULL, 
                 tags$img(src = 'img/correlacion.svg', style = "max-width: 90%;"), "navy", 6, fill = T)
      )
      isolate(updateData$code[['snai']][['docsnaie']] <- "error.snai")
      
      res
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
}
    
## To be copied in the UI
# mod_e_ingenuo_ui("e_ingenuo_ui_1")
    
## To be copied in the server
# callModule(mod_e_ingenuo_server, "e_ingenuo_ui_1")
 
