#' promedio UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom forecast forecast meanf
mod_promedio_ui <- function(id) {
  ns <- NS(id)
  
  opc_prom <- div(
    conditionalPanel(
      condition = "input.BoxProm == 'tabText' | input.BoxProm == 'tabPlot'", ns = ns,
      tabsOptions(list(icon("cog")), 100, 70, tabs.content = list(
        list(
          conditionalPanel(
            condition = "input.BoxProm == 'tabText'", ns = ns,
            options.run(ns("run_prom")), tags$hr(style = "margin-top: 0px;")
          ),
          conditionalPanel(
            condition = "input.BoxProm == 'tabPlot'", ns = ns,
            options.run(NULL), tags$hr(style = "margin-top: 0px;"),
            fluidRow(
              col_4(
                colourpicker::colourInput(
                  ns("col_train_prom"), labelInput("coltrain"), "#5470c6", 
                  allowTransparent = T)
              ), 
              col_4(
                colourpicker::colourInput(
                  ns("col_test_prom"), labelInput("coltest"), "#91cc75", 
                  allowTransparent = T)
              ), 
              col_4(
                colourpicker::colourInput(
                  ns("col_p_prom"), labelInput("colpred"), "#fac858", 
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
      id = ns("BoxProm"), opciones = opc_prom, #title = titulo_disp,
      tabPanel(
        title = labelInput("text_m"), value = "tabText",
        div(style = "height: 70vh; overflow: scroll;", 
            withLoader(verbatimTextOutput(ns("text_prom")),
                       type = "html", loader = "loader4"))
      ),
      tabPanel(
        title = labelInput("table_m"), value = "tabTable",
        withLoader(DT::dataTableOutput(ns('table_prom'), height = "70vh"),
                   type = "html", loader = "loader4")
      ),
      tabPanel(
        title = labelInput("plot_m"), value = "tabPlot",
        echarts4rOutput(ns("plot_prom"), height = "70vh")
      ),
      tabPanel(
        title = labelInput("error_m"), value = "tabError",
        uiOutput(ns("error_prom"))
      )
    )
  )
}
    
#' promedio Server Function
#'
#' @noRd 
mod_promedio_server <- function(input, output, session, updateData, rvmodelo) {
  ns <- session$ns
  
  observeEvent(input$BoxProm, {
    if(input$BoxProm == "tabText") {
      shinyjs::show('run_prom')
    } else {
      shinyjs::hide('run_prom')
    }
  })
  
  observeEvent(c(updateData$train, updateData$test), {
    updateTabsetPanel(session, "BoxProm", selected = "tabText")
  })
  
  output$text_prom <- renderPrint({
    input$run_prom
    train <- updateData$train
    test  <- updateData$test
    
    tryCatch({
      modelo <- meanf(train, h = length(test))
      pred   <- modelo$mean
      isolate(rvmodelo$prom$model <- modelo)
      isolate(rvmodelo$prom$pred  <- pred)
      isolate(rvmodelo$prom$error <- tabla.errores(list(pred), test, "prom"))
      
      cod <- paste0(
        "modelo.prom <- meanf(train, h = length(test))\n", 
        "pred.prom   <- modelo.prom$mean\n",
        "error.prom  <- tabla.errores(list(pred.prom), test, 'Prom')")
      isolate(updateData$code[['prom']] <- list(docpromm = cod))
      
      modelo
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
  
  output$table_prom <- DT::renderDataTable({
    lg <- updateData$idioma
    test <- isolate(updateData$test)
    seriedf <- tail(isolate(updateData$seriedf), length(test))
    seriedf[[1]] <- format(seriedf[[1]], '%Y-%m-%d %H:%M:%S')
    
    tryCatch({
      res <- data.frame(seriedf[[1]], seriedf[[2]], rvmodelo$prom$pred, 
                        abs(seriedf[[2]] - rvmodelo$prom$pred))
      colnames(res) <- tr(c('date', 'Real', 'table_m', 'diff'), lg)
      res[, 2:4] <- round(res[, 2:4], 3)
      
      cod <- paste0(
        "s <- tail(seriedf, length(test))\n",
        "res <- data.frame(s[[1]], s[[2]], pred.prom, abs(s[[2]] - pred.prom))\n",
        "colnames(res) <- c('", paste(colnames(res), collapse = "','"), "')\n",
        "res[, 2:4] <- round(res[, 2:4], 3)\nres")
      isolate(updateData$code[['prom']][['docpromt']] <- cod)
      
      DT::datatable(res, selection = 'none', editable = F, rownames = F,
                    options = list(dom = 'frtp', scrollY = "50vh"))
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
  
  output$plot_prom <- renderEcharts4r({
    train  <- isolate(updateData$train)
    test   <- isolate(updateData$test)
    lg     <- updateData$idioma
    pred   <- rvmodelo$prom$pred
    colors <- c(input$col_train_prom, input$col_test_prom, input$col_p_prom)
    
    tryCatch({
      serie <- data.frame(ts.union(train, test, pred))
      serie$date <- isolate(updateData$seriedf[[1]])
      colnames(serie) <- c("train", "test", "pred", "date")
      
      noms <- c(tr(c('train', 'test', 'table_m'), lg), 'pred.prom')
      isolate(updateData$code[['prom']][['docpromp']] <- code.plots(noms, colors))
      
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
  
  output$error_prom <- renderUI({
    lg <- updateData$idioma
    
    tryCatch({
      res <- div(
        style = "display: table; width: 100%; height: 70vh; overflow: scroll;",
        infoBox2(tr("mse", lg), rvmodelo$prom$error$MSE, NULL, 
                tags$img(src = 'img/ECM.svg', style = "max-width: 90%;"), "red", 6, fill = T),
        infoBox2(tr("rmse", lg), rvmodelo$prom$error$RMSE, NULL, 
                 tags$img(src = 'img/RECM.svg', style = "max-width: 90%;"), "yellow", 6, fill = T),
        infoBox2(tr("re", lg), rvmodelo$prom$error$RE, NULL, 
                tags$img(src = 'img/ER.svg', style = "max-width: 90%;"), "green", 6, fill = T),
        infoBox2(tr("cor", lg), rvmodelo$prom$error$CORR, NULL, 
                tags$img(src = 'img/correlacion.svg', style = "max-width: 90%;"), "navy", 6, fill = T)
      )
      isolate(updateData$code[['prom']][['docprome']] <- "error.prom")
      
      res
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
}
    
## To be copied in the UI
# mod_promedio_ui("promedio_ui_1")
    
## To be copied in the server
# callModule(mod_promedio_server, "promedio_ui_1")
 
