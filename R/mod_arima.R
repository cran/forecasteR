#' arima UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom stats arima
#' @importFrom forecast forecast auto.arima
#' 
mod_arima_ui <- function(id){
  ns <- NS(id)
  
  opc_arima <- div(
    conditionalPanel(
      condition = "input.BoxArima == 'tabText' | input.BoxArima == 'tabPlot' | input.BoxArima == 'tabPeri'", ns = ns,
      tabsOptions(list(icon("gear")), 100, 70, tabs.content = list(
        list(
          conditionalPanel(
            condition = "input.BoxArima == 'tabText'", ns = ns,
            options.run(ns("run_arima")), tags$hr(style = "margin-top: 0px;"),
            tabVerticalBox(
              id = ns("arimaOptMod"), width = NULL, height = "300px",
              tabPanel(
                title = labelInput("auto"), value = "tabAuto",
                col_4(numericDisabled(ns('auto_p'), 'p', 0)),
                col_4(numericDisabled(ns('auto_d'), 'd', 0)),
                col_4(numericDisabled(ns('auto_q'), 'q', 0)),
                col_4(numericDisabled(ns('auto_P'), 'P', 0)),
                col_4(numericDisabled(ns('auto_D'), 'D', 0)),
                col_4(numericDisabled(ns('auto_Q'), 'Q', 0)),
                tags$div(
                  style = "margin-left: 15px; margin-right: 15px",
                  numericDisabled(ns('auto_periodo'), labelInput('selperi'), 0)
                )
              ),
              tabPanel(
                title = labelInput("mano"), value = "tabMano",
                col_4(numericInput(ns('mano_p'), 'p', 0, 0, step = 0.5)),
                col_4(numericInput(ns('mano_d'), 'd', 0, 0, step = 0.5)),
                col_4(numericInput(ns('mano_q'), 'q', 0, 0, step = 0.5)),
                col_4(numericInput(ns('mano_P'), 'P', 0, 0, step = 0.5)),
                col_4(numericInput(ns('mano_D'), 'D', 0, 0, step = 0.5)),
                col_4(numericInput(ns('mano_Q'), 'Q', 0, 0, step = 0.5)),
                tags$div(
                  style = "margin-left: 15px; margin-right: 15px",
                  numericInput(ns('mano_periodo'), labelInput('selperi'), 0, 0, step = 0.5)
                )
              ),
              tabPanel(
                title = labelInput("brut"), value = "tabBrut",
                div(
                  col_6(
                    sliderInput(ns('ar'), labelInput('lar'), 1, 10, 2, 1),
                    sliderInput(ns('es'), labelInput('les'), 1, 10, 1, 1)
                  ),
                  col_6(
                    col_4(numericDisabled(ns('brut_p'), 'p', 0)),
                    col_4(numericDisabled(ns('brut_d'), 'd', 0)),
                    col_4(numericDisabled(ns('brut_q'), 'q', 0)),
                    col_4(numericDisabled(ns('brut_P'), 'P', 0)),
                    col_4(numericDisabled(ns('brut_D'), 'D', 0)),
                    col_4(numericDisabled(ns('brut_Q'), 'Q', 0)),
                    tags$div(
                      style = "margin-left: 15px; margin-right: 15px",
                      numericInput(ns('brut_periodo'), labelInput('selperi'), 0, 0, step = 0.5)
                    )
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.BoxArima == 'tabPeri'", ns = ns,
            options.run(NULL), tags$hr(style = "margin-top: 0px;"),
            sliderInput(ns("sel_best2"), labelInput("selbest"), 1, 20, 1)
          ),
          conditionalPanel(
            condition = "input.BoxArima == 'tabPlot'", ns = ns,
            options.run(NULL), tags$hr(style = "margin-top: 0px;"),
            fluidRow(
              col_4(
                colourpicker::colourInput(
                  ns("col_train_arima"), labelInput("coltrain"), "#5470c6", 
                  allowTransparent = T)
              ),
              col_4(
                colourpicker::colourInput(
                  ns("col_test_arima"), labelInput("coltest"), "#91cc75", 
                  allowTransparent = T)
              ),
              col_4(
                colourpicker::colourInput(
                  ns("col_p_arima"), labelInput("colpred"), "#fac858", 
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
      id = ns("BoxArima"), opciones = opc_arima, #title = titulo_disp,
      tabPanel(
        title = labelInput("text_m"), value = "tabText",
        div(style = "height: 70vh; overflow: scroll;", 
            withLoader(verbatimTextOutput(ns("text_arima")),
                       type = "html", loader = "loader4"))
      ),
      tabPanel(
        title = labelInput("corre"), value = "tabCorre",
        echarts4rOutput(ns("plot_acf"),  height = "35vh"),
        echarts4rOutput(ns("plot_pacf"), height = "35vh")
      ),
      tabPanel(
        title = labelInput("peri"), value = "tabPeri",
        echarts4rOutput(ns("plot_peri"),  height = "70vh")
      ),
      tabPanel(
        title = labelInput("table_m"), value = "tabTable",
        withLoader(DT::dataTableOutput(ns('table_arima'), height = "70vh"),
                   type = "html", loader = "loader4")
      ),
      tabPanel(
        title = labelInput("plot_m"), value = "tabPlot",
        echarts4rOutput(ns("plot_arima"), height = "70vh")
      ),
      tabPanel(
        title = labelInput("error_m"), value = "tabError",
        uiOutput(ns("error_arima"))
      )
    )
  )
}
    
#' arima Server Function
#'
#' @noRd 
mod_arima_server <- function(input, output, session, updateData, rvmodelo) {
  ns <- session$ns
  
  observeEvent(input$BoxArima, {
    if(input$BoxArima == "tabText") {
      shinyjs::show('run_arima')
    } else {
      shinyjs::hide('run_arima')
    }
  })
  
  observeEvent(c(updateData$train, updateData$test), {
    updateTabsetPanel(session, "BoxArima", selected = "tabText")
  })
  
  output$text_arima <- renderPrint({
    input$run_arima
    tabvalue <- isolate(input$arimaOptMod)
    train <- updateData$train
    test  <- updateData$test
    
    tryCatch({
      if(tabvalue == "tabAuto") {
        modelo <- auto.arima(train)
        updateNumericInput(session, "auto_p", value = modelo$arma[1])
        updateNumericInput(session, "auto_d", value = modelo$arma[6])
        updateNumericInput(session, "auto_q", value = modelo$arma[2])
        updateNumericInput(session, "auto_P", value = modelo$arma[3])
        updateNumericInput(session, "auto_D", value = modelo$arma[7])
        updateNumericInput(session, "auto_Q", value = modelo$arma[4])
        updateNumericInput(session, "auto_periodo", value = modelo$arma[5])
      } else if (tabvalue == "tabMano") {
        p <- isolate(input$mano_p)
        d <- isolate(input$mano_d)
        q <- isolate(input$mano_q)
        P <- isolate(input$mano_P)
        D <- isolate(input$mano_D)
        Q <- isolate(input$mano_Q)
        periodo <- isolate(input$mano_periodo)
        modelo  <- arima(train, order = c(p, d, q), 
                         seasonal = list(order = c(P, D, Q), period = periodo))
      } else {
        ar     <- isolate(input$ar)
        es     <- isolate(input$es)
        modelo <- calibrar.arima(train, test, isolate(input$brut_periodo), 0:ar, 0:es)
        updateNumericInput(session, "brut_p", value = modelo$arma[1])
        updateNumericInput(session, "brut_d", value = modelo$arma[6])
        updateNumericInput(session, "brut_q", value = modelo$arma[2])
        updateNumericInput(session, "brut_P", value = modelo$arma[3])
        updateNumericInput(session, "brut_D", value = modelo$arma[7])
        updateNumericInput(session, "brut_Q", value = modelo$arma[4])
      }
      
      pred   <- forecast(modelo, h = length(test))$mean
      isolate(rvmodelo$arim$model <- modelo)
      isolate(rvmodelo$arim$pred  <- pred)
      isolate(rvmodelo$arim$error <- tabla.errores(list(pred), test, c("arim")))
      
      cod <- paste0(
        "modelo.arim <- arima(train, order = c(", modelo$arma[1], ", ", 
        modelo$arma[6], ", ", modelo$arma[2], "), seasonal = list(order = c(",
        modelo$arma[3], ", ", modelo$arma[7], ", ", modelo$arma[4],
        "), period = ", modelo$arma[5], "))\n", 
        "pred.arim   <- forecast(modelo.arim, h = length(test))$mean\n",
        "error.arim  <- tabla.errores(list(pred.arim), test, 'ARIMA')")
      isolate(updateData$code[['arim']] <- list(docarimm = cod))
      
      modelo
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
  
  output$plot_acf <- renderEcharts4r({
    train <- updateData$train
    
    tryCatch({
      res <- e_acf(train)
      isolate(updateData$code[['arim']][['docarima']] <- "e_acf(train)")
      
      res
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
  
  output$plot_pacf <- renderEcharts4r({
    train <- updateData$train
    
    tryCatch({
      res <- e_pacf(train)
      isolate(updateData$code[['arim']][['docarimc']] <- "e_pacf(train)")
      
      res
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
  
  output$plot_peri <- renderEcharts4r({
    train <- updateData$train
    mejor <- input$sel_best2
    lg    <- updateData$idioma
    
    tryCatch({
      txt <- tr(c('txtbest', 'txtfreq', 'txtperi'), lg)
      res <- e_periods(train, mejor, txt)
      
      isolate(updateData$code[['arim']][['docarimo']] <- paste0(
        "e_periods(train, ", mejor, ", c('", paste(txt, collapse = "','"), "'))"
      ))
      res
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
  
  output$table_arima <- DT::renderDataTable({
    lg <- updateData$idioma
    test <- isolate(updateData$test)
    seriedf <- tail(isolate(updateData$seriedf), length(test))
    seriedf[[1]] <- format(seriedf[[1]], '%Y-%m-%d %H:%M:%S')
    
    tryCatch({
      res <- data.frame(seriedf[[1]], seriedf[[2]], rvmodelo$arim$pred, 
                        abs(seriedf[[2]] - rvmodelo$arim$pred))
      colnames(res) <- tr(c('date', 'Real', 'table_m', 'diff'), lg)
      res[, 2:4] <- round(res[, 2:4], 3)
      
      cod <- paste0(
        "s <- tail(seriedf, length(test))\n",
        "res <- data.frame(s[[1]], s[[2]], pred.arim, abs(s[[2]] - pred.arim))\n",
        "colnames(res) <- c('", paste(colnames(res), collapse = "','"), "')\n",
        "res[, 2:4] <- round(res[, 2:4], 3)\nres")
      isolate(updateData$code[['arim']][['docarimt']] <- cod)
      
      DT::datatable(res, selection = 'none', editable = F, rownames = F,
                    options = list(dom = 'frtp', scrollY = "50vh"))
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
  
  output$plot_arima <- renderEcharts4r({
    train <- isolate(updateData$train)
    test  <- isolate(updateData$test)
    lg    <- updateData$idioma
    pred  <- rvmodelo$arim$pred
    serie <- data.frame(ts.union(train, test, pred))
    serie$date <- isolate(updateData$seriedf)[[1]]
    colnames(serie) <- c("train", "test", "pred", "date")
    colors <- c(input$col_train_arima, input$col_test_arima, input$col_p_arima)
    
    tryCatch({
      noms <- c(tr(c('train', 'test', 'table_m'), lg), 'pred.arim')
      isolate(updateData$code[['arim']][['docarimp']] <- code.plots(noms, colors))
      
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
  
  output$error_arima <- renderUI({
    lg <- updateData$idioma
    
    tryCatch({
      res <- div(
        style = "display: table; width: 100%; height: 70vh; overflow: scroll;",
        infoBox2(tr("mse", lg), rvmodelo$arim$error$MSE, NULL, 
                 tags$img(src = 'img/ECM.svg', style = "max-width: 90%;"), "red", 6, fill = T),
        infoBox2(tr("rmse", lg), rvmodelo$arim$error$RMSE, NULL, 
                 tags$img(src = 'img/RECM.svg', style = "max-width: 90%;"), "yellow", 6, fill = T),
        infoBox2(tr("re", lg), rvmodelo$arim$error$RE, NULL, 
                 tags$img(src = 'img/ER.svg', style = "max-width: 90%;"), "green", 6, fill = T),
        infoBox2(tr("cor", lg), rvmodelo$arim$error$CORR, NULL, 
                 tags$img(src = 'img/correlacion.svg', style = "max-width: 90%;"), "navy", 6, fill = T)
      )
      isolate(updateData$code[['arim']][['docarime']] <- "error.arim")
      
      res
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
}
    
## To be copied in the UI
# mod_arima_ui("arima_ui_1")
    
## To be copied in the server
# callModule(mod_arima_server, "arima_ui_1")
 
