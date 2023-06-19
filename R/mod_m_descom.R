#' m_descom UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom stats stl
#' @importFrom forecast forecast
mod_m_descom_ui <- function(id){
  ns <- NS(id)
  
  opc_desc <- div(
    conditionalPanel(
      condition = "input.BoxDesc == 'tabText' | input.BoxDesc == 'tabPlot'", ns = ns,
      tabsOptions(list(icon("gear")), 100, 70, tabs.content = list(
        list(
          conditionalPanel(
            condition = "input.BoxDesc == 'tabText'", ns = ns,
            options.run(ns("run_desc")), tags$hr(style = "margin-top: 0px;"),
          ),
          conditionalPanel(
            condition = "input.BoxDesc == 'tabPlot'", ns = ns,
            options.run(NULL), tags$hr(style = "margin-top: 0px;"),
            fluidRow(
              col_4(
                colourpicker::colourInput(
                  ns("col_train_desc"), labelInput("coltrain"), "#5470c6", 
                  allowTransparent = T)
              ),
              col_4(
                colourpicker::colourInput(
                  ns("col_test_desc"), labelInput("coltest"), "#91cc75", 
                  allowTransparent = T)
              ),
              col_4(
                colourpicker::colourInput(
                  ns("col_p_desc"), labelInput("colpred"), "#fac858", 
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
      id = ns("BoxDesc"), opciones = opc_desc, #title = titulo_disp,
      tabPanel(
        title = labelInput("text_m"), value = "tabText",
        div(style = "height: 70vh; overflow: scroll;", 
            withLoader(verbatimTextOutput(ns("text_desc")),
                       type = "html", loader = "loader4"))
      ),
      tabPanel(
        title = labelInput("table_m"), value = "tabTable",
        withLoader(DT::dataTableOutput(ns('table_desc'), height = "70vh"),
                   type = "html", loader = "loader4")
      ),
      tabPanel(
        title = labelInput("plot_m"), value = "tabPlot",
        echarts4rOutput(ns("plot_desc"), height = "70vh")
      ),
      tabPanel(
        title = labelInput("error_m"), value = "tabError",
        uiOutput(ns("error_desc"))
      )
    )
  )
}
    
#' m_descom Server Function
#'
#' @noRd 
mod_m_descom_server <- function(input, output, session, updateData, rvmodelo) {
  ns <- session$ns
  
  observeEvent(input$BoxDesc, {
    if(input$BoxDesc == "tabText") {
      shinyjs::show('run_desc')
    } else {
      shinyjs::hide('run_desc')
    }
  })
  
  observeEvent(c(updateData$train, updateData$test), {
    updateTabsetPanel(session, "BoxDesc", selected = "tabText")
  })
  
  output$text_desc <- renderPrint({
    input$run_desc
    train <- updateData$train
    test  <- updateData$test
    
    tryCatch({
      modelo <- stl(train, s.window = "periodic")
      pred   <- forecast(modelo, h = length(test))$mean
      rvmodelo$desc$model <- modelo
      rvmodelo$desc$pred  <- pred
      rvmodelo$desc$error <- tabla.errores(list(pred), test, c("desc"))
      
      cod <- paste0(
        "modelo.desc <- stl(train, s.window = 'periodic')\n", 
        "pred.desc   <- forecast(modelo.desc, h = length(test))$mean\n",
        "error.desc  <- tabla.errores(list(pred.desc), test, 'STL')")
      isolate(updateData$code[['desc']] <- list(docdescm = cod))
      
      modelo
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
  
  output$table_desc <- DT::renderDataTable({
    lg <- updateData$idioma
    test <- isolate(updateData$test)
    seriedf <- tail(isolate(updateData$seriedf), length(test))
    seriedf[[1]] <- format(seriedf[[1]], '%Y-%m-%d %H:%M:%S')
    
    tryCatch({
      res <- data.frame(seriedf[[1]], seriedf[[2]], rvmodelo$desc$pred, 
                        abs(seriedf[[2]] - rvmodelo$desc$pred))
      colnames(res) <- tr(c('date', 'Real', 'table_m', 'diff'), lg)
      res[, 2:4] <- round(res[, 2:4], 3)
      
      cod <- paste0(
        "s <- tail(seriedf, length(test))\n",
        "res <- data.frame(s[[1]], s[[2]], pred.desc, abs(s[[2]] - pred.desc))\n",
        "colnames(res) <- c('", paste(colnames(res), collapse = "','"), "')\n",
        "res[, 2:4] <- round(res[, 2:4], 3)\nres")
      isolate(updateData$code[['desc']][['docdesct']] <- cod)
      
      DT::datatable(res, selection = 'none', editable = F, rownames = F, 
                    options = list(dom = 'frtp', scrollY = "50vh"))
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
  
  output$plot_desc <- renderEcharts4r({
    train <- isolate(updateData$train)
    test  <- isolate(updateData$test)
    lg    <- updateData$idioma
    pred  <- rvmodelo$desc$pred
    serie <- data.frame(ts.union(train, test, pred))
    serie$date <- isolate(updateData$seriedf)[[1]]
    colnames(serie) <- c("train", "test", "pred", "date")
    colors <- c(input$col_train_desc, input$col_test_desc, input$col_p_desc)
    
    tryCatch({
      noms <- c(tr(c('train', 'test', 'table_m'), lg), 'pred.desc')
      isolate(updateData$code[['desc']][['docdescp']] <- code.plots(noms, colors))
      
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
  
  output$error_desc <- renderUI({
    lg <- updateData$idioma
    
    tryCatch({
      res <- div(
        style = "display: table; width: 100%; height: 70vh; overflow: scroll;",
        infoBox2(tr("mse", lg), rvmodelo$desc$error$MSE, NULL, 
                 tags$img(src = 'img/ECM.svg', style = "max-width: 90%;"), "red", 6, fill = T),
        infoBox2(tr("rmse", lg), rvmodelo$desc$error$RMSE, NULL, 
                 tags$img(src = 'img/RECM.svg', style = "max-width: 90%;"), "yellow", 6, fill = T),
        infoBox2(tr("re", lg), rvmodelo$desc$error$RE, NULL, 
                 tags$img(src = 'img/ER.svg', style = "max-width: 90%;"), "green", 6, fill = T),
        infoBox2(tr("cor", lg), rvmodelo$desc$error$CORR, NULL, 
                 tags$img(src = 'img/correlacion.svg', style = "max-width: 90%;"), "navy", 6, fill = T)
      )
      isolate(updateData$code[['desc']][['docdesce']] <- "error.desc")
      
      res
  }, error = function(e) {
    showNotification(paste0("ERROR 0000: ", e), type = "error")
    return(NULL)
  })
  })
}
    
## To be copied in the UI
# mod_m_descom_ui("m_descom_ui_1")
    
## To be copied in the server
# callModule(mod_m_descom_server, "m_descom_ui_1")
 
