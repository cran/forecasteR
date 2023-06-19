#' ingenuo UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom forecast forecast naive
mod_ingenuo_ui <- function(id) {
  ns <- NS(id)
  
  opc_naive <- div(
    conditionalPanel(
      condition = "input.BoxNaive == 'tabText' | input.BoxNaive == 'tabPlot'", ns = ns,
      tabsOptions(list(icon("gear")), 100, 70, tabs.content = list(
        list(
          conditionalPanel(
            condition = "input.BoxNaive == 'tabText'", ns = ns,
            options.run(ns("run_naive")), tags$hr(style = "margin-top: 0px;")
          ),
          conditionalPanel(
            condition = "input.BoxNaive == 'tabPlot'", ns = ns,
            options.run(NULL), tags$hr(style = "margin-top: 0px;"),
            fluidRow(
              col_4(
                colourpicker::colourInput(
                  ns("col_train_naive"), labelInput("coltrain"), "#5470c6", 
                  allowTransparent = T)
              ),
              col_4(
                colourpicker::colourInput(
                  ns("col_test_naive"), labelInput("coltest"), "#91cc75", 
                  allowTransparent = T)
              ),
              col_4(
                colourpicker::colourInput(
                  ns("col_p_naive"), labelInput("colpred"), "#fac858", 
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
      id = ns("BoxNaive"), opciones = opc_naive, #title = titulo_disp,
      tabPanel(
        title = labelInput("text_m"), value = "tabText",
        div(style = "height: 70vh; overflow: scroll;", 
            withLoader(verbatimTextOutput(ns("text_naive")),
                       type = "html", loader = "loader4"))
      ),
      tabPanel(
        title = labelInput("table_m"), value = "tabTable",
        withLoader(DT::dataTableOutput(ns('table_naive'), height = "70vh"),
                   type = "html", loader = "loader4")
      ),
      tabPanel(
        title = labelInput("plot_m"), value = "tabPlot",
        echarts4rOutput(ns("plot_naive"), height = "70vh")
      ),
      tabPanel(
        title = labelInput("error_m"), value = "tabError",
        uiOutput(ns("error_naive"))
      )
    )
  )
}
    
#' ingenuo Server Function
#'
#' @noRd 
mod_ingenuo_server <- function(input, output, session, updateData, rvmodelo) {
  ns <- session$ns
  
  observeEvent(input$BoxNaive, {
    if(input$BoxNaive == "tabText") {
      shinyjs::show('run_naive')
    } else {
      shinyjs::hide('run_naive')
    }
  })
  
  observeEvent(c(updateData$train, updateData$test), {
    updateTabsetPanel(session, "BoxNaive", selected = "tabText")
  })
  
  output$text_naive <- renderPrint({
    input$run_naive
    train <- updateData$train
    test  <- updateData$test
    
    tryCatch({
      modelo <- naive(train, h = length(test))
      pred   <- modelo$mean
      rvmodelo$naiv$model <- modelo
      rvmodelo$naiv$pred  <- pred
      rvmodelo$naiv$error <- tabla.errores(list(pred), test, c("naiv"))
      
      cod <- paste0(
        "modelo.naiv <- naive(train, h = length(test))\n", 
        "pred.naiv   <- modelo.naiv$mean\n",
        "error.naiv  <- tabla.errores(list(pred.naiv), test, 'Naive')")
      isolate(updateData$code[['naiv']] <- list(docnaivm = cod))
      
      modelo
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
  
  output$table_naive <- DT::renderDataTable({
    lg <- updateData$idioma
    test <- isolate(updateData$test)
    seriedf <- tail(isolate(updateData$seriedf), length(test))
    seriedf[[1]] <- format(seriedf[[1]], '%Y-%m-%d %H:%M:%S')
    
    tryCatch({
      res <- data.frame(seriedf[[1]], seriedf[[2]], rvmodelo$naiv$pred, 
                        abs(seriedf[[2]] - rvmodelo$naiv$pred))
      colnames(res) <- tr(c('date', 'Real', 'table_m', 'diff'), lg)
      res[, 2:4] <- round(res[, 2:4], 3)
      
      cod <- paste0(
        "s <- tail(seriedf, length(test))\n",
        "res <- data.frame(s[[1]], s[[2]], pred.naiv, abs(s[[2]] - pred.naiv))\n",
        "colnames(res) <- c('", paste(colnames(res), collapse = "','"), "')\n",
        "res[, 2:4] <- round(res[, 2:4], 3)\nres")
      isolate(updateData$code[['naiv']][['docnaivt']] <- cod)
      
      DT::datatable(res, selection = 'none', editable = F, rownames = F, 
                    options = list(dom = 'frtp', scrollY = "50vh"))
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
  
  output$plot_naive <- renderEcharts4r({
    train <- isolate(updateData$train)
    test  <- isolate(updateData$test)
    lg    <- updateData$idioma
    pred  <- rvmodelo$naiv$pred
    serie <- data.frame(ts.union(train, test, pred))
    serie$date <- isolate(updateData$seriedf)[[1]]
    colnames(serie) <- c("train", "test", "pred", "date")
    colors <- c(input$col_train_naive, input$col_test_naive, input$col_p_naive)
    
    tryCatch({
      noms <- c(tr(c('train', 'test', 'table_m'), lg), 'pred.naiv')
      isolate(updateData$code[['naiv']][['docnaivp']] <- code.plots(noms, colors))
      
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
  
  output$error_naive <- renderUI({
    lg <- updateData$idioma
    
    tryCatch({
      res <- div(
        style = "display: table; width: 100%; height: 70vh; overflow: scroll;",
        infoBox2(tr("mse", lg), rvmodelo$naiv$error$MSE, NULL, 
                 tags$img(src = 'img/ECM.svg', style = "max-width: 90%;"), "red", 6, fill = T),
        infoBox2(tr("rmse", lg), rvmodelo$naiv$error$RMSE, NULL, 
                 tags$img(src = 'img/RECM.svg', style = "max-width: 90%;"), "yellow", 6, fill = T),
        infoBox2(tr("re", lg), rvmodelo$naiv$error$RE, NULL, 
                 tags$img(src = 'img/ER.svg', style = "max-width: 90%;"), "green", 6, fill = T),
        infoBox2(tr("cor", lg), rvmodelo$naiv$error$CORR, NULL, 
                 tags$img(src = 'img/correlacion.svg', style = "max-width: 90%;"), "navy", 6, fill = T)
      )
      isolate(updateData$code[['naiv']][['docnaive']] <- "error.naiv")
      
      res
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
}
    
## To be copied in the UI
# mod_ingenuo_ui("ingenuo_ui_1")
    
## To be copied in the server
# callModule(mod_ingenuo_server, "ingenuo_ui_1")
 
