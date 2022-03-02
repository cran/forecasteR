#' holtwinters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom forecast forecast
#' @importFrom stats HoltWinters
mod_holtwinters_ui <- function(id){
  ns <- NS(id)
  
  opc_holt <- div(
    conditionalPanel(
      condition = "input.BoxHolt == 'tabText' | input.BoxHolt == 'tabPlot'", ns = ns,
      tabsOptions(list(icon("cog")), 100, 70, tabs.content = list(
        list(
          conditionalPanel(
            condition = "input.BoxHolt == 'tabText'", ns = ns,
            options.run(ns("run_holt")), tags$hr(style = "margin-top: 0px;"),
            tabVerticalBox(
              id = ns("hwOptMod"), width = NULL,
              tabPanel(
                title = labelInput("auto"), value = "tabAuto",
                col_4(numericDisabled(ns('auto_alpha'), 'alpha', 1)),
                col_4(numericDisabled(ns('auto_beta'),  'beta',  0)),
                col_4(numericDisabled(ns('auto_gamma'), 'gamma', 0))
              ),
              tabPanel(
                title = labelInput("mano"), value = "tabMano",
                col_4(numericInput(ns('mano_alpha'), 'alpha', 1, 0, 1, 0.1)),
                col_4(numericInput(ns('mano_beta'),  'beta',  0, 0, 1, 0.1)),
                col_4(numericInput(ns('mano_gamma'), 'gamma', 0, 0, 1, 0.1))
              ),
              tabPanel(
                title = labelInput("brut"), value = "tabBrut",
                col_4(numericDisabled(ns('brut_alpha'), 'alpha', 1)),
                col_4(numericDisabled(ns('brut_beta'),  'beta',  0)),
                col_4(numericDisabled(ns('brut_gamma'), 'gamma', 0)),
                tags$div(
                  style = "margin-left: 15px; margin-right: 15px",
                  numericInput(ns('paso'), labelInput('paso'), 0.1, 0, 1, 0.1)
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.BoxHolt == 'tabPlot'", ns = ns,
            options.run(NULL), tags$hr(style = "margin-top: 0px;"),
            fluidRow(
              col_4(
                colourpicker::colourInput(
                  ns("col_train_holt"), labelInput("coltrain"), "#5470c6", 
                  allowTransparent = T)
              ),
              col_4(
                colourpicker::colourInput(
                  ns("col_test_holt"), labelInput("coltest"), "#91cc75", 
                  allowTransparent = T)
              ),
              col_4(
                colourpicker::colourInput(
                  ns("col_p_holt"), labelInput("colpred"), "#fac858", 
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
      id = ns("BoxHolt"), opciones = opc_holt, #title = titulo_disp,
      tabPanel(
        title = labelInput("text_m"), value = "tabText",
        div(style = "height: 70vh; overflow: scroll;", 
            withLoader(verbatimTextOutput(ns("text_holt")),
                       type = "html", loader = "loader4"))
      ),
      tabPanel(
        title = labelInput("table_m"), value = "tabTable",
        withLoader(DT::dataTableOutput(ns('table_holt'), height = "70vh"),
                   type = "html", loader = "loader4")
      ),
      tabPanel(
        title = labelInput("plot_m"), value = "tabPlot",
        echarts4rOutput(ns("plot_holt"), height = "70vh")
      ),
      tabPanel(
        title = labelInput("error_m"), value = "tabError",
        uiOutput(ns("error_holt"))
      )
    )
  )
}
    
#' holtwinters Server Function
#'
#' @noRd 
mod_holtwinters_server <- function(input, output, session, updateData, rvmodelo) {
  ns <- session$ns
  
  observeEvent(input$BoxHolt, {
    if(input$BoxHolt == "tabText") {
      shinyjs::show('run_holt')
    } else {
      shinyjs::hide('run_holt')
    }
  })
  
  observeEvent(c(updateData$train, updateData$test), {
    updateTabsetPanel(session, "BoxHolt", selected = "tabText")
  })
  
  output$text_holt <- renderPrint({
    input$run_holt
    tabvalue <- isolate(input$hwOptMod)
    train <- updateData$train
    test  <- updateData$test
    
    tryCatch({
      if(tabvalue == "tabAuto") {
        modelo <- HoltWinters(train)
        updateNumericInput(session, "auto_alpha", value = round(modelo$alpha[[1]], 7))
        updateNumericInput(session, "auto_beta",  value = round(modelo$beta[[1]],  7))
        updateNumericInput(session, "auto_gamma", value = round(modelo$gamma[[1]], 7))
      } else if (tabvalue == "tabMano") {
        alpha  <- isolate(input$mano_alpha)
        beta   <- isolate(input$mano_beta)
        gamma  <- isolate(input$mano_gamma)
        modelo <- HoltWinters(train, alpha, beta, gamma)
      } else {
        modelo <- calibrar.HW(train, test, isolate(input$paso))
        updateNumericInput(session, "brut_alpha", value = round(modelo$alpha[[1]], 7))
        updateNumericInput(session, "brut_beta",  value = round(modelo$beta[[1]],  7))
        updateNumericInput(session, "brut_gamma", value = round(modelo$gamma[[1]], 7))
      }
      
      pred <- forecast(modelo, h = length(test))$mean
      isolate(rvmodelo$holt$model <- modelo)
      isolate(rvmodelo$holt$pred  <- pred)
      isolate(rvmodelo$holt$error <- tabla.errores(list(pred), test, c("holt")))
      
      cod <- paste0(
        "modelo.holt <- HoltWinters(train, ", modelo$alpha[[1]], ", ", 
        modelo$beta[[1]], ", ", modelo$gamma[[1]], ")\n",
        "pred.holt   <- forecast(modelo.holt, h = length(test))$mean\n",
        "error.holt  <- tabla.errores(list(pred.holt), test, 'HoltWinters')")
      isolate(updateData$code[['holt']] <- list(docholtm = cod))
      
      modelo
    }, error = function(e) {
      showNotification(paste0("ERROR HW100: ", e), type = "error")
      return(NULL)
    })
  })
  
  output$table_holt <- DT::renderDataTable({
    lg <- updateData$idioma
    test <- isolate(updateData$test)
    seriedf <- tail(isolate(updateData$seriedf), length(test))
    seriedf[[1]] <- format(seriedf[[1]], '%Y-%m-%d %H:%M:%S')
    
    tryCatch({
      res <- data.frame(seriedf[[1]], seriedf[[2]], rvmodelo$holt$pred, 
                        abs(seriedf[[2]] - rvmodelo$holt$pred))
      colnames(res) <- tr(c('date', 'Real', 'table_m', 'diff'), lg)
      res[, 2:4] <- round(res[, 2:4], 3)
      
      cod <- paste0(
        "s <- tail(seriedf, length(test))\n",
        "res <- data.frame(s[[1]], s[[2]], pred.holt, abs(s[[2]] - pred.holt))\n",
        "colnames(res) <- c('", paste(colnames(res), collapse = "','"), "')\n",
        "res[, 2:4] <- round(res[, 2:4], 3)\nres")
      isolate(updateData$code[['holt']][['docholtt']] <- cod)
      
      DT::datatable(res, selection = 'none', editable = F, rownames = F,
                    options = list(dom = 'frtp', scrollY = "50vh"))
    }, error = function(e) {
      showNotification(paste0("ERROR HW200: ", e), type = "error")
      return(NULL)
    })
  })
  
  output$plot_holt <- renderEcharts4r({
    train <- isolate(updateData$train)
    test  <- isolate(updateData$test)
    lg    <- updateData$idioma
    pred  <- rvmodelo$holt$pred
    serie <- data.frame(ts.union(train, test, pred))
    serie$date <- isolate(updateData$seriedf)[[1]]
    colnames(serie) <- c("train", "test", "pred", "date")
    colors <- c(input$col_train_holt, input$col_test_holt, input$col_p_holt)
    
    tryCatch({
      noms <- c(tr(c('train', 'test', 'table_m'), lg), 'pred.holt')
      isolate(updateData$code[['holt']][['docholtp']] <- code.plots(noms, colors))
      
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
      showNotification(paste0("ERROR HW300: ", e), type = "error")
      return(NULL)
    })
  })
  
  output$error_holt <- renderUI({
    lg <- updateData$idioma
    
    tryCatch({
      res <- div(
        style = "display: table; width: 100%; height: 70vh; overflow: scroll;",
        infoBox2(tr("mse", lg), rvmodelo$holt$error$MSE, NULL, 
                 tags$img(src = 'img/ECM.svg', style = "max-width: 90%;"), "red", 6, fill = T),
        infoBox2(tr("rmse", lg), rvmodelo$holt$error$RMSE, NULL, 
                 tags$img(src = 'img/RECM.svg', style = "max-width: 90%;"), "yellow", 6, fill = T),
        infoBox2(tr("re", lg), rvmodelo$holt$error$RE, NULL, 
                 tags$img(src = 'img/ER.svg', style = "max-width: 90%;"), "green", 6, fill = T),
        infoBox2(tr("cor", lg), rvmodelo$holt$error$CORR, NULL, 
                 tags$img(src = 'img/correlacion.svg', style = "max-width: 90%;"), "navy", 6, fill = T)
      )
      isolate(updateData$code[['holt']][['docholte']] <- "error.holt")
      
      res
    }, error = function(e) {
      showNotification(paste0("ERROR HW400: ", e), type = "error")
      return(NULL)
    })
  })
}
    
## To be copied in the UI
# mod_holtwinters_ui("holtwinters_ui_1")
    
## To be copied in the server
# callModule(mod_holtwinters_server, "holtwinters_ui_1")
 
