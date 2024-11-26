#' Train-Test UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_train_test_ui <- function(id, nombrePred) {
  ns <- NS(id)
  
  tagList(
    tabBoxPrmdt(
      id = ns("BoxModel"), opciones = NULL, title = NULL,
      
      tabPanel(
        title = labelInput("generatem"), value = "tabModelo",
        tags$div(
          style = "height: 70vh;",
          col_5(
            wellPanel(
              style = "background-color: #666;color: white;",
              options.run(ns("runModelo")), tags$hr(style = "margin-top: 0px;"),
              generar.opciones(nombrePred, ns)
            )
          ),
          col_7(
            withLoader(verbatimTextOutput(ns("txtmodel")), 
                       type = "html", loader = "loader4"))
        )
      ),
      
      if(nombrePred == "arim") {
        tabPanel(
          title = labelInput("corre"), value = "tabCorre",
          echarts4rOutput(ns("plot_acf"),  height = "35vh"),
          echarts4rOutput(ns("plot_pacf"), height = "35vh")
        )
      },
      
      tabPanel(
        title = labelInput("indices"), value = "tabIndices",
        tags$div(
          style = "text-align: -webkit-center;",
          withLoader(uiOutput(ns("ERROR")), 
                     type = "html", loader = "loader4")
        ),
        tags$hr(),
        tags$div(
          col_6(
            withLoader(echarts4rOutput(ns("PREC"), width = "100%"), 
                       type = "html", loader = "loader4")
          ),
          col_6(
            withLoader(DT::dataTableOutput(ns("modeloPredTable")), 
                       type = "html", loader = "loader4")
          )
        )
      )
    )
  )
}

#' Train-Test Server Function
#'
#' @noRd 
mod_train_test_server <- function(input, output, session, updateData, rvmodelo, nombre){
  ns <- session$ns
  
  observe({
    modelo <- rvmodelo[[nombre]]
    
    if(!is.null(modelo)) {
      showTab(inputId = "BoxModel", target = "tabIndices")
    } else {
      updateTabsetPanel(session, "BoxModel", "tabModelo")
      hideTab(inputId = "BoxModel", target = "tabIndices")
    }
  })
  
  observeEvent(c(updateData$idioma), {
    lg <- updateData$idioma
    
    noms <- c(tr("auto", lg), tr("mano", lg), tr("brut", lg))
    vals <- c("auto", "mano", "brut")
    updateRadioButtons(session, "modo", choiceNames = noms, choiceValues = vals)
  })
  
  observeEvent(c(updateData$datos), {
    updateTabsetPanel(session, "BoxModel", selected = "tabModelo")
  })
  
  observeEvent(c(updateData$datos), {
    output$txtmodel <- renderPrint({cat("No se ha generado el modelo.")})
  })
  
  # Generar modelo
  observeEvent(input$runModelo, {
    output$txtmodel <- renderPrint({
      train <- isolate(updateData$train)
      test  <- isolate(updateData$test)
      
      tryCatch({
        model <- generar.modelo(nombre, train, test, input)
        pred  <- forecast(model, h = length(test))$mean
        error <- tabla.errores(list(pred), test, c(nombre))
        rvmodelo[[nombre]] <- list(model = model, pred = pred, error = error)
        print(model)
      }, error = function(e) {
        rvmodelo[[nombre]] <- NULL
        print(e)
      }, warning = function(w) {
        rvmodelo[[nombre]] <- NULL
        print(w)
      })
    })
  })
  
  # ERRORES
  output$ERROR <- renderUI({
    lg <- updateData$idioma
    
    tryCatch({
      res <- div(
        style = "display: table; width: 100%;",
        infoBox2(tr("mse", lg), rvmodelo[[nombre]]$error$MSE, NULL, 
                 tags$img(src = 'img/ECM.svg', style = "max-width: 90%;"), "red", 6, fill = T),
        infoBox2(tr("rmse", lg), rvmodelo[[nombre]]$error$RMSE, NULL, 
                 tags$img(src = 'img/RECM.svg', style = "max-width: 90%;"), "yellow", 6, fill = T),
        infoBox2(tr("re", lg), rvmodelo[[nombre]]$error$RE, NULL, 
                 tags$img(src = 'img/ER.svg', style = "max-width: 90%;"), "green", 6, fill = T),
        infoBox2(tr("cor", lg), rvmodelo[[nombre]]$error$CORR, NULL, 
                 tags$img(src = 'img/correlacion.svg', style = "max-width: 90%;"), "navy", 6, fill = T)
      )
      #isolate(updateData$code[['desc']][['docdesce']] <- "error.desc")
      
      res
    }, error = function(e) {
      showNotification(paste0("ERROR outputERROR: ", e), type = "error")
      return(NULL)
    })
  })
  
  # PRECISION
  output$PREC <- renderEcharts4r({
    train <- isolate(updateData$train)
    test  <- isolate(updateData$test)
    lg    <- updateData$idioma
    pred  <- rvmodelo[[nombre]]$pred
    serie <- data.frame(ts.union(train, test, pred))
    serie$date <- isolate(updateData$seriedf)[[1]]
    colnames(serie) <- c("train", "test", "pred", "date")
    colors <- c(input$col_train_desc, input$col_test_desc, input$col_p_desc)
    
    tryCatch({
      noms <- c(tr(c('train', 'test', 'table_m'), lg), 'pred.desc')
      #isolate(updateData$code[['desc']][['docdescp']] <- code.plots(noms, colors))
      
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
  
  # TABLA PREDICCION
  output$modeloPredTable <- DT::renderDataTable({
    lg <- updateData$idioma
    test <- isolate(updateData$test)
    pred <- rvmodelo[[nombre]]$pred
    seriedf <- tail(isolate(updateData$seriedf), length(test))
    seriedf[[1]] <- format(seriedf[[1]], '%Y-%m-%d %H:%M:%S')
    
    tryCatch({
      res <- data.frame(seriedf[[1]], seriedf[[2]], pred, 
                        abs(seriedf[[2]] - pred))
      colnames(res) <- tr(c('date', 'Real', 'table_m', 'diff'), lg)
      res[, 2:4] <- round(res[, 2:4], 3)
      
      cod <- paste0(
        "s <- tail(seriedf, length(test))\n",
        "res <- data.frame(s[[1]], s[[2]], pred.desc, abs(s[[2]] - pred.desc))\n",
        "colnames(res) <- c('", paste(colnames(res), collapse = "','"), "')\n",
        "res[, 2:4] <- round(res[, 2:4], 3)\nres")
      #isolate(updateData$code[['desc']][['docdesct']] <- cod)
      
      DT::datatable(res, selection = 'none', editable = F, rownames = F, 
                    options = list(dom = 'frtp', scrollY = "50vh"))
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
  
  # TABLA ACF
  output$plot_acf <- renderEcharts4r({
    train <- updateData$train
    
    tryCatch({
      res <- e_acf(train)
      #isolate(updateData$code[['arim']][['docarima']] <- "e_acf(train)")
      
      res
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
  
  # TABLA PACF
  output$plot_pacf <- renderEcharts4r({
    train <- updateData$train
    
    tryCatch({
      res <- e_pacf(train)
      #isolate(updateData$code[['arim']][['docarimc']] <- "e_pacf(train)")
      
      res
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
}

## To be copied in the UI
# mod_train_test_ui("train_test_ui_1")

## To be copied in the server
# callModule(mod_train_test_server, "train_test_ui_1")

