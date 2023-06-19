#' deep UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom forecast forecast nnetar
mod_deep_ui <- function(id){
  ns <- NS(id)
  
  opc_deep <- div(
    conditionalPanel(
      condition = "input.Boxdeep == 'tabText' | input.Boxdeep == 'tabPlot'", ns = ns,
      tabsOptions(list(icon("gear")), 100, 70, tabs.content = list(
        list(
          conditionalPanel(
            condition = "input.Boxdeep == 'tabText'", ns = ns,
            options.run(ns("run_deep")), tags$hr(style = "margin-top: 0px;"),
            tags$div(
              col_4(numericInput(ns("laginput"), labelInput("llag"), 1, width = "100%")),
              col_4(numericInput(ns("batinput"), labelInput("lbat"), 1, width = "100%")),
              col_4(numericInput(ns("epoinput"), labelInput("lepo"), 1, width = "100%")),
              col_4(selectInput(ns("losinput"), labelInput("llos"), c("mse", "mae"))),
              col_4(selectInput(ns("optinput"), labelInput("lopt"), c("adam", "rmsprop", "sgd"))),
              col_4(selectInput(ns("metinput"), labelInput("lmet"), c("mse", "mae", "mape"))),
              col_4(
                chooserInput(
                  ns("capa"), labelInput("lcap"), labelInput("lmod"), 
                  size = 10, idright = ns("rcapa"),
                  c("rnn", "lstm", "dense", "dropout"), c()
                )
              ),
              col_8(uiOutput(ns("capaopts")))
            )
          ),
          conditionalPanel(
            condition = "input.Boxdeep == 'tabPlot'", ns = ns,
            options.run(NULL), tags$hr(style = "margin-top: 0px;"),
            fluidRow(
              col_4(
                colourpicker::colourInput(
                  ns("col_train_deep"), labelInput("coltrain"), "#5470c6", 
                  allowTransparent = T)
              ),
              col_4(
                colourpicker::colourInput(
                  ns("col_test_deep"), labelInput("coltest"), "#91cc75", 
                  allowTransparent = T)
              ),
              col_4(
                colourpicker::colourInput(
                  ns("col_p_deep"), labelInput("colpred"), "#fac858", 
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
      id = ns("Boxdeep"), opciones = opc_deep, #title = titulo_disp,
      tabPanel(
        title = labelInput("text_m"), value = "tabText",
        div(style = "height: 70vh; overflow: scroll;", 
            withLoader(verbatimTextOutput(ns("text_deep")),
                       type = "html", loader = "loader4"))
      ),
      tabPanel(
        title = labelInput("table_m"), value = "tabTable",
        withLoader(DT::dataTableOutput(ns('table_deep'), height = "70vh"),
                   type = "html", loader = "loader4")
      ),
      tabPanel(
        title = labelInput("plot_m"), value = "tabPlot",
        echarts4rOutput(ns("plot_deep"), height = "70vh")
      ),
      tabPanel(
        title = labelInput("error_m"), value = "tabError",
        uiOutput(ns("error_deep"))
      )
    )
  )
}
    
#' deep Server Function
#'
#' @noRd
#' 
#' @importFrom stringr str_detect
mod_deep_server <- function(input, output, session, updateData, rvmodelo) {
  ns    <- session$ns
  capas <- list()
  vars  <- rv(selcapa = NULL)
  
  observeEvent(input$Boxdeep, {
    if(input$Boxdeep == "tabText") {
      shinyjs::show('run_deep')
    } else {
      shinyjs::hide('run_deep')
    }
  })
  
  observeEvent(c(updateData$train, updateData$test), {
    updateTabsetPanel(session, "Boxdeep", selected = "tabText")
  })
  
  observeEvent(input$capa, {
    agregar  <- input$capa$right[!input$capa$right %in% names(capas)]
    eliminar <- names(capas)[!names(capas) %in% input$capa$right]
    
    lags <- input$laginput
    
    if(length(agregar) == 1) {
      if(str_detect(agregar, "lstm")) {
        capas[[agregar]] <<- list(
          layer = "lstm", units = 10, activation = "tanh")
        
      } else if(str_detect(agregar, "rnn")) {
        capas[[agregar]] <<- list(
          layer = "rnn", units = 10, activation = "tanh")
        
      } else if(str_detect(agregar, "dense")) {
        capas[[agregar]] <<- list(
          layer = "dense", units = 10, activation = "linear")
        
      } else if(str_detect(agregar, "dropout")) {
        capas[[agregar]] <<- list(layer = "dropout", rate = 0.5)
      }
      
      vars$selcapa <- agregar
    } else if(length(eliminar) == 1) {
      capas[[eliminar]] <<- NULL
      vars$selcapa <- NULL
    }
  })
  
  observeEvent(input$rcapa, {
    vars$selcapa <- input$rcapa
  })
  
  output$capaopts <- renderUI({
    nombre_capa <- vars$selcapa
    
    if(is.null(nombre_capa)) {
      return(NULL)
    }
    
    opts <- capas[[nombre_capa]]
    
    res <- NULL
    
    if(str_detect(opts$layer, "lstm|rnn|dense")) {
      res <- tags$div(
        numericInput(ns("units"), "Cantidad de unidades", opts$units),
        selectInput(
          ns("activation"), "Activacion", 
          choices = c("linear", "tanh", "relu", "sigmoid", "softmax"),
          opts$activation
        )
      )
    } else if(str_detect(opts$layer, "dropout")) {
      res <- tags$div(
        fluidRow(col_12(
          sliderInput(ns("rate"), "Ratio", 0, 100, opts$rate * 100, 1, post = "%")
        ))
      )
    }
    
    return(res)
  })
  
  observeEvent(input$units, {
    units <- ifelse(is.na(input$units), 1, input$units)
    capas[[vars$selcapa]][["units"]] <<- units
  })
  
  observeEvent(input$activation, {
    capas[[vars$selcapa]][["activation"]] <<- input$activation
  })
  
  observeEvent(input$rate, {
    rate <- input$rate / 100
    capas[[vars$selcapa]][["rate"]] <<- rate
  })
  
  output$text_deep <- renderPrint({
    input$run_deep
    train <- updateData$train
    test  <- updateData$test
    
    laginput <- isolate(input$laginput)
    batinput <- isolate(input$batinput)
    epoinput <- isolate(input$epoinput)
    losinput <- isolate(input$losinput)
    optinput <- isolate(input$optinput)
    metinput <- isolate(input$metinput)
    
    tryCatch({
      modelo <- keras_model_sequential()
      cod    <- "modelo.deep <- keras_model_sequential()"
      
      for (capa in capas) {
        if(capa$layer == "lstm") {
          modelo <- modelo %>% layer_lstm(
            units = capa$units, activation = capa$activation,
            batch_input_shape = c(1, laginput, 1),
            return_sequences = TRUE, stateful = TRUE)
          cod <- paste0(cod, " %>% layer_lstm(
            units = ", capa$units, ", activation = '", capa$activation, "',
            batch_input_shape = c(1, ", laginput, ", 1),
            return_sequences = TRUE, stateful = TRUE)")
        } else if(capa$layer == "rnn") {
          modelo <- modelo %>% layer_simple_rnn(
            units = capa$units, activation = capa$activation,
            batch_input_shape = c(1, laginput, 1),
            return_sequences = TRUE, stateful = TRUE)
          cod <- paste0(cod, " %>% layer_simple_rnn(
            units = ", capa$units, ", activation = '", capa$activation, "',
            batch_input_shape = c(1, ", laginput, ", 1),
            return_sequences = TRUE, stateful = TRUE)")
        } else if(capa$layer == "dense") {
          modelo <- modelo %>% layer_dense(
            units = capa$units, activation = capa$activation,
            batch_input_shape = c(1, laginput, 1))
          cod <- paste0(cod, " %>% layer_dense(
            units = ", capa$units, ", activation = '", capa$activation, "',
            batch_input_shape = c(1, ", laginput, ", 1))")
        } else if(capa$layer == "dropout") {
          modelo <- modelo %>% layer_dropout(rate = capa$rate)
          cod <- paste0(cod, " %>% layer_dropout(rate = ", capa$rate, ")")
        }
      }
      
      modelo <- modelo %>% layer_dense(units = 1) %>%
        compile(loss = losinput, optimizer = optinput, metrics = metinput)
      cod <- paste0(cod, " %>% layer_dense(units = 1) %>%\n",
        "compile(loss = '", losinput, "', optimizer = '", optinput, 
        "', metrics = '", metinput, "')")
      
      modelo <- tskeras(train, modelo, laginput, batinput, epoinput)
      
      pred <- pred.tskeras(modelo, length(test))
      isolate(rvmodelo$deep$model <- modelo$m)
      isolate(rvmodelo$deep$pred  <- pred)
      isolate(rvmodelo$deep$error <- tabla.errores(list(pred), test, "deep"))
      
      cod <- paste0(
        cod, "\n\n",
        "modelo.deep <- tskeras(train, modelo.deep, lag = ", laginput, ")\n",
        "pred.deep   <- pred.tskeras(modelo.deep, h = length(test))\n",
        "error.deep  <- tabla.errores(list(pred.deep), test, 'Deep Learning')")
      isolate(updateData$code[['deep']] <- list(docdeepm = cod))
      
      modelo$m
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
  
  output$table_deep <- DT::renderDataTable({
    lg <- updateData$idioma
    test <- isolate(updateData$test)
    seriedf <- tail(isolate(updateData$seriedf), length(test))
    seriedf[[1]] <- format(seriedf[[1]], '%Y-%m-%d %H:%M:%S')
    
    tryCatch({
      res <- data.frame(seriedf[[1]], seriedf[[2]], rvmodelo$deep$pred, 
                        abs(seriedf[[2]] - rvmodelo$deep$pred))
      colnames(res) <- tr(c('date', 'Real', 'table_m', 'diff'), lg)
      res[, 2:4] <- round(res[, 2:4], 3)
      
      cod <- paste0(
        "s <- tail(seriedf, length(test))\n",
        "res <- data.frame(s[[1]], s[[2]], pred.deep, abs(s[[2]] - pred.deep))\n",
        "colnames(res) <- c('", paste(colnames(res), collapse = "','"), "')\n",
        "res[, 2:4] <- round(res[, 2:4], 3)\nres")
      isolate(updateData$code[['deep']][['docdeept']] <- cod)
      
      DT::datatable(res, selection = 'none', editable = F, rownames = F,
                    options = list(dom = 'frtp', scrollY = "50vh"))
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
  
  output$plot_deep <- renderEcharts4r({
    train <- isolate(updateData$train)
    test  <- isolate(updateData$test)
    lg    <- updateData$idioma
    pred  <- rvmodelo$deep$pred
    serie <- data.frame(ts.union(train, test, pred))
    serie$date <- isolate(updateData$seriedf)[[1]]
    colnames(serie) <- c("train", "test", "pred", "date")
    colors <- c(input$col_train_deep, input$col_test_deep, input$col_p_deep)
    
    tryCatch({
      noms <- c(tr(c('train', 'test', 'table_m'), lg), 'pred.deep')
      isolate(updateData$code[['deep']][['docdeepp']] <- code.plots(noms, colors))
      
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
  
  output$error_deep <- renderUI({
    lg <- updateData$idioma
    
    tryCatch({
      res <- div(
        style = "display: table; width: 100%; height: 70vh; overflow: scroll;",
        infoBox2(tr("mse", lg), rvmodelo$deep$error$MSE, NULL, 
                 tags$img(src = 'img/ECM.svg', style = "max-width: 90%;"), "red", 6, fill = T),
        infoBox2(tr("rmse", lg), rvmodelo$deep$error$RMSE, NULL, 
                 tags$img(src = 'img/RECM.svg', style = "max-width: 90%;"), "yellow", 6, fill = T),
        infoBox2(tr("re", lg), rvmodelo$deep$error$RE, NULL, 
                 tags$img(src = 'img/ER.svg', style = "max-width: 90%;"), "green", 6, fill = T),
        infoBox2(tr("cor", lg), rvmodelo$deep$error$CORR, NULL, 
                 tags$img(src = 'img/correlacion.svg', style = "max-width: 90%;"), "navy", 6, fill = T)
      )
      isolate(updateData$code[['deep']][['docdeepe']] <- "error.deep")
      
      res
    }, error = function(e) {
      showNotification(paste0("ERROR 0000: ", e), type = "error")
      return(NULL)
    })
  })
}
    
## To be copied in the UI
# mod_deep_ui("deep_ui_1")
    
## To be copied in the server
# callModule(mod_deep_server, "deep_ui_1")
 
