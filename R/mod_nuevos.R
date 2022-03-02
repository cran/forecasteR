#' nuevos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom stats ts.union
mod_nuevos_ui <- function(id){
  ns <- NS(id)
  btn_s <- "width: 100%;float: right;background-color: #3c8dbc;color: white;"
  btn_s_n <- "width: 100%;float: right;background-color: #3c8dbc;color: white;display: none;"
  
  tagList(
    div(
      id = ns("newdf"),
      div(col_11(
        box(
          title = labelInput("data"), status = "primary", width = 12,
          solidHeader = TRUE, collapsible = TRUE,
          flowLayout(
            checkboxInput(ns('n_header'), labelInput("header"), value = T),
            radioButtons(
              ns('n_sep'), labelInput("separador"), inline = T,
              choiceNames = c(';', ',', 'TAB'), choiceValues = c(';', ',', '\t')
            ),
            radioButtons(ns('n_dec'), labelInput("separadordec"), c(',', '.'),
                         inline = T),
            fileInput(
              ns('n_file'), labelInput("cargarchivo"), width = "100%",
              placeholder = "", buttonLabel = labelInput("subir"),
              accept = c('text/csv', '.csv', '.txt'))
          ), hr(),
          actionButton(ns("n_loadButton"), labelInput("cargar"), width = "100%"),
          footer = div(
            style = "height: 50vh;",
            withLoader(DT::dataTableOutput(ns('n_tabladatos')), 
                       type = "html", loader = "loader4"))
        )),
        col_1(actionButton(ns("btn_next3"), NULL, icon("angle-double-right"), style = btn_s_n))
      )
    ),
    div(
      id = ns("newtsdf"), style = "display: none;",
      div(
        col_1(actionButton(ns("btn_prev3"), NULL, icon("angle-double-left"), style = btn_s)),
        col_10(box(
          title = labelInput("cargar"), status = "primary", width = 12,
          solidHeader = TRUE, collapsible = TRUE,
          fluidRow(
            col_4(
              h3(labelInput('data')),
              selectInput(ns("sel_n_valor"), labelInput('selvalor'), "")
            ),
            col_8(
              h3(labelInput('date')),
              radioButtons(
                ns('n_colFecha'), NULL, inline = T, 
                choiceNames = list(labelInput('sel'), labelInput('cre')), 
                choiceValues = c('colum', 'nuevo')
              ),
              conditionalPanel(
                condition = "input.n_colFecha == 'colum'", ns = ns,
                selectInput(ns("sel_n_fecha"), labelInput('selfecha'), "")
              ),
              conditionalPanel(
                condition = "input.n_colFecha == 'nuevo'", ns = ns,
                selectInput(ns("n_tipofecha"), labelInput('seltipo'), NULL),
                uiOutput(ns("n_uifechas"))
              )
            ), 
            col_12(hr(), actionButton(ns("n_tsdfButton"), labelInput("cargar"), 
                                      width = "100%"))
          ),
          footer = div(
            style = "height: 46vh;",
            withLoader(DT::dataTableOutput(ns('n_seriedatos')), 
                       type = "html", loader = "loader4"))
        )),
        col_1(actionButton(ns("btn_next4"), NULL, icon("angle-double-right"), style = btn_s_n))
      )
    ),
    div(
      id = ns("newts"), style = "display: none;",
      div(
        col_1(actionButton(ns("btn_prev4"), NULL, icon("angle-double-left"), style = btn_s)),
        col_10(box(
          title = labelInput("cargar"), status = "primary", 
          width = 12, solidHeader = T, collapsible = T,
          selectInput(ns("sel_n_patron"), labelInput('selpatron'), ""), hr(),
          actionButton(ns("n_tsButton"), labelInput("cargar"), width = "100%"),
          footer = echarts4rOutput(ns('plot_n_ts'), height = "60vh")
        )),
        col_1(actionButton(ns("btn_next5"), NULL, icon("angle-double-right"), style = btn_s_n))
      )
    ),
    div(
      id = ns("newmodel"), style = "display: none;",
      div(
        col_1(actionButton(ns("btn_prev5"), NULL, icon("angle-double-left"), style = btn_s)),
        col_10(box(
          title = labelInput("cargar"), status = "primary", 
          width = 12, solidHeader = T, collapsible = T,
          fluidRow(
            col_4(selectInput(ns("sel_model"), labelInput('selmodel'), "")),
            col_8(
              numericInput(ns('n_pred'), labelInput('n_pred'), 10, 1, step = 5),
              conditionalPanel(
                condition = "input.sel_model == 'reds'", ns = ns,
                numericInput(ns("n_tam"), labelInput("tamred"), 10, min = 0, step = 5),
              ),
              conditionalPanel(
                condition = "input.sel_model == 'holt'", ns = ns,
                fluidRow(
                  col_4(numericInput(ns('n_alpha'), 'alpha', 1, 0, 1, 0.05)),
                  col_4(numericInput(ns('n_beta'),  'beta',  0, 0, 1, 0.05)),
                  col_4(numericInput(ns('n_gamma'), 'gamma', 0, 0, 1, 0.05))
                )
              ),
              conditionalPanel(
                condition = "input.sel_model == 'arim'", ns = ns,
                fluidRow(
                  col_4(numericInput(ns('n_p'), 'p', 0, 0, step = 0.5)),
                  col_4(numericInput(ns('n_d'), 'd', 0, 0, step = 0.5)),
                  col_4(numericInput(ns('n_q'), 'q', 0, 0, step = 0.5))
                ),
                fluidRow(
                  col_4(numericInput(ns('n_P'), 'P', 0, 0, step = 0.5)),
                  col_4(numericInput(ns('n_D'), 'D', 0, 0, step = 0.5)),
                  col_4(numericInput(ns('n_Q'), 'Q', 0, 0, step = 0.5))
                ),
                numericInput(ns('n_periodo'), labelInput('selperi'), 0, 0, step = 0.5)
              )
            )
          ), hr(),
          actionButton(ns("btn_model"), labelInput("cargar"), width = "100%"),
          footer = div(style = "height: 40vh; overflow: scroll;", 
                       withLoader(verbatimTextOutput(ns("text_model")),
                                  type = "html", loader = "loader4"))
        )),
        col_1(actionButton(ns("btn_next6"), NULL, icon("angle-double-right"), style = btn_s_n))
      )
    ),
    div(
      id = ns("newpred"), style = "display: none;",
      div(
        col_1(actionButton(ns("btn_prev6"), NULL, icon("angle-double-left"), style = btn_s)),
        col_11(box(
          title = labelInput("cargar"), status = "primary", 
          width = 12, solidHeader = T, collapsible = T,
          div(withLoader(DT::dataTableOutput(ns('df_new')), 
                         type = "html", loader = "loader4")),
          footer = echarts4rOutput(ns('plot_pred'), height = "40vh")
        ))
      )
    )
  )
}
    
#' nuevos Server Functions
#'
#' @noRd 
mod_nuevos_server <- function(input, output, session, updateData) {
  ns <- session$ns
  updateNew <- rv(datos = NULL, seriedf = NULL, seriets = NULL, ts_type = NULL,
                  modelo = NULL, pred = NULL, ini = NULL, fin = NULL)
  
  # Idioma
  observeEvent(updateData$idioma, {
    lg <- updateData$idioma
    fechas <- list("years", "months", "days", "hours", "min", "sec")
    names(fechas) <- tr(c("anual", "mes", "dia", "hora", "minuto", "segundo"), lg)
    updateSelectInput(session, "n_tipofecha", choices = fechas)
    
    models <- list("prom", "naiv", "snai", "drif", 
                   "desc", "reds", "holt", "arim")
    names(models) <- tr(c("prom", "naiv", "snai", "drif", 
                          "desc", "reds", "holt", "arim"), lg)
    updateSelectInput(session, "sel_model", choices = models)
  })
  
  # Hide/Show Menu
  observeEvent(input$btn_prev3, {
    show(id = "newdf",    anim = T, animType = "slide")
    hide(id = "newtsdf",  anim = T, animType = "slide")
  })
  observeEvent(input$btn_next3, {
    hide(id = "newdf",    anim = T, animType = "slide")
    show(id = "newtsdf",  anim = T, animType = "slide")
  })
  observeEvent(input$btn_prev4, {
    show(id = "newtsdf",  anim = T, animType = "slide")
    hide(id = "newts",    anim = T, animType = "slide")
  })
  observeEvent(input$btn_next4, {
    hide(id = "newtsdf",  anim = T, animType = "slide")
    show(id = "newts",    anim = T, animType = "slide")
  })
  observeEvent(input$btn_prev5, {
    show(id = "newts",    anim = T, animType = "slide")
    hide(id = "newmodel", anim = T, animType = "slide")
  })
  observeEvent(input$btn_next5, {
    hide(id = "newts",    anim = T, animType = "slide")
    show(id = "newmodel", anim = T, animType = "slide")
  })
  observeEvent(input$btn_prev6, {
    show(id = "newmodel", anim = T, animType = "slide")
    hide(id = "newpred",  anim = T, animType = "slide")
  })
  observeEvent(input$btn_next6, {
    hide(id = "newmodel", anim = T, animType = "slide")
    show(id = "newpred",  anim = T, animType = "slide")
  })
  
  ############################# Carga de datos ################################
  
  # Función del botón n_loadButton
  observeEvent(input$n_loadButton, {
    updateNew$modelo   <- NULL
    updateNew$datos    <- NULL
    updateNew$seriedf  <- NULL
    updateNew$seriets  <- NULL
    updateNew$ts_type  <- NULL
    updateData$codenew <- NULL
    
    ruta       <- isolate(input$n_file)
    sep        <- isolate(input$n_sep)
    dec        <- isolate(input$n_dec)
    encabezado <- isolate(input$n_header)
    tryCatch({
      updateNew$datos <- carga.datos(ruta$datapath, sep, dec, encabezado)
      if(ncol(var.numericas(updateNew$datos)) <= 0) {
        updateNew$datos <- NULL
        showNotification("ERROR 00020: Check Separators", type = "error")
      }
      cod                 <- code.carga(ruta$name, sep, dec, encabezado)
      updateData$codenew  <- list(doccarga = cod)
    }, error = function(e) {
      updateNew$datos <- NULL
      showNotification(paste0("ERROR 00010: ", e), type = "error")
    })
  })
  
  # Actualizar tabla al cargar los datos
  output$n_tabladatos <- DT::renderDataTable({
    datos  <- updateNew$datos
    nombre <- str_remove(isolate(input$n_file$name), '\\..[^\\.]*$')
    tipos  <- tr(c("numerico", "categorico"), isolate(updateData$idioma))
    
    tryCatch({
      nombre.columnas <- c("ID", colnames(datos))
      tipo.columnas <- sapply(colnames(datos), function(i)
        ifelse(class(datos[,i]) %in% c("numeric", "integer"),
               paste0("<span data-id='numerico'>", tipos[1], "</span>"),
               paste0("<span data-id='categorico'>", tipos[2], "</span>")))
      sketch = htmltools::withTags(table(
        tableHeader(nombre.columnas),
        tags$tfoot(
          tags$tr(tags$th(), lapply(tipo.columnas, function(i) 
            tags$th(shiny::HTML(i))))
        )
      ))
      DT::datatable(
        datos, selection = 'none', editable = TRUE,  container = sketch,
        extensions = 'Buttons', options = list(
          dom = 'Bfrtip', scrollY = "30vh", 
          buttons = list(list(extend = 'csv', filename = nombre, 
                              text = '<i class="fa fa-download"></i>'))),
      )
    }, error = function(e) {
      showNotification(paste0("ERROR 00030: ", e), type = "error")
      return(NULL)
    })
  }, server = T)
  
  # Actualiza opciones al cargar tabla de datos
  observeEvent(updateNew$datos, {
    datos     <- updateNew$datos
    numericos <- var.numericas(datos)
    
    if(is.null(datos)) {
      hide(id = "btn_next3", anim = T, animType = "fade")
    } else {
      show(id = "btn_next3", anim = T, animType = "fade")
    }
    
    updateSelectInput(session, "sel_n_valor", choices = colnames(numericos))
    updateSelectInput(session, "sel_n_fecha", choices = colnames(datos))
  }, ignoreNULL = FALSE)
  
  ############################# Carga Serie DF ################################
  
  # Generar input de fechas
  output$n_uifechas <- renderUI({
    if(input$n_tipofecha == "years") {
      f <- 'YYYY-01-01 00:00:00'
    } else if(input$n_tipofecha == "months") {
      f <- 'YYYY-MM-01 00:00:00'
    } else if(input$n_tipofecha == "days") {
      f <- 'YYYY-MM-DD 00:00:00'
    } else {
      f <- 'YYYY-MM-DD HH:mm:SS'
    }
    
    texto <- tr("hasta", updateData$idioma)
    
    fluidRow(
      col_5(datetimeInput(ns("n_startdate"), f)),
      col_2(h4(texto, style = "text-align: center;")),
      col_5(datetimeInput(ns("n_enddate"), f))
    )
  })
  
  # Actualizar fecha final
  observeEvent(input$n_startdate, {
    tryCatch({
      tipofecha <- isolate(input$n_tipofecha)
      n <- nrow(isolate(updateNew$datos)) - 1
      ini <- ymd_hms(input$n_startdate)
      if(tipofecha == "months") {
        fin <- ini + months(n)
      } else {
        fin <- ini + duration(n, units = tipofecha)
      }
      
      runjs(paste0(
        "$('#nuevos_ui_1-n_enddate').find('input').val('", fin, "');"))
      
      updateNew$ini <- ini
      updateNew$fin <- fin
    }, error = function(e) {})
  })
  
  # Actualizar fecha inicial
  observeEvent(input$n_enddate, {
    tryCatch({
      tipofecha <- isolate(input$n_tipofecha)
      n <- nrow(isolate(updateNew$datos)) - 1
      fin <- ymd_hms(input$n_enddate)
      if(tipofecha == "months") {
        ini <- fin - months(n)
      } else {
        ini <- fin - duration(n, units = tipofecha)
      }
      
      runjs(paste0(
        "$('#nuevos_ui_1-n_startdate').find('input').val('", ini, "');"))
      
      updateNew$ini <- ini
      updateNew$fin <- fin
    }, error = function(e) {})
  })
  
  # Función del botón tsdfButton
  observeEvent(input$n_tsdfButton, {
    updateNew$seriedf <- NULL
    updateNew$seriets <- NULL
    updateNew$ts_type <- NULL
    
    tryCatch({
      datos <- isolate(updateNew$datos)
      
      if(input$n_colFecha == "nuevo") {
        ini <- isolate(updateNew$ini)
        fin <- isolate(updateNew$fin)
        fechas <- seq(ini, fin, by = isolate(input$n_tipofecha))
        
        updateNew$seriedf <- data.frame(
          fechas = fechas, valor = datos[[input$sel_n_valor]])
        updateNew$ts_type <- isolate(input$n_tipofecha)
        
        cod <- code.tsdf(input$sel_n_valor, ini, fin, input$n_tipofecha)
        updateData$codenew <- list(doccarga = updateData$codenew$doccarga,
                                   doctsdf = cod)
      } else {
        fechas <- text_toDate(datos[[input$sel_n_fecha]])
        
        updateNew$seriedf <- data.frame(
          fechas = fechas[[1]], valor = datos[[input$sel_n_valor]])
        updateNew$ts_type <- fechas[[2]]
        
        cod <- code.tsdf(input$sel_n_valor, cold = input$sel_n_fecha)
        updateData$codenew <- list(doccarga = updateData$codenew$doccarga,
                                   doctsdf = cod)
      }
    }, error = function(e) {
      updateNew$seriedf <- NULL
      updateNew$ts_type <- NULL
      showNotification(paste0("ERROR 00040: ", e), type = "error")
    })
  })
  
  # Actualizar la tabla al cargar la serie de datos df
  output$n_seriedatos <- DT::renderDataTable({
    datos  <- updateNew$seriedf
    idioma <- isolate(updateData$idioma)
    nombre <- paste0(str_remove(isolate(input$n_file$name), '\\..[^\\.]*$'),
                     "_ts")
    
    tryCatch({
      if(!is.null(datos)) {
        datos$fechas <- as.character(datos$fechas)
        colnames(datos) <- c(tr("fecha", idioma), tr("valor", idioma))
      }
      DT::datatable(
        datos, selection = 'none', rownames = F, extensions = 'Buttons',
        options = list(dom = 'Bfrtip', scrollY = "30vh", buttons = list(list(
          extend = 'csv', filename = nombre, 
          text = '<i class="fa fa-download"></i>')))
      )
    }, error = function(e) {
      showNotification(paste0("ERROR 00050: ", e), type = "error")
      return(NULL)
    })
  }, server = F)
  
  # Actualiza las opciones al cargar tabla de datos
  observeEvent(updateNew$seriedf, {
    if(is.null(updateNew$seriedf)) {
      hide(id = "btn_next4", anim = T, animType = "fade")
    } else {
      show(id = "btn_next4", anim = T, animType = "fade")
    }
  }, ignoreNULL = FALSE)
  
  ############################# Carga Serie ts ################################
  
  # Actualizar al obtener la serie o cambiar el idioma
  observeEvent(c(updateNew$ts_type, updateData$idioma), {
    lg <- updateData$idioma
    tipo <- updateNew$ts_type
    
    if(!is.null(tipo)) {
      if(tipo == "years") {
        fechas <- list(1)
        names(fechas) <- c(tr("anual", lg))
      } else if(tipo == "months") {
        fechas <- list(12)
        names(fechas) <- c(tr("anual", lg))
      } else if(tipo == "days") {
        fechas <- list(365, 7)
        names(fechas) <- c(tr("anual", lg), tr("semanal", lg))
      } else if(tipo == "workdays") {
        fechas <- list(260, 5)
        names(fechas) <- c(tr("anual", lg), tr("semanal", lg))
      } else if(tipo == "hours") {
        fechas <- list(24, 8760)
        names(fechas) <- c(tr("dia", lg), tr("anual", lg))
      } else if(tipo == "min") {
        fechas <- list(60, 1440, 525600)
        names(fechas) <- tr(c('hora', 'dia', 'anual'), lg)
      } else if(tipo == "sec") {
        fechas <- list(60, 3600, 86400, 31536000)
        names(fechas) <- tr(c('minuto', 'hora', 'dia', 'anual'), lg)
      }
      
      updateSelectInput("sel_n_patron", session = session, choices = fechas)
    }
  })
  
  observeEvent(input$n_tsButton, {
    updateNew$seriets <- NULL
    
    tryCatch({
      datos <- isolate(updateNew$seriedf)
      tipo  <- isolate(updateNew$ts_type)
      f     <- as.numeric(isolate(input$sel_n_patron))
      s     <- get_start(datos[[1]][1], tipo, f)
      
      serie <- ts(datos[[2]], start = s, frequency = f)
      updateNew$seriets <- serie
      
      cod <- code.ts.new(s, f)
      updateData$codenew <- list(
        doccarga = updateData$codenew$doccarga, 
        doctsdf = updateData$codenew$doctsdf, docts = cod)
    }, error = function(e) {
      updateNew$seriets <- NULL
      showNotification(paste0("ERROR 00060: ", e), type = "error")
    })
  })
  
  # Grafico serie de tiempo
  output$plot_n_ts <- renderEcharts4r({
    seriets <- updateNew$seriets
    serie   <- isolate(updateNew$seriedf)
    if(is.null(serie) | is.null(seriets)) {
      return(NULL)
    }
    
    tryCatch({
      opts <- list(
        xAxis = list(
          type = "category", data = format(serie$fechas, "%Y-%m-%d %H:%M:%S")),
        yAxis = list(show = TRUE, scale = T),
        series = list(list(type = "line", data = serie$valor))
      )
      
      e_charts() |> e_list(opts) |> e_datazoom() |> 
        e_tooltip(trigger = 'axis') |> e_show_loading()
    }, error = function(e) {
      showNotification(paste0("ERROR 00070: ", e), type = "error")
      return(NULL)
    })
  })
  
  # Actualiza las opciones la serie de tiempo
  observeEvent(updateNew$seriets, {
    if(is.null(updateNew$seriets)) {
      hide(id = "btn_next5", anim = T, animType = "fade")
    } else {
      show(id = "btn_next5", anim = T, animType = "fade")
    }
  }, ignoreNULL = FALSE)
  
  ############################# Generar Modelo ################################
  
  output$text_model <- renderPrint({
    btn <- input$btn_model
    isolate(updateNew$modelo <- NULL)
    isolate(updateNew$pred   <- NULL)
    if(btn == 0) {
      return(NULL)
    }
    serie     <- isolate(updateNew$seriets)
    n_pred    <- isolate(input$n_pred)
    sel_model <- isolate(input$sel_model)
    
    if(sel_model == 'prom') {
      modelo <- meanf(serie, h = n_pred)
      cod <- paste0("pred <- meanf(seriets, h = ", n_pred, ")")
    } else if(sel_model == 'naiv') {
      modelo <- naive(serie, h = n_pred)
      cod <- paste0("pred <- naive(seriets, h = ", n_pred, ")")
    } else if(sel_model == 'snai') {
      modelo <- snaive(serie, h = n_pred)
      cod <- paste0("pred <- snaive(seriets, h = ", n_pred, ")")
    } else if(sel_model == 'drif') {
      modelo <- rwf(serie, drift = T, h = n_pred)
      cod <- paste0("pred <- rwf(seriets, drift = T, h = ", n_pred, ")")
    } else if(sel_model == 'desc') {
      modelo <- stl(serie, s.window = "periodic")
      cod <- paste0("model <- stl(seriets, s.window = 'periodic')\n",
                    "pred  <- forecast(model, h = ", n_pred, ")")
    } else if(sel_model == 'reds') {
      tam    <- isolate(input$n_tam)
      modelo <- nnetar(serie, size = tam)
      cod <- paste0("model <- nnetar(seriets, size = ", tam, ")\n",
                    "pred  <- forecast(model, h = ", n_pred, ", PI = T)")
    } else if(sel_model == 'holt') {
      alpha  <- isolate(input$n_alpha)
      beta   <- isolate(input$n_beta)
      gamma  <- isolate(input$n_gamma)
      modelo <- HoltWinters(serie, alpha = alpha, beta = beta, gamma = gamma)
      cod <- paste0("model <- HoltWinters(seriets, alpha = ", alpha, 
                    ", beta = ", beta, ", gamma = ", gamma, ")\n",
                    "pred <- forecast(model, h = ", n_pred, ")")
    } else if(sel_model == 'arim') {
      p <- isolate(input$n_p)
      d <- isolate(input$n_d)
      q <- isolate(input$n_q)
      P <- isolate(input$n_P)
      D <- isolate(input$n_D)
      Q <- isolate(input$n_Q)
      periodo <- isolate(input$n_periodo)
      modelo <- arima(serie, order = c(p, d, q), 
                      seasonal = list(order = c(P, D, Q), period = periodo))
      cod <- paste0("model <- arima(seriets, order = c(", p, ", ", d, ", ", q, 
                    "),\n                seasonal = list(order = c(", P, ", ",
                    D, ", ", Q, "), period = ", periodo, "))\n",
                    "pred <- forecast(model, h = ", n_pred, ")")
    }
    
    isolate(updateNew$modelo <- modelo)
    if(sel_model %in% c('arim', 'desc')) {
      isolate(updateNew$pred <- forecast(modelo, h = n_pred))
    } else {
      isolate(updateNew$pred <- forecast(modelo, h = n_pred, PI = T))
    }
    
    isolate(updateData$codenew <- list(
      doccarga = updateData$codenew$doccarga, 
      doctsdf  = updateData$codenew$doctsdf, 
      docts    = updateData$codenew$docts,
      docmodel = cod))
    
    modelo
  })
  
  # Actualiza las opciones al cargar tabla de datos
  observeEvent(updateNew$pred, {
    if(is.null(updateNew$pred)) {
      hide(id = "btn_next6", anim = T, animType = "fade")
    } else {
      show(id = "btn_next6", anim = T, animType = "fade")
    }
  }, ignoreNULL = FALSE)
  
  ########################### Generar Predicción ##############################
  
  output$df_new <- DT::renderDataTable({
    datos  <- data.frame(updateNew$pred)
    nombre <- str_remove(isolate(input$n_file$name), '\\..[^\\.]*$')
    
    tryCatch({
      DT::datatable(
        datos, selection = 'none', rownames = F, extensions = 'Buttons',
        options = list(dom = 'Bfrtip', scrollY = "20vh", buttons = list(list(
          extend = 'csv', filename = paste0(nombre, "_pred"), 
          text = '<i class="fa fa-download"></i>')))
      )
    }, error = function(e) {
      showNotification(paste0("ERROR 00050: ", e), type = "error")
      return(NULL)
    })
  })
  
  output$plot_pred <- renderEcharts4r({
    seriedf <- isolate(updateNew$seriedf)
    serie   <- isolate(updateNew$seriets)
    pred    <- updateNew$pred
    datos   <- ts.union(pred$mean, pred$lower[, 2], pred$upper[, 2])
    datos   <- ts.union(serie, datos)
    datos   <- data.frame(datos)
    names(datos) <- c("s", "p", "liminf", "limsup")
    ts_type <- isolate(updateNew$ts_type)
    datos$fecha <- seq(from = seriedf[[1]][1], by = ts_type, length.out = nrow(datos))
    datos$fecha <- format(datos$fecha, "%Y-%m-%d %H:%M:%S")
    noms <- tr(c("serie", "table_m"), updateData$idioma)
    
    tryCatch({
      cod <- code.plotnew(ts_type, noms)
      isolate(updateData$codenew <- list(
        doccarga = updateData$codenew$doccarga,
        doctsdf  = updateData$codenew$doctsdf,
        docts    = updateData$codenew$docts,
        docmodel = updateData$codenew$docmodel,
        docpred  = cod))
      
      aux <- datos |> e_charts(fecha) |> e_line(s, name = noms[1]) |> 
        e_line(p, name = noms[2]) |> e_datazoom() |> e_legend() |> 
        e_band2(liminf, limsup, color = "lightblue", itemStyle = list(borderWidth = 0)) |> 
        e_y_axis(scale = TRUE) |> e_tooltip(trigger = 'axis') |> e_show_loading()
      
      aux$x$opts$legend$data[[3]] <- NULL
      aux$x$opts$series[[3]]$tooltip <- list(show = F)
      return(aux)
    }, error = function(e) {
      showNotification(paste0("ERROR 00050: ", e), type = "error")
      return(NULL)
    })
  })
}
    
## To be copied in the UI
# mod_nuevos_ui("nuevos_ui_1")
    
## To be copied in the server
# mod_nuevos_server("nuevos_ui_1")
