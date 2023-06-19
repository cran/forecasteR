#' carga_datos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom stats ts.union
#' @importFrom lubridate duration wday days ymd_hms
mod_carga_datos_ui <- function(id) {
  ns <- NS(id)
  btn_s <- "width: 100%;float: right;background-color: #3c8dbc;color: white;"
  btn_s_n <- "width: 100%;float: right;background-color: #3c8dbc;color: white;display: none;"
  
  tagList(
    div(
      id = ns("cargadf"),
      div(col_11(
        box(
          title = labelInput("data"), status = "primary", width = 12,
          solidHeader = TRUE, collapsible = TRUE,
          flowLayout(
            checkboxInput(ns('header'), labelInput("header"), value = T),
            radioButtons(
              ns('sep'), labelInput("separador"), inline = T,
              choiceNames = c(';', ',', 'TAB'), choiceValues = c(';', ',', '\t')
            ),
            radioButtons(ns('dec'), labelInput("separadordec"), c(',', '.'),
                         inline = T),
            fluidRow(
              col_10(
                fileInput(
                  ns('archivo'), labelInput("cargarchivo"), width = "100%",
                  placeholder = "", buttonLabel = labelInput("subir"),
                  accept = c('text/csv', '.csv', '.txt'))
              ),
              col_2(
                actionButton(ns("prevfile"), NULL, icon = icon("eye"), style = "margin-top: 25px;")
              )
            )
          ), hr(),
          actionButton(ns("loadButton"), labelInput("cargar"), width = "100%"),
          footer = div(
            style = "height: 50vh;",
            withLoader(DT::dataTableOutput(ns('tabladatos')), 
                       type = "html", loader = "loader4"))
        )),
        col_1(actionButton(ns("btn_next"), NULL, icon("forward"), style = btn_s_n))
      )
    ),
    div(
      id = ns("cargatsdf"), style = "display: none;",
      div(
        col_1(actionButton(ns("btn_prev"), NULL, icon("backward"), style = btn_s)),
        col_10(box(
          title = labelInput("cargar"), status = "primary", width = 12,
          solidHeader = TRUE, collapsible = TRUE,
          fluidRow(
            col_4(
              h3(labelInput('date')),
              radioButtons(
                ns('colFecha'), NULL, inline = T, 
                choiceNames = list(labelInput('sel'), labelInput('cre')), 
                choiceValues = c('colum', 'nuevo')
              ),
              conditionalPanel(
                condition = "input.colFecha == 'colum'", ns = ns,
                selectInput(ns("sel_fecha"), labelInput('selfecha'), "")
              ),
              conditionalPanel(
                condition = "input.colFecha == 'nuevo'", ns = ns,
                selectInput(ns("tipofecha"), labelInput('seltipo'), NULL),
                uiOutput(ns("uifechas"))
              )
            ),
            col_4(
              h3(labelInput('data')),
              selectInput(ns("sel_valor"), labelInput('selvalor'), "")
            ),
            col_4(
              h3(labelInput('suav')),
              numericInput(ns("num_suavizado"), labelInput('nsuav'), 5)
            ),
            col_12(hr(), actionButton(ns("tsdfButton"), labelInput("cargar"), 
                                      width = "100%"))
          ),
          footer = div(
            style = "height: 46vh;",
            withLoader(DT::dataTableOutput(ns('seriedatos')), 
                       type = "html", loader = "loader4"))
        )),
        col_1(actionButton(ns("btn_next2"), NULL, icon("forward"), style = btn_s_n))
      )
    ),
    div(
      id = ns("cargats"), style = "display: none;",
      div(
        col_1(actionButton(ns("btn_prev2"), NULL, icon("backward"), style = btn_s)),
        col_11(box(
          title = labelInput("cargar"), status = "primary", 
          width = 12, solidHeader = T, collapsible = T,
          fluidRow(
            col_4(selectInput(ns("sel_patron"), labelInput('selpatron'), "")),
            col_8(sliderInput(ns("n_tt"), label = div(
              div(style = 'float: left; color: #428bca;', labelInput('train')),
              div(style = 'float: right; color: #91cc75;', labelInput('test'))),
              5, 95, 80, 5))), hr(),
            actionButton(ns("tsButton"), labelInput("generar"), width = "100%"),
          footer = echarts4rOutput(ns('plot_ts'), height = "55vh")
        ))
      )
    )
  )
}

#' carga_datos Server Function
#'
#' @noRd 
mod_carga_datos_server <- function(input, output, session, updateData, rvmodelo) {
  ns <- session$ns
  updateDate <- rv(ini = NULL, fin = NULL)
  
  # Idioma
  observeEvent(updateData$idioma, {
    lg <- updateData$idioma
    fechas <- list("years", "months", "days", "workdays", "hours", "min", "sec")
    names(fechas) <- tr(c('anual', 'mes', 'dia', 'dialab', 'hora', 'minuto', 'segundo'), lg)
    updateSelectInput("tipofecha", session = session, choices = fechas)
  })
  
  # Hide/Show Menu
  observeEvent(input$btn_next, {
    hide(id = "cargadf", anim = T, animType = "slide")
    hide(id = "cargats", anim = T, animType = "slide")
    show(id = "cargatsdf", anim = T, animType = "slide")
  })
  observeEvent(input$btn_prev, {
    show(id = "cargadf", anim = T, animType = "slide")
    hide(id = "cargats", anim = T, animType = "slide")
    hide(id = "cargatsdf", anim = T, animType = "slide")
  })
  observeEvent(input$btn_prev2, {
    hide(id = "cargadf", anim = T, animType = "slide")
    hide(id = "cargats", anim = T, animType = "slide")
    show(id = "cargatsdf", anim = T, animType = "slide")
  })
  observeEvent(input$btn_next2, {
    hide(id = "cargadf", anim = T, animType = "slide")
    show(id = "cargats", anim = T, animType = "slide")
    hide(id = "cargatsdf", anim = T, animType = "slide")
  })
  
  ############################# Carga de datos ################################
  
  # Previsualizar archivo
  observeEvent(input$prevfile, {
    ruta <- isolate(input$archivo)
    if(is.null(ruta)) {
      showNotification("ERROR 00005: Debe cargar un archivo.", 
                       type = "error")
    } else {
      con = file(ruta$datapath, "r")
      prev <- ""
      for (i in 1:10) {
        line = readLines(con, n = 1)
        if ( length(line) == 0 ) {
          break
        }
        prev <- paste0(prev, line, "<br>")
      }
      close(con)
      showModal(
        modalDialog(
          HTML(prev), style = "overflow: auto;", easyClose = TRUE,
          title = tr("vfil", updateData$idioma), footer = NULL, size = "xl"
        )
      )
    }
  })
  
  # Función del botón loadButton
  observeEvent(input$loadButton, {
    for (nom in names(rvmodelo)) {
      rvmodelo[[nom]] <- NULL
    }
    
    updateData$datos   <- NULL
    updateData$seriedf <- NULL
    updateData$seriets <- NULL
    updateData$train   <- NULL
    updateData$test    <- NULL
    updateData$ts_type <- NULL
    updateData$code    <- NULL
    
    ruta       <- isolate(input$archivo)
    sep        <- isolate(input$sep)
    dec        <- isolate(input$dec)
    encabezado <- isolate(input$header)
    
    tryCatch({
      updateData$datos <- carga.datos(ruta$datapath, sep, dec, encabezado)
      cod              <- code.carga(ruta$name, sep, dec, encabezado)
      updateData$code  <- list(carga = list(doccarga = cod))
      if(ncol(var.numericas(updateData$datos)) <= 0) {
        updateData$datos <- NULL
        updateData$code  <- NULL
        showNotification("ERROR 00020: Check Separators", type = "error")
      }
    }, error = function(e) {
      updateData$datos <- NULL
      updateData$code  <- NULL
      showNotification(paste0("ERROR 00010: ", e), type = "error")
    })
  })
  
  # Actualizar tabla al cargar los datos
  output$tabladatos <- DT::renderDataTable({
    datos  <- updateData$datos
    nombre <- str_remove(isolate(input$archivo$name), '\\..[^\\.]*$')
    tipos  <- c(
      tr("numerico",   isolate(updateData$idioma)),
      tr("categorico", isolate(updateData$idioma))
    )
    
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
  }, server = F)
  
  # Actualiza opciones al cargar tabla de datos
  observeEvent(updateData$datos, {
    datos     <- updateData$datos
    numericos <- var.numericas(datos)
    
    if(is.null(datos)) {
      hide(id = "btn_next", anim = T, animType = "fade")
    } else {
      show(id = "btn_next", anim = T, animType = "fade")
    }
    
    updateSelectInput(session, "sel_valor", choices = colnames(numericos))
    updateSelectInput(session, "sel_fecha", choices = colnames(datos))
  }, ignoreNULL = FALSE)
  
  ############################# Carga Serie DF ################################
  
  # Generar input de fechas
  output$uifechas <- renderUI({
    w <- F
    if(input$tipofecha == "years") {
      f <- 'YYYY-01-01 00:00:00'
    } else if(input$tipofecha == "months") {
      f <- 'YYYY-MM-01 00:00:00'
    } else if(input$tipofecha == "days") {
      f <- 'YYYY-MM-DD 00:00:00'
    } else if(input$tipofecha == "workdays") {
      f <- 'YYYY-MM-DD 00:00:00'
      w <- T
    } else if(input$tipofecha == "hours") {
      f <- 'YYYY-MM-DD HH:00:00'
    } else if(input$tipofecha == "min") {
      f <- 'YYYY-MM-DD HH:mm:00'
    } else {
      f <- 'YYYY-MM-DD HH:mm:SS'
    }
    
    texto <- tr("hasta", updateData$idioma)
    
    fluidRow(
      col_5(datetimeInput(ns("startdate"), f, w)),
      col_2(h4(texto, style = "text-align: center;")),
      col_5(datetimeInput(ns("enddate"), f, w))
    )
  })
  
  # Actualizar fecha final
  observeEvent(input$startdate, {
    tryCatch({
      tipofecha <- isolate(input$tipofecha)
      n <- nrow(isolate(updateData$datos)) - 1
      ini <- ymd_hms(input$startdate)
      if(tipofecha == "months") {
        fin <- ini + months(n)
      } else if(tipofecha == "workdays") {
        aux <- ini + duration(n + (n/5 * 2) + 2, units = "days")
        aux <- seq(ini, aux, by = "days")
        aux <- aux[wday(aux) %in% c(2, 3, 4, 5, 6)]
        fin <- aux[n+1]
      } else {
        fin <- ini + duration(n, units = tipofecha)
      }
      
      runjs(paste0(
        "$('#carga_datos_ui_1-enddate').find('input').val('", fin, "');"))
      
      updateDate$ini <- ini
      updateDate$fin <- fin
    }, error = function(e) {})
  })
  
  # Actualizar fecha inicial
  observeEvent(input$enddate, {
    tryCatch({
      tipofecha <- isolate(input$tipofecha)
      n <- nrow(isolate(updateData$datos)) - 1
      fin <- ymd_hms(input$enddate)
      if(tipofecha == "months") {
        ini <- fin - months(n)
      } else if(tipofecha == "workdays") {
        fechas <- c()
        fecha  <- as.Date(fin)
        while(length(fechas) <= n) {
          if(wday(fecha) %in% c(2, 3, 4, 5, 6)) {
            fechas <- c(fechas, fecha)
          }
          fecha <- fecha - days(1)
        }
        ini <- as.Date(fechas[length(fechas)], origin = "1970-01-01")
      } else {
        ini <- fin - duration(n, units = tipofecha)
      }
      
      runjs(paste0(
        "$('#carga_datos_ui_1-startdate').find('input').val('", ini, "');"))
      
      updateDate$ini <- ini
      updateDate$fin <- fin
    }, error = function(e) {})
  })
  
  # Función del botón tsdfButton
  observeEvent(input$tsdfButton, {
    for (nom in names(rvmodelo)) {
      rvmodelo[[nom]] <- NULL
    }
    
    updateData$seriedf <- NULL
    updateData$seriets <- NULL
    updateData$train   <- NULL
    updateData$test    <- NULL
    updateData$ts_type <- NULL
    
    tryCatch({
      datos <- isolate(updateData$datos)
      
      if(input$colFecha == "nuevo") {
        ini <- isolate(updateDate$ini)
        fin <- isolate(updateDate$fin)
        
        if(isolate(input$tipofecha) == "workdays") {
          fechas <- seq(as.Date(ini), as.Date(fin), by = "days")
          fechas <- fechas[wday(fechas) %in% c(2, 3, 4, 5, 6)]
          cod <- code.tsdf(input$sel_valor, ini, fin, "days")
        } else {
          fechas <- seq(ini, fin, by = isolate(input$tipofecha))
          cod <- code.tsdf(input$sel_valor, ini, fin, isolate(input$tipofecha))
        }
        
        updateData$seriedf <- data.frame(
          fechas = fechas, valor = datos[[input$sel_valor]])
        updateData$ts_type <- isolate(input$tipofecha)
        
        updateData$code <- list(carga = list(
          doccarga = updateData$code$carga$doccarga, doctsdf = cod))
      } else {
        fechas <- text_toDate(datos[[input$sel_fecha]])
        
        df  <- data.frame(fechas = fechas[[1]], 
                          valor = datos[[input$sel_valor]])
        
        if(fechas[[2]] == "workdays") {
          total.fechas  <- seq(df$fechas[1], df$fechas[length(df$fechas)],
                               by = "days")
          faltan.fechas <- total.fechas[!total.fechas %in% df$fechas]
          faltan.fechas <- faltan.fechas[!wday(faltan.fechas) %in% c(1, 7)]
        } else {
          total.fechas  <- seq(df$fechas[1], df$fechas[length(df$fechas)],
                               by = fechas[[2]])
          faltan.fechas <- total.fechas[!total.fechas %in% df$fechas]
        }
        
        if(length(faltan.fechas) > 0) {
          df <- merge(df, data.frame(fechas = faltan.fechas), all = TRUE)
          df <- df[order(df$fechas), ]
          df.suavizado <- smoothing(df$valor, input$num_suavizado)
          df$valor[which(is.na(df$valor))] <- df.suavizado[which(is.na(df$valor))]
        }
        
        updateData$seriedf <- df
        updateData$ts_type <- fechas[[2]]
        
        cod <- code.tsdf(input$sel_valor, cold = input$sel_fecha)
        updateData$code <- list(carga = list(
          doccarga = updateData$code$carga$doccarga, doctsdf = cod))
      }
    }, error = function(e) {
      updateData$seriedf <- NULL
      updateData$ts_type <- NULL
      showNotification(paste0("ERROR 00040: ", e), type = "error")
    })
  })
  
  # Actualizar la tabla al cargar la serie de datos df
  output$seriedatos <- DT::renderDataTable({
    datos  <- updateData$seriedf
    idioma <- isolate(updateData$idioma)
    nombre <- paste0(str_remove(isolate(input$archivo$name), '\\..[^\\.]*$'),
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
  observeEvent(updateData$seriedf, {
    serie <- updateData$seriedf
    
    if(is.null(serie)) {
      hide(id = "btn_next2", anim = T, animType = "fade")
    } else {
      show(id = "btn_next2", anim = T, animType = "fade")
    }
  }, ignoreNULL = FALSE)
  
  ############################# Carga Serie ts ################################
  
  # Actualizar al obtener la serie o cambiar el idioma
  observeEvent(c(updateData$ts_type, updateData$idioma), {
    lg <- updateData$idioma
    tipo <- updateData$ts_type
    
    if(!is.null(tipo)) {
      if(tipo == "years") {
        fechas <- list(1)
        names(fechas) <- c(tr("anual", lg))
      } else if(tipo == "months") {
        fechas <- list(12)
        names(fechas) <- c(tr("anual", lg))
      } else if(tipo == "days") {
        fechas <- list(365, 30, 7)
        names(fechas) <- c(tr("anual", lg), tr("mes", lg), tr("semanal", lg))
      } else if(tipo == "workdays") {
        fechas <- list(260, 5)
        names(fechas) <- c(tr("anual", lg), tr("semanal", lg))
      } else if(tipo == "hours") {
        fechas <- list(8760, 720, 24)
        names(fechas) <- c(tr("anual", lg), tr("mes", lg), tr("dia", lg))
      } else if(tipo == "min") {
        fechas <- list(525600, 43200, 1440, 60)
        names(fechas) <- tr(c('anual', 'mes', 'dia', 'hora'), lg)
      } else if(tipo == "sec") {
        fechas <- list(31536000, 2592000, 86400, 3600, 60)
        names(fechas) <- tr(c('anual', 'mes', 'dia', 'hora', 'minuto'), lg)
      }
      
      updateSelectInput("sel_patron", session = session, choices = fechas)
    }
  })
  
  observeEvent(input$tsButton, {
    for (nom in names(rvmodelo)) {
      rvmodelo[[nom]] <- NULL
    }
    
    updateData$seriets <- NULL
    updateData$train   <- NULL
    updateData$test    <- NULL
    
    tryCatch({
      datos   <- isolate(updateData$seriedf)
      tipo    <- isolate(updateData$ts_type)
      f       <- as.numeric(isolate(input$sel_patron))
      n_tt    <- isolate(input$n_tt)
      n_train <- round(nrow(datos) * (n_tt/100))
      n_test  <- nrow(datos) - n_train
      s       <- get_start(datos[[1]][1], tipo, f)
      
      serie <- ts(datos[[2]], start = s, frequency = f)
      updateData$seriets <- serie
      updateData$train   <- head(serie, n_train)
      updateData$test    <- tail(serie, n_test)
      
      cod <- code.ts(s, f, n_train, n_test)
      updateData$code <- list(carga = list(
        doccarga = updateData$code$carga$doccarga, 
        doctsdf = updateData$code$carga$doctsdf, docts = cod))
    }, error = function(e) {
      updateData$seriets <- NULL
      updateData$train   <- NULL
      updateData$test    <- NULL
      showNotification(paste0("ERROR 00060: ", e), type = "error")
    })
  })
  
  # Grafico serie de tiempo
  output$plot_ts <- renderEcharts4r({
    train <- updateData$train
    test  <- updateData$test
    names <- tr(c('train', 'test'), updateData$idioma)
    if(is.null(train) | is.null(test)) {
      return(NULL)
    }
    
    serie      <- data.frame(ts.union(train, test))
    serie$date <- isolate(updateData$seriedf)[[1]]
    
    tryCatch({
      opts <- list(
        xAxis = list(
          type = "category", data = format(serie$date, "%Y-%m-%d %H:%M:%S")),
        yAxis = list(show = TRUE, scale = T),
        series = list(
          list(type = "line", data = serie$train, name = names[1]),
          list(type = "line", data = serie$test,  name = names[2])
        )
      )
      
      e_charts() |> e_list(opts) |> e_legend() |> e_datazoom() |>
        e_tooltip(trigger = 'axis') |> e_show_loading()
    }, error = function(e) {
      showNotification(paste0("ERROR 00070: ", e), type = "error")
      return(NULL)
    })
  })
}

## To be copied in the UI
# mod_carga_datos_ui("carga_datos_ui_1")
    
## To be copied in the server
# callModule(mod_carga_datos_server, "carga_datos_ui_1")
 
