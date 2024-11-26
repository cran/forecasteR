#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' 
#' @noRd
app_server <- function( input, output, session ) {
  ##################################  Options  ################################
  options(shiny.maxRequestSize = 200*1024^2)
  options(
    DT.options = list(
      aLengthMenu = c(10, 20, -1), iDisplayLength = 10,
      scrollX = TRUE, language = list(
        search = shiny::HTML('<i class="fa fa-search"></i>'),
        info = "", emptyTable = "", zeroRecords = "",
        paginate = list(
          "previous" = shiny::HTML('<i class="fa fa-backward"></i>'),
          "next"     = shiny::HTML('<i class="fa fa-forward"></i>'),
          "first"    = shiny::HTML('<i class="fa fa-fast-backward"></i>'), 
          "last"     = shiny::HTML('<i class="fa fa-fast-forward"></i>')))
    )
  )
  
  onStop(function() stopApp())
  
  ##################################  Variables  ##############################
  updateData <- rv(datos = NULL, seriedf = NULL, seriets = NULL, train = NULL,
                   test = NULL, ts_type = NULL, idioma = NULL, code = NULL, 
                   codenew = NULL)
  
  rvmodelo <- rv(prom = NULL, inge = NULL, eing = NULL, drif = NULL, 
                 desc = NULL, holt = NULL, arim = NULL, reds = NULL,
                 deep = NULL)
  
  ###################################  Update  ################################
  # Update on Language
  observeEvent(input$idioma, {
    updateData$idioma <- input$idioma
    etiquetas <- names(translation)
    updateLabelInput(session, etiquetas, tr(etiquetas, input$idioma))
  })
  
  # Update Code
  observeEvent(c(updateData$code, updateData$codenew, input$idioma), {
    todo  <- updateData$code
    nuevo <- updateData$codenew
    lg    <- input$idioma
    
    comp <- todo[["comp"]]
    todo[["comp"]] <- NULL
    
    cod <- paste0(
      "library(keras)\n", "library(forecast)\n", "library(lubridate)\n",
      "library(echarts4r)\n", "library(forecasteR)\n\n"
    )
    for (modulo in todo) {
      for (n in names(modulo)) {
        cod <- paste0(cod, "### ", tr(n, lg), "\n", modulo[[n]], "\n\n")
      }
    }
    for (n in names(comp)) {
      cod <- paste0(cod, "### ", tr(n, lg), "\n", comp[[n]], "\n\n")
    }
    if(!is.null(nuevo)) {
      cod <- paste0(cod, "############  ", tr("news", lg), "  ###########\n\n")
    }
    for (n in names(nuevo)) {
      cod <- paste0(cod, "### ", tr(n, lg), "\n", nuevo[[n]], "\n\n")
    }
    updateAceEditor(session, "codeTotal", value = cod)
  })
  
  # Enable/disable on load data
  observe({
    element <- "#sidebarItemExpanded li"
    menu.values <- c("[class^=treeview]", " a[data-value=comp]")
    
    lapply(menu.values, function(i) {
      if(is.null(updateData$seriets)) {
        addClass(class = "disabled", selector = paste0(element, i))
      } else {
        removeClass(class = "disabled", selector = paste0(element, i))
      }
    })
  })
  
  # Descarga del código
  output$btn_code <- downloadHandler(
    filename = function() {
      "forecasteR.R"
    },
    content = function(file) {
      writeLines(input$codeTotal, file)
    }
  )
  
  ###################################  Modules  ###############################
  callModule(mod_carga_datos_server, "carga_datos_ui_1", updateData, rvmodelo)
  
  callModule(mod_normal_server,       "normal_ui_1",       updateData)
  callModule(mod_t_c_server,          "t_c_ui_1",          updateData)
  callModule(mod_descom_server,       "descom_ui_1",       updateData)
  callModule(mod_periodograma_server, "periodograma_ui_1", updateData)
  
  ########################## Entrenamiento-Prueba #############################
  callModule(mod_train_test_server, "tt_prom_ui", updateData, rvmodelo, "prom")
  callModule(mod_train_test_server, "tt_inge_ui", updateData, rvmodelo, "inge")
  callModule(mod_train_test_server, "tt_eing_ui", updateData, rvmodelo, "eing")
  callModule(mod_train_test_server, "tt_drif_ui", updateData, rvmodelo, "drif")
  callModule(mod_train_test_server, "tt_desc_ui", updateData, rvmodelo, "desc")
  callModule(mod_train_test_server, "tt_reds_ui", updateData, rvmodelo, "reds")
  #callModule(mod_train_test_server, "tt_deep_ui", updateData, rvmodelo, "deep")
  callModule(mod_train_test_server, "tt_holt_ui", updateData, rvmodelo, "holt")
  callModule(mod_train_test_server, "tt_arim_ui", updateData, rvmodelo, "arim")
  
  ########################## comparación #############################
  callModule(mod_comparacion_server, "comparacion_ui_1", updateData, rvmodelo)
  
  ########################## Individuos Nuevos #############################
  callModule(mod_nuevos_server, "nuevos_ui_1", updateData)
}
