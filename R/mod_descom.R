#' descom UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_descom_ui <- function(id){
  ns <- NS(id)
  
  opts_descom <- tabsOptions(list(icon("gear")), 100, 70, tabs.content = list(
    list(
      options.run(), tags$hr(style = "margin-top: 0px;"),
      colourpicker::colourInput(
        ns("col_hist"), labelInput("colts"), "#5470c6", allowTransparent = T),
      colourpicker::colourInput(
        ns("col_tend"), labelInput("coltend"), "#91cc75", 
        allowTransparent = T),
      colourpicker::colourInput(
        ns("col_seas"), labelInput("colseas"), "#fac858", 
        allowTransparent = T),
      colourpicker::colourInput(
        ns("col_resi"), labelInput("colresi"), "#ef6566", allowTransparent = T)
    )
  ))
  
  tagList(
    tabBoxPrmdt(
      id = ns("tabDescom"), opciones = opts_descom, title = NULL,
      tabPanel(title = labelInput("desc"), 
               echarts4rOutput(ns('plot_descom'), height = "70vh"))
    )
  )
}

#' descom Server Function
#'
#' @noRd 
mod_descom_server <- function(input, output, session, updateData){
  ns <- session$ns
  
  # Gráfico de Descomposición
  output$plot_descom <- renderEcharts4r({
    serie  <- updateData$seriets
    datos  <- updateData$seriedf
    colors <- c(input$col_hist, input$col_tend, input$col_seas, input$col_resi)
    
    lg    <- updateData$idioma
    noms  <- tr(c('serie', 'tend', 'seas', 'resi'), lg)
    
    tryCatch({
      res <- e_decompose(serie, datos[[1]], noms) |> e_color(colors)
      cod <- paste0(
        "e_decompose(seriets, seriedf[[1]], c('", paste(noms, collapse = "','"), "'))",
        " |>\n  e_color(c('", paste(colors, collapse = "','"), "'))")
      isolate(updateData$code[['basico']][['docdesc']] <- cod)
      
      res
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
}

## To be copied in the UI
# mod_descom_ui("descom_ui_1")

## To be copied in the server
# callModule(mod_descom_server, "descom_ui_1")

