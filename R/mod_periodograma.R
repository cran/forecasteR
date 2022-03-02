#' periodograma UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_periodograma_ui <- function(id){
  ns <- NS(id)
  
  opts_cor <- tabsOptions(list(icon("cog")), 100, 70, tabs.content = list(
    list(
      options.run(), tags$hr(style = "margin-top: 0px;"),
      sliderInput(ns("sel_best"), labelInput("selbest"), 1, 20, 1),
      colourpicker::colourInput(
        ns("col_ts2"), labelInput("colperi"), "#5470c6", 
        allowTransparent = T),
      colourpicker::colourInput(
        ns("col_best"), labelInput("colbest"), "#91cc75", 
        allowTransparent = T)
    )
  ))
  
  tagList(
    tabBoxPrmdt(
      id = ns("tabPeriodo"), opciones = opts_cor, title = NULL,
      tabPanel(title = labelInput("peri"), 
               echarts4rOutput(ns('plot_periodo'), height = "70vh"))
    )
  )
}
    
#' periodograma Server Function
#'
#' @noRd 
mod_periodograma_server <- function(input, output, session, updateData) {
  ns <- session$ns
  
  # Gráfico de Periodograma
  output$plot_periodo <- renderEcharts4r({
    serie <- updateData$seriets
    lg    <- updateData$idioma
    
    mejor  <- input$sel_best
    colors <- c(input$col_ts2, input$col_best)
    noms <- tr(c('txtbest', 'txtfreq', 'txtperi'), lg)
    
    tryCatch({
      res <- e_periods(serie, mejor, noms) |> e_color(colors)
      cod <- paste0(
        "e_periods(seriets, ", mejor, ", c('", paste(noms, collapse = "','"), "'))",
        " |>\n  e_color(c('", paste(colors, collapse = "','"), "'))")
      isolate(updateData$code[['basico']][['docperi']] <- cod)
      
      res
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
    
  })
}
    
## To be copied in the UI
# mod_periodograma_ui("periodograma_ui_1")
    
## To be copied in the server
# callModule(mod_periodograma_server, "periodograma_ui_1")
 
