#' t_c UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom stats lm
mod_t_c_ui <- function(id) {
  ns <- NS(id)
  
  opts_tc <- tabsOptions(list(icon("gear")), 100, 70, tabs.content = list(
    list(
      options.run(), tags$hr(style = "margin-top: 0px;"),
      colourpicker::colourInput(
        ns("col_ts"), labelInput("colts"), "#5470c6", allowTransparent = T),
      colourpicker::colourInput(
        ns("col_reg"), labelInput("coltend"), "#91cc75", allowTransparent = T),
      colourpicker::colourInput(
        ns("col_fou"), labelInput("colcicl"), "#fac858", allowTransparent = T)
    )
  ))
  
  tagList(
    tabBoxPrmdt(
      id = ns("tabTC"), opciones = opts_tc, title = NULL,
      tabPanel(title = labelInput("t_c"), 
               echarts4rOutput(ns('plot_tc'), height = "70vh"))
    )
  )
}
    
#' t_c Server Function
#'
#' @noRd 
mod_t_c_server <- function(input, output, session, updateData) {
  ns <- session$ns
  
  # Gráfico de Tendencia y Ciclicidad
  output$plot_tc <- renderEcharts4r({
    serie <- updateData$seriets
    datos <- updateData$seriedf
    
    noms  <- c(tr("serie", updateData$idioma), tr("tend", updateData$idioma),
              tr("cicl", updateData$idioma))
    colors <- c(input$col_ts, input$col_reg, input$col_fou)
    
    tryCatch({
      res <- e_tc(serie, datos[[1]], noms) |> e_color(colors)
      cod <- paste0(
        "e_tc(seriets, seriedf[[1]], c('", paste(noms, collapse = "','"), "'))",
        " |>\n  e_color(c('", paste(colors, collapse = "','"), "'))")
      isolate(updateData$code[['basico']][['doctc']] <- cod)
      
      res
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
}
    
## To be copied in the UI
# mod_t_c_ui("t_c_ui_1")
    
## To be copied in the server
# callModule(mod_t_c_server, "t_c_ui_1")
 
