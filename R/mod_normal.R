#' normal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_normal_ui <- function(id) {
  ns <- NS(id)
  
  opc_hist <- tabsOptions(list(icon("cog")), 100, 70, tabs.content = list(
    list(options.run(), tags$hr(style = "margin-top: 0px;"),
         conditionalPanel(
           "input.BoxNormal == 'tabNormalPlot'",
           colourpicker::colourInput(
             ns("col_hist_bar"), labelInput("selcolbar"),
             value = "#5470c6", allowTransparent = T),
           colourpicker::colourInput(
             ns("col_hist_line"), labelInput("selcolline"),
             value = "#91cc75", allowTransparent = T)
         ),
         conditionalPanel(
           "input.BoxNormal == 'tabQPlot'",
           colourpicker::colourInput(
             ns("col_qq_point"), labelInput("selcolpoint"),
             value = "#5470c6", allowTransparent = T),
           colourpicker::colourInput(
             ns("col_qq_line"), labelInput("selcolline"),
             value = "#91cc75", allowTransparent = T)
         ),
         conditionalPanel(
           "input.BoxNormal == 'tabNormalCalc'",
           sliderInput(ns("slide_inter"), labelInput("alfa"), 
                       min = 0, max = 0.2, step = 0.01, value = 0.05)
         )
    )
  ))
  
  tagList(
    tabBoxPrmdt(
      id = "BoxNormal", opciones = opc_hist, title = labelInput("sdiff"),
      tabPanel(
        title = labelInput("plotnormal"), value = "tabNormalPlot",
        echarts4rOutput(ns('plot_normal'), height = "70vh")),
      tabPanel(
        title = "Qplot + Qline", value = "tabQPlot",
        echarts4rOutput(ns('plot_qq'), height = "70vh")),
      tabPanel(
        title = labelInput("norm"), value = "tabNormalCalc",
        withLoader(DT::DTOutput(ns('calc_normal')), 
                   type = "html", loader = "loader4"))
    )
  )
}

#' normal Server Function
#'
#' @noRd 
mod_normal_server <- function(input, output, session, updateData) {
  ns <- session$ns
  
  # Grafico Test de normalidad
  output$plot_normal <- renderEcharts4r({
    input$run_normal
    datos     <- updateData$seriedf[[2]]
    colorBar  <- input$col_hist_bar
    colorLine <- input$col_hist_line
    noms      <- tr(c("histograma", "curvanormal"), updateData$idioma)
    
    tryCatch({
      cod <- paste0("e_histnormal(diff(seriedf[[2]]), '", colorBar, "', '", 
                    colorLine, "', c('", noms[1], "', '", noms[2], "'))")
      isolate(updateData$code[['basico']][['docnormal']] <- cod)
      e_histnormal(diff(datos), colorBar, colorLine, noms)
    }, error = function(e) {
      showNotification(paste0("ERROR 01010: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
  
  # Grafico qqplot + qqline
  output$plot_qq <- renderEcharts4r({
    datos      <- updateData$seriedf[[2]]
    colorPoint <- input$col_qq_point
    colorLine  <- input$col_qq_line
    
    tryCatch({
      cod <- paste0("e_qq(diff(seriedf[[2]]), '", colorPoint, "', '", colorLine, "')")
      isolate(updateData$code[['basico']][['docqq']] <- cod)
      e_qq(diff(datos), colorPoint, colorLine)
    }, error = function(e) {
      showNotification(paste0("ERROR 01020: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
  
  # Resumen Test de normalidad
  output$calc_normal <- DT::renderDT({
    datos <- updateData$seriedf[[2]]
    alfa  <- as.numeric(input$slide_inter)
    lg    <- updateData$idioma
    noms  <- tr(c('asimetria', 'normalidad','sigue', 
                  'pvalue', 'tasim', 'sdiff'), lg)
    
    tryCatch({
      isolate(updateData$code[['basico']][['docnormaldf']] <- "dfnormal(diff(seriedf[[2]]))")
      res <- dfnormal(data.frame(diff(datos)))
      rownames(res) <- noms[6]
      
      res <- res[, c(1, 5)]
      res <- round(res, 3)
      res$asimetria <- res$fisher > 0
      res$asimetria <- ifelse(res$asimetria, '<i class="fa fa-plus" style="color: green;"></i>', 
                              '<i class="fa fa-minus" style="color: red;"></i>')
      res$normal <- res$shapiro > alfa
      res$normal <- ifelse(res$normal, '<i class="fa fa-check" style="color: green;"></i>', 
                           '<i class="fa fa-times" style="color: red;"></i>')
      res$shapiro <- paste0(res$shapiro, " > ", alfa)
      res <- res[, c(1, 3, 2, 4)]
      
      sketch <- htmltools::withTags(table(
        tags$thead(
          tags$tr(
            tags$th(rowspan = 2, 'Variables'), 
            tags$th(colspan = 2, style = "text-align: center;", 
                    labelInput('asimetria', noms[1])),
            tags$th(colspan = 2, style = "text-align: center;", 
                    labelInput('normalidad', noms[2]))
          ),
          tags$tr(
            tags$th(labelInput('tasim', noms[5])), tags$th(labelInput('asimetria', noms[1])),
            tags$th(labelInput('pvalue', noms[4])), tags$th(labelInput('sigue', noms[3]))
          )
        )
      ))
      DT::datatable(
        res, selection = 'none', container = sketch, escape = F,
        options = list(dom = 'frtip', scrollY = "50vh")
      )
    }, error = function(e) {
      showNotification(paste0("ERROR 01030: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
}

## To be copied in the UI
# mod_normal_ui("normal_ui_1")

## To be copied in the server
# callModule(mod_normal_server, "normal_ui_1")