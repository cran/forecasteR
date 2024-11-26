#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import rlang
#' @import shinyAce
#' @import htmlwidgets
#' @import shinycustomloader
#' @import shinydashboardPlus
#' @importFrom stats stl
#' @importFrom colourpicker colourInput
#' @importFrom DT tableHeader formatStyle
#' @importFrom utils read.table write.csv
#' @importFrom forecast meanf naive snaive nnetar auto.arima
#' @importFrom shinyjs useShinyjs show hide addClass removeClass runjs
#' @importFrom shinydashboard dashboardBody menuItem menuSubItem sidebarMenu tabBox tabItem tabItems infoBox
#'
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    dashboardPage(
      title = "PROMiDAT - forecasteR",
      dashboardHeader(
        title = HTML(paste0(
          '<span class = "logo-lg">
            <a href = "https://promidat.com" target = "_blank">
              <img src = "img/logo.png" width = "100%" style = "padding-top:2px; padding-bottom:6px;">
            </a>
          </span>',
          '<img src= "img/logo_small.png" height = 50%, width = "120%">'
        )), controlbarIcon = icon("gears")
      ),
      
      dashboardSidebar(
        sidebarMenu(
          id = "principal",
          tags$div(style = "padding-top:10px;"),
          menuItem(labelInput("data"), tabName = "cargar", 
                   icon = icon("database")),
          menuItem(
            labelInput("basico"), tabName = "parte1",
            icon = icon("table-list"),
            menuSubItem(labelInput("norm"), "norm", icon = icon("chart-simple")),
            menuSubItem(labelInput("t_c"),  "t_c",  icon = icon("arrow-up-right-dots")),
            menuSubItem(labelInput("desc"), "descom", icon = icon("water")),
            menuSubItem(labelInput("peri"), "peri", icon = icon("road"))
          ),
          menuItem(
            labelInput("apre"), tabName = "parte2",
            icon = icon("table-list"),
            menuSubItem(labelInput("prom"), "prom", icon = icon("scale-balanced")),
            menuSubItem(labelInput("inge"), "inge", icon = icon("arrow-right-long")),
            menuSubItem(labelInput("eing"), "eing", icon = icon("arrow-trend-up")),
            menuSubItem(labelInput("drif"), "drif", icon = icon("arrow-turn-up")),
            menuSubItem(labelInput("desc"), "desc", icon = icon("water")),
            #menuSubItem(labelInput("deep"), "deep", icon = icon("code-branch")),
            menuSubItem("Holt-Winters", "holt", icon = icon("chart-line")),
            menuSubItem("ARIMA", "arim", icon = icon("chart-bar")),
            menuSubItem(labelInput("reds"), "reds", icon = icon("brain"))
          ),
          menuItem(labelInput("comp"), tabName = "comp", icon = icon("eye")),
          menuItem(labelInput("news"), tabName = "news", icon = icon("wand-magic-sparkles")),
          menuItem(labelInput("acercade"), tabName = "acercaDe",
                   icon = icon("info")),
          hr(),
          menu.idioma(),
          hr(),
          img(src = "img/forecasteR.png",
              style = paste0("margin-left: auto;",
                             "margin-right: auto;display: block;width: 80%;")),
          tags$div(style = "display:none;",
                   sliderInput(inputId = "aux", min = 2, value = 2,
                               label = "Cantidad de Clusters", max = 10),
                   colourpicker::colourInput(
                     "auxColor", NULL, value = "red", allowTransparent = T),
                   codigo.monokai("auxcode", height = "10vh")
          )
        )
      ),
      
      dashboardBody(
        tabItems(
          
          # Carga de Datos
          tabItem(tabName = "cargar",  mod_carga_datos_ui("carga_datos_ui_1")),
          
          # Normalidad
          tabItem(tabName = "norm",  mod_normal_ui("normal_ui_1")),
          
          # Tendecia y Ciclicidad
          tabItem(tabName = "t_c",  mod_t_c_ui("t_c_ui_1")),
          
          # Descomposición
          tabItem(tabName = "descom",  mod_descom_ui("descom_ui_1")),
          
          # Periodograma
          tabItem(tabName = "peri",  mod_periodograma_ui("periodograma_ui_1")),
          
          # Modelado
          tabItem(tabName = "prom", mod_train_test_ui("tt_prom_ui", "prom")),
          tabItem(tabName = "inge", mod_train_test_ui("tt_inge_ui", "inge")),
          tabItem(tabName = "eing", mod_train_test_ui("tt_eing_ui", "eing")),
          tabItem(tabName = "drif", mod_train_test_ui("tt_drif_ui", "drif")),
          tabItem(tabName = "desc", mod_train_test_ui("tt_desc_ui", "desc")),
          #tabItem(tabName = "deep", mod_train_test_ui("tt_deep_ui", "deep")),
          tabItem(tabName = "holt", mod_train_test_ui("tt_holt_ui", "holt")),
          tabItem(tabName = "arim", mod_train_test_ui("tt_arim_ui", "arim")),
          tabItem(tabName = "reds", mod_train_test_ui("tt_reds_ui", "reds")),
          
          # Comparación de Datos
          tabItem(tabName = "comp",  mod_comparacion_ui("comparacion_ui_1")),
          
          # Predicción de Individuos Nuevos
          tabItem(tabName = "news",  mod_nuevos_ui("nuevos_ui_1")),
          
          # Acerca De
          tabItem(tabName = "acercaDe", mod_acercade_ui("acercade_ui_1"))
        )
      ),
      
      dashboardControlbar(
        width = 500,
        div(
          style = "margin-right: 15px; margin-left: 15px;", 
          h3(labelInput('code')), hr(), 
          codigo.monokai("codeTotal", height = "70vh"),
          downloadButton("btn_code", NULL, style = "width: 100%;")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  
  add_resource_path('www', app_sys('app/www'))
  add_resource_path('img', app_sys('app/img'))
  add_resource_path('lang', app_sys('app/lang'))
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'forecasteR'
    ),
    
    shinyjs::useShinyjs()
  )
}


