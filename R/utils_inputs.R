datetimeInput <- function(id, f = 'YYYY-01-01', weekends = F) {
  tagList(
    div(
      id = id, class = "input-group-date input-group date",
      tags$input(type = "text", class = "form-control"),
      tags$span(class = "input-group-addon", tags$i(class = "fa fa-calendar"))
    ),
    
    if(weekends) {
      tags$script(HTML(paste0("$('#", id, "').datetimepicker({format: '", f,
                              "', daysOfWeekDisabled: [0, 6]});")))
    } else {
      tags$script(HTML(paste0("$('#", id, "').datetimepicker({format: '", f, "'});")))
    }
  )
}

infoBox2 <- function (title, value = NULL, subtitle = NULL, icon = shiny::icon("bar-chart"), 
          color = "aqua", width = 4, href = NULL, fill = FALSE) {
  colorClass <- paste0("bg-", color)
  boxContent <- div(class = "info-box", class = if (fill) 
    colorClass, span(class = "info-box-icon", class = if (!fill) 
      colorClass, icon), div(class = "info-box-content", span(class = "info-box-text", 
                                                              title), if (!is.null(value)) 
                                                                span(class = "info-box-number", value), if (!is.null(subtitle)) 
                                                                  p(subtitle)))
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  div(class = if (!is.null(width)) 
    paste0("col-sm-", width), boxContent)
}

infoBoxPROMiDAT <- function(titulo, valor, icono) {
  tags$div(
    class = "info-box bg-promidat",
    tags$span(class = "info-box-icon", icono),
    tags$div(class="info-box-content", 
             tags$span(class = "info-box-text", titulo),
             tags$span(class = "info-box-number", valor)
    )
  )
}

codigo.monokai <- function(id, height) {
  aceEditor(
    id, mode = "r", theme = "monokai", value = "", 
    readOnly = T, height = height
  )
}

labelInput <- function(inputId, value = ""){
  tags$span(`data-id` = inputId, value)
}

updateLabelInput <- function (session, labelid, value = NULL) {
  message <- dropNulls(list(labelid = labelid))
  if(length(labelid) == 1) {
    labelid <- list(labelid)
  }
  ifelse(
    is.null(value), sentvalue <- labelid,
    ifelse(length(value) == 1, sentvalue <- list(value),
           sentvalue <- value))
  session$sendCustomMessage(
    type = 'updateLabel',
    message = list(ids = labelid, values = sentvalue))
}

###################################### Shiny ##################################

chooserInput <- function(inputId, leftLabel, rightLabel, leftChoices, rightChoices,
                         size = 5, multiple = FALSE, idleft = NULL, idright = NULL) {
  
  leftChoices  <-  lapply(leftChoices, function(x) tags$option(paste0(x, "_1"), label = x))
  rightChoices <- lapply(rightChoices, function(x) tags$option(paste0(x, "_1"), label = x))
  
  if (multiple)
    multiple <- "multiple"
  else
    multiple <- NULL
  
  tagList(
    singleton(tags$head(
      tags$script(src = "forecastR_chooser.js"),
      tags$style(type = "text/css",
                 HTML(".chooser-container { display: inline-block; }")
      )
    )),
    div(id=inputId, class="chooser",
        div(class="chooser-container chooser-left-container",
            style = "width: 45%;",
            h4(leftLabel, style = "color: white;"),
            tags$select(class = "left", size = size, multiple = multiple,
                        leftChoices, style = "width: 100%;")
        ),
        div(class="chooser-container chooser-center-container",
            icon("circle-arrow-right", "right-arrow fa-3x"),
            tags$br(),
            icon("circle-arrow-left", "left-arrow fa-3x")
        ),
        div(class="chooser-container chooser-right-container",
            style = "width: 45%;",
            h4(rightLabel, style = "color: white;"),
            tags$select(id = idright, class = "right", size = size, 
                        multiple = multiple, rightChoices, style = "width: 100%;")
        )
    )
  )
}
