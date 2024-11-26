# Opciones REDES
opciones.reds <- function(ns) {
  opciones <- tags$div(
    numericInput(ns("size"), labelInput("tamred"), 10, min = 0, step = 5)
  )
  return(opciones)
}

# Opciones HOLT-WINTERS
opciones.holt <- function(ns) {
  noms <- c(tr("auto", "es"), tr("mano", "es"), tr("brut", "es"))
  vals <- c("auto", "mano", "brut")
  opciones <- tags$div(
    radioButtons(ns("modo"), NULL, choiceNames = noms, choiceValues = vals),
    conditionalPanel(
      condition = "input.modo == 'mano'", ns = ns,
      fluidRow(
        col_4(numericInput(ns('alfa'), 'alpha', 1, 0, 1, 0.1)),
        col_4(numericInput(ns('beta'), 'beta',  0, 0, 1, 0.1)),
        col_4(numericInput(ns('gama'), 'gamma', 0, 0, 1, 0.1))
      )
    ),
    conditionalPanel(
      condition = "input.modo == 'brut'", ns = ns,
      numericInput(ns('paso'), labelInput('paso'), 0.1, 0, 1, 0.1)
    )
  )
  return(opciones)
}

# Opciones ARIMA
opciones.arim <- function(ns, vc = F) {
  noms <- c(tr("auto", "es"), tr("mano", "es"), tr("brut", "es"))
  vals <- c("auto", "mano", "brut")
  opciones <- tags$div(
    radioButtons(ns("modo"), NULL, choiceNames = noms, choiceValues = vals),
    conditionalPanel(
      condition = "input.modo == 'mano'", ns = ns,
      fluidRow(
        col_4(numericInput(ns('p'), 'p', 0, 0, step = 0.5)),
        col_4(numericInput(ns('d'), 'd', 0, 0, step = 0.5)),
        col_4(numericInput(ns('q'), 'q', 0, 0, step = 0.5)),
        col_4(numericInput(ns('P'), 'P', 0, 0, step = 0.5)),
        col_4(numericInput(ns('D'), 'D', 0, 0, step = 0.5)),
        col_4(numericInput(ns('Q'), 'Q', 0, 0, step = 0.5)),
      )
    ),
    conditionalPanel(
      condition = "input.modo == 'brut'", ns = ns,
      sliderInput(ns('ar'), labelInput('lar'), 1, 10, 2, 1),
      sliderInput(ns('es'), labelInput('les'), 1, 10, 1, 1)
    ),
    conditionalPanel(
      condition = "input.modo == 'mano' || input.modo == 'brut'", ns = ns,
      numericInput(ns('periodo'), labelInput('selperi'), 0, 0, step = 0.5)
    )
  )
  return(opciones)
  return(opciones)
}

# Generar opciones
generar.opciones <- function(nombre, ns) {
  opciones <- NULL
  if(nombre == "prom") {
    opciones <- NULL
  } else if (nombre == "svm") {
    opciones <- NULL
  } else if (nombre == "inge") {
    opciones <- NULL
  } else if (nombre == "eing") {
    opciones <- NULL
  } else if (nombre == "drif") {
    opciones <- NULL
  } else if (nombre == "desc") {
    opciones <- NULL
  } else if (nombre == "holt") {
    opciones <- opciones.holt(ns)
  } else if (nombre == "arim") {
    opciones <- opciones.arim(ns)
  } else if (nombre == "reds") {
    opciones <- opciones.reds(ns)
  } else if (nombre == "deep") {
    opciones <- NULL
  }
  
  return(opciones)
}

# Generar modelo
generar.modelo <- function(nombre, train, test, input) {
  modelo <- NULL
  n <- length(test)
  tryCatch({
    if(nombre == "prom") {
      modelo <- meanf(train, h = n)
    } else if (nombre == "inge") {
      modelo <- naive(train, h = n)
    } else if (nombre == "eing") {
      modelo <- snaive(train, h = n)
    } else if (nombre == "drif") {
      modelo <- rwf(train, h = n, drift = T)
    } else if (nombre == "desc") {
      modelo <- stl(train, s.window = "periodic")
    } else if (nombre == "holt") {
      modo <- isolate(input$modo)
      paso <- isolate(input$paso)
      alfa <- isolate(input$alfa)
      beta <- isolate(input$beta)
      gama <- isolate(input$gama)
      if(modo == "auto") {
        modelo <- HoltWinters(train)
      } else if(modo == "mano") {
        modelo <- HoltWinters(train, alfa, beta, gama)
      } else {
        modelo <- calibrar.HW(train, test, paso)
      }
    } else if (nombre == "arim") {
      modo <- isolate(input$modo)
      p <- isolate(input$p)
      d <- isolate(input$d)
      q <- isolate(input$q)
      P <- isolate(input$P)
      D <- isolate(input$D)
      Q <- isolate(input$Q)
      periodo <- isolate(input$periodo)
      ar <- isolate(input$ar)
      es <- isolate(input$es)
      if(modo == "auto") {
        modelo <- auto.arima(train)
      } else if(modo == "mano") {
        modelo <- arima(train, order = c(p, d, q), 
                        seasonal = list(order = c(P, D, Q), period = periodo))
      } else {
        modelo <- calibrar.arima(train, test, periodo, 0:ar, 0:es)
        modelo$call$order <- c(modelo$arma[1], modelo$arma[6], modelo$arma[2])
        modelo$call$seasonal <- list(
          order = c(modelo$arma[3], modelo$arma[7], modelo$arma[4]), 
          period = modelo$arma[5])
      } 
    } else if (nombre == "reds") {
      size <- isolate(input$size)
      modelo <- nnetar(train, size = size)
    } else if (nombre == "deep") {
      modelo <- NULL
    }
    
    return(modelo)
  }, error = function(e) {
    print(e)
    return(list(modelo = NULL, algoritmo = NULL))
  })
}
