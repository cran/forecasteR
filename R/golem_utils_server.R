#' Inverted versions of in, is.null and is.na
#' 
#' @noRd
#' 
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#' 
#' @noRd
#' 
#' @example 
#' dropNulls(list(1, NULL, 2))
dropNulls <- function (x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

#' If x is `NULL`, return y, otherwise return x
#' 
#' @param x,y Two elements to test, one potentially `NULL`
#' 
#' @noRd
#' 
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y){
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#' 
#' @param x,y Two elements to test, one potentially `NA`
#' 
#' @noRd
#' 
#' @examples
#' NA %||% 1
"%|NA|%" <- function(x, y){
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Typing reactiveValues is too long
#' 
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#' 
#' @noRd
rv <- shiny::reactiveValues
rvtl <- shiny::reactiveValuesToList

########################################################

load("inst/app/lang/translation.bin")

tr <- function(text, idioma = "es") {
  sapply(text, function(s) {
    elem <- ifelse(is.null(translation[[s]][[idioma]]), s,
                   translation[[s]][[idioma]])
    Encoding(elem) <- "utf8"
    
    elem
  }, USE.NAMES = F)
}

cambiar.labels <- function() {
  x <- c('data', 'date', 'acercade', 'idioma', 'selidioma','copyright', 'info',
         'version', 'cargar', 'header', 'separador', 'separadordec',
         'cargarchivo', 'subir', 'sig', 'ant', 'selvalor', 'selfecha', 'train',
         'test', 'opciones', 'ejecutar', 'basico', 'norm', 'plotnormal',
         'selcolbar', 'selcolline', 'selcolpoint', 't_c', 'desc', 'peri',
         'apre', 'comp', 'prom', 'naiv', 'snai', 'drif', 'reds', 'text_m',
         'table_m', 'plot_m', 'error_m', 'mse', 'rmse', 'seltipo', 'selrango',
         'hasta', 'sel', 'cre', 'selpatron', 'colts', 'coltend', 'colcicl', 
         'colseas', 'colresi', 'colperi', 'colbest', 'selbest', 'coltrain',
         'coltest', 'colpred', 'tamred', 'selperi', 'corre', 'cali', 'calip',
         'paso', 'lar', 'les', 'news', 'txterror', 'n_pred', 'code', 'generar',
         'sdiff', 'auto', 'mano', 'brut', 'selmodel')
  
  return(x)
}

# Función para generar diccionario.
# crear.traslation <- function() {
#   library(plyr)
#   archivo <- read.table("diccionario.csv", header = TRUE, sep = ";", as.is = TRUE)
#   translation <- dlply(archivo , .(key), function(s) key = as.list(s))
# 
#   save(translation, file = "translation.bin")
# }