#' Filter numeric variables of a data.frame
#'
#' @param data a data.frame object.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return data.frame
#' @export var.numericas
#' @examples
#' var.numericas(iris)
#' 
var.numericas <- function(data) {
  if(is.null(data)) return(NULL)
  subset(data, select = sapply(data, class) %in% c('numeric', 'integer'))
}

#' Filter category variables of a data.frame
#'
#' @param data a data.frame object.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return data.frame
#' @export var.categoricas
#' @examples
#' var.categoricas(iris)
#' 
var.categoricas <- function(data) {
  if(is.null(data)) return(NULL)
  subset(data, select = !sapply(data, class) %in% c('numeric', 'integer'))
}

#' Convert character to dates
#'
#' @param f a vector of character.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return list
#' @importFrom stringr str_extract str_replace str_remove str_to_lower
#' @export text_toDate
#' @examples
#' text_toDate("2021 january 30")
#' 
text_toDate <- function(f) {
  e <- list(y = NA, ms = NA, d = NA, h = NA, m = NA, s = NA)
  meses <- c("enero" = 1, "febrero" = 2, "marzo" = 3, "abril" = 4, 
             "mayo" = 5, "junio" = 6, "julio" = 7, "agosto" = 8, 
             "septiembre" = 9, "octubre" = 10, "noviembre" = 11, "diciembre" = 12,
             "january" = 1, "february" = 2, "march" = 3, "april" = 4, 
             "may" = 5, "june" = 6, "july" = 7, "august" = 8, 
             "september" = 9, "october" = 10, "november" = 11, "december" = 12)
  f <- str_to_lower(f)
  
  e[['y']] <- str_extract(f, "\\d{4}")
  f <- str_remove(f, "\\d{4}")
  
  f <- str_replace(f, "setiembre", "septiembre")
  f <- str_replace(f, "set", "sep")
  e[['ms']] <- str_extract(f, paste(names(meses), collapse = "|"))
  if(any(is.na(e[['ms']]))) {
    names(meses) <- str_extract(names(meses), "\\w{3}")
    e[['ms']] <- str_extract(f, paste(names(meses), collapse = "|"))
  }
  if(!any(is.na(e[['ms']]))) {
    e[['ms']] <- meses[e[["ms"]]]
  }
  
  aux <- str_extract(f, "\\d{1,2}")
  while(!any(is.na(aux))) {
    aux <- as.numeric(aux)
    if(max(aux) <= 12 & any(is.na(e[['ms']]))) {
      e[['ms']] <- aux
    } else if(max(aux) <= 23) {
      e[['h']] <- aux
    } else if(max(aux) <= 31) {
      e[['d']] <- aux
    } else if(max(aux) <= 59 & any(is.na(e[['m']]))) {
      e[['m']] <- aux
    } else if(max(aux) <= 59) {
      e[['s']] <- aux
    } else if(any(is.na(e[['y']]))) {
      e[['y']] <- aux
    }
    
    f <- str_remove(f, "\\d{1,2}")
    aux <- str_extract(f, "\\d{1,2}")
  }
  
  tipo <- 'years'
  ifelse(any(is.na(e[["y"]])),  e[["y"]]  <- 2021, tipo <- 'years')
  ifelse(any(is.na(e[["ms"]])), e[["ms"]] <- 1, tipo <- 'months')
  ifelse(any(is.na(e[["d"]])),  e[["d"]]  <- 1, tipo <- 'days')
  ifelse(any(is.na(e[["h"]])),  e[["h"]]  <- 0, tipo <- 'hours')
  ifelse(any(is.na(e[["m"]])),  e[["m"]]  <- 0, tipo <- 'min')
  ifelse(any(is.na(e[["s"]])),  e[["s"]]  <- 0, tipo <- 'sec')
  
  res <- paste0(e[["y"]], "-", e[["ms"]], "-", e[["d"]], " ", 
                e[["h"]], ":", e[["m"]], ":", e[["s"]])
  res <- as.POSIXct(res, tz = "UTC")
  
  if(tipo == "days" & !any(wday(res) %in% c(1, 7))) {
    tipo <- "workdays"
  }
  
  return(list(res, tipo))
}

#' Get ts start of a time series
#'
#' @param ini a Date object.
#' @param tipo_f type of the time series ('year', 'month', ..., 'seconds').
#' @param patron frequency of time series.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return numeric vector of lenght 2
#' @importFrom lubridate ymd_hms year hour minute second
#' @export get_start
#' @examples
#' get_start(as.Date("2021-06-30"), 'days', 365)
#' 
get_start <- function(ini, tipo_f, patron) {
  if(patron == 1) {
    return(c(year(ini), 1))
  } else if(patron == 5) {
    return(c(1, wday(ini) - 1))
  } else if(patron == 7) {
    return(c(1, wday(ini)))
  } else if(patron == 24) {
    return(c(1, hour(ini) + 1))
  } else if(patron == 60 & tipo_f == "min") {
    return(c(1, minute(ini) + 1))
  } else if(patron == 1440) {
    return(c(1, hour(ini) * 60 + minute(ini) + 1))
  } else if(patron == 60) {
    return(c(1, second(ini) + 1))
  } else if(patron == 3600) {
    return(c(1, minute(ini) * 60 + second(ini) + 1))
  } else if(patron == 86400) {
    return(c(1, hour(ini) * 3600 + minute(ini) * 60 + second(ini) + 1))
  } else if(patron == 260) {
    ini <- as.Date(ini)
    fechas <- seq(as.Date(paste0(year(ini), "-01-01")), 
                  as.Date(paste0(year(ini), "-12-31")), by = "days")
    return(c(year(ini), which(fechas == ini)))
  } else {
    start <- ymd_hms(paste0(year(ini), "-01-01 00:00:00"))
    end   <- ymd_hms(paste0(year(ini), "-12-31 23:59:59"))
    dates <- seq(start, end, by = tipo_f)
    res   <- which(dates == ini) 
    return(c(year(ini), res))
  }
}

carga.datos <- function(ruta = NULL, sep = ";", dec = ",", encabezado = T) {
  if(!is.null(ruta)) {
    ruta <- gsub("\\", "/", ruta, fixed = T)
  }
  return(read.table(ruta, header = encabezado, sep = sep, dec = dec, row.names = NULL))
}

############################### Generar Código ################################

code.carga <- function(ruta = NULL, separador = ";", sep.decimal = ",", 
                       encabezado = T) {
  paste0("datos <- read.table(stringsAsFactors = T, '", ruta, "', header=",
         encabezado, ", sep='", separador, "', dec = '", sep.decimal, "')")
}

code.tsdf <- function(col, ini = NULL, fin = NULL, tipo = NULL, cold = NULL) {
  agregar <- ifelse(
    !is.null(cold), paste0("d <- text_toDate(datos[['", cold, "']])[[1]]"),
    paste0("d <- seq(as.POSIXct('", ini, "'), as.POSIXct('", fin, "'), by = '",
           tipo, "')"))
  paste0(agregar, "\nseriedf <- data.frame(d = d, v = datos[['", col, "']])")
}

code.ts <- function(s, f, n_train, n_test) {
  paste0(
    "seriets <- ts(seriedf[[2]], start = c(", s[1], ", ", s[2], "), frequency = ", f, ")\n",
    "train   <- head(seriets, ", n_train, ")\n",
    "test    <- tail(seriets, ", n_test, ")"
  )
}

code.ts.new <- function(s, f) {
  paste0(
    "seriets <- ts(seriedf[[2]], start = c(", s[1], ", ", s[2], "), frequency = ", f, ")"
  )
}