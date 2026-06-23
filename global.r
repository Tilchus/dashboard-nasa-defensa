# global.R
library(shiny)
library(httr)
library(jsonlite, warn.conflicts = FALSE)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(dotenv)
library(bslib, warn.conflicts = FALSE)
library(bsicons)
library(plotly) 
library(DT)     
library(urltools)

try(load_dot_env(".env"), silent = TRUE)

# Función de traducción
traducir_texto <- function(texto) {
  if (is.null(texto) || texto == "") return("")
  
  if (nchar(texto) <= 450) {
    texto_codificado <- url_encode(texto)
    url_traductor <- paste0("https://api.mymemory.translated.net/get?q=", 
                            texto_codificado, "&langpair=en|es")
    res <- try(GET(url_traductor), silent = TRUE)
    if (inherits(res, "try-error") || res$status_code != 200) return(texto)
    res_json <- fromJSON(content(res, "text", encoding = "UTF-8"))
    return(res_json[["responseData"]][["translatedText"]])
  }
  
  oraciones <- unlist(strsplit(texto, "(?<=\\.)\\s+", perl = TRUE))
  texto_traducido_total <- c()
  bloque_actual <- ""
  
  for (oracion in oraciones) {
    if (nchar(bloque_actual) + nchar(oracion) + 1 > 450) {
      if (bloque_actual != "") {
        texto_codificado <- url_encode(bloque_actual)
        url_traductor <- paste0("https://api.mymemory.translated.net/get?q=", 
                                texto_codificado, "&langpair=en|es")
        res <- try(GET(url_traductor), silent = TRUE)
        if (!inherits(res, "try-error") && res$status_code == 200) {
          res_json <- fromJSON(content(res, "text", encoding = "UTF-8"))
          texto_traducido_total <- c(texto_traducido_total, res_json[["responseData"]][["translatedText"]])
        } else {
          texto_traducido_total <- c(texto_traducido_total, bloque_actual) 
        }
      }
      bloque_actual <- oracion
    } else {
      if (bloque_actual == "") {
        bloque_actual <- oracion
      } else {
        bloque_actual <- paste(bloque_actual, oracion)
      }
    }
  }
  
  if (bloque_actual != "") {
    texto_codificado <- url_encode(bloque_actual)
    url_traductor <- paste0("https://api.mymemory.translated.net/get?q=", 
                            texto_codificado, "&langpair=en|es")
    res <- try(GET(url_traductor), silent = TRUE)
    if (!inherits(res, "try-error") && res$status_code == 200) {
      res_json <- fromJSON(content(res, "text", encoding = "UTF-8"))
      texto_traducido_total <- c(texto_traducido_total, res_json[["responseData"]][["translatedText"]])
    } else {
      texto_traducido_total <- c(texto_traducido_total, bloque_actual)
    }
  }
  
  return(paste(texto_traducido_total, collapse = " "))
}