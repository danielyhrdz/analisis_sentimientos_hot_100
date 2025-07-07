library(rvest) # para leer htmls crudos directamente en R y separar nodos para identificar elementos deseados
library(stringr) # para manejo de strings o cadenas de texto en R
library(dplyr)# para manipulación de datos en general
library(tidyr)# para transformación de conjuntos de datos enteros 
library(lubridate)# para manejo de fechas 
library(purrr) # para crear procesos iterativos eficientes
library(glue) # para concatenación de palabras
library(xml2) # para leer e interpretar xmls

# Cargar funciones

source("get_billboard_hot.R")

## Rango de fechas a consultar 
fecha_inicio <- lubridate::as_date("2017-09-16")
fecha_fin <- lubridate::as_date("2025-04-12")

## Objeto con 1 fecha por semana dentro del rango definido
fechas <- seq(from = fecha_inicio, to = fecha_fin, by = "week")

## Ejecuta consulta para las fechas definidas
consulta_completa <- purrr::map(fechas, ~ get_billboard_hot_safe(.x))

## Une las consultas en un solo objeto 
consulta_df <- purrr::list_rbind(consulta_completa)

## Escribe csv con consulta completa para evitar pérdida de resultados
write.csv(consulta_df, "consulta_hot_100_completa.csv")

# Unión de conjunto de datos de Kaggle y consulta complementario ---------------

## Leer csv descargado de Kaggle y llevar a formato deseado
viejo_hot_100 <- read.csv('Hot Stuff.csv') |> 
  dplyr::select(c(WeekID, Week.Position, Song, Performer)) |> 
  dplyr::rename(c(fecha = WeekID, posicion = Week.Position, 
                  cancion = Song, artista = Performer)) |> 
  dplyr::mutate(fecha = mdy(str_replace_all(fecha, "/", "-")),
                fecha = ymd(fecha)) |> 
  dplyr::filter(fecha < "2017-09-16") # se agrega filtro para evitar superposición 

## Leer consulta complementaria y llevar a formato deseado
hot_100_nuevo <- read.csv('consulta_hot_100_completa.csv') |> 
  dplyr::select(-X) |> 
  dplyr::rename(c(posicion = rank)) |> 
    dplyr::mutate(fecha = ymd(fecha))

## Unir ambos conjuntos de datos
hot_100 <- dplyr::bind_rows(viejo_hot_100, hot_100_nuevo)

hot_100_unico <- hot_100 |> 
  dplyr::distinct(cancion, artista, .keep_all = TRUE)

## Escribir csv con todas las listas obtenidas
write.csv(hot_100, "historico_hot_100.csv")
write.csv(hot_100_unico, "historico_hot_100_unico.csv")
