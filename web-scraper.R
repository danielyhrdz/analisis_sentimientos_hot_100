library(rvest)
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(glue)
library(xml2)

# Consulta complementario ------------------------------------------------------

source("get_billboard_hot.R")

## Rango de fechas a consultar 
fecha_inicio <- as_date("2017-09-16")
fecha_fin <- as_date("2025-04-12")

## Objeto con 1 fecha por semana dentro del rango definido
fechas <- seq(from = fecha_inicio, to = fecha_fin, by = "week")

## Ejecuta consulta para las fechas definidas
consulta_completa <- map(fechas, ~ get_billboard_hot_safe(.x))

## Une las consultas en un solo objeto 
consulta_df <- list_rbind(consulta_completa)

## Escribe csv con consulta completa para evitar pérdida de resultados
write.csv(consulta_df, "consulta_hot_100_completa.csv")

# Unión de conjunto de datos de Kaggle y consulta complementario ---------------

## Leer csv descargado de Kaggle y llevar a formato deseado
viejo_hot_100 <- read.csv('Hot Stuff.csv') |> 
  select(c(WeekID, Week.Position, Song, Performer)) |> 
  rename(c(fecha = WeekID, posicion = Week.Position, 
           cancion = Song, artista = Performer)) |> 
  mutate(fecha = mdy(str_replace_all(fecha, "/", "-")),
         fecha = ymd(fecha)) |> 
  filter(fecha < "2017-09-16") # se agrega filtro para evitar superposición 

## Leer consulta complementaria y llevar a formato deseado
hot_100_nuevo <- read.csv('consulta_hot_100_completa.csv') |> 
  select(-X) |> 
  rename(c(posicion = rank)) |> 
    mutate(fecha = ymd(fecha))

## Unir ambos conjuntos de datos
hot_100 <- bind_rows(viejo_hot_100, hot_100_nuevo) |> 
  distinct(cancion, artista, .keep_all = TRUE)

## Escribir csv con todas las listas obtenidas
write.csv(hot_100, "historico_hot_100.csv")
