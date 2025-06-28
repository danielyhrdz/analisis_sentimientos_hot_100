library(rvest)
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(glue)
library(xml2)
library(bigrquery)
library(DBI)
library(readxl)

# Conexión a BigQuery ----------------------------------------------------------
bq_project_id <- "tesis-2025-457202" 
bq_dataset_id <- "sentiment_analysis_db"
bq_table_id   <- "letras_canciones_hot_100_new"

bigrquery::bq_auth(path = "./sa_tesis.json")

conn <- DBI::dbConnect(
  bigrquery::bigquery(),
  project = bq_project_id,
  dataset = bq_dataset_id,
  billing = bq_project_id
)

# Evitar buscar letras que ya tenemos ------------------------------------------

`%notin%` <- Negate(`%in%`)

## Leer direcciones web a las que consultar
urls <- tbl(conn, "genius_urls_new") |>  
  collect() 

if (DBI::dbExistsTable(conn, "letras_canciones_hot_100_new")) {
  
  # If the table exists, filter out the URLs we already have
  message("Table 'letras_canciones_hot_100_new' found. Filtering out existing songs.")
  
  letras_obtenidas <- tbl(conn, "letras_canciones_hot_100_new") |> 
    distinct(letra, .keep_all = TRUE) |> 
    select(cancion, artista) |>
    collect() |> 
    mutate(artista = tolower(artista),
           artista = str_replace_all(artista, "[[:punct:]]", " "),
           artista = str_replace_all(artista, " ", ""),
           cancion = tolower(cancion),
           cancion = str_replace_all(cancion, "[[:punct:]]", " "),
           cancion = str_replace_all(cancion, " ", ""),
           llave = paste(cancion, artista, sep = "_")) 
  
  urls_a_buscar <- urls |>
     mutate(artista = tolower(artista),
            artista = str_replace_all(artista, "[[:punct:]]", " "),
            artista = str_replace_all(artista, " ", ""),
            cancion = tolower(cancion),
            cancion = str_replace_all(cancion, "[[:punct:]]", " "),
            cancion = str_replace_all(cancion, " ", ""),
            llave = paste(cancion, artista, sep = "_")) |> 
      filter(llave %notin% letras_obtenidas$llave, !is.na(url))
  
} else {
  
  message("Table 'letras_canciones_hot_100_new' not found. Processing all URLs.")
  
  urls_a_buscar <- urls |>
    mutate(llave = paste(cancion, artista, sep = "_"))
    
}

# Raspado web por lotes y escritura a BigQuery ---------------------------------

table_ref <- bq_table(bq_project_id, bq_dataset_id, bq_table_id)

## Cargar funciones necesarias
source("lyrics.R")

## Parámetros de los lotes
total_urls <- nrow(urls_a_buscar)
batch_size <- 50
total_batches <- ceiling(total_urls / batch_size)

## Procesar lotes y escribir en BigQuery
for (i in 1:total_batches) {
  
  # Calcular las filas de inicio y fin para el lote actual
  start_row <- (i - 1) * batch_size + 1
  end_row <- min(i * batch_size, total_urls) 
  
  # Imprimir un mensaje de progreso en la consola
  cat(paste0("Procesando lote ", i, "/", total_batches, ": Filas ", start_row, " a ", end_row, "...\n"))
  
  # Seleccionar el lote actual del dataframe de URLs
  current_batch <- urls_a_buscar[start_row:end_row, ] |> 
    distinct(url, .keep_all = TRUE) |> 
    filter(cancion != "cancion")
  
  # Procesar el lote actual para obtener las letras de las canciones
  processed_batch <- tryCatch({
    current_batch %>%
      mutate(letra = purrr::map_chr(url, raspar_letras_safe)) |> 
      select(-llave)
  }, error = function(e) {
    # Manejo de errores
    cat(paste0("  ERROR en lote ", i, ": ", e$message, "\n"))
    current_batch %>% mutate(letra = "no lyric")|> 
      select(-llave)
  })
  
  # Escribir el lote procesado en BigQuery
  tryCatch({
    bq_table_upload(
      x = table_ref,
      values = processed_batch,
      write_disposition = "WRITE_APPEND"
    )
    
    cat(paste0("  ✓ Escritura exitosa del lote ", i, " a BigQuery\n"))
    
  }, error = function(e) {
    cat(paste0("  ✗ ERROR escribiendo lote ", i, " to BigQuery: ",
               e$message, "\n"))
  })
  
  # Pausar entre lotes
  Sys.sleep(1)
}

## Cerrar conexión
DBI::dbDisconnect(conn)

cat("Procesamiento completado!\n")