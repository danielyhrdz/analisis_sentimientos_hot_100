library(geniusr)
library(tidyverse)
library(bigrquery)

# Lectura de historico desde BQ ------------------------------------------------

bigrquery::bq_auth(path = "./sa_tesis.json")

conn <- DBI::dbConnect(
  bigrquery::bigquery(),
  project = "tesis-2025-457202",
  dataset = "sentiment_analysis_db",
  billing = "tesis-2025-457202"
)

`%notin%` <- Negate(`%in%`)

## Utilizado para paralelizar 
filas <- eval(parse(text = Sys.getenv("FILAS_LEER")))

urls_ya <- tbl(conn, "genius_urls_new") |> 
  filter(!is.na(url)) |> 
  distinct(url, cancion, artista, .keep_all = TRUE) |> 
  collect() |> 
  mutate(artista = tolower(artista),
           artista = str_replace_all(artista, "[[:punct:]]", " "),
           artista = str_replace_all(artista, " ", ""),
           cancion = tolower(cancion),
           cancion = str_replace_all(cancion, "[[:punct:]]", " "),
           cancion = str_replace_all(cancion, " ", ""),
           llave = paste(cancion, artista, sep = "_"))

urls_na <- tbl(conn, "genius_urls_new") |> 
  filter(is.na(url)) |> 
  distinct(url, cancion, artista, .keep_all = TRUE) |> 
  collect() |> 
  mutate(artista = tolower(artista),
           artista = str_replace_all(artista, "[[:punct:]]", " "),
           artista = str_replace_all(artista, " ", ""),
           cancion = tolower(cancion),
           cancion = str_replace_all(cancion, "[[:punct:]]", " "),
           cancion = str_replace_all(cancion, " ", ""),
           llave = paste(cancion, artista, sep = "_"))

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

historico <- tbl(conn, "hot_100_historico_unico") |> 
  collect() |>  
  mutate(performer_clean = tolower(performer),
           performer_clean = str_replace_all(performer_clean, "[[:punct:]]", " "),
           performer_clean = str_replace_all(performer_clean, " ", ""),
           song_clean = tolower(song),
           song_clean = str_replace_all(song_clean, "[[:punct:]]", " "),
           song_clean = str_replace_all(song_clean, " ", ""),
           llave = paste(song_clean, performer_clean, sep = "_")) |> 
  filter(llave %notin% urls_ya$llave,
         llave %notin% letras_obtenidas$llave)

## Definir canciones a las que buscar el url
canciones <- historico |> 
  pull(song)

artistas <- historico |> 
  pull(performer)

# Buscar urls ------------------------------------------------------------------

## Cargar funcion para buscar url
get_genius_url_new <- function(cancion, artista) {

  Sys.sleep(1)

  url_result <- tryCatch({
    # Buscar canción en genius 
    search_results <- search_song(cancion, n_results = 60) |>
      mutate(artist_name = tolower(artist_name),
             primera_palabra = substr(artist_name, 1, 5),
             artist_name = str_replace_all(artist_name, "[[:punct:]]", " "),
             artist_name = str_replace_all(artist_name, " ", ""))
    
    artista <- tolower(artista)
    primero <- substr(artista, 1, 5)
    artista <- str_replace_all(artista, "[[:punct:]]", " ")
    artista <- str_replace_all(artista, " ", "")

    # Filtrar resultados por el más popular 
    url <- search_results |>
      filter(tolower(song_name) == tolower(cancion),
             str_detect(artista, artist_name)) |> 
      slice(1) |> 
      pull(song_lyrics_url)

    # Si no encuentra un url, buscar con primera palabra del artista
    if (length(url) == 0) {
      url <- search_results |>
        mutate(artist_name = tolower(artist_name),
               primera_palabra = substr(artist_name, 1, 5)) |> 
        filter(str_detect(primero, primera_palabra) | str_detect(primero, artist_name)) |> 
      slice(1) |> 
      pull(song_lyrics_url)
      return(NA_character_)
    } else {
      return(url)
    }
  }, error = function(e) {
    message(paste("Ocurrión un error para:", cancion, "-", artista, "\nDetalles:", e$message))
    return(NA_character_)
  })

  return(url_result)
}

# Iterar sobre faltantes
bq_project_id <- "tesis-2025-457202" 
bq_dataset_id <- "sentiment_analysis_db"
bq_table_id   <- "genius_urls_new"

# Crear referencia de la tabla a rellenar con urls
table_ref <- bq_table(bq_project_id, bq_dataset_id, bq_table_id)

# Configurar parámetros de los lotes de busqueda y subida a BigQuery
total_songs <- length(canciones)
batch_size <- 50
num_batches <- ceiling(total_songs / batch_size)

# Iterar sobre los lotes definidos
for (i in 1:num_batches) {
  # Print progress
  cat(paste0("Procesando lote ", i, " de ", num_batches, "...\n"))
  
  # Determinar el inicio y final del lote
  start_index <- (i - 1) * batch_size + 1
  end_index <- min(i * batch_size, total_songs) 
  
  # Definir canciones y artistas del lote actual
  batch_canciones <- canciones[start_index:end_index]
  batch_artistas <- artistas[start_index:end_index]
  
  # Ejecutar busqueda de url sobre el lote
  batch_urls <- map2_chr(batch_canciones, batch_artistas, get_genius_url_new)
  
  # Crear dataframe con el resultado de busqueda sobre el lote actual
  batch_df <- tibble(
    cancion = batch_canciones,
    artista = batch_artistas,
    url = batch_urls
  )
  
  # Escribir resultados a BigQuery
  tryCatch({
    bq_table_upload(table_ref, batch_df, write_disposition = "WRITE_APPEND")
    cat(paste0("Lote  ", i, " completo. Results escritos en BigQuery.\n\n"))
  }, error = function(e) {
    cat(paste0("Error subiendo lote ", i, " a BigQuery: ", e$message, "\n"))
  })
}

cat("Proceso finalizado\n")