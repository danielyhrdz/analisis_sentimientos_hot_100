#' Conseguir url de genius para leer letra de la canción
#'
#' @param cancion Canción a buscar en url
#' @param artista Artista de canción a buscar en url
#'
#' @return
#' 
get_genius_url <- function(cancion, artista) {

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
}

