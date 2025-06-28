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
    search_results <- search_song(cancion, n_results = 50) |> 
      

    # Filtrar resultados por el más popular 
    url <- search_results |>
      filter(tolower(song_name) == tolower(cancion),
             tolower(artista) %in% tolower(artist_name)) |> 
      slice(1) |> 
      pull(song_lyrics_url)

    # Si no encuentra un url, deolver NA
    if (length(url) == 0) {
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
