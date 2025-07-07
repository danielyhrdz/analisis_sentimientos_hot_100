#' Raspar letras de canciones de la página de genius
#'
#' @param url dirección web de genius a leer
#'
#' @return objeto con la letra encontrada
#' 
raspar_letras <- function(url) {
  
  # leer contenedor de la letra usando el xpath
  lyrics <-  url %>%
    html_nodes(xpath = '//div[contains(@class, "Lyrics__Container")]')
  
  xml_find_all(lyrics, ".//br") %>% 
    xml_add_sibling("p", "\n")
  xml_find_all(lyrics, ".//br") %>% 
    xml_remove()
  
  # tomar el texto plano del contenedor de letras
  lyrics <- html_text(lyrics, trim = TRUE)
  
  # limpiar texto plano 
  lyrics <- unlist(strsplit(lyrics, split = "\n"))
  lyrics <- grep(pattern = "[[:alnum:]]", lyrics, value = TRUE)
  
  # manejo de errores
  if (is_empty(lyrics)) {
    return(lyric = "instrumental only")
  } else{ 
    lyrics_with_breaks <- paste(lyrics, collapse = "\n")
    return(lyric = lyrics_with_breaks)
  }
}

#' Wrapper seguro de la función 
#'
#' @param path dirección web a pasar a la función de raspar_letras
#'
#' @return letra de la canción o "no lyric" si hay error
#' 
raspar_letras_safe <- function(path) {
  tryCatch({
    url <- read_html(path)
    song <- raspar_letras(url = url)
    return(song)
  }, error = function(e) {
    warning(paste("Error raspando letras de: ", path, e$message))
    return("no lyric")
  })
}


