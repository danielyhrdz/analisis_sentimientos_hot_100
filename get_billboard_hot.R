#' Encontrar lista de Hot 100 de Billboard para una semana
#'
#' @param date Fecha para la que se va a consultar la lista
#'
#' @return Un dataframe con la lista Hot 100 para la semana de la fecha indicada
#'
get_billboard_hot <- function(date) {
  
  week_date <- as.Date(date)
  
  # construye el url basado en la fecha que se le da a la función
  input <- paste0("https://www.billboard.com/charts/hot-100/", date) 
  chart_page <- xml2::read_html(input)
  
  # busca los artistas dentro de la lista de esa semana
  artists <- chart_page %>% 
    html_nodes("li.o-chart-results-list__item") %>% 
    html_node("span") %>% 
    html_text2() |> 
    as_tibble() |> 
    drop_na() %>%
    filter(is.na(as.numeric(as.character(value))),
           value != "-",
           value != "NEW",
           value != "RE- ENTRY"
    )
  
  # busca las canciones dentro de la lista de esa semana
  titles <- chart_page %>% 
    html_elements("#title-of-a-story") |> 
    html_text2() |> 
    as_tibble() |> 
    filter(value != "Songwriter(s):",
           value != "Producer(s):",
           value != "Imprint/Promotion Label:",
           value != "Have a Tip?",
           value != "Follow Us",
           value != "Gains in Weekly Performance",
           value != "Additional Awards") |> 
    filter(nchar(as.character(value)) < 50) |> 
    slice(1:100)
  
  # une todo en un solo objeto 
  chart_df <- tibble(fecha = week_date, 
                     rank = c(1:100),
                     artista = artists$value,
                     cancion = titles$value)
  
  # mensaje de finalización 
  print(glue::glue("Finalicé la consulta de la semana de {date}"))
  
  # devuelve un objeto dataframe con toda la información
  return(chart_df)
}

#' Wrapper con atrapado de errores sobre función get_billboard_hot
#'
#' @param fecha Fecha que se le entrega a la función get_billboard_hot
#'
#' @return Resultado de get_billboard_hot o mensaje de error
#'
get_billboard_hot_safe <- function(fecha) {
  tryCatch({
    get_billboard_hot(date = fecha)
  }, error = function(e) {
    warning(paste0("Error consultando la semana: ", date, ": ", e$message))
  })
}