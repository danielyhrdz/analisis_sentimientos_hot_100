resultados_hasta_ahora <- read.csv("genius_url_results.csv") |> 
  filter(!is.na(lyrics_url))

write.csv(resultados_hasta_ahora, "urls_conseguidas.csv")




library(arrow)

letras_parquet <- read.csv("hot_100_with_lyrics.csv") |> 
  rename("letra" = "lyrics") |> 
  filter(letra != "no lyric") |> 
  write_parquet("letras_hot_100_filtrado.parquet")

## fix nuevas

unicos_nuevos_fixed <- unicos_nuevos %>%
  mutate(artista = case_when(
    row_number() < 509 ~ artista,          # For rows before 509, keep the value
    row_number() == 509 ~ NA_character_,   # At row 509, insert NA
    row_number() > 509 ~ lag(artista)      # For rows after 509, use the previous value
  ))

base <- unicos_nuevos |> 
  select(fecha, posicion, artista)

artistas_bien <- unicos_nuevos |> 
  mutate(index=row_number()) |> 
  filter(index != 509) |> 
  pull(artista)

canciones_bien <- unicos_nuevos |> 
  pull(cancion)

nuevo_fixed <- bind_cols(artistas_bien, canciones_bien[2:23808]) |> 
  slice(1:509) |> 
  janitor::clean_names() |> 
  rename(artista = x1, 
  cancion = x2)

arrow::write_parquet(nuevo_fixed, "509_nuevo_fixed_2.parquet")
