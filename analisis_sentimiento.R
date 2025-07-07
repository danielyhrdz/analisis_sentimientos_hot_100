library(DBI) # Conexión de los datos
library(bigrquery) # Conexión con BigQuery
library(dplyr) # Para manipulación de datos
library(stringr) # Para limpieza avanzada de texto con expresiones regulares
library(tidytext) # Para tokenización y acceso a léxicos de sentimiento
library(stopwords) # Para tomar stopwords
library(textdata) # Para descargar léxicos de sentimiento
library(googleLanguageR) # Para realizar traduccion de léxicos
library(tidymodels) # Ecosistema principal para modelado
library(ggplot2) # Para visualización de datos
library(textrecipes) # Pasos de receta específicos para texto
library(vip) # Para importancia de variables
library(discrim) # Para modelos de clasificación basados en métodos discriminantes
library(e1071) # Para Naive Bayes y SVM
library(caret) # Para partición de datos y evaluación de modelos
library(VIM) # Para manejo de valores faltantes
library(SnowballC) # Para stemming
library(Matrix) # Para matrices dispersas
library(randomForest) # Opcional: para comparación
library(pROC) # Para curvas ROC
library(quanteda) # Manejo de corpus
library(tm) # Manejo de corpus
library(doParallel) # Para paralelizar
library(future) # Alternative parallel backend
library(LiblineaR) # Para modelado
library(naivebayes) # Para modelado
library(lubridate) # Para manejo de fechas

# Conexión a BigQuery ----------------------------------------------------------
bq_proyecto_id <- "tesis-2025-457202"
bq_dataset_id <- "sentiment_analysis_db"
bq_tabla_id <- "letras_canciones_hot_100_new"

bigrquery::bq_auth(path = "./sa_tesis.json")

# Crear conexión Big Query

conn <- DBI::dbConnect(
  bigrquery::bigquery(),
  project = bq_proyecto_id,
  dataset = bq_dataset_id,
  billing = bq_proyecto_id
)

# Llamar la tabla

datos_canciones_crudos <- tbl(conn, bq_tabla_id) |>
  mutate(id_cancion = row_number()) |>
  filter(letra != "no lyrics") |>
  select(-url) |>
  relocate(id_cancion, artista, cancion, letra) |>
  distinct(letra, .keep_all = TRUE) |>
  collect()

datos_canciones_crudos_ml <- tbl(conn, bq_tabla_id) |>
  mutate(id_cancion = row_number()) |>
  filter(letra != "no lyrics") |>
  select(-url) |>
  collect()

# Limpieza de letra -------------------------------------------------------------

letras_refinadas <- datos_canciones_crudos |>
  mutate(
    letra_aislada = case_when(
      # Si encuentra un [ asume que ahí empieza la letra y extrae solo lo que sigue
      str_detect(letra, "\\[") ~ str_extract(letra, "(?s)\\[.*"),

      # Si no hay [, busca "read more" o "lyrics".
      str_detect(letra, "(?i)read more|lyrics") ~
        str_remove(letra, "(?is)^.*(?:read more|lyrics)"),

      TRUE ~ letra # Si no encuentra esos indicadores utiliza la letra original
    ),

    # Ahora aplicamos las limpiezas de formato únicamente al texto correcto.
    letra_limpia = letra_aislada |>
      str_remove_all("\\[[^\\]]*\\]|\\([^\\)]*\\)") |> # Elimina [tags] y (anotaciones)
      tolower() |> # Convierte a minúsculas
      str_remove_all("[[:punct:]]|[[:digit:]]") |> # Elimina puntuación y números
      str_squish() # Normaliza todos los espacios
  ) |>
  # Asegurar de que el resultado no esté vacío y seleccionamos las columnas finales.
  filter(nchar(letra_limpia) > 5) |>
  select(-letra, -letra_aislada) # Eliminar columnas de trabajo

letras_refinadas_ml <- datos_canciones_crudos_ml |>
  mutate(
    letra_aislada = case_when(
      str_detect(letra, "\\[") ~ str_extract(letra, "(?s)\\[.*"),
      str_detect(letra, "(?i)read more|lyrics") ~
        str_remove(letra, "(?is)^.*(?:read more|lyrics)"),
      TRUE ~ letra
    ),
    letra_limpia = letra_aislada |>
      str_remove_all("\\[[^\\]]*\\]|\\([^\\)]*\\)") |>
      tolower() |>
      str_remove_all("[[:punct:]]|[[:digit:]]") |>
      str_squish()
  ) |>
  filter(nchar(letra_limpia) > 5) |>
  select(-letra, -letra_aislada)

# Tokenizaciónn y eliminación de stopwords -------------------------------------
letras_tokenizadas <- letras_refinadas |>
  unnest_tokens(
    output = palabra,
    input = letra_limpia,
    token = "words"
  )

letras_tokenizadas_ml <- letras_refinadas_ml |>
  unnest_tokens(
    output = palabra,
    input = letra_limpia,
    token = "words"
  )

# Eliminar palabras vacias en español e inglés
palabras_vacias_es <- stopwords::stopwords(language = "es")
palabras_vacias_en <- stopwords::stopwords(language = "en") |>
  str_replace_all("[[:punct:]]", "") # Limpiamos puntuacion

palabras_vacias <- c(palabras_vacias_en, palabras_vacias_es) |>
  as_tibble() |>
  rename("palabra" = value)

# Filtrar palabras vacías
letras_tokenizadas <- letras_tokenizadas |>
  anti_join(palabras_vacias, by = "palabra")

letras_tokenizadas_ml <- letras_tokenizadas_ml |>
  anti_join(palabras_vacias, by = "palabra")

# Preparación para analisis con léxicos ----------------------------------------

# Cargar los léxicos desde tidytext
lexico_afinn_en <- get_sentiments("afinn") |>
  rename(sentimiento = value, palabra = word)
lexico_bing_en <- get_sentiments("bing") |>
  rename(sentimiento = value, palabra = word)
lexico_loughran_en <- get_sentiments("loughran") |>
  rename(sentimiento = value, palabra = word)
lexico_nrc_en <- get_sentiments("nrc") |>
  rename(sentimiento = value, palabra = word)

# Traducción

gl_auth("./sa_tesis.json")

traducir_y_unir_lexico <- function(lexico) {
  traduccion_es <- googleLanguageR::gl_translate(
    lexico$palabra,
    target = "es",
    source = "en"
  )
  traduccion_es_procesada <- traduccion_es |>
    left_join(lexico, by = c("text" = "palabra")) |>
    select(palabra = translatedText, sentimiento)

  lexico_completo <- traduccion_es_procesada |>
    full_join(lexico)

  return(lexico_completo)
}

lexico_afinn_es_en <- traducir_y_unir_lexico(lexico_afinn_en) |> distinct()
lexico_bing_es_en <- traducir_y_unir_lexico(lexico_bing_en) |> distinct()
lexico_loughran_es_en <- traducir_y_unir_lexico(lexico_loughran_en) |>
  distinct()
lexico_nrc_es_en <- traducir_y_unir_lexico(lexico_nrc_en) |> distinct()

sentimiento_afinn <- letras_tokenizadas |>
  inner_join(
    lexico_afinn_es_en,
    by = c("palabra" = "palabra"),
    relationship = "many-to-many"
  )

sentimiento_bing <- letras_tokenizadas |>
  inner_join(
    lexico_bing_es_en,
    by = c("palabra" = "palabra"),
    relationship = "many-to-many"
  )

sentimiento_loughran <- letras_tokenizadas |>
  inner_join(
    lexico_loughran_es_en,
    by = c("palabra" = "palabra"),
    relationship = "many-to-many"
  )

sentimiento_nrc <- letras_tokenizadas |>
  inner_join(
    lexico_nrc_es_en,
    by = c("palabra" = "palabra"),
    relationship = "many-to-many"
  )

write.csv(sentimiento_afinn, "analisis_sentimiento_afinn.csv")
write.csv(sentimiento_bing, "analisis_sentimiento_bing.csv")
write.csv(sentimiento_loughran, "analisis_sentimiento_loughran.csv")
write.csv(sentimiento_nrc, "analisis_sentimiento_nrc.csv")

## Se suben estos .csv a BigQuery

sentimiento_afinn <- tbl(conn, "lexicos_afinn") |>
  collect() |>
  mutate(
    artista_llave = tolower(artista),
    artista_llave = str_replace_all(artista_llave, "[[:punct:]]", " "),
    artista_llave = str_replace_all(artista_llave, " ", ""),
    cancion_llave = tolower(cancion),
    cancion_llave = str_replace_all(cancion_llave, "[[:punct:]]", " "),
    cancion_llave = str_replace_all(cancion_llave, " ", ""),
    llave = paste(cancion_llave, artista_llave, sep = "_")
  )
sentimiento_bing <- tbl(conn, "lexicos_bing") |>
  collect() |>
  mutate(
    artista_llave = tolower(artista),
    artista_llave = str_replace_all(artista_llave, "[[:punct:]]", " "),
    artista_llave = str_replace_all(artista_llave, " ", ""),
    cancion_llave = tolower(cancion),
    cancion_llave = str_replace_all(cancion_llave, "[[:punct:]]", " "),
    cancion_llave = str_replace_all(cancion_llave, " ", ""),
    llave = paste(cancion_llave, artista_llave, sep = "_")
  )
sentimiento_loughran <- tbl(conn, "lexicos_loughran") |>
  collect() |>
  mutate(
    artista_llave = tolower(artista),
    artista_llave = str_replace_all(artista_llave, "[[:punct:]]", " "),
    artista_llave = str_replace_all(artista_llave, " ", ""),
    cancion_llave = tolower(cancion),
    cancion_llave = str_replace_all(cancion_llave, "[[:punct:]]", " "),
    cancion_llave = str_replace_all(cancion_llave, " ", ""),
    llave = paste(cancion_llave, artista_llave, sep = "_")
  )
sentimiento_ncr <- tbl(conn, "lexicos_nrc") |>
  collect() |>
  mutate(
    artista_llave = tolower(artista),
    artista_llave = str_replace_all(artista_llave, "[[:punct:]]", " "),
    artista_llave = str_replace_all(artista_llave, " ", ""),
    cancion_llave = tolower(cancion),
    cancion_llave = str_replace_all(cancion_llave, "[[:punct:]]", " "),
    cancion_llave = str_replace_all(cancion_llave, " ", ""),
    llave = paste(cancion_llave, artista_llave, sep = "_")
  )

# Analisis resumen de léxicos --------------------------------------------------

bq_tabla_id_hist   <- "hot_100_historico"

hot_100_historico <- tbl(conn, bq_tabla_id_hist) |>
  collect() %>% 
  mutate(artista_llave = tolower(artista),
         artista_llave = str_replace_all(artista_llave, "[[:punct:]]", " "),
         artista_llave = str_replace_all(artista_llave, " ", ""),
         cancion_llave = tolower(cancion),
         cancion_llave = str_replace_all(cancion_llave, "[[:punct:]]", " "),
         cancion_llave = str_replace_all(cancion_llave, " ", ""),
         llave = paste(cancion_llave, artista_llave, sep = "_")) 

## Limpieza
sentimiento_afinn <- sentimiento_afinn |>
  mutate(artista_llave = tolower(artista),
         artista_llave = str_replace_all(artista_llave, "[[:punct:]]", " "),
         artista_llave = str_replace_all(artista_llave, " ", ""),
         cancion_llave = tolower(cancion),
         cancion_llave = str_replace_all(cancion_llave, "[[:punct:]]", " "),
         cancion_llave = str_replace_all(cancion_llave, " ", ""),
         llave = paste(cancion_llave, artista_llave, sep = "_")
  ) |>
  select(cancion, artista, llave, palabra, sentimiento)

## Unir los datos históricos con el lexico
datos_afinn_anual <- inner_join(hot_100_historico, sentimiento_afinn,
                                 by = "llave", relationship = "many-to-many") |>
  select(fecha, cancion = cancion.x, artista = artista.x, llave, palabra, sentimiento)


## Limpieza
sentimiento_nrc <- sentimiento_nrc |>
  mutate(artista_llave = tolower(artista),
         artista_llave = str_replace_all(artista_llave, "[[:punct:]]", " "),
         artista_llave = str_replace_all(artista_llave, " ", ""),
         cancion_llave = tolower(cancion),
         cancion_llave = str_replace_all(cancion_llave, "[[:punct:]]", " "),
         cancion_llave = str_replace_all(cancion_llave, " ", ""),
         llave = paste(cancion_llave, artista_llave, sep = "_")
  ) |>
  select(cancion, artista, llave, palabra, sentimiento)

## Unir los datos históricos con el lexico
datos_nrc_anual <- inner_join(hot_100_historico, sentimiento_nrc,
                                by = "llave", relationship = "many-to-many") |>
  select(fecha, cancion = cancion.x, artista = artista.x, llave, palabra, sentimiento)

## Se agrupa por año y se calcula el sentimiento AFINN promedio.
tendencia_afinn <- datos_afinn_anual |>
  mutate(anio = year(fecha)) |>
  group_by(anio) |>
  summarise(sentimiento_promedio = mean(sentimiento, na.rm = TRUE)) |>
  ungroup()

## Se añade una media móvil de 5 años para suavizar la tendencia y observar patrones a largo plazo
tendencia_afinn <- tendencia_afinn |>
  mutate(media_movil_5a = zoo::rollmean(sentimiento_promedio, k = 5, fill = NA, align = "right"))

## Gráfico de tendencia histórica del sentimiento (AFINN)
ggplot(tendencia_afinn, aes(x = anio)) +
  geom_line(aes(y = sentimiento_promedio, color = "Promedio Anual"), alpha = 0.5) +
  geom_line(aes(y = media_movil_5a, color = "Media Móvil (5 años)"), linewidth = 1.2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Promedio Anual" = "blue", "Media Móvil (5 años)" = "Coral")) +
  labs(
    title = "Evolución del sentimiento en la música popular (1958-2025)",
    subtitle = "Basado en el puntaje de valencia del léxico AFINN",
    x = "Año",
    y = "Puntaje de sentimiento oromedio"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())

## Se categorizan los sentimientos positivos y negativos (1, -1)
## para aproximar la el sentimiento promedio de AFINN
tendencia_nrc_pos_neg <- datos_nrc_anual |> 
  mutate(anio = year(fecha),
         sentimiento = case_when(
           sentimiento == "positive" ~ 1,
           sentimiento == "negative" ~ -1
         )) |> 
  group_by(anio) |> 
  summarise(sentimiento_promedio = mean(sentimiento, na.rm = TRUE)) |> 
  ungroup()

emociones_a_graficar <- tendencia_nrc |> 
  distinct(sentimiento) |> 
  pull(sentimiento) # hacer el vector de sentimienos

## Gráfico de tendencia histórica del sentimiento (NRC)
ggplot(filter(tendencia_nrc, sentimiento %in% emociones_a_graficar),
       aes(x = anio, y = proporcion, color = sentimiento)) +
  geom_line(alpha = 0.8, linewidth = 1) +
  geom_smooth(se = FALSE, method = "loess", span = 0.2, linewidth = 1.2, linetype = "dashed") +
  facet_wrap(~sentimiento, scales = "free_y") +
  labs(
    title = "Tendencia de sentimientos en las Letras (1958-2025)",
    subtitle = "Proporción de palabras asociadas a cada emoción según el léxico NRC",
    x = "Año",
    y = "Proporción de palabras"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

## Gráfico de tendencia histórica del sentimiento (NRC - Aproximado)
tendencia_nrc_pos_neg <- tendencia_nrc_pos_neg |> 
  mutate(media_movil_5a = zoo::rollmean(sentimiento_promedio, k = 5, fill = NA, align = "right"))

ggplot(tendencia_nrc_pos_neg, aes(x = anio)) +
  geom_line(aes(y = sentimiento_promedio, color = "Promedio anual"), alpha = 0.5) +
  geom_line(aes(y = media_movil_5a, color = "Media móvil (5 años)"), linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Promedio anual" = "grey50", "Media móvil (5 años)" = "Coral")) +
  labs(
    title = "Evolución del sentimiento en la música popular (1958-2025)",
    subtitle = "Basado en el puntaje de valencia del léxico NRC",
    x = "Año",
    y = "Puntaje de sentimiento promedio"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())

# Análisis con NRC
analisis_covid_nrc <- tendencia_nrc |> 
  filter(anio >= 2017) |> 
  mutate(periodo = case_when(
    anio %in% periodo_pre_pandemia ~ "Pre-Pandemia (2017-2019)",
    anio %in% periodo_pandemia     ~ "Pandemia (2020-2022)",
    anio %in% periodo_post_pandemia ~ "Post-Pandemia (2023-2025)"
  )) |> 
  group_by(periodo, sentimiento) |> 
  summarise(proporcion_promedio_periodo = mean(proporcion)) |> 
  filter(sentimiento %in% emociones_a_graficar)

analisis_covid_nrc_pos_neg <- tendencia_nrc_pos_neg |> 
  filter(anio >= 2017) |> 
  mutate(periodo = case_when(
    anio %in% periodo_pre_pandemia ~ "Pre-Pandemia (2017-2019)",
    anio %in% periodo_pandemia     ~ "Pandemia (2020-2022)",
    anio %in% periodo_post_pandemia ~ "Post-Pandemia (2023-2025)"
  )) |> 
  group_by(periodo) |> 
  summarise(sentimiento_promedio_periodo = mean(sentimiento_promedio))


## Gráfico de comparación de emociones (NRC) Pre y Durante la Pandemia

ggplot(analisis_covid_nrc, aes(x = sentimiento, y = proporcion_promedio_periodo, fill = periodo)) +
  geom_col(position = "dodge") +
  labs(
    title = "Comparación de sentimientos clave (NRC)",
    subtitle = "Periodo pre-pandemia vs. durante la pandemia",
    x = "Emoción",
    y = "Proporción promedio de palabras"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

## Grafico tendencia con afinn
ggplot(tendencia_afinn, aes(x = anio)) +
  geom_line(aes(y = sentimiento_promedio), color = "dodgerblue", alpha = 0.6, linewidth = 1.1) +
  
  labs(
    title = "Tendencia de sentimientos en la música popular (1958-2025)",
    subtitle = "Promedio anual basado en el léxico AFINN",
    x = "Año",
    y = "Puntaje de Sentimiento Promedio"
  ) +
  theme_minimal()

df_afinn <- sentimiento_afinn |>
  mutate(lexico = "AFINN", sentimiento_categorizado = as.character(sentimiento))
df_bing <- sentimiento_bing |>
  mutate(lexico = "Bing")
df_loughran <- sentimiento_loughran |>
  mutate(lexico = "Loughran")
df_nrc <- sentimiento_ncr |>
  mutate(lexico = "NRC")

# Categorizar el sentimiento para que sea comparable
df_afinn <- df_afinn |>
  mutate(
    sentimiento_categorizado = case_when(
      sentimiento > 0 ~ "positive",
      sentimiento < 0 ~ "negative",
      sentimiento == 0 ~ "neutral",
      TRUE ~ "unknown" # Para cualquier valor inesperado
    )
  )

# Renombrar la columna 'sentimiento' para Bing, Loughran y NRC para mantener consistencia
df_bing <- df_bing |>
  rename(sentimiento_categorizado = sentimiento)

df_loughran <- df_loughran |>
  rename(sentimiento_categorizado = sentimiento)

df_nrc <- df_nrc |>
  rename(sentimiento_categorizado = sentimiento)

# Seleccionar columnas comunes para la unión
df_afinn_unified <- df_afinn |>
  select(
    id_cancion,
    cancion,
    artista,
    palabra,
    sentimiento_categorizado,
    lexico
  )
df_bing_unified <- df_bing |>
  select(
    id_cancion,
    cancion,
    artista,
    palabra,
    sentimiento_categorizado,
    lexico
  )
df_loughran_unified <- df_loughran |>
  select(
    id_cancion,
    cancion,
    artista,
    palabra,
    sentimiento_categorizado,
    lexico
  )
df_nrc_unified <- df_nrc |>
  select(
    id_cancion,
    cancion,
    artista,
    palabra,
    sentimiento_categorizado,
    lexico
  )

# Unir todos los data frames en uno solo
todos_sentimientos <- bind_rows(
  df_afinn_unified,
  df_bing_unified,
  df_loughran_unified,
  df_nrc_unified
)

# Sentimientos más comunes por léxico
sentimientos_comunes_lexico <- todos_sentimientos |>
  group_by(lexico, sentimiento_categorizado) |>
  summarise(conteo = n(), .groups = 'drop') |>
  arrange(lexico, desc(conteo))

print(sentimientos_comunes_lexico)

# Sentimientos más comunes en general
sentimientos_comunes_general <- todos_sentimientos |>
  group_by(sentimiento_categorizado) |>
  summarise(conteo = n(), .groups = 'drop') |>
  arrange(desc(conteo))

print(sentimientos_comunes_general)

# Proporción de sentimientos positivos, negativos y neutros por léxico
todos_sentimientos_simple <- todos_sentimientos |>
  mutate(
    sentimiento_simplificado = case_when(
      sentimiento_categorizado == "positive" ~ "Positive",
      sentimiento_categorizado == "negative" ~ "Negative",
      sentimiento_categorizado == "neutral" ~ "Other", # AFINN categorizado
      sentimiento_categorizado == "1" ~ "Positive",
      sentimiento_categorizado == "2" ~ "Positive",
      sentimiento_categorizado == "3" ~ "Positive",
      sentimiento_categorizado == "4" ~ "Positive",
      sentimiento_categorizado == "5" ~ "Positive",
      sentimiento_categorizado == "-1" ~ "Negative",
      sentimiento_categorizado == "-2" ~ "Negative",
      sentimiento_categorizado == "-3" ~ "Negative",
      sentimiento_categorizado == "-4" ~ "Negative",
      sentimiento_categorizado == "-5" ~ "Negative",
      sentimiento_categorizado == "superfluous" ~ "Other", # Loughran
      sentimiento_categorizado == "uncertainty" ~ "Other", # Loughran
      sentimiento_categorizado %in%
        c("joy", "anticipation", "trust", "surprise", "positive") ~
        "Positive", # Considerados como positivos
      sentimiento_categorizado %in%
        c("fear", "anger", "sadness", "disgust", "negative") ~
        "Negative", # Considerados como negativos
      TRUE ~ "Other" # Cualquier otro sentimiento no mapeado explícitamente
    )
  )

proporcion_sentimientos <- todos_sentimientos_simple |>
  group_by(lexico, sentimiento_simplificado) |>
  summarise(conteo = n(), .groups = 'drop') |>
  group_by(lexico) |>
  mutate(proporcion = conteo / sum(conteo)) |>
  arrange(lexico, desc(proporcion))

print(proporcion_sentimientos)

# Visualización de la proporción simplificada
ggplot(
  proporcion_sentimientos,
  aes(x = lexico, y = proporcion, fill = sentimiento_simplificado)
) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Proporción de sentimientos simplificados por léxico",
    x = "Léxico",
    y = "Proporción",
    fill = "Sentimiento"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")


# Modelado ML ------------------------------------------------------------------

datos_ml_afinn <- letras_refinadas_ml |>
  mutate(
    artista_llave = tolower(artista),
    artista_llave = str_replace_all(artista_llave, "[[:punct:]]", " "),
    artista_llave = str_replace_all(artista_llave, " ", ""),
    cancion_llave = tolower(cancion),
    cancion_llave = str_replace_all(cancion_llave, "[[:punct:]]", " "),
    cancion_llave = str_replace_all(cancion_llave, " ", ""),
    llave = paste(cancion_llave, artista_llave, sep = "_")
  ) |>
  inner_join(sentimiento_afinn, by = "llave") |>
  mutate(
    sentimiento_categoria = factor(case_when(
      sentimiento > 0 ~ "positive",
      sentimiento < 0 ~ "negative"
    )),
  sentimiento = factor(sentimiento)
  ) |>
  dplyr::select(letra_limpia, sentimiento_categoria, sentimiento) 

datos_ml_nrc_bin <- letras_refinadas_ml |>
  mutate(
    artista_llave = tolower(artista),
    artista_llave = str_replace_all(artista_llave, "[[:punct:]]", " "),
    artista_llave = str_replace_all(artista_llave, " ", ""),
    cancion_llave = tolower(cancion),
    cancion_llave = str_replace_all(cancion_llave, "[[:punct:]]", " "),
    cancion_llave = str_replace_all(cancion_llave, " ", ""),
    llave = paste(cancion_llave, artista_llave, sep = "_")
  ) |>
  inner_join(sentimiento_ncr, by = "llave") |>
  filter(sentimiento == "positive" | sentimiento == "negative") |>
  dplyr::select(letra_limpia, sentimiento)

datos_ml_nrc_multi <- letras_refinadas_ml |>
  mutate(
    artista_llave = tolower(artista),
    artista_llave = str_replace_all(artista_llave, "[[:punct:]]", " "),
    artista_llave = str_replace_all(artista_llave, " ", ""),
    cancion_llave = tolower(cancion),
    cancion_llave = str_replace_all(cancion_llave, "[[:punct:]]", " "),
    cancion_llave = str_replace_all(cancion_llave, " ", ""),
    llave = paste(cancion_llave, artista_llave, sep = "_")
  ) |>
  inner_join(sentimiento_ncr, by = "llave") |>
  filter(sentimiento != "positive", sentimiento != "negative") |>
  dplyr::select(letra_limpia, sentimiento)

# Verificar la distribución de clases
table(datos_ml_afinn$sentimiento_categoria)
table(datos_ml_afinn$sentimiento)
table(datos_ml_nrc_bin$sentimiento)
table(datos_ml_nrc_multi$sentimiento)

# Para parelelizar entrenamiento -----------------------------------------------
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

# Tomar una muestra de los datos
set.seed(123)
max_samples <- 10000

if (nrow(datos_ml_afinn) > max_samples) {
  class_counts <- table(datos_ml_afinn$sentimiento_categoria)
  sample_per_class <- floor(max_samples / length(class_counts))

  datos_ml_afinn_sample <- datos_ml_afinn %>%
    group_split(sentimiento_categoria) %>%
    map_dfr(~ slice_sample(.x, n = min(sample_per_class, nrow(.x))))
} else {
  datos_ml_afinn_sample <- datos_ml_afinn
}

if (nrow(datos_ml_nrc_bin) > max_samples) {
  # Calcular muestra por grupo
  samples_per_class_nrc <- floor(
    max_samples / length(unique(datos_ml_nrc_bin$sentimiento))
  )

  datos_ml_nrc_bin_sample <- datos_ml_nrc_bin %>%
    group_split(sentimiento) %>%
    map_dfr(~ slice_sample(.x, n = min(samples_per_class_nrc, nrow(.x))))
} else {
  datos_ml_nrc_bin_sample <- datos_ml_nrc_bin
}

if (nrow(datos_ml_nrc_multi) > max_samples) {
  # Calcular muestra por grupo
  samples_per_class_nrc <- floor(
    max_samples / length(unique(datos_ml_nrc_multi$sentimiento))
  )

  datos_ml_nrc_multi_sample <- datos_ml_nrc_multi %>%
    group_split(sentimiento) %>%
    map_dfr(~ slice_sample(.x, n = min(samples_per_class_nrc, nrow(.x))))
} else {
  datos_ml_nrc_multi_sample <- datos_ml_nrc_multi
}

# Crear splits para cada diccionario
data_split_afinn_bin <- initial_split(
  datos_ml_afinn_sample,
  prop = 0.7,
  strata = sentimiento_categoria
)
data_split_nrc_bin <- initial_split(
  datos_ml_nrc_bin_sample,
  prop = 0.7,
  strata = sentimiento
)

data_split_afinn_multi <- initial_split(
  datos_ml_afinn,
  prop = 0.7,
  strata = sentimiento
)
data_split_nrc_multi <- initial_split(
  datos_ml_nrc_bin_sample,
  prop = 0.7,
  strata = sentimiento
)

train_data_afinn_bin <- training(data_split_afinn_bin)
test_data_afinn_bin <- testing(data_split_afinn_bin)

train_data_nrc_bin <- training(data_split_nrc_bin)
test_data_nrc_bin <- testing(data_split_nrc_bin)

train_data_afinn_multi <- training(data_split_afinn_multi)
test_data_afinn_multi <- testing(data_split_afinn_multi)

train_data_nrc_multi <- training(data_split_nrc_bin)
test_data_nrc_multi <- testing(data_split_nrc_bin)

cat("Tamaño conjunto entrenamiento AFINN binario:", nrow(train_data_afinn_bin), "\n")
cat("Tamaño conjunto entrenamiento NRC binario:", nrow(train_data_nrc_bin), "\n")

cat("Tamaño conjunto prueba AFINN binario:", nrow(test_data_afinn_bin), "\n")
cat("Tamaño conjunto prueba NRC binario:", nrow(test_data_nrc_bin), "\n")

cat("Tamaño conjunto entrenamiento AFINN multiclase:", nrow(train_data_afinn_multi), "\n")
cat("Tamaño conjunto entrenamiento NRC multiclase:", nrow(train_data_nrc_multi), "\n")

cat("Tamaño conjunto prueba AFINN multiclase:", nrow(test_data_afinn_multi), "\n")
cat("Tamaño conjunto prueba NRC multiclase:", nrow(test_data_nrc_multi), "\n")

## Preprocesamiento
text_recipe_afinn_bin_opt <- recipe(
  sentimiento_categoria ~ letra_limpia,
  data = train_data_afinn_bin
) %>%
  step_tokenize(letra_limpia) %>%
  step_stopwords(letra_limpia, language = "en") %>%
  step_stopwords(letra_limpia, language = "es") %>%
  step_stem(letra_limpia) %>%
  step_tokenfilter(letra_limpia, max_tokens = 200, min_times = 2) %>%
  step_tfidf(letra_limpia) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

text_recipe_nrc_bin_opt <- recipe(
  sentimiento ~ letra_limpia,
  data = train_data_nrc_bin
) %>%
  step_tokenize(letra_limpia) %>%
  step_stopwords(letra_limpia, language = "en") %>%
  step_stopwords(letra_limpia, language = "es") %>%
  step_stem(letra_limpia) %>%
  step_tokenfilter(letra_limpia, max_tokens = 200, min_times = 2) %>%
  step_tfidf(letra_limpia) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

text_recipe_afinn_multi <- recipe(
  sentimiento ~ letra_limpia,
  data = train_data_afinn_multi
) %>%
  step_tokenize(letra_limpia) %>%
  step_stopwords(letra_limpia, language = "en") %>%
  step_stopwords(letra_limpia, language = "es") %>%
  step_stem(letra_limpia) %>%
  step_tokenfilter(letra_limpia, max_tokens = 200, min_times = 2) %>%
  step_tfidf(letra_limpia) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

text_recipe_nrc_multi <- recipe(
  sentimiento ~ letra_limpia,
  data = train_data_nrc_multi
) %>%
  step_tokenize(letra_limpia) %>%
  step_stopwords(letra_limpia, language = "en") %>%
  step_stopwords(letra_limpia, language = "es") %>%
  step_stem(letra_limpia) %>%
  step_tokenfilter(letra_limpia, max_tokens = 200, min_times = 2) %>%
  step_tfidf(letra_limpia) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

## Especificción de modelos
nb_spec_opt <- naive_Bayes(smoothness = 1) %>%
  set_engine("naivebayes") %>%
  set_mode("classification")

svm_linear_spec_opt <- svm_linear(cost = 1) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

## Crear flujos de trabajo
workflows_list <- list(
  nb_afinn = workflow() %>%
    add_recipe(text_recipe_afinn_multi) %>%
    add_model(nb_spec_opt),

  svm_afinn = workflow() %>%
    add_recipe(text_recipe_afinn_bin_opt) %>%
    add_model(svm_linear_spec_opt),

  nb_nrc = workflow() %>%
    add_recipe(text_recipe_nrc_multi) %>%
    add_model(nb_spec_opt),

  svm_nrc = workflow() %>%
    add_recipe(text_recipe_nrc_bin_opt) %>%
    add_model(svm_linear_spec_opt)
)

## Procesar por lotes
results_list <- list()
splits_list <- list(
  afinn = data_split_afinn_bin,
  nrc = data_split_nrc_bin,
  afinn2 = data_split_afinn_bin,
  nrc2 = data_split_nrc_bin
)

## Crear objeto con métricas a caclcular
multi_metric <- metric_set(yardstick::accuracy, 
                           yardstick::precision, 
                           yardstick::sensitivity, 
                           yardstick::f_meas)

start_time <- Sys.time()

for (i in seq_along(workflows_list)) {
  model_name <- names(workflows_list)[i]
  cat(
    "Entrenando modelo ",
    i,
    "de",
    length(workflows_list),
    ":",
    model_name,
    "\n"
  )

  split_to_use <- if (grepl("afinn", model_name)) {
    splits_list$afinn
  } else {
    splits_list$nrc
  }

  tryCatch(
    {
      model_start <- Sys.time()

       results_list[[model_name]] <- last_fit(
      workflows_list[[i]], 
      split_to_use,
      metrics = multi_metric # Calcula metricas de comparación
    )

      model_time <- difftime(Sys.time(), model_start, units = "mins")
      cat("Completado", model_name, "en", round(model_time, 2), "minutos\n")
    },
    error = function(e) {
      cat("Error en modelo", model_name, ":", e$message, "\n")
      results_list[[model_name]] <- NULL
    }
  )
}

total_time <- difftime(Sys.time(), start_time, units = "mins")
cat("Tiempo total de entrenamiento:", round(total_time, 2), "minutos\n")

successful_models <- results_list[!sapply(results_list, is.null)]

if (length(successful_models) > 0) {
  comparison_results <- map_dfr(names(successful_models), function(model_name) {
    collect_metrics(successful_models[[model_name]]) %>%
      mutate(modelo = model_name)
  })

  comparison_summary <- comparison_results %>%
    select(modelo, .metric, .estimate) %>%
    pivot_wider(names_from = .metric, values_from = .estimate) %>%
    arrange(desc(accuracy))

  print(comparison_summary)

  ## Tomar el mejor modelo
  best_model_name <- comparison_summary$modelo[1]
  best_model <- successful_models[[best_model_name]]

  cat("\nMejor modelo:", best_model_name, "\n")
  cat("Métricas del mejor modelo:\n")
  collect_metrics(best_model) %>% print()

  # Guardar mejor modelo
  best_workflow <- workflows_list[[best_model_name]]

  # Aplicar mejor modelo
  if (grepl("afinn", best_model_name)) {
    final_model <- fit(best_workflow, train_data_afinn_bin)
  } else {
    final_model <- fit(best_workflow, train_data_nrc_bin)
  }

  saveRDS(final_model, "mejor_modelo.rds")
} else {
  cat(
    "Error al entrenar todos los modelos.\n"
  )
}

## Limpiar paralelización
if (exists("cl")) {
  stopCluster(cl)
  registerDoSEQ()
}

if (exists("final_model")) {
  test_prediction <- tibble(letra_limpia = "I love this song it makes me happy")
  pred_result <- predict(final_model, test_prediction)
  cat("\nQuick test prediction:", pred_result$.pred_class, "\n")
}
