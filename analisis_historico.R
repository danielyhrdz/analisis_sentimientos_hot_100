# Librerías ----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(tidyr)
library(zoo)

# Lectura desde BigQuery ---------------------------------------------------

bq_proyecto_id <- "tesis-2025-457202"
bq_dataset_id <- "sentiment_analysis_db"
bq_tabla_id_hist   <- "hot_100_historico"

bigrquery::bq_auth(path = "./sa_tesis.json")

conn <- DBI::dbConnect(
 bigrquery::bigquery(),
 project = bq_proyecto_id,
 dataset = bq_dataset_id,
 billing = bq_proyecto_id
) 

hot_100_historico <- tbl(conn, bq_tabla_id_hist) |>
  select(fecha = chart_date, posicion = , cancion = song, artista = performer) |> 
  collect()

## Se crea la columna año (anio) y se limpian los datos para crear una llave para cada canción
hot_100_historico <- hot_100_historico |> 
  mutate(anio = year(fecha),
            artista_llave = tolower(artista),
            artista_llave = str_replace_all(artista_llave, "[[:punct:]]", " "),
            artista_llave = str_replace_all(artista_llave, " ", ""),
            cancion_llave = tolower(cancion),
            cancion_llave = str_replace_all(cancion_llave, "[[:punct:]]", " "),
            cancion_llave = str_replace_all(cancion_llave, " ", ""),
            llave = paste(cancion_llave, artista_llave, sep = "_")
          ) |> 
  select(fecha, anio, cancion, artista, llave)


## Se unen los datos históricos con los sentimientos de AFINN.
### subir esto a bq y leerlo desde ahi

sentimiento_afinn <- read.csv("./analisis_sentimiento_afinn.csv")

## Se realiza la misma limpieza para la base de datos con los lexicos y palabras basados en AFINN
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

## Se unen los datos históricos con el lexico
datos_afinn_anual <- inner_join(hot_100_historico, sentimiento_afinn, 
                                  by = "llave", relationship = "many-to-many") |> 
  select(fecha, cancion = cancion.x, artista = artista.x, llave, palabra, sentimiento)


sentimiento_nrc <- read.csv("./analisis_sentimiento_nrc.csv")

## Se realiza la misma limpieza para la base de datos con los lexicos y palabras basados en NRC
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

## Se unen los datos históricos con el lexico
datos_nrc_anual <- inner_join(hot_100_historico, sentimiento_nrc, 
                                by = "llave", relationship = "many-to-many") |> 
select(fecha, cancion = cancion.x, artista = artista.x, llave, palabra, sentimiento)

### Verificamos la estructura de los datos unidos
glimpse(datos_afinn_anual)
glimpse(datos_nrc_anual)


# Análisis histórico con AFINN --------------------------------------------

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
  geom_line(aes(y = media_movil_5a, color = "Media Móvil (5 años)"), linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Promedio Anual" = "grey50", "Media Móvil (5 años)" = "Coral")) +
  labs(
    title = "Evolución del Sentimiento en la Música Popular (1958-2025)",
    subtitle = "Basado en el puntaje de valencia del léxico AFINN",
    x = "Año",
    y = "Puntaje de Sentimiento Promedio",
    caption = "Fuente: Elaboración propia con datos de Billboard Hot 100 y léxico AFINN."
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())


# Análisis histórico con NRC ---------------------------------------------

## Se calcula la proporción de cada emoción por año
tendencia_nrc <- datos_nrc_anual |> 
  mutate(anio = year(fecha)) |> 
  count(anio, sentimiento, name = "total_palabras_emocion") |> # Sentimientos más frecuentes por año
  group_by(anio) |> 
  mutate(total_anual = sum(total_palabras_emocion)) |> # Total canciones por año
  ungroup() |> 
  mutate(proporcion = total_palabras_emocion / total_anual)

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


# Analisis covid-19 ------------------------------------------------------------

# Definimos los periodos de interés
periodo_pre_pandemia <- 2017:2019
periodo_pandemia <- 2020:2022
periodo_post_pandemia <- 2023:2025

# Análisis con AFINN
analisis_covid_afinn <- tendencia_afinn |> 
  filter(anio >= 2017) |> 
  mutate(periodo = case_when(
    anio %in% periodo_pre_pandemia ~ "Pre-pandemia (2017-2019)",
    anio %in% periodo_pandemia     ~ "Pandemia (2020-2022)",
    anio %in% periodo_post_pandemia ~ "Post-pandemia (2023-2025)"
  )) |> 
  group_by(periodo) |> 
  summarise(sentimiento_promedio_periodo = mean(sentimiento_promedio))

# Análisis con NRC
analisis_covid_nrc <- tendencia_nrc |> 
  filter(anio >= 2017) |> 
  mutate(periodo = case_when(
    anio %in% periodo_pre_pandemia ~ " Pre-Pandemia (2017-2019)",
    anio %in% periodo_pandemia     ~ "Pandemia (2020-2022)",
    anio %in% periodo_post_pandemia ~ "Post-Pandemia (2023-2025)"
  )) |> 
  group_by(periodo, sentimiento) |> 
  summarise(proporcion_promedio_periodo = mean(proporcion)) |> 
  filter(sentimiento %in% emociones_a_graficar)

analisis_covid_nrc_pos_neg <- tendencia_nrc_pos_neg |> 
  filter(anio >= 2017) |> 
  mutate(periodo = case_when(
    anio %in% periodo_pre_pandemia ~ " Pre-Pandemia (2017-2019)",
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


ggplot(analisis_covid_nrc_pos_neg, aes(x = periodo, y = sentimiento_promedio_periodo, fill = periodo)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(sentimiento_promedio_periodo, 3)), vjust = -0.5) +
  labs(
    title = "Comparación del Sentimiento Promedio (AFINN)",
    subtitle = "Periodo Pre-Pandemia vs. Durante la Pandemia", #cambiar
    x = "Periodo",
    y = "Puntaje de Sentimiento Promedio"
  ) +
  theme_minimal()


# 6. ANÁLISIS DE TENDENCIAS EN CONTEXTO HISTÓRICO
# ------------------------------------------------

# Creamos un dataframe con eventos históricos clave para anotar en el gráfico.
eventos_historicos <- data.frame(
  anio = c(1968, 1989, 2001, 2008, 2020),
  evento = c("Pico Guerra\nde Vietnam", "Caída del Muro\nde Berlín", "Atentados\ndel 9-11", "Crisis\nFinanciera", "Inicio Pandemia\nCOVID-19"),
  y_pos = c(0.8, 0.9, 0.7, 0.85, 0.6) # Posición vertical para evitar solapamiento
)

# Reutilizamos el GRÁFICO 4.1 y le añadimos las anotaciones
ggplot(tendencia_afinn, aes(x = anio)) +
  geom_line(aes(y = sentimiento_promedio), color = "grey", alpha = 0.6) +
  geom_line(aes(y = media_movil_5a), color = "firebrick", linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  
  # Añadir líneas verticales para los eventos
  geom_vline(data = eventos_historicos, aes(xintercept = anio), linetype = "dotted", color = "navy") +
  
  # Añadir texto para los eventos
  geom_text(data = eventos_historicos, aes(x = anio, y = y_pos, label = evento),
            hjust = 0.5, vjust = 0, color = "navy", size = 3, angle = 0, fontface = "italic") +
  
  labs(
    title = "Tendencia de Sentimientos en la Música en Contexto Histórico",
    subtitle = "Media móvil de 5 años del puntaje AFINN superpuesta con eventos clave",
    x = "Año",
    y = "Puntaje de Sentimiento Promedio",
    caption = "Fuente: Elaboración propia con datos de Billboard Hot 100."
  ) +
  theme_minimal()

ggplot(tendencia_nrc_pos_neg, aes(x = anio)) +
  geom_line(aes(y = sentimiento_promedio), color = "grey", alpha = 0.6) +
  geom_line(aes(y = media_movil_5a), color = "firebrick", linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  
  # Añadir líneas verticales para los eventos
  geom_vline(data = eventos_historicos, aes(xintercept = anio), linetype = "dotted", color = "navy") +
  
  # Añadir texto para los eventos
  geom_text(data = eventos_historicos, aes(x = anio, y = y_pos, label = evento),
             color = "navy", size = 3, angle = 0, fontface = "italic") +
  labs(
    title = "Tendencia de Sentimientos en la Música en Contexto Histórico",
    subtitle = "Media móvil de 5 años del puntaje NRC superpuesta con eventos clave",
    x = "Año",
    y = "Puntaje de Sentimiento Promedio",
    caption = "Fuente: Elaboración propia con datos de Billboard Hot 100."
  ) +
  theme_minimal() 

ggplot(tendencia_afinn, aes(x = anio)) +
  geom_line(aes(y = sentimiento_promedio), color = "dodgerblue", alpha = 0.6, linewidth = 1.1) +
  
  labs(
    title = "Tendencia de sentimientos en la música popular (1958-2025)",
    subtitle = "Promedio anual basado en el léxico AFINN",
    x = "Año",
    y = "Puntaje de Sentimiento Promedio"
  ) +
  theme_minimal()

