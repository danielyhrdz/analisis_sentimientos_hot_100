# -----------------------------------------------------------------------------
# CAPÍTULO 4: ANÁLISIS HISTÓRICO DE SENTIMIENTOS
# -----------------------------------------------------------------------------
# Autor: [Tu Nombre]
# Fecha: 27 de junio de 2025
# Descripción: Script para el análisis de tendencias de sentimientos en las
#              canciones del Billboard Hot 100 desde 1958 a 2025.
# -----------------------------------------------------------------------------

# 1. CARGA DE LIBRERÍAS
# ---------------------
# Asegúrate de tener estas librerías instaladas: install.packages("nombre_libreria")
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr) # Para la manipulación de datos (pivot_wider)
library(zoo)   # Para medias móviles (rollmean)

# 2. PREPARACIÓN DE DATOS
# -----------------------
# Asumimos que los dataframes `hot_100_historico`, `sentimiento_afinn` y
# `sentimiento_nrc` ya están cargados en el entorno de R.

# Extraer el año de la fecha del chart para facilitar la agregación.
hot_100_historico <- hot_100_historico %>%
  mutate(year = year(chart_date))

# Unir los datos históricos con los sentimientos de AFINN.
# Nota: Usamos `hot_100_historico` en lugar de `hot_100_historico_unico` para
# que las canciones más exitosas (que aparecen más semanas) tengan más peso
# en el análisis anual.
datos_afinn_anual <- inner_join(hot_100_historico, sentimiento_afinn, by = "song_id")

# Unir los datos históricos con los sentimientos de NRC.
datos_nrc_anual <- inner_join(hot_100_historico, sentimiento_nrc, by = "song_id")

# Verificamos la estructura de los datos unidos (opcional)
# glimpse(datos_afinn_anual)
# glimpse(datos_nrc_anual)

# 3. ANÁLISIS DE TENDENCIAS CON AFINN
# ------------------------------------

# Agrupar por año y calcular el sentimiento AFINN promedio.
# Asumimos que la columna de score en `sentimiento_afinn` se llama 'value'.
# Si tiene otro nombre (ej. 'score'), ajústalo aquí.
tendencia_afinn <- datos_afinn_anual %>%
  group_by(year) %>%
  summarise(sentimiento_promedio = mean(value, na.rm = TRUE)) %>%
  ungroup()

# Añadir una media móvil de 5 años para suavizar la tendencia y observar patrones a largo plazo.
tendencia_afinn <- tendencia_afinn %>%
  mutate(media_movil_5a = zoo::rollmean(sentimiento_promedio, k = 5, fill = NA, align = "right"))

# GRÁFICO 4.1: Tendencia histórica del sentimiento (AFINN)
# --------------------------------------------------------
ggplot(tendencia_afinn, aes(x = year)) +
  geom_line(aes(y = sentimiento_promedio, color = "Promedio Anual"), alpha = 0.5) +
  geom_line(aes(y = media_movil_5a, color = "Media Móvil (5 años)"), linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Promedio Anual" = "grey50", "Media Móvil (5 años)" = "firebrick")) +
  labs(
    title = "Evolución del Sentimiento en la Música Popular (1958-2025)",
    subtitle = "Basado en el puntaje de valencia del léxico AFINN",
    x = "Año",
    y = "Puntaje de Sentimiento Promedio",
    caption = "Fuente: Elaboración propia con datos de Billboard Hot 100 y léxico AFINN."
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())

# 4. ANÁLISIS DE EMOCIONES CON NRC
# ---------------------------------
# Asumimos que `sentimiento_nrc` tiene un formato "tidy" con las columnas 'sentiment' y 'n'.
# Si el formato es ancho (una columna por emoción), se debe transformar primero.
# Aquí asumimos que el dataframe ya está procesado y contiene las emociones por canción.

# Calculamos la proporción de cada emoción por año.
tendencia_nrc <- datos_nrc_anual %>%
  # Contamos la frecuencia de cada tipo de sentimiento por año
  count(year, sentiment, name = "total_palabras_emocion") %>%
  # Calculamos el total de palabras con emoción por año
  group_by(year) %>%
  mutate(total_anual = sum(total_palabras_emocion)) %>%
  ungroup() %>%
  # Calculamos la proporción
  mutate(proporcion = total_palabras_emocion / total_anual)

# GRÁFICO 4.2: Evolución de emociones clave (NRC)
# -----------------------------------------------
# Filtramos para visualizar las emociones más contrastantes e informativas.
emociones_a_graficar <- c("joy", "sadness", "anger", "fear", "trust") # cambiar a que sean todas, no hace

ggplot(filter(tendencia_nrc, sentiment %in% emociones_a_graficar),
       aes(x = year, y = proporcion, color = sentiment)) +
  geom_line(alpha = 0.8, linewidth = 1) +
  geom_smooth(se = FALSE, method = "loess", span = 0.2, linewidth = 1.2, linetype = "dashed") +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(
    title = "Tendencia de Emociones Específicas en las Letras (1958-2025)",
    subtitle = "Proporción de palabras asociadas a cada emoción según el léxico NRC",
    x = "Año",
    y = "Proporción de Palabras",
    caption = "Fuente: Elaboración propia con datos de Billboard Hot 100 y léxico NRC."
  ) +
  theme_minimal() +
  theme(legend.position = "none") # El facet_wrap ya etiqueta cada gráfico

# 5. ANÁLISIS ESPECÍFICO: IMPACTO DE LA PANDEMIA DE COVID-19
# ----------------------------------------------------------

# Definimos los periodos de interés
periodo_pre_pandemia <- 2017:2019
periodo_pandemia <- 2020:2022

# Análisis con AFINN
analisis_covid_afinn <- tendencia_afinn %>%
  filter(year >= 2017 & year <= 2022) %>%
  mutate(periodo = case_when(
    year %in% periodo_pre_pandemia ~ "Pre-Pandemia (2017-2019)",
    year %in% periodo_pandemia     ~ "Pandemia (2020-2022)"
  )) %>%
  group_by(periodo) %>%
  summarise(sentimiento_promedio_periodo = mean(sentimiento_promedio))

# GRÁFICO 4.3: Comparación de sentimiento (AFINN) Pre y Durante la Pandemia
# --------------------------------------------------------------------------
ggplot(analisis_covid_afinn, aes(x = periodo, y = sentimiento_promedio_periodo, fill = periodo)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(sentimiento_promedio_periodo, 3)), vjust = -0.5) +
  labs(
    title = "Comparación del Sentimiento Promedio (AFINN)",
    subtitle = "Periodo Pre-Pandemia vs. Durante la Pandemia",
    x = "Periodo",
    y = "Puntaje de Sentimiento Promedio"
  ) +
  theme_minimal()

# Análisis con NRC
analisis_covid_nrc <- tendencia_nrc %>%
  filter(year >= 2017 & year <= 2022) %>%
  mutate(periodo = case_when(
    year %in% periodo_pre_pandemia ~ "Pre-Pandemia (2017-2019)",
    year %in% periodo_pandemia     ~ "Pandemia (2020-2022)"
  )) %>%
  group_by(periodo, sentiment) %>%
  summarise(proporcion_promedio_periodo = mean(proporcion)) %>%
  filter(sentiment %in% c("joy", "sadness", "fear", "anger", "anticipation"))

# GRÁFICO 4.4: Comparación de emociones (NRC) Pre y Durante la Pandemia
# ---------------------------------------------------------------------
ggplot(analisis_covid_nrc, aes(x = sentiment, y = proporcion_promedio_periodo, fill = periodo)) +
  geom_col(position = "dodge") +
  labs(
    title = "Comparación de Emociones Clave (NRC)",
    subtitle = "Periodo Pre-Pandemia vs. Durante la Pandemia",
    x = "Emoción",
    y = "Proporción Promedio de Palabras"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

# 6. ANÁLISIS DE TENDENCIAS EN CONTEXTO HISTÓRICO
# ------------------------------------------------

# Creamos un dataframe con eventos históricos clave para anotar en el gráfico.
eventos_historicos <- data.frame(
  year = c(1968, 1989, 2001, 2008, 2020),
  label = c("Pico Guerra\nde Vietnam", "Caída del Muro\nde Berlín", "Atentados\ndel 11-S", "Crisis\nFinanciera", "Inicio Pandemia\nCOVID-19"),
  y_pos = c(0.8, 0.9, 0.7, 0.85, 0.6) # Posición vertical para evitar solapamiento
)

# Reutilizamos el GRÁFICO 4.1 y le añadimos las anotaciones
ggplot(tendencia_afinn, aes(x = year)) +
  geom_line(aes(y = sentimiento_promedio), color = "grey", alpha = 0.6) +
  geom_line(aes(y = media_movil_5a), color = "firebrick", linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  
  # Añadir líneas verticales para los eventos
  geom_vline(data = eventos_historicos, aes(xintercept = year), linetype = "dotted", color = "navy") +
  
  # Añadir texto para los eventos
  geom_text(data = eventos_historicos, aes(x = year, y = y_pos, label = label),
            hjust = 0.5, vjust = 0, color = "navy", size = 3, angle = 0, fontface = "italic") +
  
  labs(
    title = "Tendencia de Sentimientos en la Música en Contexto Histórico",
    subtitle = "Media móvil de 5 años del puntaje AFINN superpuesta con eventos clave",
    x = "Año",
    y = "Puntaje de Sentimiento Promedio",
    caption = "Fuente: Elaboración propia con datos de Billboard Hot 100."
  ) +
  theme_minimal()