graph TD
    A[Inicio] --> B[Cargar librerías para manipulación y visualización de datos];
    B --> C[Conectar a BigQuery];
    C --> D[Leer tabla hot_100_historico de BigQuery];
    D --> E[Leer léxicos de sentimiento AFINN y NRC desde BigQuery];
    E --> F[Normalizar nombres de artista/canción para uniones];
    F --> G[Unir datos históricos con sentimientos AFINN];
    F --> H[Unir datos históricos con sentimientos NRC];
    G --> I[Calcular sentimiento promedio anual y media móvil a 5 años con AFINN];
    I --> J[Graficar la evolución del sentimiento histórico con AFINN];
    J --> K[Definir períodos de interés: pre-pandemia, pandemia, post-pandemia];
    K --> L[Calcular y comparar sentimiento promedio por período con AFINN];
    H --> M[Calcular y comparar proporción de emociones clave por período con NRC];
    L & M --> N[Interpretar resultados y buscar correlaciones con eventos históricos];
    N --> O[Fin];
