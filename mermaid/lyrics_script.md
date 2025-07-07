graph TD
    A[Inicio] --> B[Cargar librerías R para web scraping y BigQuery];
    B --> C[Conectar a BigQuery];
    C --> D[Leer tabla genius_urls de BigQuery];
    D --> E[Filtrar URLs de letras ya raspadas];
    E --> F[Preparar lista de URLs pendientes para scraping];
    F --> G[Cargar funciones auxiliares de scraping: lyrics.R];
    G --> H[Iterar sobre las URLs pendientes en lotes de 50];
    H --> I[Llamar función raspar_letras_safe para extraer el texto de la letra];
    I -- Manejo de errores para scraping fallido --> J[Obtener texto de la letra o no lyric];
    J --> K[Escribir letras obtenidas a la tabla letras_canciones_hot_100 en BigQuery];
    K --> L[Cerrar conexión con BigQuery];
    L --> M[Fin];