graph TD
    A[Inicio] --> B[Cargar librerías R];
    B --> C[Conectar a BigQuery];
    C --> D[Leer tabla hot_100_historico_unico de BigQuery];
    D --> E[Filtrar canciones cuyas URLs ya han sido obtenidas para evitar duplicados];
    E --> F[Preparar lista de canciones y artistas para búsqueda];
    F --> G[Iterar sobre las canciones en lotes de 50];
    G --> H[Llamar función get_genius_url para buscar URL en Genius API];
    H -- Manejo de errores para búsquedas fallidas --> I[Obtener URL de la letra de la canción];
    I --> J[Escribir URLs obtenidas a la tabla genius_urls en BigQuery por lotes];
    J --> K[Fin];