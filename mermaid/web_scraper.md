graph TD
    A[Inicio] --> B[Cargar librerías R];
    B --> C[Cargar funciones auxiliares: get_billboard_hot.R];
    C --> D[Definir rango de fechas para scraping web];
    D --> E[Iterar sobre fechas y ejecutar scraping del sitio web Billboard Hot 100];
    E -- Errores manejados con tryCatch --> F[Obtener datos de artistas y canciones por semana];
    F --> G[Unir todos los resultados en un dataframe temporal];
    G --> H[Escribir dataframe temporal a CSV: consulta_hot_100_completa.csv];
    H --> I[Leer dataset histórico de Kaggle: Hot Stuff.csv];
    I --> J[Unir datos de Kaggle con los datos recién raspados];
    J --> K[Crear un dataframe con canciones únicas: hot_100_unico];
    K --> L[Escribir el dataframe completo: historico_hot_100.csv];
    K --> M[Escribir el dataframe de únicos: historico_hot_100_unico.csv];
    L --> N[Escribir tabla hot_100_historico en BigQuery];
    M --> O[Escribir tabla hot_100_historico_unico en BigQuery];
    N & O --> P[Fin];