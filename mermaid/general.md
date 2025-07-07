graph TD
    subgraph "Recolección y almacenamiento"
        A[Dataset Kaggle: Billboard Hot 100: 1958-2017]
        B[billboard.com/charts/hot-100: 2017-2025]
        C(web-scraper.R)
        D[BigQuery: hot_100_historico]
        E[BigQuery: hot_100_historico_unico]

        A --> C
        B --> C
        C -- Crea/Actualiza --> D
        C -- Crea/Actualiza --> E
    end

    subgraph "Lee canciones únicas"
        F(genius-script.R)
        G[BigQuery: genius_urls]
        H(lyrics_script.R)
        I[BigQuery: letras_canciones_hot_100]

        F -- Escribe URLs --> G
        G -- Lee URLs --> H
        H -- Escribe letras --> I
    end

    subgraph "Análisis de sentimientos y modelado"
        J(analisis_sentimiento.R)
        K[BigQuery: léxicos - AFINN,<br/>Bing, NRC, Loughran-McDonals]
        L[Modelos ML entrenados y Métricas<br/>de Rendimiento]

        J -- Crea/Actualiza --> K
        J -- Produce --> L
    end

    subgraph "Análisis histórico"
        M(analisis_historico.R)
        N[Análisis de tendencias históricas<br/>de sentimiento]
        M -- Genera --> N
    end

    %% Conexiones entre subgrafos
    E -- Lee canciones únicas --> F
    I -- Lee letras para análisis --> J
    D -- Lee histórico --> M
    K -- Lee léxicos para análisis --> M