library(tidyverse)
library(broom)
library(cluster)
library(tidyr)
library(dplyr)
library(cowplot)
library(ggplot2)
library(scales)
library(hexbin)

#1.Simulacion de base de datos.
sim_data <- tibble(
  Title = paste("Game", 1:1000),
  Price = runif(1000, 0, 60), 
  Positive = sample(0:5000, 1000, replace = TRUE), 
  Negative = sample(0:2000, 1000, replace = TRUE),
  `User score` = runif(1000, 0, 10), 
  `Estimated players` = rpois(1000, lambda = 2000) 
)

#2. Regresion lineal
linear_model <- lm(`Estimated players` ~ `User score` + Price + Positive, data = sim_data)

model_summary <- summary(linear_model)

print(model_summary)

model_coefficients <- tidy(linear_model)

print(model_coefficients)

#3 Clustering con K-means
k <- 3

# Aplicar el algoritmo K-means
kmeans_result <- kmeans(select(sim_data, Price, Positive, `User score`), centers = k, nstart = 25)

sim_data <- sim_data %>%
  mutate(cluster = as.factor(kmeans_result$cluster))

# Visualizar los centroides de los clústeres
print(kmeans_result$centers)

# Gráfico de los clústeres
ggplot(sim_data, aes(x = Price, y = Positive, color = cluster)) +
  geom_point(alpha = 0.6) +
  labs(title = "Clustering de Videojuegos ", x = "Precio", y = "Reseñas Positivas") +
  theme_minimal()
 
#4 Ajuste de Parámetros y Validación: Podemos realizar un ajuste de parámetros para el modelo de regresión y para el algoritmo K-means para mejorar su rendimiento.

#4.1.Modelo de regresion lineal:Se puede ajustar utilizando diferentes combinaciones de variables predictoras y agregando interacciones o transformaciones logarítmicas.

sim_data <- sim_data %>%
  mutate(predicted_players = predict(linear_model, newdata = sim_data))

# Calcular R-cuadrado ajustado

r_squared <- model_summary$adj.r.squared

print(paste("R-cuadrado ajustado: ", round(r_squared, 3)))

#4.2 Clustering K-means:Probar con diferentes valores de k para observar cuál minimiza la distancia intracluster, así como considerar otros algoritmos de clustering

#Usaremos la métrica del coeficiente de Silhouette,la cual es una forma de medir qué tan bien se han agrupado los datos en un análisis de clústeres

# Calcular el coeficiente

silhouette_score <- silhouette(kmeans_result$cluster, dist(select(sim_data, Price, Positive, `User score`)))
avg_sil_score <- mean(silhouette_score[, 3])
print(paste("Coeficiente de Silhouette promedio: ", round(avg_sil_score, 3)))


#Graficos:
summary(clean_database)
#1
ggplot(clean_database, aes(x = ReleaseDate, y = EstimatedPlayers)) +
  geom_point(alpha = 0.9, color = "blue", size = 0.5) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", color = "white", alpha = 0.6) +
  scale_fill_viridis_c(option = "plasma", name = "Densidad de Jugadores") +  
  scale_y_log10(labels = comma) + 
  labs(
    title = "Distribución de Jugadores a lo Largo del Tiempo",
    x = "Año de Lanzamiento",
    y = "Cantidad Estimada de Jugadores"
  ) +
  theme_minimal() 
#2

playtime_platforms_long <- clean_database %>%
  select(Windows, Mac, Linux, Average.playtime.forever) %>%
  pivot_longer(cols = c(Windows, Mac, Linux), names_to = "Plataform", values_to = "Supported")
# Gráfico de violín con puntos dispersos para cada plataforma
ggplot(playtime_platforms_long, aes(x = Plataform, y = Average.playtime.forever, fill = Plataform)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_jitter(width = 0.2, alpha = 0.3, color = "black", size = 1) +
  scale_fill_manual(values = c("darkorange", "darkturquoise", "orchid4")) +
  labs(
    title = "Tiempo de Juego Promedio por Plataforma",
    x = "Plataforma",
    y = "Tiempo de Juego Promedio (horas)"
  ) +
  theme_classic(base_family = "Arial") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none",
    panel.grid.major = element_line(color = "gray85")
  )
# Cris
#1
library(ggplot2)
library(cowplot)
library(hexbin)
ggplot(clean_database, aes(x = Price, y = EstimatedPlayers)) +
  geom_hex(alpha = 0.5, size = 1, position = position_jitter(width = 0.1, height = 0.1)) +  # Cambié alpha a 0.5
  scale_y_log10(labels = scales::number) +
  scale_x_continuous(breaks = seq(0, 70, by = 10)) +
  labs(title = "Gráfico de dispersión entre el número de jugadores y el precio",
       x = "Precio",
       y = "Número de jugadores",
       fill = "Gradiente de densidad") +
  cowplot::theme_cowplot() +
  scale_fill_gradient(low = "blue", high = "red")  # Asegúrate de que no haya caracteres extraños
#2
ggplot(clean_database, aes(x = Metacritic.score, y = EstimatedPlayers)) +
  geom_point(alpha = 20, color = "black", position = position_jitter()) +
  scale_y_log10(labels = scales::number) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  labs(title = "Dispersión entre las variables jugadores estimados y nota en Metacritic", x = "Nota en Metacritic", y = "Jugadores estimados") +
  cowplot::theme_cowplot() +
  geom_smooth(method = "lm", se = FALSE,color = "blue")
#3
ggplot(clean_database, aes(x = clean_database$ReleaseDate)) +
  geom_freqpoly(alpha = 20, binwidth = 2.5, color = "black", size = 0.9) +
  labs(x = "Año de lanzamiento", y = "Cantidad de videojuegos", title = "Cantidad de videojuegos lanzados por año") +
  cowplot::theme_cowplot()

#Oscar:
# Cargar las librerías necesarias
library(dplyr)
library(kableExtra)

# Crear el data frame
tabla <- data.frame(
  Tipo = c("Teórico", "Metodológico", "Teórico", "Temático", "Metodológico", 
           "Metodológico", "Temático", "Metodológico", "Teórico", 
           "Metodológico", "Temático"),
  Tema_General = c("Videojuegos y cultura japonesa", "Adicción a videojuegos", 
                   "Diversión en videojuegos", "Videojuegos y política", 
                   "Industria de videojuegos", "Marketing de videojuegos", 
                   "Innovación en videojuegos", "Experiencia de juego", 
                   "Psicología del juego", "Análisis de comportamiento", 
                   "Juegos móviles y mercado"),
  Tema_Especifico = c("Análisis de éxito global de videojuegos japoneses", 
                      "Uso de videojuegos como estrategia de afrontamiento emocional", 
                      "Concepto de diversión a través del análisis de videojuegos", 
                      "Ludoficción política en Steam", 
                      "Análisis de la industria del videojuego en España", 
                      "Plan de marketing para empresas de videojuegos", 
                      "Innovación en productos y éxito en videojuegos", 
                      "Procesamiento predictivo y disfrute de la incertidumbre en videojuegos", 
                      "Experiencia óptima (flow)", 
                      "Análisis experimental del comportamiento", 
                      "Estudio de mercado sobre juegos para móviles y gaming"),
  Titulo = c("Manga, anime y videojuegos japoneses: análisis de los principales factores de su éxito global",
             "Problematic video game use as an emotional coping strategy",
             "¿Qué hace divertido un videojuego? Acercamiento al concepto de diversión",
             "LA POLÍTICA A LA QUE JUGAMOS. CULTURA, VIDEOJUEGOS Y LUDOFICCIÓN POLÍTICA EN STEAM",
             "Análisis de la industria del videojuego en España",
             "Plan de marketing para una empresa de videojuegos",
             "Innovate or game over? Examining effects of product innovativeness on video game success",
             "Mastering uncertainty: A predictive processing account of enjoying uncertain success in video game play",
             "Flow: The Psychology of Optimal Experience",
             "The Behavior of Organisms: An Experimental Analysis",
             "Estudio de mercado sobre juegos para móviles y gaming"),
  Año = c(2012, 2019, 2015, 2024, 2024, 2024, 2022, 2022, 1990, 1938, 2024),
  Autores = c("Hevia, Carme Mangiron", "Di Blasi, Maria", "Guerrero Pastor, Marta", 
              "STEAM, ON", "Autor Desconocido", "Autor Desconocido", 
              "Handrich, Franziska; Heidenreich, Sven; Kraemer, Tobias", 
              "Deterding, Sebastian; Andersen, Marc Malmdorf; Kiverstein, Julian", 
              "Csikszentmihalyi, Mihaly", "Skinner, B.F.", "We Are Testers")
)

# Crear la tabla en formato horizontal
tabla %>%
  kable("html", caption = "Tabla de Temas Relacionados con Videojuegos", align = 'c', escape = FALSE) %>%
  kable_styling("striped", full_width = TRUE, position = "center") %>%
  column_spec(1:5, width = "15em") %>% 
  kable_styling(latex_options = "striped", html_font = "Arial") %>%
  row_spec(0, bold = TRUE) %>% 
  footnote(general = 

