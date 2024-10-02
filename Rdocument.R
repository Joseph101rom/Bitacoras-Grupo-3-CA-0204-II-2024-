library(tidyverse)
library(broom)
library(cluster)
library(tidyr)
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
  pivot_longer(cols = c(Windows, Mac, Linux), names_to = "Plataform", values_to = "Supported") %>%
  filter(Supported == TRUE)

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

