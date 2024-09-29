library(readr)
library(dplyr)

datos_limpios <- read_csv("datos_limpios.csv")
head(datos_limpios)
summary(datos_limpios)
str(datos_limpios)

library(ggplot2)

ggplot(datos_limpios, aes(x = Price)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribución de Precios", x = "Precio", y = "Frecuencia")

ggplot(datos_limpios, aes(x = `Metacritic.score`, y = Positive)) +
  geom_point() +
  labs(title = "Reseñas Positivas vs. Puntuación de Usuarios", x = "Puntuación de Usuarios", y = "Reseñas Positivas")


datos_regresion <- datos_limpios %>%
  select(`Metacritic.score`, Price, Positive, `EstimatedPlayers`) %>%
  na.omit()

# Ajustar el modelo de regresión lineal
modelo_regresion <- lm(`EstimatedPlayers` ~ `Metacritic.score` + Price + Positive, data = datos_regresion)

# Resumen del modelo
summary(modelo_regresion)


# Dividir los datos
set.seed(123) # Para reproducibilidad
train_indices <- sample(1:nrow(datos_regresion), 0.8 * nrow(datos_regresion))
train_data <- datos_regresion[train_indices, ]
test_data <- datos_regresion[-train_indices, ]

# Ajustar el modelo con datos de entrenamiento
modelo_train <- lm(`EstimatedPlayers` ~ `Metacritic.score` + Price + Positive, data = train_data)

# Hacer predicciones
predicciones <- predict(modelo_train, newdata = test_data)

# Comparar predicciones con valores reales
resultados <- data.frame(Real = test_data$`EstimatedPlayers`, Predicho = predicciones)
head(resultados)