library(viridis)
library(readxl)   
library(dplyr)
library(tidyverse)
library(broom)
library(ProjectTemplate)
library(RColorBrewer)
library(car)
library(forcats)
library(stringr)
library(ggplot2)
library(extrafont)
library(tidyverse)
library(broom)
library(cluster)
library(knitr)
library(tidyr)
library(cowplot)
library(ggplot2)
library(scales)
library(hexbin)
library(kableExtra)



file_path <- "clean_database.xlsx"  
if (file.exists(file_path)) {
 
  clean_database <<- read_excel(file_path)  
  

  message("Archivo cargado exitosamente.")
} else {
 
  message("El archivo clean_database.xlsx no se encuentra en la ruta especificada.")
}

avg_metacritic_by_developer <- read_excel("avg_metacritic_by_developer.xlsx")


clean_databaseForGenres <- read_excel("clean_databaseForGenres.xlsx")


developers_lumped <- read_excel("developers_lumped.xlsx")


genres_factoured <- read_excel("genres_factoured.xlsx")


mean_genres_per_developer <- read_excel("mean_genres_per_developer.xlsx")


ultimate_genres <- read_excel("ultimate_genres.xlsx")


mean_genres_per_genre <- read_excel("mean_genres_per_genre.xlsx")

