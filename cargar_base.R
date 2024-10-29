# Cargar las librerías necesarias
library(readxl)   # Para archivos de Excel
library(dplyr)
library(tidyverse)
library(broom)
library(ProjectTemplate)


file_path <- "clean_database.xlsx"  
if (file.exists(file_path)) {
 
  clean_database <<- read_excel(file_path)  
  

  message("Archivo cargado exitosamente.")
  print(head(clean_database))
} else {
 
  message("El archivo clean_database.xlsx no se encuentra en la ruta especificada.")
}

