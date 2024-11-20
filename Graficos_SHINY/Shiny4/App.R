library(shiny)
library(dplyr)
library(ggplot2)
library(cowplot)
library(readxl)

# Cargar datos
games_per_year <- read_excel("data/ultimate_genres.xlsx")

# Preprocesamiento de datos
games_per_year <- games_per_year %>%
  group_by(ReleaseDate, Genres) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Genres = ifelse(is.na(Genres), "Others", Genres))

# Asignación de colores a los géneros
genre_colors <- scales::hue_pal()(length(unique(games_per_year$Genres)))

color_mapping <- data.frame(
  Genres = unique(games_per_year$Genres),
  Color = genre_colors
)

# Interfaz de usuario
ui <- fluidPage(
  titlePanel("Juegos Lanzados por Año según Género"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_genres", 
                  label = "Seleccionar Géneros:", 
                  choices = unique(games_per_year$Genres), 
                  selected = unique(games_per_year$Genres)[1], 
                  multiple = TRUE), 
      
      helpText("Puede seleccionar más de un género a la vez y para deseleccionarlo solo debe borrar el nombre de la barra de búsqueda."),
      
      sliderInput("year_range", 
                  label = "Seleccionar Rango de Años:", 
                  min = min(games_per_year$ReleaseDate), 
                  max = max(games_per_year$ReleaseDate), 
                  value = c(min(games_per_year$ReleaseDate), max(games_per_year$ReleaseDate)), 
                  step = 1,
                  sep = ""),
      
      helpText("Si no se muestra nada en el período escogido, es porque no se lanzaron videojuegos en ese período para los géneros seleccionados.")
    ),
    
    mainPanel(
      plotOutput("genrePlot")
    )
  )
)

# Lógica del servidor
server <- function(input, output) {
  
  output$genrePlot <- renderPlot({
    # Filtrar los datos según el rango de años y géneros seleccionados
    filtered_data <- games_per_year %>%
      filter(Genres %in% input$selected_genres,
             ReleaseDate >= input$year_range[1],
             ReleaseDate <= input$year_range[2])
    
    # Extraer las últimas posiciones para las etiquetas de los géneros
    last_positions <- filtered_data %>%
      group_by(Genres) %>%
      filter(ReleaseDate == max(ReleaseDate)) %>%
      select(ReleaseDate, Count, Genres)
    
    # Crear el gráfico
    ggplot(filtered_data, aes(x = ReleaseDate, y = Count, color = Genres)) +
      geom_line(size = 2) +
      geom_label(data = last_positions, aes(label = Genres), vjust = -0.5, nudge_y = 0.5) +
      labs(
        title = paste("Número de Juegos Lanzados en los Géneros:", paste(input$selected_genres, collapse = ", "),
                      "\nDesde", input$year_range[1], "hasta", input$year_range[2]),
        x = "Año", 
        y = "Número de Juegos"
      ) +
      scale_x_continuous(breaks = seq(input$year_range[1], input$year_range[2], by = 1)) +
      scale_color_manual(values = color_mapping$Color[color_mapping$Genres %in% input$selected_genres]) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = -2),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
