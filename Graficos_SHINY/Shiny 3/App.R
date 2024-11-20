library(shiny)
library(tidyverse)
library(readxl)
library(kableExtra)

# Cargar y preprocesar los datos
games_per_year <- read_xlsx("data/ultimate_genres.xlsx") %>%
  mutate(ReleaseDate = as.numeric(ReleaseDate)) %>%  # Asegúrate de convertir a numérico
  group_by(ReleaseDate, Genres) %>%
  summarise(Count = n(), .groups = 'drop')  # Calcular el conteo por año y género

# Interfaz de usuario
ui <- fluidPage(
  titlePanel("Filtrar Juegos por Año"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearRange", "Selecciona el rango de años:",
                  min = min(games_per_year$ReleaseDate),
                  max = max(games_per_year$ReleaseDate),
                  value = c(min(games_per_year$ReleaseDate), max(games_per_year$ReleaseDate)))
    ),
    
    mainPanel(
      htmlOutput("gamesTable")
    )
  )
)

# Lógica del servidor
server <- function(input, output) {
  
  filtered_data <- reactive({
    games_per_year %>%
      filter(ReleaseDate >= input$yearRange[1] & ReleaseDate <= input$yearRange[2])
  })
  
  output$gamesTable <- renderUI({
    if (nrow(filtered_data()) == 0) {
      return(h3("No hay datos disponibles para el rango seleccionado."))
    }
    
    table_html <- filtered_data() %>%
      pivot_wider(names_from = ReleaseDate, values_from = Count, values_fill = list(Count = 0)) %>%
      kbl("html", escape = FALSE) %>%
      kable_styling(full_width = FALSE, position = "left") %>%
      column_spec(1, bold = TRUE) %>%
      row_spec(0, bold = TRUE)
    
    HTML(table_html)
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)

