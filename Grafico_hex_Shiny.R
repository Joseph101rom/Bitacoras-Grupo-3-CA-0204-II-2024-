
library(shiny)
library(ggplot2)
library(plotly)
library(scales)

ui <- fluidPage(
  titlePanel("Gráfico interactivo: Jugadores estimados vs Nota en Metacritic"),
  mainPanel(
    plotlyOutput("interactivePlot"),
    hr(),
    wellPanel(
      h4("Herramientas de configuración"),
      sliderInput("alpha", "Transparencia de los puntos:", 
                  min = 0.1, max = 1, value = 0.8, step = 0.1),
      p("Ajusta la transparencia de los puntos en el gráfico, útil para observar datos superpuestos."),
      sliderInput("jitter", "Nivel de dispersión (jitter):", 
                  min = 0, max = 2, value = 0.5, step = 0.1),
      p("Controla el nivel de dispersión horizontal de los puntos para evitar que se solapen en valores similares."),
      checkboxInput("showRegression", "Mostrar regresión lineal", value = TRUE),
      p("Activa o desactiva la línea de regresión lineal.")
    )
  )
)

server <- function(input, output) {
  output$interactivePlot <- renderPlotly({
    p <- ggplot(clean_database, aes(x = Metacritic.score, y = EstimatedPlayers)) +
      geom_point(alpha = input$alpha, color = "orange", 
                 position = position_jitter(width = input$jitter)) +
      scale_y_log10(labels = scales::number) +
      scale_x_continuous(breaks = seq(0, 100, by = 10)) +
      labs(
        title = "Dispersión de jugadores estimados y Metacritic", 
        x = "Nota en Metacritic", 
        y = "Jugadores estimados"
      ) +
      theme_minimal()
    if (input$showRegression) {
      p <- p + geom_smooth(method = "lm", se = FALSE, color = "green")
    }
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)
