library(shiny)
library(ggplot2)
library(plotly)
library(cowplot)

ui <- fluidPage(
  titlePanel("Videojuegos lanzados por año"),
  mainPanel(
    plotlyOutput("freqPlot"),
    hr(),
    wellPanel(
      h4("Personalización del gráfico"),
      sliderInput("binwidth", "Ancho de los intervalos (años):", 
                  min = 1, max = 5, value = 2.5, step = 0.5),
      sliderInput("alpha", "Transparencia de la línea:", 
                  min = 0.1, max = 1, value = 0.8, step = 0.1),
      sliderInput("size", "Tamaño de la línea:", 
                  min = 0.5, max = 2, value = 0.9, step = 0.1)
    )
  )
)

server <- function(input, output) {
  output$freqPlot <- renderPlotly({
    p <- ggplot(clean_database, aes(x = ReleaseDate)) +
      geom_freqpoly(
        alpha = input$alpha, 
        binwidth = input$binwidth, 
        color = "steelblue", 
        size = input$size
      ) +
      labs(
        x = "Año de lanzamiento", 
        y = "Cantidad de videojuegos", 
        title = "Cantidad de videojuegos lanzados por año"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(size = 12),
        legend.position = "none"
      )
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)
