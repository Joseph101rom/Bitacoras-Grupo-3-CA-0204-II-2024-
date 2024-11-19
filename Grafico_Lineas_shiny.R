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
      p("Ajusta el ancho de los intervalos en los que se agrupan los datos."),
      
      sliderInput("alpha", "Transparencia de la línea:", 
                  min = 0.1, max = 1, value = 0.8, step = 0.1),
      p("Controla la transparencia de la línea para hacerla más o menos visible."),
      
      sliderInput("size", "Tamaño de la línea:", 
                  min = 0.5, max = 2, value = 0.9, step = 0.1),
      p("Modifica el grosor de la línea para ajustar su apariencia.")
    )
  )
)

server <- function(input, output) {
  output$freqPlot <- renderPlotly({
    p <- ggplot(clean_database, aes(x = ReleaseDate)) +
      geom_freqpoly(alpha = input$alpha, binwidth = input$binwidth, 
                    color = "black", size = input$size) +
      labs(
        x = "Año de lanzamiento", 
        y = "Cantidad de videojuegos", 
        title = "Cantidad de videojuegos lanzados por año"
      ) +
      cowplot::theme_cowplot()
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)
