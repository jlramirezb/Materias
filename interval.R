# interval.R

library(shiny)
library(ggplot2)
# Suppress warnings for variables used in ggplot2 aesthetics
utils::globalVariables(c("x", "y", "lower", "upper"))

ui <- fluidPage(
  titlePanel("Visualizador de Intervalos"),

  sidebarLayout(
    sidebarPanel(
      numericInput("lower_bound", "Extremo Inferior:", value = 0),
      checkboxInput("lower_closed", "Cerrado por la izquierda"),
      numericInput("upper_bound", "Extremo Superior:", value = 10),
      checkboxInput("upper_closed", "Cerrado por la derecha"),
      colourInput("point_color", "Color de los puntos:", value = "red"),
      numericInput("line_width", "Grosor de la línea:", value = 1, min = 0.1, step = 0.1)
    ),

    mainPanel(
      plotOutput("interval_plot"),
      h4("Representación Algebraica:"),
      verbatimTextOutput("algebraic_representation")
    )
  )
)

server <- function(input, output) {

  output$interval_plot <- renderPlot({
    lower <- input$lower_bound
    upper <- input$upper_bound
    lower_closed <- input$lower_closed
    upper_closed <- input$upper_closed
    p <- ggplot() +
      geom_hline(yintercept = 0, linewidth = 1) +
      xlim(lower - abs(lower) * 0.1, upper + abs(upper) * 0.1) +
      ylim(-0.5, 0.5) +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_blank())

    # Agregar el intervalo
    interval_data <- data.frame(x = c(lower, upper), y = c(0, 0))
    p <- p + geom_line(data = interval_data, linewidth = input$line_width, color = "blue")

    # Agregar puntos para indicar cerrado o abierto
    if (lower_closed) {
      p <- p + geom_point(aes(x = lower, y = 0), size = 5, color = input$point_color)
    } else {
      p <- p + geom_point(aes(x = lower, y = 0), size = 5, shape = 1, color = input$point_color)
    }

    if (upper_closed) {
      p <- p + geom_point(aes(x = upper, y = 0), size = 5, color = input$point_color)
    } else {
      p <- p + geom_point(aes(x = upper, y = 0), size = 5, shape = 1, color = input$point_color)
    }

    p
  })

  output$algebraic_representation <- renderText({
    lower <- input$lower_bound
    upper <- input$upper_bound

    # Validate that lower_bound is less than or equal to upper_bound
    lower_symbol <- ifelse(lower_closed, "[", "(")
    upper_symbol <- ifelse(upper_closed, "]", ")")
    
    paste0(lower_symbol, lower, ", ", upper, upper_symbol)
    # based on whether it is closed or open
    upper_symbol <- ifelse(upper_closed, "]", ")")
    
    paste0(lower_symbol, " ", lower, " , ", upper, " ", upper_symbol)

    paste0(lower_symbol, " ", lower, ", ", upper, " ", upper_symbol)
  })
}