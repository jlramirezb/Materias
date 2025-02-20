library(shiny)

ui <- fluidPage(
  titlePanel("Ubicación de Intervalos en la Recta Real"),
  sidebarLayout(
    sidebarPanel(
      numericInput("limite_izquierdo", "Límite Izquierdo:", value = 0),
      selectInput("tipo_izquierdo", "Tipo de Límite Izquierdo:",
                  choices = c("Cerrado" = "closed", "Abierto" = "open")),
      numericInput("limite_derecho", "Límite Derecho:", value = 1),
      selectInput("tipo_derecho", "Tipo de Límite Derecho:",
                  choices = c("Cerrado" = "closed", "Abierto" = "open")),
      textOutput("intervalo_notacion") # Add text output for interval notation
    ),
    mainPanel(
      plotOutput("recta_real")
    )
  )
)

server <- function(input, output) {
  output$recta_real <- renderPlot({
    lim_izq <- input$limite_izquierdo
    lim_der <- input$limite_derecho
    tipo_izq <- input$tipo_izquierdo
    tipo_der <- input$tipo_derecho
    # Asegurarse de que el límite izquierdo sea menor o igual que el derecho
    if (lim_izq > lim_der) {
      temp <- lim_izq
      lim_izq <- lim_der
      lim_der <- temp
    }
    # Crear el gráfico de la recta real
    plot(c(lim_izq - 1, lim_der + 1), c(0, 0), type = "n",
         xlab = "Eje Real", ylab = "", yaxt = "n")
    # Dibujar la recta real
    segments(lim_izq - 1, 0, lim_der + 1, 0, lwd = 2)
    # Marcar el intervalo
    if (tipo_izq == "closed") {
      points(lim_izq, 0, pch = 16, col = "red")
      # Filled circle for closed interval
    } else {
      points(lim_izq, 0, pch = 1, col = "red") # Open circle for open interval
    }
    if (tipo_der == "closed") {
      points(lim_der, 0, pch = 16, col = "red")
      # Filled circle for closed interval
    } else {
      points(lim_der, 0, pch = 1, col = "red") # Open circle for open interval
    }
    # Etiquetar los límites del intervalo
    text(lim_izq, -0.2, labels = lim_izq, col = "red")
    text(lim_der, -0.2, labels = lim_der, col = "red")
  })
  output$intervalo_notacion <- renderText({
    lim_izq <- input$limite_izquierdo
    lim_der <- input$limite_derecho
    tipo_izq <- input$tipo_izquierdo
    tipo_der <- input$tipo_derecho
    # Asegurarse de que el límite izquierdo sea menor o igual que el derecho
    if (lim_izq > lim_der) {
      temp <- lim_izq
      lim_izq <- lim_der
      lim_der <- temp
    }
    izq_bracket <- ifelse(tipo_izq == "closed", "[", "(")
    der_bracket <- ifelse(tipo_der == "closed", "]", ")")
    paste0("Intervalo: ", izq_bracket, lim_izq, ", ", lim_der, der_bracket)
  })
}

shinyApp(ui = ui, server = server)
