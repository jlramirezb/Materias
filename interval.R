library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Random Data Generator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("n",
                         "Number of data points:",
                         min = 1,
                         max = 1000,
                         value = 50)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        data <- rnorm(input$n)
        plot(data, type = "l", main = paste("Random Data (n =", input$n, ")"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
