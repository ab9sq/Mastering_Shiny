#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

freqpoly <- function(x1, x2, 
                     binwidth = 0.1,
                     xlim = c(-3, 3)){
    df <- data.frame(
        x = c(x1, x2),
        g = c(rep("x1", length(x1)),
              rep("x2", length(x2)))
    )
    
    ggplot(df, aes(x, color = g)) +
        geom_freqpoly(binwidth = binwidth,
                      size = 1) +
        coord_cartesian(xlim = xlim)
}


# Define UI for application that draws a histogram
ui <- fluidPage(
    fluidRow(
        column(3,
               numericInput("lambda1",
                            label = "lambda 1",
                            value = 3),
               numericInput("lambda2",
                            label = "lambda 2",
                            value = 5),
               numericInput("n",
                            label = "n",
                            value = 1e4,
                            min = 0)
               ),
        column(9,
               plotOutput("hist"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, server) {
    timer <- reactiveTimer(500)
    
    #x1 <- reactive(rpois(input$n, input$lambda1))
    x1 <- reactive({
        timer()
        rpois(input$n, input$lambda1)
    })
    #x2 <- reactive(rpois(input$n, input$lambda2))
    x2 <- reactive({
        timer()
        rpois(input$n, input$lambda2)
    })
    output$hist <- renderPlot({
        freqpoly(x1(), x2(),
                 binwidth = 1,
                 xlim = c(0, 40))
    }, res = 96)
}

# Run the application 
shinyApp(ui = ui, server = server)
