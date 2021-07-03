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

t_test <- function(x1, x2){
    test <- t.test(x1, x2)
    
    #use sprintf() to format t.test() resukts
    sprintf(
        "p value; %0.3f\n[%0.2f, %0.2f]",
        test$p.value, test$conf.int[1], test$conf.int[2]
    )
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    fluidRow(
        column(4,
               "Distribution 1",
               numericInput("n1",
                            label = "n",
                            value = 1000,
                            min = 1),
               numericInput("mean1",
                            label = "u",
                            value = 0,
                            step = 0.1),
               numericInput("sd1",
                            label = "o",
                            value = 0.5,
                            min = 0.1,
                            step = 0.1)
               ),
        column(4,
               "Distribution 2",
               numericInput("n2",
                            label = "n",
                            value = 1000,
                            min = 1),
               numericInput("mean2",
                            label = "u",
                            value = 0,
                            step = 0.1),
               numericInput("sd2",
                            label = "o",
                            value = 0.5,
                            min = 0.1,
                            step = 0.1)
        ),
        column(4,
               "Frequency Polygon",
               numericInput("binwidth",
                            label = "Bin width",
                            value = 0.1,
                            step = 0.1),
               sliderInput("range",
                            label = "Range",
                            value = c(-3, 3),
                            min = -5,
                            max = 5)
               )
    ),
    fluidRow(
        column(9,
               plotOutput("hist")),
        column(3,
               verbatimTextOutput("ttest"))
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output, server) {
    x1 <- reactive(rnorm(input$n1, input$mean1, input$sd1))
    x2 <- reactive(rnorm(input$n2, input$mean2, input$sd2))
    output$hist <- renderPlot({
        freqpoly(x1(), x2(),
                 binwidth = input$binwidth,
                 xlim = input$range)
    }, res = 96)
    
    output$ttest <- renderText({
        t_test(x1(), x2())
    })
  }

# Run the application 
shinyApp(ui = ui, server = server)
