#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

injuries <- vroom::vroom("../../neiss/injuries.tsv.gz")
products <- vroom::vroom("../../neiss/products.tsv")
population <- vroom::vroom("../../neiss/population.tsv")

count_top <- function(df, var, n = 5) {
    df %>%
        mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
        group_by({{ var }}) %>%
        summarise(n = as.integer(sum(weight)))
}

prod_codes <- setNames(products$prod_code, products$title)

# Define UI for application that draws a histogram
ui <- fluidPage(
    fluidRow(
        column(6,
               selectInput("code", "Product", choices = prod_codes)
               )
    ),
    fluidRow(
        column(4, tableOutput("diag")),
        column(4, tableOutput("body_part")),
        column(4, tableOutput("location"))
    ),
    fluidRow(
        column(12, plotOutput("age_sex"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, server) {
    selected <- reactive(injuries %>%
                         filter(prod_code == input$code))
    
    output$diag <- renderTable(
        count_top(selected(), diag), width = "100%"
    )
    
    output$body_part <- renderTable(
        count_top(selected(), body_part), width = "100%"
    )
    
    output$location <- renderTable(
        count_top(selected(), location), width = "100%"
    )
    
    summary <- reactive({
        selected() %>%
            count(age, sex, wt = weight) %>%
            left_join(population, by = c("age", "sex")) %>%
            mutate(rate = n / population * 1e4)
    })
    
    output$age_sex <- renderPlot({
        summary() %>%
            ggplot(aes(age, n, color = sex)) +
            geom_line() +
            labs(y = "Estimated number of injuries")
    }, res = 96)
}









# Run the application 
shinyApp(ui = ui, server = server)
