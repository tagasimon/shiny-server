library(shiny)
library(tidyverse)
library(data.table)
library(plotly)
library(gapminder)
library(colourpicker)
library(shinycssloaders)

# Define UI for application that draws a histogram
ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            textInput("title", "Title", "GDP vs life exp"),
            numericInput("size", "Point size", 1, 1),
            checkboxInput("fit", "Add line of best fit", FALSE),
            colourInput("color", "Choose a Color", value = "blue"),
            selectInput("continents", "Continents",
                        choices = levels(gapminder$continent),
                        multiple = TRUE,
                        selected = "Europe"),
            # Add a slider selector for years to filter
            sliderInput("years", "Years", 
                        min = min(gapminder$year), 
                        max = max(gapminder$year), 
                        value = c(1977, 2002))
        ),
        mainPanel(
            h1("Gapminder Dataset"),
            plotly::plotlyOutput("plot",height = "600") %>% withSpinner(color="#0dc5c1")
        )
    )
)

# Define the server logic
server <- function(input, output) {
    output$plot <- plotly::renderPlotly({
        # Subset the gapminder data by the chosen years
        data <- subset(gapminder,
                       continent %in% input$continents &
                           year >= input$years[1] & year <= input$years[2])
        
        p <- ggplot(data, aes(gdpPercap, lifeExp)) +
            geom_point(size = input$size, col = input$color) +
            scale_x_log10() +
            ggtitle(input$title)
        
        if (input$fit) {
            p <- p + geom_smooth(method = "lm")
        }
        p
    })
}

shinyApp(ui = ui, server = server)