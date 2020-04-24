library(shiny)
library(tidyverse)
library(data.table)
library(plotly)
library(gapminder)
library(colourpicker)
library(shinycssloaders)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # shinythemes::themeSelector(),
    theme = shinythemes::shinytheme("lumen"),
    # Application title
    titlePanel("Vaccination Coverage Series"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("country", "Select a Country", 
                        choices = unique(country_codes$country), 
                        selected = c("Uganda"), 
                        multiple = TRUE),
            sliderInput("years", "Select a Year", 
                        min = 1981, 
                        max = 2018, 
                        step = 1, 
                        value = c(2005, 2018)),
            selectInput("vaccine", "select a Vaccine", 
                        choices = c("BCG" , "DTP1", "DTP3","DTP4","HepB_BD" ,"HepB3","Hib3","IPV1","MCV1","MCV2","PCV1","PCV2","PCV3","Pol3","Rota1","RotaC", "TT2plus","VAD1","DiphCV4","DiphCV5"   ,"DiphCV6","HepB_BDall" ,"Mumps","PerCV4" ,"Pol1","RCV1","TTCV4","TTCV5" ,"TTCV6","PAB","PAB_DTP1","HPVfem","VAD2","VADpp","YFV","HPVmale" ,"MenA","MMR","Rubpp","IPV1frac"  ,"IPV2frac","DTP2","VADother","JapEnc" ,"Typhoid","rotac" ), 
                        selected = c("VADpp", "BCG", "PCV3"), 
                        multiple = TRUE
            ), 
            downloadButton("download_csv", "Download Dataset") 
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Filtered Data", 
                         DT::DTOutput("table") %>% withSpinner()
                ),
                tabPanel("Analysis & Plots",
                         plotly::plotlyOutput("plot1") %>% withSpinner(), 
                         plotly::plotlyOutput("plot2") %>% withSpinner()
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    filt_data <- reactive({
        data <- readr::read_csv("./data/vaccination coverage_series.csv") %>%
            # data <- readr::read_csv("https://raw.githubusercontent.com/tagasimon/Upskill-Project/master/data/vaccination%20coverage_series.csv") %>%
            filter(
                Cname %in% input$country,
                Vaccine %in% input$vaccine,
                Year >= input$years[1] & Year <= input$years[2])
        data
        
    })
    output$table <- DT::renderDT({
        filt_data()
    })
    output$plot1 <- plotly::renderPlotly({
        filt_data() %>% 
            ggplot(aes(as.factor(Year), Percent_covrage, fill = Vaccine)) +
            geom_col(position = "dodge") + 
            coord_flip() + 
            facet_wrap(~Cname) + 
            labs(x = "", 
                 y = "Percentage Coverage", 
                 title = "Vaccine Coverage Comparison")
    })
    output$plot2 <- plotly::renderPlotly({
        filt_data() %>%
            ggplot(aes(Year, Percent_covrage, color = Vaccine)) +
            geom_line() +
            facet_wrap(~Cname) +
            labs(x = "",
                 y = "Percentage Coverage",
                 title = "Vaccine Coverage Comparison")
    })
    output$download_csv <- downloadHandler(
        filename = "data.csv", 
        content = function(file){
            write.csv(filt_data(), file)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)









# conditionalPanel(
#     # The condition should be that the user selects
#     # "file" from the radio buttons
#     condition = "input.source == 'file'",
#     fileInput("file", "Select a file")
# )
