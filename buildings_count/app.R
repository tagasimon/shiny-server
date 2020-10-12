library("EBImage")
library(shiny)
library(glue)
library(shinyjs)
library(shinycssloaders)
library(magrittr)
# options(repos = BiocManager::repositories())
# setRepositories(addURLs = c(BioC = "https://bioconductor.org/packages/3.8/bioc"))

# Define UI for data upload app ----
ui <- fluidPage(
    # App title ----
    useShinyjs(),
    titlePanel(Sys.Date()),
        uiOutput("website"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel(
            # Input: Select a file ----
            fileInput(
                "file1",
                "Step 1: Upload Google Map Screen Shot",
                multiple = FALSE,
                accept = c(".PNG", ".png", ".JPG", ".JPEG", "jpg", ".jpeg"),
                placeholder = "Google Maps Screen Shot",
                buttonLabel = "Browse..."
            ),
            
            # Horizontal line ----
            tags$hr(),
            sliderInput(
                "ImageofBuildingThreshol",
                label = "Adjust Threshold for better Results",
                min = 0.4,
                max = 0.8,
                step = 0.01,
                value = 0.61
            ),
            tags$hr(),
            actionButton(
                "calc",
                "Calculate",
                icon = icon("calculator"),
                class = "btn btn-success",
                width = "100%"
            ),
            tags$hr(),
            span(uiOutput("option1"), style = "color:orange"),
            displayOutput("imgg", width = "100%", height = "500px") %>%
                withSpinner()
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            titlePanel("Buildings in the Area from Google Maps Screen Shot"),
            tags$hr(),
            # textOutput("buildings_count"),
            uiOutput("count_l"),
            tags$hr(),
            span(uiOutput("processed"), style = "color:orange"),
            displayOutput("img_thres") %>%
                withSpinner(color = "purple"),
        ),
        
        
    ),
    tags$head(
        tags$style("#count_l{color: red;
                                 font-size: 30px;
                                 font-style: italic;
                                 }")
    ),
    
)

# Define server logic to read selected file ----
server <- function(input, output) {
    
    url <- a("MY BLOG", href="https://www.simonsayz.xyz/")
    output$website <- renderUI({
        tagList("Check Out", url)
    })
    ## This code removes the loading icon when the app starts
    start_loads <- function() {
        req(input$file1)
        tryCatch({
            shinyjs::showElement("calc")
            shinyjs::showElement("ImageofBuildingThreshol")
            city_image = readImage(input$file1$datapath)
            display(city_image)
        },
        error = function(e) {
            stop(safeError(e))
        })
        
    }
    
    
    output$imgg <- renderDisplay({
        start_loads()
    })
    
    output$img_thres <- renderDisplay({
        start_loads()
    })
    output$about <- renderText("www.simonsayz.xyz")
    
    calc_buildings <- function() {
        req(input$file1)
        tryCatch({
            city_image = readImage(input$file1$datapath)
            ImageofBuildingThreshol <- input$ImageofBuildingThreshol
            city_image_threshold <-
                city_image > ImageofBuildingThreshol
            building_count <- bwlabel(city_image_threshold)
            b_count_max <- max(building_count)
            output$img_thres <- renderDisplay({
                display(city_image_threshold)
            })
            output$count_l <- renderText({
                # shinyjs::disable("calc")
                glue::glue("Approximatly ~", {
                    b_count_max
                })
                # max(b_count_min)
                
            })
            
            output$option1 <- renderText("Uploaded Image")
            output$processed <- renderText("Processed Image")
            
            
        },
        error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        })
        
    }
    
    observeEvent(input$calc, {
        calc_buildings()
    })
    
    observeEvent(input$ImageofBuildingThreshol, {
        calc_buildings()
    })
    
    
}

# Create Shiny app ----
shinyApp(ui, server)