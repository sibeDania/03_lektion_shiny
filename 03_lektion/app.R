library(shiny)
library(tidyverse)

#Skal ikke sættes når app'en skal deployes. Det er kun til lokal testning
#setwd("C:/Users/sibe/OneDrive - EaDania/Datavisualisering/2025/GitHub/03_lektion_shiny/03_lektion")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  title = "Datavisualisering",

    # Application title
    titlePanel("EA Dania 2025"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

# Logo --------------------------------------------------------------------

        
            div(img(height = 65, width = 80, src = "dania_logo.png"), # Her indsætter jeg et logo
              style = "text-align: center;"),
          

# Slider ------------------------------------------------------------------

          
            uiOutput("slider_ui")
        ),

# Main panel --------------------------------------------------------------


        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(tabPanel("Plot 1",
                      plotOutput("distPlot")),
                      tabPanel("Plot 2",
                               sliderInput("hej",
                                           "Test",
                                           min = 1,
                                           max = 10,
                                           step = 1,
                                           value = 5),
                               hr(),
                               plotOutput("ggplot")))
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$slider_ui <- renderUI({
    
    req(passat())
    
    passat <- passat()
    
    sliderInput("bins",
                "Number of bins:",
                min = min(passat$year, na.rm = TRUE),
                max = max(passat$year, na.rm = TRUE),
                value = 30)
    
  })
  

    output$distPlot <- renderPlot({
      
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
    
    
    output$ggplot <- renderPlot({
      
      #print(passat())
      
      passat <- readxl::read_excel("data/passat.xlsx")
      
      print(passat)
      
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white',
           xlab = 'Waiting time to next eruption (in mins)',
           main = 'Histogram of waiting times')
    })
    

# Reactive ----------------------------------------------------------------

    
    
    passat <- reactive({
      
      passat <- readxl::read_excel("data/passat.xlsx") %>% 
        filter(year >= 2014)
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
