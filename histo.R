library(shiny)
library(shinythemes)
data(airquality)


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  theme = shinytheme("cerulean"),
  
  tabPanel("Navbar 1",
           sidebarPanel(
             tags$h3("Input:"),
             textInput("txt1", "Given Name:", ""),
             textInput("txt2", "Surname:", ""),
             
           ),
           
           
           # sidebarPanel
           mainPanel(
             h1("Header 1"),
             
             h4("Output 1"),
             verbatimTextOutput("txtout"),
             
           )
           
           
           ),
  
  
  # App title ----
  titlePanel("Ozone level!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
      
                  
      ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  output$distPlot <- renderPlot({
    
    x    <- airquality$Ozone
    x    <- na.omit(x)
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "black",
         xlab = "Ozone level",
         main = "Histogram of Ozone level")
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)