library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      radioButtons("choose_country", h3("Defina o teste de hipótese"),
                   choices=c('Bilateral'='B','Unilateral à Direita'='UD','Unilateral à Esquerda'='UE')),
      numericInput(
        inputId = "variancia",
        label = "Variancia",
        value = 0.0 ,
        min  =  1 ,
        max  =  10 ,
        step  =  NA ,
        width  =  NULL 
        
      ),
      numericInput(
        inputId = "v1",
        label = "Variavel continua 1 ",
        value = 0.0 ,
        min  =  1 ,
        max  =  10 ,
        step  =  NA ,
        width  =  NULL 
        
      ),
      numericInput(
        inputId = "v2",
        label = "Variavel continua 2",
        value = 0.0 ,
        min  =  1 ,
        max  =  10 ,
        step  =  NA ,
        width  =  NULL 
        
      ),
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Nivel de Confianca",
                  min = 1,
                  max = 3,
                  value = 0)
      
    ),
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
}
shinyApp(ui = ui, server = server)


