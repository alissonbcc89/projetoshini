library(shiny)
library(shinythemes)

# Define UI
ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage("Dashboard",
                           
                  tabPanel("Teste de Hipóteses",
                           titlePanel("Teste de Hipóteses"),
                           
                           sidebarPanel(
                             radioButtons("dados", "Escolher dados",
                                          c("Dados 1" = "d1",
                                            "Dados 2" = "d2",
                                            "Dados 3" = "d3")
                             ),
                             
                             radioButtons("tipo", "Escolha o tipo do teste",
                                          c("Bilateral" = "B",
                                            "Unilateral à Direita" = "UD",
                                            "Unilateral à Esquerda" = "UE")
                             ),
                             
                             numericInput(
                               inputId = "variancia",
                               label = "Variancia",
                               value = 0.0 ,
                               min  =  1 ,
                               max  =  10 ,
                             ),
                             
                             sliderInput("mu0", "Selecione mu0",
                                         min = 0, max = 30, value = 15
                             ),
                             
                             sliderInput("alfa", "Selecione alfa",
                                         min = 0.01, max = 0.10, value = 0.055
                             ),
                             
                           ), # sidebarPanel
                           mainPanel(
                             tableOutput('table')
                             
                           ) # mainPanel
                           
                  ), # tabPanel, teste de hipoteses
                  
                  tabPanel("Regressão",
                           titlePanel("Regressão"),
                  
                             # Show a plot of the generated distribution
                             mainPanel(
                               plotOutput("distPlot"),
                             )
                  )

                ) # navbarPage
) # fluidPage

server <- function(input, output) {
  tabela = (read.table("data.txt", header = TRUE, sep = ",", dec = "."))
  variavel_continua1 = (tabela$Altura)
  variavel_continua2 = (tabela$Peso)
  
  dados = reactive(input$dados)
  escolha_dados = renderText(dados())
  
  x = reactive({
    if(escolha_dados() == "d1"){
      c(2,5,13, 27, 21, 10, 9, 15)
    }else if(escolha_dados() == "d2"){
      c(15,2,13, 15, 12, 10, 5, 4)
    }else{
      c(1, 2, 13, 27, 7, 16, 5, 20)
    }
  })
  

  
  n = reactive(length(x()))
  xbarra = reactive(mean(x()))
  sig = reactive(sd(x()))
  sig_xbar = reactive(sig()/sqrt(n()))
  
  mu0 = reactive({
    as.integer(input$mu0)
  })
  
  alfa = reactive(as.numeric(input$alfa))
  
  tipo = reactive(input$tipo)
  teste = renderText(tipo())
  
  p = reactive({
    if(teste() == "B"){
      1 - alfa() + alfa()/2
    }else if(teste() == "UE"){
      alfa()
    }else{
      1-alfa()
    }
  })
  
  ztab = reactive(
    as.numeric(qnorm(p()))
  )
  
  zcalc = reactive(
    as.numeric((xbarra()-mu0())/sig_xbar())
  )
  
  output$table <- renderTable(
    if(zcalc() < ztab() & zcalc() > -ztab()){
      data.frame(Resultado = paste0('Aceitamos H0 ao nível de sig. = ', alfa()))
    }else{
      data.frame(Resultado = paste0('Rejeitamos H0 ao nível de sig. = ', alfa()))
    }
  )
  
#Grafico de dispersao
  output$distPlot <- renderPlot({
    
    data_frame = data.frame(variavel_continua1, variavel_continua2)

    regressao = lm(data_frame$variavel_continua2 ~ data_frame$variavel_continua1)
    
    plot(data_frame$variavel_continua1, data_frame$variavel_continua2,
         main = "Dispersão Altura X Peso",
         xlab = "Altura", ylab = "Peso", )
    abline(regressao)
    
  })
  
}
shinyApp(ui = ui, server = server)


