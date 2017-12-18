library(shiny)
library(shinydashboard)
library(gridExtra)
library(ggplot2)
library(triangle)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    radioButtons("radio", label =NULL,
                 choices = list("Variable" = 1, "Cálculo" = 2), 
                 selected = 1, inline = T),
    actionButton('agregar','Agregar',icon = icon('plus')),
    #Agregar variable
    conditionalPanel(
      condition = "input.radio == '1'",
      textInput('nomd','Nombre de la distribución'),
      selectInput("select", label = "Distribución", 
                  choices = list( " " = 9,"Normal" = 'rnorm',
                                 "Normal Truncada" ='normt', 
                                 "Lognormal" = 'lognorm',
                                 "Binomial" = 'rbinom',
                                 "Gamma" = 'rgamma', 
                                 "Chi Cuadrada" = 'rchisq',
                                 "Poisson" = 'rpois',
                                 "Uniforme" = 'runif', 
                                 "Triangular" = 'rtriang'),
                  selected = 9),
      #Distribuciones----
      #normal
      conditionalPanel(
        condition = "input.select =='rnorm' || input.select =='normt' || input.select =='lognorm'",
        numericInput('mediaN','Media',0),
        numericInput('sigmaN','Varianza',1)
      ),
      #binomial
      conditionalPanel(
        condition = "input.select =='rbinom'",
        numericInput('nBin','Número de eventos',10),
        numericInput('pBin','Probabilidad',.5)
      ),
      #Gamma
      conditionalPanel(
        condition = "input.select =='rgamma'",
        numericInput('kGam','k',10),
        numericInput('lGam',HTML("&lambda;"),.5)
      ),
      #Chi cuadrada
      conditionalPanel(
        condition = "input.select =='rchisq'",
        numericInput('kChi','Grados de libertad',10)
      ),
      #poisson
      conditionalPanel(
        condition = "input.select =='rpois'",
        numericInput('lPoi',HTML("&lambda;"),.5)
      ),
      #uniforme
      conditionalPanel(
        condition = "input.select =='runif'",
        numericInput('aUni','Mínimo',0),
        numericInput('bUni','Máximo',10)
      ),
      #triangular
      #‘triangle'
      #rtriangle(n, a=0, b=1, c=(a+b)/2)
      conditionalPanel(
        condition = "input.select =='rtriang'",
        numericInput('aTri','Mínimo',0),
        numericInput('bTri','Máximo',10),
        numericInput('cTri','Moda',6)
      )
    ),
    #Agregar cálculo
    conditionalPanel(
      condition = "input.radio == '2'",
      textInput('nomf','Nombre del cálculo'),
      textInput('form','Fórmula')
    )
    

  ),
  dashboardBody(
    plotOutput('grafica')
  )
)

#server----
server <- function(input, output, session) {
  
  output$grafica <- renderPlot({
    
    df <- data.frame(x=curdata$variable1)
    
    ggplot(df,aes(x=x)) + geom_histogram(bins = 20) + ggtitle(input$nomd)
    
  })

  curdata <- reactiveValues()
  observeEvent(input$agregar,{
    
    curdata$variable1 <- switch(input$select,
             'rnorm' = rnorm(1000,input$mediaN,input$sigmaN),
             'normt' = rnorm(1000,input$mediaN,input$sigmaN),
             'lognorm' = rnorm(1000,input$mediaN,input$sigmaN),
             'rbinom' = rbinom(1000,input$nBin,input$pBin),
             'rgamma' = rgamma(1000,input$kGam,input$lGam),
             'rchisq' = rchisq(1000,input$kChi),
             'rpois' = rpois(1000,input$lPoi),
             'runif' = runif(1000,input$aUni,input$bUni),
             'rtriang' = rtriangle(1000,input$aTri,input$bTri,input$cTri),
             NULL)
      
  })
  
}

shinyApp(ui, server)