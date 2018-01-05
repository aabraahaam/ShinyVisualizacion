library(shiny)
library(shinydashboard)
library(gridExtra)
library(ggplot2)
library(triangle)
library(dbplyr)
library(dplyr)
#ui----
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    radioButtons("radio", label =NULL,
                 choices = list("Variable" = 1, "Cálculo" = 2), 
                 selected = 1, inline = T),
    actionButton('agregar','Agregar',icon = icon('plus')),
    actionButton('agg','Agg',icon = icon('plus')),
    #Agregar variable
    conditionalPanel(
      condition = "input.radio == '1'",
      textInput('nomd','Nombre de la distribución'),
      selectInput("select", label = "Distribución",selectize = T, 
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
    #Agregar cálculo----
    conditionalPanel(
      condition = "input.radio == '2'",
      textInput('nomf','Nombre del cálculo'),
      textInput('form','Fórmula')
    )
    

  ),
  #Cuerpo----
  dashboardBody(
    plotOutput('grafica'),
    plotOutput('grafica2'),
    verbatimTextOutput('texto')
  )
)

#server----
server <- function(input, output, session) {
  
  curdata <- reactiveValues(matriz=matrix(ncol=0,nrow = 1000),
                            matrizCalc=matrix(ncol=0,nrow = 1000) )
  
  output$grafica <- renderPlot({
    
    req(curdata$variable1)
    df <- as.data.frame(curdata$matriz)
    plist <-list()
    n<-1
     for (i in curdata$nombre){
       print(n)
       g <-ggplot(df,aes_string(x=i)) + geom_histogram(bins = 20) + ggtitle(i)
       plist[[n]] <- g
       n <- n+1
     }

     grid.arrange(grobs=plist)
    
  })
  
  output$grafica2 <- renderPlot({
    
    req(curdata$variable2)
    df <- as.data.frame(curdata$matrizCalc)
    plist <-list()
    n<-1
    for (i in curdata$nomF){
      print(n)
      g <-ggplot(df,aes_string(x=i)) + geom_histogram(bins = 20) + ggtitle(i)
      plist[[n]] <- g
      n <- n+1
    }
    
    grid.arrange(grobs=plist)
  })

  observeEvent(input$agg,{
    curdata$nomF <- c(curdata$nomF,input$nomf)
    df <- as.data.frame(curdata$matriz)
    curdata$variable2=eval(parse(text=input$form),envir = df)
    curdata$matrizCalc <- cbind(curdata$matrizCalc,curdata$variable2)
    colnames(curdata$matrizCalc)<-curdata$nomF
    
  })
  
  observeEvent(input$agregar,{
    curdata$nombre <- c(curdata$nombre,input$nomd)
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
    curdata$matriz <- cbind(curdata$matriz,curdata$variable1)
    colnames(curdata$matriz)<-curdata$nombre
    
  })
  
  output$texto <- renderText({
    # req(curdata$nombre)
    # req(input$form)
    # curdata$nombre
    # input$agregar
    # df <- as.data.frame(curdata$matriz)
    # eval(parse(text=input$form),envir = df)
    })
}

shinyApp(ui, server)