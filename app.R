library(msm)
library(truncnorm)
library(shiny)
library(shinydashboard)
library(gridExtra)
library(ggplot2)
library(triangle)
library(dplyr)
library(plotly)
f <- list(
  family = "Courier New, monospace",
  size = 13,
  color = "#7f7f7f ")

#ui----
ui <- dashboardPage(skin='green',
  dashboardHeader(),
  dashboardSidebar(disable = T),
  #Cuerpo----
  dashboardBody(
    fluidRow(
    tabBox(title = "Excel",
           id = "tabset1",width = 12,
           ###tab1-----
           tabPanel(title = 'Agregar Variable',
                    column(4,textInput('nomd','Nombre de la distribución')),
                    column(4,selectInput("select", label = "Distribución",
                                selectize = T, 
                                choices = list( " " = 9,"Normal" = 'rnorm',
                                                "Normal Truncada" ='normt', 
                                                "Lognormal" = 'lognorm',
                                                "Binomial" = 'rbinom',
                                                "Gamma" = 'rgamma', 
                                                "Chi Cuadrada" = 'rchisq',
                                                "Poisson" = 'rpois',
                                                "Uniforme" = 'runif', 
                                                "Triangular" = 'rtriang'),
                                selected = 9)),
                    column(4,numericInput('iter','Numero de iteraciones',
                                          1000,1,1000000,100)),
                    #Distribuciones----
                    #normal
                    conditionalPanel(
                      condition = "input.select =='rnorm' || input.select =='lognorm'",
                      column(6,numericInput('mediaN','Media',0)),
                      column(6,numericInput('sigmaN','Varianza',1))
                    ),
                    #normal truncada
                    conditionalPanel(
                      condition = "input.select =='normt'",
                      column(3,numericInput('mediaT','Media',0)),
                      column(3,numericInput('sigmaT','Varianza',1)),
                      column(3,numericInput('minT','Mínimo',-1)),
                      column(3,numericInput('maxT','Máximo',1))
                      
                    ),
                    #binomial
                    conditionalPanel(
                      condition = "input.select =='rbinom'",
                      column(6,numericInput('nBin','Número de eventos',10)),
                      column(6,numericInput('pBin','Probabilidad',.5))
                    ),
                    #Gamma
                    conditionalPanel(
                      condition = "input.select =='rgamma'",
                      column(6,numericInput('kGam','k',10)),
                      column(6,numericInput('lGam',HTML("&lambda;"),.5))
                    ),
                    #Chi cuadrada
                    conditionalPanel(
                      condition = "input.select =='rchisq'",
                      column(12,numericInput('kChi','Grados de libertad',10))
                    ),
                    #poisson
                    conditionalPanel(
                      condition = "input.select =='rpois'",
                      column(12,numericInput('lPoi',HTML("&lambda;"),.5))
                    ),
                    #uniforme
                    conditionalPanel(
                      condition = "input.select =='runif'",
                      column(6,numericInput('aUni','Mínimo',0)),
                      column(6,numericInput('bUni','Máximo',10))
                    ),
                    #triangular
                    #‘triangle'
                    #rtriangle(n, a=0, b=1, c=(a+b)/2)
                    conditionalPanel(
                      condition = "input.select =='rtriang'",
                      column(4,numericInput('aTri','Mínimo',0)),
                      column(4,numericInput('bTri','Máximo',10)),
                      column(4,numericInput('cTri','Moda',6))
                    ),
                    #fin distribuciones----
                    actionButton('agregar','Agregar',icon = icon('plus'))
           )
           
           ,
           #tab2----
           tabPanel(title = 'Agregar Cálculo',
                    #Agregar cálculo----
                    column(6,textInput('nomf','Nombre del cálculo')),
                    column(6,textInput('form','Fórmula')),
                    actionButton('agg','Agregar',icon = icon('plus'))
           ),
           tabPanel('Eliminar',
                    actionButton('eliminar','Eliminar',icon = icon('trash')))
    )),
    fluidRow(
    ###graficas----
    column(6,plotlyOutput('grafica')),
    column(6,plotlyOutput('grafica2')),
    verbatimTextOutput('texto')
    ),
    fluidRow(
      br(),
      infoBoxOutput('infoVar'),
      infoBoxOutput('infoCal')
    )
  )
)

#server----
server <- function(input, output, session) {
  curdata <- reactiveValues(matriz=matrix(ncol=0,nrow = 10000),
                            matrizCalc=matrix(ncol=0,nrow = 10000) )
  
  output$grafica <- renderPlotly({
    
    req(curdata$variable1)
    df <- as.data.frame(curdata$matriz)
    plist <-list()
    n<-1
    for (i in curdata$nombre){
      g <- plot_ly(df, x= df[,i], type = "histogram",name=i,
                   text = paste('Media:',round(mean(df[,i]),4),
                                 '</br> Varianza:', round(sd(df[,i]),4))) %>% layout(
        font=f,title='Variables',xaxis=list(title=i,titlefont=f),autosize=T
      )
      plist[[n]] <- g
      n <- n+1
    }
    
    subplot(plist,titleX = TRUE,which_layout=1)
    
  })
  
  output$grafica2 <- renderPlotly({

    req(curdata$variable2)
    df <- as.data.frame(curdata$matrizCalc)
    plist <-list()
    n<-1
    for (i in curdata$nomF){
      print(n)
      g <- plot_ly(df, x= df[,i], type = "histogram",name=i,
                   text = paste('Media:',round(mean(df[,i]),4),
                                 '</br> Varianza:', round(sd(df[,i]),4))) %>% layout(
        font=f,title='Cálculos',xaxis=list(title=i,titlefont=f)
      )
      plist[[n]] <- g
      n <- n+1
    }
    
    subplot(plist,titleX = T,which_layout=1)
    
  })
  
  observeEvent(input$agg,{
    
    req(input$nomf)
    curdata$nomF <- c(curdata$nomF,input$nomf)
    df <- as.data.frame(curdata$matriz)
    curdata$variable2=eval(parse(text=input$form),envir = df)
    curdata$matrizCalc <- cbind(curdata$matrizCalc,curdata$variable2)
    colnames(curdata$matrizCalc)<-curdata$nomF
    
  })
  
  observeEvent(input$agregar,{
    req(input$nomd)
    curdata$nombre <- c(curdata$nombre,input$nomd)
    curdata$variable1 <- switch(input$select,
                                'rnorm' = rnorm(10000,input$mediaN,input$sigmaN),
                                'normt' = rtnorm(10000,input$minT,
                                                       input$maxT,input$mediaT,
                                                       input$sigmaT),
                                'lognorm' = rlnorm(10000,input$mediaN,input$sigmaN),
                                'rbinom' = rbinom(10000,input$nBin,input$pBin),
                                'rgamma' = rgamma(10000,input$kGam,input$lGam),
                                'rchisq' = rchisq(10000,input$kChi),
                                'rpois' = rpois(10000,input$lPoi),
                                'runif' = runif(10000,input$aUni,input$bUni),
                                'rtriang' = rtriangle(10000,input$aTri,input$bTri,input$cTri),
                                NULL)
    curdata$matriz <- cbind(curdata$matriz,curdata$variable1)
    colnames(curdata$matriz)<-curdata$nombre
    
  })
  
  output$texto <- renderText({
  })
  
  output$infoVar <- renderInfoBox({
    req(curdata$matriz)
    a<-t(colnames(curdata$matriz))
    infoBox('Variables',a,icon = icon('list'))
  })
  
  output$infoCal <- renderInfoBox({
    req(curdata$matrizCalc)
    a <- t(colnames(curdata$matrizCalc))
    infoBox('Cálculos',a,icon = icon('list'),color = 'red')
  })
}

shinyApp(ui, server)