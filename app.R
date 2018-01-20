library(msm)
library(truncnorm)
library(shiny)
library(shinydashboard)
library(gridExtra)
library(ggplot2)
library(triangle)
library(dplyr)
library(plotly)
library(data.table)
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
                                        column(4,numericInput('iter','Número de iteraciones',
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
                                        column(12,htmlOutput('lista2')),
                                        column(12,htmlOutput('lista')),
                                        actionButton('eliminar','Eliminar',
                                                     icon = icon('trash')))
                        )),
                      fluidRow(
                        ###graficas----
                        htmlOutput('html2'),
                        br(),
                        htmlOutput('html')
                      ),
                      fluidRow(
                        br(),
                        infoBoxOutput('infoVar'),
                        infoBoxOutput('infoCal')
                        # column(12,tableOutput('tabla1'))
                      )
                    )
)

#server------------------------------------------------
server <- function(input, output, session) {
  
  curdata <- reactiveValues(matriz=list(),
                            matrizCalc=list())
  
  output$grafica <- renderPlotly({
    req(curdata$matriz)
    req(curdata$nombre)
    df <- as.data.frame(curdata$matriz)
    plist <-list()
    n<-1
    for (i in curdata$nombre){
      g <- plot_ly(df, x= df[,i], type = "histogram",name=i,
                   text = paste('Media:',round(mean(df[,i]),4),
                                '</br> Varianza:', round(sd(df[,i]),4),
                                '</br> Mediana:', round(median(df[,i]),4),
                                '</br> 3 Percentil:', round(quantile(df[,i],.75),4),
                                '</br> Iteraciones:', nrow(df))) %>% layout(
                                  font=f,title='Variables',xaxis=list(title=i,titlefont=f),autosize=T
                                )
      plist[[n]] <- g
      n <- n+1
    }
    
    subplot(plist,titleX = TRUE,which_layout=1)
  })
  
  output$grafica2 <- renderPlotly({
    
    req(curdata$variable2)
    req(curdata$matrizCalc)
    req(curdata$nomF)
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
    curdata$matrizCalc[[length(curdata$matrizCalc)+1]] <- curdata$variable2
    names(curdata$matrizCalc)<-curdata$nomF
    
  })
  
  observeEvent(input$agregar,{
    req(input$nomd)
    curdata$nombre <- c(curdata$nombre,input$nomd)
    curdata$variable1 <- switch(input$select,
                                'rnorm' = rnorm(input$iter,input$mediaN,input$sigmaN),
                                'normt' = rtnorm(input$iter,input$minT,
                                                 input$maxT,input$mediaT,
                                                 input$sigmaT),
                                'lognorm' = rlnorm(input$iter,input$mediaN,input$sigmaN),
                                'rbinom' = rbinom(input$iter,input$nBin,input$pBin),
                                'rgamma' = rgamma(input$iter,input$kGam,input$lGam),
                                'rchisq' = rchisq(input$iter,input$kChi),
                                'rpois' = rpois(input$iter,input$lPoi),
                                'runif' = runif(input$iter,input$aUni,input$bUni),
                                'rtriang' = rtriangle(input$iter,input$aTri,input$bTri,input$cTri),
                                NULL)
    curdata$matriz[[length(curdata$matriz)+1]] <- curdata$variable1
    names(curdata$matriz) <- curdata$nombre
    
    
  })
  
  # output$infoVar <- renderInfoBox({
  #   req(curdata$matriz)
  #   a<-t(colnames(curdata$matriz))
  #   infoBox('Variables',a,icon = icon('list'))
  # })
  # 
  # output$infoCal <- renderInfoBox({
  #   req(curdata$matrizCalc)
  #   a <- t(colnames(curdata$matrizCalc))
  #   infoBox('Cálculos',a,icon = icon('list'),color = 'red')
  # })
  
  observeEvent(input$eliminar,{
    req(curdata$matriz)
    if(input$eliminarVar!="" & is.null(input$eliminarCal)){
      numV <-which( names(curdata$matriz)==input$eliminarVar )
      curdata$matriz <- curdata$matriz[-numV]
      curdata$nombre <- curdata$nombre[-numV]
    }
    else if(input$eliminarVar!="" & input$eliminarCal!=""){
      numV <-which( names(curdata$matriz)==input$eliminarVar )
      numF <-which( names(curdata$matrizCalc)==input$eliminarCal )
      curdata$matriz <- curdata$matriz[-numV]
      curdata$nombre <- curdata$nombre[-numV]
      curdata$matrizCalc <- curdata$matrizCalc[-numF]
      curdata$nomF <- curdata$nomF[-numF]
    }
    else if(input$eliminarVar!="" & input$eliminarCal==""){
      numV <-which( names(curdata$matriz)==input$eliminarVar )
      curdata$matriz <- curdata$matriz[-numV]
      curdata$nombre <- curdata$nombre[-numV]
      
    }
    else if(input$eliminarVar=="" & input$eliminarCal!=""){
      numF <-which( names(curdata$matrizCalc)==input$eliminarCal )
      curdata$matrizCalc <- curdata$matrizCalc[-numF]
      curdata$nomF <- curdata$nomF[-numF]
    }
    else{
      n <-1
    }
  })
  
  output$lista <- renderUI({
    req(curdata$nombre)
    selectInput('eliminarVar', 'Eliminar Variable',c('',curdata$nombre))
  })
  
  output$lista2 <- renderUI({
    req(curdata$nomF)
    selectInput('eliminarCal', 'Eliminar Cálculo',c('',curdata$nomF))
  })
  
  output$html <- renderUI({
    req(curdata$matriz)
    req(curdata$nombre)
    plotlyOutput('grafica')
  })
  
  output$html2 <- renderUI({
    req(curdata$matrizCalc)
    req(curdata$nomF)
    plotlyOutput('grafica2')
  })
  
  # tablaData <- reactiveValues({
  #   req(curdata$nombre)
  #   as.data.frame(Variables=curdata$nombre,
  #                 Cálculos=curdata$nomF)
  #   })
  # output$tabla1 <- renderTable(
  #   as.data.frame(curdata$nombre) %>% setNames('Variables')
  #   )
}



shinyApp(ui, server)