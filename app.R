library(shiny)
library(shinydashboard)
library(gridExtra)
library(ggplot2)

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
                  choices = list( " " = 9,"Normal" = 1, "Normal Truncada" = 2, "Lognormal" = 3,
                                 "Binomial" = 4, "Gamma" = 5, "Chi Cuadrada" = 6,
                                 "Poisson" = 7, "Uniforme" = 8, "Triangular" = 10),
                  selected = 9),
      #normal
      conditionalPanel(
        condition = "input.select =='1' || input.select =='2' || input.select =='3'",
        numericInput('mediaN','Media',0),
        numericInput('sigmaN','Varianza',1)
      ),
      #binomial
      conditionalPanel(
        condition = "input.select =='4'",
        numericInput('nBin','Número de eventos',10),
        numericInput('pBin','Probabilidad',.5)
      ),
      #Gamma
      conditionalPanel(
        condition = "input.select =='5'",
        numericInput('kGam','k',10),
        numericInput('lGam',HTML("&lambda;"),.5)
      ),
      #Chi cuadrada
      conditionalPanel(
        condition = "input.select =='6'",
        numericInput('kChi','Grados de libertad',10)
      ),
      #poisson
      conditionalPanel(
        condition = "input.select =='7'",
        numericInput('lPoi',HTML("&lambda;"),.5)
      ),
      #uniforme
      conditionalPanel(
        condition = "input.select =='8'",
        numericInput('aUni','Mínimo',0),
        numericInput('bUni','Máximo',10)
      ),
      #triangular
      #‘triangle'
      #rtriangle(n, a=0, b=1, c=(a+b)/2)
      conditionalPanel(
        condition = "input.select =='10'",
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


server <- function(input, output, session) {
  
  # df <- reactiveValues(main = NULL)
  # 
  # 
  # 
  # observeEvent(input$agregar,{
  #   curdata <- reactive({
  #     dist <- switch(input$select, 
  #            norm = rnorm, 
  #            # '2' = runif, 
  #            # '3' = rexp,
  #            rnorm)
  #     dist(100)
  #   })
  # #   dist <- switch(input$select,
  # #                  1 = rnorm,
  # #                  2 = runif,
  # #                  3 = rlnorm,
  # #                  4 = rexp,
  # #                  rnorm)
  # # 
  # #   df$main <-dist(input$n)
  #   #df$main <- as.data.frame(rnorm(1000,0,1))
  # df$main <- as.data.frame(curdata)
  # })
  # 
  # output$grafica <- renderPlot({
  #   if (is.null(df$main)){
  #     
  #   }else{
  #     ggplot(df$main,aes(x=df$main[,1])) + geom_histogram(bins = 20) 
  #     }
  # 
  # })
  
}

shinyApp(ui, server)