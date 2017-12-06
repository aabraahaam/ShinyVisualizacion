library(shiny)
library(shinydashboard)

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
                  choices = list("Normal" = 1, "Normal Truncada" = 2, "Lognormal" = 3,
                                 "Binomial" = 4, "Gamma" = 5, "Chi Cuadrada" = 6,
                                 "Poisson" = 7, "Uniforme" = 8, " " = 9), 
                  selected = 9),
      conditionalPanel(
        condition = "input.select =='1' || input.select =='2' || input.select =='3'",
        numericInput('mediaN','Media',0),
        numericInput('sigmaN','Varianza',1)
      )
    ),
    #Agregar cálculo
    conditionalPanel(
      condition = "input.radio == '2'",
      textInput('nomf','Nombre del cálculo'),
      textInput('form','Fórmula')
    )
    

  ),
  dashboardBody()
)


server <- function(input, output, session) {
  
}

shinyApp(ui, server)