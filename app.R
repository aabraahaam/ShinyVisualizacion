library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    radioButtons("radio", label ="Radio buttons",
                 choices = list("Choice 1" = 1, "Choice 2" = 2), 
                 selected = 1, inline = T),
    actionButton('agregar','Agregar',icon = icon('plus')),
    textInput('nombre','Nombre de la distribución'),
    selectInput("select", label = "Distribución", 
                choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                selected = 1)
  ),
  dashboardBody()
)


server <- function(input, output, session) {
  
}

shinyApp(ui, server)