library(shiny)
library(tidyverse)
base::source("./PlotFunctions/plot.R")

ui <- fluidPage(
  titlePanel("Uso Tecnologia Uruguay 2019"),
  tabsetPanel(
    tabPanel(
      "Razones No uso de Internet",
      sidebarLayout(
        sidebarPanel(
          selectInput("razonesNoUsoInternetSelect","Detalle",
                      c("Razones por la que no se usa Internet","Boxplot de Edad","Segun Nivel Educativo", "Segun Departamento"))
        ),
        mainPanel(
          conditionalPanel(condition = "input.razonesNoUsoInternetSelect == 'Razones por la que no se usa Internet'",
            plotOutput("razonesNoUsoInternet")
          ),
          conditionalPanel(condition = "input.razonesNoUsoInternetSelect == 'Boxplot de Edad'",
            plotOutput("edadPersonasUsanInternet")
          ),
          conditionalPanel(condition = "input.razonesNoUsoInternetSelect == 'Segun Nivel Educativo'",
            plotOutput("usoInternetNivelEducativo")
          ),
          conditionalPanel(condition = "input.razonesNoUsoInternetSelect == 'Segun Departamento'",
            plotOutput("usoInternetDepartamento")
          )
        )
      )
    ),
    tabPanel(
      "Incidencia del uso de Internet por nivel educativo",
      sidebarLayout(
        sidebarPanel(
          selectInput("usoInternetNivelEduSelect","Detalle",
                      c("Uso internet en el trabajo","Uso internet por nivel educativo","Uso redes por nivel educativo"))
        ),
        mainPanel(
          conditionalPanel(condition = "input.usoInternetNivelEduSelect == 'Uso internet en el trabajo'",
                           plotOutput("usoInternetTrabajoNivelEducativo")
          ),
          conditionalPanel(condition = "input.usoInternetNivelEduSelect == 'Uso internet por nivel educativo'",
                           plotOutput("usoInternetPorNivelEducativo")
          ),
          conditionalPanel(condition = "input.usoInternetNivelEduSelect == 'Uso redes por nivel educativo'",
                           plotOutput("usoRedesSocialesNivelEducativo")
          )
        )
      )
      ),
    tabPanel("Uso de redes sociales y caracterizacion por departamentos")
  )
)

server <-function(input, output){
  # Seccion 1: plot razones de no uso de Internet 
  output$razonesNoUsoInternet      <- renderPlot({PlotRazonesNoUsoInternet()})
  output$edadPersonasUsanInternet  <- renderPlot({PlotEdadPersonasUsanInternet()})
  output$usoInternetNivelEducativo <- renderPlot({PlotUsoInternetNivelEducativo()})
  output$usoInternetDepartamento   <- renderPlot({PlotUsoInternetDepartamento()})
  
  #Seccion 2: Incidencia del uso de Internet por nivel educativo
  output$usoInternetTrabajoNivelEducativo <- renderPlot({PlotUsoInternetTrabajoNivelEducativo()})
  output$usoInternetPorNivelEducativo  <- renderPlot({PlotUsoInternetPorNivelEducativo()})
  output$usoRedesSocialesNivelEducativo <- renderPlot({PlotUsoRedesSocialesNivelEducativo()})
}
shinyApp(ui, server)