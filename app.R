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
                           plotOutput("usoInternetTrabajoNivelEducativo"),
                           sliderInput("grados","Grados",
                                       min = 0,
                                       max=360,
                                       value = 300)
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
    tabPanel("Uso de redes sociales y caracterizacion por departamentos",
             sidebarLayout(
               sidebarPanel(
                 selectInput("usoredes","Detalle",
                             c("Uso de Facebook por departamento","Uso de WhatsApp por departamento",
                               "Uso de Twitter por departamento","Uso de Instagram por departamento",
                               "Indice de uso de redes sociales"))
               ),
               mainPanel(
                 conditionalPanel(condition = "input.usoredes == 'Uso de Facebook por departamento'",
                                  plotOutput("usoFacebookDepartamento")
                 ),
                 conditionalPanel(condition = "input.usoredes == 'Uso de WhatsApp por departamento'",
                                  plotOutput("usoWhatsAppDepartamento")
                 ),
                 conditionalPanel(condition = "input.usoredes == 'Uso de Twitter por departamento'",
                                  plotOutput("usoTwitterDepartamento")
                 ),
                 conditionalPanel(condition = "input.usoredes == 'Uso de Instagram por departamento'",
                                  plotOutput("usoInstagramDepartamento")
                 ),
                 conditionalPanel(condition = "input.usoredes == 'Indice de uso de redes sociales'",
                                  plotOutput("indiceUsoRedesSociales")
                 )
               )
             ))
  )
)

server <-function(input, output){
  # Seccion 1: plot razones de no uso de Internet 
  output$razonesNoUsoInternet      <- renderPlot({PlotRazonesNoUsoInternet()})
  output$edadPersonasUsanInternet  <- renderPlot({PlotEdadPersonasUsanInternet()})
  output$usoInternetNivelEducativo <- renderPlot({PlotUsoInternetNivelEducativo()})
  output$usoInternetDepartamento   <- renderPlot({PlotUsoInternetDepartamento()})
  
  #Seccion 2: Incidencia del uso de Internet por nivel educativo
  g1<-reactive(PlotUsoInternetTrabajoNivelEducativo()+
                 theme(axis.text.x = element_text(angle = input$grados)))
  output$usoInternetTrabajoNivelEducativo <- renderPlot({g1()})
  output$usoInternetPorNivelEducativo  <- renderPlot({PlotUsoInternetPorNivelEducativo()})
  output$usoRedesSocialesNivelEducativo <- renderPlot({PlotUsoRedesSocialesNivelEducativo()})
  #Seccion 3: Uso de redes sociales y caracterizacion por departamentos
  output$usoFacebookDepartamento      <- renderPlot({PlotUsoFacebookDepartamento()})
  output$usoWhatsAppDepartamento      <- renderPlot({PlotUsoWhatsAppDepartamento()})
  output$usoTwitterDepartamento      <- renderPlot({PlotUsoTwitterDepartamento()})
  output$usoInstagramDepartamento      <- renderPlot({PlotUsoInstagramDepartamento()})
  output$indiceUsoRedesSociales      <- renderPlot({PlotIndiceUsoRedesSociales()})
}
shinyApp(ui, server)