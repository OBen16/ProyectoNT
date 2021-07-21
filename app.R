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
                      c("Razones por la que no se usa Internet","Boxplot de Edad","Gráfico de barras")),
          conditionalPanel(condition = "input.razonesNoUsoInternetSelect == 'Razones por la que no se usa Internet'",
                           sliderInput(
                             inputId = "ed",
                             label = "Edad:", min = 14, max = 98,
                             value = c(14,98))
                           ),
          conditionalPanel(condition = "input.razonesNoUsoInternetSelect == 'Gráfico de barras'",
                           radioButtons("elegirgraf", "Gráfico de barras uso de internet según:", c("Departamento","Nivel educativo")),
                           sliderInput(
                             inputId = "ed2",
                             label = "Edad:", min = 14, max = 98,
                             value = c(14,98))
          )
        ),
        mainPanel(
          conditionalPanel(condition = "input.razonesNoUsoInternetSelect == 'Razones por la que no se usa Internet'",
            plotOutput("razonesNoUsoInternet")
          ),
          conditionalPanel(condition = "input.razonesNoUsoInternetSelect == 'Boxplot de Edad'",
            plotOutput("edadPersonasUsanInternet")
          ),
          conditionalPanel(condition = "input.razonesNoUsoInternetSelect == 'Gráfico de barras'",
          plotOutput("usoInternetgrafbar")
          )
        )
      )
    ),
    tabPanel(
      "Incidencia del uso de Internet por nivel educativo",
      sidebarLayout(
        sidebarPanel(
          selectInput("usoInternetNivelEduSelect","Detalle",
                      c("Uso internet en el trabajo","Uso internet por nivel educativo","Uso redes por nivel educativo")),
          dataTableOutput("TabVar")
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
                 ),
                 conditionalPanel(condition = "input.usoredes != 'Indice de uso de redes sociales'",
                                  selectInput(inputId = "departamento_redes_sociales",label="Seleccione Departamento",
                                              choices =  c("Montevideo"=1,
                                                           "Canelones"=2,
                                                           "Cerro Largo"=3,
                                                           "Colonia"=4,
                                                           "Rio Negro"=9,
                                                           "Tacuarembó"=13), selected=1)
                 )
               )
             ))
  )
)

server <-function(input, output){
  # Seccion 1: plot razones de no uso de Internet
  output$razonesNoUsoInternet      <- renderPlot({
    nousa<- filter(datos,C8>=input$ed[1] & C8<=input$ed[2]) %>% filter(C9==2 & C24!=99)
    plot1<-  nousa %>% 
      ggplot(aes(x=fct_rev(fct_infreq(as.factor(C24))),fill=as.factor(C24)))+geom_bar(stat = "count",aes(y = (..count..)/sum(..count..)))+scale_x_discrete(labels=c(
      "1"="No sabe como podria servirle",
      "2"="No sabe usarlo",
      "3"="No tiene dispositivos digitales",
      "4"="Le resulta caro",
      "5"="No tiene tiempo",
      "6"="Discapacidad",
      "7"="No le interesa o no quiere",
      "8"="Falta de conocimiento de idioma extranjero",
      "9"="Inseguro respecto al contenido",
      "10"="Le preocupa privacidad",
      "11"="Otra"))+coord_flip()+labs(fill="Motivo de no uso",x="Razon",y="Cantidad")+scale_fill_brewer(palette="Paired",labels=c(
        "1"="No sabe como podria servirle",
        "2"="No sabe usarlo",
        "3"="No tiene dispositivos digitales",
        "4"="Le resulta caro",
        "5"="No tiene tiempo",
        "6"="Discapacidad",
        "7"="No le interesa o no quiere",
        "8"="Falta de conocimiento de idioma extranjero",
        "9"="Inseguro respecto al contenido",
        "10"="Le preocupa privacidad",
        "11"="Otra"))+scale_y_continuous(labels = scales::percent)
    print(plot1)
  })
  output$edadPersonasUsanInternet  <- renderPlot({PlotEdadPersonasUsanInternet()})
  output$usoInternetgrafbar <- renderPlot({
    datosgrafbar<- filter(datos,C8>=input$ed2[1] & C8<=input$ed2[2])
    if(input$elegirgraf=="Nivel educativo"){
      datosgrafbar %>% ggplot(aes(fill = as.factor(C9), x = as.factor(niveledu))) + geom_bar(position = "fill") +
        labs(y = "Proporcion", x = "Nivel educativo", fill = "Usa internet") +
        scale_fill_brewer(palette = "Dark2", labels = c("1" = "Si", "2" ="No"))
      }else{
      datosgrafbar %>% ggplot(aes(fill = as.factor(C9), x = fct_reorder(as.factor(DOMDEPARTAMENTO),-as.numeric(C9),mean))) + geom_bar(position = "fill") +
          labs(y = "Proporcion", x = "Departamento", fill="Usa internet") +
          scale_fill_brewer(palette="Dark2",labels=c("1"="Si","2"="No")) + scale_x_discrete(labels=c("1"="Montevideo",
                                                                                                     "3"="Canelones",
                                                                                                     "4"="Cerro Largo",
                                                                                                     "5"="Colonia",
                                                                                                     "8"="Florida",
                                                                                                     "10"="Maldonado",
                                                                                                     "6"="Durazno",
                                                                                                     "11"="Paysandu",
                                                                                                     "12"="Rio Negro",
                                                                                                     "13"="Rivera",
                                                                                                     "14"="Rocha",
                                                                                                     "16"="San José",
                                                                                                     "18"="Tacuarembó")) +
          theme(axis.text.x = element_text(angle = 270, vjust = 0))
      }
    })
  
  #Seccion 2: Incidencia del uso de Internet por nivel educativo
  g1<-reactive(PlotUsoInternetTrabajoNivelEducativo())
  output$usoInternetTrabajoNivelEducativo <- renderPlot({g1()})
  output$usoInternetPorNivelEducativo  <- renderPlot({PlotUsoInternetPorNivelEducativo()})
  output$usoRedesSocialesNivelEducativo <- renderPlot({PlotUsoRedesSocialesNivelEducativo()})
  output$TabVar<-renderDataTable({TablaVariables()})
  
  #Seccion 3: Uso de redes sociales y caracterizacion por departamentos
  output$usoFacebookDepartamento      <- renderPlot({
      if (input$departamento_redes_sociales ==1) {
        indice = 1
      } else  if (input$departamento_redes_sociales ==2) {
        indice = 2
      }else  if (input$departamento_redes_sociales ==3) {
        indice = 3
      }else  if (input$departamento_redes_sociales ==4) {
        indice = 4
      }else  if (input$departamento_redes_sociales ==9) {
        indice = 9
      }else  if (input$departamento_redes_sociales ==13) {
        indice = 13
      }
      PlotUsoFacebookDepartamento(indice)
    })
  output$usoWhatsAppDepartamento      <- renderPlot({
    if (input$departamento_redes_sociales ==1) {
      indice = 1
    } else  if (input$departamento_redes_sociales ==2) {
      indice = 2
    }else  if (input$departamento_redes_sociales ==3) {
      indice = 3
    }else  if (input$departamento_redes_sociales ==4) {
      indice = 4
    }else  if (input$departamento_redes_sociales ==9) {
      indice = 9
    }else  if (input$departamento_redes_sociales ==13) {
      indice = 13
    }
    PlotUsoWhatsAppDepartamento(indice)
  })
  output$usoTwitterDepartamento      <- renderPlot({
    if (input$departamento_redes_sociales ==1) {
      indice = 1
    } else  if (input$departamento_redes_sociales ==2) {
      indice = 2
    }else  if (input$departamento_redes_sociales ==3) {
      indice = 3
    }else  if (input$departamento_redes_sociales ==4) {
      indice = 4
    }else  if (input$departamento_redes_sociales ==9) {
      indice = 9
    }else  if (input$departamento_redes_sociales ==13) {
      indice = 13
    }
    PlotUsoTwitterDepartamento(indice)
  })
  output$usoInstagramDepartamento      <- renderPlot({
    if (input$departamento_redes_sociales ==1) {
      indice = 1
    } else  if (input$departamento_redes_sociales ==2) {
      indice = 2
    }else  if (input$departamento_redes_sociales ==3) {
      indice = 3
    }else  if (input$departamento_redes_sociales ==4) {
      indice = 4
    }else  if (input$departamento_redes_sociales ==9) {
      indice = 9
    }else  if (input$departamento_redes_sociales ==13) {
      indice = 13
    }
    PlotUsoInstagramDepartamento(indice)
  })
  output$indiceUsoRedesSociales      <- renderPlot({PlotIndiceUsoRedesSociales()})
}
shinyApp(ui, server)
