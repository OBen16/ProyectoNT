library(shiny)
library(tidyverse)
base::source("./PlotFunctions/plot.R")


ui <- fluidPage(
  titlePanel("Uso Tecnologia Uruguay 2019"),
  tabsetPanel(
    tabPanel(
      "Uso de internet",
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
          ),
          conditionalPanel(condition = "input.razonesNoUsoInternetSelect== 'Boxplot de Edad'",
                           selectInput("selop","Selecciona una opción",c("Uruguay","Comparar departamentos")),
                           conditionalPanel(condition ="input.selop == 'Comparar departamentos'",
                                            checkboxGroupInput("seldep","Selecciona los departamento que quieras comparar:",
                                                               choiceNames = list("Montevideo","Canelones","Cerro Largo","Colonia",
                                                                                  "Durazno","Florida","Maldonado","Paysandú",
                                                                                  "Río Negro","Rivera","Rocha","San José","Tacuarembó"),
                                                               choiceValues = list("Montevideo","Canelones","Cerro Largo","Colonia",
                                                                                   "Durazno","Florida","Maldonado","Paysandú",
                                                                                   "Río Negro","Rivera","Rocha","San José","Tacuarembó"),
                                                               selected = c("Montevideo","Canelones")))
          )
        ),
        mainPanel(
          conditionalPanel(condition = "input.razonesNoUsoInternetSelect == 'Razones por la que no se usa Internet'",
                           plotOutput("razonesNoUsoInternet")
          ),
          conditionalPanel(condition = "input.razonesNoUsoInternetSelect == 'Boxplot de Edad'",
                           conditionalPanel(condition = "input.selop == 'Uruguay'",
                                            plotOutput("boxedaduy")
                           ),
                           conditionalPanel(condition = "input.selop == 'Comparar departamentos'",
                                            plotOutput("boxedad")
                           )
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
                             c("Uso de Facebook Montevideo","Uso de WhatsApp Montevideo",
                               "Uso de Twitter Montevideo","Uso de Instagram Montevideo",
                               "Indice de uso de redes sociales", "Heatmap por departamentos"))
               ),
               mainPanel(
                 conditionalPanel(condition = "input.usoredes == 'Uso de Facebook Montevideo'",
                                  plotOutput("usoFacebookDepartamento")
                 ),
                 conditionalPanel(condition = "input.usoredes == 'Uso de WhatsApp Montevideo'",
                                  plotOutput("usoWhatsAppDepartamento")
                 ),
                 conditionalPanel(condition = "input.usoredes == 'Uso de Twitter Montevideo'",
                                  plotOutput("usoTwitterDepartamento")
                 ),
                 conditionalPanel(condition = "input.usoredes == 'Uso de Instagram Montevideo'",
                                  plotOutput("usoInstagramDepartamento")
                 ),
                 conditionalPanel(condition = "input.usoredes == 'Indice de uso de redes sociales'",
                                  plotOutput("indiceUsoRedesSociales")
                 ),
                 conditionalPanel(condition = "input.usoredes == 'Heatmap por departamentos'",
                                  plotOutput("heatmapPorDepartamentos")
                 ),
                 conditionalPanel(condition = "input.usoredes == 'Heatmap por departamentos'",
                                  selectInput(inputId = "heatmapInputOption",label="Detalle",
                                              choices =  c("Sin dendograma"=1,
                                                           "Dendograma por departamento"=2,
                                                           "Dendograma por red social"=3), selected=1)
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
  
  datos2<-datos %>%
    mutate(DOMDEPARTAMENTO = case_when(
      DOMDEPARTAMENTO==1 ~ "Montevideo",
      DOMDEPARTAMENTO == 3 ~ "Canelones",
      DOMDEPARTAMENTO == 4 ~ "Cerro Largo",
      DOMDEPARTAMENTO == 5 ~ "Colonia",
      DOMDEPARTAMENTO == 6 ~ "Durazno",
      DOMDEPARTAMENTO == 8 ~ "Florida",
      DOMDEPARTAMENTO == 10 ~ "Maldonado",
      DOMDEPARTAMENTO == 11 ~ "Paysandú",
      DOMDEPARTAMENTO == 12 ~ "Río Negro",
      DOMDEPARTAMENTO == 13 ~ "Rivera",
      DOMDEPARTAMENTO == 14 ~ "Rocha",
      DOMDEPARTAMENTO == 16 ~ "San José",
      DOMDEPARTAMENTO == 18 ~ "Tacuarembó",
    ))
  output$boxedaduy <- renderPlot({ggplot(datos, aes(x = C8, y = as.factor(C9))) + geom_boxplot() + coord_flip() +
      scale_y_discrete(labels = c("1" = "Si", "2" = "No")) +
      labs(x = "Edad", y = "Usa internet")})
  output$boxedad <- renderPlot({
    seldep <- input$seldep
    if (length(seldep) == 0) {
      return
    } else {
      datosboxedad <- filter(datos2, DOMDEPARTAMENTO %in% input$seldep)
      datosboxedad %>% ggplot(aes(x = C8, y = as.factor(C9))) + geom_boxplot() + coord_flip() +
        scale_y_discrete(labels = c("1" = "Si", "2" = "No")) +
        labs(x = "Edad", y = "Usa internet") + facet_wrap( ~ DOMDEPARTAMENTO)
    }
  })
  
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
  output$heatmapPorDepartamentos      <- renderPlot({
    if (input$heatmapInputOption ==1) {
      indice = 1
    } else  if (input$heatmapInputOption ==2) {
      indice = 2
    }else  if (input$heatmapInputOption ==3) {
      indice = 3
    }
    PlotHeatMapDepartamentosRedesSociales(indice)
  })
  output$usoFacebookDepartamento      <- renderPlot({
    indice = 1
    PlotUsoFacebookDepartamento(indice)
  })
  output$usoWhatsAppDepartamento      <- renderPlot({
    indice = 1
    PlotUsoWhatsAppDepartamento(indice)
  })
  output$usoTwitterDepartamento      <- renderPlot({
    indice = 1
    PlotUsoTwitterDepartamento(indice)
  })
  output$usoInstagramDepartamento      <- renderPlot({
    indice = 1
    PlotUsoInstagramDepartamento(indice)
  })
  output$indiceUsoRedesSociales      <- renderPlot({PlotIndiceUsoRedesSociales()})
}
shinyApp(ui, server)