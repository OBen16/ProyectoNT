library(tidyverse)
library(haven)
library(shiny)
datos<-read_sav("datos.sav")

ui <- fluidPage(
  tabsetPanel(
    tabPanel("aaa",
  sidebarLayout(
    sidebarPanel(
    sliderInput(
    inputId = "ed",
                            label = "Edad:", min = 14, max = 100,
                            value = c(30,45))
),
mainPanel(plotOutput("graf"))))))
server <- function(input, output){
  output$graf <- renderPlot({dist <- filter(datos,C8>=input$ed[1] & C8<=input$ed[2])   
  gg <- dist %>% 
    ggplot(aes(x=as.factor(C9),fill=as.factor(C9))) + geom_bar() +
    labs(y = "Cantidad", x = "Usa", fill="¿Usa internet?",
         title = "Grafico de barras de uso de internet") +
    scale_fill_brewer(palette="Dark2",labels=c("1"="Si","2"="No")) +
    scale_x_discrete(labels=c("1"="Si","2"="No"))
  print(gg)
  })
}
shinyApp(ui, server)


