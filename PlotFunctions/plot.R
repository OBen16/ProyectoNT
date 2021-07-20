library(haven)
library(tidyverse)
library(ggplot2)
library(forcats)
library(xtable)
options(xtable.comment = FALSE)
library(DT)
#maps library's
library(maps)
library(dplyr)
library(maptools)
library(rgdal)
library(here)
library(plotly)
# grafico pie
library(scales)


datos <- read_sav("PlotFunctions/datos.sav")
nousa <- datos %>% filter(C9==2 & C24!=99)
print(getwd())

PlotRazonesNoUsoInternet <- function() {
  #Gráfico de barras de las razones por las que las personas no usan internet
  ggplot(nousa,aes(x=fct_rev(fct_infreq(as.factor(C24))),fill=as.factor(C24)))+geom_bar(stat = "count")+scale_x_discrete(labels=c(
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
      "11"="Otra"))+ 
    stat_count(geom = "text",
               aes(label = ..count..),position=position_stack(vjust=0.5))
}

#Boxplot de eded de las personas que usan internet y las que no
PlotEdadPersonasUsanInternet <- function() {
  ggplot(datos, aes(x = C8, y = as.factor(C9))) + geom_boxplot() + coord_flip() +
    scale_y_discrete(labels = c("1" = "Si", "2" = "No")) +
    labs(x = "Edad", y = "Usa internet")
}


#Gráfico de barra apiladas al 100% del uso de internet según nivel educativo
PlotUsoInternetNivelEducativo <- function() {
  ggplot(datos, aes(fill = as.factor(C9), x = as.factor(niveledu))) + geom_bar(position = "fill") +
    labs(y = "Proporcion", x = "Nivel educativo", fill="Usa internet") +
    scale_fill_brewer(palette="Dark2",labels=c("1"="Si","2"="No"))
}

#Gráfico de barra apiladas al 100% del uso de internet según quintil
PlotUsoInternetQuintil <- function() {
  ggplot(datos, aes(fill = as.factor(C9), x = as.factor(Quintil))) + geom_bar(position = "fill") +
    labs(y = "Proporcion", x = "Quintil", fill="Usa internet",
         title = "Grafico de barras apiladas al 100% de uso de internet segun quintil de ingreso per capita") +
    scale_fill_brewer(palette="Dark2",labels=c("1"="Si","2"="No"))
}

#Gráfico de barra apiladas al 100% del uso de internet según departamento
PlotUsoInternetDepartamento <- function() {
  ggplot(datos, aes(fill = as.factor(C9), x = fct_reorder(as.factor(DOMDEPARTAMENTO),-as.numeric(C9),mean))) + geom_bar(position = "fill") +
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

#Frecuencia de uso de internet en el trabajo por nivel educativo
PlotUsoInternetTrabajoNivelEducativo <- function() {
  datos$niveledu
  datos%>%filter(!is.na(C13_2))%>%
    ggplot(aes(x=as.factor(niveledu),fill=as.factor(C13_2)))+geom_bar(position = "fill")+
    scale_fill_discrete("Uso internet",breaks=c(1,2,3,4,99),
      labels=c("Todos los dias",
                        "Al menos una vez a la semana",
                        "Con menor frecuencia",
                        "No utilizó",
                        "S/D"))+
    scale_x_discrete(labels=c("PI",
                     "CBI",
                     "SCI",
                     "TI",
                     "TNU",
                     "TU"))+
    labs(x="Nivel educativo",y="Proporcion")
}

#Uso de internet por nivel educativo
PlotUsoInternetPorNivelEducativo <- function() {
  datos$C11
  datos%>%filter(!is.na(C11))%>%
    ggplot(aes(x=as.factor(niveledu),fill=as.factor(C11)))+geom_bar(position = "fill")+
    scale_x_discrete(labels=c("PI",
                              "CBI",
                              "SCI",
                              "TI",
                              "TNU",
                              "TU"))+
    scale_fill_discrete("Uso internet",breaks=c(1,2,3,4,99),
                        labels=c("Todos los dias",
                                 "Al menos una vez a la semana",
                                 "Con menor frecuencia",
                                 "No utilizó",
                                 "S/D"))+
    labs(x="Nivel educativo",y="Proporcion")

}

#Uso de redes sociales por nivel educativo
PlotUsoRedesSocialesNivelEducativo <- function() {
  datos$C9_1
  datos%>%filter(!is.na(C9_1))%>%
    ggplot(aes(x=as.factor(niveledu),fill=as.factor(C9_1)))+geom_bar(position = "fill")+
    scale_x_discrete(labels=c("PI",
                              "CBI",
                              "SCI",
                              "TI",
                              "TNU",
                              "TU"))+
    scale_fill_discrete("Uso de redes",
                        breaks=c(1,2),
                        labels=c("Si","No"))+
    labs(x="Nivel educativo",y="Proporcion")
}


#Gráfico de barra apiladas  del uso de Facebook segun departamento
PlotUsoFacebookDepartamento <- function() {
  ggplot(datos, aes(fill = as.factor(DOMDEPARTAMENTO), x = as.factor(C18_1))) + geom_bar(stat = "count") +
    labs(y = "Proporcion", x = "Uso") +
  scale_fill_discrete("Departamento",labels=c("1"="Montevideo",
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
                                             "18"="Tacuarembó")) + scale_x_discrete(labels=c("1" = "Alto", "2" = "Medio", "3" = "Poco" , "4" = "Nunca", "99" = "N/A"))

}

#Gráfico de barra apiladas  del uso de Whatsapp segun departamento
PlotUsoWhatsAppDepartamento <- function() {
ggplot(datos, aes(fill = as.factor(DOMDEPARTAMENTO), x = as.factor(C18_2))) + geom_bar(stat = "count") +
  labs(y = "Proporcion", x = "Uso") +
  scale_fill_discrete("Departamento",labels=c("1"="Montevideo",
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
                                             "18"="Tacuarembó")) + scale_x_discrete(labels=c("1" = "Alto", "2" = "Medio", "3" = "Poco" , "4" = "Nunca", "99" = "N/A"))

}

#Gráfico de barra apiladas  del uso de TWITTER segun departamento
PlotUsoTwitterDepartamento <- function() {
ggplot(datos, aes(fill = as.factor(DOMDEPARTAMENTO), x = as.factor(C18_3))) + geom_bar(stat = "count") +
  labs(y = "Proporcion", x = "Uso") +
  scale_fill_discrete("Departamento",labels=c("1"="Montevideo",
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
                                             "18"="Tacuarembó")) + scale_x_discrete(labels=c("1" = "Alto", "2" = "Medio", "3" = "Poco" , "4" = "Nunca", "99" = "N/A"))
}

#Gráfico de barra apiladas  del uso de Instagram segun departamento
PlotUsoInstagramDepartamento <- function() {
  ggplot(datos, aes(fill = as.factor(DOMDEPARTAMENTO), x = as.factor(C18_4))) + geom_bar(stat = "count") +
    labs(y = "Proporcion", x = "Uso") +
    scale_fill_discrete("Departamento",labels=c("1"="Montevideo",
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
                                                "18"="Tacuarembó")) + scale_x_discrete(labels=c("1" = "Alto", "2" = "Medio", "3" = "Poco" , "4" = "Nunca", "99" = "N/A"))
}

PlotIndiceUsoRedesSociales <- function() {
  # Cambios para crear indice de uso de redes sociales
  datos = datos %>% mutate(C18_1 = replace(C18_1, C18_1 == 99, 0))
  datos = datos %>% mutate(C18_2 = replace(C18_2, C18_2 == 99, 0))
  datos = datos %>% mutate(C18_3 = replace(C18_3, C18_3 == 99, 0))
  datos = datos %>% mutate(C18_4 = replace(C18_4, C18_4 == 99, 0))
  datos = datos %>% mutate(C18_5 = replace(C18_5, C18_5 == 99, 0))
  datos = datos %>% mutate(C18_6 = replace(C18_6, C18_6 == 99, 0))
  datos = datos %>% mutate(C18_7 = replace(C18_7, C18_7 == 99, 0))
  
  datos = datos %>% replace_na(list(C18_1 = 0, C18_2 =0,  C18_3 =0,  C18_4 =0, C18_5 =0, C18_6 =0, C18_7 = 0))
  # se agrega la columna uso_redes_sociales con un indice entre 0 y 1 que maca la intensidad de uso de redes sociales. 
  datos = mutate(datos, uso_redes_sociales =  1 - ( (( ( C18_1 + C18_2 + C18_3 + C18_4 + C18_5 + C18_6 + C18_7) / 7 ) - 1 ) / 3 )  )
  
  # se agrupa por apartamento y se hace una media de la nueva columna uso_redes_sociales
  # Aunque hay departamentos con una intensidad de uso menor, el uso se ve homogeneo. 
  datos %>%
    group_by(DOMDEPARTAMENTO) %>%
    summarise(
      media_uso_redes_sociales = mean(uso_redes_sociales, na.rm = TRUE)
    ) %>%
    ggplot(aes(x = as.factor(DOMDEPARTAMENTO), y= media_uso_redes_sociales) ,fill=DOMDEPARTAMENTO) + labs(y = "Indice uso", x = "Departamento") +geom_bar(stat="identity") + scale_x_discrete(labels=c("1"="Montevideo",
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
    theme(axis.text.x = element_text(angle = 330))
}

TablaVariables<-function(){datatable(matrix(c("PI",
                                   "CBI",
                                   "SCI",
                                   "TI",
                                   "TNU",
                                   "TU",
                                   "Primaria incompleta",
                                   "Ciclo Basico incompleto",
                                   "Segundo Ciclo incompleto",
                                   "Terciaria incompleta",
                                   "Terciaria no Universitaria",
                                   "Terciaria Universitaria"),nrow = 6, ncol = 2),
                          colnames = c("Abreviacion","Variable"))
  }

TablaVar<-function(){
  TablaVariables<-data.frame(matrix(c("PI",
                                      "CBI",
                                      "SCI",
                                      "TI",
                                      "TNU",
                                      "TU",
                                      "Primaria incompleta",
                                      "Ciclo Basico incompleto",
                                      "Segundo Ciclo incompleto",
                                      "Terciaria incompleta",
                                      "Terciaria no Universitaria",
                                      "Terciaria Universitaria"),nrow = 6, ncol = 2))
  colnames(TablaVariables)<-c("Abreviacion","Variable")
  print(xtable(TablaVariables),include.rownames=FALSE)
}

tabvar<-function(){
  tablavariables<-data.frame(matrix(c("Departamento","Departamento donde vive la persona.",
                                      "Uso de internet","Si la persona usa internet o no.",
                                      "Edad","Edad de la persona.","Frecuencia de uso de internet",
                                      "Que tanto usa la persona el internet.","Motivos de no uso de internet",
                                      "Razones de quienes no usan internet para no usarlo.",
                                      "Nivel educativo","Nivel educativo alcanzado."),ncol=2))
  colnames(tablavariables)<-c("Variable","Descripcion")
  print(xtable(tablavariables),include.rownames=FALSE)
}