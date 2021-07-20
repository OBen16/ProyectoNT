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
library(plotrix)
#general
library(labelled)


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


CreateDFRedesSocialesTotalPorcentaje <- function(datos) {
  datos_RS = datos %>%
    group_by(DOMDEPARTAMENTO) %>%
    summarise(
      # Facebook
      porcentage_Facebook_1 = sum(C18_1 == 1, na.rm = TRUE) / sum(C18_1 >=1 & C18_1 <= 99 , na.rm = TRUE),
      porcentage_Facebook_2 = sum(C18_1 == 2, na.rm = TRUE) / sum(C18_1 >=1 & C18_1 <= 99 , na.rm = TRUE),
      porcentage_Facebook_3 = sum(C18_1 == 3, na.rm = TRUE) / sum(C18_1 >=1 & C18_1 <= 99 , na.rm = TRUE),
      porcentage_Facebook_4 = sum(C18_1 == 4, na.rm = TRUE) / sum(C18_1 >=1 & C18_1 <= 99 , na.rm = TRUE),
      porcentage_Facebook_99 = sum(C18_1 == 99, na.rm = TRUE) / sum(C18_1 >=1 & C18_1 <= 99 , na.rm = TRUE),
      # Whats app
      porcentage_WhatsApp_1 = sum(C18_2 == 1, na.rm = TRUE) / sum(C18_2 >=1 & C18_2 <= 4 , na.rm = TRUE),
      porcentage_WhatsApp_2 = sum(C18_2 == 2, na.rm = TRUE) / sum(C18_2 >=1 & C18_2 <= 4 , na.rm = TRUE),
      porcentage_WhatsApp_3 = sum(C18_2 == 3, na.rm = TRUE) / sum(C18_2 >=1 & C18_2 <= 4 , na.rm = TRUE),
      porcentage_WhatsApp_4 = sum(C18_2 == 4, na.rm = TRUE) / sum(C18_2 >=1 & C18_2 <= 4 , na.rm = TRUE),
      porcentage_WhatsApp_99 = sum(C18_2 == 99, na.rm = TRUE) / sum(C18_2 >=1 & C18_2 <= 99 , na.rm = TRUE),
      # Twitter
      porcentage_Twitter_1 = sum(C18_3 == 1, na.rm = TRUE) / sum(C18_3 >=1 & C18_3 <= 4 , na.rm = TRUE),
      porcentage_Twitter_2 = sum(C18_3 == 2, na.rm = TRUE) / sum(C18_3 >=1 & C18_3 <= 4 , na.rm = TRUE),
      porcentage_Twitter_3 = sum(C18_3 == 3, na.rm = TRUE) / sum(C18_3 >=1 & C18_3 <= 4 , na.rm = TRUE),
      porcentage_Twitter_4 = sum(C18_3 == 4, na.rm = TRUE) / sum(C18_3 >=1 & C18_3 <= 4 , na.rm = TRUE),
      porcentage_Twitter_99 = sum(C18_3 == 99, na.rm = TRUE) / sum(C18_3 >=1 & C18_3 <= 99 , na.rm = TRUE),
      # Instagram
      porcentage_Instagram_1 = sum(C18_4 == 1, na.rm = TRUE) / sum(C18_4 >=1 & C18_4 <= 4 , na.rm = TRUE),
      porcentage_Instagram_2 = sum(C18_4 == 2, na.rm = TRUE) / sum(C18_4 >=1 & C18_4 <= 4 , na.rm = TRUE),
      porcentage_Instagram_3 = sum(C18_4 == 3, na.rm = TRUE) / sum(C18_4 >=1 & C18_4 <= 4 , na.rm = TRUE),
      porcentage_Instagram_4 = sum(C18_4 == 4, na.rm = TRUE) / sum(C18_4 >=1 & C18_4 <= 4 , na.rm = TRUE),
      porcentage_Instagram_99 = sum(C18_4 == 99, na.rm = TRUE) / sum(C18_4 >=1 & C18_4 <= 99 , na.rm = TRUE),
    )
  return(datos_RS)
}

################################################
# plot 3d uso Facebook por departamento indice #
################################################

PlotUsoFacebookIndiceDepartamento = function(indice_departamento, datos){
  proporciones <- c(datos$porcentage_Facebook_1[indice_departamento],
                    datos$porcentage_Facebook_2[indice_departamento],
                    datos$porcentage_Facebook_3[indice_departamento],
                    datos$porcentage_Facebook_4[indice_departamento],
                    datos$porcentage_Facebook_99[indice_departamento])
  etiqueta_alto = as.character(format(round(proporciones[1] * 100, 2), nsmall = 2))
  etiqueta_medio = as.character(format(round(proporciones[2] * 100, 2), nsmall = 2))
  etiqueta_poco = as.character(format(round(proporciones[3] * 100, 2), nsmall = 2))
  etiqueta_nunca = as.character(format(round(proporciones[4] * 100, 2), nsmall = 2))
  etiqueta_na = as.character(format(round(proporciones[5] * 100, 2), nsmall = 2))
  etiquetas <- c(paste("Alto ", etiqueta_alto , "%"), 
                 paste("Medio ", etiqueta_medio , "%"),
                 paste("Poco ", etiqueta_poco , "%"), 
                 paste("Nunca ", etiqueta_nunca , "%"), 
                 paste("N/A ", etiqueta_na , "%"))
  dep_name <- val_label(datos$DOMDEPARTAMENTO, as.factor(datos$DOMDEPARTAMENTO[indice_departamento]))
  pie3D(proporciones,labels=etiquetas,
        explode=0.1,
        main= paste("Uso Facebook en " ,dep_name))
}


#Gráfico Pie 3d  del uso de Facebook segun departamento
PlotUsoFacebookDepartamento <- function() {
  data = CreateDFRedesSocialesTotalPorcentaje(datos)
  PlotUsoFacebookIndiceDepartamento(1, data)
}


################################################
# plot 3d uso WhatsApp por departamento indice #
################################################

PlotUsoWhatsAppIndiceDepartamento = function(indice_departamento, datos){
  proporciones <- c(datos$porcentage_WhatsApp_1[indice_departamento],
                    datos$porcentage_WhatsApp_2[indice_departamento],
                    datos$porcentage_WhatsApp_3[indice_departamento],
                    datos$porcentage_WhatsApp_4[indice_departamento],
                    datos$porcentage_WhatsApp_99[indice_departamento])
  etiqueta_alto = as.character(format(round(proporciones[1] * 100, 2), nsmall = 2))
  etiqueta_medio = as.character(format(round(proporciones[2] * 100, 2), nsmall = 2))
  etiqueta_poco = as.character(format(round(proporciones[3] * 100, 2), nsmall = 2))
  etiqueta_nunca = as.character(format(round(proporciones[4] * 100, 2), nsmall = 2))
  etiqueta_na = as.character(format(round(proporciones[5] * 100, 2), nsmall = 2))
  etiquetas <- c(paste("Alto ", etiqueta_alto , "%"), 
                 paste("Medio ", etiqueta_medio , "%"),
                 paste("Poco ", etiqueta_poco , "%"), 
                 paste("Nunca ", etiqueta_nunca , "%"), 
                 paste("N/A ", etiqueta_na , "%"))
  dep_name <- val_label(datos$DOMDEPARTAMENTO, as.factor(datos$DOMDEPARTAMENTO[indice_departamento]))
  pie3D(proporciones,labels=etiquetas,
        explode=0.1,
        main= paste("Uso WhatsApp en " ,dep_name))
}

#Gráfico Pie 3d  del uso de Whatsapp segun departamento
PlotUsoWhatsAppDepartamento <- function() {
  data = CreateDFRedesSocialesTotalPorcentaje(datos)
  PlotUsoWhatsAppIndiceDepartamento(1, data)
}


################################################
# plot 3d uso Twitter por departamento indice #
################################################

PlotUsoTwitterIndiceDepartamento = function(indice_departamento, datos){
  proporciones <- c(datos$porcentage_Twitter_1[indice_departamento],
                    datos$porcentage_Twitter_2[indice_departamento],
                    datos$porcentage_Twitter_3[indice_departamento],
                    datos$porcentage_Twitter_4[indice_departamento],
                    datos$porcentage_Twitter_99[indice_departamento])
  etiqueta_alto = as.character(format(round(proporciones[1] * 100, 2), nsmall = 2))
  etiqueta_medio = as.character(format(round(proporciones[2] * 100, 2), nsmall = 2))
  etiqueta_poco = as.character(format(round(proporciones[3] * 100, 2), nsmall = 2))
  etiqueta_nunca = as.character(format(round(proporciones[4] * 100, 2), nsmall = 2))
  etiqueta_na = as.character(format(round(proporciones[5] * 100, 2), nsmall = 2))
  etiquetas <- c(paste("Alto ", etiqueta_alto , "%"), 
                 paste("Medio ", etiqueta_medio , "%"),
                 paste("Poco ", etiqueta_poco , "%"), 
                 paste("Nunca ", etiqueta_nunca , "%"), 
                 paste("N/A ", etiqueta_na , "%"))
  dep_name <- val_label(datos$DOMDEPARTAMENTO, as.factor(datos$DOMDEPARTAMENTO[indice_departamento]))
  pie3D(proporciones,labels=etiquetas,
        explode=0.1,
        main= paste("Uso Twitter en " ,dep_name))
}

#Gráfico 3d PIE del uso de TWITTER segun departamento
PlotUsoTwitterDepartamento <- function() {
  data = CreateDFRedesSocialesTotalPorcentaje(datos)
  PlotUsoTwitterIndiceDepartamento(1, data)
}

################################################
# plot 3d uso Twitter por departamento indice #
################################################

PlotUsoInstagramIndiceDepartamento = function(indice_departamento, datos){
  proporciones <- c(datos$porcentage_Instagram_1[indice_departamento],
                    datos$porcentage_Instagram_2[indice_departamento],
                    datos$porcentage_Instagram_3[indice_departamento],
                    datos$porcentage_Instagram_4[indice_departamento],
                    datos$porcentage_Instagram_99[indice_departamento])
  etiqueta_alto = as.character(format(round(proporciones[1] * 100, 2), nsmall = 2))
  etiqueta_medio = as.character(format(round(proporciones[2] * 100, 2), nsmall = 2))
  etiqueta_poco = as.character(format(round(proporciones[3] * 100, 2), nsmall = 2))
  etiqueta_nunca = as.character(format(round(proporciones[4] * 100, 2), nsmall = 2))
  etiqueta_na = as.character(format(round(proporciones[5] * 100, 2), nsmall = 2))
  etiquetas <- c(paste("Alto ", etiqueta_alto , "%"), 
                 paste("Medio ", etiqueta_medio , "%"),
                 paste("Poco ", etiqueta_poco , "%"), 
                 paste("Nunca ", etiqueta_nunca , "%"), 
                 paste("N/A ", etiqueta_na , "%"))
  dep_name <- val_label(datos$DOMDEPARTAMENTO, as.factor(datos$DOMDEPARTAMENTO[indice_departamento]))
  pie3D(proporciones,labels=etiquetas,
        explode=0.1,
        main= paste("Uso Instagram en " ,dep_name))
}

#Gráfico de barra apiladas  del uso de Instagram segun departamento
PlotUsoInstagramDepartamento <- function() {
  data = CreateDFRedesSocialesTotalPorcentaje(datos)
  PlotUsoInstagramIndiceDepartamento(1, data)
}


################################
# Indice Uso de redes sociales #
################################

CreateIndiceData <- function(data) {
  # Cambios para crear indice de uso de redes sociales
  data = data %>% mutate(C18_1 = replace(C18_1, C18_1 == 99, 0))
  data = data %>% mutate(C18_2 = replace(C18_2, C18_2 == 99, 0))
  data = data %>% mutate(C18_3 = replace(C18_3, C18_3 == 99, 0))
  data = data %>% mutate(C18_4 = replace(C18_4, C18_4 == 99, 0))
  data = data %>% mutate(C18_5 = replace(C18_5, C18_5 == 99, 0))
  data = data %>% mutate(C18_6 = replace(C18_6, C18_6 == 99, 0))
  data = data %>% mutate(C18_7 = replace(C18_7, C18_7 == 99, 0))
  
  data = data %>% replace_na(list(C18_1 = 0, C18_2 =0,  C18_3 =0,  C18_4 =0, C18_5 =0, C18_6 =0, C18_7 = 0))
  # se agrega la columna uso_redes_sociales con un indice entre 0 y 1 que maca la intensidad de uso de redes sociales. 
  data = mutate(data, uso_redes_sociales =  1 - ( (( ( C18_1 + C18_2 + C18_3 + C18_4 + C18_5 + C18_6 + C18_7) / 7 ) - 1 ) / 3 )  )
  
  # se agrupa por apartamento y se hace una media de la nueva columna uso_redes_sociales
  # Aunque hay departamentos con una intensidad de uso menor, el uso se ve homogeneo. 
  data = data %>%
    group_by(DOMDEPARTAMENTO) %>%
    summarise(
      media_uso_redes_sociales = mean(uso_redes_sociales, na.rm = TRUE)
    ) 
  return(data)
}

AddIndexDepartamentoMapa <- function(data) {
  # cosas raras de R
  data <- data %>% mutate(id = MapDep(DOMDEPARTAMENTO) )
  index = 0
  for (i in data$DOMDEPARTAMENTO){
    index = index + 1
    data$id[index] = MapDepartamento(data$DOMDEPARTAMENTO[index])
  }
  return(data)
}

MapDep = function(i){
  return(i*1)
}

MapDepartamento = function(i){
  if (i==1){
    departamento = "0" # Montevideo
  } else if ( i == 2) {
    departamento = "1" # Artigas
  } else if ( i == 3) {
    departamento =  "2" # Canelones
  }else if ( i == 4) {
    departamento = "19" # Cerro Largo
  }else if ( i == 5) {
    departamento =  "3" # Colonia
  }else if ( i == 6) {
    departamento = "4" # Durazno
  }else if ( i == 7) {
    departamento = "17" # Flores
  }else if ( i == 8) {
    departamento = "5" # Florida
  }else if ( i == 9) {
    departamento =  "6" # Lavalleja
  }else if ( i == 10) {
    departamento = "18" # Maldonado
  } else if ( i == 11) {
    departamento = "7" # Paysandu
  }else if ( i == 12) {
    departamento =  "8" # Rio Negro
  } else if ( i == 13) {
    departamento =  "9" # Rivera
  }else if ( i == 14) {
    departamento = "10" # Rocha
  }else if ( i == 15) {
    departamento = "11" # Salto
  }else if ( i == 16) {
    departamento =  "12" # San Jose
  }else if ( i == 17) {
    departamento = "13" # Soriano
  }else if ( i == 18) {
    departamento =  "16" # Tacuarembo
  }else if ( i == 19) {
    departamento = "14" # Flores
  }
  return(departamento)
}

PlotIndiceUsoRedesSociales <- function() {
  data = CreateIndiceData(datos)
  # es necesario hacer el mapeo de indices entre el mapa
  # y el que esta en la BD.
  data = AddIndexDepartamentoMapa(data)
  
  # Uruguay Plot Map
  sp_depto <- readOGR("PlotFunctions/ine_depto.shp")
  dframe_depto <- ggplot2::fortify(sp_depto) #conviere a data.frame
  # JOIN by id
  mapa <- left_join(dframe_depto, data, by = "id")
  # Finally plot Uy!
  ggplot(mapa, aes(long, lat, group = group))+
    geom_polygon(aes(fill = media_uso_redes_sociales), color = "white")+
    scale_fill_viridis_c()
  
}

#############################
# Tabla de valores dinamica #
#############################

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