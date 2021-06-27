library(haven)
datos<-read_sav("Pruebasproyecto/datos.sav")
library(tidyverse)
library(ggplot2)
library(forcats)
datos2<-datos %>% filter(C24>=1)
datos3<-datos %>% filter(C24>=10)
nousa<-datos %>% filter(C9==2 & C24!=99)
datos11<-datos %>% filter(C24==11)
ggplot(nousa,aes(x=fct_rev(fct_infreq(as.factor(C24))),fill=as.factor(C24)))+geom_bar(stat = "count")+scale_x_discrete(labels=c(
  "1"="No sabe como podria servirle",
  "2"="No sabe usarlo",
  "3"="No tiene dispositivos digitales",
  "4"="Le resulta caro",
  "5"="No tiene tiempo",
  "6"="discapacidad",
  "7"="no le interesa o no quiere",
  "8"="falta de conocimiento de idioma extranjero",
  "9"="inseguro respecto al contenido",
  "10"="le preocupa privacidad",
  "11"="otra"))+coord_flip()+labs(fill="Motivo de no uso",x="Razón",y="Cantidad",title = "Razones por las que no usan internet")+scale_fill_brewer(palette="Paired",labels=c(
    "1"="No sabe como podria servirle",
    "2"="No sabe usarlo",
    "3"="No tiene dispositivos digitales",
    "4"="Le resulta caro",
    "5"="No tiene tiempo",
    "6"="discapacidad",
    "7"="no le interesa o no quiere",
    "8"="falta de conocimiento de idioma extranjero",
    "9"="inseguro respecto al contenido",
    "10"="le preocupa privacidad",
    "11"="otra"))+ 
  stat_count(geom = "text",
             aes(label = ..count..),position=position_stack(vjust=0.5))

ggplot(datos, aes(x = C8, y = as.factor(C9))) + geom_boxplot() + coord_flip() +
  scale_y_discrete(labels = c("1" = "Sí", "2" = "No")) +
  labs(x = "Edad", y = "¿Usa internet?",
       title = "Boxplot de edad segun si usan o no internet")


ggplot(datos, aes(fill = as.factor(C9), x = as.factor(niveledu))) + geom_bar(position = "fill") +
  labs(y = "Proporcion", x = "Nivel educativo", fill="¿Usa internet?",
       title = "Grafico de barras apiladas al 100porciento de uso de internet segun nivel educativo") +
  scale_fill_brewer(palette="Dark2",labels=c("1"="Sí","2"="No"))

ggplot(datos, aes(fill = as.factor(C9), x = as.factor(Quintil))) + geom_bar(position = "fill") +
  labs(y = "Proporcion", x = "Quintil", fill="¿Usa internet?",
       title = "Grafico de barras apiladas al 100porciento de uso de internet segun quintil de ingreso per capita") +
  scale_fill_brewer(palette="Dark2",labels=c("1"="Sí","2"="No"))

ggplot(datos, aes(fill = as.factor(C9), x = fct_reorder(as.factor(DOMDEPARTAMENTO),-as.numeric(C9),mean))) + geom_bar(position = "fill") +
  labs(y = "Proporcion", x = "Departamento", fill="¿Usa internet?",
       title = "Grafico de barras apiladas al 100porciento de uso de internet segun departamento") +
  scale_fill_brewer(palette="Dark2",labels=c("1"="Sí","2"="No")) + scale_x_discrete(labels=c("1"="Montevideo",
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
                                                                                             "18"="Tacuarembó"))


