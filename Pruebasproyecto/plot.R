library(haven)
library(tidyverse)
library(ggplot2)
library(forcats)

datos <- read_sav("Pruebasproyecto/datos.sav")
nousa <- datos %>% filter(C9==2 & C24!=99)

#Gráfico de barras de las razones por las que las personas no usan internet
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

#Boxplot de eded de las personas que usan internet y las que no
ggplot(datos, aes(x = C8, y = as.factor(C9))) + geom_boxplot() + coord_flip() +
  scale_y_discrete(labels = c("1" = "Sí", "2" = "No")) +
  labs(x = "Edad", y = "¿Usa internet?",
       title = "Boxplot de edad segun si usan o no internet")

#Gráfico de barra apiladas al 100% del uso de internet según nivel educativo
ggplot(datos, aes(fill = as.factor(C9), x = as.factor(niveledu))) + geom_bar(position = "fill") +
  labs(y = "Proporcion", x = "Nivel educativo", fill="¿Usa internet?",
       title = "Grafico de barras apiladas al 100% de uso de internet segun nivel educativo") +
  scale_fill_brewer(palette="Dark2",labels=c("1"="Sí","2"="No"))

#Gráfico de barra apiladas al 100% del uso de internet según quintil
ggplot(datos, aes(fill = as.factor(C9), x = as.factor(Quintil))) + geom_bar(position = "fill") +
  labs(y = "Proporcion", x = "Quintil", fill="¿Usa internet?",
       title = "Grafico de barras apiladas al 100% de uso de internet segun quintil de ingreso per capita") +
  scale_fill_brewer(palette="Dark2",labels=c("1"="Sí","2"="No"))

#Gráfico de barra apiladas al 100% del uso de internet según departamento
ggplot(datos, aes(fill = as.factor(C9), x = fct_reorder(as.factor(DOMDEPARTAMENTO),-as.numeric(C9),mean))) + geom_bar(position = "fill") +
  labs(y = "Proporcion", x = "Departamento", fill="¿Usa internet?",
       title = "Grafico de barras apiladas al 100% de uso de internet segun departamento") +
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

#Frecuencia de uso de internet en el trabajo por nivel educativo
datos%>%filter(!is.na(C13_2))%>%
  ggplot(aes(x=as.factor(niveledu),fill=as.factor(C13_2)))+geom_bar(position = "fill")


#Uso de internet por nivel educativo
datos$C11
datos%>%filter(!is.na(C11))%>%
  ggplot(aes(x=as.factor(niveledu),fill=as.factor(C11)))+geom_bar(position = "fill")

#Uso de redes sociales por nivel educativo
datos$C9_1
datos%>%filter(!is.na(C9_1))%>%
  ggplot(aes(x=as.factor(niveledu),fill=as.factor(C9_1)))+geom_bar(position = "fill")


# Genera indice uso de redes sociales agrupando todas las redes.
datos = datos %>% replace_na(list(C18_1 = 4, C18_2 =4,  C18_3 =4,  C18_4 =4, C18_5 =4, C18_6 =4, C18_7 = 4))
datos = mutate(datos, uso_redes_sociales =  1 - ( (( ( C18_1 + C18_2 + C18_3 + C18_4 + C18_5 + C18_6 + C18_7) / 7 ) - 1 ) / 3 )  )

datos$uso_redes_sociales = datos$uso_redes_sociales %>% replace_na(0)

datos %>%
  group_by(DOMDEPARTAMENTO) %>%
  summarise(
    media_uso_redes_sociales = mean(uso_redes_sociales, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = DOMDEPARTAMENTO, y = media_uso_redes_sociales)) + geom_point()

