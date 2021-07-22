# Uso de la Tecnología de la Información y Comunicaciones (EUTIC) en Uruguay 2019

Uso de la Tecnología de la Información y Comunicaciones (EUTIC) en Uruguay 2019 es un proyecto en R 
que analiza en forma descriptiva una muestra de Uruguay el uso de la tecnologia desde distntas perspectivas. 


## Instalacion

Utilizar R-studio con version de R actualizada.

Para compilar el Rmarkdown utilizar
```bash
UsoTecnologia2019Uruguay.rmd
```

Para compilar el app utilizar
```bash
app.R
```

## Paquetes Requeridos

Para el uso corrector asegurarse que estan instalados los siguientes packages
```R
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
library(ggmosaic)
# heatmap
library("pheatmap")
require(ggplot2)
require(colorspace)
library(grid)
```

## Para contribuir al codigo
Pedidos de Pull son bienvenidos. Para cambios mayores, por favor abrir un Issue primariamente para discutir el cambio. 



## License
[MIT](https://choosealicense.com/licenses/mit/)
