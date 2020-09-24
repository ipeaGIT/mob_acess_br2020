Sys.setenv(TZ='UTC') # Fuso horario local

# carregar bibliotecas
library(ggplot2)      # visualizacao de dados
library(ggthemes)     # temas para visualizacao de dados
library(sf)           # leitura e manipulacao de dados espaciais
library(data.table)   # manipulacao de dados
library(geobr)        # dados espaciais do brasil
library(pbapply)      # progress bar
library(readr)        # rapida leitura de dados 
library(tidyr)        # manipulacao de dados
library(stringr)      # operacoes em strings
library(lubridate)    # dados em data/horario
library(mapview)      # visualizacao interativa dos dados
library(RColorBrewer) # paleta de cores
library(extrafont)    # fontes de texto
#loadfonts()

library(furrr)
library(purrr)
library(dplyr)
library(hrbrthemes)
library(beepr)
library(datapasta)
library(patchwork)
library(sidrar)
library(devtools)
library(janitor)
library(lemon)
library(rio)
library(scales)
library(rlist)
library(survey)
library(srvyr)
# devtools::install_github("lucasmation/microdadosBrasil")
library(microdadosBrasil)
library(Hmisc)
library(forcats)
library(gridExtra)
library(ggplotify)
library(grid)
library(PNADcIBGE)


# disable scientific notation
options(scipen=10000)


# Use GForce Optimisations in data.table operations
# details > https://jangorecki.gitlab.io/data.cube/library/data.table/html/datatable-optimize.html
options(datatable.optimize=Inf)

# set number of threads used in data.table
data.table::setDTthreads(percent = 100)



## usefull support functions

`%nin%` = Negate(`%in%`)
`%nlike%` = Negate(`%like%`)


  