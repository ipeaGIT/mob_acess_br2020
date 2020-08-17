# Libraries ----
library(extrafont)
loadfonts()
library(devtools)
library(sidrar)
library(data.table)
library(dplyr)
library(ggtext)
library(dplyr)
library(ggplot2)
library(janitor)
library(lemon)
library(rio)
library(lubridate)
library(readr)
library(stringr)
library(hrbrthemes)
library(scales)
library(purrr)
library(rlist)
library(survey)
library(srvyr)
#devtools::install_github("lucasmation/microdadosBrasil")
library(microdadosBrasil)
library(readr)


# Download data ----

#years_to_download <- c(1992:1993,1995:1999,2001:2009,2011:2015)
years_to_download <- c(2001:2009,2011:2015)
years_to_download <- c(2013:2015)

downlonad_pnad <- function(years_to_download){
  cat('downloading', years_to_download," \n")
  download_sourceData("PNAD", years_to_download, root_path = "input/PNAD/PNADteste/", unzip = F)
  }

lapply(years_to_download, downlonad_pnad)

# Save as csv COMPLETAR! ----