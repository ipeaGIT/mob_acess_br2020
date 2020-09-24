# Libraries ----
source("./R/setup.R")


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