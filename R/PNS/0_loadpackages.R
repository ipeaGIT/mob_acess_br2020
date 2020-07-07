


# Options
  options(digits=3)   # number of digits to show
  options(scipen=999) # disable scientific notation


#####################  List of packages -------------------------------------------------------


  
# List of packages we are going to use
list.of.packages <- 
  c(
    # download data
    "downloader"
    
    # Read data 
    , "readr"
    , "foreign"
    , "readxl"
    , "SAScii"
    
    # Manipulate data frames
    , "data.table"
    , "plyr"
    , "dplyr"
    , "fasttime"
    , "pbapply"
    , "tidyr"
    , "reshape"
    , "taRifx"
    , "sqldf"
    
    # Complex Sample Survey analysis
    , "survey"
    , "descr"
    
    # stats functions
    , "ineq"
    , "stats"
    
    # Make Plots
    , "ggplot2"
    , "grid"
    , "gridExtra"
    , "cowplot"
    , "patchwork"
    
    # Plots: Nice themes and colors
    , "RColorBrewer"
    , "scales"
    , "ggthemes"
    , "viridis"
    , "paletteer"
    , "patchwork"
    , "ggnewscale"
    , "gtable"
    , "wesanderson"
    
    # Miscellaneous 
    , "beepr"
    , "bit64"
    , "devtools"
    , "stringi"
  )

#####################  install packages -------------------------------------------------------
# install packages that are NOT already installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

  
# check installation of microdadosBrasil package
  library(devtools)
  library(stringi) 
  new.packages <- !('microdadosBrasil' %in% installed.packages()[,"Package"])
  if(length(new.packages)) devtools::install_github("lucasmation/microdadosBrasil")
  library(microdadosBrasil)

#####################  Load libraries  -------------------------------------------------------
lapply(list.of.packages, require, character.only = TRUE)

# Clean environment and memory
rm(list=ls())
gc(reset=T )









