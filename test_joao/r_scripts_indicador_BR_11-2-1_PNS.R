# Set working directory ------------
setwd("R:/Dropbox/bases_de_dados/PNS/2013_PNS")



###### Load Packages -----------

# options
  options(digits=10)   # number of digits to show
  options(scipen=999) # disable scientific notation


# List of packages we are going to use
list.of.packages <- 
  c(
    "downloader"
    
    # Read data 
    , "readr"
    , "foreign"
    , "readxl"
    , "SAScii"
    
    # Manipulate data frames
    , "data.table"
    , "dplyr"
    , "pbapply"
    , "tidyr"
    , "ggplot2"
    , "grid"
    , "gridExtra"
    , "cowplot"
    
    # Plots: Nice themes and colors
    , "RColorBrewer"
    , "scales"
    , "ggthemes"
    , "viridis"
    , "gtable"
    , "gghighlight"

    # Miscellaneous 
    , "beepr"
    , "bit64"
  )



# Load libraries
  lapply(list.of.packages, require, character.only = TRUE)





# 1: Donwload, save and read Pns2013 DATA----------------

# Download and unzip file
  # download.file("ftp://ftp.ibge.gov.br/PNS/2013/microdados/pns_2013_microdados_2017_03_23.zip", "PNS2013.zip", quiet = FALSE)
  # dir.create("PNS2013")
  # unzip("PNS2013.zip", exdir="PNS2013", junkpaths=T)

#Household data - Read .txt data using SAS instructions
sas_file <- "./PNS2013/input_DOMPNS2013.sas" #SAS instructions
sas_file <- parse.SAScii(sas_file)
datafile <- "./PNS2013/DOMPNS2013.txt"

# Read the .txt file
pns2013dom <-   read_fwf(datafile,
                         fwf_widths( widths    = dput(sas_file$width),
                                     col_names = dput(sas_file$varname)),
                         col_types = strrep("c", length(sas_file$varname)),
                         progress = interactive())


head(pns2013dom)

# clean memory
gc(reset = T)


#Individuals data - Read .txt data using SAS instructions
sas_file <- "./PNS2013/input_PESPNS2013.sas" #SAS instructions
sas_file <- parse.SAScii(sas_file)
datafile <- "./PNS2013/PESPNS2013.txt"

# Read the .txt file
  pns2013pes <-   read_fwf(datafile,
                         fwf_widths( widths    = dput(sas_file$width),
                                     col_names = dput(sas_file$varname)),
                         col_types = strrep("c", length(sas_file$varname)),
                         progress = interactive())

# clean memory
rm(list=setdiff(ls(), c("pns2013pes", "pns2013dom")))
gc(reset = T)


### Save 
saveRDS(pns2013pes, "./data_r/pns2013pes.rds")
saveRDS(pns2013dom, "./data_r/pns2013dom.rds")





# 3. Merge household and individual data sets---------

# Merge datasets
pns2013 <- left_join(pns2013pes, pns2013dom)





# clean memory
  #rm(list=setdiff(ls(), c("pns2013")))
  gc(reset = T)


# 4: Add Variables to pns2013 data ---------------------------------

setDT(pns2013)


  # Recode Urban Rural Variable, make it compatible with PNAD
  pns2013[, urban := ifelse(V0026=="1", "Urban", ifelse(V0026=="2", "Rural", NA)) ]


  
# year variable     
  pns2013[, year := 2013]

# Count variable     
  pns2013[, vcount := 1]
  table(pns2013$vcount)

# cria VariaVel de Regiao
  pns2013[, V0001 := as.numeric(V0001)]
  
  pns2013[V0001 < 20, region :="North"]
  pns2013[V0001 > 19 & V0001 < 30, region :="Northeast"]
  pns2013[V0001 > 29 & V0001 < 40, region :="Southwest"]
  pns2013[V0001 > 39 & V0001 < 50, region :="South"]
  pns2013[V0001 > 49 & V0001 < 60, region :="Midwest"]
  table(pns2013$region)    


  
# Recode Race variable into string
  pns2013[, C009 := as.numeric(C009)]
  
  pns2013[ C009==1 , race := "White" ]
  pns2013[ C009==2 , race := "Black" ]
  pns2013[ C009==3 , race := "Asian" ]
  pns2013[ C009==4 , race := "Brown" ]
  pns2013[ C009==5 , race := "Indigenous" ]
  pns2013[ C009==9 , race :=  NA ]
  
  table(pns2013$C009)
  table(pns2013$race)
  

# Recode Sex Variable, make it compatible with PNAD
  pns2013[, C006 := as.numeric(C006)]
  
  pns2013[ C006==1 , sex := "Men" ]
  pns2013[ C006==2 , sex := "Women" ]
  table(pns2013$sex)


# Recode UF Variable
  pns2013[, V0001 := as.numeric(V0001)]
  
  pns2013[ V0001 == 11, uf :=	"RO" ]
  pns2013[ V0001 == 12, uf :=	"AC" ]
  pns2013[ V0001 == 13, uf :=	"AM" ]
  pns2013[ V0001 == 14, uf :=	"RR" ]
  pns2013[ V0001 == 15, uf :=	"PA" ]
  pns2013[ V0001 == 16, uf :=	"AP" ]
  pns2013[ V0001 == 17, uf :=	"TO" ]
  pns2013[ V0001 == 21, uf :=	"MA" ]
  pns2013[ V0001 == 22, uf :=	"PI" ]
  pns2013[ V0001 == 23, uf :=	"CE" ]
  pns2013[ V0001 == 24, uf :=	"RN" ]
  pns2013[ V0001 == 25, uf :=	"PB" ]
  pns2013[ V0001 == 26, uf :=	"PE" ]
  pns2013[ V0001 == 27, uf :=	"AL" ]
  pns2013[ V0001 == 28, uf :=	"AL" ]
  pns2013[ V0001 == 29, uf :=	"BA" ]
  pns2013[ V0001 == 31, uf :=	"MG" ]
  pns2013[ V0001 == 32, uf :=	"ES" ]
  pns2013[ V0001 == 33, uf :=	"RJ" ]
  pns2013[ V0001 == 35, uf :=	"SP" ]
  pns2013[ V0001 == 41, uf :=	"PR" ]
  pns2013[ V0001 == 42, uf :=	"SC" ]
  pns2013[ V0001 == 43, uf :=	"RS" ]
  pns2013[ V0001 == 50, uf :=	"MS" ]
  pns2013[ V0001 == 51, uf :=	"MT" ]
  pns2013[ V0001 == 52, uf :=	"GO" ]
  pns2013[ V0001 == 53, uf :=	"DF" ]



# # Recode Metropolitan area Variable, make it compatible with PNAD
# pns2013$v4727[pns2013$V0031==1 | pns2013$V0031==2] <- 1
# pns2013$v4727[pns2013$V0031>2 ] <- 3
# 
# 
# # Create Variable Metropolitan area
# pns2013$metro[pns2013$V0031==4] <- "Non-metropolitan area"
# pns2013$metro[pns2013$V0001==15 & pns2013$V0031<3] <-"Belem"
# pns2013$metro[pns2013$V0001==23 & pns2013$V0031<3] <-"Fortaleza"
# pns2013$metro[pns2013$V0001==26 & pns2013$V0031<3] <-"Recife"
# pns2013$metro[pns2013$V0001==29 & pns2013$V0031<3] <-"Salvador"
# pns2013$metro[pns2013$V0001==31 & pns2013$V0031<3] <-"Belo Horizonte"
# pns2013$metro[pns2013$V0001==33 & pns2013$V0031<3] <-"Rio de Janeiro"
# pns2013$metro[pns2013$V0001==35 & pns2013$V0031<3] <-"Sao Paulo"
# pns2013$metro[pns2013$V0001==41 & pns2013$V0031<3] <-"Curitiba"
# pns2013$metro[pns2013$V0001==43 & pns2013$V0031<3] <-"Porto Alegre"
# pns2013$metro[pns2013$V0001==53 & pns2013$V0031<3] <-"Federal District"

  

# Recode Acctive Travel Variable P040 into string
  pns2013[, actv_trav := ifelse(P040==1, "Yes, all the journey",
                         ifelse(P040==2, "Yes, part of the journey",
                         ifelse(P040==3, "No",NA))) ]
  
  table(pns2013$P040)
  table(pns2013$actv_trav)





########## 5. Save modified DAta files  ----------------

saveRDS(pns2013, file="./data_R/pns2013.Rds")



########## 6: Calculate ODS indicator 11.2.1  ----------------
  # 
  # pns2013 <- readRDS("./data_R/pns2013.Rds")
  # head(pns2013)

# subset urban areas
  pns2013_urban <- subset(pns2013, urban == "Urban")
  
  
# converte variavel de PESO e Active travel em numerica
  pns2013_urban[, V0029 := as.numeric(V0029)]
  pns2013_urban[, P040 := as.numeric(P040)]
  
  

# Indicador Nacional
nacional <-   pns2013_urban[ , sum(V0029[which( P040 ==  1 )])      /   # total de pessoas que vao a pe/bike  
                   sum(V0029[which( !is.na(P040) )]) ]      # total de pessoas que responderam pergunta de deslocamento ativo
  

temp_nacional <- data.frame(uf="Brasil", V1=nacional)
  

estadual <-   pns2013_urban[ , sum(V0029[which( P040 ==  1 )])      /        # total de pessoas que vao a pe/bike  
                   sum(V0029[which( !is.na(P040) )]),            # total de pessoas que responderam pergunta de deslocamento ativo
                                                      by=uf ] # by UF

  
# dados de output  
output <-  rbind(estadual,  temp_nacional) %>% setDT()
output <- output[order(V1)]


# plot
  # temp_plot <- ggplot(data=output) + 
  #                   geom_bar( aes(x=reorder(uf, V1), y=V1, fill=(ifelse(uf=="Brasil", "steelblue4", "gray60"))), stat="identity") +
  #                   scale_y_continuous(labels=percent) +
  #                   theme( legend.position = "none",
  #                   axis.title = element_blank())

temp_plot <- ggplot(data=output) + 
                  geom_bar( aes(x=reorder(uf, V1), y=V1), stat="identity") +
                  gghighlight(uf=="Brasil") +
                  scale_y_continuous(labels=percent) +
                  theme_minimal() +
                  theme( legend.position = "none",
                         axis.title = element_blank())

ggsave(temp_plot, width = 20, height = 10, units="cm", dpi=300,
       filename="R:/Dropbox/IPEA/demandas_institucionais/201711_ODS/Relatorio_linha de base/plots/indicador_BR_11-2-1.png")
       

fwrite( output[order(-uf)] , "R:/Dropbox/git_projects/ods_indicadores/output_tables/tabela_11_2_1br.csv" )



