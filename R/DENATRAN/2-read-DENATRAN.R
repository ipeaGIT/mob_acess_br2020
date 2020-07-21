#
# initial config----------------
#

rm(list=ls())
gc(reset = T)
library(XLConnect)
source("R/PNS/0_loadpackages.R",local = TRUE)
`%nin%` = Negate(`%in%`)
ls_initial_list <- ls()

# basic list of states abbreviation

state_geobr <- geobr::read_state() %>% 
  data.table::setDT() 
state <- state_geobr %>% 
  dplyr::select(abbrev_state) %>% 
  unlist() %>% as.vector()

#
# function read individually since 2001 -------------
# 
# fix name columns
toupper_noaccent <- function(i){
  stringi::stri_trans_general(i,id = "Latin-ASCII") %>% 
    toupper()}

data_fix <- function(dt,uf,muni,year,state){
  
  # working files
  
  # wb <- XLConnect::loadWorkbook("data-raw/DENATRAN/2001/Frota Tipo-Munic 2001.xls")
  # dt <- XLConnect::readWorksheet(wb,sheet = 1,startRow = 3)
  # year = 2001
  # state = state
  # uf = "Unidades.da.Federação"
  # muni <- "Municípios"
  # 
  
  # to DT and rename
  dt <- data.table::setDT(dt)
  data.table::setnames(dt,c(uf,muni),c("UF","MUNICIPIO"))
  
  colnames(dt) <- toupper_noaccent(colnames(dt))
  dt[, UF := toupper_noaccent(UF)]
  dt[, MUNICIPIO := toupper_noaccent(MUNICIPIO)]
  
  # year and state filter 
  dt[,ANO := year]
  dt <- dt[UF %in% state,]
  
  # remove comma
  remove_comma <- function(i){gsub(",","",i)}
  dt[,colnames(dt) := lapply(.SD,remove_comma), .SDcols = colnames(dt)]
  
  # as.numeric
  numeric_cols <- colnames(dt)[colnames(dt) %nin% c("UF","MUNICIPIO")]
  dt[,(numeric_cols) := lapply(.SD,as.numeric), .SDcols = numeric_cols]
  
  # only certain columns
  veh_columns <- c("UF","MUNICIPIO",
                   "AUTOMOVEL","CAMINHONETE","CAMIONETA",
                   "MOTOCICLETA","MOTONETA","ANO")
  
  dt <- dt[,.SD,.SDcols = veh_columns]
  return(dt)
}

#
#  list files-------------------
#

dtfiles <- data.table(path = c(
  "data-raw/DENATRAN/2001/Frota Tipo-Munic 2001.xls",
  "data-raw/DENATRAN/2002/Frota Tipo-Munic 2002.xls",
  "data-raw/DENATRAN/2003/1Frota Tipo-UF Jan_03.xls",
  "data-raw/DENATRAN/2004/Frota Munic 012004 Internet.xls",
  "data-raw/DENATRAN/2005/Frota Munic 012005 Internet.xls",
  "data-raw/DENATRAN/2006/Frota Munic Jan2006 Internet.xls",
  "data-raw/DENATRAN/2007/Frota Munic Jan2007 Internet.xls",
  "data-raw/DENATRAN/2008/Frota Munic Jan2008 Internet.xls",
  "data-raw/DENATRAN/2009/Frota Munic Jan2009.xls",
  "data-raw/DENATRAN/2010/Frota_Municipios/Frota Munic Jan2010.xls",
  "data-raw/DENATRAN/2011/Frota Munic. 2011/Frota Munic. JAN 2011.xls",
  "data-raw/DENATRAN/2012/Frota Munic. 2012/Frota Munic. JAN.2012.xls",
  "data-raw/DENATRAN/2013/Frota Munic.JAN.2013.xls",
  "data-raw/DENATRAN/2014/FrotaMunicJan2014.xlsx",
  "data-raw/DENATRAN/2015/Frota_por_Municipio_e_Tipo-JAN_15.xls",
  "data-raw/DENATRAN/2016/Frota_por_Municipio_e_Tipo-JAN_16.xlsx",
  "data-raw/DENATRAN/2017/Frota_Munic_Janeiro_2017.xls",
  "data-raw/DENATRAN/2018/Frota_Munic_Janeiro_2018.xls",
  "data-raw/DENATRAN/2019/Frota_Munic_Modelo_Janeiro_2019.xls",
  "data-raw/DENATRAN/2020/Frota_Munic_Modelo_Janeiro_2020.xls"),
  sheet =    c(1  ,1   ,1   ,1   ,1    ,2   ,2   ,2   ,2   ,2   ,2   ,2   ,2   ,2   ,2   ,2   ,1   ,1   ,1   ,1   ),
  startrow = c(3  ,3   ,3   ,4   ,3    ,3   ,3   ,3   ,3   ,3   ,3   ,4   ,4   ,4   ,3   ,4   ,4   ,4   ,4   ,4   ),
  year =     c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
  )

#
# check if this reading properly--------
# PS: for january, it's okay!
#

# for(i in ){ # i = 1
#   message(dtfiles$year[i])
#   wb <- XLConnect::loadWorkbook(dtfiles$path[i])
#   temp <- XLConnect::readWorksheet(wb,sheet = dtfiles$sheet[i],startRow = dtfiles$startrow[i])
#   print(head(temp,1))
# }

#
# read and fix data ----------------
#
obs <- lapply(1:nrow(dtfiles),function(i){ # i = 1
  
  message(dtfiles$year[i])
  
  wb <- XLConnect::loadWorkbook(dtfiles$path[i])
  temp <- XLConnect::readWorksheet(wb,sheet = dtfiles$sheet[i],
                                   startRow = dtfiles$startrow[i])
  veh <- data_fix(dt = temp,
                  uf = names(temp)[1],
                  muni = names(temp)[2],
                  year = dtfiles$year[i], state = state)
  return(veh)
}) %>% data.table::rbindlist()

#
# sum categories -----
#

obs[,TOTAL_AUTOS := AUTOMOVEL + CAMINHONETE + CAMIONETA]
obs[,TOTAL_MOTOS := MOTOCICLETA + MOTONETA]

#
# add metropolitan region info------
#
break()
metro <- geobr::read_metro_area() %>% data.table::setDT()
metro[,name_muni := toupper_noaccent(name_muni)]
metro[,name_metro := toupper_noaccent(name_metro)]
metro[,muni_uf := paste0(name_muni,"-",abbrev_state)]

temp_name_pns <- c("Belém","Fortaleza","Recife",  
                   "Salvador","Belo Horizonte","Rio de Janeiro","São Paulo","Curitiba",
                   "Porto Alegre","Brasília") %>% toupper_noaccent()
uf_name_pns <- c("PA","CE","PE","BA","MG","RJ","SP","PR","RS","DF")
temp_name_pns <- paste0(temp_name_pns,"-",uf_name_pns)
temp_rm_name_pns <- metro[muni_uf %in% temp_name_pns,name_metro]


metro_pns <- metro[name_metro %in% temp_rm_name_pns,]

# add metro area

obs[,MUNI_UF := paste0(MUNICIPIO,"-",UF)]
obs[metro,on = c("MUNI_UF" = "muni_uf"), name_metro := i.name_metro]
obs[metro_pns,on = c("MUNI_UF" = "muni_uf"), name_metro_pns := i.name_metro]

# name_region

obs[state_geobr,on = c('UF' = 'abbrev_state'),name_region := i.name_region]

#
# add municipality area --------------
#

muni_geom <- geobr::read_municipality(code_muni = "all") %>% 
  data.table::setDT()
muni_geom[,name_muni := toupper_noaccent(name_muni)]
muni_geom[,muni_uf := paste0(name_muni,"-",abbrev_state)]

obs[muni_geom,on = c('MUNI_UF' = 'muni_uf'),geometry := i.geom]
obs[muni_geom,on = c('MUNI_UF' = 'muni_uf'),CODE := i.code_muni]

obs <- obs[,CODE := as.character(CODE)][!is.na(ANO),]

#
# add population info from IBGE  ---------------------
#

wb <- XLConnect::loadWorkbook("data-raw/IBGE/População Municipal 1920-2018.xls")
ibge <- XLConnect::readWorksheet(wb,sheet = 1,startRow = 1) %>% 
  data.table::setDT()
colpop <- c("Sigla","Codigo","Município",paste0("X",2001:2018))
ibge <- ibge[,.SD,.SDcols = (colpop)] 
data.table::setnames(ibge,paste0("X",2001:2018),paste0(2001:2018))
colnames(ibge) <- toupper_noaccent(colnames(ibge))
ibge[,MUNICIPIO := toupper_noaccent(MUNICIPIO)]

# ibge2019

ibge2019 <- openxlsx::read.xlsx("data-raw/IBGE/tabela6579.xlsx",startRow = 4) %>% 
  data.table::setDT()
colnames(ibge2019) <- c("LEVEL","COD","MUNICIPIO","2019")
ibge2019[,MUNICIPIO := toupper_noaccent(MUNICIPIO)]
ibge2019 <- ibge2019[,.(COD,MUNICIPIO,`2019`)]

# merge files
ibge[ibge2019,on = c('CODIGO' = 'COD'),`2019` := i.2019]

# temporarily add 2020 as 2019

ibge[,`2020` := `2019`]


#
# merge ibge pop into DENATRAN
#

obs2 <- lapply(unique(obs$ANO),function(i){ # i = 2001
  temp_obs <- obs[ANO %in% i, ]
  temp_ibge <- ibge[,.SD,.SDcols = c('CODIGO',as.character(i))]
  data.table::setnames(temp_ibge,as.character(i),'ANO')
  
  temp_obs[temp_ibge,on = c('CODE' = 'CODIGO'), POP := i.ANO]
  
  return(temp_obs)
}) %>% data.table::rbindlist()

#
# motorization rate
#
head(obs2,2)
obs2[, AUTOS_PER_POP := TOTAL_AUTOS / POP]
obs2[, MOTOS_PER_POP := TOTAL_MOTOS / POP]
obs2[, MOTO_RATE := (TOTAL_AUTOS + TOTAL_MOTOS) / POP]

#
# write files -------------------
#

dir.create("data/DENATRAN")
readr::write_rds(obs2,"data/DENATRAN/DENATRAN_jan.rds")
