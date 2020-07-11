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

state <- geobr::read_state() %>% 
  data.table::setDT() %>% 
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
#  aggregate files
#

dtfiles <- data.table(path = c(
  "data-raw/DENATRAN/2001/Frota Tipo-Munic 2001.xls",
  "data-raw/DENATRAN/2002/Frota Tipo-Munic 2002.xls",
  "data-raw/DENATRAN/2003/Frota_Mun_Dez_03t.xls",
  "data-raw/DENATRAN/2004/Frota Munic 122004 Internet.xls",
  "data-raw/DENATRAN/2005/Frota Munic 122005 Internet.xls",
  "data-raw/DENATRAN/2006/Frota Munic DEZ2006 Internet.xls",
  "data-raw/DENATRAN/2007/Frota Munic Dez2007 Internet.xls",
  "data-raw/DENATRAN/2008/Frota Munic Dez2008.xls",
  "data-raw/DENATRAN/2009/Frota Munic Dez2009.xls",
  "data-raw/DENATRAN/2010/Frota_Municipios/Frota Munic DEZ2010.xls",
  "data-raw/DENATRAN/2011/Frota Munic. 2011/Frota Munic. DEZ.2011.xls",
  "data-raw/DENATRAN/2012/Frota Munic. 2012/Frota Munic.DEZ.2012.xls",
  "data-raw/DENATRAN/2013/Frota Munic.DEZ.2013.xls",
  "data-raw/DENATRAN/2014/Frota_Por_Municipio_e_Tipo_DEZ_2014.xlsx",
  "data-raw/DENATRAN/2015/Frota_por_Municipio_e_Tipo-DEZ_15.xls",
  "data-raw/DENATRAN/2016/Frota_por_Municipio_e_Tipo-DEZ_16.xlsx",
  "data-raw/DENATRAN/2017/Frota_Munic_Dezembro_2017.xls",
  "data-raw/DENATRAN/2018/Frota_Munic_Dezembro_2018.xls",
  "data-raw/DENATRAN/2019/Frota_Munic_Modelo_Dezembro_2019.xls",
  "data-raw/DENATRAN/2020/Frota_Munic_Modelo_Maio_2020.xls"),
  sheet = c(1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1),
  startrow = c(3,3,1,3,3,3,3,3,3,3,4,4,4,4,4,4,3,4,4,4),
  year = 2001:2020)

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

break()
#
# metropolitan region------
#

metro <- geobr::read_metro_area() %>% data.table::setDT()
metro[,name_muni := toupper_noaccent(name_muni)]
metro[,name_metro := toupper_noaccent(name_metro)]

muni_name_pns <- c("Belém","Fortaleza","Recife",  
              "Salvador","Belo Horizonte","Rio de Janeiro","São Paulo","Curitiba",
              "Porto Alegre","Distrito Federal") %>% toupper_noaccent()
metro_name_pns <- metro[name_muni %in% muni_name_pns,name_metro]

metro_pns <- metro[name_metro %in% metro_name_pns,]
# add metro area

obs[metro,on = c("MUNICIPIO" = "name_muni"), name_metro := i.name_metro]
obs[metro_pns,on = c("MUNICIPIO" = "name_muni"), name_metro_pns := i.name_metro]

#
# add municipality area --------------
#

muni_geom <- geobr::read_municipality(code_muni = "all") %>% 
  data.table::setDT()
muni_geom[,name_muni := toupper_noaccent(name_muni)]
muni_geom[,muni_uf := paste0(name_muni,"-",abbrev_state)]

obs[,MUNI_UF := paste0(MUNICIPIO,"-",UF)]

obs[muni_geom,on = c('MUNI_UF' = 'muni_uf'),geometry := geom]

#
# write files -------------------
#

dir.create("data/DENATRAN")
readr::write_rds(obs,"data/DENATRAN/DENATRAN_dez.rds")
