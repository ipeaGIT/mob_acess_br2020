
######## Dowload and Clean POF microdata ----------------------------------------------------------

### Setup

source("R/setup.R")

# Root directory
root_dir <- here::here('R','POF')
setwd(root_dir)

###### 0. Download and unzip original data sets -----------------

# Create folders to keep raw zipped files
dir.create('./data-raw')
dir_2017 <- paste0("./data-raw/","2017")
dir_2008 <- paste0("./data-raw/","2008")
dir_2002 <- paste0("./data-raw/","2002")
dir.create(dir_2017)
dir.create(dir_2008)
dir.create(dir_2002)

# POF 2017-2018

download.file("ftp://ftp.ibge.gov.br/Orcamentos_Familiares/Pesquisa_de_Orcamentos_Familiares_2017_2018/Microdados/Dados_20200917.zip",
              destfile = paste0(dir_2017,"/POF_2017.zip"))

unzip(paste0(dir_2017,"/POF_2017.zip"), exdir = dir_2017)

# POF 2008-2009

download.file("ftp://ftp.ibge.gov.br/Orcamentos_Familiares/Pesquisa_de_Orcamentos_Familiares_2008_2009/Microdados/Dados_20201005.zip",
              destfile = paste0(dir_2008,"/POF_2008.zip"))

unzip(paste0(dir_2008,"/POF_2008.zip"), exdir = dir_2008)


# POF 2002-2003

download.file('ftp://ftp.ibge.gov.br/Orcamentos_Familiares/Pesquisa_de_Orcamentos_Familiares_2002_2003/Microdados/Dados.zip',
              destfile = paste0(dir_2002,"/POF_2002.zip"))

unzip(paste0(dir_2002,"/POF_2002.zip"), exdir = dir_2002)


###### 1. Read Data and save in .rds format --------------------------------------


# 2017-2018 ------

# Despesa Individual

DESPESA_INDIVIDUAL <- 
  read.fwf(paste0(dir_2017,'/Arquivos de dados/DESPESA_INDIVIDUAL.txt')
           , widths = c(2,4,1,9,2,1,2,2,2,7,2,10,2
                        ,2,1,1,1,12,10,1,2,14,14,10)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC",
                           "COD_INFORMANTE", "QUADRO", "SEQ", "V9001",
                           "V9002", "V8000", "V9010", "V9011", "V9012",
                           "V4104", "V4105", "DEFLATOR", "V8000_DEFLA",
                           "COD_IMPUT_VALOR", "FATOR_ANUALIZACAO",
                           "PESO", "PESO_FINAL", "RENDA_TOTAL"
           )
           , dec="."
  )   

readr::write_rds(DESPESA_INDIVIDUAL, paste0(dir_2017,'DESPESA_INDIVIDUAL.rds'))
rm(DESPESA_INDIVIDUAL)

# Morador

MORADOR <- 
  read.fwf(paste0(dir_2017,'/Arquivos de dados/"MORADOR.txt')
           , widths = c(2,4,1,9,2,1,2,2,1,2,2,4,3,1,1,
                        1,1,1,2,1,2,1,1,1,1,1,1,1,1,1,
                        1,1,1,1,1,2,1,1,2,1,1,2,1,1,1,
                        2,1,2,14,14,10)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC",
                           "COD_INFORMANTE", "V0306", "V0401",
                           "V04021", "V04022", "V04023", "V0403",
                           "V0404", "V0405", "V0406", "V0407",
                           "V0408", "V0409", "V0410", "V0411",
                           "V0412", "V0413", "V0414", "V0415",
                           "V0416", "V041711", "V041712", "V041721",
                           "V041722", "V041731", "V041732","V041741",
                           "V041742", "V0418", "V0419", "V0420",
                           "V0421", "V0422", "V0423", "V0424",
                           "V0425", "V0426", "V0427", "V0428",
                           "V0429", "V0430", "ANOS_ESTUDO","PESO", 
                           "PESO_FINAL", "RENDA_TOTAL")
           , dec="."
  )   

readr::write_rds(MORADOR, paste0(dir_2017,'MORADOR.rds'))
rm(MORADOR)

DESPESA_COLETIVA <- 
  read.fwf(paste0(dir_2017,'/Arquivos de dados/DESPESA_COLETIVA.txt')
           , widths = c(2,4,1,9,2,1,2,2,7,2,4,10,2,2,1
                        ,10,1,12,10,10,1,1,2,14,14,10)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC", "QUADRO",
                           "SEQ", "V9001", "V9002", "V9005", "V8000",
                           "V9010", "V9011", "V9012", "V1904",
                           "V1905", "DEFLATOR", "V8000_DEFLA",
                           "V1904_DEFLA", "COD_IMPUT_VALOR",
                           "COD_IMPUT_QUANTIDADE", "FATOR_ANUALIZACAO",
                           "PESO", "PESO_FINAL", "RENDA_TOTAL"
           )
           , dec="."
  )

readr::write_rds(DESPESA_COLETIVA, paste0(dir_2017,'DESPESA_COLETIVA.rds'))
rm(DESPESA_COLETIVA)


# 2008-2009 ---------------------------

# MORADOR

MORADOR <- 
  read.fwf(paste0(dir_2008,"/Dados/T_MORADOR_S.txt")
           , widths = c(2,2,3,1,2,1,2,2,14,14,2,2,2,2,2,2,4,3,6,7,
                        2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,16,16,16,2,5,5,
                        5,5,5,5,5,16,8,
                        2,2,2,2,2,2,2,2,2,2,2,2,2)
           , na.strings=c(" ")
           , col.names = c('tipo_de_registro',"UF",'num_seq','dv_seq','num_dom',
                           'num_uc','COD_INFORMANTE',
                           "ESTRATO_POF", "fatorexpansao1", 'fatorexpansao2',
                           "condicao", "num_fam", 'cond_fam','presenca',
                           "dia", "mes", "ano",
                           "IDADE", "idade_dias", "idade_meses","SEXO", "alfab",'freq',
                           'curso','duracao','serie',
                           "instrucao", 'duracao2', "serie2",'escolaridade', "ANOS_ESTUDO", "COR",
                           "ORÇAMENTO_REND", "ORÇAMENTO_DESP", "cc", "ce", 'CCC','cccc',
                           'RENDA1', 'RENDA2', 'RENDA_TOTAL','GRAVIDA','COMPRI',
                           'ALTIMPUT','PESOIMPUT','peso2','compimput','altorig',
                           'pesorif', 'RENDAPERCAÍTA','RELIGIAO','PLANOSAUDE',
                           "planosaude2","nplanosaude", "COD_UC_AL", "MED","SAUDE", "GESTACAO", "AMAMENTA",
                           "LEITE",'ALIME', 'NMESE','FREQS', 'ALIM2')
           , dec="."
  )   

readr::write_rds(MORADOR,paste0(dir_2008,"MORADOR.rds"))
rm(MORADOR) 

# Despesa Individual

DESPESA_INDIVIDUAL <- 
  read.fwf(paste0(dir_2008,"/Dados/T_DESPESA_INDIVIDUAL_S.txt")
           , widths = c(2,2,3,1,2,1,2,2,14,14,
                        2,5,2,11,2,5,11,16,2,16,16,16,2,5,2,2)
           , na.strings=c(" ")
           , col.names = c('tipo_de_registro',"UF",'num_seq','dv_seq','num_dom',
                           'num_uc','COD_INFORMANTE',
                           "ESTRATO_POF", "fatorexpansao1", 'fatorexpansao2',
                           "QUADRO", "'COD_ITEM", "AQUISI",
                           "VALOR", "FATOR_ANUALIZACAO", "DEFLATOR", "VALOR_DEFLA",
                           "VALOR_DEFLA_ANUALIZADO",
                           "COD_IMPUT_VALOR", "RENDA1", 'RENDA2',"RENDA_TOTAL",
                           "MED", "LOCAL",'MOTIVO','UF2'
           )
           , dec="."
  )   

readr::write_rds(DESPESA_INDIVIDUAL,paste0(dir_2008,"DESPESA_INDIVIDUAL.rds"))
rm(DESPESA_INDIVIDUAL)

# Despesa Coletiva

DESPESA_12MESES <- 
  read.fwf(paste0(dir_2008,"/Dados/T_DESPESA_12MESES_S.txt")
           , widths = c(2,2,3,1,2,1,2,14,14,
                        2,5,2,11,2,2,2,5,11,16,2,16,16,16,5)
           , na.strings=c(" ")
           , col.names = c('tipo_de_registro',"UF",'num_seq','dv_seq','num_dom',
                           'num_uc',
                           "ESTRATO_POF", "fatorexpansao1", 'fatorexpansao2',
                           "QUADRO", "'COD_ITEM", "AQUISI",
                           "VALOR", 'MES1','MES2',"FATOR_ANUALIZACAO", "DEFLATOR", "VALOR_DEFLA",
                           "VALOR_DEFLA_ANUALIZADO",
                           "COD_IMPUT_VALOR", "RENDA1", 'RENDA2',"RENDA_TOTAL",
                           'CODLOCAL'
           )
           , dec="."
  )   

readr::write_rds(DESPESA_12MESES,paste0(dir_2008,"DESPESA_12MESES.rds"))
rm(DESPESA_12MESES)  


DESPESA_90DIAS <- 
  read.fwf(paste0(dir_2008,"/Dados/T_DESPESA_90DIAS_S.txt")
           , widths = c(2,2,3,1,2,1,2,14,14,
                        2,5,2,11,2,5,11,16,2,16,16,16,4,5,5,14,2,5)
           , na.strings=c(" ")
           , col.names = c('tipo_de_registro',"UF",'num_seq','dv_seq','num_dom',
                           'num_uc',
                           "ESTRATO_POF", "fatorexpansao1", 'fatorexpansao2',
                           "QUADRO", "'COD_ITEM", "AQUISI",
                           "VALOR","FATOR_ANUALIZACAO", "DEFLATOR", "VALOR_DEFLA",
                           "VALOR_DEFLA_ANUALIZADO",
                           "COD_IMPUT_VALOR", "RENDA1", 'RENDA2',"RENDA_TOTAL",
                           'Q','W','E','R','T',
                           'CODLOCAL'
           )
           , dec="."
  )   

readr::write_rds(DESPESA_90DIAS,paste0(dir_2008,"DESPESA_90DIAS.rds"))
rm(DESPESA_90DIAS)  


# 2002 - 2003 -----------------

# MORADOR

MORADOR <- 
  read.fwf(paste0(dir_2002,"/T_MORADOR.txt") 
           , widths = c(2,2,3,1,2,1,2,11,11,2,1,1,1,2,2,4,3,7,6,
                        1,2,2,2,1,3,10,10,1,1,1,1,1,2,12,1,5,5,3)
           , na.strings=c(" ")
           , col.names = c('tipo_de_registro',"UF",'num_seq','dv_seq','num_dom',
                           'num_uc',
                           "ESTRATO_POF", "fatorexpansao1", 'fatorexpansao2',
                           "COD_INFORMANTE", "relacao", "presença",
                           "SEXO", "dia", "mes", "ano",
                           "IDADE", "idade_dias", "idade_meses", "freq",
                           "instrucao", "serie", "ANOS_ESTUDO", "COR",
                           "RELIGIAO", "PESO", "ALTURA", "ORÇAMENTO_DESP",
                           "ORÇAMENTO_REND", "cc", "ce", "planosaude",
                           "nplanosaude", "RENDA_TOTAL", "codimput","pesoimput",
                           "altimput", "retricao")
           , dec="."
  )   

readr::write_rds(MORADOR,paste0(dir_2002,"MORADOR.rds"))
rm(MORADOR) 


# DESPESA INDIVIDUAL

DESPESA_INDIVIDUAL <- 
  read.fwf(paste0(dir_2002,"/T_DESPESA.txt")
           , widths = c(2,2,3,1,2,1,2,11,11,2,2,5,5,11,1,2,16,12,1,5,5,5,5,1,1)
           , na.strings=c(" ")
           , col.names = c('tipo_de_registro',"UF",'num_seq','dv_seq','num_dom',
                           'num_uc',
                           "ESTRATO_POF", "fatorexpansao1", 'fatorexpansao2',
                           "COD_INFORMANTE", "QUADRO", "COD_ITEM",
                           "LOCALCOMPRA", "VALOR", "AQUISICAO", "FATOR_ANUALIZACAO",
                           "VALOR_DEFLA_ANUALIZADO", "RENDA_TOTAL", "codimput","DEFLATOR",
                           "QUANT", "UNIDADEMEDIDA",'PESOVOLUME','ORIGEM','CARAC'
           )
           , dec="."
  )   

readr::write_rds(DESPESA_INDIVIDUAL,paste0(dir_2002,"DESPESA_INDIVIDUAL.rds"))
rm(DESPESA_INDIVIDUAL)

DESPESA_90DIAS <- 
  read.fwf(paste0(dir_2002,"/T_DESPESA_90DIAS.txt" )
           , widths = c(2,2,3,1,2,1,2,11,11,2,2,5,5,11,1,2,16,12,1,5,5,1,5)
           , na.strings=c(" ")
           , col.names = c('tipo_de_registro',"UF",'num_seq','dv_seq','num_dom',
                           'num_uc',
                           "ESTRATO_POF", "fatorexpansao1", 'fatorexpansao2',
                           "COD_INFORMANTE", "QUADRO", "COD_ITEM",
                           "LOCALCOMPRA", "VALOR", "AQUISICAO", "FATOR_ANUALIZACAO",
                           "VALOR_DEFLA_ANUALIZADO", "RENDA_TOTAL", "codimput","DEFLATOR",
                           "QUANT", "UN",'UNN'
           )
           , dec="."
  )   

readr::write_rds(DESPESA_90DIAS,paste0(dir_2002,"DESPESA_90DIAS.rds"))
rm(DESPESA_90DIAS)  

DESPESA_12MESES <- 
  read.fwf(paste0(dir_2002,"/T_DESPESA_12MESES.txt" )
           , widths = c(2,2,3,1,2,1,2,11,11,2,2,5,5,11,2,1,2,16,12,1,5)
           , na.strings=c(" ")
           , col.names = c('tipo_de_registro',"UF",'num_seq','dv_seq','num_dom',
                           'num_uc',
                           "ESTRATO_POF", "fatorexpansao1", 'fatorexpansao2',
                           "COD_INFORMANTE", "QUADRO", "COD_ITEM",
                           "LOCALCOMPRA", "VALOR", 'NMESES',"AQUISICAO", "FATOR_ANUALIZACAO",
                           "VALOR_DEFLA_ANUALIZADO", "RENDA_TOTAL", "codimput","DEFLATOR"
           )
           , dec="."
  )   

readr::write_rds(DESPESA_12MESES,paste0(dir_2002,"DESPESA_12MESES.rds"))
rm(DESPESA_12MESES)  
