### Setup

source("setup.R")

######## 1. Dowload and Clean data ----------------------------------------------------------

######################################################################
### POF 2017-2018 ####################################################
######################################################################

setwd("~/POF/2017_2018")

# Despesa Individual ----------------

despesa_individual <- 
  # Recuperando microdados de despesa
  readRDS("DESPESA_INDIVIDUAL.rds")

pof_2017_total_familias <-        
  despesa_individual %>%    
  # Seleciona as variáveis
  dplyr::select(             
    UF, ESTRATO_POF, COD_UPA,
    NUM_DOM, NUM_UC, QUADRO,
    N_MESES = V9011, VALOR_DEFLA = V8000_DEFLA,
    FATOR_ANUALIZACAO, PESO_FINAL, RENDA_TOTAL
  ) %>%
  dplyr::mutate(
    ID_FAMILIA = paste(COD_UPA, NUM_DOM, NUM_UC, sep = ""),
    CONSUMO_ANUAL = 
      ifelse(is.na(N_MESES),
      VALOR_DEFLA * FATOR_ANUALIZACAO,
      VALOR_DEFLA * N_MESES * FATOR_ANUALIZACAO),
    RENDA_ANUAL = RENDA_TOTAL * 12 
  )  %>%
  dplyr::filter(
    QUADRO == '23' | QUADRO == '33' | QUADRO == '50' | # Transporte
    QUADRO == '24' | # Alimentação
    QUADRO == '28' | # Cultura, Lazer e Esporte
    QUADRO == '34' | QUADRO == '35' | QUADRO == '36' | QUADRO == '38'  # Roupas
  ) %>% 
  dplyr::select(
    UF, ESTRATO_POF, RENDA_ANUAL, QUADRO, 
    PESO_FINAL, ID_FAMILIA, CONSUMO_ANUAL
  )

# Despesa Coletiva ----------------

despesa_coletiva <- 
  # Recuperando microdados de despesa
  readRDS("DESPESA_COLETIVA.rds")

despesa_coletiva <-
  despesa_coletiva %>% 
  dplyr::select(             
    UF, ESTRATO_POF, COD_UPA,
    NUM_DOM, NUM_UC, QUADRO,
    N_MESES = V9011, VALOR_DEFLA = V8000_DEFLA,
    FATOR_ANUALIZACAO, PESO_FINAL, RENDA_TOTAL
  ) %>%
  dplyr::mutate(
    ID_FAMILIA = paste(COD_UPA, NUM_DOM, NUM_UC, sep = ""),
    CONSUMO_ANUAL = ifelse(is.na(N_MESES),
    VALOR_DEFLA * FATOR_ANUALIZACAO,
    VALOR_DEFLA * N_MESES * FATOR_ANUALIZACAO),
    RENDA_ANUAL = RENDA_TOTAL * 12 
  )  %>%
  # Filtra Quadros de interesse
  dplyr::filter(
    QUADRO == '6' | QUADRO == '7' | QUADRO == '8' | # Habitação
    QUADRO == '9' | QUADRO == '10' | QUADRO == '12' | QUADRO == '19'
  ) %>% 
  dplyr::select(
    UF, ESTRATO_POF, RENDA_ANUAL, QUADRO,
    PESO_FINAL, ID_FAMILIA, CONSUMO_ANUAL
  )

pof_2017_total_familias <-
  bind_rows(pof_2017_total_familias, despesa_coletiva) 

# 2. Recode Data -------------------------------------------------------

pof_2017_total_familias <- 
  pof_2017_total_familias %>% 
  # Cria classes para os Estratos Urbano, Rural, RM e interior
  dplyr::mutate(
    ESTRATO_POF = as.numeric(ESTRATO_POF),
    Estrato = dplyr::case_when(
    ESTRATO_POF == 1101 | ESTRATO_POF == 1102 | ESTRATO_POF == 1201 |
    ESTRATO_POF >= 1301 & ESTRATO_POF <= 1306 | ESTRATO_POF == 1401 & ESTRATO_POF == 1402 |
    ESTRATO_POF >= 1501 & ESTRATO_POF <= 1503 | ESTRATO_POF >= 1601 & ESTRATO_POF <= 1602 |
    ESTRATO_POF == 1701 | ESTRATO_POF >= 2101 & ESTRATO_POF <= 2103 |
    ESTRATO_POF >= 2201 & ESTRATO_POF <= 2203 | ESTRATO_POF >= 2301 & ESTRATO_POF <= 2306 |
    ESTRATO_POF >= 2401 & ESTRATO_POF <= 2402 | ESTRATO_POF >= 2501 & ESTRATO_POF <= 2503 |
    ESTRATO_POF >= 2601 & ESTRATO_POF <= 2603 | ESTRATO_POF >= 2701 & ESTRATO_POF <= 2703 |
    ESTRATO_POF >= 2801 & ESTRATO_POF <= 2802 | ESTRATO_POF >= 2901 & ESTRATO_POF <= 2906 |
    ESTRATO_POF >= 3101 & ESTRATO_POF <= 3106 | ESTRATO_POF >= 3201 & ESTRATO_POF <= 3202 |
    ESTRATO_POF >= 3301 & ESTRATO_POF <= 3309 | ESTRATO_POF >= 3501 & ESTRATO_POF <= 3509 |
    ESTRATO_POF >= 4101 & ESTRATO_POF <= 4105 | ESTRATO_POF >= 4201 & ESTRATO_POF <= 4202 |
    ESTRATO_POF >= 4301 & ESTRATO_POF <= 4306 | ESTRATO_POF >= 5001 & ESTRATO_POF <= 5003 |
    ESTRATO_POF >= 5101 & ESTRATO_POF <= 5102 | ESTRATO_POF >= 5201 & ESTRATO_POF <= 5203 |
    ESTRATO_POF >= 5301 & ESTRATO_POF <= 5306 ~ "Capital",
    ESTRATO_POF == 1307 | ESTRATO_POF >= 1504 & ESTRATO_POF <= 1505 |
    ESTRATO_POF == 1603 | ESTRATO_POF == 2104 | ESTRATO_POF >= 2307 & ESTRATO_POF <= 2309 |
    ESTRATO_POF == 2403 | ESTRATO_POF >= 2504 & ESTRATO_POF <= 2505 |
    ESTRATO_POF >= 2604 & ESTRATO_POF <= 2606 | ESTRATO_POF == 2704 |
    ESTRATO_POF == 2803 | ESTRATO_POF >= 2907 & ESTRATO_POF <= 2909 |
    ESTRATO_POF >= 3107 & ESTRATO_POF <= 3109 | ESTRATO_POF >= 3203 & ESTRATO_POF <= 3205 |
    ESTRATO_POF >= 3310 & ESTRATO_POF <= 3318 | ESTRATO_POF >= 3510 & ESTRATO_POF <= 3515 |
    ESTRATO_POF >= 4106 & ESTRATO_POF <= 4108 | ESTRATO_POF >= 4203 & ESTRATO_POF <= 4204 |
    ESTRATO_POF >= 4307 & ESTRATO_POF <= 4309 | ESTRATO_POF == 5103 |
    ESTRATO_POF >= 5204 & ESTRATO_POF <= 5206 ~ 'RM da Capital',
    ESTRATO_POF == 1103 | ESTRATO_POF == 1107 | ESTRATO_POF == 1202 |
    ESTRATO_POF >= 1308 & ESTRATO_POF <= 1310 | ESTRATO_POF == 1403 |
    ESTRATO_POF >= 1506 & ESTRATO_POF <= 1511 | ESTRATO_POF == 1604 |
    ESTRATO_POF >= 1702 & ESTRATO_POF <= 1705 | ESTRATO_POF >= 2105 & ESTRATO_POF <= 2113 |
    ESTRATO_POF >= 2204 & ESTRATO_POF <= 2209 | ESTRATO_POF >= 2310 & ESTRATO_POF <= 2320 |
    ESTRATO_POF >= 2404 & ESTRATO_POF <= 2408 | ESTRATO_POF >= 2506 & ESTRATO_POF <= 2511 |
    ESTRATO_POF >= 2607 & ESTRATO_POF <= 2615 | ESTRATO_POF >= 2705 & ESTRATO_POF <= 2708 |
    ESTRATO_POF >= 2804 & ESTRATO_POF <= 2806 | ESTRATO_POF >= 2910 & ESTRATO_POF <= 2925 |
    ESTRATO_POF >= 3110 & ESTRATO_POF <= 3130 | ESTRATO_POF >= 3206 & ESTRATO_POF <= 3211 |
    ESTRATO_POF >= 3319 & ESTRATO_POF <= 3330 | ESTRATO_POF >= 3516 & ESTRATO_POF <= 3536 |
    ESTRATO_POF >= 4109 & ESTRATO_POF <= 4124 | ESTRATO_POF >= 4205 & ESTRATO_POF <= 4217 |
    ESTRATO_POF >= 4310 & ESTRATO_POF <= 4324 | ESTRATO_POF >= 5004 & ESTRATO_POF <= 5009 |
    ESTRATO_POF >= 5104 & ESTRATO_POF <= 5112 | ESTRATO_POF >= 5207 & ESTRATO_POF <= 5217 ~
    "Interior Urbano", TRUE ~ "Interior Rural")
  )

pof_2017_total_familias <-
  pof_2017_total_familias %>% 
  mutate(
    Grupo = case_when(
    QUADRO == '23' | QUADRO == '33' | QUADRO == '50' ~ 'Transporte Urbano',
    QUADRO == '24' ~ 'Alimentação',
    QUADRO == '28' ~ 'Cultura/Lazer/Esporte',
    QUADRO == '34' | QUADRO == '35' | 
    QUADRO == '36' | QUADRO == '38' ~ 'Roupas e Calçados',
    TRUE ~ 'Habitação'),
    Ano = '2017'
  ) 

pof_2017_total_familias$UF <- 
  # Recodifica nome dos estados
  dplyr::recode(pof_2017_total_familias$UF,
  "11" =  "RO", "12" =  "AC", "13" =  "AM",
  "14" =  "RR", "15" =  "PA", "16" =  "AP",
  "17" =  "TO", "21" =  "MA", "22" =  "PI",
  "23" =  "CE", "24" =  "RN", "25" =  "PB",
  "26" =  "PE", "27" =  "AL", "28" =  "SE",
  "29" =  "BA", "31" =  "MG", "32" =  "ES",
  "33" =  "RJ", "35" =  "SP", "41" =  "PR",
  "42" =  "SC", "43" =  "RS", "50" =  "MS",
  "51" =  "MT", "52" =  "GO", "53" =  "DF"
  )

setwd("C:/Users/lucas/Desktop/R/pof")
readr::write_rds(pof_2017_total_familias, 'pof_agregado_2017.rds')

############################################
############################################
### POF 2008-2009 ##########################
############################################

setwd("~/POF/2008_2009")

despesa_individual <-
  # Recuperando microdados de despesa
  readRDS("DESPESA_INDIVIDUAL.rds")

pof_2008_total_familias <-
  despesa_individual %>%
  dplyr::select(
    UF, ESTRATO_POF, NUM_SEQ = num_seq, DV_SEQ = dv_seq, NUM_DOM = num_dom,
    NUM_UC  = num_uc, QUADRO, VALOR_DEFLA, FATOR_ANUALIZACAO, 
    PESO_FINAL  = fatorexpansao2, RENDA_TOTAL
  ) %>%
  dplyr::mutate(
    ID_FAMILIA = paste(UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, sep = ""),
    CONSUMO_ANUAL = VALOR_DEFLA * FATOR_ANUALIZACAO,
    RENDA_ANUAL = RENDA_TOTAL * 12 
  ) %>%
  # Filtra Quadros de interesse
  dplyr::filter(
    QUADRO == '23' | QUADRO == '43' | QUADRO == '50' | # Transporte
    QUADRO == '24' | # Alimentação
    QUADRO == '28' | # Cultura, Lazer e Esporte
    QUADRO == '34' | QUADRO == '35' | QUADRO == '36' | QUADRO == '38'  # Roupas
  ) %>% 
  dplyr::select(
    UF, ESTRATO_POF, RENDA_ANUAL, QUADRO,
    PESO_FINAL, ID_FAMILIA, CONSUMO_ANUAL
  ) 

# Despesa 90 dias ----------------

despesa_90dias <- 
  # Recuperando microdados de despesa
  readRDS("DESPESA_90DIAS.rds")

despesa_90dias <-
  despesa_90dias %>%
  select(
  UF, ESTRATO_POF, NUM_SEQ = num_seq,
  DV_SEQ = dv_seq, NUM_DOM = num_dom,
  NUM_UC  = num_uc, QUADRO, VALOR_DEFLA,
  FATOR_ANUALIZACAO, PESO_FINAL  = fatorexpansao2,
  RENDA_TOTAL
  ) %>%
  dplyr::mutate(
    ID_FAMILIA = paste(UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, sep = ""),
    CONSUMO_ANUAL = VALOR_DEFLA * FATOR_ANUALIZACAO,
    RENDA_ANUAL = RENDA_TOTAL * 12 
  ) %>% 
  dplyr::select(
    UF, ESTRATO_POF, RENDA_ANUAL,
    QUADRO, PESO_FINAL, ID_FAMILIA,
    CONSUMO_ANUAL
  )  

# Despesa 12 meses ----------------

despesa_12meses <- 
  # Recuperando microdados de despesa
  readRDS("DESPESA_12MESES.rds")

despesa_12meses <-
  despesa_12meses %>%
  select(
    UF, ESTRATO_POF, NUM_SEQ = num_seq,
    DV_SEQ = dv_seq, NUM_DOM = num_dom,
    NUM_UC  = num_uc, QUADRO, VALOR_DEFLA, FATOR_ANUALIZACAO, 
    PESO_FINAL  = fatorexpansao2, RENDA_TOTAL 
  ) %>%
  dplyr::mutate(
    ID_FAMILIA = paste(UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, sep = ""),
    CONSUMO_ANUAL = VALOR_DEFLA * FATOR_ANUALIZACAO,
    RENDA_ANUAL = RENDA_TOTAL * 12 
  ) %>%
  # Filtra Quadros de interesse
  dplyr::filter(
    QUADRO == '10' | QUADRO == '12'
  ) %>% 
  dplyr::select(
    UF, ESTRATO_POF, RENDA_ANUAL, QUADRO, 
    PESO_FINAL, ID_FAMILIA, CONSUMO_ANUAL
  ) 

pof_2008_total_familias <-
  dplyr::bind_rows(pof_2008_total_familias, despesa_90dias, despesa_12meses)

# 2. Recode Data -------------------------------------------------------

pof_2008_total_familias <-
  pof_2008_total_familias %>%
  dplyr::mutate(
    ESTRATO_POF = as.numeric(
      ifelse(ESTRATO_POF < 10, paste(UF, ESTRATO_POF, sep = "0"), paste0(UF, ESTRATO_POF))),
    Estrato = dplyr::case_when(
    ESTRATO_POF == 1101 | ESTRATO_POF == 1102 | ESTRATO_POF == 1201 |
    ESTRATO_POF >= 1301 & ESTRATO_POF <= 1304 | ESTRATO_POF == 1401 |
    ESTRATO_POF == 1501 | ESTRATO_POF == 1502 | ESTRATO_POF == 1601 |
    ESTRATO_POF == 1701 | ESTRATO_POF >= 2101 & ESTRATO_POF <= 2103 |
    ESTRATO_POF >= 2201 & ESTRATO_POF <= 2203 | ESTRATO_POF >= 2301 & ESTRATO_POF <= 2309 |
    ESTRATO_POF >= 2401 & ESTRATO_POF <= 2402 | ESTRATO_POF >= 2501 & ESTRATO_POF <= 2503 |
    ESTRATO_POF >= 2601 & ESTRATO_POF <= 2603 | ESTRATO_POF >= 2701 & ESTRATO_POF <= 2703 |
    ESTRATO_POF >= 2801 & ESTRATO_POF <= 2802 | ESTRATO_POF >= 2901 & ESTRATO_POF <= 2906 |
    ESTRATO_POF >= 3101 & ESTRATO_POF <= 3106 | ESTRATO_POF == 3201 |
    ESTRATO_POF >= 3301 & ESTRATO_POF <= 3309 | ESTRATO_POF >= 3501 & ESTRATO_POF <= 3509 |
    ESTRATO_POF >= 4101 & ESTRATO_POF <= 4106 | ESTRATO_POF >= 4201 & ESTRATO_POF <= 4202 |
    ESTRATO_POF >= 4301 & ESTRATO_POF <= 4306 | ESTRATO_POF >= 5001 & ESTRATO_POF <= 5003 |
    ESTRATO_POF >= 5101 & ESTRATO_POF <= 5103 | ESTRATO_POF >= 5201 & ESTRATO_POF <= 5203 |
    ESTRATO_POF >= 5301 & ESTRATO_POF <= 5307 ~ "Capital",
    ESTRATO_POF == 1503 | ESTRATO_POF == 1504 | ESTRATO_POF >= 2310 & ESTRATO_POF <= 2312 |
    ESTRATO_POF >= 2604 & ESTRATO_POF <= 2606 | ESTRATO_POF >= 2907 & ESTRATO_POF <= 2908 |
    ESTRATO_POF >= 3107 & ESTRATO_POF <= 3109 | ESTRATO_POF >= 3310 & ESTRATO_POF <= 3318 |
    ESTRATO_POF >= 3510 & ESTRATO_POF <= 3515 | ESTRATO_POF >= 4107 & ESTRATO_POF <= 4109 |
    ESTRATO_POF >= 4307 & ESTRATO_POF <= 4309 ~ 'RM da Capital',
    ESTRATO_POF >= 1103 & ESTRATO_POF <= 1106 | ESTRATO_POF == 1202 |
    ESTRATO_POF >= 1305 & ESTRATO_POF <= 1308 | ESTRATO_POF == 1402 |
    ESTRATO_POF >= 1505 & ESTRATO_POF <= 1508 | ESTRATO_POF == 1602 | ESTRATO_POF == 1603 |
    ESTRATO_POF >= 1702 & ESTRATO_POF <= 1705 | ESTRATO_POF >= 2104 & ESTRATO_POF <= 2112 |
    ESTRATO_POF >= 2204 & ESTRATO_POF <= 2209 | ESTRATO_POF >= 2313 & ESTRATO_POF <= 2323 |
    ESTRATO_POF >= 2403 & ESTRATO_POF <= 2408 | ESTRATO_POF >= 2504 & ESTRATO_POF <= 2509 |
    ESTRATO_POF >= 2607 & ESTRATO_POF <= 2615 | ESTRATO_POF >= 2704 & ESTRATO_POF <= 2708 |
    ESTRATO_POF >= 2803 & ESTRATO_POF <= 2807 | ESTRATO_POF >= 2909 & ESTRATO_POF <= 2921 |
    ESTRATO_POF >= 3110 & ESTRATO_POF <= 3127 | ESTRATO_POF >= 3202 & ESTRATO_POF <= 3209 |
    ESTRATO_POF >= 3319 & ESTRATO_POF <= 3330 | ESTRATO_POF >= 3516 & ESTRATO_POF <= 3530 |
    ESTRATO_POF >= 4110 & ESTRATO_POF <= 4118 | ESTRATO_POF >= 4203 & ESTRATO_POF <= 4213 |
    ESTRATO_POF >= 4310 & ESTRATO_POF <= 4318 | ESTRATO_POF >= 5004 & ESTRATO_POF <= 5008 |
    ESTRATO_POF >= 5104 & ESTRATO_POF <= 5110 | ESTRATO_POF >= 5204 & ESTRATO_POF <= 5217 ~
    "Interior Urbano", TRUE ~ "Interior Rural")
  )

pof_2008_total_familias <-
  pof_2008_total_familias %>% 
  mutate(
    Grupo = dplyr::case_when(
      QUADRO == '23' | QUADRO == '43' |
      QUADRO == '50' ~ 'Transporte Urbano',
      QUADRO == '24' ~ 'Alimentação',
      QUADRO == '28' ~ 'Cultura/Lazer/Esporte',
      QUADRO == '34' | QUADRO == '35' |
      QUADRO == '36' | QUADRO == '38' ~ 'Roupas e Calçados',
      TRUE ~ 'Habitação'),
    Ano = '2008')

pof_2008_total_familias$UF <- 
  dplyr::recode(pof_2008_total_familias$UF,
  "11" =  "RO", "12" =  "AC", "13" =  "AM",
  "14" =  "RR", "15" =  "PA", "16" =  "AP",
  "17" =  "TO", "21" =  "MA", "22" =  "PI",
  "23" =  "CE", "24" =  "RN", "25" =  "PB",
  "26" =  "PE", "27" =  "AL", "28" =  "SE",
  "29" =  "BA", "31" =  "MG", "32" =  "ES",
  "33" =  "RJ", "35" =  "SP", "41" =  "PR",
  "42" =  "SC", "43" =  "RS", "50" =  "MS",
  "51" =  "MT", "52" =  "GO", "53" =  "DF"
  )

setwd("C:/Users/lucas/Desktop/R/pof")
readr::write_rds(pof_2008_total_familias, 'pof_agregado_2008.rds')

############################################
############################################
### POF 2002-2003 ##########################
############################################

setwd("~/POF/2002_2003")

despesa_individual <-
  readRDS("DESPESA_INDIVIDUAL.rds")

pof_2002_total_familias <-
  despesa_individual %>%
  dplyr::select(
  UF, ESTRATO_POF, NUM_SEQ = num_seq, DV_SEQ = dv_seq,
  NUM_DOM = num_dom, NUM_UC  = num_uc, QUADRO, 
  VALOR_DEFLA = VALOR_DEFLA_ANUALIZADO, FATOR_ANUALIZACAO, 
  PESO_FINAL  = fatorexpansao2, RENDA_TOTAL
  ) %>%
  dplyr::mutate(
    ID_FAMILIA = paste(UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, sep = ""),
    CONSUMO_ANUAL = VALOR_DEFLA,
    RENDA_ANUAL = RENDA_TOTAL * 12 
  ) %>%
  # Filtra Quadros de interesse
  dplyr::filter(
  QUADRO == '23' | QUADRO == '43' | QUADRO == '50' | # Transporte
  QUADRO == '24' | # Alimentação
  QUADRO == '28' | # Cultura, Lazer e Esporte
  QUADRO == '34' | QUADRO == '35' | QUADRO == '36' | QUADRO == '38'  # Roupas
  ) %>% 
  dplyr::select(
    UF, ESTRATO_POF, RENDA_ANUAL, QUADRO,
    PESO_FINAL, ID_FAMILIA, CONSUMO_ANUAL
  ) 

# Despesa 90 dias -----------------

despesa_90dias <-
  readRDS("DESPESA_90DIAS.rds")

despesa_90dias <-
  despesa_90dias %>%
  dplyr::select(
    UF, ESTRATO_POF, NUM_SEQ = num_seq,
    DV_SEQ = dv_seq, NUM_DOM = num_dom,
    NUM_UC  = num_uc, QUADRO, VALOR_DEFLA = VALOR_DEFLA_ANUALIZADO,
    FATOR_ANUALIZACAO, PESO_FINAL  = fatorexpansao2, RENDA_TOTAL
  ) %>%
  dplyr::mutate(
    ID_FAMILIA = paste(UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, sep = ""),
    CONSUMO_ANUAL = VALOR_DEFLA,
    RENDA_ANUAL = RENDA_TOTAL * 12 
  ) %>%
  dplyr::select(
    UF, ESTRATO_POF, RENDA_ANUAL,
    QUADRO, PESO_FINAL, ID_FAMILIA, CONSUMO_ANUAL
  ) 

# Despesa 12 meses ------------------------------

despesa_12meses <-
  readRDS("DESPESA_12MESES.rds")

despesa_12meses <-
  despesa_12meses %>%
  dplyr::select(
  UF, ESTRATO_POF, NUM_SEQ = num_seq,
  DV_SEQ = dv_seq, NUM_DOM = num_dom,
  NUM_UC  = num_uc, QUADRO, VALOR_DEFLA = VALOR_DEFLA_ANUALIZADO,
  FATOR_ANUALIZACAO, PESO_FINAL  = fatorexpansao2, RENDA_TOTAL
  ) %>%
  dplyr::mutate(
    ID_FAMILIA = paste(UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, sep = ""),
    CONSUMO_ANUAL = VALOR_DEFLA,
    RENDA_ANUAL = RENDA_TOTAL * 12 
  ) %>%
  dplyr::filter(QUADRO == '10'|QUADRO == '12') %>% 
  dplyr::select(
    UF, ESTRATO_POF, RENDA_ANUAL, QUADRO, PESO_FINAL,
    ID_FAMILIA, CONSUMO_ANUAL
  ) 

pof_2002_total_familias <-
  bind_rows(pof_2002_total_familias, despesa_90dias, despesa_12meses)

# 2. Recode Data -----------------------------------

pof_2002_total_familias <-
  pof_2002_total_familias %>%
  dplyr::mutate(
    ESTRATO_POF = as.numeric(
      ifelse(ESTRATO_POF < 10, paste(UF, ESTRATO_POF, sep = "0"), paste0(UF, ESTRATO_POF))),
    Estrato = dplyr::case_when(
    ESTRATO_POF >= 1101 & ESTRATO_POF <= 1105 | ESTRATO_POF >= 1201 & ESTRATO_POF <= 1204 |
    ESTRATO_POF >= 1301 & ESTRATO_POF <= 1305 | ESTRATO_POF >= 1401 & ESTRATO_POF <= 1404 |
    ESTRATO_POF >= 1501 & ESTRATO_POF <= 1505 | ESTRATO_POF >= 1601 & ESTRATO_POF <= 1605 |
    ESTRATO_POF >= 1701 & ESTRATO_POF <= 1705 | ESTRATO_POF >= 2101 & ESTRATO_POF <= 2105 |
    ESTRATO_POF >= 2201 & ESTRATO_POF <= 2205 | ESTRATO_POF >= 2301 & ESTRATO_POF <= 2305 |
    ESTRATO_POF >= 2401 & ESTRATO_POF <= 2405 | ESTRATO_POF >= 2501 & ESTRATO_POF <= 2505 |
    ESTRATO_POF >= 2601 & ESTRATO_POF <= 2605 | ESTRATO_POF >= 2701 & ESTRATO_POF <= 2705 |
    ESTRATO_POF >= 2801 & ESTRATO_POF <= 2805 | ESTRATO_POF >= 2901 & ESTRATO_POF <= 2905 |
    ESTRATO_POF >= 3101 & ESTRATO_POF <= 3105 | ESTRATO_POF >= 3201 & ESTRATO_POF <= 3205 |
    ESTRATO_POF >= 3301 & ESTRATO_POF <= 3310 | ESTRATO_POF >= 3501 & ESTRATO_POF <= 3510 |
    ESTRATO_POF >= 4101 & ESTRATO_POF <= 4105 | ESTRATO_POF >= 4201 & ESTRATO_POF <= 4205 |
    ESTRATO_POF >= 4301 & ESTRATO_POF <= 4305 | ESTRATO_POF >= 5001 & ESTRATO_POF <= 5005 |
    ESTRATO_POF >= 5101 & ESTRATO_POF <= 5105 | ESTRATO_POF >= 5201 & ESTRATO_POF <= 5205 |
    ESTRATO_POF >= 5301 & ESTRATO_POF <= 5305 ~ "Capital",
    ESTRATO_POF >= 1506 & ESTRATO_POF <= 1509 | ESTRATO_POF >= 2306 & ESTRATO_POF <= 2310 |
    ESTRATO_POF >= 2606 & ESTRATO_POF <= 2610 | ESTRATO_POF >= 2906 & ESTRATO_POF <= 2910 |
    ESTRATO_POF >= 3106 & ESTRATO_POF <= 3110 | ESTRATO_POF >= 3311 & ESTRATO_POF <= 3320 |
    ESTRATO_POF >= 3511 & ESTRATO_POF <= 3520 | ESTRATO_POF >= 4106 & ESTRATO_POF <= 4110 |
    ESTRATO_POF >= 4306 & ESTRATO_POF <= 4310 ~ 'RM da Capital',
    ESTRATO_POF >= 1106 & ESTRATO_POF <= 1109 | ESTRATO_POF >= 1205 & ESTRATO_POF <= 1108 |
    ESTRATO_POF >= 1306 & ESTRATO_POF <= 1310 | ESTRATO_POF >= 1405 & ESTRATO_POF <= 1406 |
    ESTRATO_POF >= 1510 & ESTRATO_POF <= 1514 | ESTRATO_POF >= 1606 & ESTRATO_POF <= 1608 |
    ESTRATO_POF >= 1706 & ESTRATO_POF <= 1710 | ESTRATO_POF >= 2106 & ESTRATO_POF <= 2110 |
    ESTRATO_POF >= 2206 & ESTRATO_POF <= 2210 | ESTRATO_POF >= 2311 & ESTRATO_POF <= 2315 |
    ESTRATO_POF >= 2406 & ESTRATO_POF <= 2410 | ESTRATO_POF >= 2506 & ESTRATO_POF <= 2510 |
    ESTRATO_POF >= 2611 & ESTRATO_POF <= 2615 | ESTRATO_POF >= 2706 & ESTRATO_POF <= 2710 |
    ESTRATO_POF >= 2806 & ESTRATO_POF <= 2809 | ESTRATO_POF >= 2911 & ESTRATO_POF <= 2915 |
    ESTRATO_POF >= 3111 & ESTRATO_POF <= 3114 | ESTRATO_POF >= 3206 & ESTRATO_POF <= 3210 |
    ESTRATO_POF >= 3321 & ESTRATO_POF <= 3325 | ESTRATO_POF >= 3521 & ESTRATO_POF <= 3525 |
    ESTRATO_POF >= 4111 & ESTRATO_POF <= 4115 | ESTRATO_POF >= 4206 & ESTRATO_POF <= 4210 |
    ESTRATO_POF >= 4311 & ESTRATO_POF <= 4315 | ESTRATO_POF >= 5006 & ESTRATO_POF <= 5010 |
    ESTRATO_POF >= 5106 & ESTRATO_POF <= 5110 | ESTRATO_POF >= 5206 & ESTRATO_POF <= 5210 |
    ESTRATO_POF >= 5306 & ESTRATO_POF <= 5310 ~ "Interior Urbano",
    TRUE ~  "Interior Rural")
  )

pof_2002_total_familias <-
  pof_2002_total_familias %>% 
  mutate(
    Grupo = dplyr::case_when(
    QUADRO == '23' | QUADRO == '43' |
    QUADRO == '50' ~ 'Transporte Urbano',
    QUADRO == '24' ~ 'Alimentação',
    QUADRO == '28' ~ 'Cultura/Lazer/Esporte',
    QUADRO == '34' | QUADRO == '35' |
    QUADRO == '36' | QUADRO == '38' ~ 'Roupas e Calçados',
    TRUE ~ 'Habitação'),
    Ano = '2002'
  ) 

pof_2002_total_familias$UF <- 
  dplyr::recode(pof_2002_total_familias$UF,
  "11" =  "RO", "12" =  "AC", "13" =  "AM",
  "14" =  "RR", "15" =  "PA", "16" =  "AP",
  "17" =  "TO", "21" =  "MA", "22" =  "PI",
  "23" =  "CE", "24" =  "RN", "25" =  "PB",
  "26" =  "PE", "27" =  "AL", "28" =  "SE",
  "29" =  "BA", "31" =  "MG", "32" =  "ES",
  "33" =  "RJ", "35" =  "SP", "41" =  "PR",
  "42" =  "SC", "43" =  "RS", "50" =  "MS",
  "51" =  "MT", "52" =  "GO", "53" =  "DF"
  )

setwd("C:/Users/lucas/Desktop/R/pof")
readr::write_rds(pof_2002_total_familias, 'pof_agregado_2002.rds')

######## 3. Merge and Group data -------------------------------------------


pof_2002_agregado <- readr::read_rds("pof_agregado_2002.rds")
pof_2008_agregado <- readr::read_rds("pof_agregado_2008.rds")
pof_2017_agregado <- readr::read_rds("pof_agregado_2017.rds")

pof_agregado <- 
  dplyr::bind_rows(
    pof_2002_agregado, pof_2008_agregado, pof_2017_agregado 
  )

readr::write_rds(pof_agregado, "pof_agregado.rds")

###### 4. Prepare data -------------------

pof_agregado <-
  pof_agregado %>% 
  group_by(ID_FAMILIA, UF, ESTRATO_POF, RENDA_ANUAL, Grupo, PESO_FINAL, Estrato, Ano) %>% 
  mutate(gasto_grupo = sum(CONSUMO_ANUAL), prop = gasto_grupo/RENDA_ANUAL) %>% 
  ungroup()

pof_agregado <- as.data.table(pof_agregado)

pof_agregado[, 
  decil_renda := cut(x = RENDA_ANUAL, breaks = Hmisc::wtd.quantile(
    x = RENDA_ANUAL, weights = PESO_FINAL, probs = 0:10/10,
    type = c('quantile','(i-1)/(n-1)', 'i/(n+1)','i/n'),
    normwt = F, na.rm = T), labels = F, include.lowest = T),
  by = .(Ano, UF)]

readr::write.rds(pof_agregado, "pof_agregado_final.rds")

pof_agregado_familias <-
  pof_agregado %>% 
  filter(prop<1) %>% 
  group_by(
    UF, RENDA_ANUAL, PESO_FINAL, ID_FAMILIA,
    Estrato, Grupo, Ano, decil_renda) %>% 
  summarise(
    gasto = mean(gasto_grupo, na.rm = T), prop = mean(prop, na.rm = T))

readr::write_rds(pof_agregado_familias, "pof_agregado_familias.rds")
