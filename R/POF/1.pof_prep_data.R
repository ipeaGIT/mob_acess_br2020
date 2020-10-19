### Setup

source("R/setup.R")

# Root directory
root_dir <- here::here('R','POF')
setwd(root_dir)

# Folders with raw data
dir_2017 <- paste0("./data-raw/","2017")
dir_2008 <- paste0("./data-raw/","2008")
dir_2002 <- paste0("./data-raw/","2002")

# Create folders to save clean data
dir.create("./data", showWarnings = FALSE)
destdir_clean_2017 <- paste0("./data/","2017")
destdir_clean_2008 <- paste0("./data/","2008")
destdir_clean_2002 <- paste0("./data/","2002")
dir.create(destdir_clean_2017)
dir.create(destdir_clean_2008)
dir.create(destdir_clean_2002)

######## 1. Dowload and Clean data ----------------------------------------------------------

######################################################################
### POF 2017-2018 ####################################################
######################################################################

setwd(dir_2017)

# Despesa Individual ----------------

despesa_individual <- 
  # Recuperando microdados de despesa
  readRDS("DESPESA_INDIVIDUAL.rds")

pof_2017_total <-        
  despesa_individual %>%    
  # Seleciona as variáveis
  dplyr::select(             
    UF, ESTRATO_POF, COD_UPA,
    NUM_DOM, NUM_UC, COD_INFORMANTE,
    QUADRO, COD_ITEM = V9001,
    N_MESES = V9011, DEFLATOR,
    VALOR_DEFLA = V8000_DEFLA,
    FATOR_ANUALIZACAO, PESO_FINAL,
    RENDA_TOTAL
  ) %>%
  # Cria ID para Família e Indivíduo
  # Calcula Renda e Consumo por produto Anual
  dplyr::mutate(
    ID_FAMILIA = paste(COD_UPA, NUM_DOM, NUM_UC, sep = ""),
    ID_MORADOR = paste(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE, sep = ""),
    CONSUMO_ANUAL = ifelse(is.na(N_MESES), 
      VALOR_DEFLA * FATOR_ANUALIZACAO,
      VALOR_DEFLA * N_MESES * FATOR_ANUALIZACAO),
    RENDA_ANUAL = RENDA_TOTAL * 12 * DEFLATOR
  )  %>%
  # Calcula despesa anual por produto por indivíduo
  dplyr::group_by(
    UF, ESTRATO_POF, RENDA_ANUAL,
    QUADRO, COD_ITEM, PESO_FINAL,
    ID_FAMILIA, ID_MORADOR
  ) %>%
  dplyr::summarise(valor_total = sum(CONSUMO_ANUAL, na.rm = T)
  )

# Moradores ------------------------------

moradores <- 
  # Recupera microdados de características dos indivíduos
  readRDS("MORADOR.rds")

moradores <- 
  moradores %>% 
  # Seleciona variáveis
  dplyr::select(
    COD_UPA, NUM_DOM,
    NUM_UC, COD_INFORMANTE,
    IDADE = V0403, SEXO = V0404,
    COR = V0405
  ) %>%
  # Cria ID para o indivíduo
  dplyr::mutate(
    ID_MORADOR = paste(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE, sep = "")
  )

pof_2017_total <- # Seleciona variáveis
  moradores %>%
  # Seleciona variáveis
  dplyr::select(
    IDADE, SEXO, COR, ID_MORADOR
  ) %>%
  # Join com df de despesas com transporte
  dplyr::right_join(pof_2017_total, by = "ID_MORADOR")

######## 2. Recode data -------------------------------------------------------------------

pof_2017_total <-  
  pof_2017_total %>% 
  # Calcula renda per capita familiar,
  dplyr::group_by(ID_FAMILIA) %>% 
  dplyr::mutate(
    n = n_distinct(ID_MORADOR),
    renda_pc = RENDA_ANUAL / n
  ) %>% 
  dplyr::rename(renda_total = RENDA_ANUAL) %>% 
  dplyr::ungroup()


pof_2017_total <- 
  # Recodifica variáveis com informações dos indivíduos
  pof_2017_total %>%
  dplyr::mutate(
    faixa_etaria = dplyr::case_when(
      IDADE < 15       ~ "0-14",
      IDADE %in% 15:24 ~ "15-24",
      IDADE %in% 25:34 ~ "25-34", 
      IDADE %in% 35:49 ~ "35-49", 
      IDADE %in% 50:64 ~ "50-64", 
      IDADE > 64 ~ "65+"),
    genero = 
      ifelse(SEXO == 1, "Homem", "Mulher"),
    etnia = dplyr::case_when( 
      COR == 1 ~ "Branca",
      COR == 2 ~ "Preta",
      COR == 3 ~ "Amarela",
      COR == 4 ~ "Parda",
      COR == 5 ~ "Indígena",
      TRUE     ~ "Sem declaração"),
    regiao = dplyr::case_when(
      UF < 20 ~ "Norte",
      UF < 30 ~ "Nordeste",
      UF < 40 ~ "Sudeste",
      UF < 50 ~ "Sul",
      TRUE    ~ "Centro-Oeste"),
    Ano = "2017"
  )

pof_2017_total <- 
  pof_2017_total %>% 
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

pof_2017_total <- 
  # Cria df selecionando variáveis
  pof_2017_total %>%
  dplyr::select(
    PESO_FINAL, ID_MORADOR, ID_FAMILIA, QUADRO,
    COD_ITEM, renda_total, valor_total,
    renda_pc, faixa_etaria, genero,
    etnia, regiao, Ano, UF, Estrato
  )

pof_2017_total$UF <- 
  # Recodifica nome dos estados
  dplyr::recode(pof_2017_total$UF,
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

pof_2017_transporte <-
  pof_2017_total %>% 
  # Filtra despesas com transporte
  dplyr::filter(
    QUADRO == "23" | QUADRO == '33' | QUADRO == '50'
  ) %>% 
  # Classifica Modos de Transporte
  dplyr::mutate(
    Modo = 
      ifelse(
        COD_ITEM <= 2300404 |
        COD_ITEM >= 2300701 & COD_ITEM <= 2301301 |
        COD_ITEM >= 2302301 & COD_ITEM <= 2303001 , "Transporte Coletivo",
        "Transporte Individual")
  )

# Salva df e limpa memória
setwd(destdir_clean_2017)
readr::write_rds(pof_2017_total, "pof_2017_total.rds")
readr::write_rds(pof_2017_transporte, "pof_2017_transporte.rds")
rm(list = ls())

###############################################################
### POF 2008-2009 #############################################
###############################################################

setwd(dir_2008)

# Despesa Individual ----------------------

despesa_individual <-
  # Recuperando microdados de despesa
  readRDS("DESPESA_INDIVIDUAL.rds")

pof_2008_total <-
  despesa_individual %>%
  dplyr::select(
    UF, ESTRATO_POF, NUM_SEQ = num_seq,
    DV_SEQ = dv_seq, NUM_DOM = num_dom,
    NUM_UC  = num_uc, COD_INFORMANTE,
    QUADRO, COD_ITEM = "X.COD_ITEM",
    DEFLATOR, VALOR_DEFLA, FATOR_ANUALIZACAO,
    PESO_FINAL  = fatorexpansao2, RENDA_TOTAL
  ) %>%
  dplyr::mutate(
    ID_FAMILIA = paste(UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, sep = ""),
    ID_MORADOR = paste(UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, COD_INFORMANTE, sep = ""),
    COD_ITEM = ifelse(
      COD_ITEM <= 999, paste(QUADRO, COD_ITEM, sep = "00"),
      paste(QUADRO, COD_ITEM, sep = "0")),
    CONSUMO_ANUAL = VALOR_DEFLA * FATOR_ANUALIZACAO,
    RENDA_ANUAL = RENDA_TOTAL * 12 * DEFLATOR
  ) %>%
  dplyr::group_by(
    UF, ESTRATO_POF, RENDA_ANUAL,
    QUADRO, COD_ITEM, PESO_FINAL,
    ID_FAMILIA, ID_MORADOR
  ) %>%
  dplyr::summarise(valor_total = sum(CONSUMO_ANUAL, na.rm = T)
  )

# Moradores ------------------------------

moradores <-
  readRDS("MORADOR.rds")

moradores <-
  moradores %>%
  dplyr::select(
    UF, NUM_SEQ = num_seq, DV_SEQ = dv_seq,
    NUM_DOM = num_dom, NUM_UC  = num_uc,
    COD_INFORMANTE, IDADE, SEXO, COR, 
  ) %>%
  dplyr::mutate(
    ID_MORADOR = paste(UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, COD_INFORMANTE, sep = "")
  )

pof_2008_total <-
  moradores %>%
  dplyr::select(
    IDADE, SEXO, COR, ID_MORADOR
  ) %>%
  dplyr::right_join(pof_2008_total, by = "ID_MORADOR")

######## 2. Recode data -------------------------------------------------------------------

pof_2008_total <-
  pof_2008_total %>% 
  dplyr::group_by(ID_FAMILIA) %>% 
  dplyr::mutate(
    renda_pc = RENDA_ANUAL /  n_distinct(ID_MORADOR)
  ) %>% 
  dplyr::rename(renda_total = RENDA_ANUAL) %>% 
  dplyr::ungroup()

pof_2008_total <- 
  pof_2008_total %>%
  dplyr::mutate(
    faixa_etaria = dplyr::case_when(
      IDADE < 15       ~ "0-14",
      IDADE %in% 15:24 ~ "15-24",
      IDADE %in% 25:34 ~ "25-34", 
      IDADE %in% 35:49 ~ "35-49", 
      IDADE %in% 50:64 ~ "50-64", 
      IDADE > 64 ~ "65+"),
    genero = 
      ifelse(SEXO == 1, "Homem", "Mulher"),
    etnia = dplyr::case_when( 
      COR == 1 ~ "Branca",
      COR == 2 ~ "Preta",
      COR == 3 ~ "Amarela",
      COR == 4 ~ "Parda",
      COR == 5 ~ "Indígena",
      TRUE     ~ "Sem declaração"),
    regiao = dplyr::case_when(
      UF < 20 ~ "Norte",
      UF < 30 ~ "Nordeste",
      UF < 40 ~ "Sudeste",
      UF < 50 ~ "Sul",
      TRUE    ~ "Centro-Oeste"),
    Ano = "2008"
  )

pof_2008_total <-
  pof_2008_total %>%
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

pof_2008_total <-
  pof_2008_total %>%
  dplyr::select(
    PESO_FINAL, ID_MORADOR, ID_FAMILIA,
    QUADRO, COD_ITEM, renda_total, valor_total,
    renda_pc, faixa_etaria, genero, etnia,
    regiao, Ano, UF, Estrato
  )

pof_2008_total$UF <- 
  # Recodifica nome dos estados
  dplyr::recode(pof_2008_total$UF,
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

pof_2008_transporte <-
  pof_2008_total %>% 
  dplyr::filter(
   QUADRO == "23" | QUADRO == '43' | QUADRO == '50'
  ) %>%
  dplyr::mutate(
     Modo = 
      ifelse(
      COD_ITEM <= 2300201 | COD_ITEM >= 2300401 & COD_ITEM <= 2300510 |
      COD_ITEM >= 2301001 & COD_ITEM <= 2301101 | COD_ITEM >= 2301401 & COD_ITEM <= 2301601 |
      COD_ITEM >= 2302001 & COD_ITEM <= 2302304 , "Transporte Coletivo", "Transporte Individual")
  )

setwd(destdir_clean_2008)
readr::write_rds(pof_2008_total, "pof_2008_total.rds")
readr::write_rds(pof_2008_transporte, "pof_2008_transporte.rds")
rm(list = ls())

###############################################################
### POF 2002-2003 #############################################
###############################################################

setwd(dir_2002)

# Despesa Individual ----------------------

despesa_individual <-
  readRDS("DESPESA_INDIVIDUAL.rds")

pof_2002_total <-
  despesa_individual %>%
  dplyr::select(
    UF, ESTRATO_POF, NUM_SEQ = num_seq,
    DV_SEQ = dv_seq, NUM_DOM = num_dom,
    NUM_UC  = num_uc, COD_INFORMANTE,
    QUADRO, COD_ITEM, DEFLATOR, VALOR_DEFLA = VALOR_DEFLA_ANUALIZADO,
    FATOR_ANUALIZACAO, PESO_FINAL  = fatorexpansao2, RENDA_TOTAL
  ) %>%
  dplyr::mutate(
    ID_FAMILIA = paste(UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, sep = ""),
    ID_MORADOR = paste(UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, COD_INFORMANTE, sep = ""),
    COD_ITEM = ifelse(
      COD_ITEM <= 999, paste(QUADRO, COD_ITEM, sep = "00"), paste(QUADRO, COD_ITEM, sep = "0")),
    CONSUMO_ANUAL = VALOR_DEFLA,
    RENDA_ANUAL = RENDA_TOTAL * 12 * DEFLATOR
  ) %>%
  dplyr::group_by(
    UF, ESTRATO_POF, RENDA_ANUAL,
    QUADRO, COD_ITEM, PESO_FINAL,
    ID_FAMILIA, ID_MORADOR
  ) %>%
  dplyr::summarise(valor_total = sum(CONSUMO_ANUAL, na.rm = T))

# Moradores ------------------------------

moradores <-
  readRDS("MORADOR.rds")

moradores <-
  moradores %>%
  dplyr::select(
    UF, ESTRATO_POF, NUM_SEQ = num_seq,
    DV_SEQ = dv_seq, NUM_DOM = num_dom,
    NUM_UC  = num_uc, COD_INFORMANTE,
    IDADE, SEXO, COR
  ) %>%
  dplyr::mutate(
    ID_MORADOR = paste(UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, COD_INFORMANTE, sep = "")
  )

pof_2002_total <-
  moradores %>%
  dplyr::select(
    IDADE, SEXO, COR, ID_MORADOR
  ) %>%
  dplyr::right_join(pof_2002_total, by = "ID_MORADOR")

######## 2. Recode data -------------------------------------------------------------------

pof_2002_total <-
  pof_2002_total %>% 
  dplyr::group_by(ID_FAMILIA) %>%  
  dplyr::mutate(
    renda_pc = RENDA_ANUAL / n_distinct(ID_MORADOR),
  ) %>% 
  dplyr::rename(renda_total = RENDA_ANUAL) %>% 
  dplyr::ungroup()

pof_2002_total <- 
  pof_2002_total %>%
  dplyr::mutate(
    faixa_etaria = dplyr::case_when(
      IDADE < 15       ~ "0-14",
      IDADE %in% 15:24 ~ "15-24",
      IDADE %in% 25:34 ~ "25-34", 
      IDADE %in% 35:49 ~ "35-49", 
      IDADE %in% 50:64 ~ "50-64", 
      IDADE > 64 ~ "65+"),
    genero = 
      ifelse(SEXO == 1, "Homem", "Mulher"),
    etnia = dplyr::case_when( 
      COR == 1 ~ "Branca",
      COR == 2 ~ "Preta",
      COR == 3 ~ "Amarela",
      COR == 4 ~ "Parda",
      COR == 5 ~ "Indígena",
      TRUE     ~ "Sem declaração"),
    regiao = dplyr::case_when(
      UF < 20 ~ "Norte",
      UF < 30 ~ "Nordeste",
      UF < 40 ~ "Sudeste",
      UF < 50 ~ "Sul",
      TRUE    ~ "Centro-Oeste"),
    Ano = "2002"
  )

pof_2002_total <-
  pof_2002_total %>%
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

pof_2002_total <-
  pof_2002_total %>%
  dplyr::select(
    PESO_FINAL, ID_MORADOR, ID_FAMILIA,
    QUADRO, COD_ITEM, renda_total, valor_total,
    renda_pc, faixa_etaria, genero,
    etnia, regiao, Ano, UF, Estrato
  )

pof_2002_total$UF <- 
  # Recodifica nome dos estados
  dplyr::recode(pof_2002_total$UF,
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

pof_2002_transporte <-
  pof_2002_total %>% 
  dplyr::filter(
   QUADRO == "23" | QUADRO == '50' | QUADRO == '43'
  ) %>%
  dplyr::mutate(
    Modo = 
     ifelse(
    COD_ITEM <= 2300201 | COD_ITEM >= 2300401 & COD_ITEM <= 2300507 |
    COD_ITEM >= 2301001 & COD_ITEM <= 2301101 | COD_ITEM >= 2301401 & COD_ITEM <= 2301502 |
    COD_ITEM >= 2302001 & COD_ITEM <= 2302304 | COD_ITEM == 2302701 | 
    COD_ITEM == 2302901 ,"Transporte Coletivo", "Transporte Individual"),
  )

setwd(destdir_clean_2002)
readr::write_rds(pof_2002_total, "pof_2002_total.rds")
readr::write_rds(pof_2002_transporte, "pof_2002_transporte.rds")
rm(list = ls())

######## 3. Merge and Group data -------------------------------------------

setwd(root_dir)

pof_2002_total <- readr::read_rds(paste0(destdir_clean_2002,"/pof_2002_total.rds"))# %>% #select(-COD_ITEM)
pof_2008_total <- readr::read_rds(paste0(destdir_clean_2008,"/pof_2008_total.rds"))# %>% #select(-COD_ITEM)
pof_2017_total <- readr::read_rds(paste0(destdir_clean_2017,"/pof_2017_total.rds"))# %>% #select(-COD_ITEM)

pof_total <- 
  dplyr::bind_rows(
    pof_2002_total, pof_2008_total, pof_2017_total 
  )

readr::write.rds(pof_total, "pof_total.rds")

pof_2002_transporte <- readr::read_rds(paste0(destdir_clean_2002,"/pof_2002_transporte.rds"))#%>% #select(-COD_ITEM)
pof_2008_transporte <- readr::read_rds(paste0(destdir_clean_2008,"/pof_2008_transporte.rds"))# %>% #select(-COD_ITEM)
pof_2017_transporte <- readr::read_rds(paste0(destdir_clean_2017,"/pof_2017_transporte.rds")) #%>% #select(-COD_ITEM)

pof_transporte <- 
  dplyr::bind_rows(
    pof_2002_transporte, pof_2008_transporte, pof_2017_transporte 
  )

readr::write_rds(pof_transporte, "pof_transporte.rds")

###### 4. Prepare data -------------------

# POF Transporte =======

pof_transporte_final <-
  pof_transporte %>%
  # Calcula gasto e comprometimento por família
  group_by(ID_FAMILIA, Ano) %>% 
  mutate(
    gasto_transp_fam = sum(valor_total),
    prop_transp_fam = gasto_transp_fam / renda_total
  ) %>% 
  ungroup() %>% 
  # Calcula gasto e comprometimento por modo e família
  group_by(ID_FAMILIA, Ano, Modo) %>% 
  mutate(
    gasto_modo_fam = sum(valor_total),
    prop_modo_fam = gasto_modo_fam / renda_total
  ) %>% 
  ungroup() %>% 
  # Calcula gasto e comprometimento por morador
  group_by(ID_MORADOR, Ano) %>% 
  mutate(
    gasto_transp_ind = sum(valor_total),
    prop_transp_ind = gasto_transp_ind / renda_pc
  ) %>% 
  ungroup() %>% 
  # Calcula gasto e comprometimento por modo e morador
  group_by(ID_MORADOR, Ano, Modo) %>% 
  mutate(
    gasto_modo_ind = sum(valor_total),
    prop_modo_ind = gasto_modo_ind / renda_pc
  ) %>%
  ungroup() %>% 
  filter(
    prop_transp_fam < 1 &
      prop_modo_fam < 1 &
      prop_transp_ind < 1 &
      prop_modo_ind < 1
  )

readr::write_rds(pof_transporte_final, "pof_transporte_final.rds")

# Brasil Urbano ========

pof_transporte_urbano <-
  pof_transporte_final %>%
  dplyr::filter(
    Estrato != "Interior Rural" 
  ) %>%
  dplyr::mutate( 
    RM = case_when(
      Estrato != "Interior Urbano" & UF == "SP" ~ "São Paulo",
      Estrato != "Interior Urbano" & UF == "RJ" ~ "Rio de Janeiro",
      Estrato != "Interior Urbano" & UF == "PR" ~ "Curitiba",
      Estrato != "Interior Urbano" & UF == "RS" ~ "Porto Alegre",
      Estrato != "Interior Urbano" & UF == "BA" ~ "Salvador",
      Estrato != "Interior Urbano" & UF == "PE" ~ "Recife",
      Estrato != "Interior Urbano" & UF == "CE" ~ "Fortaleza",
      Estrato != "Interior Urbano" & UF == "PA" ~ "Belém",
      Estrato != "Interior Urbano" & UF == "MG" ~ "Belo Horizonte",
      Estrato != "Interior Urbano" & UF == "DF" ~ "Brasília",
      TRUE ~ "Brasil Urbano")
  )

pof_transporte_urbano <-
  pof_transporte_urbano %>% as.data.table()

pof_transporte_urbano[, 
  decil_renda := cut(x = renda_pc, breaks = Hmisc::wtd.quantile(
  x = renda_pc, weights = PESO_FINAL, probs = 0:10/10,
  type = c('quantile','(i-1)/(n-1)', 'i/(n+1)','i/n'),
  normwt = F, na.rm = T), labels = F, include.lowest = T),
  by = .(Ano, RM)]


pof_transporte_urbano[, 
  quintil_renda := cut(x = renda_pc, breaks = Hmisc::wtd.quantile(
    x = renda_pc, weights = PESO_FINAL, probs = 0:5/5,
    type = c('quantile','(i-1)/(n-1)', 'i/(n+1)','i/n'),
    normwt = F, na.rm = T),labels = F, include.lowest = T),
  by = .(Ano, RM)]

readr::write_rds(pof_transporte_urbano, 'pof_transporte_urbano.rds')