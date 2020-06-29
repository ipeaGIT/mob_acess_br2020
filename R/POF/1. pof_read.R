################# # Leitura, limpeza e organização dos microdados POF 2002, 2008 e 2017
##### Lucas ##### # A rotina do código é a mesma para todos os anos, apenas 2017 está comentado
################# # Lê, padroniza e calcula variáveis, salva .csv para cada ano e um .csv de todos os anos

### Setup

source("setup.R")


######## 1. Dowload and Clean data ----------------------------------------------------------

#####################
### POF 2017-2018 ###
#####################

setwd("~/POF/2017_2018")

# Despesa Individual ----------------

despesa_individual <- # Recuperando microdados de despesa
  readRDS("DESPESA_INDIVIDUAL.rds")

despesa_individual <- # Seleciona variáveis
  despesa_individual %>% # Calcula ID para família e indivíduo
  dplyr::select(           # Calcula consumo e renda anuais
    UF,
    ESTRATO_POF,
    COD_UPA,
    NUM_DOM,
    NUM_UC,
    COD_INFORMANTE,
    QUADRO,
    COD_ITEM = V9001,
    N_MESES = V9011,
    VALOR_DEFLA = V8000_DEFLA,
    FATOR_ANUALIZACAO,
    PESO_FINAL,
    RENDA_TOTAL
  ) %>%
  dplyr::mutate(
    ID_FAMILIA = paste(COD_UPA, NUM_DOM, NUM_UC, sep = ""),
    ID_MORADOR = paste(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE, sep = ""),
    CONSUMO_ANUAL = ifelse(is.na(N_MESES),
      VALOR_DEFLA * FATOR_ANUALIZACAO,
      VALOR_DEFLA * N_MESES * FATOR_ANUALIZACAO
    ),
    RENDA_ANUAL = RENDA_TOTAL * 12
  ) 

total_despesas <-
  despesa_individual %>% # Calcula valor total gasto por ano por produto
  dplyr::group_by(
    UF,
    ESTRATO_POF,
    RENDA_ANUAL,
    QUADRO,
    COD_ITEM,
    PESO_FINAL,
    ID_FAMILIA,
    ID_MORADOR
  ) %>%
  dplyr::summarise(valor = sum(CONSUMO_ANUAL, na.rm = T))

total_despesas_transporte <- # Filtra despesas de transporte
  total_despesas %>%
  dplyr::filter(QUADRO == "23" | QUADRO == '33' | QUADRO == '50' | QUADRO == '51' |
                QUADRO == '41' & COD_ITEM >= '4100201' & COD_ITEM <= '4101601')

total_despesas_transporte <-
  total_despesas_transporte %>% # Calcula valor total gasto por ano com transporte
  dplyr::group_by(
    UF,
    ESTRATO_POF,
    RENDA_ANUAL,
    PESO_FINAL,
    ID_FAMILIA,
    ID_MORADOR
  ) %>%
  dplyr::summarise(valor_total = sum(valor, na.rm = T))

# Moradores ------------------------------

moradores <- # Recupera microdados de características dos indivíduos
  readRDS("MORADOR.rds")

moradores <- # Seleciona variáveis
  moradores %>% # Calcula ID para o indivíduo
  dplyr::select(
    COD_UPA,
    NUM_DOM,
    NUM_UC,
    COD_INFORMANTE,
    IDADE = V0403,
    SEXO = V0404,
    COR = V0405
  ) %>%
  dplyr::mutate(
    ID_MORADOR = paste(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE, sep = "")
  )

total_despesas_transporte <- # Seleciona variáveis
  moradores %>% # Join com df de despesas com transporte
  dplyr::select(
    IDADE,
    SEXO,
    COR,
    ID_MORADOR
  ) %>%
  dplyr::right_join(total_despesas_transporte, by = "ID_MORADOR")

######## 2. Recode data -------------------------------------------------------------------

total_despesas_transporte <- # Calcula renda per capita familiar, gasto total por família e gasto per capita por família
  total_despesas_transporte %>% 
  dplyr::group_by(ID_FAMILIA) %>% 
  dplyr::mutate(renda_pc = RENDA_ANUAL / n(),
                gasto_total = sum(valor_total),
                gasto_pc = gasto_total / n()) %>% 
  dplyr::rename(renda_total = RENDA_ANUAL) %>% 
  dplyr::ungroup()

quintiles <- # Calcula quintis de renda ponderados 
  Hmisc::wtd.quantile(
    total_despesas_transporte$renda_pc,
    weights = total_despesas_transporte$PESO_FINAL,
    probs = c(seq(0, 1, 0.2)),
    type = c("quantile", "(i-1)/(n-1)", "i/(n+1)", "i/n"),
    normwt = FALSE, na.rm = T
  )

deciles <- # Calcula decis de renda ponderados 
  Hmisc::wtd.quantile(
    total_despesas_transporte$renda_pc,
    weights = total_despesas_transporte$PESO_FINAL,
    probs = c(seq(0, 1, 0.1)),
    type = c("quantile", "(i-1)/(n-1)", "i/(n+1)", "i/n"),
    normwt = FALSE, na.rm = T
  )

total_despesas_transporte <-
  total_despesas_transporte %>% 
  data.table::as.data.table()

total_despesas_transporte[, quintil_renda := findInterval(
  renda_pc, quintiles[-length(quintiles)]
)]

total_despesas_transporte[, quintil_renda := fifelse(
  quintil_renda == 1, "Q1 poorest",
  fifelse(
    quintil_renda == 5, "Q5 richest",
    paste0("Q", quintil_renda)
  )
)]

total_despesas_transporte[, decil_renda := findInterval(
  renda_pc, deciles[-length(deciles)]
)]

total_despesas_transporte[, decil_renda := fifelse(
  decil_renda == 1, "Q1 poorest",
  fifelse(
    decil_renda == 10, "Q10 richest",
    paste0("Q", decil_renda)
  )
)]

total_despesas_transporte <-
  total_despesas_transporte %>% # Calcula parcela da renda familiar gasta com transporte
  dplyr::group_by(ID_FAMILIA) %>% 
  dplyr::mutate(prop_transporte_familia = gasto_total / renda_total) %>% 
  dplyr::ungroup() %>% # Calcula gasto individual com transporte e parcela da renda individual gasta com transporte
  dplyr::group_by(ID_MORADOR) %>% 
  dplyr::mutate(gasto_ind = sum(valor_total),
         prop_transporte_ind = gasto_ind / renda_pc) %>% 
  dplyr::ungroup()

total_despesas_transporte <- # Recodifica variáveis com informações dos indivíduos
  total_despesas_transporte %>%
  dplyr::mutate(
    faixa_etaria = ifelse(IDADE <= 10, "até 10 anos",
      ifelse(IDADE <= 19, "10 a 19 anos",
        ifelse(IDADE <= 29, "20 a 29 anos",
          ifelse(IDADE <= 39, "30 a 39 anos",
            ifelse(IDADE <= 49, "40 a 49 anos",
              ifelse(IDADE <= 59, "50 a 59 anos",
                ifelse(IDADE <= 69, "60 a 69 anos",
                  ifelse(IDADE <= 79, "70 a 79 anos",
                    "80 anos ou mais"
                     )))))))),
    genero = ifelse(SEXO == 1, "Homem", "Mulher"),
    etnia = ifelse(COR == 1, "Branca",
      ifelse(COR == 2, "Preta",
        ifelse(COR == 3, "Amarela",
          ifelse(COR == 4, "Parda",
            ifelse(COR == 5, "Indígena",
              "Sem declaração"
            ))))),
    regiao = ifelse(UF < 20, "Norte",
      ifelse(UF < 30, "Nordeste",
        ifelse(UF < 40, "Sudeste",
          ifelse(UF < 50, "Sul",
            "Centro-Oeste"
          )))),
    Ano = "2017"
  )

total_despesas_transporte <-
  total_despesas_transporte %>% # Cria classes para os Estratos Urbano, Rural, RM e interior
  dplyr::mutate(
    ESTRATO_POF = as.numeric(ESTRATO_POF),
    Estrato = ifelse(
      ESTRATO_POF == 1101 | ESTRATO_POF == 1102 |
      ESTRATO_POF == 1201 |
      ESTRATO_POF >= 1301 & ESTRATO_POF <= 1306 |
      ESTRATO_POF == 1401 & ESTRATO_POF == 1402 |
      ESTRATO_POF >= 1501 & ESTRATO_POF <= 1503 |
      ESTRATO_POF >= 1601 & ESTRATO_POF <= 1602 |
      ESTRATO_POF == 1701 |
      ESTRATO_POF >= 2101 & ESTRATO_POF <= 2103 |
      ESTRATO_POF >= 2201 & ESTRATO_POF <= 2203 |
      ESTRATO_POF >= 2301 & ESTRATO_POF <= 2306 |
      ESTRATO_POF >= 2401 & ESTRATO_POF <= 2402 |
      ESTRATO_POF >= 2501 & ESTRATO_POF <= 2503 |
      ESTRATO_POF >= 2601 & ESTRATO_POF <= 2603 |
      ESTRATO_POF >= 2701 & ESTRATO_POF <= 2703 |
      ESTRATO_POF >= 2801 & ESTRATO_POF <= 2802 |
      ESTRATO_POF >= 2901 & ESTRATO_POF <= 2906 |
      ESTRATO_POF >= 3101 & ESTRATO_POF <= 3106 |
      ESTRATO_POF >= 3201 & ESTRATO_POF <= 3202 |
      ESTRATO_POF >= 3301 & ESTRATO_POF <= 3309 |
      ESTRATO_POF >= 3501 & ESTRATO_POF <= 3509 |
      ESTRATO_POF >= 4101 & ESTRATO_POF <= 4105 |
      ESTRATO_POF >= 4201 & ESTRATO_POF <= 4202 |
      ESTRATO_POF >= 4301 & ESTRATO_POF <= 4306 |
      ESTRATO_POF >= 5001 & ESTRATO_POF <= 5003 |
      ESTRATO_POF >= 5101 & ESTRATO_POF <= 5102 |
      ESTRATO_POF >= 5201 & ESTRATO_POF <= 5203 |
      ESTRATO_POF >= 5301 & ESTRATO_POF <= 5306,
      "Capital",
      ifelse(
        ESTRATO_POF == 1307 |
        ESTRATO_POF >= 1504 & ESTRATO_POF <= 1505 |
        ESTRATO_POF == 1603 |
        ESTRATO_POF == 2104 |
        ESTRATO_POF >= 2307 & ESTRATO_POF <= 2309 |
        ESTRATO_POF == 2403 |
        ESTRATO_POF >= 2504 & ESTRATO_POF <= 2505 |
        ESTRATO_POF >= 2604 & ESTRATO_POF <= 2606 |
        ESTRATO_POF == 2704 |
        ESTRATO_POF == 2803 |
        ESTRATO_POF >= 2907 & ESTRATO_POF <= 2909 |
        ESTRATO_POF >= 3107 & ESTRATO_POF <= 3109 |
        ESTRATO_POF >= 3203 & ESTRATO_POF <= 3205 |
        ESTRATO_POF >= 3310 & ESTRATO_POF <= 3318 |
        ESTRATO_POF >= 3510 & ESTRATO_POF <= 3515 |
        ESTRATO_POF >= 4106 & ESTRATO_POF <= 4108 |
        ESTRATO_POF >= 4203 & ESTRATO_POF <= 4204 |
        ESTRATO_POF >= 4307 & ESTRATO_POF <= 4309 |
        ESTRATO_POF == 5103 |
        ESTRATO_POF >= 5204 & ESTRATO_POF <= 5206,
        'RM da Capital',
        ifelse(
          ESTRATO_POF == 1103 | ESTRATO_POF == 1107 |
          ESTRATO_POF == 1202 |
          ESTRATO_POF >= 1308 & ESTRATO_POF <= 1310 |
          ESTRATO_POF == 1403 |
          ESTRATO_POF >= 1506 & ESTRATO_POF <= 1511 |
          ESTRATO_POF == 1604 |
          ESTRATO_POF >= 1702 & ESTRATO_POF <= 1705 |
          ESTRATO_POF >= 2105 & ESTRATO_POF <= 2113 |
          ESTRATO_POF >= 2204 & ESTRATO_POF <= 2209 |
          ESTRATO_POF >= 2310 & ESTRATO_POF <= 2320 |
          ESTRATO_POF >= 2404 & ESTRATO_POF <= 2408 |
          ESTRATO_POF >= 2506 & ESTRATO_POF <= 2511 |
          ESTRATO_POF >= 2607 & ESTRATO_POF <= 2615 |
          ESTRATO_POF >= 2705 & ESTRATO_POF <= 2708 |
          ESTRATO_POF >= 2804 & ESTRATO_POF <= 2806 |
          ESTRATO_POF >= 2910 & ESTRATO_POF <= 2925 |
          ESTRATO_POF >= 3110 & ESTRATO_POF <= 3130 |
          ESTRATO_POF >= 3206 & ESTRATO_POF <= 3211 |
          ESTRATO_POF >= 3319 & ESTRATO_POF <= 3330 |
          ESTRATO_POF >= 3516 & ESTRATO_POF <= 3536 |
          ESTRATO_POF >= 4109 & ESTRATO_POF <= 4124 |
          ESTRATO_POF >= 4205 & ESTRATO_POF <= 4217 |
          ESTRATO_POF >= 4310 & ESTRATO_POF <= 4324 |
          ESTRATO_POF >= 5004 & ESTRATO_POF <= 5009 |
          ESTRATO_POF >= 5104 & ESTRATO_POF <= 5112 |
          ESTRATO_POF >= 5207 & ESTRATO_POF <= 5217,
          "Interior Urbano",
          "Interior Rural")))
    )

pof_2017 <- # filtra e seleciona variáveis
  total_despesas_transporte %>%
  dplyr::filter(prop_transporte_familia <= 1 &
                  prop_transporte_ind <= 1  ) %>%
  dplyr::select(
    PESO_FINAL,
    ID_MORADOR,
    ID_FAMILIA,
    renda_total,
    gasto_total,
    renda_pc,
    gasto_pc,
    gasto_ind,
    prop_transporte_familia,
    prop_transporte_ind,
    quintil_renda,
    decil_renda,
    faixa_etaria,
    genero,
    etnia,
    regiao,
    Ano,
    UF,
    Estrato
  )

pof_2017$UF <- # Recodifica nome dos estados
  dplyr::recode(pof_2017$UF,
    "11" =  "RO",
    "12" =  "AC",
    "13" =  "AM",
    "14" =  "RR",
    "15" =  "PA",
    "16" =  "AP",
    "17" =  "TO",
    "21" =  "MA",
    "22" =  "PI",
    "23" =  "CE",
    "24" =  "RN",
    "25" =  "PB",
    "26" =  "PE",
    "27" =  "AL",
    "28" =  "SE",
    "29" =  "BA",
    "31" =  "MG",
    "32" =  "ES",
    "33" =  "RJ",
    "35" =  "SP",
    "41" =  "PR",
    "42" =  "SC",
    "43" =  "RS",
    "50" =  "MS",
    "51" =  "MT",
    "52" =  "GO",
    "53" =  "DF"
  )

write.csv(pof_2017, "pof_2017.csv")
rm(list = ls())

#####################
### POF 2008-2009 ###
#####################

setwd("C:/Users/lucas/Documents/POF/2008_2009")

# Despesa Individual ----------------------

despesa_individual <-
  readRDS("DESPESA_INDIVIDUAL.rds")

despesa_individual <-
  despesa_individual %>%
  dplyr::select(
    UF,
    ESTRATO_POF,
    NUM_SEQ = num_seq,
    DV_SEQ = dv_seq,
    NUM_DOM = num_dom,
    NUM_UC  = num_uc,
    COD_INFORMANTE,
    QUADRO,
    COD_ITEM = "X.COD_ITEM",
    VALOR_DEFLA,
    FATOR_ANUALIZACAO,
    PESO_FINAL  = fatorexpansao2,
    RENDA_TOTAL
  ) %>%
  dplyr::mutate(
    ID_FAMILIA = paste(UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, sep = ""),
    ID_MORADOR = paste(UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, COD_INFORMANTE, sep = ""),
    CONSUMO_ANUAL = VALOR_DEFLA * FATOR_ANUALIZACAO,
    RENDA_ANUAL = RENDA_TOTAL * 12
  )

total_despesas <-
  despesa_individual %>%
  dplyr::group_by(
    UF,
    ESTRATO_POF,
    RENDA_ANUAL,
    QUADRO,
    COD_ITEM,
    PESO_FINAL,
    ID_FAMILIA,
    ID_MORADOR
  ) %>%
  dplyr::summarise(valor = sum(CONSUMO_ANUAL, na.rm = T))

total_despesas_transporte <-
  total_despesas %>%
  dplyr::filter(QUADRO == "23" | QUADRO == '33' | QUADRO == '50' | QUADRO == '51' |
                QUADRO == '41' & COD_ITEM >= '41001' & COD_ITEM <= '41022')

total_despesas_transporte <-
  total_despesas_transporte %>%
  dplyr::group_by(
    UF,
    ESTRATO_POF,
    RENDA_ANUAL,
    PESO_FINAL,
    ID_FAMILIA,
    ID_MORADOR
  ) %>%
  dplyr::summarise(valor_total = sum(valor, na.rm = T))

# Moradores ------------------------------

moradores <-
  readRDS("MORADOR.rds")

moradores <-
  moradores %>%
  dplyr::select(UF,
    NUM_SEQ = num_seq,
    DV_SEQ = dv_seq,
    NUM_DOM = num_dom,
    NUM_UC  = num_uc,
    COD_INFORMANTE,
    IDADE,
    SEXO,
    COR
  ) %>%
  dplyr::mutate(
    ID_MORADOR = paste(UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, COD_INFORMANTE, sep = "")
  )

total_despesas_transporte <-
  moradores %>%
  dplyr::select(
    IDADE,
    SEXO,
    COR,
    ID_MORADOR
  ) %>%
  dplyr::right_join(total_despesas_transporte, by = "ID_MORADOR")

######## 2. Recode data -------------------------------------------------------------------

total_despesas_transporte <-
  total_despesas_transporte %>% 
  dplyr::group_by(ID_FAMILIA) %>% 
  dplyr::mutate(renda_pc = RENDA_ANUAL / n(),
                gasto_total = sum(valor_total),
                gasto_pc = gasto_total / n()) %>% 
  dplyr::rename(renda_total = RENDA_ANUAL) %>% 
  dplyr::ungroup()

quintiles <-
  Hmisc::wtd.quantile(
    total_despesas_transporte$renda_pc,
    weights = total_despesas_transporte$PESO_FINAL,
    probs = c(seq(0, 1, 0.2)),
    type = c("quantile", "(i-1)/(n-1)", "i/(n+1)", "i/n"),
    normwt = FALSE, na.rm = T
  )

deciles <-
  Hmisc::wtd.quantile(
    total_despesas_transporte$renda_pc,
    weights = total_despesas_transporte$PESO_FINAL,
    probs = c(seq(0, 1, 0.1)),
    type = c("quantile", "(i-1)/(n-1)", "i/(n+1)", "i/n"),
    normwt = FALSE, na.rm = T
  )

total_despesas_transporte <-
  total_despesas_transporte %>% 
  data.table::as.data.table()

total_despesas_transporte[, quintil_renda := findInterval(
  renda_pc, quintiles[-length(quintiles)]
)]

total_despesas_transporte[, quintil_renda := fifelse(
  quintil_renda == 1, "Q1 poorest",
  fifelse(
    quintil_renda == 5, "Q5 richest",
    paste0("Q", quintil_renda)
  )
)]

total_despesas_transporte[, decil_renda := findInterval(
  renda_pc, deciles[-length(deciles)]
)]

total_despesas_transporte[, decil_renda := fifelse(
  decil_renda == 1, "Q1 poorest",
  fifelse(
    decil_renda == 10, "Q10 richest",
    paste0("Q", decil_renda)
  )
)]

total_despesas_transporte <-
  total_despesas_transporte %>% 
  dplyr::group_by(ID_FAMILIA) %>% 
  dplyr::mutate(prop_transporte_familia = gasto_total / renda_total) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(ID_MORADOR) %>% 
  dplyr::mutate(gasto_ind = sum(valor_total),
                prop_transporte_ind = gasto_ind / renda_pc) %>% 
  dplyr::ungroup()

total_despesas_transporte <-
  total_despesas_transporte %>%
  dplyr::mutate(
    faixa_etaria = ifelse(IDADE <= 10, "até 10 anos",
      ifelse(IDADE <= 19, "10 a 19 anos",
        ifelse(IDADE <= 29, "20 a 29 anos",
          ifelse(IDADE <= 39, "30 a 39 anos",
            ifelse(IDADE <= 49, "40 a 49 anos",
              ifelse(IDADE <= 59, "50 a 59 anos",
                ifelse(IDADE <= 69, "60 a 69 anos",
                  ifelse(IDADE <= 79, "70 a 79 anos",
                    "80 anos ou mais"
                  )))))))),
    genero = ifelse(SEXO == 1, "Homem", "Mulher"),
    etnia = ifelse(COR == 1, "Branca",
      ifelse(COR == 2, "Preta",
        ifelse(COR == 3, "Amarela",
          ifelse(COR == 4, "Parda",
            ifelse(COR == 5, "Indígena",
              "Sem declaração"
            ))))),
    regiao = ifelse(UF < 20, "Norte",
      ifelse(UF < 30, "Nordeste",
        ifelse(UF < 40, "Sudeste",
          ifelse(UF < 50, "Sul",
            "Centro-Oeste"
          )))),
    Ano = "2008"
  )

total_despesas_transporte <-
  total_despesas_transporte %>%
  dplyr::mutate(
    ESTRATO_POF = as.numeric(
      ifelse(ESTRATO_POF < 10,
             paste(UF, ESTRATO_POF, sep = "0"),
             paste0(UF, ESTRATO_POF))),
    Estrato = ifelse(
      ESTRATO_POF == 1101 | ESTRATO_POF == 1102 |
      ESTRATO_POF == 1201 |
      ESTRATO_POF >= 1301 & ESTRATO_POF <= 1304 |
      ESTRATO_POF == 1401 |
      ESTRATO_POF == 1501 | ESTRATO_POF == 1502 |
      ESTRATO_POF == 1601 |
      ESTRATO_POF == 1701 |
      ESTRATO_POF >= 2101 & ESTRATO_POF <= 2103 |
      ESTRATO_POF >= 2201 & ESTRATO_POF <= 2203 |
      ESTRATO_POF >= 2301 & ESTRATO_POF <= 2309 |
      ESTRATO_POF >= 2401 & ESTRATO_POF <= 2402 |
      ESTRATO_POF >= 2501 & ESTRATO_POF <= 2503 |
      ESTRATO_POF >= 2601 & ESTRATO_POF <= 2603 |
      ESTRATO_POF >= 2701 & ESTRATO_POF <= 2703 |
      ESTRATO_POF >= 2801 & ESTRATO_POF <= 2802 |
      ESTRATO_POF >= 2901 & ESTRATO_POF <= 2906 |
      ESTRATO_POF >= 3101 & ESTRATO_POF <= 3106 |
      ESTRATO_POF == 3201 |
      ESTRATO_POF >= 3301 & ESTRATO_POF <= 3309 |
      ESTRATO_POF >= 3501 & ESTRATO_POF <= 3509 |
      ESTRATO_POF >= 4101 & ESTRATO_POF <= 4106 |
      ESTRATO_POF >= 4201 & ESTRATO_POF <= 4202 |
      ESTRATO_POF >= 4301 & ESTRATO_POF <= 4306 |
      ESTRATO_POF >= 5001 & ESTRATO_POF <= 5003 |
      ESTRATO_POF >= 5101 & ESTRATO_POF <= 5103 |
      ESTRATO_POF >= 5201 & ESTRATO_POF <= 5203 |
      ESTRATO_POF >= 5301 & ESTRATO_POF <= 5307 ,
      "Capital",
      ifelse(
        ESTRATO_POF == 1503 | ESTRATO_POF == 1504 |
        ESTRATO_POF >= 2310 & ESTRATO_POF <= 2312 |
        ESTRATO_POF >= 2604 & ESTRATO_POF <= 2606 |
        ESTRATO_POF >= 2907 & ESTRATO_POF <= 2908 |
        ESTRATO_POF >= 3107 & ESTRATO_POF <= 3109 |
        ESTRATO_POF >= 3310 & ESTRATO_POF <= 3318 |
        ESTRATO_POF >= 3510 & ESTRATO_POF <= 3515 |
        ESTRATO_POF >= 4107 & ESTRATO_POF <= 4109 |
        ESTRATO_POF >= 4307 & ESTRATO_POF <= 4309 ,
        'RM da Capital',
        ifelse(
          ESTRATO_POF >= 1103 & ESTRATO_POF <= 1106 |
          ESTRATO_POF == 1202 |
          ESTRATO_POF >= 1305 & ESTRATO_POF <= 1308 |
          ESTRATO_POF == 1402 |
          ESTRATO_POF >= 1505 & ESTRATO_POF <= 1508 |
          ESTRATO_POF == 1602 | ESTRATO_POF == 1603 |
          ESTRATO_POF >= 1702 & ESTRATO_POF <= 1705 |
          ESTRATO_POF >= 2104 & ESTRATO_POF <= 2112 |
          ESTRATO_POF >= 2204 & ESTRATO_POF <= 2209 |
          ESTRATO_POF >= 2313 & ESTRATO_POF <= 2323 |
          ESTRATO_POF >= 2403 & ESTRATO_POF <= 2408 |
          ESTRATO_POF >= 2504 & ESTRATO_POF <= 2509 |
          ESTRATO_POF >= 2607 & ESTRATO_POF <= 2615 |
          ESTRATO_POF >= 2704 & ESTRATO_POF <= 2708 |
          ESTRATO_POF >= 2803 & ESTRATO_POF <= 2807 |
          ESTRATO_POF >= 2909 & ESTRATO_POF <= 2921 |
          ESTRATO_POF >= 3110 & ESTRATO_POF <= 3127 |
          ESTRATO_POF >= 3202 & ESTRATO_POF <= 3209 |
          ESTRATO_POF >= 3319 & ESTRATO_POF <= 3330 |
          ESTRATO_POF >= 3516 & ESTRATO_POF <= 3530 |
          ESTRATO_POF >= 4110 & ESTRATO_POF <= 4118 |
          ESTRATO_POF >= 4203 & ESTRATO_POF <= 4213 |
          ESTRATO_POF >= 4310 & ESTRATO_POF <= 4318 |
          ESTRATO_POF >= 5004 & ESTRATO_POF <= 5008 |
          ESTRATO_POF >= 5104 & ESTRATO_POF <= 5110 |
          ESTRATO_POF >= 5204 & ESTRATO_POF <= 5217 ,
          "Interior Urbano",
          "Interior Rural")))
  )

pof_2008 <-
  total_despesas_transporte %>%
  dplyr::filter(prop_transporte_familia <= 1 &
                prop_transporte_ind <= 1) %>%
  dplyr::select(
    PESO_FINAL,
    ID_MORADOR,
    ID_FAMILIA,
    prop_transporte_familia,
    prop_transporte_ind,
    renda_pc,
    gasto_pc,
    renda_total,
    gasto_total, 
    gasto_ind,
    quintil_renda,
    decil_renda,
    faixa_etaria,
    genero,
    etnia,
    regiao,
    Ano,
    UF,
    Estrato
  )

pof_2008$UF <-
  dplyr::recode(pof_2008$UF,
    "11" =  "RO",
    "12" =  "AC",
    "13" =  "AM",
    "14" =  "RR",
    "15" =  "PA",
    "16" =  "AP",
    "17" =  "TO",
    "21" =  "MA",
    "22" =  "PI",
    "23" =  "CE",
    "24" =  "RN",
    "25" =  "PB",
    "26" =  "PE",
    "27" =  "AL",
    "28" =  "SE",
    "29" =  "BA",
    "31" =  "MG",
    "32" =  "ES",
    "33" =  "RJ",
    "35" =  "SP",
    "41" =  "PR",
    "42" =  "SC",
    "43" =  "RS",
    "50" =  "MS",
    "51" =  "MT",
    "52" =  "GO",
    "53" =  "DF"
  )

write.csv(pof_2008, "pof_2008.csv")
rm(list = ls())

#####################
### POF 2002-2003 ###
#####################

setwd("C:/Users/lucas/Documents/POF/2002_2003")

# Despesa Individual ----------------------

despesa_individual <-
  readRDS("DESPESA_INDIVIDUAL.rds")

despesa_individual <-
  despesa_individual %>%
  dplyr::select(UF,
    ESTRATO_POF,
    NUM_SEQ = num_seq,
    DV_SEQ = dv_seq,
    NUM_DOM = num_dom,
    NUM_UC  = num_uc,
    COD_INFORMANTE,
    QUADRO,
    COD_ITEM,
    VALOR_DEFLA = VALOR_DEFLA_ANUALIZADO,
    FATOR_ANUALIZACAO,
    PESO_FINAL  = fatorexpansao2,
    RENDA_TOTAL
  ) %>%
  dplyr::mutate(
    ID_FAMILIA = paste(UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, sep = ""),
    ID_MORADOR = paste(UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, COD_INFORMANTE, sep = ""),
    CONSUMO_ANUAL = VALOR_DEFLA,
    RENDA_ANUAL = RENDA_TOTAL * 12
  )

total_despesas <-
  despesa_individual %>%
  dplyr::group_by(
    UF,
    ESTRATO_POF,
    RENDA_ANUAL,
    QUADRO,
    COD_ITEM,
    PESO_FINAL,
    ID_FAMILIA,
    ID_MORADOR
  ) %>%
  dplyr::summarise(valor = sum(CONSUMO_ANUAL, na.rm = T))

total_despesas_transporte <-
  total_despesas %>%
  dplyr::filter(QUADRO == "23" | QUADRO == '50' | QUADRO == '51' | QUADRO == '43' |
                  QUADRO == '41' & COD_ITEM >= '4100902' & COD_ITEM <= '4102301')

total_despesas_transporte <-
  total_despesas_transporte %>%
  dplyr::group_by(
    UF,
    ESTRATO_POF,
    RENDA_ANUAL,
    PESO_FINAL,
    ID_FAMILIA,
    ID_MORADOR
  ) %>%
  dplyr::summarise(valor_total = sum(valor, na.rm = T))

# Moradores ------------------------------

moradores <-
  readRDS("MORADOR.rds")

moradores <-
  moradores %>%
  dplyr::select(UF,
    ESTRATO_POF,
    NUM_SEQ = num_seq,
    DV_SEQ = dv_seq,
    NUM_DOM = num_dom,
    NUM_UC  = num_uc,
    COD_INFORMANTE,
    IDADE,
    SEXO,
    COR
  ) %>%
  dplyr::mutate(
    ID_MORADOR = paste(UF, NUM_SEQ, DV_SEQ, NUM_DOM, NUM_UC, COD_INFORMANTE, sep = "")
  )

total_despesas_transporte <-
  moradores %>%
  dplyr::select(
    IDADE,
    SEXO,
    COR,
    ID_MORADOR
  ) %>%
  dplyr::right_join(total_despesas_transporte, by = "ID_MORADOR")

######## 2. Recode data -------------------------------------------------------------------

total_despesas_transporte <-
  total_despesas_transporte %>% 
  dplyr::group_by(ID_FAMILIA) %>% 
  dplyr::mutate(renda_pc = RENDA_ANUAL / n(),
                gasto_total = sum(valor_total),
                gasto_pc = gasto_total / n()) %>% 
  dplyr::rename(renda_total = RENDA_ANUAL) %>% 
  dplyr::ungroup()

quintiles <-
  Hmisc::wtd.quantile(
    total_despesas_transporte$renda_pc,
    weights = total_despesas_transporte$PESO_FINAL,
    probs = c(seq(0, 1, 0.2)),
    type = c("quantile", "(i-1)/(n-1)", "i/(n+1)", "i/n"),
    normwt = FALSE, na.rm = T
  )

deciles <-
  Hmisc::wtd.quantile(
    total_despesas_transporte$renda_pc,
    weights = total_despesas_transporte$PESO_FINAL,
    probs = c(seq(0, 1, 0.1)),
    type = c("quantile", "(i-1)/(n-1)", "i/(n+1)", "i/n"),
    normwt = FALSE, na.rm = T
  )

total_despesas_transporte <-
  total_despesas_transporte %>% 
  data.table::as.data.table()

total_despesas_transporte[, quintil_renda := findInterval(
  renda_pc, quintiles[-length(quintiles)]
)]

total_despesas_transporte[, quintil_renda := fifelse(
  quintil_renda == 1, "Q1 poorest",
  fifelse(
    quintil_renda == 5, "Q5 richest",
    paste0("Q", quintil_renda)
  )
)]

total_despesas_transporte[, decil_renda := findInterval(
  renda_pc, deciles[-length(deciles)]
)]

total_despesas_transporte[, decil_renda := fifelse(
  decil_renda == 1, "Q1 poorest",
  fifelse(
    decil_renda == 10, "Q10 richest",
    paste0("Q", decil_renda)
  )
)]

total_despesas_transporte <-
  total_despesas_transporte %>% 
  dplyr::group_by(ID_FAMILIA) %>% 
  dplyr::mutate(prop_transporte_familia = gasto_total / renda_total) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(ID_MORADOR) %>% 
  dplyr::mutate(gasto_ind = sum(valor_total),
                prop_transporte_ind = gasto_ind / renda_pc) %>% 
  dplyr::ungroup()

total_despesas_transporte <-
  total_despesas_transporte %>%
  dplyr::mutate(
    faixa_etaria = ifelse(IDADE <= 10, "até 10 anos",
      ifelse(IDADE <= 19, "10 a 19 anos",
        ifelse(IDADE <= 29, "20 a 29 anos",
          ifelse(IDADE <= 39, "30 a 39 anos",
            ifelse(IDADE <= 49, "40 a 49 anos",
              ifelse(IDADE <= 59, "50 a 59 anos",
                ifelse(IDADE <= 69, "60 a 69 anos",
                  ifelse(IDADE <= 79, "70 a 79 anos",
                    "80 anos ou mais"
                  )
                )
              )
            )
          )
        )
      )
    ),
    genero = ifelse(SEXO == 1, "Homem", "Mulher"),
    etnia = ifelse(COR == 1, "Branca",
      ifelse(COR == 2, "Preta",
        ifelse(COR == 3, "Amarela",
          ifelse(COR == 4, "Parda",
            ifelse(COR == 5, "Indígena",
              "Sem declaração"
            )
          )
        )
      )
    ),
    regiao = ifelse(UF < 20, "Norte",
      ifelse(UF < 30, "Nordeste",
        ifelse(UF < 40, "Sudeste",
          ifelse(UF < 50, "Sul",
            "Centro-Oeste"
          )
        )
      )
    ),
    Ano = "2002"
  )

total_despesas_transporte <-
  total_despesas_transporte %>%
  dplyr::mutate(
      ESTRATO_POF = as.numeric(
        ifelse(ESTRATO_POF < 10,
               paste(UF, ESTRATO_POF, sep = "0"),
               paste0(UF, ESTRATO_POF))),
    Estrato = ifelse(
      ESTRATO_POF >= 1101 & ESTRATO_POF <= 1105 |
      ESTRATO_POF >= 1201 & ESTRATO_POF <= 1204 |
      ESTRATO_POF >= 1301 & ESTRATO_POF <= 1305 |
      ESTRATO_POF >= 1401 & ESTRATO_POF <= 1404 |
      ESTRATO_POF >= 1501 & ESTRATO_POF <= 1505 |
      ESTRATO_POF >= 1601 & ESTRATO_POF <= 1605 |
      ESTRATO_POF >= 1701 & ESTRATO_POF <= 1705 |
      ESTRATO_POF >= 2101 & ESTRATO_POF <= 2105 |
      ESTRATO_POF >= 2201 & ESTRATO_POF <= 2205 |
      ESTRATO_POF >= 2301 & ESTRATO_POF <= 2305 |
      ESTRATO_POF >= 2401 & ESTRATO_POF <= 2405 |
      ESTRATO_POF >= 2501 & ESTRATO_POF <= 2505 |
      ESTRATO_POF >= 2601 & ESTRATO_POF <= 2605 |
      ESTRATO_POF >= 2701 & ESTRATO_POF <= 2705 |
      ESTRATO_POF >= 2801 & ESTRATO_POF <= 2805 |
      ESTRATO_POF >= 2901 & ESTRATO_POF <= 2905 |
      ESTRATO_POF >= 3101 & ESTRATO_POF <= 3105 |
      ESTRATO_POF >= 3201 & ESTRATO_POF <= 3205 |
      ESTRATO_POF >= 3301 & ESTRATO_POF <= 3310 |
      ESTRATO_POF >= 3501 & ESTRATO_POF <= 3510 |
      ESTRATO_POF >= 4101 & ESTRATO_POF <= 4105 |
      ESTRATO_POF >= 4201 & ESTRATO_POF <= 4205 |
      ESTRATO_POF >= 4301 & ESTRATO_POF <= 4305 |
      ESTRATO_POF >= 5001 & ESTRATO_POF <= 5005 |
      ESTRATO_POF >= 5101 & ESTRATO_POF <= 5105 |
      ESTRATO_POF >= 5201 & ESTRATO_POF <= 5205 |
      ESTRATO_POF >= 5301 & ESTRATO_POF <= 5305,
      "Capital",
      ifelse(
        ESTRATO_POF >= 1506 & ESTRATO_POF <= 1509 |
        ESTRATO_POF >= 2306 & ESTRATO_POF <= 2310 |
        ESTRATO_POF >= 2606 & ESTRATO_POF <= 2610 |
        ESTRATO_POF >= 2906 & ESTRATO_POF <= 2910 |
        ESTRATO_POF >= 3106 & ESTRATO_POF <= 3110 |
        ESTRATO_POF >= 3311 & ESTRATO_POF <= 3320 |
        ESTRATO_POF >= 3511 & ESTRATO_POF <= 3520 |
        ESTRATO_POF >= 4106 & ESTRATO_POF <= 4110 |
        ESTRATO_POF >= 4306 & ESTRATO_POF <= 4310 ,
        'RM da Capital',
        ifelse(
          ESTRATO_POF >= 1106 & ESTRATO_POF <= 1109 |
          ESTRATO_POF >= 1205 & ESTRATO_POF <= 1108 |
          ESTRATO_POF >= 1306 & ESTRATO_POF <= 1310 |
          ESTRATO_POF >= 1405 & ESTRATO_POF <= 1406 |
          ESTRATO_POF >= 1510 & ESTRATO_POF <= 1514 |
          ESTRATO_POF >= 1606 & ESTRATO_POF <= 1608 |
          ESTRATO_POF >= 1706 & ESTRATO_POF <= 1710 |
          ESTRATO_POF >= 2106 & ESTRATO_POF <= 2110 |
          ESTRATO_POF >= 2206 & ESTRATO_POF <= 2210 |
          ESTRATO_POF >= 2311 & ESTRATO_POF <= 2315 |
          ESTRATO_POF >= 2406 & ESTRATO_POF <= 2410 |
          ESTRATO_POF >= 2506 & ESTRATO_POF <= 2510 |
          ESTRATO_POF >= 2611 & ESTRATO_POF <= 2615 |
          ESTRATO_POF >= 2706 & ESTRATO_POF <= 2710 |
          ESTRATO_POF >= 2806 & ESTRATO_POF <= 2809 |
          ESTRATO_POF >= 2911 & ESTRATO_POF <= 2915 |
          ESTRATO_POF >= 3111 & ESTRATO_POF <= 3114 |
          ESTRATO_POF >= 3206 & ESTRATO_POF <= 3210 |
          ESTRATO_POF >= 3321 & ESTRATO_POF <= 3325 |
          ESTRATO_POF >= 3521 & ESTRATO_POF <= 3525 |
          ESTRATO_POF >= 4111 & ESTRATO_POF <= 4115 |
          ESTRATO_POF >= 4206 & ESTRATO_POF <= 4210 |
          ESTRATO_POF >= 4311 & ESTRATO_POF <= 4315 |
          ESTRATO_POF >= 5006 & ESTRATO_POF <= 5010 |
          ESTRATO_POF >= 5106 & ESTRATO_POF <= 5110 |
          ESTRATO_POF >= 5206 & ESTRATO_POF <= 5210 |
          ESTRATO_POF >= 5306 & ESTRATO_POF <= 5310 ,
          "Interior Urbano",
          "Interior Rural")))
  )

pof_2002 <-
  total_despesas_transporte %>%
  dplyr::filter(prop_transporte_familia <= 1 &
                prop_transporte_ind <= 1) %>%
  dplyr::select(
    PESO_FINAL,
    ID_MORADOR,
    ID_FAMILIA,
    prop_transporte_familia,
    prop_transporte_ind,
    renda_pc,
    gasto_pc,
    gasto_total,
    renda_total,
    gasto_ind,
    quintil_renda,
    decil_renda,
    faixa_etaria,
    genero,
    etnia,
    regiao,
    Ano,
    UF,
    Estrato
  )

pof_2002$UF <-
  dplyr::recode(pof_2002$UF,
    "11" =  "RO",
    "12" =  "AC",
    "13" =  "AM",
    "14" =  "RR",
    "15" =  "PA",
    "16" =  "AP",
    "17" =  "TO",
    "21" =  "MA",
    "22" =  "PI",
    "23" =  "CE",
    "24" =  "RN",
    "25" =  "PB",
    "26" =  "PE",
    "27" =  "AL",
    "28" =  "SE",
    "29" =  "BA",
    "31" =  "MG",
    "32" =  "ES",
    "33" =  "RJ",
    "35" =  "SP",
    "41" =  "PR",
    "42" =  "SC",
    "43" =  "RS",
    "50" =  "MS",
    "51" =  "MT",
    "52" =  "GO",
    "53" =  "DF"
  )

write.csv(pof_2002, "pof_2002.csv")
rm(list = ls())

######## 3. Merge and Group data -------------------------------------------

setwd('~/POF/')

pof_2002 <- read_csv("~/POF/2002_2003/pof_2002.csv")
pof_2008 <- read_csv("~/POF/2008_2009/pof_2008.csv")
pof_2017 <- read_csv("~/POF/2017_2018/pof_2017.csv")

pof <- dplyr::bind_rows(pof_2002, pof_2008, pof_2017)
pof <- pof %>%
  dplyr::select(-X1)

write.csv(pof, "pof_total.csv")
