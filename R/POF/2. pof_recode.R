### Setup

source("setup.R")


######## 2. Recode and Subset data ----------------------------------------------------------

pof_transporte <-
  # Recupera df consolidado
  fread('pof_transporte.csv')

pof_total <-
  # Recupera df consolidado
  fread('pof_total.csv')

## POF Transporte ------------

pof_transporte <-
  # Calcula variáveis
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

write.csv(pof_transporte, 'pof_transporte_final.csv')

# POF Urbano --------------------

pof_transporte_urbano <-
  # Cria df Brasil Urbano
  pof_transporte %>%
  #  filtra Brasil Urbano
  dplyr::filter(
    Estrato != "Interior Rural" 
  ) %>%
  # Nomeia Regiões Metropolitanas
  dplyr::mutate( 
    RM = 
      ifelse(Estrato != "Interior Urbano" & UF == "SP", "São Paulo",
      ifelse(Estrato != "Interior Urbano" & UF == "RJ", "Rio de Janeiro",
      ifelse(Estrato != "Interior Urbano" & UF == "PR", "Curitiba",
      ifelse(Estrato != "Interior Urbano" & UF == "RS", "Porto Alegre",
      ifelse(Estrato != "Interior Urbano" & UF == "BA", "Salvador",
      ifelse(Estrato != "Interior Urbano" & UF == "PE", "Recife",
      ifelse(Estrato != "Interior Urbano" & UF == "CE", "Fortaleza",
      ifelse(Estrato != "Interior Urbano" & UF == "PA", "Belém",
      ifelse(Estrato != "Interior Urbano" & UF == "MG", "Belo Horizonte",
      ifelse(Estrato != "Interior Urbano" & UF == "DF", "Brasília",
      "Brasil Urbano"))))))))))
  )

pof_transporte_urbano <-
  pof_transporte_urbano %>% 
  as.data.table()

pof_transporte_urbano[, decil_renda := cut(
  x = renda_pc,
  breaks = Hmisc::wtd.quantile(
    x = renda_pc, weights = PESO_FINAL, probs = 0:10/10,
    type = c('quantile','(i-1)/(n-1)', 'i/(n+1)','i/n'),
    normwt = F, na.rm = T),
  labels = F, include.lowest = T),
  by = .(Ano, RM)]


pof_transporte_urbano[, quintil_renda := cut(
  x = renda_pc,
  breaks = Hmisc::wtd.quantile(
    x = renda_pc, weights = PESO_FINAL, probs = 0:5/5,
    type = c('quantile','(i-1)/(n-1)', 'i/(n+1)','i/n'),
    normwt = F, na.rm = T),
  labels = F, include.lowest = T),
  by = .(Ano, RM)]

write.csv(pof_transporte_urbano, 'pof_transporte_urbano.csv')

## POF Total ------------

pof_total <-
  # Calcula variáveis
  pof_total %>%
  select(-V1) %>% 
  # Calcula gasto e comprometimento por família
  group_by(ID_FAMILIA, Ano, QUADRO) %>% 
  mutate(
    gasto_QUADRO_fam = sum(valor_total),
    prop_QUADRO_fam = gasto_QUADRO_fam / renda_total
  ) %>% 
  ungroup() %>% 
  # Calcula gasto e comprometimento por morador
  group_by(ID_MORADOR, Ano, QUADRO) %>% 
  mutate(
    gasto_QUADRO_ind = sum(valor_total),
    prop_QUADRO_ind = gasto_QUADRO_ind / renda_pc
  ) %>% 
  ungroup() %>% 
  filter(
      prop_QUADRO_ind < 1 &
      prop_QUADRO_fam < 1
  )

write.csv(pof_total, 'pof_total_final.csv')

# POF Urbano --------------------

pof_total_urbano <-
  # Cria df Brasil Urbano
  pof_total %>%
  #  filtra Brasil Urbano
  dplyr::filter(
    Estrato != "Interior Rural" 
  ) %>%
  # Nomeia Regiões Metropolitanas
  dplyr::mutate( 
    RM = 
      ifelse(Estrato != "Interior Urbano" & UF == "SP", "São Paulo",
      ifelse(Estrato != "Interior Urbano" & UF == "RJ", "Rio de Janeiro",
      ifelse(Estrato != "Interior Urbano" & UF == "PR", "Curitiba",
      ifelse(Estrato != "Interior Urbano" & UF == "RS", "Porto Alegre",
      ifelse(Estrato != "Interior Urbano" & UF == "BA", "Salvador",
      ifelse(Estrato != "Interior Urbano" & UF == "PE", "Recife",
      ifelse(Estrato != "Interior Urbano" & UF == "CE", "Fortaleza",
      ifelse(Estrato != "Interior Urbano" & UF == "PA", "Belém",
      ifelse(Estrato != "Interior Urbano" & UF == "MG", "Belo Horizonte",
      ifelse(Estrato != "Interior Urbano" & UF == "DF", "Brasília",
      "Brasil Urbano"))))))))))
  )

pof_total_urbano <-
  pof_total_urbano %>% 
  as.data.table()

pof_total_urbano[, decil_renda := cut(
  x = renda_pc,
  breaks = Hmisc::wtd.quantile(
    x = renda_pc, weights = PESO_FINAL, probs = 0:10/10,
    type = c('quantile','(i-1)/(n-1)', 'i/(n+1)','i/n'),
    normwt = F, na.rm = T),
  labels = F, include.lowest = T),
  by = .(Ano, RM)]


pof_total_urbano[, quintil_renda := cut(
  x = renda_pc,
  breaks = Hmisc::wtd.quantile(
    x = renda_pc, weights = PESO_FINAL, probs = 0:5/5,
    type = c('quantile','(i-1)/(n-1)', 'i/(n+1)','i/n'),
    normwt = F, na.rm = T),
  labels = F, include.lowest = T),
  by = .(Ano, RM)]

write.csv(pof_total_urbano, 'pof_total_urbano.csv')
