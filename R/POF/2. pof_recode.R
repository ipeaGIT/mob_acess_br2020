### Setup

source("setup.R")


######## 2. Recode and Subset data ----------------------------------------------------------

pof <-
  # Recupera df consolidado
  fread('pof_totalb.csv') 

## POF Total ------------

pof <-
  # Calcula variáveis
  pof %>%
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

write.csv(pof, 'pof_final.csv')

# POF Urbano --------------------

pof_urbano <-
  # Cria df Brasil Urbano
  pof %>%
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

write.csv(pof_urbano, 'pof_urbano.csv')

# POF Status Vulnerabilidade --------------------

pof_status_total <-
  # Cria df status para transporte total
  pof_urbano %>% 
  filter(RM != 'Brasil Urbano') %>% 
  group_by(ID_MORADOR, PESO_FINAL, RM, Estrato, genero, etnia, Ano) %>% 
  # Resume dados do indivíduo
  summarise(
    renda = mean(renda_pc),
    gasto = mean(gasto_transp_ind),
    prop = mean(prop_transp_ind)
  ) %>% 
  group_by(RM, Ano) %>%
  # Calcula medias por RM e Ano
  mutate(
    prop_avg = weighted.mean(prop, PESO_FINAL),
    renda_avg = log(weighted.mean(renda, PESO_FINAL)),
    gasto_avg = log(weighted.mean(gasto, PESO_FINAL))
  ) %>% 
  ungroup() %>% 
  # Cria status de vulnerabilidade
  mutate(
    status = 
     ifelse(log(renda) <= renda_avg & prop >= prop_avg, "Renda Baixa/Gasto Alto",
     ifelse(log(renda) <= renda_avg & prop < prop_avg, "Renda Baixa/Gasto Baixo",
     ifelse(log(renda) > renda_avg & prop >= prop_avg, "Renda Alta/Gasto Alto",
     'Renda Alta/Gasto Baixo')))
  ) %>% 
  group_by(RM, status, Ano) %>% 
  # Calcula parcela da população em cada status
  mutate(pop_status = n()) %>% 
  ungroup() %>% 
  group_by(RM, Ano) %>% 
  mutate(pop_rm = n()) %>% 
  ungroup() %>% 
  mutate(share = pop_status/pop_rm
  )

write.csv(pof_status_total, 'pof_status_total.csv')

pof_status_individual <-
  # Status de vulnerabilidade para transporte individual
  pof_urbano %>% 
  filter(RM != 'Brasil Urbano' & Modo == 'Transporte Individual') %>% 
  group_by(ID_MORADOR, PESO_FINAL, RM, Estrato, genero, etnia, Ano) %>% 
  summarise(
    renda = mean(renda_pc),
    gasto = mean(gasto_modo_ind),
    prop = mean(prop_modo_ind)
  ) %>% 
  group_by(RM, Ano) %>%
  mutate(
    prop_avg = weighted.mean(prop, PESO_FINAL),
    renda_avg = log(weighted.mean(renda, PESO_FINAL)),
    gasto_avg = log(weighted.mean(gasto, PESO_FINAL))
  ) %>% 
  ungroup() %>% 
  mutate(
    status = 
    ifelse(log(renda) <= renda_avg & prop >= prop_avg, "Renda Baixa/Gasto Alto",
    ifelse(log(renda) <= renda_avg & prop < prop_avg, "Renda Baixa/Gasto Baixo",
    ifelse(log(renda) > renda_avg & prop >= prop_avg, "Renda Alta/Gasto Alto",
    'Renda Alta/Gasto Baixo')))
  ) %>% 
  group_by(RM, status, Ano) %>% 
  mutate(pop_status = n()) %>% 
  ungroup() %>% 
  group_by(RM, Ano) %>% 
  mutate(pop_rm = n()) %>% 
  ungroup() %>% 
  mutate(share = pop_status/pop_rm)

write.csv(pof_status_individual, 'pof_status_individual.csv')

# POF Medias por tipo de transporte --------------

pof_medias <-
pof_urbano %>% filter(RM != 'Brasil Urbano') %>% 
  group_by(RM, Ano, decil_renda, Modo) %>% 
  summarise(
    prop = weighted.mean(prop_modo_ind, PESO_FINAL),
    sd = sd(prop_modo_ind),
    upper = prop + 1.96*sd/sqrt(n()),
    lower = prop - 1.96*sd/sqrt(n())
  ) %>% group_by(Ano, Modo) %>% 
  mutate(media = mean(prop)) %>% 
  ungroup()

write.csv(pof_medias, 'pof_medias.csv')

# POF desigualdade --------------

desigualdade <-
  pof_status_total %>% 
  # Composição racial da RM
  group_by(RM, Ano, etnia) %>% 
  mutate(pop_etnia = n()) %>% 
  ungroup() %>% 
  mutate(share_etnia = pop_etnia/pop_rm) %>% 
  # Composição racial do status dentro da RM
  group_by(RM, Ano, etnia, status) %>% 
  mutate(pop_etnia_status = n()) %>% 
  ungroup() %>% 
  mutate(share_etnia_status = pop_etnia_status/pop_status) %>% 
  group_by(RM, Ano, status, etnia) %>% 
  summarise(
    share_etnia = mean(share_etnia),
    share_etnia_status = mean(share_etnia_status)) %>% 
  mutate(diff = share_etnia_status - share_etnia)



