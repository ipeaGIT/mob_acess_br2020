# Libraries ----
source("./R/setup.R")

fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
} 

# Read .csv data ----

  pnads <- fread("data/PNAD/bases_pnads_agregadas.csv")


  # Tabela proportion above 1h (example) ----
  
  ## 10% richer vs. 40%  poorer
  
  ## Brasil Urbano (aggregate)
  #dt_prop_r_10_p_40_BR <- left_join(
  #  pnads[
  #    !is.na(v9057) & !(r_10_p_40 == "Restante"),
  #    .(total_trips = sum(v4729, na.rm = T)),
  #    by = .(ano, r_10_p_40)
  #    ][, regiao := "Brasil Urbano"] %>% 
  #    select(1,4,2,3),
  #  pnads[
  #    v9057 %in% c(5,7) & !(r_10_p_40 == "Restante"),
  #    .(above1h = sum(v4729, na.rm = T)),
  #    by = .(ano, r_10_p_40)
  #    ][, regiao := "Brasil Urbano"] %>% 
  #    select(1,4,2,3)
  #)[order(ano, r_10_p_40)]
  
  ## Calculate above1h ratio
  #setDT(dt_prop_r_10_p_40_BR)[, above1h_p := (above1h / total_trips) , by = .(r_10_p_40)]
  
  ## RMs (aggregate)
  #dt_prop_r_10_p_40_RMs <- left_join(
  #  pnads[
  #    !is.na(v9057) & metropol == "RM" & !(r_10_p_40 == "Restante"),
  #    .(total_trips = sum(v4729, na.rm = T)),
  #    by = .(ano, r_10_p_40)
  #    ][, regiao := "Regiões Metropol."] %>% 
  #    select(1,4,2,3),
  #  pnads[
  #    v9057 %in% c(5,7) & metropol == "RM" & !(r_10_p_40 == "Restante"),
  #    .(above1h = sum(v4729, na.rm = T)),
  #    by = .(ano, r_10_p_40)
  #    ][, regiao := "Regiões Metropol."] %>% 
  #    select(1,4,2,3)
  #)[order(ano, r_10_p_40)]
  
  # Calculate above1h ratio
  #setDT(dt_prop_r_10_p_40_RMs)[, above1h_p := (above1h / total_trips) , by = .(r_10_p_40)]
  
  ## By RMs
  #dt_prop_r_10_p_40_regiao <- left_join(
  #  pnads[
  #    !is.na(v9057) & !(r_10_p_40 == "Restante") & !regiao == "Urbano Não-Metropolitano",
  #    .(total_trips = sum(v4729, na.rm = T)),
  #    by = .(ano, regiao, r_10_p_40)
  #    ],
  #  pnads[
  #    v9057 %in% c(5,7) & !(r_10_p_40 == "Restante") & !regiao == "Urbano Não-Metropolitano",
  #    .(above1h = sum(v4729, na.rm = T)),
  #    by = .(ano, regiao, r_10_p_40)
  #    ]
  #)
  
  # Calculate above1h ratio
  #setDT(dt_prop_r_10_p_40_regiao)[, above1h_p := (above1h / total_trips) , by = .(r_10_p_40)]
  
  ## Row-bind dts
  #dt_prop_r_10_p_40_total <- data.table::rbindlist(
  #  list(dt_prop_r_10_p_40_BR,dt_prop_r_10_p_40_RMs, dt_prop_r_10_p_40_regiao)
  #)[order(ano)] %>% 
  #  mutate(
  #    regiao = factor(
  #      regiao, 
  #      levels = c(
  #        "Belém","Brasil Urbano",'Porto Alegre','Salvador','Curitiba','Fortaleza','Recife',
  #        'Belo Horizonte',"Regiões Metropol.","Brasília",'Rio de Janeiro','São Paulo'
  #      ),
  #    )
  #  )
  ## rm
  #rm(dt_prop_r_10_p_40_BR, dt_prop_r_10_p_40_RMs, dt_prop_r_10_p_40_regiao)
  
  ## remover (por nao ser utilizada no momento)
  #rm(dt_prop_r_10_p_40_total)
  
  # Tabelas utilizadas --------
  
  # 3.1 - Mean time: Brasil Urbano + Media RMs + por RM -----
  
  mean_time <- data.table::rbindlist(
    l = list(
      ## Mean time by year and all Brasil urban area (aggregate)
      pnads[
        ,
        .(commute_time = weighted.mean(x = commute_time, w = v4729, na.rm = T)),
        by = .(ano)
        ][, regiao := "Brasil Urbano"] %>% 
        select(1,3,2),
      ## Mean time by year and all RMs (aggregate)
      pnads[
        metropol == "RM",
        .(commute_time = weighted.mean(x = commute_time, w = v4729, na.rm = T)),
        by = .(ano)
        ][, regiao := "Regiões Metropol."] %>% 
        select(1,3,2),
      ## Mean time by year and region (w/o Non Metrop Urban) (non-aggregate)
      pnads[
        !regiao == "Urbano Não-Metropolitano",
        .(commute_time = weighted.mean(x = commute_time, w = v4729, na.rm = T)),
        by = .(ano, regiao)
        ]
    )
  ) %>%
    mutate(
      regiao = factor(
        regiao,
        levels = c(
          "Brasil Urbano",'Porto Alegre','Curitiba',"Belém",'Fortaleza','Belo Horizonte',
          'Salvador','Recife',"Brasília","Regiões Metropol.",'São Paulo','Rio de Janeiro'
        ),
        ordered = T
      ),
      regiao2 = regiao,
      regiao_wrap = stringr::str_wrap(regiao, width = 13),
      regiao_wrap = factor(
        regiao_wrap,
        levels = c(
          'Brasil Urbano',"Belém",'Fortaleza','Porto Alegre','Salvador','Curitiba',
          'Belo\nHorizonte','Recife',"Regiões\nMetropol.","Brasília",'Rio de\nJaneiro',
          'São Paulo'
        ),
        ordered = T
      )
    )
  
  # 3.2 - Mean time por decil: Brasil Urbano + Media RMs + por RM ----
  
  mean_time_decil <- data.table::rbindlist(
    l = list(
      # Urban Brasil (aggregate)
      pnads[
        ,
        .(commute_time = weighted.mean(x = commute_time, w = v4729, na.rm = T)),
        by = .(ano, decilBR)
        ][, regiao := "Brasil Urbano"] %>% 
        select(1,4,2,3),
      # All RMs
      pnads[
        metropol == "RM",
        .(commute_time = weighted.mean(x = commute_time, w = v4729, na.rm = T)),
        by = .(ano, decilBR)
        ][, regiao := "Regiões Metropol."] %>% 
        select(1,4,2,3),
      # By region
      pnads[
        !regiao == "Urbano Não-Metropolitano",
        .(commute_time = weighted.mean(x = commute_time, w = v4729, na.rm = T)),
        by = .(ano, regiao, decilBR)
        ]
    )
  )[!is.na(decilBR)] %>% 
    mutate(
      regiao = factor(
        regiao, 
        levels = c(
          "Belém",'Fortaleza',"Brasil Urbano",'Salvador','Recife','Curitiba','Belo Horizonte',
          'Porto Alegre',"Regiões Metropol.",'Rio de Janeiro','São Paulo',"Brasília"
        ),
        ordered = T
      )
    )
  
  # 3.3 - Mean time 10% rich, average, 40% poor: Brasil Urbano + Media RMs + por RM ----
  mean_time_r_10_p_40 <- data.table::rbindlist(
    l = list(
      # Urban Brasil (aggregate)
      pnads[
        !(r_10_p_40 == 'Restante'),
        .(commute_time = weighted.mean(x = commute_time, w = v4729, na.rm = T)),
        by = .(ano, r_10_p_40)
      ][, regiao := "Brasil Urbano"] %>% 
        select(1,4,2,3),
      # All RMs
      pnads[
        !(r_10_p_40 == 'Restante') & metropol == "RM",
        .(commute_time = weighted.mean(x = commute_time, w = v4729, na.rm = T)),
        by = .(ano, r_10_p_40)
        ][, regiao := "Regiões Metropol."] %>% 
        select(1,4,2,3),
      # By region
      pnads[
        !(r_10_p_40 == 'Restante') & !regiao == "Urbano Não-Metropolitano",
        .(commute_time = weighted.mean(x = commute_time, w = v4729, na.rm = T)),
        by = .(ano, regiao, r_10_p_40)
        ]
    )
  )[order(ano)] %>% 
    mutate(
      regiao = factor(
        regiao, 
        levels = c(
          "Brasil Urbano","Belém",'Fortaleza','Porto Alegre','Salvador','Curitiba',
          'Belo Horizonte','Recife',"Regiões Metropol.","Brasília",'Rio de Janeiro',
          'São Paulo'
        ),
        ordered = T
      ),
      regiao_wrap = stringr::str_wrap(regiao, width = 13),
      regiao_wrap = factor(
        regiao_wrap,
        levels = c(
          'Brasil Urbano',"Belém",'Fortaleza','Porto Alegre','Salvador','Curitiba',
          'Belo\nHorizonte','Recife',"Regiões\nMetropol.","Brasília",'Rio de\nJaneiro',
          'São Paulo'
        ),
        ordered = T
      )
    )
  
  mean_time_r_10_p_40 <- data.table::rbindlist(
    l = list(
      mean_time_r_10_p_40 %>% select(-regiao),
      mean_time %>% 
        select(-c(regiao, regiao2)) %>% 
        mutate(r_10_p_40 = "Média") %>% 
        select(ano, r_10_p_40, commute_time, regiao_wrap)
    )
  ) %>% 
    mutate(
      r_10_p_40 = factor(
        r_10_p_40, levels = c("40% mais pobres", "Média", "10% mais ricos"), ordered = T
      )
    ) %>% 
    arrange(ano, regiao_wrap)
    
  
  # 3.4 - Mean time escolaridade, raca_sexo: Brasil Urbano + Media RMs + por RM ----
  
  mean_time_esc_raca_sexo <- data.table::rbindlist(
    l = list(
      # Urban Brasil (aggregate)
      pnads[
        !escolaridade == "Não aplicável" & !raca_sexo == "Restante",
        .(commute_time = weighted.mean(x = commute_time, w = v4729, na.rm = T)),
        by = .(ano, escolaridade, raca_sexo)
        ] [, regiao := "Brasil Urbano"] %>% 
        select(1,5,2:4),
      # All RMs (aggregate)
      pnads[
        metropol == "RM" & !escolaridade == "Não aplicável" & !raca_sexo == "Restante",
        .(commute_time = weighted.mean(x = commute_time, w = v4729, na.rm = T)),
        by = .(ano, escolaridade, raca_sexo)
        ] [, regiao := "Regiões Metropol."] %>% 
        select(1,5,2:4),
      # By RMs
      pnads[
        !escolaridade == "Não aplicável" & !raca_sexo == "Restante" & 
          !regiao=="Urbano Não-Metropolitano",
        .(commute_time = weighted.mean(x = commute_time, w = v4729, na.rm = T)),
        by = .(ano, regiao, escolaridade, raca_sexo)
        ]
    )
  )[order(ano)] %>% 
    mutate(
      regiao = factor(
        regiao,
        levels = c(
          "Brasil Urbano","Belém",'Porto Alegre','Recife','Fortaleza','Curitiba',
          'Salvador','Belo Horizonte',"Regiões Metropol.","Brasília",'Rio de Janeiro',
          'São Paulo'
        ),
        ordered = T
      ),
      escolaridade = factor(
        escolaridade,
        levels = c("Baixa", "Média", "Alta")
      ),
      escolaridade2 = fct_case_when(
        escolaridade == "Baixa" ~ "Baixa\nEscolaridade",
        escolaridade == "Média" ~ "Média\nEscolaridade",
        escolaridade == "Alta" ~ "Alta\nEscolaridade"
      ),
      raca_sexo = factor(
        raca_sexo,
        levels = c('Mulher Negra', 'Mulher Branca', 'Homem Negro', 'Homem Branco'),
        ordered = T
      )
    )
  
  
  # remover tudo ----
  #rm(list = ls())
  

  

  
  








