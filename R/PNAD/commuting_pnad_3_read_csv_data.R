# Libraries ----
source("R/PNAD/commuting_pnad_0_libraries.R")


# Read .csv data ----

  # 1992;1993;1995:1999  ----

  
  # 2001:2009; 2011-2015 ----

  
  # Define variables to read
  
  # rbindlist (bind all PNADs to a single df)
  
  # filter data (pensar na pergunta que queremos responder (commuting time))
  # workers in rural area
  # workers in farms
  # workers in night shift
  # keep only workers in RMs
  
  
  # dt T F variaveis
  #dt_var2 <- rio::import(
  #  file = "input/PNAD/teste_var.xlsx"
  #) %>% 
  # dt to long/tidy format
  #pivot_longer(
  #  cols = -c(1,2,3), names_to = "ano"
  #)

  # dt T F variaveis
  dt_var <- data.table(
    variavel = c('V0101','UF','V0102','V0103','V0302','V8005','V0404','V0607','V6007','V0611',
                 'V9031','V9054','V9057','V4721','V4742','V4727','V4728','V4729','V0101','UF',
                 'V0102','V0103','V0105','V4105','V4107','V4611','UPA','V4615','STRAT','V4617',
                 'PSU','V4618','V4703', 'V4803'),
    pnad = c(rep('pessoas',18), rep('domicilios',14),rep('pessoas',2)),
    colClasses = c(
      'integer','integer','character','character',rep('integer',9),'double','double',
      rep('integer',5),'character','character',rep('integer',4),rep('character',6),
      rep('integer',2)
    ),
    `2001` = c(
      
      # CORRIGIR: ADICIONAR 4721 PARA TODOS OS ANOS 
      #F,T,T,T,T,T,T,T,F,T,T,T,T,T,F,T,T,T,F,T,T,T,T,T,T,T,T,F,T,F,T,F
      F,rep(T,7),F,rep(T,5),F,rep(T,3),F,rep(T,8),F,T,F,T,F,T,F
    ),
    `2002` = c(
      F,rep(T,7),F,rep(T,5),F,rep(T,3),F,rep(T,8),F,F,T,F,T,T,F
    ),
    `2003` = c(
      F,rep(T,7),F,rep(T,5),F,rep(T,3),F,rep(T,7),F,T,F,T,F,T,T,F
    ),
    `2004` = c(
      F,rep(T,7),F,rep(T,9),F,rep(T,8),F,F,T,F,T,T,F
    ),
    `2005` = c(
      F,rep(T,7),F,rep(T,9),F,rep(T,8),F,F,T,F,T,T,F
    ),
    `2006` = c(
      F,rep(T,7),F,rep(T,9),F,rep(T,8),F,F,T,F,T,T,F
    ),
    `2007` = c(
      F,rep(T,6),F,rep(T,10),F,rep(T,8),F,F,T,F,T,F,T
    ),
    `2008` = c(
      rep(T,7),F,rep(T,19),F,F,T,F,T,F,T
    ),
    `2009` = c(
      rep(T,7),F,rep(T,19),F,F,T,F,T,F,T
    ),
    `2011` = c(
      F,rep(T,6),F,rep(T,19),F,F,T,F,T,F,T
    ),
    `2012` = c(
      F,rep(T,6),F,rep(T,19),F,F,T,F,T,F,T
    ),
    `2013` = c(
      F,rep(T,6),F,rep(T,10),F,rep(T,8),F,F,T,F,T,F,T
    ),
    `2014` = c(
      rep(T,7),F,rep(T,10),F,rep(T,8),F,F,T,F,T,F,T
    ),
    `2015` = c(
      rep(T,7),F,rep(T,10),F,rep(T,8),F,F,T,F,T,F,T
    )
  ) %>% 
    # dt to long/tidy format
    tidyr::pivot_longer(
      cols = -c(1,2,3), names_to = "ano"
    )

  
  # list with variables 
  var_list <- dt_var %>% 
    filter(value == T) %>%          # filter variables used
    dplyr::group_by(pnad, ano) %>%  # group by type and year
    dplyr::group_split()            # group_split
  
  # rename list elements
  names(var_list) <- c(
    map("dom", ~paste0(., c(2001:2009,2011:2015)))[[1]], 
    map("pes", ~paste0(., c(2001:2009,2011:2015)))[[1]]
  )
  
  # create vector with only variable code/name , for each element
  variaveis <- lapply(var_list, "[[", 'variavel')
  # lapply(eagora, "[", , 'variavel')
  # vars_pes
  #vars_pes <- variaveis[str_detect(names(variaveis), "pes")]
  
  
  
  # list files to be read
  arquivos <- list.files(
    path = "data/PNAD", pattern = "^pnad_", full.names = T
  )
  
  
  classes_colunas <- var_list %>% 
    purrr::map(~split(., .$colClasses)) %>% # split by colClasses column
    purrr::modify_depth(.depth = 2, .f = purrr::pluck(1)) # pluck column with variable name
  

  lista_pnad <- list(
    file = arquivos,
    select = variaveis,
    colClasses = classes_colunas
  ) %>% 
    purrr::pmap(~fread(
      file = ..1,
      select = ..2,
      colClasses = ..3
    ))  
  
  # rename list elements
  names(lista_pnad) <- c(
    map("dom", ~paste0(., c(2001:2009,2011:2015)))[[1]], 
    map("pes", ~paste0(., c(2001:2009,2011:2015)))[[1]]
  )
  
  ### Deal with different names between the years  
  # clean names
  lista_pnad <- lista_pnad %>% 
    modify(clean_names)
  
  # create columns (year and id)
  lista_pnad <- lista_pnad %>% 
    purrr::imodify(
      ~mutate(
        .x, 
        ano = as.integer(str_extract(.y,"\\d{4}$")), # year 
        id_dom = paste0(ano, uf, v0102, v0103) # id for the household
        )
      )
  
  # pes
  pes <- lista_pnad[str_detect(names(lista_pnad), "pes")]
  # dom
  dom <- lista_pnad[str_detect(names(lista_pnad), "dom")]
  
  rm(lista_pnad)
  
  # 2003: rename v4615 -> upa
  names(dom$dom2003)[names(dom$dom2003) == 'v4615'] <- 'upa'
  # 2001: rename strat -> v4617
  names(dom$dom2001)[names(dom$dom2001) == 'strat'] <- 'v4617'
  # 2001: rename psu -> v4618
  names(dom$dom2001)[names(dom$dom2001) == 'psu'] <- 'v4618'
  
  # left join pes & dom datasets
  pnad_merge <- map2(
    pes, 
    dom %>% map(~select(., c('v0105','upa','v4617','v4618','id_dom'))), 
    function(x,y)
      left_join(x, y, by = c('id_dom' = 'id_dom'))
  )
  
  #
  rm(pes, dom)
  
  # rename dts
  names(pnad_merge) <- c(
    map("pnad", ~paste0(., c(2001:2009,2011:2015)))[[1]]
  )
  
  # Subset data ----

  fct_case_when <- function(...) {
    args <- as.list(match.call())
    levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
    levels <- levels[!is.na(levels)]
    factor(dplyr::case_when(...), levels=levels)
  }  
  
  # correct household income values
  pnad_merge <- pnad_merge %>% 
    modify(
      ~mutate(
        .x,
        v4721_mod = dplyr::case_when(
          v4721 == 999999999999 ~ NA_real_,
          TRUE ~ v4721
        )
      )
    ) %>% 
  # calculate per capita houshold income
    modify(
      ~mutate(
        .x,
        renda_capita = v4721_mod / v0105
      )
    )
  
  # Subset data
  pnad_merge <- pnad_merge %>% 
    modify(~dplyr::filter(
      .x,
      # keep metropolitan areas only (DON'T -> make new variable for "RM" and "Urban Non-RM")
      #v4727 == 1,
      # keep urban areas
      v4728 %in% 1:3,
      # remove people working at night shifts
      !(v9031 %in% 1),
      # remove people working in farms
      !(v9054 %in% 2),
      # remove people with invalid income data
      !is.na(renda_capita),
      is.finite(renda_capita)
    ))
  
  # Create var for education (based on v4703/v4803)
  pnad_merge <- pnad_merge %>% 
    modify_at(
      map("pnad", ~paste0(., c(2001:2006)))[[1]],
      ~mutate(
        .x,
        esc = v4703 - 1,
        escolaridade = dplyr::case_when(
          esc <= 7 ~ "Baixa",
          esc >= 8 & esc <= 14 ~ "Média",
          esc == 15 ~ "Alta",
          TRUE ~ "Não aplicável"
        )
      )
    ) %>% 
    modify_at(
      map("pnad", ~paste0(., c(2007:2009,2011:2015)))[[1]],
      ~mutate(
        .x,
        esc = v4803 -1,
        escolaridade = dplyr::case_when(
          esc <= 7 ~ "Baixa",
          esc >= 8 & esc <= 14 ~ "Média",
          esc == 15 ~ "Alta",
          TRUE ~ "Não aplicável"
        )
      )
    )
 
  # Rbind dts and select variables to keep
  pnads <- pnad_merge %>% 
    map(~select(
      ., 
      c('id_dom','ano','uf','v0302','v8005','v0404','escolaridade','v4727','v4729','v9057',
        'renda_capita'))
    )
  
  #
  pnads <- data.table::rbindlist(pnads)
  
  #
  rm(pnad_merge)
  
  
  # Add columns -----
  
  # Columns: "raca",'sexo','metropol',"commute_time",'idade'
  pnads[
    ,
    c("raca",'sexo','metropol',"commute_time",'idade') := list(
      # raça
      fct_case_when(
        v0404 == 0 ~ "Indígena",
        v0404 %in% c(4,8) ~ "Negra",
        v0404 == 6 ~"Amarela",
        v0404 == 2 ~ "Branca",
        TRUE ~ 'Ignorada'
      ),
      # sexo
      fct_case_when(
        v0302 == 2 ~ 'Masculino',
        TRUE ~ "Feminino"
      ),
      # metropol
      dplyr::case_when(
        v4727 == 1 ~ 'RM',
        TRUE ~ "Non-RM"
      ),
      # commute_time
      dplyr::case_when(
        v9057 == 1 ~ 15,
        v9057 == 3 ~ 45,
        v9057 == 5 ~ 90,
        v9057 == 7 ~ 120,
        TRUE ~ NA_real_
      ),
      # idade
      fct_case_when(
        # remover pessoas com < 18 anos?
        v8005 < 18 ~ "Até 18",
        v8005 >= 18 & v8005 <= 24 ~ "18-24",
        v8005 >= 25 & v8005 <= 34 ~ "25-34",
        v8005 >= 35 & v8005 <= 44 ~ "35-44",
        v8005 >= 45 & v8005 <= 54 ~ "45-54",
        v8005 >= 55 & v8005 <= 64 ~ "55-64",
        v8005 >= 65 ~ "65+",
        TRUE ~ NA_character_
      )
    )
    ]
  
  # 'regiao','raca_sexo'
  pnads[
    ,
    c('regiao','raca_sexo') := list(
      # regiao
      dplyr::case_when(
        metropol == "RM" & uf == 15 ~ "Belém",
        metropol == "RM" & uf == 23 ~ "Fortaleza",
        metropol == "RM" & uf == 26 ~ "Recife",
        metropol == "RM" & uf == 29 ~ "Salvador",
        metropol == "RM" & uf == 31 ~ "Belo Horizonte",
        metropol == "RM" & uf == 33 ~ "Rio de Janeiro",
        metropol == "RM" & uf == 35 ~ "São Paulo",
        metropol == "RM" & uf == 41 ~ "Curitiba",
        metropol == "RM" & uf == 43 ~ "Porto Alegre",
        metropol == "RM" & uf == 53 ~ "Brasília",
        metropol == "Non-RM" ~ "Urbano Não-Metropolitano",
        TRUE ~ NA_character_
      ),
      # raca_sexo
      fct_case_when(
        v0302 == 4 & raca == 'Negra' ~ 'Mulher Negra',
        v0302 == 2 & raca == 'Negra' ~ 'Homem Negro',
        v0302 == 4 & raca == "Branca" ~ 'Mulher Branca',
        v0302 == 2 & raca == "Branca" ~ 'Homem Branco',
        TRUE ~ 'Restante'
      )
    )
    ]
  
  # Quantile (quintil e decil)
  pnads[
    ,
    decilBR := cut(
      x = renda_capita,
      breaks = Hmisc::wtd.quantile(
        x = renda_capita, weights = v4729, probs = (0:10)/10,
        type = c('quantile','(i-1)/(n-1)','i/(n+1)','i/n'),
        normwt=FALSE, na.rm=T
      ),
      labels = 1:10,
      include.lowest = T
    ),
    by = .(ano, regiao)
    ]
  
  pnads[
    ,
    c('decilBR', 'quintilBR') := list(
      # decilBR
      cut(
        x = renda_capita,
        breaks = Hmisc::wtd.quantile(
          x = renda_capita, weights = v4729, probs = (0:10)/10,
          type = c('quantile','(i-1)/(n-1)','i/(n+1)','i/n'),
          normwt = FALSE, na.rm=T
        ),
        labels = 1:10,
        include.lowest = T
      ),
      # quintilBR
      cut(
        x = renda_capita,
        breaks = Hmisc::wtd.quantile(
          x = renda_capita, weights = v4729, probs = seq(0, 1, 0.2),
          type = c('quantile','(i-1)/(n-1)','i/(n+1)','i/n'),
          normwt = FALSE, na.rm=T
        ),
        labels = 1:5,
        include.lowest = T
      )
    ),
    by = .(ano, regiao)
    ]
  
  pnads[
    ,
    r_10_p_40 := fct_case_when(
      decilBR %in% c(1:4) ~ "40% mais pobres",
      decilBR == 10 ~ "10% mais ricos",
      TRUE ~ 'Restante'
    )
    ]
  
  
  # Filter people with 18 years or less
  #pnads <- subset(pnads, !(idade %in% "Até 18"))
  
  #
  pnads <- pnads %>% 
    mutate(
      escolaridade = factor(
        escolaridade,
        levels = c("Não aplicável", "Baixa", "Média", "Alta")
      )
    )
  
  # Save dt
  fwrite(pnads, file = "data/PNAD/bases_pnads_agregadas.csv")
  
  
  
  # remover tudo
  #rm(list = ls())
  