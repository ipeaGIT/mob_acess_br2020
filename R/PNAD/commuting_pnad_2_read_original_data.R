# Libraries ----
source("./R/setup.R")



# Read data ----

  # 1992;1993;1995:1999 -> COMANDOS ABAIXO: CORRIGIR ----
  
  # trimws(substr(lines[1], 1,2), ) -> para linhas com longos espacos em branco
  # funcao. entrada: linha de string; saida: linha na estrutura de dados que quer
  # map(estrutura_entrada, funcao)
  
  # checar UF vs. UF/PAIS ESTRANGEIRO NASC
  
  # Ler dicionarios
  
  dicionarios <- list(
    # Pessoas
    pes = list(
      pes_92_95 = read_fwf(
        "data-raw/PNAD/dicionarios/pessoas_1995.TXT", 
        fwf_empty("data-raw/PNAD/dicionarios/pessoas_1995.TXT")
      ) %>% 
        janitor::row_to_names(row_number = 1) %>% 
        clean_names() %>% 
        mutate(across(c("inicio","tamanho","decimais"), as.integer)) %>% 
        na.omit() %>% 
        mutate(final = inicio + (tamanho - 1)) %>% 
        select(variavel,inicio,final,tamanho,decimais,tipo) %>% 
        mutate(
          col_type = case_when(
            tipo %in% "Caracter" ~ 'c',
            TRUE ~ 'd'
          )
        )
    ),
    
    # Domicilios
    dom = list(
      dom_92_95 = read_fwf(
        "data-raw/PNAD/dicionarios/domicilios_1995.TXT", 
        fwf_empty("data-raw/PNAD/dicionarios/domicilios_1995.TXT")
      ) %>% 
        janitor::row_to_names(row_number = 1) %>% 
        clean_names() %>% 
        mutate(across(c("inicio","tamanho","decimais"), as.integer)) %>% 
        mutate(final = inicio + (tamanho - 1)) %>% 
        select(variavel,inicio,final,tamanho,decimais,tipo) %>% 
        mutate(
          col_type = case_when(
            tipo %in% "Caracter" ~ 'c',
            TRUE ~ 'd'
          )
        )
    )
  )
  
  # 1992-1995
  
  # teste pes -> OK
  pnad_pes_92 <- read_fwf(
    "data-raw/PNAD/dados/PES92.DAT",
    fwf_positions(
      start = dicionarios$pes$pes_92_95$inicio, 
      end = dicionarios$pes$pes_92_95$final,
      col_names = dicionarios$pes$pes_92_95$variavel
    ),
    col_types = as.list(dicionarios$pes$pes_92_95$col_type)
  )
  
  # teste dom -> CONFERIR parsing failures
  pnad_dom_92 <- read_fwf(
    "data-raw/PNAD/dados/DOM92.DAT",
    fwf_positions(
      start = dicionarios$dom$dom_92_95$inicio, 
      end = dicionarios$dom$dom_92_95$final,
      col_names = dicionarios$dom$dom_92_95$variavel
    ),
    col_types = as.list(dicionarios$dom$dom_92_95$col_type)
  )

  
  files_pes_92_95 <- list.files(
    path = "data-raw/PNAD/dados", pattern = "[9][2-5]", full.names = T
  )
  
  pnad_pes_90s <- map(
    files_pes_92_95, function(x)
      read_fwf(
        file = x,
        fwf_positions(
          start = dicionarios$pes$pes_92_95$inicio, 
          end = dicionarios$pes$pes_92_95$final,
          col_names = dicionarios$pes$pes_92_95$variavel
        ),
        col_types = as.list(dicionarios$pes$pes_92_95$col_type)
      )
  )
  
  names(pnad_pes_90s) <- paste0("pnad_", c("1992","1993","1995"))
  
  # testar if
  pnad_90s <- map(
    files_92_95, function(x)
      if (str_detect(x, "DOM")) {
        read_fwf(
          file = x,
          fwf_positions(
            start = dicionarios$dom$dom_92_95$inicio, 
            end = dicionarios$dom$dom_92_95$final,
            col_names = dicionarios$dom$dom_92_95$variavel
          ),
          col_types = as.list(dicionarios$dom$dom_92_95$col_type)
        )        
      } else {
        read_fwf(
          file = x,
          fwf_positions(
            start = dicionarios$pes$pes_92_95$inicio, 
            end = dicionarios$pes$pes_92_95$final,
            col_names = dicionarios$pes$pes_92_95$variavel
          ),
          col_types = as.list(dicionarios$pes$pes_92_95$col_type)
        )
      }

  )
  
  
  
  # 2001:2009; 2011-2015 ----

#teste_2001 <- read_PNAD(ft = "pessoas", i = 2001, file = "input/PNAD/2001/Dados/PES2001.TXT")
  
  # pes
  pes_2001_2015 <- list.files(
    path = "data-raw/PNAD/dados", pattern = "^PES20[0-1][0-9]", full.names = T
    )
  
  # dom
  dom_2001_2015 <- list.files(
    path = "data-raw/PNAD/dados", pattern = "^DOM20[0-1][0-9]", full.names = T
  )

  #pes_2001_2015 <- map_chr(pes_2001_2015, function(x) 
  #  paste0("input/PNAD/apenas_dados/", x)
  #)
  
  anos_2001_2015 <- c(2001:2009,2011:2015)

  # Read data 
  
  # Pessoas
  pes <-  map2(
    anos_2001_2015, pes_2001_2015, function(x, y)
      read_PNAD(
        ft = "pessoas",
        i = x,
        file = y
      )
  )
  
  # Domicilios
  dom <-  map2(
    anos_2001_2015, dom_2001_2015, function(x, y)
      read_PNAD(
        ft = "domicilios",
        i = x,
        file = y
      )
    )
  
  # Rename dfs
  names(pes) <- paste0("pnad_pes_", anos_2001_2015)
  names(dom) <- paste0("pnad_dom_", anos_2001_2015)
  
  # Combine lists
  pnad_2000s <- c(pes, dom)
  
  
  ## Write dfs to .csv (easier to use)
  # Define function
  output_csv <- function(data, names){
    
    folder_path = "data/PNAD/"
    
    fwrite(data, paste0(folder_path, names, ".csv"))
  }
  
  # Write dfs
  list(
    data = pnad_2000s,
    names = names(pnad_2000s)
  ) %>% 
    purrr::pmap(output_csv)
  
  
  
  
  
  # 2016:2019 PNADc (NAO NECESSARIO, pois nao tem a variavel) ----
  
  # Ler dados da PNADc para comparar (separadamente) com preços do periodo
  
  pnadc <- map(
    2016:2019, function(x)
      PNADcIBGE::get_pnadc(year = x, interview = 1, design = F)
  )
  
  # Rename dfs
  names(pnadc) <- paste0("pnadc_vis1_", 2016:2019)
  
  # Write dfs to .csv (easier to use)
  list(
    data = pnadc,
    names = names(pnadc)
  ) %>% 
    purrr::pmap(output_csv)
  
  