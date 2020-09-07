# Libraries
source("R/inflacao/inflacao_0_libraries.R")

# Read data ----
tab_1419 <- fread(file = "data/inflacao/sidra_1419_original.csv")

# Clean data ----
tab_1419 <- tab_1419 %>% 
  dplyr::mutate(
    mes = readr::parse_date(mes, "%B %Y", locale = locale("pt")),
    #componente_orig = stringr::str_replace_all(componente_orig, "\\d|[:punct:]", ""),
    componente_orig = dplyr::case_when(componente_orig %in% "Índice geral" ~ "IPCA", TRUE ~ componente_orig),
    #componente_orig = factor(
    #  componente_orig, levels = c("IPCA","Gasolina","Metrô","Óleo diesel","Ônibus urbano","Veículo próprio")
    #),
    valor = valor / 100,
    # valor = case_when(is.na(valor) ~ 0, TRUE ~ valor)
    valor_1 = valor + 1,
    componente_orig = factor(componente_orig),
    componente_orig = fct_relevel(componente_orig, "IPCA")
  ) %>% 
  tidyr::separate(componente_orig, 
           into = c("tipo","componente"), sep = "\\.", remove = F, fill = "left") %>% 
  dplyr::mutate(
    tipo = case_when(
      nchar(tipo) == 1 ~ "Grupo",
      nchar(tipo) == 2 ~ "Subgrupo",
      nchar(tipo) == 4 ~ "Item",
      nchar(tipo) == 7 ~ "Subitem",
      TRUE ~ "Índice geral"
    )
  ) %>% 
  dplyr::select(
    nivel_territorial, regiao,variavel, mes, tipo, componente_orig, componente, valor, valor_1
    )

tab_1419$componente <- factor(
  tab_1419$componente,
  levels = unique(tab_1419$componente[order(tab_1419$componente_orig)]),
  ordered = T
)

# Cumulative product (replace NA with 1: don't affect multiplication operation)
funcao <- function(x){
  cumprod(replace(x, is.na(x), 1)) * ifelse(is.na(x), NA, 1)
}

# Create cumulative rate column
tab_1419 <- data.table::setDT(tab_1419)[
  order(mes),
  acumulada := funcao(valor_1) - 1,
  by = .(regiao, variavel, componente)
  ]

# Save cleaned data ----
fwrite(tab_1419, file = "data/inflacao/sidra_1419_cleaned.csv")
saveRDS(tab_1419, file = "data/inflacao/sidra_1419_cleaned.rds")

