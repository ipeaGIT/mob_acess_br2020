
# Libraries ----
source("R/inflacao/inflacao_0_libraries.R")


# Download data: sidrar::get_sidra ----
grupo_1419 <- sidrar::get_sidra(api = "/t/1419/n1/all/n7/all/n6/all/v/63,66,2265/p/all/c315/7169,7170,7445,7486,7558,7625,7660,7712,7766,7786/d/v63%202,v66%204,v2265%202") %>% 
  janitor::clean_names() %>% 
  data.table::setnames(old = c('brasil_regiao_metropolitana_e_municipio', 'geral_grupo_subgrupo_item_e_subitem'),
                       new = c('regiao', 'componente_orig'))

# TRANSPORTES
transportes_item <- sidrar::get_sidra(api = "/t/1419/n1/all/n7/all/n6/all/v/63,66,2265/p/all/c315/7627,7640,7656/d/v63%202,v66%204,v2265%202") %>% 
  janitor::clean_names() %>% 
  data.table::setnames(old = c('brasil_regiao_metropolitana_e_municipio', 'geral_grupo_subgrupo_item_e_subitem'),
                       new = c('regiao', 'componente_orig'))

transportes_var_subitem <- sidrar::get_sidra(api = "/t/1419/n1/all/n7/all/n6/all/v/63/p/all/c315/7628,7629,7630,7631,7632,7634,7635,7639,7641,7642,7643,7644,7645,7647,7648,7649,7650,7653,7654,7657,7658,7659,12410,12411,107653,107654,107656,107657/d/v63%202") %>% 
  janitor::clean_names() %>% 
  data.table::setnames(old = c('brasil_regiao_metropolitana_e_municipio', 'geral_grupo_subgrupo_item_e_subitem'),
                       new = c('regiao', 'componente_orig'))

transportes_var12_subitem <- sidrar::get_sidra(api = "/t/1419/n1/all/n7/all/n6/all/v/2265/p/all/c315/7628,7629,7630,7631,7632,7634,7635,7639,7641,7642,7643,7644,7645,7647,7648,7649,7650,7653,7654,7657,7658,7659,12410,12411,107653,107654,107656,107657/d/v2265%202") %>% 
  janitor::clean_names() %>% 
  data.table::setnames(old = c('brasil_regiao_metropolitana_e_municipio', 'geral_grupo_subgrupo_item_e_subitem'),
                       new = c('regiao', 'componente_orig'))

transportes_peso_subitem <- sidrar::get_sidra(api = "/t/1419/n1/all/n7/all/n6/all/v/66/p/all/c315/7628,7629,7630,7631,7632,7634,7635,7639,7641,7642,7643,7644,7645,7647,7648,7649,7650,7653,7654,7657,7658,7659,12410,12411,107653,107654,107656,107657/d/v66%204") %>% 
  janitor::clean_names() %>% 
  data.table::setnames(old = c('brasil_regiao_metropolitana_e_municipio', 'geral_grupo_subgrupo_item_e_subitem'),
                       new = c('regiao', 'componente_orig'))


# Merge data ----
tab_1419 <- bind_rows(
  grupo_1419, transportes_item, 
  transportes_var_subitem, transportes_var12_subitem, transportes_peso_subitem
)

# Save as .csv data ----
fwrite(tab_1419, file = "data/inflacao/sidra_1419_original.csv")

