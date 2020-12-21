### Setup ------------------------------------------------------

source("R/setup.R")
source("R/style.R")
source("R/colours.R")

# Root directory
root_dir <- here::here('R','POF')
setwd(root_dir)

# Recover Data ---------------------------------

pof_transporte_urbano <- 
  readr::read_rds('pof_transporte_urbano.rds')
pof_agregado <-
  readr::read_rds('pof_agregado.rds')

######## 3. Plot data ----------------------------------------------------------

setwd("C:/Users/lucas/Desktop/R/mob_acess_br2020")
library(ggtext)
windowsFonts(Helvetica='Helvetica')

# 0. Evolução dos gastos 

plot0 <-
  pof_agregado %>% 
  dplyr::filter(Ano != 2002) %>% 
  dplyr::filter(prop<1) %>% 
  dplyr::group_by(Ano, Grupo) %>% 
  dplyr::summarise(
    prop = weighted.mean(prop, PESO_FINAL, na.rm = T))

plot0$Grupo <- factor(plot0$Grupo, levels = c('Habitação','Transporte Urbano','Alimentação','Roupas e Calçados','Cultura/Lazer/Esporte'))

plot0 %>% 
  dplyr::filter(Grupo != 'Cultura/Lazer/Esporte') %>% 
  ggplot(aes(Ano,prop,group = Grupo)) +
  geom_col(aes(fill= Grupo),position = 'dodge')+
  scale_fill_aop(palette = 'original') +
  scale_y_continuous(labels = scales::percent, limits = c(0,.2)) +
  labs(
    x = 'Ano', y = '% da renda total familiar', fill = 'Categoria'
    #,title = 'Comprometimento da renda familiar: principais despesas'
  )+
  geom_text(aes(label = scales::percent(prop)),position = position_dodge(width = .9),vjust=-1,size=3) +
  theme_minimal() +
  aop_style() +
  theme(
    legend.position = 'bottom',
    axis.title = element_markdown(size = 8, colour = '#808080'))

ggsave('gastos_categoria.png', path = 'figures/POF', width = 16, height = 11, units = 'cm', dpi = 300)
ggsave('gastos_categoria.pdf', path = 'figures/POF', width = 16, height = 11, units = 'cm', dpi = 300)

# 1. Parallel Coord -------------------------

plot1 <- 
  # Arrange df
  pof_transporte_urbano %>% 
  #select(-V1,-V1) %>%
  dplyr::filter(QUADRO == 23) %>% 
  dplyr::group_by(Ano, decil_renda) %>% 
  dplyr::mutate(n_t  = dplyr::n_distinct(ID_FAMILIA)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(Ano, Modo, decil_renda) %>% 
  dplyr::summarise(
    n = dplyr::n_distinct(ID_FAMILIA),
    prop = mean(n/n_t)
  ) 

plot1 %>% na.omit() %>% 
ggplot() +
  geom_line(
    aes(as.factor(Ano), prop, group = decil_renda, color = as.factor(decil_renda)), linetype = 'dashed') + 
  geom_point(
    aes(as.factor(Ano), prop, fill = as.factor(decil_renda)),
    size = 2.5, alpha = 1, shape = 21)+
  scale_colour_aop(palette = 'blue_red')+
  scale_fill_aop(palette = 'blue_red')+
  hrbrthemes::scale_y_percent()+
  labs(
    y = 'Porcentagem das famílias', x="",
    color = 'Decil de Renda', fill = 'Decil de Renda'
    #,title = 'Famílias com despesas em transporte por tipo: 2002 - 2017',
    #subtitle = 'Evolução da parcela das famílias com despesas em transporte, por modo de transporte e faixa de renda.'
  )+
  facet_wrap(~Modo, nrow = 1) +
  aop_style() +
  theme(
    legend.position = 'bottom',
    axis.title.x = element_markdown(size = 8, colour = '#808080'),
    axis.title.y = element_markdown(size = 8, colour = '#808080',angle = 90))

ggsave('parallel_tipo_transporte.png', path = 'figures/POF', width = 16, height = 11, units = 'cm', dpi = 300)
ggsave('parallel_tipo_transporte.pdf', path = 'figures/POF', width = 16, height = 11, units = 'cm', dpi = 300)

# 2. Evolução Renda -------------------------

plot2 <- 
  pof_transporte_urbano %>% 
  #dplyr::select(-V1,-V1) %>% 
  dplyr::group_by(UF,regiao, quintil_renda, Ano) %>% 
  dplyr::summarise(
    renda = weighted.mean(renda_pc, PESO_FINAL)) %>% 
  dplyr::mutate(
    renda_defla = dplyr::case_when(
      Ano == 2002 ~ renda*2.6137,
      Ano == 2008 ~ renda*1.8540,
      Ano == 2017 ~ renda*1.5976),
    names = UF) %>% 
  tidyr::pivot_wider(
    names_from = Ano, values_from = c(renda_defla,renda))%>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(variacao = renda_defla_2017/renda_defla_2002) %>% 
  dplyr::select(UF, regiao,quintil_renda,variacao) %>% 
  dplyr::mutate(quintil_renda = as.factor(quintil_renda))

plot2$UF <- factor(plot2$UF, levels = c(
  "SC",'RJ','ES','PB','PA','PR','MS','MT','SP',
  'RO','RN','MA','MG','PI','RS','AC','AL','GO',
  'DF','AM','BA','RR','AP','TO','PE','SE','CE')
  )

ne <-
  plot2 %>% 
  na.omit() %>% 
  dplyr::filter(regiao=='Nordeste') %>% 
  ggplot() +
    geom_vline(aes(xintercept = 1), linetype = 'dashed') +
    geom_vline(aes(xintercept = 2), linetype = 'dashed') +
    geom_path(
      aes(variacao, UF,group = UF),
      linetype = 'dotted') +
    geom_point(
      aes(variacao, UF, fill = as.factor(quintil_renda)), shape = 21, size = 2.5, alpha = 1) +
  scale_x_continuous(limits = c(0.4, 3.1), breaks = seq(0, 3, .5)) +
  scale_fill_aop(palette =  'blue_red') +
  theme_minimal() +
  labs(
    #title = "Ganho de renda real no período 2002 - 2017: Fator de multiplicação da renda entre os anos por faixa de renda e UF",
    #x = 'Fator de multiplicação', fill = "Quintil de Renda", y=""
    #,subtitle = "Fator igual a 1 equivale dizer que a renda (ajustada pela inflação) permaneceu a mesma no período. \nFator de multiplicação 2 indica que a renda em 2017 é o dobro da de 2002."
  ) +
  aop_style() +
  facet_wrap(~regiao,ncol = 1,strip.position = 'right') +
  theme(
    legend.position = 'none', 
    axis.text.x = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    strip.placement = NULL,
    plot.margin = unit(c(0,0,0,0), "cm"))

n <-
  plot2 %>% 
  na.omit() %>% 
  dplyr::filter(regiao=='Norte') %>% 
  ggplot() +
  geom_vline(aes(xintercept = 1), linetype = 'dashed') +
  geom_vline(aes(xintercept = 2), linetype = 'dashed') +
  geom_path(
    aes(variacao, UF,group = UF),
    linetype = 'dotted') +
  geom_point(
    aes(variacao, UF, fill = as.factor(quintil_renda)), shape = 21, size = 2.5, alpha = 1) +
  scale_x_continuous(limits = c(0.4, 3.1), breaks = seq(0, 3, .5)) +
  scale_fill_aop(palette =  'blue_red') +
  theme_minimal() +
  labs(
    #title = "Ganho de renda real no período 2002 - 2017: Fator de multiplicação da renda entre os anos por faixa de renda e UF",
    #x = 'Fator de multiplicação', fill = "Quintil de Renda", y=""
    #,subtitle = "Fator igual a 1 equivale dizer que a renda (ajustada pela inflação) permaneceu a mesma no período. \nFator de multiplicação 2 indica que a renda em 2017 é o dobro da de 2002."
  ) +
  aop_style() +
  facet_wrap(~regiao,ncol = 1,strip.position = 'right') +
  theme(
    legend.position = 'none', 
    axis.text.x = element_blank(),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    strip.placement = NULL,
    plot.margin = unit(c(0,0,0,0), "cm"))

co <-
  plot2 %>% 
  na.omit() %>% 
  dplyr::filter(regiao=='Centro-Oeste') %>% 
  ggplot() +
  geom_vline(aes(xintercept = 1), linetype = 'dashed') +
  geom_vline(aes(xintercept = 2), linetype = 'dashed') +
  geom_path(
    aes(variacao, UF,group = UF),
    linetype = 'dotted') +
  geom_point(
    aes(variacao, UF, fill = as.factor(quintil_renda)), shape = 21, size = 2.5, alpha = 1) +
  scale_x_continuous(limits = c(0.4, 3.1), breaks = seq(0, 3, .5)) +
  scale_fill_aop(palette =  'blue_red') +
  theme_minimal() +
  labs(
    #title = "Ganho de renda real no período 2002 - 2017: Fator de multiplicação da renda entre os anos por faixa de renda e UF",
    #x = 'Fator de multiplicação', fill = "Quintil de Renda", y=""
    #,subtitle = "Fator igual a 1 equivale dizer que a renda (ajustada pela inflação) permaneceu a mesma no período. \nFator de multiplicação 2 indica que a renda em 2017 é o dobro da de 2002."
  ) +
  aop_style() +
  facet_wrap(~regiao,ncol = 1,strip.position = 'right') +
  theme(
    legend.position = 'none', 
    axis.text.x = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    strip.placement = NULL,
    plot.margin = unit(c(0,0,0,0), "cm"))

se <-
  plot2 %>% 
  na.omit() %>% 
  dplyr::filter(regiao=='Sudeste') %>% 
  ggplot() +
  geom_vline(aes(xintercept = 1), linetype = 'dashed') +
  geom_vline(aes(xintercept = 2), linetype = 'dashed') +
  geom_path(
    aes(variacao, UF,group = UF),
    linetype = 'dotted') +
  geom_point(
    aes(variacao, UF, fill = as.factor(quintil_renda)), shape = 21, size = 2.5, alpha = 1) +
  scale_x_continuous(limits = c(0.4, 3.1), breaks = seq(0, 3, .5)) +
  scale_fill_aop(palette =  'blue_red') +
  theme_minimal() +
  labs(
    #title = "Ganho de renda real no período 2002 - 2017: Fator de multiplicação da renda entre os anos por faixa de renda e UF",
    #x = 'Fator de multiplicação', fill = "Quintil de Renda", y=""
    #,subtitle = "Fator igual a 1 equivale dizer que a renda (ajustada pela inflação) permaneceu a mesma no período. \nFator de multiplicação 2 indica que a renda em 2017 é o dobro da de 2002."
  ) +
  aop_style() +
  facet_wrap(~regiao,ncol = 1,strip.position = 'right') +
  theme(
    legend.position = 'none', 
    axis.text.x = element_blank(),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    strip.placement = NULL,
    plot.margin = unit(c(0,0,0,0), "cm"))

s <-
  plot2 %>% 
  na.omit() %>% 
  dplyr::filter(regiao=='Sul') %>% 
  ggplot() +
  geom_vline(aes(xintercept = 1), linetype = 'dashed') +
  geom_vline(aes(xintercept = 2), linetype = 'dashed') +
  geom_path(
    aes(variacao, UF,group = UF),
    linetype = 'dotted') +
  geom_point(
    aes(variacao, UF, fill = as.factor(quintil_renda)), shape = 21, size = 2.5, alpha = 1) +
  scale_x_continuous(limits = c(0.4, 3.1), breaks = seq(0, 3, .5)) +
  scale_fill_aop(palette =  'blue_red') +
  
  theme_minimal() +
  labs(
    #title = "Ganho de renda real no período 2002 - 2017: Fator de multiplicação da renda entre os anos por faixa de renda e UF",
    x = 'Fator de multiplicação', fill = "Quintil de Renda", y=""
    #,subtitle = "Fator igual a 1 equivale dizer que a renda (ajustada pela inflação) permaneceu a mesma no período. \nFator de multiplicação 2 indica que a renda em 2017 é o dobro da de 2002."
  ) +
  aop_style() +
  facet_wrap(~regiao,ncol = 1,strip.position = 'right') +
  theme(
    legend.position = 'bottom',
    axis.title.x = element_markdown(size = 8, colour = '#808080'),
    strip.placement = NULL,
    plot.margin = unit(c(0,0,0,0), "cm"))

library(patchwork)
p2 <- ne/n/co/se/s
p2
ne

ggsave('clev_evol_renda.png', path = 'figures/POF', width = 16, height = 16, units = 'cm', dpi = 300)
ggsave('clev_evol_renda.pdf', path = 'figures/POF', width = 16, height = 16, units = 'cm', dpi = 300)

# 3. Gasto com Transporte ------------------------------------------

plot3 <-
  pof_transporte_urbano %>% 
  #select(-V1, -V1) %>%
  dplyr::filter(Ano == 2017) %>% 
  dplyr::group_by(ID_FAMILIA, Modo) %>% 
  dplyr::mutate(gasto_pc = sum(valor_total)/dplyr::n_distinct(ID_MORADOR)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(RM, decil_renda, Modo) %>% 
  dplyr::summarise(
    gasto_pc_mensal = weighted.mean(gasto_pc, PESO_FINAL)/12) %>% 
  dplyr::group_by(Modo) %>% 
  dplyr::mutate(
    RM = ifelse(RM=='Brasil Urbano','Brasil Urbano não metropolitano',RM),
    gastobr = mean(gasto_pc_mensal),
    names = RM) %>% 
  dplyr::ungroup()

pof_transporte_urbano %>%  
  #select(-V1, -V1) %>%
  dplyr::filter(Ano == 2017) %>% 
  dplyr::group_by(ID_FAMILIA, Modo) %>% 
  dplyr::mutate(gasto_pc = sum(valor_total)/dplyr::n_distinct(ID_MORADOR)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(Modo) %>% 
  dplyr::summarise(
    gastobr = weighted.mean(gasto_pc, PESO_FINAL)/12)

plot3 <- plot3 %>% 
  dplyr::mutate(
    media = ifelse(Modo == 'Transporte Coletivo',133,538))

plot3 %>% na.omit() %>% 
  dplyr::mutate(decil_renda = as.factor(decil_renda)) %>% 
  ggplot() +
  geom_hline(
    aes(yintercept = media, color = Modo), linetype = 'dashed', alpha = .95) +
  geom_line(
    data = plot3 %>% dplyr::select(-RM) %>% na.omit(),
    aes(as.factor(decil_renda), gasto_pc_mensal, color = Modo, group = interaction(Modo,names)), alpha = .2,size=.4) +
  geom_line(
    aes(decil_renda, gasto_pc_mensal, color = Modo, group = interaction(Modo,RM)),size=.8) +
  geom_point(
  aes(decil_renda, gasto_pc_mensal, color = Modo)) +
  scale_colour_aop(palette = 'original') +
  scale_y_continuous(limits = c(0,1400), labels = c(0,300,600,900,1200,1500),breaks = c(0,300,600,900,1200,1500)) +
  labs(
    x = 'Decis de Renda', y = 'Gasto familiar per capita mensal (R$)',
    color = 'Modo'
    #title = 'Gasto com Transporte: Coletivo e Individual',
    #subtitle = 'Gasto familiar mensal médio por pessoa, por modo de transporte e faixa de renda. \nLinhas pontilhadas representam o gasto médio nacional para todos as faixas de renda.'
  ) +
  facet_wrap(~RM, nrow = 4) +
  aop_style() +
  theme(
    legend.position = 'bottom',
    axis.title.x = element_markdown(size = 8, colour = '#808080'),
    axis.title.y = element_markdown(size = 8, colour = '#808080',angle = 90))

ggsave('gastos_transp_pc.png', path = 'figures/POF', width = 16, height = 12, units = 'cm', dpi = 300)
ggsave('gastos_transp_pc.pdf', path = 'figures/POF', width = 16, height = 12, units = 'cm', dpi = 300)


# 5. Comprometimento da Renda ------------------------------------------

plot5 <-
  pof_transporte_urbano %>% 
  #select(-V1, -V1) %>% 
  dplyr::group_by(Ano, ID_FAMILIA, Modo) %>% 
  dplyr::mutate(
    gasto_pc = sum(valor_total)/dplyr::n_distinct(ID_MORADOR),
    prop = gasto_pc/renda_pc) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(prop<1) %>% 
  dplyr::group_by(Ano, RM, decil_renda, Modo) %>% 
  dplyr::summarise(
    gasto_pc = weighted.mean(gasto_pc, PESO_FINAL),
    renda_pc = weighted.mean(renda_pc, PESO_FINAL),
    prop = weighted.mean(prop, PESO_FINAL)) %>% 
  dplyr::group_by(Ano, Modo) %>% 
  dplyr::mutate(
    propbr = mean(prop, na.rm = T),
    gastobr = mean(gasto_pc)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    RM = ifelse(RM=='Brasil Urbano','Brasil Urbano não metropolitano',RM),
    names = RM,
    decil_renda = as.factor(decil_renda)
  )

plot5 %>% na.omit() %>% 
  ggplot() +
  geom_line(
    data = plot5 %>% dplyr::filter(Ano == 2017)%>% na.omit(),
    aes(decil_renda, prop, color = Modo, linetype = 'type1', group = interaction(Modo,RM)),size=.8) +
  geom_point(
    data = plot5 %>% dplyr::filter(Ano == 2017)%>% na.omit(),
    aes(decil_renda, prop, color = Modo)) +
  geom_line(
    data = plot5 %>% dplyr::filter(Ano == 2002)%>% na.omit(),
    aes(decil_renda, prop, color = Modo, linetype = 'type2', group = interaction(Modo,RM)),
    size = .8, alpha = .7) +
  scale_linetype_manual(name='Ano',
                        values = c("type1" = "solid", "type2" = "dashed"),
                        labels = c("2017", "2002")) +
  hrbrthemes::scale_y_percent()+
  scale_colour_aop(palette = 'original') +
  facet_wrap(~RM, nrow = 4) +
  aop_style() +
  theme(legend.position = 'bottom', ) +
  labs(
    x = 'Decis de Renda', y = 'Porcentagem da renda total familiar',
    fill = 'Modo', color = 'Modo'
    #title = 'Comprometimento da renda com Transporte: Coletivo e Individual',
    #subtitle = 'Parcela da renda familiar destinada a despesas com transporte, por modo de transporte e faixa de renda \nMédias para regiões metropolitanas em 2017 e 2008 (linha pontilhada)'
  )

ggsave('comp_renda_transp.png', path = 'figures/POF', width = 16, height = 12, units = 'cm', dpi = 300)
ggsave('comp_renda_transp.pdf', path = 'figures/POF', width = 16, height = 12, units = 'cm', dpi = 300)

# 6. Cleveland: Cor e Raça: RM x Capital ---------------------------------

plot6 <-
  pof_transporte_urbano %>% 
  #select(-V1, -V1) %>% 
  dplyr::filter(etnia == 'Branca' | etnia == 'Preta' | etnia == 'Parda') %>% 
  dplyr::mutate(cor = ifelse(etnia == 'Branca', 'Branca', 'Negra')) %>% 
  dplyr::group_by(Ano, cor, genero, Estrato, Modo) %>% 
  dplyr::summarise(prop = weighted.mean(prop_transp_ind, PESO_FINAL)) %>% 
  dplyr::mutate(grupo = paste(genero, cor, sep = " "))

plot6$grupo <-
  recode(plot6$grupo,
    'Homem Branca' = 'Homem Branco',
    'Homem Negra' = 'Homem Negro',
    'Mulher Branca' = 'Mulher Branca',
    'Mulher Negra' = 'Mulher Negra'
  )

plot6$Estrato <-
  factor(plot6$Estrato,
    levels = c('Capital','RM da Capital','Interior Urbano')
  )

plot6$Ano <-
  factor(plot6$Ano,
    levels = c('2017','2008','2002')
  )

plot6 %>% 
  dplyr::mutate(Ano = as.factor(Ano)) %>% 
  ggplot(aes(prop, Ano, group = Ano)) +
  geom_path(
    aes(group = interaction(Ano, genero)),
    position = position_dodge(width = .5),
    linetype = 'dotted', size = 0.1, alpha = .9) +
  geom_point(
    aes(prop, Ano, colour = cor,shape = genero, group = genero),
    size = 1.75, alpha = 1,
    position = position_dodge(width = .5)) +
  scale_colour_aop(palette = 'original') +
  hrbrthemes::scale_x_percent(expand = c(0.1,0)) +
  guides(fill = guide_legend(ncol = 2)) +
  labs(
    x = '% da renda destinada a despesas com transporte', y="", colour = 'Cor', shape = 'Sexo' 
    #title = 'Comprometimento da renda com Transporte: 2002 - 2017'
    #subtitle = 'Evolução darcela da renda gasta com transporte urbano por sexo e raça, conforme estrato geográfico.'
    )+
  aop_style()+
  theme(
    panel.border = element_rect(fill = NA),
    legend.position = 'bottom',
    panel.grid.minor.x = element_line(),
    axis.title.x = element_markdown(size = 8, colour = '#808080'),
    axis.title.y = element_markdown(size = 8, colour = '#808080',angle = 90)) +
facet_grid(Estrato ~ Modo, scales = 'free')

ggsave('clev_cor_sexo.png', path = 'figures/POF', width = 16, height = 12, units = 'cm', dpi = 300)
ggsave('clev_cor_sexo.pdf', path = 'figures/POF', width = 16, height = 12, units = 'cm', dpi = 300)

#==================================================================================================