### Setup ------------------------------------------------------

source("setup.R")

# Recover Data ---------------------------------

pof_transporte_urbano <- 
  readr::read_rds('pof_transporte_urbano.rds')
pof_agregado <-
  readr::read_rds('pof_agregado_familias.rds')

######## 3. Plot data ----------------------------------------------------------

# 0. Evolução dos gastos 

plot0 <-
  pof_agregado %>% 
  group_by(Ano, Grupo) %>% 
  summarise(
    gasto = weighted.mean(gasto,PESO_FINAL, na.rm = T),
    prop = weighted.mean(prop, PESO_FINAL), na.rm = T)

plot0 %>% 
  na.omit() %>% 
  arrange(desc(prop)) %>% 
  ggplot(aes(Ano,prop)) +
  geom_col(aes(fill=Grupo),position = position_dodge(width = .75))+
  scale_fill_brewer(palette = 'Dark2') +
  scale_y_continuous(labels = scales::percent, limits = c(0,.4)) +
  labs(
    x = 'Ano', y = '% da renda total familiar',
    fill = 'Categoria'
    #title = 'Comprometimento da renda familiar: principais despesas'
  )+
  theme_minimal() +
  theme(
    legend.position = 'bottom',
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = rel(1.3)),
    plot.subtitle = element_text(size = rel(1)),
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(.8)),
    legend.title = element_text(size = rel(1.2)),
    legend.text = element_text(size = rel(1))) 


# 1. Parallel Coord -------------------------

plot1 <- 
  # Arrange df
  pof_transporte_urbano %>% 
  #select(-V1,-V1) %>%
  filter(QUADRO == 23) %>% 
  group_by(Ano, decil_renda) %>% 
  mutate(n_t  = n_distinct(ID_FAMILIA)) %>% 
  ungroup() %>% 
  group_by(Ano, Modo, decil_renda) %>% 
  summarise(
    n = n_distinct(ID_FAMILIA),
    prop = mean(n/n_t)
  )

ggplot(plot1) +
  geom_line(
    aes(as.factor(Ano), prop, group = decil_renda, color = as.factor(decil_renda)),
    size = 1.1, linetype = 'dashed') + 
  geom_point(
    aes(as.factor(Ano), prop, fill = as.factor(decil_renda)),
    size = 4.5, alpha = 1, shape = 21)+
  scale_color_brewer(palette = 'Spectral')+
  scale_fill_brewer(palette = 'Spectral')+
  facet_wrap(~Modo, nrow = 1) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  labs(
    y = '% das famílias', x='',
    color = 'Decil de Renda', fill = 'Decil de Renda'
    #,title = 'Famílias com despesas em transporte por tipo: 2002 - 2017',
    #subtitle = 'Evolução da parcela das famílias com despesas em transporte, por modo de transporte e faixa de renda.'
  ) +
  theme_bw() +
  theme(
    legend.position = 'bottom',
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = rel(1.5)),
    plot.subtitle = element_text(size = rel(1.2)),
    axis.title = element_text(size = rel(1.2)),
    axis.text = element_text(size = rel(1)),
    legend.title = element_text(size = rel(1.2)),
    legend.text = element_text(size = rel(1)))

# 2. Evolução Renda -------------------------

plot2 <- 
  pof_transporte_urbano %>% 
  #dplyr::select(-V1,-V1) %>% 
  dplyr::group_by(UF, quintil_renda, Ano) %>% 
  dplyr::summarise(
    renda = weighted.mean(renda_pc, PESO_FINAL)) %>% 
  mutate(
    renda_defla = case_when(
      Ano == 2002 ~ renda*2.6137,
      Ano == 2008 ~ renda*1.8540,
      Ano == 2017 ~ renda*1.5976),
    names = UF) %>% 
  pivot_wider(
    names_from = Ano, values_from = c(renda_defla,renda))%>% 
  ungroup() %>% 
  mutate(variacao = renda_defla_2017/renda_defla_2002)

plot2 %>% 
  ggplot() +
    geom_vline(aes(xintercept = 1), linetype = 'dashed') +
    geom_vline(aes(xintercept = 2), linetype = 'dashed') +
    geom_path(
      aes(variacao, reorder(UF, interaction(variacao,quintil_renda)),group = UF),
      linetype = 'dotted') +
    geom_point(
      aes(variacao, reorder(UF, interaction(variacao,quintil_renda)),
        fill = as.factor(quintil_renda)), shape = 21, size = 4.5, alpha = 1) +
  scale_fill_brewer(palette =  'Spectral') +
  theme_minimal() +
  labs(
    #title = "Ganho de renda real no período 2002 - 2017: Fator de multiplicação da renda entre os anos por faixa de renda e UF",
    x = 'Fator de multiplicação', fill = "Quintil de Renda", y=""
    #,subtitle = "Fator igual a 1 equivale dizer que a renda (ajustada pela inflação) permaneceu a mesma no período. \nFator de multiplicação 2 indica que a renda em 2017 é o dobro da de 2002."
  ) +
  theme(panel.grid = element_blank(), legend.position = 'bottom')


# 3. Gasto com Transporte ------------------------------------------

plot3 <-
  pof_transporte_urbano %>% 
  #select(-V1, -V1) %>%
  filter(Ano == 2017) %>% 
  group_by(ID_FAMILIA, Modo) %>% 
  mutate(gasto_pc = sum(valor_total)/n_distinct(ID_MORADOR)) %>% 
  ungroup() %>% 
  group_by(RM, decil_renda, Modo) %>% 
  summarise(
    gasto_pc_mensal = weighted.mean(gasto_pc, PESO_FINAL)/12) %>% 
  group_by(Modo) %>% 
  mutate(
    gastobr = mean(gasto_pc_mensal),
    names = RM) %>% 
  ungroup()

plot3 %>%
  mutate(decil_renda = as.factor(decil_renda)) %>% 
  ggplot() +
  geom_hline(
    aes(yintercept = gastobr, color = Modo), linetype = 'dashed', size = 1, alpha = .95) +
  geom_line(
    data = plot2 %>% select(-RM),
    aes(as.factor(decil_renda), gasto_pc_mensal, color = Modo, group = interaction(Modo,names)), alpha = .5) +
  geom_line(
    aes(decil_renda, gasto_pc_mensal, color = Modo, group = interaction(Modo,RM)), size = 1) +
  geom_point(
  aes(decil_renda, gasto_pc_mensal, color = Modo), size = 2) +
  scale_color_brewer(palette = 'Dark2') +
  scale_y_continuous(limits = c(0,1400), labels = c(0,200,400,600,800,1000,1200,1400),breaks = c(0,200,400,600,800,1000,1200,1400)) +
  labs(
    x = 'Decis de Renda', y = 'Gasto familiar per capita mensal (R$)',
    color = 'Modo'
    #title = 'Gasto com Transporte: Coletivo e Individual',
    #subtitle = 'Gasto familiar mensal médio por pessoa, por modo de transporte e faixa de renda. \nLinhas pontilhadas representam o gasto médio nacional para todos as faixas de renda.'
  ) +
  facet_wrap(~RM, nrow = 4) +
  theme_minimal() +
  theme(
    legend.position = 'bottom',
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = rel(1.3)),
    plot.subtitle = element_text(size = rel(1)),
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(.8)),
    legend.title = element_text(size = rel(1.2)),
    legend.text = element_text(size = rel(1))) 

# 4 Elasticidade  ------------------------------------------

plot4 <-
  pof_transporte_urbano %>%
  select(-V1, -V1) %>% 
  group_by(Ano, ID_FAMILIA, Modo, renda_pc) %>% 
  mutate(gasto_pc = sum(valor_total)/n_distinct(ID_MORADOR)) %>% 
  filter(Ano == 2017) %>% 
  group_by(Ano,ID_FAMILIA, Modo, RM, PESO_FINAL, regiao) %>% 
  summarise(gasto = log(sum(gasto_pc)), renda = log(mean(renda_pc)))

plot4 %>% 
  ggplot(aes(renda, gasto, color = Modo, weight = PESO_FINAL)) +
    geom_jitter(alpha = .1) +
    geom_smooth(method = 'lm', se = FALSE, alpha = .6) +
    scale_color_brewer(palette = 'Dark2') +
    theme_minimal() +
    labs(
      x = '(log) Renda per capita', y="(log) Gasto per capita"
      #title = 'Elasticidade - Renda do gasto per capita com transporte por tipo'
    )+
  theme(
    legend.position = 'bottom',
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = rel(1.5)),
    plot.subtitle = element_text(size = rel(1.2)),
    axis.title = element_text(size = rel(1.2)),
    axis.text = element_text(size = rel(1)),
    legend.title = element_text(size = rel(1.2)),
    legend.text = element_text(size = rel(1))) +
  facet_grid(Modo~regiao)

# 5. Comprometimento da Renda ------------------------------------------

plot5 <-
  pof_transporte_urbano %>% 
  #select(-V1, -V1) %>% 
  group_by(Ano, ID_FAMILIA, Modo) %>% 
  mutate(
    gasto_pc = sum(valor_total)/n_distinct(ID_MORADOR),
    prop = gasto_pc/renda_pc) %>% 
  ungroup() %>% 
  filter(prop<1) %>% 
  group_by(Ano, RM, decil_renda, Modo) %>% 
  summarise(
    gasto_pc = weighted.mean(gasto_pc, PESO_FINAL),
    renda_pc = weighted.mean(renda_pc, PESO_FINAL),
    prop = weighted.mean(prop, PESO_FINAL)) %>% 
  group_by(Ano, Modo) %>% 
  mutate(
    propbr = mean(prop, na.rm = T),
    gastobr = mean(gasto_pc)) %>% 
  ungroup() %>% 
  mutate(
    names = RM,
    decil_renda = as.factor(decil_renda)
  )

plot5 %>%
  ggplot() +
  geom_line(
    data = plot3 %>% filter(Ano == 2017),
    aes(decil_renda, prop, color = Modo, group = interaction(Modo,RM)),
    size = 1) +
  geom_point(
    data = plot3 %>% filter(Ano == 2017),
    aes(decil_renda, prop, color = Modo), size = 2) +
  geom_line(
    data = plot3 %>% filter(Ano == 2008),
    aes(decil_renda, prop, color = Modo, group = interaction(Modo,RM)),
    size = 1, linetype = 'dashed', alpha = .9) +
  scale_color_brewer(palette = 'Dark2') +
  scale_y_continuous(labels = scales::percent, limits = c(0,.5)) +
  labs(
    x = 'Decis de Renda', y = '% da renda total familiar',
    fill = 'Modo', color = 'Modo'
    #title = 'Comprometimento da renda com Transporte: Coletivo e Individual',
    #subtitle = 'Parcela da renda familiar destinada a despesas com transporte, por modo de transporte e faixa de renda \nMédias para regiões metropolitanas em 2017 e 2008 (linha pontilhada)'
    ) +
  facet_wrap(~RM, nrow = 4) +
  theme_minimal() +
  theme(
    legend.position = 'bottom',
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = rel(1.3)),
    plot.subtitle = element_text(size = rel(1)),
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(.8)),
    legend.title = element_text(size = rel(1.2)),
    legend.text = element_text(size = rel(1))) 

# 6. Cleveland: Cor e Raça: RM x Capital ---------------------------------

plot6 <-
  pof_transporte_urbano %>% 
  select(-V1, -V1) %>% 
  filter(etnia == 'Branca' | etnia == 'Preta' | etnia == 'Parda') %>% 
  mutate(cor = ifelse(etnia == 'Branca', 'Branca', 'Preta')) %>% 
  group_by(Ano, cor, genero, Estrato, Modo) %>% 
  summarise(prop = weighted.mean(prop_transp_ind, PESO_FINAL)) %>% 
  mutate(grupo = paste(genero, cor, sep = " "))

plot6$grupo <-
  recode(plot4$grupo,
    'Homem Branca' = 'Homem Branco',
    'Homem Preta' = 'Homem Preto',
    'Mulher Branca' = 'Mulher Branca',
    'Mulher Preta' = 'Mulher Preta'
  )

plot6$Estrato <-
  factor(plot4$Estrato,
    levels = c('Interior Urbano','RM da Capital','Capital')
  )

plot6$Ano <-
  factor(plot4$Ano,
    levels = c('2017','2008','2002')
  )

plot6 %>% 
  mutate(Ano = as.factor(Ano)) %>% 
  ggplot(aes(prop, Estrato, group = Estrato)) +
  geom_path(
    aes(group = interaction(Estrato, genero)),
    position = position_dodge(width = .5),
    linetype = 'dashed', size = 1, alpha = .9) +
  geom_point(
    aes(prop, Estrato, fill = grupo, group = genero),
    size = 4, alpha = 1, shape = 21,
    position = position_dodge(width = .5)) +
  scale_fill_brewer(palette = 'Paired') +
  scale_color_brewer(palette = 'Paired') +
  scale_x_continuous(labels = scales::percent) +
  theme_bw() +
  guides(fill = guide_legend(ncol = 2)) +
  labs(
    x = '% da renda destinada a despesas com transporte', y="",
    fill = '', title = 'Comprometimento da renda com Transporte: 2002 - 2017'
    #subtitle = 'Evolução darcela da renda gasta com transporte urbano por sexo e raça, conforme estrato geográfico.'
    )+
  theme(
    legend.position = 'bottom',
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = rel(1.5)),
    plot.subtitle = element_text(size = rel(1.2)),
    axis.title = element_text(size = rel(1.2)),
    axis.text = element_text(size = rel(1)),
    legend.title = element_text(size = rel(1.2)),
    legend.text = element_text(size = rel(1))) +
  facet_grid(Ano ~ Modo)

#==================================================================================================