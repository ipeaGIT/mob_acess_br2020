### Setup

source("setup.R")


######## 3. Plot data ----------------------------------------------------------

# Vulnerabilidade -------------

pof_status_total <-
  # Recupera df
  fread('pof_status_total.csv') %>% 
  mutate(
    status = factor(status,
      levels = c(
        'Renda Baixa/Gasto Alto', 
        'Renda Alta/Gasto Alto', 
        'Renda Baixa/Gasto Baixo', 
        'Renda Alta/Gasto Baixo'))
  )

pof_status_total %>% filter(Ano == 2017) %>%
  # Plot 1
  ggplot(aes(log(renda), prop)) +
  geom_jitter(
    data = pof_status_total %>% filter(Estrato != 'Capital'),
    size = 1.5, alpha = .5) +
  geom_jitter(
    aes(color = status), size = 1.5, alpha = .5) +
  geom_hline(
    aes(yintercept = prop_avg, group = RM),
    linetype = 'dashed', alpha = .8) +
  geom_vline(
    aes(xintercept = renda_avg, group = RM),
    linetype = 'dashed', alpha = .8) +
  geom_text(
    data = pof_status_total %>% filter(Ano == 2017 & status == "Renda Baixa/Gasto Alto"),
    aes(13,.65, group = RM, label = scales::percent(share)),
    size = 4.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0,.75)) +
  scale_x_continuous(limits = c(8,14)) +
  scale_color_brewer(palette = 'Spectral') +
  theme_bw() +
  labs(
    x = '(log) Renda familiar per capita',
    y = '% da Renda destinada a gastos com transporte',
    color = '',
    title = "POF 2017: Comprometimento da renda com despesas de transporte",
    subtitle = "Parcela da população no quadrante 'Baixa Renda / Gasto Alto' \nPontos em preto indicam moradores da periferia da Região Metropolitana da Capital") +
  theme(legend.position = 'bottom') +
  facet_wrap(~RM, nrow = 2)

ggsave('vulnerabilidade.png')

# Evolução do gasto por tipo -------------

pof_medias <- 
  fread('pof_medias.csv') %>% 
  mutate(
    decil_renda = factor(
      decil_renda, levels = c(
        "Q1 mais pobre",
        "Q2",
        "Q3",
        "Q4",
        "Q5",
        "Q6",
        "Q7",
        "Q8",
        "Q9",
        "Q10 mais rico"))
  )

pof_medias %>% 
  # Plot 2
  ggplot() +
  geom_line(
    data = pof_medias %>%  filter(Ano == 2017),
    aes(decil_renda, prop, group = Modo, color = Modo),
    size = .75) +
 geom_line(
    data = pof_medias %>%  filter(Ano == 2008),
    aes(decil_renda, prop, group = Modo, color = Modo),
    alpha = .9, linetype = 'dotted', size = .8) +
  geom_point(
    data = pof_medias %>%  filter(Ano == 2017),
    aes(decil_renda, prop, color = Modo)) +
  scale_color_brewer(palette = 'Set1') +
  scale_x_discrete(labels = c('1','2','3','4','5','6','7','8','9','10'))+
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(
    x = 'Decis de Renda',
    y= '% da renda gasta com transporte', 
    title = 'Evolução do Gasto por tipo de Transporte', 
    subtitle = 'Parcela da renda total comprometida com gastos com transporte \n Evolução entre 2008 (linha pontilhada) e 2017 (linha contínua).')+
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(~RM, nrow = 2)

ggsave('evolucao.png')

# Vulnerabilidade por Cor e Gênero -------------

pof_status_total %>% filter(Ano == 2017)  %>% 
  # Plot 3
  ggplot(aes(log(renda), prop)) +
  geom_jitter(
    data = pof_status_total %>% filter(etnia == 'Branca' & Ano == 2017),
    aes(color = etnia), size = 1.5, alpha = .5) +
  geom_jitter(
    data = pof_status_total %>% filter(etnia == 'Preta' & Ano == 2017),
    color = 'brown', size = 1.5, alpha = .5) +
  geom_hline(
    aes(yintercept = prop_avg, group = RM),
    linetype = 'dashed', alpha = .8) +
  geom_vline(
    aes(xintercept = renda_avg, group = RM),
    linetype = 'dashed', alpha = .8) +
  scale_y_continuous(labels = scales::percent, limits = c(0,.75)) +
  scale_x_continuous(limits = c(8,14)) +
  scale_color_brewer(palette = 'Dark2') +
  theme_bw() +
  labs(
    x = '(log) Renda familiar per capita',
    y = '% da Renda destinada a gastos com transporte',
    color = 'Cor',
    title = "POF 2017: Comprometimento da renda com despesas de transporte",
    subtitle = "Condição em relação ao gasto e renda médios por raça", color = 'Cor') +
  theme(legend.position = 'bottom') +
  facet_wrap(~RM, nrow = 2)

ggsave('vulnerabilidade2.png')

desigualdade %>% 
# Plot 4
  filter(Ano == '2017') %>% 
  filter(etnia == 'Branca' | etnia == 'Parda'|etnia == 'Preta') %>% 
  ggplot() +
  geom_vline(aes(xintercept = 0), alpha = .9, linetype = 'dashed') +
  geom_point(aes(diff, RM, color = etnia), size = 3, position = position_dodge(width = .75)) +
  geom_linerange(aes(xmin=0,xmax=diff,y=RM, color=etnia),position = position_dodge(width = .75))+
  scale_color_brewer(palette = 'Dark2') +
  scale_x_continuous(breaks = c(-.2,-.1,0,.1,.2),labels = c('-20','10','0','10','20')) +
  theme_bw() +
  scale_y_discrete(guide = guide_axis(n.dodge = 2)) +
  coord_flip() +
  theme(legend.position = 'bottom') +
  labs(y = '', x = 'Diferença entre parcela na população total e \nparticipação no grupo (p.p.)',
       color = 'Cor ou Raça', title = 'POF 2017: Comprometimento da renda com transporte e Desigualdade racial',
       subtitle = 'Diferença (em p.p.) da representação no grupo em relação a composição da população \nDiferença positiva (negativa) indica (sub) sobrerepresentatividade') +
  facet_wrap(~status)

ggsave('desigualdade.png')


