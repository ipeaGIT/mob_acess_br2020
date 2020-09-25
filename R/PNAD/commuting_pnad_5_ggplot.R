# Libraries ----
source('R/colours.R')
source("./R/setup.R")


# Create tables ----

  source(file = "R/PNAD/commuting_pnad_4_read_final_data.R")


fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
} 

annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data){
  layer(data = data, stat = StatIdentity, position = PositionIdentity, 
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = TRUE, params = list(grob = grob, 
                                          xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax))
}

# Graficos utilizados ----
  
  # * 3.1- serie temporal de tempo medio por RM + e urbano nao metropolitano + Media RMs ----

  #png("figures/PNAD/ts_mean_rms.png", 
  #    width = 16, height = 8.8, units = 'cm', res = 600, type = 'cairo')
  
ggplot() + 
  geom_line(
    data = transform(mean_time, regiao = NULL),
    aes(x = ano, y = commute_time, group = regiao2),
    colour = 'grey80',
    alpha = 0.6, 
    size = 0.25,
    lineend = 'round', linejoin = 'round'
  ) +
  geom_line(
    data = mean_time,
    aes(x = ano, y = commute_time, group = regiao),
    size = 0.5, 
    colour = '#323232',
    lineend = 'round', linejoin = 'round'
  ) +
  geom_text(
    data = mean_time %>% filter(ano %in% c(2001, 2015)),
    aes(x = ano, y = commute_time, label = as.integer(commute_time)),
    size = 2.81, nudge_y = 5.5,
    colour = '#323232'
    #point.padding = 0.5, segment.colour = NA # for ggrepel::geom_text_repel
  ) +
  lemon::facet_rep_wrap(~reorder(regiao, desc(regiao))) +
  scale_x_continuous(
    breaks = c(2001,2006,2011,2015)#,
    #limits = c(2000.5, 2015.5)
    #expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0,0.1)),
    limits = c(20,55),
    breaks = seq(20,50, by = 10)
  ) +
  labs(
    #title = "Condições de mobilidade nas principais regiões metropolitanas brasileiras (2001-2015)",
    subtitle = 'Tempo médio (em minutos) no deslocamento casa-trabalho por ano',
    #caption = 'Fonte: PNAD (IBGE, 2001-2015).'
    x = 'Ano',
    y = 'Tempo de deslocamento'
  ) +
  aop_style() +
  coord_cartesian(clip = "off")
  
  ggsave("figures/PNAD/ts_mean_rms.png", 
         width = 16, height = 8.8, units = "cm", dpi = 300, device = 'png')

  ggsave("figures/PNAD/ts_mean_rms.pdf", 
         width = 16, height = 8.8, units = "cm", dpi = 300, device = 'pdf')
  
  # * 3.2 - tempo no deslocamento por decil - 2001 vs 2015 ----
  

  ggplot(
    data = mean_time_decil %>% filter(ano %in% c(2001, 2015)),
    aes(
      x = decilBR, y = commute_time, group = as.factor(ano),
      colour = as.factor(ano)
    )
  ) +
    geom_line() +
    geom_point(
      aes(shape = as.factor(ano))
    ) +
    geom_text(
      data = mean_time_decil %>% 
        filter(ano %in% c(2015) & decilBR %in% c(1,10)),
      aes(x = decilBR, y = commute_time, label = as.integer(commute_time)),
      colour = "#274370",
      size = 2.81,
      nudge_y = 8
    ) +
    lemon::facet_rep_wrap(~reorder(regiao, desc(regiao))) +
    aop_style() +
    theme(
      #panel.grid.major.y = element_blank()
      legend.position = 'bottom',
      legend.box.margin = margin(t = -0.5,r = -0.5,b = -0.5,l = -0.5, unit = 'cm'),
      #legend.spacing.x = unit(0, 'cm'),
      #legend.spacing.y = unit(-0.25, 'cm'),
    ) +
    scale_x_continuous(breaks = 1:10) +
    scale_y_continuous(
      expand = expansion(mult = c(0,0.05)),
      limits = c(20,55)
    ) +
    scale_colour_aop(
      values = c('2001' = "#6A9BB3", '2015' = "#274370")
    ) +
    scale_shape_manual(
      values = c('2001' = 15, '2015' = 19)
    ) +
    labs(
      #title = "Diferenças nas condições de mobilidade por renda nas cidades brasileiras entre <b style='color:#6A9BB3'>2001</b> e <b style='color:#274370'>2015</b>",
      subtitle = "Tempo médio (em minutos) no deslocamento casa-trabalho do 1º (mais pobre) ao 10º (mais rico) decil de<br>Renda domiciliar *per capita*",
      #caption = "Fonte: PNAD (IBGE, 2001 e 2015).",
      #linetype = "Ano", shape = "Ano",
      x = "Decil de renda",
      y = "Tempo de deslocamento",
      colour = 'Ano',
      shape = 'Ano'
    ) +
    coord_cartesian(clip = 'off') #+
    #guides(
    #  colour = guide_legend(ncol = 1, reverse = T),
    #  shape = guide_legend(ncol = 1, reverse = T)
    #  )
  

  ggsave("figures/PNAD/mean_decil_rms.png", 
         width = 16, height = 10, units = "cm", dpi = 300, device = 'png')
  
  ggsave("figures/PNAD/mean_decil_rms.pdf", 
         width = 16, height = 10, units = "cm", dpi = 300, device = 'pdf')
  
  # * 3.3 - cleveland plot para extremos de renda em cada RM 2001 vs 2015 ----
  
  regiao_labels <- c(
    `Brasil Urbano` = 'Brasil\nUrbano', 
    `Belém` = 'Belém',
    `Fortaleza` = 'Fortaleza',
    `Porto Alegre` = 'Porto\nAlegre',
    `Salvador` = 'Salvador',
    `Curitiba` = 'Curitiba',
    `Belo\nHorizonte` = 'Belo\nHorizonte',
    `Recife` = 'Recife',
    `Regiões\nMetropol.` = 'Regiões\nMetrop.',
    `Brasília` = 'Brasília',
    `Rio de\nJaneiro` = 'Rio de\nJaneiro',
    `São Paulo` = 'São\nPaulo'
  )
  
  ggplot(
    data = mean_time_r_10_p_40 %>% 
      filter(ano %in% c(2001,2015)),
    aes(x = as.factor(ano), y = commute_time)
  ) +
    geom_line(aes(group = ano), colour = '#bfbfbf', size = 0.4) +
    geom_point(
      aes(colour = r_10_p_40),
      size = 2, alpha = 0.9
    ) +
    facet_grid(
      ~regiao_wrap, 
      labeller = as_labeller(regiao_labels)
    ) +
    #lemon::facet_rep_grid(
    #  ~regiao_wrap, repeat.tick.labels = F,
    #  labeller = as_labeller(regiao_labels)
    #  ) +
    scale_y_continuous(
      breaks = seq(20,50, by = 10), limits = c(20,50), expand = expansion(mult = c(0,0.05))
    ) +
    scale_x_discrete(
      labels = c('2001' = '01', '2015' = '15')
    ) +
    scale_colour_aop(
      palette = 'blue_red', reverse = F
    ) +
    aop_style() +
    theme(
      legend.position = 'bottom',
      panel.spacing.x = unit(0.05, "cm"),
      plot.margin = unit(c(t = 0.5,r = 0.25, b = 0.5, l = 0.25), "cm"),
      legend.box.margin = margin(t = -0.5,r = -0.5,b = -0.5,l = -0.5, unit = 'cm'),
      legend.spacing.x = unit(0, 'cm'),
      axis.title.y = ggtext::element_markdown(margin = margin(r = 0.1,l = 0,unit = 'cm')),
      axis.text.y = ggtext::element_markdown(margin = margin(r = 0,l = 0.05, unit = 'cm')),
      #axis.text.x = ggtext::element_markdown(size = 7)
      strip.text = element_text(hjust = 0.5)
    ) +
    labs(
      colour = "Renda domiciliar per capita",
      #title = "Diferenças nas mobilidades entre os <b style='color:#274370'>10% mais ricos</b>, <b style='color:#9a9a9a'>a média</b> e os <b style='color:#872E2B'>40% mais pobres</b> das cidades brasileiras",
      subtitle = "Tempo médio (em minutos) no deslocamento casa-trabalho por Renda domiciliar *per capita* (2001 e 2015)",
      #caption = "Fonte: PNAD (IBGE, 2001 e 2015)."
      x = 'Ano',
      y = 'Tempo de deslocamento'
    ) +
    coord_cartesian(clip = 'off') 
    #coord_fixed(ratio = , clip = 'off')
  
  ggsave("figures/PNAD/clev_1040_mean.png", 
         width = 16, height = 12, units = "cm", dpi = 300, device = 'png')
  
  ggsave("figures/PNAD/clev_1040_mean.pdf", 
         width = 16, height = 12, units = "cm", dpi = 300, device = 'pdf')

  
  
  # * 3.4 - cleveland plot de tempo por escolaridade & sexo em cada RM 2001 vs 2015 ----
  
    # Eixo y = factor(ano)

  
  anotacoes <- data.frame(
    anotacao = c('primeira', 'segunda', 'terceira'),
    ano = c(rep(2015,2),2001),
    escolaridade = c("Média","Média","Média"),
    commute_time = c(32.5, 47, 45),
    legenda = c(
      "Elevação no tempo de<br>deslocamento de<br>todos os grupos em<br>2015...",
      "...especialmente para <br> <b style='color:#326287'>Homens Negros</b><br>e <b style='color:#872e2b'>Mulheres Negras</b>...",
      #"...Porém, alguns<br>grupos foram<br>afetados de maneira<br>desproporcional.<br>Particularmente,<br><b style='color:#872E2B'> Mulheres Negras</b> de<br>baixa escolaridade."
      "...aumentando a desigualdade<br>já existente em todos os<br>níveis de escolaridade em 2001."
    )
  )
  
  ggplot(
    data = mean_time_esc_raca_sexo %>% 
      filter(ano %in% c(2001,2015) & regiao == "Regiões Metropol."),
    aes(x = commute_time, y = escolaridade) #as.factor(ano)
  ) +
    geom_line(aes(group = escolaridade), colour = '#bfbfbf') +
    geom_point(aes(colour = raca_sexo), size = 2#, alpha = 0.85
    ) +
    ggtext::geom_richtext(
      data = anotacoes,
      aes(x = commute_time, y = escolaridade, label = legenda),
      colour = '#323232',
      #fontface = 'bold',
      family = "Helvetica",
      size = 2.81,
      fill = NA, label.color = NA, # remove background and outline
    #  label.padding = grid::unit(rep(0, 4), "pt") # remove padding
    ) +
    lemon::facet_rep_wrap(
      ~reorder(as.factor(ano), desc(as.factor(ano))), 
      nrow = 2,
      scales = "free_x",repeat.tick.labels = T
    )  +
    scale_x_continuous(
      position = 'bottom',
      limits = c(29.4,50),
      expand = c(0,0)
    ) +
    scale_colour_aop(
      values = c(
        "Mulher Negra" = '#872e2b',
        "Homem Negro" = '#326287',
        'Mulher Branca' = '#c29365',
        'Homem Branco' = '#6a9bb3'
      )
    ) +
    aop_style() +
    theme(
      axis.line.x = element_line(size = 0.5, color = "grey"),
      #axis.line.x = element_blank(),
      axis.line.y = element_line(size = 0.5, color = "grey"),
      #plot.margin = margin(t = 1.5, r = 14, b = 1.5, l = 14, unit = 'lines'),
      #panel.grid.major.x = element_line(
      #  linetype = "dotted",colour = '#bfbfbf',size = 0.25
      #),
      legend.position = 'bottom',
      axis.text.y = element_markdown(vjust = 0.5),
      panel.spacing.y = unit(0.5, "cm"),
      legend.spacing.x = unit(0, 'cm'),
      legend.spacing.y = unit(-0.25, 'cm'),
      legend.box.margin = margin(t = -0.5,b = -0.25, unit = 'cm'),
      #axis.title.y = element_markdown(
      #  size = 16, 
        #angle = 0,
      #  colour = "#808080", 
        #vjust = 1
        #margin = margin(t = 8, b = 2, unit = 'pt')
      #)
    ) +
    labs(
      colour = "Sexo e Etnia",
      #title = "Condições de mobilidade e características sociodemográficas",
      subtitle = 
        #expression(paste('Tempo médio (em minutos) no deslocamento por grau de ', escolaridade^1,', de acordo com sexo e etnia')),
        #"Deslocamento médio (em minutos) por grau de escolaridade<sup>1</sup>, de acordo com sexo e etnia<br>nas Regiões Metropolitanas brasileiras (2001 e 2015)",
        "Tempo médio (em minutos) no deslocamento por grau de escolaridade, de acordo com sexo e etnia, nas<br>Regiões Metropolitanas brasileiras (2001 e 2015)",
      #caption = "Fonte: PNAD (IBGE, 2001 e 2015).<br>Nota: <sup>1</sup>Escolaridade - Baixa: Ensino Fundamental; Média: Ensino Médio; Alta: Ensino Superior.",
      #"Fonte: PNAD (IBGE, 2001 e 2015).<br>Nota: <sup>1</sup>Escolaridade - Baixa: Até 7 anos de estudo; Média: 8 à 14 anos de estudo; Alta: 15 ou mais anos de estudo."
      y = "Escolaridade",
      x = 'Tempo de deslocamento'
    ) +
    guides(colour = guide_legend(reverse = F, ncol = 2))
  
  
  ggsave("figures/PNAD/clev_esc_raca_sexo_mean.png", 
         width = 16, height = 16, units = "cm", dpi = 600, device = 'png')
  
  ggsave("figures/PNAD/clev_esc_raca_sexo_mean.pdf", 
         width = 16, height = 16, units = "cm", dpi = 600, device = 'pdf')
  
  
  #png("figures/PNAD/clev_esc_raca_sexo_mean.png", 
  #    width = 15.3, height = 8.3, units = 'in', res = 300, type = 'cairo')
  #dev.off()
  
  
  
  
  
  
  
  
  # remover tudo ----
  #rm(list = ls())
  

  

  
  








