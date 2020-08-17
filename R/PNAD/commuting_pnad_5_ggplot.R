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

# Theme ----
tema <- theme(
  text = element_text(family = "Roboto"),
  # Panel
  panel.grid.major.y = element_line(
    linetype = "dotted",colour = '#bfbfbf',size = 0.25
  ),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  panel.border = element_blank(),
  panel.spacing.x = unit(2, "lines"),
  # background
  plot.background = element_rect(fill = NA, colour = NA),
  panel.background = element_rect(fill = NA, colour = NA),
  # Axis
  axis.line.x = element_line(size = 0.5, color = "grey"),
  axis.line.y = element_blank(),
  axis.ticks = element_blank(),
  axis.title.y = element_blank(),
  #axis.title.y = element_text(size = 14, angle = 0, vjust = 0),
  axis.title.x = element_blank(),
  axis.text = element_markdown(size = 14, colour = '#808080'),
  # Titulo
  plot.title = element_markdown(
    lineheight = 1.5, family = "Roboto", size = 20),
  plot.title.position = "plot",
  plot.subtitle = element_markdown(
    lineheight = 1.5, colour = "#808080", family = "Roboto", size = 16,
    margin = margin(t = 0., r = 0, b = 0.5, l = 0, unit = 'cm')
    ),
  plot.caption = element_markdown(
    margin = margin(t = 10), size = 12, hjust = 0, colour = "#808080"),
  plot.caption.position = "plot",
  # Legend
  legend.position = "none",
  legend.title = element_markdown(size = 16, colour = "#808080"),
  legend.key = element_blank(),
  legend.text = element_markdown(size = 14, colour = "#808080"),
  # Strip
  strip.placement = "outside",
  strip.background = ggplot2::element_rect(fill = NA),
  strip.text = element_text(size = 14, face = "plain", colour = "#808080"),
  #strip.text.y.left = element_text(size = 12, face = "bold", colour = "black", angle = 0),
  # Margin
  plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
)

  # Graficos utilizados ----
  
  # 3.1- serie temporal de tempo medio por RM + e urbano nao metropolitano + Media RMs ----

  png("figures/PNAD/ts_mean_rms.png", 
      width = 15.3, height = 8.3, units = 'in', res = 300, type = 'cairo')
  
  ggplot() + 
    geom_line(
      data = transform(mean_time, regiao = NULL),
      aes(x = ano, y = commute_time, group = regiao2),
      colour = 'grey80',
      alpha = 0.6, 
      lineend = 'round', linejoin = 'round'
    ) +
    geom_line(
      data = mean_time,
      aes(x = ano, y = commute_time, group = regiao),
      size = 1, 
      lineend = 'round', linejoin = 'round'
    ) +
    geom_text(
      data = mean_time %>% filter(ano %in% c(2001, 2015)),
      aes(x = ano, y = commute_time + 2.75, label = as.integer(commute_time)),
      size = 4,
      #point.padding = 0.5, segment.colour = NA # for ggrepel::geom_text_repel
    ) +
    lemon::facet_rep_wrap(~reorder(regiao, desc(regiao))) +
    tema +
    theme(panel.grid.major.y = element_blank()) +
    scale_x_continuous(
      breaks = c(2001,2006,2011,2015)#,
      #limits = c(2000.5, 2015.5)
      #expand = expansion(mult = c(0.01, 0.01))
      ) +
    scale_y_continuous(
      expand = expansion(mult = c(0,0.1)),
      limits = c(20,52),
      breaks = seq(20,50, by = 10)
    ) +
    labs(
      title = "Condições de mobilidade nas principais regiões metropolitanas brasileiras (2001-2015)",
      subtitle = 'Tempo médio (em minutos) no deslocamento casa-trabalho',
      caption = 'Fonte: PNAD (IBGE, 2001-2015).'
    )
  
  dev.off()
  
  # 3.2 - tempo no deslocamento por decil - 2001 vs 2015 ----
  
  png("figures/PNAD/mean_decil_rms.png", 
      width = 15.3, height = 8.3, units = 'in', res = 300, type = 'cairo')
  
  ggplot(
    data = mean_time_decil %>% filter(ano %in% c(2001, 2015)),
    aes(x = decilBR, y = commute_time, group = as.factor(ano))
  ) +
    geom_line(
      aes(colour = as.factor(reorder(ano, desc(ano)))),
      size = 0.75
      ) + 
    geom_point(
      aes(colour = as.factor(reorder(ano, desc(ano)))),
      size = 2
      ) +
    geom_text(
      data = mean_time_decil %>% 
        filter(ano %in% c(2015) & decilBR %in% c(1,10)),
      aes(x = decilBR, y = commute_time + 5, label = as.integer(commute_time), 
          colour = as.factor(reorder(ano, desc(ano)))),
      size = 4,
      #point.padding = 0.5, segment.colour = NA # for ggrepel::geom_text_repel
    ) +
    lemon::facet_rep_wrap(~reorder(regiao, desc(regiao))) +
    tema +
    theme(
      axis.title.x = element_markdown(
        size = 16, margin = margin(t = 8, b = 2, unit = 'pt'), colour = "#808080"),
      #panel.grid.major.y = element_blank()
      #legend.position = 'bottom',
      ) +
    scale_x_continuous(breaks = 1:10) +
    scale_y_continuous(
      expand = expansion(mult = c(0,0.05)),
      limits = c(20,53)
    ) +
    scale_colour_manual(
      values = c('2001' = "#bc5090", '2015' = "#003f5c")
    ) +
    labs(
      title = "Diferenças nas condições de mobilidade por renda nas cidades brasileiras entre <b style='color:#bc5090'>2001</b> e <b style='color:#003f5c'>2015</b>",
      subtitle = "Tempo médio (em minutos) no deslocamento casa-trabalho do 1º (mais pobre) ao 10º (mais rico) decil de Renda domiciliar *per capita*",
      caption = "Fonte: PNAD (IBGE, 2001 e 2015).",
      colour = "Renda domiciliar per capita",
      #linetype = "Ano", shape = "Ano",
      x = "Decil de renda"
    ) 
  
  dev.off()
  
  # 3.3 - cleveland plot para extremos de renda em cada RM 2001 vs 2015 ----
  
    ## Legenda no titulo
  png("figures/PNAD/clev_1040_mean_sem_legenda_com_media.png", 
      width = 15.3, height = 8.3, units = 'in', res = 300, type = 'cairo')
  
  ggplot(
    data = mean_time_r_10_p_40 %>% 
      filter(ano %in% c(2001,2015)),
    aes(x = as.factor(ano), y = commute_time)
  ) +
    geom_line(aes(group = ano), colour = '#bfbfbf') +
    geom_point(
      data = mean_time %>% filter(ano %in% c(2001, 2015)),
      colour = '#9a9a9a', size = 3, alpha = 0.9
    ) +
    geom_point(aes(colour = r_10_p_40), size = 3, alpha = 0.9) +
    lemon::facet_rep_grid(~regiao_wrap)  +
    #scale_x_continuous(
    #  position = 'top',
    #  breaks = c(20,30,40), limits = c(20,50), expand = expansion(c(0,0))
    #) +
    scale_y_continuous(
      breaks = seq(20,50, by = 10), limits = c(20,50), expand = expansion(mult = c(0,0.01))
    ) +
    scale_colour_manual(
      values = c("10% mais ricos" = "#00BFC4", "40% mais pobres" = "#F8766D")
    ) +
    tema +
    #theme(panel.grid.minor.y = element_line(
    #  linetype = "dotted",colour = '#bfbfbf',size = 0.25
    #)) +
    theme(
      legend.position = 'none',
      panel.spacing.x = unit(-0.05, "lines"),
      #strip.text = element_text(size = 13, face = "plain", colour = "#808080")
      ) +
    labs(
      colour = "Renda domiciliar per capita",
      title = "Diferença nas condições de mobilidade entre os <b style='color:#00BFC4'>10% mais ricos</b>, <b style='color:#9a9a9a'>a média</b> e os <b style='color:#F8766D'>40% mais pobres</b> das cidades brasileiras",
      subtitle = "Tempo médio (em minutos) no deslocamento casa-trabalho, de acordo com a Renda domiciliar *per capita* (2001 e 2015)",
      caption = "Fonte: PNAD (IBGE, 2001 e 2015)."
    )
  
  dev.off()
  
  
  # 3.4 - cleveland plot de tempo por escolaridade & sexo em cada RM 2001 vs 2015 ----
  
    # Eixo y = factor(ano)
  png("figures/PNAD/clev_esc_raca_sexo_mean.png", 
      width = 15.3, height = 8.3, units = 'in', res = 300, type = 'cairo')
  
  anotacoes <- data.frame(
    anotacao = c('primeira', 'segunda', 'terceira'),
    ano = c(rep(2015,2),2001),
    escolaridade = c("Média","Média","Média"),
    commute_time = c(32.5, 47, 45),
    legenda = c(
      "Elevação no tempo de<br>deslocamento de<br>todos os grupos em<br>2015...",
      "...especialmente para <br> <b style='color:#2166ac'>Homens Negros</b><br>e <b style='color:#b2182b'>Mulheres Negras</b>...",
      #"...Porém, alguns<br>grupos foram<br>afetados de maneira<br>desproporcional.<br>Particularmente,<br><b style='color:#b2182b'> Mulheres Negras</b> de<br>baixa escolaridade."
      "...aumentando a desigualdade<br>já existente em todos os<br>níveis de escolaridade em 2001."
    )
  )
  
  ggplot(
    data = mean_time_esc_raca_sexo %>% 
      filter(ano %in% c(2001,2015) & regiao == "Regiões Metropol."),
    aes(x = commute_time, y = escolaridade) #as.factor(ano)
  ) +
    geom_line(aes(group = escolaridade), colour = '#bfbfbf') +
    geom_point(aes(colour = raca_sexo), size = 3.5#, alpha = 0.85
    ) +
    ggtext::geom_richtext(
      data = anotacoes,
      aes(x = commute_time, y = escolaridade, label = legenda),
      colour = 'black',
      #fontface = 'bold',
      family = "Roboto",
      size = 4,
      fill = NA, label.color = NA, # remove background and outline
    #  label.padding = grid::unit(rep(0, 4), "pt") # remove padding
    ) +
    lemon::facet_rep_wrap(
      ~reorder(as.factor(ano), desc(as.factor(ano))), 
      nrow = 2,
      scales = "free_x",repeat.tick.labels = T
    )  +
    scale_x_continuous(
      position = 'top',
      limits = c(29.4,50),
      expand = c(0,0)
    ) +
    scale_colour_manual(
      values = c(
        "Mulher Negra" = '#b2182b',
        "Homem Negro" = '#2166ac',
        'Mulher Branca' = '#f4a582',
        'Homem Branco' = '#92c5de'
      )
    ) +
    tema +
    theme(
      axis.line.x = element_line(size = 0.5, color = "grey"),
      #axis.line.x = element_blank(),
      axis.line.y = element_line(size = 0.5, color = "grey"),
      plot.margin = margin(t = 1.5, r = 14, b = 1.5, l = 14, unit = 'lines'),
      #panel.grid.major.x = element_line(
      #  linetype = "dotted",colour = '#bfbfbf',size = 0.25
      #),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position = 'bottom',
      axis.text.y = element_markdown(vjust = 0.5),
      axis.title.y = element_markdown(
        size = 16, 
        #angle = 0,
        colour = "#808080", 
        #vjust = 1
        #margin = margin(t = 8, b = 2, unit = 'pt')
      )
    ) +
    labs(
      colour = "Sexo e Etnia",
      title = "Condições de mobilidade por características sociodemográficas nas<br>Regiões Metropolitanas brasileiras (2001 e 2015)",
      subtitle = 
        #expression(paste('Tempo médio (em minutos) no deslocamento por grau de ', escolaridade^1,', de acordo com sexo e etnia')),
        "Deslocamento médio (em minutos) por grau de escolaridade<sup>1</sup>, de acordo com sexo e etnia",
      caption = "Fonte: PNAD (IBGE, 2001 e 2015).<br>Nota: <sup>1</sup>Escolaridade - Baixa: Ensino Fundamental; Média: Ensino Médio; Alta: Ensino Superior.",
      #"Fonte: PNAD (IBGE, 2001 e 2015).<br>Nota: <sup>1</sup>Escolaridade - Baixa: Até 7 anos de estudo; Média: 8 à 14 anos de estudo; Alta: 15 ou mais anos de estudo."
      y = "Escolaridade"
    ) +
    guides(colour = guide_legend(reverse = T))
  
  
  dev.off()
  
  
  
  
  
  
  
  
  # remover tudo ----
  #rm(list = ls())
  

  

  
  








