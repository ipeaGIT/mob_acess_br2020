# Libraries ----
source("./R/setup.R")
source('R/colours.R')
# funcoes -----

fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
} 

# https://stackoverflow.com/a/10526535/12707859
annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data){
    layer(data = data, stat = StatIdentity, position = PositionIdentity, 
          geom = ggplot2:::GeomCustomAnn,
          inherit.aes = TRUE, params = list(grob = grob, 
                                            xmin = xmin, xmax = xmax, 
                                            ymin = ymin, ymax = ymax))
  }

# Read clean data ----
#tab_1419 <- fread(file = "output/dados/sidra_1419_cleaned.csv")
tab_1419 <- readRDS(file = "data/inflacao/sidra_1419_cleaned.rds")

# Bases ----

# * Bases Grupos -----
tab_1419_grupos <- tab_1419 %>% 
  filter(
    regiao %in% 'Brasil' & tipo %in% c('Índice geral',"Grupo") & variavel %in% "IPCA - Variação mensal"
  ) %>% 
  mutate(
    componente = fct_drop(componente),
    componente = factor(
      componente, 
      levels = c(
        # Maior (oleo) -> menor (automovel novo)
        "Educação","Despesas pessoais","Saúde e cuidados pessoais","Alimentação e bebidas",
        'Habitação','IPCA','Transportes','Vestuário','Artigos de residência','Comunicação'
        ), 
      ordered = T),
    componente2 = componente
  )

## Base ultimas observacoes (legenda valor)
ultimo_facets_grupo <- tab_1419_grupos %>% 
  filter(
    mes == max(mes) & 
      componente %in% c('IPCA') &
      variavel %in% "IPCA - Variação mensal" &
      regiao == "Brasil"
  ) %>% 
  mutate(
    mes_legenda = as.Date('2020-08-01'),
    legenda = "IPCA<br>(56%)",
    componente = 'Educação'
  )

## Base ultimas observacoes (legenda valor)
ultimo_facets_transportes <- tab_1419_grupos %>% 
  filter(
    mes == max(mes) & 
      componente %in% c('Transportes') &
      variavel %in% "IPCA - Variação mensal" &
      regiao == "Brasil"
  ) %>% 
  mutate(
    mes_legenda = as.Date('2020-08-01'),
    legenda = "(39%)"
  )


# * Bases Brasil Subitens (facets = subitens) ----
  ## Base filtrada
tab_1419_subitens <- tab_1419 %>% 
  filter(
    regiao %in% 'Brasil' & 
      componente %in% c(
        "IPCA","Gasolina","Metrô",'Óleo diesel',"Ônibus urbano","Automóvel novo", 'Motocicleta'
      ) &
      variavel %in% "IPCA - Variação mensal"
  ) %>% 
  mutate(
    componente = fct_drop(componente),
    componente = factor(
      componente, 
      levels = c(
        # Maior (oleo) -> menor (automovel novo)
        "Óleo diesel","Gasolina","Ônibus urbano","IPCA","Metrô","Motocicleta",'Automóvel novo'), 
      ordered = T),
    componente2 = componente,
    item = fct_case_when(
      componente %in% c('Óleo diesel','Gasolina') ~ 'Combustíveis',
      componente %in% c('Ônibus urbano','Metrô') ~ 'Transporte público',
      componente %in% c('Motocicleta','Automóvel novo') ~ 'Veículo próprio',
      TRUE ~ 'IPCA'
    )
  )

## Base ultimas observacoes (legenda valor)
ultimo_subitens <- tab_1419 %>% 
  filter(
    mes==max(mes) & 
      componente %in% c(
        "Gasolina","Metrô",'Óleo diesel',"Ônibus urbano","Automóvel novo", 'Motocicleta'
      ) &
      variavel %in% "IPCA - Variação mensal" &
      regiao == "Brasil"
  ) %>% 
  arrange(componente_orig) %>% 
  mutate(
    mes_legenda = as.Date('2019-12-01'),
    legenda = c("62%",'48%','7%','13%','65%','86%'),
    acumulada_novo = c(0.685,0.41,0,0.195,0.7,0.925)
  )

# * Bases RMs ----

## Base filtrada RMs
tab_1419_RMs <- tab_1419 %>% 
  filter(
    !regiao %in% 'Brasil' & 
      componente %in% c(
        "IPCA","Gasolina","Metrô",'Óleo diesel',"Ônibus urbano","Automóvel novo", 'Motocicleta'
      ) &
      variavel %in% "IPCA - Variação mensal"
  ) %>% 
  mutate(
    componente = fct_drop(componente),
    componente = factor(
      componente, 
      levels = c(
        "Óleo diesel","Gasolina","Ônibus urbano","IPCA","Metrô","Motocicleta",'Automóvel novo'), 
      ordered = T),
    componente2 = componente,
    item = fct_case_when(
      componente %in% c('Óleo diesel','Gasolina') ~ 'Combustíveis',
      componente %in% c('Ônibus urbano','Metrô') ~ 'Transporte público',
      componente %in% c('Motocicleta','Automóvel novo') ~ 'Veículo próprio',
      TRUE ~ 'IPCA'
    )
  )

## Base ultimas observacoes (legenda valor)
ultimo_RMs <- data.table(
  regiao = c(
    rep('Brasília - DF',2), rep('Goiânia - GO',2), rep('Recife - PE',1), rep('São Paulo - SP',2)
  ),
  mes = as.Date('2020-09-01'),
  acumulada = c(0.8, 0.35, 0.95, 0.55, 0.35, 0.05, 0.55),
  legenda = c("Ônibus<br>urbano", 'Gasolina', 'Óleo<br>diesel', 'IPCA', 'Motocicleta',
              "Automóvel<br>novo", "Metrô"),
  componente = c("Ônibus urbano", 'Gasolina','Óleo diesel', 'IPCA', 'Motocicleta',
                 'Automóvel novo', "Metrô")
)




# Graficos utilizados ----

# * Grafico: Infl. acumulada; Brasil; facets = Subitens ----

# base para criar linhas verticais
linhas_vert_BR <- data.table(
  componente = c(
    "Óleo diesel", rep('Gasolina',2), "Ônibus urbano", 'Metrô'
  ),
  mes = c(
    as.Date('2018-05-01'),
    as.Date('2016-06-01'),
    as.Date('2017-06-01'),
    rep(as.Date('2013-06-06'),2)
    )
  )

# Com facets (componente)

png("figures/inflacao/acumulada_brasil_transportes_facets.png", 
    width = 16, height = 10, units = 'cm', res = 300, type = 'cairo')

pdf("figures/inflacao/acumulada_brasil_transportes_facets.pdf", 
    width = measurements::conv_unit(16, 'cm', 'inch'), 
    height = measurements::conv_unit(10, 'cm', 'inch')
    ) 

gg_BR <- ggplot(
  data = tab_1419_subitens %>% filter(!componente=='IPCA')
) + 
  geom_vline(
    data = linhas_vert_BR,
    aes(xintercept = mes),
    linetype = 'dotted', colour = '#575757', size = 0.35
  ) +
  geom_line(
    data = transform(tab_1419_subitens %>% filter(!componente=='IPCA'), componente = NULL),
    aes(mes, acumulada, group = componente2), 
    colour = 'grey80',
    alpha = 0.6, 
    #size = 0.75,
    lineend = 'round', linejoin = 'round'
  ) +
  geom_line(
    data = transform(tab_1419_subitens %>% filter(componente=='IPCA'), componente = NULL),
    aes(mes, acumulada, group = componente2), 
    colour = '#323232',
    alpha = 0.9, 
    size = 0.75,
    linetype = 'twodash',lineend = 'round', linejoin = 'round'
  ) +
  ggtext::geom_richtext(
    data = ultimo_subitens,
    aes(mes_legenda, acumulada_novo, colour = componente, label = legenda),
    family = "Helvetica",
    size = 2.81, 
    fill = NA, label.color = NA#, # remove background and outline
    #label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  ) +
  geom_line(
    aes(mes, acumulada, group = componente, colour = componente),
    size = 0.75
  )  +
  lemon::facet_rep_wrap(
    ~factor(componente), ncol = 3, 
    #scales = "fixed", 
    #repeat.tick.labels = F
  ) +
  hrbrthemes::scale_y_percent(
    expand = expansion(add = 0.05)
  ) +
  ggplot2::scale_x_date(
    #date_breaks = "24 months", 
    limits = as.Date(c('2012-01-01','2020-01-01')),
    expand = expansion(mult = c(0,0.032)),
    date_labels = "%Y"#,
    #breaks = function(x) seq.Date(from = min(x), to = max(x), by = "24 months")
  ) + 
  scale_colour_aop(
    values = c(
      ## cores paleta 'ipea'
      # Menor (automovel novo -> Maior (Oleo diesel)
      'Automóvel novo' = '#326287', 'Motocicleta' = '#6a9bb3',
      'Metrô' = '#6c6766','Ônibus urbano' = '#c29365', 
      'Gasolina' = '#a85a3f', 'Óleo diesel' = '#872E2B'
    )
  ) +
  aop_style() +
  theme(
    plot.margin = margin(t = 0.5, r = 1.5, b = 0.5, l = 0.25, unit = 'cm'),
    strip.text = element_blank(),
    panel.spacing.y = unit(0.25, "cm"),
    plot.subtitle = element_markdown(
      margin = margin(t = 0, r = 0, b = 1.25, l = 0, unit = 'cm')
    )
  ) +
  labs(
    #title = 'Encarecimento relativo do transporte público e barateamento do transporte privado no Brasil',
    subtitle = "Inflação acumulada dos subitens do <b style='color:#323232'>IPCA</b> associados ao Transporte (2012-2019)",
    #caption = "Fonte: IBGE - Índice Nacional de Preços ao Consumidor Amplo (IPCA).<br>Nota: Unidade territorial de cálculo do índice: Brasil."
    x = 'Ano',
    y = 'Inflação acumulada'
  )

# Lista testGrobs
lista_grob_BR <- list(
  oleo = textGrob(
    label = "Óleo\ndiesel",
    gp = grid::gpar(fontsize = 8, col = '#872E2B', lineheight = 0.75, fontface = 'bold',
                    fontfamily = "Helvetica")
  ),
  gasolina = textGrob(
    label = "Gasolina",
    gp = grid::gpar(fontsize = 8, col = '#a85a3f', lineheight = 0.75, fontface = 'bold',
                    fontfamily = "Helvetica")
  ),
  onibus = textGrob(
    label = "Ônibus\nurbano",
    gp = grid::gpar(fontsize = 8, col = '#c29365', lineheight = 0.75, fontface = 'bold',
                    fontfamily = "Helvetica")
  ),
  ipca_text_grob = textGrob(
    label = "IPCA",
    gp = grid::gpar(fontsize = 8, col = '#323232', lineheight = 0.5, fontface = 'bold',
                    fontfamily = "Helvetica")
  ),
  ipca_number_grob = textGrob(
    label = "56%",
    gp = grid::gpar(fontsize = 8, col = '#323232', lineheight = 0.5,
                    fontfamily = "Helvetica")
  ),
  metro = textGrob(
    label = "Metrô",
    gp = grid::gpar(fontsize = 8, col = '#6c6766', lineheight = 0.75, fontface = 'bold',
                    fontfamily = "Helvetica")
  ),
  motocicleta = textGrob(
    label = "Motocicleta",
    gp = grid::gpar(fontsize = 8, col = '#6a9bb3', lineheight = 0.75, fontface = 'bold',
                    fontfamily = "Helvetica")
  ),
  automovel = textGrob(
    label = "Automóvel\nnovo",
    gp = grid::gpar(fontsize = 8, col = '#326287', lineheight = 0.75, fontface = 'bold',
                    fontfamily = "Helvetica")
  ),
  jun2016_data = textGrob(
    label = "Jun/2016", 
    just = 'right',
    gp = grid::gpar(fontsize = 8, col = '#323232', lineheight = 0.75,
                    fontfamily = "Helvetica")
  ),
  jun2016_texto = textGrob(
    label = "Fim da política de\ncontrole de preços",
    just = 'right',
    gp = grid::gpar(fontsize = 8, col = '#323232', lineheight = 0.75, fontface = 'bold',
                    fontfamily = "Helvetica")
  ),
  jun2017_data = textGrob(
    label = "Jun/2017",
    just = 'left',
    gp = grid::gpar(fontsize = 8, col = '#323232', lineheight = 0.75,
                    fontfamily = "Helvetica")
  ),
  jun2017_texto = textGrob(
    label = "Maior frequência\nde reajustes",
    just = 'left',
    gp = grid::gpar(fontsize = 8, col = '#323232', lineheight = 0.75, fontface = 'bold',
                    fontfamily = "Helvetica")
  ),
  maio2018_data = textGrob(
    label = "Maio/2018",
    just = 'right',
    gp = grid::gpar(fontsize = 8, col = '#323232', lineheight = 0.75,
                    fontfamily = "Helvetica")
  ),
  maio2018_texto = textGrob(
    label = "Greve dos\ncaminhoneiros",
    just = 'right',
    gp = grid::gpar(fontsize = 8, col = '#323232', lineheight = 0.75, fontface = 'bold',
                    fontfamily = "Helvetica")
  ),
  jun_2013_data = textGrob(
    label = "Jun/2013",
    just = 'left',
    gp = grid::gpar(fontsize = 8, col = '#323232', lineheight = 0.75,
                    fontfamily = "Helvetica")
  ),
  jun_2013_texto = textGrob(
    label = "Protestos contra aumentos\nno transporte público",
    just = 'left',
    gp = grid::gpar(fontsize = 8, col = '#323232', lineheight = 0.75, fontface = 'bold',
                    fontfamily = "Helvetica")
  )
)

# Add annotations
gg_annotation_BR <- gg_BR + 
  annotation_custom2(
    grob = lista_grob_BR$ipca_text_grob,
    xmin = as.Date('2020-10-01'), xmax = as.Date("2020-10-01"),
    ymin = 0.56, ymax = 0.56, data = data.frame(componente = "Óleo diesel")
  ) +
  annotation_custom2(
    grob = lista_grob_BR$ipca_number_grob, 
    xmin = as.Date('2019-12-01'), xmax = as.Date("2019-12-01"),
    ymin = 0.46, ymax = 0.46, data = data.frame(componente = "Óleo diesel")
  ) +
  annotation_custom2(
    grob = lista_grob_BR$oleo, 
    xmin = as.Date('2020-10-01'), xmax = as.Date("2020-10-01"),
    ymin = 0.79, ymax = 0.79, data = data.frame(componente = "Óleo diesel")
  ) +
  annotation_custom2(
    grob = lista_grob_BR$gasolina, 
    xmin = as.Date('2021-06-01'), xmax = as.Date("2021-06-01"),
    ymin = 0.61, ymax = 0.61, data = data.frame(componente = "Gasolina")
  ) +
  annotation_custom2(
    grob = lista_grob_BR$onibus, 
    xmin = as.Date('2021-08-01'), xmax = as.Date("2021-08-01"),
    ymin = 0.62, ymax = 0.62, data = data.frame(componente = "Ônibus urbano")
  ) +
  annotation_custom2(
    grob = lista_grob_BR$metro, 
    xmin = as.Date('2020-12-01'), xmax = as.Date("2020-12-01"),
    ymin = 0.52, ymax = 0.52, data = data.frame(componente = "Metrô")
  ) +
  annotation_custom2(
    grob = lista_grob_BR$motocicleta, 
    xmin = as.Date('2019-12-01'), xmax = as.Date("2019-12-01"),
    ymin = 0.3, ymax = 0.3, data = data.frame(componente = "Motocicleta")
  ) +
  annotation_custom2(
    grob = lista_grob_BR$automovel, 
    xmin = as.Date('2021-08-01'), xmax = as.Date("2021-08-01"),
    ymin = 0.075, ymax = 0.075, data = data.frame(componente = "Automóvel novo")
  ) +
  annotation_custom2(
    grob = lista_grob_BR$jun2016_data, 
    xmin = as.Date('2016-06-01'), xmax = as.Date("2016-06-01"),
    ymin = 1.04, ymax = 1.04, data = data.frame(componente = "Gasolina")
  ) +
  annotation_custom2(
    grob = lista_grob_BR$jun2016_texto, 
    xmin = as.Date('2016-06-01'), xmax = as.Date("2016-06-01"),
    ymin = 1.2, ymax = 1.2, data = data.frame(componente = "Gasolina")
  ) +
  annotation_custom2(
    grob = lista_grob_BR$jun2017_data, 
    xmin = as.Date('2017-06-01'), xmax = as.Date("2017-06-01"),
    ymin = 1.04, ymax = 1.04, data = data.frame(componente = "Gasolina")
  ) +
  annotation_custom2(
    grob = lista_grob_BR$jun2017_texto, 
    xmin = as.Date('2017-06-01'), xmax = as.Date("2017-06-01"),
    ymin = 1.2, ymax = 1.2, data = data.frame(componente = "Gasolina")
  ) +
  annotation_custom2(
    grob = lista_grob_BR$maio2018_data, 
    xmin = as.Date('2018-05-01'), xmax = as.Date("2018-05-01"),
    ymin = 1.04, ymax = 1.04, data = data.frame(componente = "Óleo diesel")
  ) +
  annotation_custom2(
    grob = lista_grob_BR$maio2018_texto, 
    xmin = as.Date('2018-05-01'), xmax = as.Date("2018-05-01"),
    ymin = 1.2, ymax = 1.2, data = data.frame(componente = "Óleo diesel")
  ) +
  annotation_custom2(
    grob = lista_grob_BR$jun_2013_data, 
    xmin = as.Date('2013-06-01'), xmax = as.Date("2013-06-01"),
    ymin = 1.04, ymax = 1.04, data = data.frame(componente = "Ônibus urbano")
  ) +
  annotation_custom2(
    grob = lista_grob_BR$jun_2013_texto, 
    xmin = as.Date('2013-06-01'), xmax = as.Date("2013-06-01"),
    ymin = 1.2, ymax = 1.2, data = data.frame(componente = "Ônibus urbano")
  ) +
  annotation_custom2(
    grob = lista_grob_BR$jun_2013_data, 
    xmin = as.Date('2013-06-01'), xmax = as.Date("2013-06-01"),
    ymin = 1.04, ymax = 1.04, data = data.frame(componente = "Metrô")
  ) +
  
  ## lines ; ref: https://stackoverflow.com/a/40622343/12707859
  
  # jun/2016
  annotation_custom2(
    grob = linesGrob(y = c(0, 0), x = c(0.235, 0.535),  
                     gp = gpar(col = "#575757", lwd = 1, lty="solid")),
    ymin = 0.985, ymax = 0.985, 
    xmin = -Inf, xmax = Inf, 
    data = data.frame(componente = "Gasolina")
  ) +
  # jun/2017
  annotation_custom2(
    grob = linesGrob(y = c(0, 0), x = c(0.6575, 0.9575),  
                     gp = gpar(col = "#575757", lwd = 1, lty="solid")),
    ymin = 0.985, ymax = 0.985, 
    xmin = -Inf, xmax = Inf, 
    data = data.frame(componente = "Gasolina")
  ) +
  # maio/2018
  annotation_custom2(
    grob = linesGrob(y = c(0, 0), x = c(0.4245, 0.7675),  
                     gp = gpar(col = "#575757", lwd = 1, lty="solid")),
    ymin = 0.985, ymax = 0.985, 
    xmin = -Inf, xmax = Inf, 
    data = data.frame(componente = "Óleo diesel")
  ) +
  # jun/2013 - ONIBUS
  annotation_custom2(
    grob = linesGrob(y = c(0, 0), x = c(0.1725, 0.4725),  
                     gp = gpar(col = "#575757", lwd = 1, lty="solid")),
    ymin = 0.985, ymax = 0.985, 
    xmin = -Inf, xmax = Inf, 
    data = data.frame(componente = "Ônibus urbano")
  ) +
  # jun/2013 - METRO
  annotation_custom2(
    grob = linesGrob(y = c(0, 0), x = c(0.1725, 0.4725),  
                     gp = gpar(col = "#575757", lwd = 1, lty="solid")),
    ymin = 0.985, ymax = 0.985, 
    xmin = -Inf, xmax = Inf, 
    data = data.frame(componente = "Metrô")
  )

# Code to override clipping
gt_BR <- ggplotGrob(gg_annotation_BR)
gt_BR$layout[grepl("panel", gt_BR$layout$name), ]$clip <- "off"

# Draw the plot
grid.newpage()
grid.draw(gt_BR)

dev.off()

# Save plot
#ggsave("figures/inflacao/teste.png", 
#       width = 16, height = 12, units = "cm", dpi = 300, device = 'png')

#ggsave("figures/inflacao/acumulada_brasil_transportes_facets.pdf", 
#       width = 16, height = 12, units = "cm", dpi = 300, device = 'pdf')


# * Grafico: Infl. acumulada; facets = RMs ----

png("figures/inflacao/acumulada_transportes_rms_facets.png", 
    width = 16, height = 12, units = 'cm', res = 300, type = 'cairo')

pdf("figures/inflacao/acumulada_transportes_rms_facets.pdf", 
    width = measurements::conv_unit(16, 'cm', 'inch'), 
    height = measurements::conv_unit(12, 'cm', 'inch')
) 

gg_RMs <- ggplot(
  data = tab_1419_RMs %>% 
    filter(!regiao %in% c(
      'Aracaju - SE','Campo Grande - MS','Rio Branco - AC','São Luís - MA'
    ))
) +
  geom_line(
    aes(mes, acumulada, group = componente, colour = componente, linetype = componente),
    size = 0.75
  ) +
  lemon::facet_rep_wrap(~factor(regiao), nrow = 4) +
  hrbrthemes::scale_y_percent(
    expand = expansion(mult = 0.01),
    limits = c(-0.15, 1.0),
    breaks = seq(0,1, by = 0.25)
  ) +
  ggplot2::scale_x_date(
    #date_breaks = "24 months", 
    #limits = as.Date(c('2012-01-01','2021-01-01')),
    limits = as.Date(c('2012-01-01','2020-01-01')),
    expand = expansion(mult = c(0,0.032)),
    date_labels = "%Y"#,
    #breaks = function(x) seq.Date(from = min(x), to = max(x), by = "24 months")
  ) + 
  scale_colour_aop(
    values = c(
      # Menor (automovel novo -> Maior (Oleo diesel)
      'Automóvel novo' = '#326287', 'Motocicleta' = '#6a9bb3',
      'Metrô' = '#6c6766','Ônibus urbano' = '#c29365', 
      'Gasolina' = '#a85a3f', 'Óleo diesel' = '#872E2B',
      # IPCA CINZA ESCURO
      'IPCA' = '#323232'
    )
  ) +
  scale_linetype_manual(
    values = c(
      'Automóvel novo' = 'solid', 'Motocicleta' = 'solid',
      'Gasolina' = 'solid', 'Óleo diesel' = 'solid',
      'Ônibus urbano' = 'solid', 'Metrô' = 'solid',
      'IPCA' = 'twodash'
    )
  ) +
  aop_style() +
  theme(
    #plot.margin = margin(t = 0.5, r = 3, b = 0.5, l = 0.5, unit = 'cm'),
    plot.margin = margin(t = 0.5, r = 1.55, b = 0.5, l = 0.25, unit = 'cm'),
    panel.spacing.x = unit(-0.25, "cm"),
  ) +
  labs(
    #title = 'Encarecimento relativo do transporte público e barateamento do transporte privado nas cidades brasileiras',
    subtitle = "Inflação acumulada dos subitens do <b style='color:#323232'>IPCA</b> associados ao Transporte por região metropolitana (2012-2019)",
    #caption = "Fonte: IBGE - Índice Nacional de Preços ao Consumidor Amplo (IPCA).<br>Nota: Unidades territoriais de cálculo do índice: regiões metropolitanas de Belém (PA), Fortaleza (CE), Recife (PE), Salvador (BA), Belo Horizonte (MG), Grande Vitória (ES), Rio de Janeiro (RJ),<br>São Paulo (SP), Curitiba (PR) e Porto Alegre (RS) e municípios de Goiânia (GO) e Brasília (DF).",
    colour = "Componente",
    linetype = "Componente",
    x = 'Ano',
    y = 'Inflação acumulada'
  )

# Lista testGrobs
lista_grob_RMs <- list(
  oleo = textGrob(
    label = "Óleo diesel",
    gp = grid::gpar(fontsize = 8, col = '#872E2B', lineheight = 0.75, fontface = 'bold',
                    fontfamily = "Helvetica")
  ),
  gasolina = textGrob(
    label = "Gasolina",
    gp = grid::gpar(fontsize = 8, col = '#a85a3f', lineheight = 0.75, fontface = 'bold',
                    fontfamily = "Helvetica")
  ),
  onibus = textGrob(
    label = "Ônibus\nurbano",
    gp = grid::gpar(fontsize = 8, col = '#c29365', lineheight = 0.75, fontface = 'bold',
                    fontfamily = "Helvetica")
  ),
  ipca = textGrob(
    label = "IPCA",
    gp = grid::gpar(fontsize = 8, col = '#323232', lineheight = 0.75, fontface = 'bold',
                    fontfamily = "Helvetica")
  ),
  metro = textGrob(
    label = "Metrô",
    gp = grid::gpar(fontsize = 8, col = '#6c6766', lineheight = 0.75, fontface = 'bold',
                    fontfamily = "Helvetica")
  ),
  motocicleta = textGrob(
    label = "Motocicleta",
    gp = grid::gpar(fontsize = 8, col = '#6a9bb3', lineheight = 0.75, fontface = 'bold',
                    fontfamily = "Helvetica")
  ),
  automovel = textGrob(
    label = "Automóvel\nnovo",
    gp = grid::gpar(fontsize = 8, col = '#326287', lineheight = 0.75, fontface = 'bold',
                    fontfamily = "Helvetica")
  )
)

# Lista regiao
lista_regiao <- list(
  data_df = data.frame(regiao = "Brasília - DF"),
  data_go = data.frame(regiao = "Goiânia - GO"),
  data_pe = data.frame(regiao = "Recife - PE"),
  data_sp = data.frame(regiao = "São Paulo - SP")
)

# Add annotations
gg_annotation_RMs <- gg_RMs + 
  annotation_custom2(
    lista_grob_RMs$onibus, xmin = as.Date('2021-09-01'), xmax = as.Date("2021-09-01"),
    ymin = 0.70, ymax = 0.7, data = lista_regiao$data_df
  ) +
  annotation_custom2(
    lista_grob_RMs$oleo, xmin = as.Date('2021-09-01'), xmax = as.Date("2021-09-01"),
    ymin = 0.925, ymax = 0.925, data = lista_regiao$data_go
  ) +
  annotation_custom2(
    lista_grob_RMs$gasolina, xmin = as.Date('2021-09-01'), xmax = as.Date("2021-09-01"),
    ymin = 0.625, ymax = 0.625, data = lista_regiao$data_go
  ) +
  annotation_custom2(
    lista_grob_RMs$ipca, xmin = as.Date('2021-09-01'), xmax = as.Date("2021-09-01"),
    ymin = 0.6, ymax = 0.6, data = lista_regiao$data_pe
  ) +
  annotation_custom2(
    lista_grob_RMs$motocicleta, xmin = as.Date('2021-09-01'), xmax = as.Date("2021-09-01"),
    ymin = 0.225, ymax = 0.225, data = lista_regiao$data_pe
  ) +
  annotation_custom2(
    lista_grob_RMs$metro, xmin = as.Date('2021-09-01'), xmax = as.Date("2021-09-01"),
    ymin = 0.50, ymax = 0.50, data = lista_regiao$data_sp
  ) +
  annotation_custom2(
    lista_grob_RMs$automovel, xmin = as.Date('2021-09-01'), xmax = as.Date("2021-09-01"),
    ymin = 0.075, ymax = 0.075, data = lista_regiao$data_sp
  )


# Code to override clipping
gt_RMs <- ggplotGrob(gg_annotation_RMs)
gt_RMs$layout[grepl("panel", gt_RMs$layout$name), ]$clip <- "off"

# Draw the plot
grid.newpage()
grid.draw(gt_RMs)

dev.off()

# Save plot
#ggsave("figures/inflacao/acumulada_transportes_rms_facets.png", 
#       width = 16, height = 8.8, units = "cm", dpi = 300, device = 'png')

#ggsave("figures/inflacao/acumulada_transportes_rms_facets.pdf", 
#       width = 16, height = 8.8, units = "cm", dpi = 300, device = 'pdf')

# Remover tudo ----
#rm(list = ls())
  
