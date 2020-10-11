# Setup ----
source('R/setup.R')
source('R/colours.R')
source('R/style.R')

# Functions ----
fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}


# https://stackoverflow.com/questions/25199851/r-how-to-get-the-week-number-of-the-month/28007793#28007793
# function get number of the week (based on the year or month)
nth_week <- function(dates = NULL,
                     count_weeks_in = c("month","year"),
                     begin_week_on = "Sunday"){
  
  require(lubridate)
  
  count_weeks_in <- tolower(count_weeks_in[1])
  
  # day_names and day_index are for beginning the week on a day other than Sunday
  # (this vector ordering matters, so careful about changing it)
  day_names<- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
  
  # index integer of first match
  day_index <- pmatch(tolower(begin_week_on),
                      tolower(day_names))[1]
  
  
  ### Calculate week index of each day
  
  if (!is.na(pmatch(count_weeks_in, "year"))) {
    
    # For year:
    # sum the day of year, index for day of week at start of year, and constant 5 
    #  then integer divide quantity by 7   
    # (explicit on package so lubridate and data.table don't fight)
    n_week <- (5 + 
                 lubridate::yday(dates) + 
                 lubridate::wday(floor_date(dates, 'year'), 
                                 week_start = day_index)
    ) %/% 7
    
  } else {
    
    # For month:
    # same algorithm as above, but for month rather than year
    n_week <- (5 + 
                 lubridate::day(dates) + 
                 lubridate::wday(floor_date(dates, 'month'), 
                                 week_start = day_index)
    ) %/% 7
    
  }
  
  # naming very helpful for review
  names(n_week) <- paste0(lubridate::wday(dates,T), '-', dates)
  
  n_week
  
}

# DADOS COVID ----
# github IDB https://github.com/EL-BID/IDB-IDB-Invest-Coronavirus-Impact-Dashboard
# dictionary https://github.com/EL-BID/IDB-IDB-Invest-Coronavirus-Impact-Dashboard/blob/master/docs/Data%20Dictionary.md

# download data
daily <- fread(
  "http://tiny.cc/idb-traffic-daily",
  colClasses = c(
    rep('character', 7), 'integer', 'character', rep('integer', 5), rep('double', 2)
  ),
  encoding = "UTF-8"
  )

# filter BR
daily_br <- daily[country_name == 'Brazil'][
  order(month, day)
]


# Verificar o locale atual
Sys.getlocale(category = "LC_TIME")
# Mudar o locale
Sys.setlocale("LC_ALL","pt_BR.UTF-8")



daily_br <- daily_br %>% 
  mutate(
    year = 2020,
    dia = ifelse(day < 10, paste0(0, day), day),
    mes = ifelse(month < 10, paste0(0, month), month),
    date = as.Date(paste(year, mes, dia, sep = "-")),
    #date = format(date, "%d/%m/%Y"),
    mes = lubridate::month(date, label = T),
    semana_mes = ceiling(mday(date)/7),
    dia_semana = wday(date, label = T),
    semana_mes2 = nth_week(dates = date, count_weeks_in = 'month', begin_week_on = 'Sunday'),
    semana_ano = nth_week(dates = date, count_weeks_in = 'year', begin_week_on = 'Sunday'),
    tcp_percent = tcp / 100
    ) %>% 
  select(-c(country_iso_code, country_idb_code))

setDT(daily_br)
daily_br <- daily_br[
  ,
  c('primeiro_dia_sem', 'ultimo_dia_sem', 'semana_label') := list(
    format(min(date), "%d/%m"),
    format(max(date), "%d/%m"),
    paste(format(min(date), "%d/%m"), format(max(date), "%d/%m"), sep = "-")
  ),
  by = .(semana_ano)
][
  ,
  c('primeiro_dia_mes', 'ultimo_dia_mes') := list(
    min(date),
    max(date)
  ),
  by = .(mes)
]

daily_br[
  ,
  c('total_dias_mes') := list(
    ultimo_dia_mes - primeiro_dia_mes
  ),
  by = .(mes)
  ]


daily_br[
  ,
  c('n_dias_total') := .(
    date - as.Date('2020-03-08')
  ),
  by = .(date)
]

daily_br[
  ,
  c('n_dias_mes') := list(
    date - primeiro_dia_mes + 1
  ),
  by = .(mes)
  ]

daily_br[
  ,
  c('media_cum_tcp') := .(
    cumsum(tcp) / as.numeric(n_dias_total)
  ),
  by = .(region_name)
]

daily_br[
  ,
  c('media_cum_tcp_mes') := .(
    cumsum(tcp) / as.numeric(n_dias_mes)
  ),
  by = .(region_name, mes)
]

  # Base cidades (para grafico com as cidades desejadas) ----
    # Ordernar pela maior variacao total (tcp) ao longo de todo o periodo.
  cities <- daily_br %>% 
    filter(region_type == 'city' #& 
             #!region_slug %in% c('saojosedoscampos', 'sorocaba', 'santos', 'campinas')
           )
  
  # Maiores variações % (total e no ultimo mes) 
  
  levels_cresc <- cities %>% 
    filter(date == max(date)) %>% 
    arrange(media_cum_tcp) %>% 
    select(region_name) %>% 
    as_vector()

  # Ordenar factor region_name base
  cities <- cities %>% 
    mutate(
      region_name = factor(
        region_name,
        levels = levels_cresc,
        ordered = T
      )
    ) 
  
  # Incluir coluna de cores
  cities <- cities %>% 
    mutate(cores = case_when(
      tcp > 0 ~ "#6a9bb3",
      tcp < 0 ~ "#c75450",
      TRUE ~ "#c8c8c8"
    )) 
  

    
  # Vetor com top 9
  #var_total_top9 <- daily_br %>% 
  #  filter(date == max(date) & region_type == 'city' & 
  #           !region_slug %in% c('saojosedoscampos', 'sorocaba', 'santos', 'campinas')) %>% 
  #  arrange(desc(media_cum_tcp)) %>% 
  #  slice_max(order_by = media_cum_tcp, prop = 0.5) %>% 
  #  select(region_name) %>% 
  #  as_vector()
  
  # Vetor com bottom 9
  #var_total_bottom9 <- daily_br %>% 
  #  filter(date == max(date) & region_type == 'city' & 
  #           !region_slug %in% c('saojosedoscampos', 'sorocaba', 'santos', 'campinas')) %>% 
  #  arrange(desc(media_cum_tcp)) %>% 
  #  slice_min(order_by = media_cum_tcp, prop = 0.5) %>% 
  #  select(region_name) %>% 
  #  as_vector()

  
  # Obter media por dia de semana por cidades
  #cities[
  #  ,
  #  lapply(.SD, mean),
  #  by = .(region_name, dia_semana),
  #  .SDcols = c('tcp')
  #] 
  
  # Base estados (para grafico com as cidades desejadas) ----
  # Ordernar pela maior variacao total (tcp) ao longo de todo o periodo.
  estados <- daily_br %>% 
    filter(region_type == 'state') 
  
  # Maiores variações % (total e no ultimo mes) 
  
  levels_cresc_estados <- estados %>% 
    filter(date == max(date)) %>% 
    arrange(media_cum_tcp) %>% 
    select(region_name) %>% 
    as_vector()
  
  # Ordenar factor region_name base
  estados <- estados %>% 
    mutate(
      region_name = factor(
        region_name,
        levels = levels_cresc_estados,
        ordered = T
      )
    ) 



# heatmap -----
bimonthly <- function(x) {
  x_range <- range(x, na.rm = TRUE)
  
  date_range <- c(
    floor_date(x_range[1], "month"),
    ceiling_date(x_range[2], "month")
  )
  monthly <- seq(date_range[1], date_range[2], by = "1 month")
  
  sort(c(monthly, monthly + days(14)))
}

monthly <- function(x) {
  x_range <- range(x, na.rm = TRUE)
  
  date_range <- c(
    floor_date(x_range[1], "month"),
    ceiling_date(x_range[2], "month")
  )
  monthly <- seq(date_range[1], date_range[2], by = "1 month")
  
  sort(c(monthly))
}

# Rescale ver
# https://stackoverflow.com/questions/48424682/how-do-i-limit-the-range-of-the-viridis-colour-scale
# https://stackoverflow.com/questions/14000232/2-color-heatmap-in-r-with-middle-color-anchored-to-a-specific-value/14000512#14000512
# https://stackoverflow.com/questions/40208694/ggplot2-scale-colours-for-heatmap?rq=1
# https://stackoverflow.com/questions/43640843/ggplot2-scale-fill-gradient-with-discrete-cap?rq=1

limits_x <- c(min(cities$date),max(cities$date))

gg_heatmap_cities <- 
  ggplot(
  cities,
  aes(x = date, y = region_name, fill = tcp)
) +
  geom_tile(
    color = "#d9d9d9"
  ) +
  aop_style() +
  scale_x_date(
    sec.axis = dup_axis(),
    position = "top",
    expand = c(0,0),
    breaks = monthly,
    date_labels = "%d/%b" #"%b"#"%d/%b"
  ) +
  scale_y_discrete(
    expand = c(0,0)
  ) +
  scale_fill_viridis_c(
    option = 'A', direction = -1,
    limits = c(-100, 100),
    breaks = c(-100, 0, 100),#seq(-100, 100, by = 100),
    values = scales::rescale(c(-100, -50, 0, 50, 100, 200)),
    oob = scales::squish,
    labels = c(-100, 0, '100 ou mais')
  ) +
  #scale_fill_viridis_c(
  #  option = 'A', direction = -1,
  #  limits = c(-100, max(cities$tcp)),
  #  breaks = seq(-100, 300, by = 100),
  #  values = rescale(c(min(cities$tcp), -50, 0, 50, 100, max(cities$tcp)))
  #) +
  theme(
    axis.line.x = element_blank(),
    legend.position = 'right',
    axis.ticks.x = element_line(size = 0.5, color = "grey"),
    panel.border = element_rect(color = "grey", fill = NA, size = 0.5),
    legend.box.margin = margin(t = 0.5,r = -0.5,b = -0.5,l = -0.25, unit = 'cm'),
    axis.text.y = ggtext::element_markdown(margin = margin(r = 0,l = -0.5, unit = 'cm')),
    #plot.margin = unit(c(t = 0.5,r = 0.25, b = 0.5, l = 0.25), "cm"),
  ) +
  labs(
    #title = 'Evolução do impacto da pandemia no congestionamento nas cidades brasileiras',
    #'Evolução temporal dos impactos do Covid-19 na mobilidade urbana brasiliera'
    subtitle = "Variações percentuais diárias em Intensidade de Congestionamento no Trânsito em cidades brasileiras",
    #caption = "Fonte: Inter-American Development Bank and IDB Invest Coronavirus Impact Dashboard, 2020.<br>Nota: Variações percentuais diárias na Intensidade de Congestionamento no Trânsito, calculadas para capitiais selecionadas com relação à semana de referência de 02 à 08 de Março de 2020.",
    fill = 'Variação (%)<br> em relação<br>ao período base',
    y = '',
    x = ''
  ) +
  guides(fill = guide_colorbar(barwidth = 0.75, barheight = 5)) +
  coord_cartesian(xlim = limits_x, expand = FALSE)

ggsave("figures/waze_IDB/heatmap_cidades.png", 
       width = 16, height = 12, units = "cm", dpi = 300, device = 'png')

ggsave("figures/waze_IDB/heatmap_cidades.pdf", 
       width = 16, height = 12, units = "cm", dpi = 300, device = 'pdf')


# heatmap estados ----

#ggplot(
#  estados,
#  aes(x = date, y = region_name, fill = tcp)
#) +
#  geom_tile(
#    color = "#d9d9d9"
#  ) +
#  aop_style() +
#  scale_x_date(
#    sec.axis = dup_axis(),
#    position = "top",
#    expand = c(0,0),
#    breaks = monthly,
#    date_labels = "%d/%b" #"%b"#"%d/%b"
#  ) +
#  scale_y_discrete(
#    expand = c(0,0)
#  ) +
#  scale_fill_viridis_c(
#    option = 'A', direction = -1,
#    limits = c(-100, max(estados$tcp)),
    #breaks = seq(-100, 300, by = 100),
    #values = rescale(c(min(cities$tcp),  0, 100, max(cities$tcp))),
    #oob = scales::squish
#  ) +
#) +
#  theme(
#  axis.line.x = element_blank(),
#  legend.position = 'right',
#  axis.ticks.x = element_line(size = 0.5, color = "grey"),
#  panel.border = element_rect(color = "grey", fill = NA, size = 0.5),
# legend.box.margin = margin(t = -0.5,r = -0.5,b = -0.5,l = -0.25, unit = 'cm'),
#  axis.text.y = ggtext::element_markdown(margin = margin(r = 0,l = -0.5, unit = 'cm')),
  #plot.margin = unit(c(t = 0.5,r = 0.25, b = 0.5, l = 0.25), "cm"),
#) +
#  labs(
    #title = 'Evolução do impacto da pandemia no congestionamento nas cidades brasileiras',
    #'Evolução temporal dos impactos do Covid-19 na mobilidade urbana brasiliera'
#    subtitle = "Variações percentuais diárias em Intensidade de Congestionamento no Trânsito em cidades brasileiras",
    #caption = "Fonte: Inter-American Development Bank and IDB Invest Coronavirus Impact Dashboard, 2020.<br>Nota: Variações percentuais diárias na Intensidade de Congestionamento no Trânsito, calculadas para capitiais selecionadas com relação à semana de referência de 02 à 08 de Março de 2020.",
#    fill = 'Variação<br>diária (%)',
#    y = '',
#    x = ''
#  ) +
#  guides(fill = guide_colorbar(barwidth = 0.75, barheight = 5))

#ggsave("figures/waze_IDB/heatmap_estados.png", 
#       width = 16, height = 12, units = "cm", dpi = 300, device = 'png')

#ggsave("figures/waze_IDB/heatmap_estados.pdf", 
#       width = 16, height = 12, units = "cm", dpi = 300, device = 'pdf')


# Boxplot -----

avg_df <- cities[, lapply(.SD, mean), .SDcols = 'tcp', by = date]
avg_df[, frollmean7 := data.table::frollmean(tcp,n = 7)]
avg_df$coluna <- 'valor2'

cities$coluna <- 'valor1'

anotacoes <- data.frame(
  anotacao = c('primeira', 'segunda'),
  date = c(as.Date('2020-03-18'), as.Date('2020-08-01')),
  tcp = c(-5, 220),
  legenda = c(
    "<b style='color:#872e2b'>Média móvel<br>(7 dias)</b>",
    "<b style='color:#323232'>*Outliers*</b>"
  )
)

gg_boxplot <- 
  ggplot() +
  geom_boxplot(
    data = cities, 
    aes(x = date, y = tcp, group = date, colour = coluna),
    outlier.size = 0.5
    #fill = '#323232'
  ) +
  geom_line(
    data = avg_df,
    aes(x = date, y = frollmean7, colour = coluna),
    size = 1
    #colour = "#872e2b"
  ) +
  ggtext::geom_richtext(
    data = anotacoes,
    aes(x = date, y = tcp, label = legenda),
    colour = '#323232',
    #fontface = 'bold',
    family = "Helvetica",
    size = 2.81,
    fill = NA, label.color = NA, # remove background and outline
    #  label.padding = grid::unit(rep(0, 4), "pt") # remove padding
    hjust = 0
  ) +
  #geom_segment(
  #  aes(
  #    x = as.Date('2020-03-18'), xend = as.Date('2020-04-03'),
  #    y = -13, yend = 75
  #    ),
  #  linetype = 'dotted', colour = '#808080'
  #) +
  #geom_segment(
  #  aes(
  #    x = as.Date('2020-04-03'), xend = as.Date('2020-04-17'),
  #    y = 75, yend = 75
  #  ),
  #  linetype = 'solid', colour = '#808080'
  #) +
  scale_x_date(
    expand = expansion(mult = c(0,0.02)),
    #expand = c(0,0.1),
    breaks = monthly,
    date_labels = "%d/%b"
  ) +
  scale_y_continuous(
    breaks = seq(-100, 100, by = 50),
    limits = c(-100, 100)
  ) +
  scale_colour_aop(
    values = c('valor1' = '#323232', 'valor2' = "#872e2b")
  ) +
  #scale_colour_aop(
  #  values = 
  #)
  aop_style() +
  labs(
    #title = 'Evolução do impacto da pandemia no congestionamento nas cidades brasileiras',
    #'Evolução temporal dos impactos do Covid-19 na mobilidade urbana brasiliera'
    subtitle = "Distribuição das variações percentuais diárias em Intensidade de Congestionamento no Trânsito em<br>cidades brasileiras",
    #caption = "Fonte: Inter-American Development Bank and IDB Invest Coronavirus Impact Dashboard, 2020.<br>Nota: Variações percentuais diárias na Intensidade de Congestionamento no Trânsito, calculadas para capitiais selecionadas com relação à semana de referência de 02 à 08 de Março de 2020.",
    y = 'Variação diária (%)',
    x = 'Dia/Mês'
    #colour = 'Média móvel:<br>7 dias'
  ) +
  theme(
    #legend.position = 'bottom'
    axis.text.y = ggtext::element_markdown(
      margin = margin(r = 0.25,l = 0, unit = 'cm'),
      #vjust = 0
      ),
    #panel.grid.major.x = element_line(size = 0.15, color = "grey"),
    axis.ticks.x = element_line(colour = 'grey', size = 0.5, linetype = 1),
    #axis.ticks.y = element_line(colour = 'grey', size = 0.5, linetype = 1),
    #axis.ticks.length.y = unit(0.5, 'cm'),
    axis.text.x = ggtext::element_markdown(hjust = 0),
    plot.margin = unit(c(0.25,0.5,0.25,0.5), "cm")
  ) +
  coord_cartesian(clip = "off", xlim = limits_x#, expand = F)
  )

ggsave("figures/waze_IDB/boxplot.png", 
       width = 16, height = 12, units = "cm", dpi = 300, device = 'png')

ggsave("figures/waze_IDB/boxplot.pdf", 
       width = 16, height = 12, units = "cm", dpi = 300, device = 'pdf')



# patchwork ----
#gg_heatmap_cities / gg_boxplot

wrap_elements(full = gg_heatmap_cities) / wrap_elements(full = gg_boxplot) + 
  plot_layout(ncol = 1) &
  theme(plot.margin = unit(c(0,0.25,0,0.25), "cm"))

# y title separado (do boxplot)
gg_heatmap_cities / gg_boxplot &
  theme(plot.margin = unit(c(0.25,0.25,0.25,0.25), "cm"))

ggsave("figures/waze_IDB/heatmap_boxplot.png", 
       width = 16, height = 18, units = "cm", dpi = 300, device = 'png')

ggsave("figures/waze_IDB/heatmap_boxplot.pdf", 
       width = 16, height = 18, units = "cm", dpi = 300, device = 'pdf')

