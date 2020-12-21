
# 1 Load Setup ------------------------------------------------------------

#rm(list=ls())
#gc(reset = T)

source('R/setup.R')
source('R/colours.R')
source('R/style.R')

# check current locale
Sys.getlocale(category = "LC_TIME")
# change current locale
Sys.setlocale("LC_ALL","pt_BR.UTF-8")


# * 1.1 Functions ---------------------------------------------------------

# case_when for factors
fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}

# function to get number of the week (based on the year or month)
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


bimonthly <- function(x) {
  x_range <- range(x, na.rm = TRUE)
  
  date_range <- c(
    floor_date(x_range[1], "month"),
    ceiling_date(x_range[2], "month")
  )
  monthly <- seq(date_range[1], date_range[2], by = "1 month")
  
  sort(c(monthly, monthly + days(14)))
}

# Functions to determine data breaks (montlhy and bimonthly)
monthly <- function(x) {
  x_range <- range(x, na.rm = TRUE)
  
  date_range <- c(
    floor_date(x_range[1], "month"),
    ceiling_date(x_range[2], "month")
  )
  monthly <- seq(date_range[1], date_range[2], by = "1 month")
  
  sort(c(monthly))
}


# 2 Data ------------------------------------------------------------------

# * 2.1 Download ----------------------------------------------------------

# download
cities <- fread(
  "http://tiny.cc/idb-traffic-daily",
  colClasses = c(
    rep('character', 7), 'integer', 'character', rep('integer', 5), rep('double', 2)
  ),
  encoding = "UTF-8"
)

# * 2.2 Cleanse -----------------------------------------------------------

# * * 2.2.1 Filter --------------------------------------------------------

# filter cities from BR
cities <- cities[country_name == 'Brazil' & region_type == 'city'][
  order(month, day)
]

# * * 2.2.2 Add columns ---------------------------------------------------

# add columns
cities <- cities %>% 
  mutate(
    year = 2020,
    dia = ifelse(day < 10, paste0(0, day), day),
    mes = ifelse(month < 10, paste0(0, month), month),
    date = as.Date(paste(year, mes, dia, sep = "-")),
    #date = format(date, "%d/%m/%Y"),
    mes = lubridate::month(date, label = T),
    #semana_mes = ceiling(mday(date)/7),
    dia_semana = wday(date, label = T),
    #semana_mes2 = nth_week(dates = date, count_weeks_in = 'month', begin_week_on = 'Sunday'),
    semana_ano = nth_week(dates = date, count_weeks_in = 'year', begin_week_on = 'Sunday'),
    tcp_percent = tcp / 100
  ) %>% 
  # exclude unnecessary columns
  select(-c(country_iso_code, country_idb_code))

# change class to DT
setDT(cities)

# add columns by desired variable
cities <- cities[
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

cities[
  ,
  c('total_dias_mes') := list(
    ultimo_dia_mes - primeiro_dia_mes
  ),
  by = .(mes)
]

cities[
  ,
  c('n_dias_total') := .(
    date - as.Date('2020-03-08')
  ),
  by = .(date)
]

cities[
  ,
  c('n_dias_mes') := list(
    date - primeiro_dia_mes + 1
  ),
  by = .(mes)
]

cities[
  ,
  c('media_cum_tcp') := .(
    cumsum(tcp) / as.numeric(n_dias_total)
  ),
  by = .(region_name)
]

cities[
  ,
  c('media_cum_tcp_mes') := .(
    cumsum(tcp) / as.numeric(n_dias_mes)
  ),
  by = .(region_name, mes)
]

# * * 2.2.3 Reorder cities names ------------------------------------------

# Order dt by greatest total variation (tcp) over the whole period 

# vector arrange by greatest varation
levels_cresc <- cities %>% 
  filter(date == max(date)) %>% 
  arrange(media_cum_tcp) %>% 
  select(region_name) %>% 
  as_vector()

# order regions by vector
cities <- cities %>% 
  mutate(
    region_name = factor(
      region_name,
      levels = levels_cresc,
      ordered = T
    )
  ) 

# * 2.3 Plot --------------------------------------------------------------

# * * 2.3.1 Heatmap -------------------------------------------------------

# set limits for x axis
limits_x <- c(min(cities$date), max(cities$date))

# plot heatmap
gg_heatmap_cities <- 
  ggplot(
    data=cities,
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
    # subtitle = "Variações percentuais diárias em Intensidade de Congestionamento no Trânsito em cidades brasileiras",
    #caption = "Fonte: Inter-American Development Bank and IDB Invest Coronavirus Impact Dashboard, 2020.<br>Nota: Variações percentuais diárias na Intensidade de Congestionamento no Trânsito, calculadas para capitiais selecionadas com relação à semana de referência de 02 à 08 de Março de 2020.",
    fill = 'Variação<br>relativa (%)<br>',
    y = '',
    x = ''
  ) +
  guides(fill = guide_colorbar(barwidth = 0.75, barheight = 5)) +
  coord_cartesian(xlim = limits_x, expand = FALSE)

ggsave("figures/waze_IDB/heatmap_cidades.png", 
       width = 16, height = 12, units = "cm", dpi = 300, device = 'png')

ggsave("figures/waze_IDB/heatmap_cidades.pdf", 
       width = 16, height = 12, units = "cm", dpi = 300, device = 'pdf')


# * * 2.3.2 Boxplot -------------------------------------------------------

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
    aes(x = date, y = frollmean7, colour = coluna)
    # , size = 1
    #colour = "#872e2b"
  ) +
  geom_hline(yintercept = 0, color= "black", linetype='dotted') +
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
  aop_style() +
  labs(
    #title = 'Evolução do impacto da pandemia no congestionamento nas cidades brasileiras',
    #'Evolução temporal dos impactos do Covid-19 na mobilidade urbana brasiliera'
    # subtitle = "Distribuição das variações percentuais diárias em Intensidade de Congestionamento no Trânsito em<br>cidades brasileiras",
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

ggsave(gg_boxplot,
       filename = "figures/waze_IDB/boxplot.png", 
       width = 16, height = 12, units = "cm", dpi = 300, device = 'png')

ggsave("figures/waze_IDB/boxplot.pdf", 
       width = 16, height = 12, units = "cm", dpi = 300, device = 'pdf')


# * * 2.3.3 Errorbar ------------------------------------------------------

cities[, tcp := as.numeric(tcp)]
cities2 <- cities[, .(median = median(tcp, na.rm=T),
                      # q25 = quantile(x=value,probs = .25, na.rm=T),
                      # q75 = quantile(x=value,probs = .75, na.rm=T)),
                      lo = mean(tcp, na.rm=T) - 2*sd(tcp, na.rm=T),
                      hi = mean(tcp, na.rm=T) + 2*sd(tcp, na.rm=T)),
                  by= .(date)]

avg_df <- cities[, lapply(.SD, mean), .SDcols = 'tcp', by = date]
avg_df[, frollmean7 := data.table::frollmean(tcp,n = 7)]
avg_df$coluna <- 'valor2'

cities$coluna <- 'valor1'

anotacoes <- data.frame(
  anotacao = c('primeira', 'segunda'),
  date = c(as.Date('2020-03-18'), as.Date('2020-08-01')),
  tcp = c(0, 220),
  legenda = c(
    "<b style='color:#872e2b'>Média móvel<br>(7 dias)</b>",
    "<b style='color:#323232'>*Outliers*</b>"
  )
)

gg_errorbar <- 
  ggplot(data=cities2,  aes(x = date )) +
  geom_errorbar( aes(ymax = lo, ymin = hi),
                 size = .5, width = 0, color='gray40') +
  geom_point( aes(y=median), size = 1.5) +
  
  geom_line(
    data = avg_df,
    aes(x = date, y = frollmean7), color='red'
    # , size = 1
    #colour = "#872e2b"
  ) +
  geom_hline(yintercept = 0, color= "black", linetype='dotted') +
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
    # subtitle = "Distribuição das variações percentuais diárias em Intensidade de Congestionamento no Trânsito em<br>cidades brasileiras",
    #caption = "Fonte: Inter-American Development Bank and IDB Invest Coronavirus Impact Dashboard, 2020.<br>Nota: Variações percentuais diárias na Intensidade de Congestionamento no Trânsito, calculadas para capitiais selecionadas com relação à semana de referência de 02 à 08 de Março de 2020.",
    y = 'Variação diária (%)',
    x = 'Dia/Mês'
    #colour = 'Média móvel:<br>7 dias'
  ) +
  theme(
    #legend.position = 'bottom'
    axis.text.y = ggtext::element_markdown(
      margin = margin(r = 0.25,l = -0.25, unit = 'cm'),
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

ggsave(gg_errorbar,
       filename = "figures/waze_IDB/errorbar.png", 
       width = 16, height = 12, units = "cm", dpi = 300, device = 'png')

ggsave("figures/waze_IDB/boxplot.pdf", 
       width = 16, height = 12, units = "cm", dpi = 300, device = 'pdf')


# * * 2.3.4 Patchwork -----------------------------------------------------

# Errorbar
pf <- gg_heatmap_cities / gg_errorbar + plot_layout(heights = c(3, 2.0)) + plot_annotation(tag_levels = 'A')
theme_set(theme_cowplot() + theme(text = element_text(colour = "#575757")))
pf <- plot_grid(
  gg_heatmap_cities, gg_errorbar, labels = c('A', 'B'), ncol = 1, align = "v", axis = 'lr',
  hjust = -0.5
  )
ggsave(pf,
       filename = "figures/waze_IDB/heatmap_errobar2.png", 
       width = 16, height = 16, units = "cm", dpi = 300, device = 'png')

# Boxplot
wrap_elements(full = gg_heatmap_cities) / wrap_elements(full = gg_boxplot) + 
  plot_layout(ncol = 1) &
  theme(plot.margin = unit(c(0,0.25,0,0.25), "cm"))

# y title separado (do boxplot)
gg_heatmap_cities / gg_boxplot &
  theme(plot.margin = unit(c(0.25,0.25,0.25,0.25), "cm"))


ggsave(filename = "figures/waze_IDB/heatmap_boxplot.png", 
       width = 16, height = 18, units = "cm", dpi = 300, device = 'png')

ggsave("figures/waze_IDB/heatmap_boxplot.pdf", 
       width = 16, height = 18, units = "cm", dpi = 300, device = 'pdf')




# * * 2.3.5 Errorbar Separate ---------------------------------------------

cities[, tcp := as.numeric(tcp)]
cities2 <- cities[, .(median = median(tcp, na.rm=T),
                      # q25 = quantile(x=value,probs = .25, na.rm=T),
                      # q75 = quantile(x=value,probs = .75, na.rm=T)),
                      lo = mean(tcp, na.rm=T) - 2*sd(tcp, na.rm=T),
                      hi = mean(tcp, na.rm=T) + 2*sd(tcp, na.rm=T)),
                  by= .(region_name, date)]

avg_df <- cities[, lapply(.SD, mean), .SDcols = 'tcp', by = .(region_name, date)]
avg_df <- avg_df[order(region_name, date)]
avg_df[, frollmean7 := data.table::frollmean(tcp,n = 7), by = .(region_name)]
summary(avg_df$frollmean7)
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

gg_errorbar2 <- 
  ggplot(data=cities2,  aes(x = date )) +
  # geom_errorbar( aes(ymax = lo, ymin = hi),
  #                size = .5, width = 0, color='gray40') +
  geom_point( aes(y=median), size = 1, color='gray50', alpha=.4) +
  geom_line(
    data = avg_df,
    aes(x = date, y = frollmean7, color=region_name)
    # , size = 1
    #colour = "#872e2b"
  ) +
  geom_hline(yintercept = 0, color= "black", linetype='dotted') +
  facet_wrap(.~region_name, ncol = 4) +
  # ggtext::geom_richtext(
  #   data = anotacoes,
  #   aes(x = date, y = tcp, label = legenda),
  #   colour = '#323232',
  #   #fontface = 'bold',
  #   family = "Helvetica",
  #   size = 2.81,
  #   fill = NA, label.color = NA, # remove background and outline
  #   #  label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  #   hjust = 0
  # ) +
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
  # scale_colour_aop(
  #   values = c('valor1' = '#323232', 'valor2' = "#872e2b")
  # ) +
  #scale_colour_aop(
  #  values = 
  #)
  aop_style() +
  labs(
    #title = 'Evolução do impacto da pandemia no congestionamento nas cidades brasileiras',
    #'Evolução temporal dos impactos do Covid-19 na mobilidade urbana brasiliera'
    # subtitle = "Distribuição das variações percentuais diárias em Intensidade de Congestionamento no Trânsito em<br>cidades brasileiras",
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

ggsave(gg_errorbar2,
       filename = "figures/waze_IDB/errorbar_separate.png", 
       width = 16, height = 20, units = "cm", dpi = 300, device = 'png')

