# Setup ----
rm(list=ls())
gc(reset = T)

source('R/setup.R')
source('R/colours.R')
source('R/style.R')
source("R/DENATRAN/colors_plot.R")
source("R/DENATRAN/aop_style1.R")


# source("R/PNS/0_loadpackages.R",local = TRUE)
`%nin%` = Negate(`%in%`)

# generate cities geometries from geobr
# cities <- geobr::read_municipality()
# readr::write_rds(cities,"figures/GOOGLE/read_municipality_data.rds")

# dir.create("figures/GOOGLE/")
# dir.create("data-raw/GOOGLE")

#
# name columns function-----
#
toupper_noaccent <- function(i){
  stringi::stri_trans_general(i,id = "Latin-ASCII") %>% 
    toupper() %>% stringr::str_replace_all("-"," ")  %>% 
    stringr::str_remove_all("'")
}

ls_initial_list <- c("google","activities","%nin%","toupper_noaccent")
# first manipulation

statebr <- geobr::read_state(code_state = "all") %>% data.table::setDT()
statebr[,name_state := toupper_noaccent(name_state)]

# static google url

link <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
google_raw <- data.table::fread(input = link, encoding = "UTF-8")
#google <- data.table::fread("../../data-raw/GOOGLE/Global_Mobility_Report.csv", encoding = "UTF-8")


# data analysis

google <- data.table::copy(google_raw)[country_region %in% "Brazil",]
google <- google[date < as.Date("2020-12-13"),]

google[,sub_region_1_fix := toupper_noaccent(sub_region_1) %>% 
         stringr::str_remove_all("STATE OF ")]
google[sub_region_1_fix %in% "FEDERAL DISTRICT",sub_region_1_fix := "DISTRITO FEDERAL"]
google[,date_fix := as.POSIXct(date,tz = "America/Bahia")]
google[,year_month := format(date_fix,"%m/%Y")]
google[,day_month := format(date_fix,"%d/%m")]
google[,day_month_id := .GRP,by = date_fix]
google[,sub_region_2_fix := toupper_noaccent(sub_region_2)]
google[statebr,on = c('sub_region_1_fix' = 'name_state'), state_abrev := i.abbrev_state]
google[,name_muni := paste0(sub_region_2_fix,"-",state_abrev)]

#
# cities names-------------
#
my_cities <- c("Belém","Belo Horizonte","Campinas",
               "Curitiba","Fortaleza","Goiânia",
               "João Pessoa","Maceió","Manaus",
               "Natal","Porto Alegre","Recife",
               "Rio de Janeiro","Salvador","Santos",
               "São José dos Campos","São Luís","São Paulo",
               "Sorocaba", "Teresina","Vitória")
google[sub_region_2 %in% "Sao Jose dos Campos",sub_region_2 := "São José dos Campos"]
google <- google[sub_region_2 %in% my_cities,]


activities <- c("retail_and_recreation",
                #"grocery_and_pharmacy",
                #"parks",
                "transit_stations",
                "workplaces",
                "residential")
local_categories <- c('Varejo e lazer',
                      #'Mercados e farmácias',
                      #'Parques',
                      'Estações de transporte público',
                      'Locais de trabalho','Residencial')
description <- c('Tendências de mobilidade de lugares como restaurantes, cafés, \n shopping centers, parques temáticos, museus, bibliotecas e cinemas.',
                 #'Tendências de mobilidade de lugares como mercados, armazéns de \n alimentos, feiras, lojas especializadas em alimentos, drogarias e farmácias.',
                 #'Tendências de mobilidade de lugares como parques locais e nacionais, \n praias públicas, marinas, parques para cães, praças e jardins públicos.',
                 'Tendências de mobilidade de lugares como terminais de transporte público,\n tipo estações de metrô, ônibus e trem',
                 'Tendências de mobilidade de locais de trabalho',
                 'Tendências de mobilidade de áreas residenciais')
#
# check names-------------
#
google$sub_region_1 %>% unique()
google$sub_region_2 %>% unique()
google$sub_region_2 %>% uniqueN()
google$metro_area %>% unique()
google$iso_3166_2_code %>% unique()
google$census_fips_code %>% unique()
google$date %>% unique()
google$state_abrev %>% unique()


# 
# general graph-------------
#

google1 <- data.table::melt(data = google,
                            id.vars = c('date_fix','day_month_id','state_abrev','sub_region_2'),
                            measure.vars =  list('change' = c('retail_and_recreation_percent_change_from_baseline',
                                                              'grocery_and_pharmacy_percent_change_from_baseline',
                                                              'parks_percent_change_from_baseline',
                                                              'transit_stations_percent_change_from_baseline',
                                                              'workplaces_percent_change_from_baseline',
                                                              'residential_percent_change_from_baseline')))
#label_x <- c("15/02","01/03","15/03","01/04","15/04",
#             "01/05","15/05","01/06","15/06","01/07","15/07",
#             "01/08","15/08","01/09","01/10","01/11","01/12")
label_x <- c(paste0("01/0",3:9),paste0("01/",10:12))
label_plot <- c(#"15/fev",
                paste0("01/",c('Mar','Abr','Mai','Jun','Jul',
                               'Ago','Set','Out','Nov','Dez')))
break_x <- google[day_month %in% label_x,unique(day_month_id)]
limits_x <- c(min(google1$day_month_id),max(google1$day_month_id))

break()
# exclude groceries and parks
my_pallete <- c("E","D","B","A")
for(i in 1:length(activities)){ # i = 1
  
  message(activities[i])
  # i = 2
  google2 <- data.table::copy(google1)[variable %like% activities[i],]
  # orderuf
  orderuf <- google2[data.table::between(date_fix,"2020-07-01","2020-12-12"),
                     lapply(.SD,sum),
                     .SDcols = 'value',by = sub_region_2][order(value),sub_region_2]
  orderuf <- as.character(orderuf)
  data.table::setkey(google2,sub_region_2)
  google2 <- google2[data.table::data.table(orderuf)]
  google2[,grp := .GRP, by = sub_region_2]
  labels_y <- google2[,.SD[1], by = sub_region_2][, sub_region_2]
  breaks_y <- google2[,.SD[1], by = sub_region_2][, grp]
  range_fill <- seq(min(google2$value, na.rm=T),max(google2$value, na.rm=T),length.out = 6)
  
  
  plot1 <- ggplot(data = google2, aes(x = day_month_id,y = grp)) + 
    geom_tile(aes(fill = value),colour = "white") +
    viridis::scale_fill_viridis("Mudança\nrelativa (%)",
                                option = my_pallete[i],
                                limits = c(min(range_fill),max(range_fill)),
                                direction = -1,
                                breaks = round(range_fill,0)) +
    scale_x_continuous(breaks = break_x,
                       labels = label_plot,
                       sec.axis = sec_axis(~ .,
                                           breaks = break_x,
                                           labels = label_plot))+ 
    scale_y_continuous(NULL,breaks = breaks_y,
                       labels = labels_y)+#,
                       #sec.axis = sec_axis(~ .,
                      #                     breaks = breaks_y,
                      #                     labels = labels_y)) + 
    labs( title = local_categories[i],
          # subtitle = description[i],
          x = NULL, y = "Estados",
          fill = "Mudança\nrelativa (%)") +
    aop_style1() +
    theme(legend.position = 'right',
          axis.ticks = element_line(
            colour = "black",
            size = 0.5,
            linetype = 1),
          axis.text.x = element_text(angle = 0, hjust = 0,size=8),
          axis.text.y = element_text(angle = 0, hjust = 1,size=8),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) + 
    coord_cartesian(xlim = limits_x, expand = FALSE)
  
  
  
  
  #
  # errorbar ------------------
  #
  google2[, value := as.numeric(value)]
  google3 <- google2[, .(median = median(value, na.rm=T),
                         # q25 = quantile(x=value,probs = .25, na.rm=T),
                         # q75 = quantile(x=value,probs = .75, na.rm=T)),
                         lo = mean(value, na.rm=T) - 2*sd(value, na.rm=T),
                         hi = mean(value, na.rm=T) + 2*sd(value, na.rm=T)),
                     by= .(date_fix, day_month_id)]
  
  avg_df <- google2[,lapply(.SD,mean), .SDcols = 'value', by = .(date_fix, day_month_id)]
  avg_df <- avg_df[order(day_month_id)]
  avg_df[,frollmean7 := data.table::frollmean(value,n = 7)]
  subset(avg_df, frollmean7==min(frollmean7, na.rm=T))
  
  plot2 <-
    ggplot(data=google3,  aes(x = day_month_id )) +
    geom_errorbar( aes(ymax = lo, ymin = hi),
                   size = .5, width = 0, color='gray40') +
    geom_point( aes(y=median), size = 1.5) +
    geom_line(data = avg_df,aes(x = day_month_id, y = frollmean7),color = 'red') +
    geom_hline(yintercept = 0, color= "black", linetype='dotted') +
    labs(x = NULL, 
         y = "Mudança relativa"
         #, caption = 'Fonte: Google COVID-19 Community Mobility Reports'
    ) + 
    scale_x_continuous(breaks = break_x,
                       labels = label_plot) + 
    scale_y_continuous(breaks = range_fill,
                       labels = round(range_fill,0)) + 
    aop_style1() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0,size=8),
          axis.text.y = element_text(angle = 0, hjust = 1,size=8),
          axis.line.x = element_line(size = 0.5, color = "grey"),
          axis.ticks = element_line(
            colour = "black",
            size = 0.5,
            linetype = 1),
          panel.grid.major.x = element_line(size = 0.15, color = "grey"),
          panel.grid.minor = element_blank())+
    coord_cartesian(xlim = limits_x, expand = FALSE)
  
  plot2
  
  
  pf <- plot1 / plot2 +  plot_layout(heights = c(3, 2.0)) + plot_annotation(tag_levels = 'A')
  
  ggsave(filename = paste0("figures/GOOGLE/",activities[i],".png"),
         width = 23.7, height = 17.6,dpi = 300, units = "cm")
  ggsave(filename = paste0("figures/GOOGLE/",activities[i],".pdf"),
         width = 23.7, height = 17.6,dpi = 300, units = "cm")
  
}

# clean up unusefull files

lista <- ls()[ls() %nin% c(ls_initial_list,"ls_initial_list")]
rm(list = lista)

# 
# # Errorbar separate -----
# 
# google2 <- data.table::copy(google1)[ variable %like% c("retail_and_recreation|transit_stations|workplaces") &
#                                         sub_region_2 %in% "" & 
#                                         state_abrev %nin% "" & 
#                                         !is.na(state_abrev),]
# 
# 
# google2[, variable := fcase( variable %like% "retail_and_recreation", "Varejo e lazer",
#                              variable %like% "workplaces", "Locais de trabalho",
#                              variable %like% "transit_stations", "Estações de TP")]
# 
# 
# avg_df <- google2[,lapply(.SD,mean), .SDcols = 'value', by = .(state_abrev, variable, date_fix, day_month_id)]
# avg_df <- avg_df[order(state_abrev, variable, day_month_id)]
# avg_df[,frollmean7 := data.table::frollmean(value,n = 7), by = .(state_abrev, variable)]
# subset(avg_df, frollmean7==min(frollmean7, na.rm=T))
# 
# 
# 
# plot2 <-
#   ggplot( ) +
#   # geom_point( aes(y=median), size = 1, color='gray60', aplha=.3) +
#   geom_line(data = avg_df,aes(x = day_month_id, y = frollmean7, color = variable)) +
#   geom_hline(yintercept = 0, color= "black", linetype='dotted') +
#   facet_wrap(.~state_abrev, ncol = 4) +
#   labs(x = NULL, 
#        y = "Mudança relativa"
#        #, caption = 'Fonte: Google COVID-19 Community Mobility Reports'
#   ) + 
#   scale_x_continuous(breaks = break_x,
#                      labels = label_plot) + 
#   scale_y_continuous(breaks = range_fill,
#                      labels = round(range_fill,0)) + 
#   #theme_bw() +
#   aop_style1() +
#   theme(axis.text.x = element_text(angle = 0, hjust = 0,size=8),
#         axis.text.y = element_text(angle = 0, hjust = 1,size=8),
#         axis.line.x = element_line(size = 0.5, color = "grey"),
#         axis.ticks = element_line(
#           colour = "black",
#           size = 0.5,
#           linetype = 1),
#         panel.grid.major.x = element_line(size = 0.15, color = "grey"),
#         panel.grid.minor = element_blank()) +
#   coord_cartesian(xlim = limits_x, expand = FALSE) +
#   theme(legend.position="top")
# 
# plot2
# 
# ggsave(plot2,
#        filename = "figures/GOOGLE/line_separate.png", 
#        width = 16, height = 20, units = "cm", dpi = 300, device = 'png')
# 
# 
# #
# # cities analysis-----
# #
# cities <- readr::read_rds("figures/GOOGLE/read_municipality_data.rds") %>% data.table::setDT()
# cities[,name_muni := paste0(toupper_noaccent(name_muni),"-",abbrev_state)]
# 
# google1 <- data.table::copy(google)[name_muni %in% cities$name_muni,]
# google1[cities,on = 'name_muni',geometry := i.geom]
# 
# google2 <- data.table::copy(google1)[name_muni %in% "VERA CRUZ-SP",] 
# google2
# google2 <- sf::st_as_sf(google2)
# plot(google2['residential_percent_change_from_baseline'])
# uniqueN(google1$sub_region_2_fix)
# head(google1,4)
# google1 <- data.table::melt(data = google,
#                             id.vars = c('date_fix','day_month_id','state_abrev','sub_region_2'),
#                             measure.vars =  list('change' = c('retail_and_recreation_percent_change_from_baseline',
#                                                               'grocery_and_pharmacy_percent_change_from_baseline',
#                                                               'parks_percent_change_from_baseline',
#                                                               'transit_stations_percent_change_from_baseline',
#                                                               'workplaces_percent_change_from_baseline',
#                                                               'residential_percent_change_from_baseline')))
# 
# 
# 
# 
