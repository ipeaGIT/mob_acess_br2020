# Setup ----
rm(list=ls())
gc(reset = T)

source('R/setup.R')
source('R/colours.R')
source('R/style.R')
source("R/DENATRAN/colors_plot.R")
source("R/DENATRAN/aop_style1.R")

`%nin%` = Negate(`%in%`)

# name columns function-----
#
toupper_noaccent <- function(i){
  stringi::stri_trans_general(i,id = "Latin-ASCII") %>% 
    toupper() %>% stringr::str_replace_all("-"," ")  %>% 
    stringr::str_remove_all("'")
}

ls_initial_list <- c("google","activities","%nin%","toupper_noaccent")

# static google url

link <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
google <- data.table::fread(input = link, encoding = "UTF-8")
#google <- data.table::fread("../../data-raw/GOOGLE/Global_Mobility_Report.csv", encoding = "UTF-8")
google <- google[country_region %in% "Brazil",]
google <- google[date < as.Date("2020-10-01"),]

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

activities <- c("retail_and_recreation",
                "transit_stations",
                "workplaces",
                "residential")
local_categories <- c('Varejo e lazer',
                      'Estações de transporte público',
                      'Locais de trabalho','Residencial')
description <- c('Tendências de mobilidade de lugares como restaurantes, cafés, \n shopping centers, parques temáticos, museus, bibliotecas e cinemas.',
                 'Tendências de mobilidade de lugares como terminais de transporte público,\n tipo estações de metrô, ônibus e trem',
                 'Tendências de mobilidade de locais de trabalho',
                 'Tendências de mobilidade de áreas residenciais')
#
# cities names-------------
#
my_cities <- c("Teresina","Vitória","Brasília","Salvador","Rio de Janeiro",
               "Fortaleza","João Pessoa","São Luís","Goiânia","Campinas",
               "Natal","Belo Horizonte","São José dos Campos","São Paulo",
               "Porto Alegre","Maceió","Curitiba","Belém","Manaus","Sorocaba",
               "Recife","Santos")
google[sub_region_2 %in% "Sao Jose dos Campos",sub_region_2 := "São José dos Campos"]
google <- google[sub_region_2 %in% my_cities,]

# 
# general graph-------------
#

google1 <- data.table::melt(data = google,
                            id.vars = c('date_fix','day_month_id','sub_region_2'),
                            measure.vars =  list('change' = c('retail_and_recreation_percent_change_from_baseline',
                                                              'grocery_and_pharmacy_percent_change_from_baseline',
                                                              'parks_percent_change_from_baseline',
                                                              'transit_stations_percent_change_from_baseline',
                                                              'workplaces_percent_change_from_baseline',
                                                              'residential_percent_change_from_baseline')))

label_x <- c(paste0("01/0",3:9),"30/09")
label_plot <- paste0("01/",c('Mar','Abr','Mai','Jun','Jul','Ago','Set','Out'))
break_x <- google[day_month %in% label_x,unique(day_month_id)]
limits_x <- c(min(google1$day_month_id),max(google1$day_month_id))
my_pallete <- c("E","D","B","A")
break()
#
# facet_grid -----
#

grid_categories <- c("Estações de transporte público",
                     "Varejo e lazer",
                     "Locais de trabalho")
google2 <- data.table::copy(google1)
google2 <- google2[variable %like% activities[3] |
                     variable %like% activities[2] |
                     variable %like% activities[1],]
orderuf <- unique(google2$variable)
google2[,variable := factor(variable,orderuf)]
google2[,grp := .GRP, by = sub_region_2]
google2[,frollmean := data.table::frollmean(value,n = 7), 
        by = .(variable,sub_region_2)]

ggplot(data = google2, 
       aes(x = day_month_id,y = frollmean)) + 
  geom_line(aes(color = variable)) + 
  facet_wrap(facets = vars(sub_region_2),ncol = 3) +
  geom_hline(yintercept = 0, color= "black", linetype='dotted') +
  scale_x_continuous(breaks = break_x,
                     labels = label_x) +
  scale_color_manual(labels = grid_categories,
                       values = c(as.vector(aop_colors$wrapper[c(2)]),
                         as.vector(aop_colors$qualitativo[3]),
                         as.vector(aop_colors$cartola[6]))) +
labs(x = NULL, y = "Mudança\nrelativa(%)",
     color = "Categorias") +
  aop_style() +
  theme(legend.position = 'top',
        axis.ticks = element_line(
          colour = "black",
          size = 0.5,
          linetype = 1),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1,size=8),
        axis.text.y = element_text(angle = 0, hjust = 1,size=8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

ggsave(filename = paste0("figures/GOOGLE/cities_grid.png"),
       width = 23.7, height = 30.7,dpi = 300,scale = 0.65, units = "cm")

