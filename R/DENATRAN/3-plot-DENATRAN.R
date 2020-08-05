#
# initial config----------------
#

rm(list=ls())
gc(reset = T)
library(XLConnect)
library(gganimate)  # install.packages("gganimate")
library(transformr) # install.packages("transformr")
library(gifski)
source("R/PNS/0_loadpackages.R",local = TRUE)
`%nin%` = Negate(`%in%`)

dir.create("figures/DENATRAN")

denatran <- readr::read_rds("data/DENATRAN/DENATRAN_jan.rds")
denatran[name_metro_pns %in% "RIDE - REGIAO INTEGRADA DE DESENVOLVIMENTO DO DISTRITO FEDERAL E ENTORNO",
         name_metro_pns := "RM BRASILIA"]
ls_initial_list <- "denatran"

#
# check names-------------
#

unique(denatran$UF)
unique(denatran$ANO)
unique(denatran$name_metro)
unique(denatran$name_metro_pns)
unique(denatran$name_region)

#
# taxa / rms / 2001-2020 ------
#

temp_pns <- denatran[!is.na(name_metro_pns),]
temp_nmetro <- denatran[is.na(name_metro_pns),][,name_metro_pns := "BRASIL NÃO-METROPOLITANO"]
temp_metro <- denatran[!is.na(name_metro_pns),][,name_metro_pns := "BRASIL METROPOLITANO"]

temp_den <- list(temp_pns,temp_nmetro,temp_metro) %>% data.table::rbindlist()
temp_den <- temp_den[!is.na(name_metro_pns) & !is.na(TOTAL_AUTOS) 
                     & !is.na(TOTAL_MOTOS) & !is.na(POP),]
temp_den <- temp_den[,list(TOTAL_AUTOS = sum(TOTAL_AUTOS),
                           TOTAL_MOTOS = sum(TOTAL_MOTOS),
                           POP = sum(POP)),
                     by = .(name_metro_pns,ANO)]
temp_den[, AUTOS_PER_POP := TOTAL_AUTOS / POP]
temp_den[, MOTOS_PER_POP := TOTAL_MOTOS / POP]
temp_den[, c('POP','TOTAL_AUTOS','TOTAL_MOTOS') := NULL]


temp_den <- data.table::melt(data = temp_den,
                             id.vars =  c('ANO','name_metro_pns'),
                             variable.name = 'TYPE',
                             value.name = "TOTAL")

temp_den[,name_metro_pns := stringr::str_to_title(name_metro_pns)]
temp_den[,name_metro_pns := gsub("Rm ","",name_metro_pns)]
temp_den[,name_metro_pns := gsub("De","de",name_metro_pns)]

temp_den[,TOTAL_city := sum(TOTAL), by = .(ANO,name_metro_pns)]
ggplot(temp_den,aes(x = ANO,y = TOTAL,fill = TYPE)) +
  geom_area(colour="black", size=.3, alpha=.8) +
  scale_fill_brewer(palette="Blues",labels = c("Automóvel","Motocicleta"))+
  geom_text(data = temp_den[ANO %in% "2001",],
            aes(y = TOTAL_city,label = round(TOTAL_city,2)),
            vjust = -0.7, hjust = 0.15,size = 2.5) + 
  geom_text(data = temp_den[ANO %in% "2020",],
            aes(y = TOTAL_city,label = round(TOTAL_city,2)),
            vjust = -0.7, hjust = 0.75,size = 2.5) + 
  ylim(c(0,max(temp_den$TOTAL_city) * 1.1)) + 
  labs(x = NULL,y = "Taxa de motorização (veículos/hab.)",
       title = "Taxa de motorização",
       subtitle = "Relação veículo/habitante por Região Metropolitana de 2001 a 2020",
       fill="Tipo de veículo") + 
  geom_point(data = temp_den[ANO %in% c("2001","2020"),],
             aes(x = ANO,y = TOTAL_city), shape = 3,size = 0.75) + 
  facet_wrap(~name_metro_pns,ncol = 4)+
  theme_bw()+
  theme(legend.position = "bottom") + 
  guides(fill = guide_legend(override.aes = list(shape = NA)))

ggsave("figures/DENATRAN/rm_taxa-motorizacao.png",scale=0.8,
       width = 25,height = 18,units = "cm")

lista <- ls()[ls() %nin% c(ls_initial_list,"ls_initial_list")]
rm(list = lista)


#
# map motorization rate Brasil-------------
#

map_br <- data.table::melt(data = denatran[ANO %in% c(2001,2010,2020),],
                              id.vars = c('ANO','MUNICIPIO','UF','geometry'),
                              measure.vars =  c('AUTOS_PER_POP','MOTOS_PER_POP'))
#map_br <- map_br[UF %in% "SP",]
#state_map <- geobr::read_state("all",simplified = FALSE)

max(denatran$AUTOS_PER_POP,na.rm = TRUE)
max(denatran$MOTOS_PER_POP,na.rm = TRUE)
#map_br <- sf::st_as_sf(map_br[UF %in% 'AC',])
map_br <- sf::st_as_sf(map_br)
ggplot() + 
  facet_grid(rows = vars(variable), 
             cols = vars(ANO),
             switch = "y",
             labeller = as_labeller(c('MOTOS_PER_POP' = "Motocicletas",
                                      'AUTOS_PER_POP' = "Automóveis",
                                      '2001' = '2001',
                                      '2010' = '2010',
                                      '2020' = '2020'))) + 
  geom_sf(data = map_br[map_br$variable %in% 'MOTOS_PER_POP',],
          aes(fill = value),size=0) +
  viridis::scale_fill_viridis("Motocicletas \n(veículos/hab.)",limits = c(0,0.6),
                              breaks = seq(0,0.6,by = 0.1),option = "B") +
  
  ggnewscale::new_scale_fill() + 
  
  geom_sf(data = map_br[map_br$variable %in% 'AUTOS_PER_POP',],
          aes(fill = value),size=0) +
  viridis::scale_fill_viridis("Automóveis \n(veículos/hab.)",limits = c(0,1.1),
                              breaks = seq(0,1.1,by = 0.15),option = "D") + 
  
  #geom_sf(data = state_map,fill = NA, color = 'white') + 
  theme_bw() + 
  labs(title = 'Taxa de motorização em 2019',
       subtitle = 'Número de veículos por habitante') + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = 'right')


ggsave("figures/DENATRAN/map_years_BR2.png",
       scale = 0.9,width = 30,height = 15,units = "cm")

lista <- ls()[ls() %nin% c(ls_initial_list,"ls_initial_list")]
rm(list = lista)

#
# map motorization rate RM-------------
#

head(denatran,2)
map_mot <- denatran[!is.na(name_metro_pns) & ANO %in% 2019,] %>% sf::st_as_sf()
head(map_mot)

lista_metro <- list()
metro_name <- unique(map_mot$name_metro_pns)
limits_plot <- c(min(map_mot$MOTO_RATE),max(map_mot$MOTO_RATE))

for(i in seq_along(metro_name)){ # i = 1
  lista_metro[[i]] <-  ggplot(data = 
                                map_mot[map_mot$name_metro_pns  %in% metro_name[i],]
  ) + 
    geom_sf(aes(fill = MOTO_RATE),size=0.2,color = "black") +
    viridis::scale_fill_viridis(limits = limits_plot) + 
    facet_grid(~name_metro_pns) + 
    theme_bw() + 
    labs(fill= "Taxa\n(veículos/hab.)") + 
    theme(axis.ticks = element_blank(),
          axis.text = element_blank()) + 
    theme(legend.position = 'none')
  
  #last obs
  if(i == 9){
    lista_metro[[i]] <- lista_metro[[i]] +
      theme(legend.position = "right")
  }
}
p1 <- (lista_metro[[1]] | lista_metro[[2]] | lista_metro[[3]] | lista_metro[[4]]) / 
  (lista_metro[[10]] | lista_metro[[6]] |  lista_metro[[9]] ) /
  (lista_metro[[5]] | lista_metro[[8]] | lista_metro[[7]] ) + plot_layout(nrow = 3, byrow = FALSE)

p1 +  plot_annotation(title = 'Taxa de motorização em 2019',
                      subtitle = 'Número de veículos por habitante',
                      theme = theme_classic())

ggsave("figures/DENATRAN/map_motorization_rate1.png",width = 20,height = 15,units = "cm")


p1 <- ((lista_metro[[1]] + lista_metro[[2]] + lista_metro[[3]] + lista_metro[[4]])) + 
  plot_layout(nrow = 1, byrow = FALSE)
p2 <- (lista_metro[[5]] + lista_metro[[8]]) / ( lista_metro[[9]] + lista_metro[[10]] ) + 
  plot_layout(nrow = 2, byrow = FALSE)
p3 <- lista_metro[[6]] + lista_metro[[7]] + plot_layout(nrow = 1, byrow = FALSE)

p1 / (p2 | p3) +  plot_annotation(title = 'Taxa de motorização em 2019',
                                  subtitle = 'Número de veículos por habitante',
                                  theme = theme_classic())

ggsave("figures/DENATRAN/map_motorization_rate.png")

lista <- ls()[ls() %nin% c(ls_initial_list,"ls_initial_list")]
rm(list = lista)

#
# density and urban motorization------
#

urban <- geobr::read_urban_area(simplified = FALSE)
urban$area_geo <- sf::st_area(urban$geom)
urban <- data.table::setDT(urban)
urban <- urban[,list(area = sum(area_geo)),by = code_muni]
urban[,area := units::set_units(area,"km^2") %>% as.numeric()]
urban[,code_muni := as.character(code_muni)]


temp_denatran <- denatran[ANO %in% 2015 & CODE %in% 
                            unique(urban$code_muni),]
temp_denatran[urban,on = c('CODE' = 'code_muni'), urban_area := i.area]

temp_denatran[,pop_dens := POP/urban_area]

ggplot(data = temp_denatran)  + 
  geom_point(aes(x = pop_dens,y = MOTO_RATE, 
                 fill = name_region),shape = 21,size=2.5) +
  labs(x = "Densidade populacional urbana (hab./km²)",
       y = "Taxa de motorização (veículos/hab.)",
       fill = "Região") + 
  scale_x_continuous(breaks = seq(from = 0,to = 20000,by = 2500)) + 
  scale_y_continuous(breaks = seq(0,0.8,by= 0.1)) + 
  scale_fill_viridis_d(option = "A") + 
  theme_bw() + 
  theme(legend.position = c(0.85,0.75))

ggsave("figures/DENATRAN/motorizacao_x_densidade.png",scale = 0.9,
       width = 20,height = 12,units = "cm")
plot(density$pop_dens,density$MOTO_RATE)

lista <- ls()[ls() %nin% c(ls_initial_list,"ls_initial_list")]
rm(list = lista)

#
# tx moto/ aggregated population / 2001-2020--------------
#

names(denatran)
temp <- data.table::copy(denatran)
temp[POP < 5e3, class_pop := "< 5 mil"]
temp[POP >= 5e3 & POP < 10e3, class_pop := "5 mil  - 10 mil"]
temp[POP >= 10e3 & POP < 20e3, class_pop := "10 mil - 20 mil"]
temp[POP >= 20e3 & POP < 50e3, class_pop := "20 mil - 50 mil"]
temp[POP >= 50e3 & POP < 100e3, class_pop := "50 mil - 100 mil"]
temp[POP >= 100e3 & POP < 500e3, class_pop := "100 mil - 500 mil"]
temp[POP >= 500e3, class_pop := "> 500 mil"]

temp <- temp[,lapply(.SD,mean), by = .(ANO,class_pop), 
             .SDcols = c('AUTOS_PER_POP','MOTOS_PER_POP')]

temp_br <- data.table::melt(data = temp,
                           id.vars = c('ANO','class_pop'),
                           measure.vars =  c('AUTOS_PER_POP','MOTOS_PER_POP'))

order_classpop <- c("< 5 mil","5 mil  - 10 mil","10 mil - 20 mil","20 mil - 50 mil",
                    "50 mil - 100 mil","100 mil - 500 mil","> 500 mil")
temp_br$class_pop <- factor(temp_br$class_pop,order_classpop)

temp_br[,TOTAL_city := sum(value), by = .(ANO,class_pop)]
temp_br[variable %in% "MOTOS_PER_POP",TOTAL_city := value]
temp_br <- temp_br[!is.na(class_pop),]
ggplot(temp_br,aes(x = ANO,y = value,fill = variable)) +
  geom_area(colour="black", size=.3, alpha=.8) +
  scale_fill_brewer(palette="Blues",labels = c("Automóvel","Motocicleta"))+
  
  geom_point(data = temp_br[ANO %in% c("2001","2020"),],
             aes(x = ANO,y = TOTAL_city), shape = 3,size = 0.75) + 
  geom_text(data = temp_br[ANO %in% "2001",],
            aes(y = TOTAL_city,label = round(value,2)),
            vjust = -0.7, hjust = 0.15,size = 2.5) + 
  geom_text(data = temp_br[ANO %in% "2020",],
            aes(y = TOTAL_city,label = round(value,2)),
            vjust = -0.7, hjust = 0.75,size = 2.5) + 
  labs(x = NULL,y = "Taxa de motorização (veículos/hab.)",
       title = "Taxa de motorização",
       subtitle = "Relação veículo/habitante por tamanho de município",
       fill="Tipo de veículo") + 
  facet_wrap(~class_pop,ncol = 4)+
  theme_bw()+
  theme(legend.position = "bottom") + 
  ylim(c(0,max(temp_br$TOTAL_city) * 1.05)) + 
  guides(fill = guide_legend(override.aes = list(shape = NA)))

ggsave("figures/DENATRAN/rm_taxa_city.png",scale=0.8,
       width = 25,height = 15,units = "cm")

lista <- ls()[ls() %nin% c(ls_initial_list,"ls_initial_list")]
rm(list = lista)
  #
  # aumento relativo (2001-2020)--------------
  #

names(denatran)
temp <- data.table::copy(denatran)
temp[POP < 5e3, class_pop := "< 5 mil"]
temp[POP >= 5e3 & POP < 10e3, class_pop := "5 mil  - 10 mil"]
temp[POP >= 10e3 & POP < 20e3, class_pop := "10 mil - 20 mil"]
temp[POP >= 20e3 & POP < 50e3, class_pop := "20 mil - 50 mil"]
temp[POP >= 50e3 & POP < 100e3, class_pop := "50 mil - 100 mil"]
temp[POP >= 100e3 & POP < 500e3, class_pop := "100 mil - 500 mil"]
temp[POP >= 500e3, class_pop := "> 500 mil"]

temp <- temp[,lapply(.SD,mean), by = .(ANO,class_pop), 
             .SDcols = c('AUTOS_PER_POP','MOTOS_PER_POP')]
temp[,AUTOS_PER_POP := AUTOS_PER_POP/min(AUTOS_PER_POP), by = class_pop]
temp[,MOTOS_PER_POP := MOTOS_PER_POP/min(MOTOS_PER_POP), by = class_pop]

temp_br <- data.table::melt(data = temp,
                            id.vars = c('ANO','class_pop'),
                            measure.vars =  c('AUTOS_PER_POP','MOTOS_PER_POP'))

order_classpop <- c("< 5 mil","5 mil  - 10 mil","10 mil - 20 mil","20 mil - 50 mil",
                    "50 mil - 100 mil","100 mil - 500 mil","> 500 mil")
temp_br$class_pop <- factor(temp_br$class_pop,order_classpop)

temp_br <- temp_br[!is.na(class_pop),]
ggplot(temp_br,aes(x = ANO,y = value)) +
  geom_line(aes(color = variable), size=.8, alpha=.8) +
  scale_color_discrete(labels = c("Automóvel","Motocicleta"))+
  
  geom_point(data = temp_br[ANO %in% c("2001","2020"),],
             aes(x = ANO,y = value), shape = 1,size = 0.75) + 
  geom_text(data = temp_br[ANO %in% "2001",],
            aes(y = value,label = round(value,2)),
            vjust = -0.7, hjust = 0.15,size = 2.5) + 
  geom_text(data = temp_br[ANO %in% "2020",],
            aes(y = value,label = round(value,2)),
            vjust = -0.7, hjust = 0.75,size = 2.5) + 
  labs(x = NULL,y = "Aumento com relação a 2001",
       title = "Aumento da taxa de motorização",
       subtitle = "Crescimento da taxa de motorização com relação ao ano base 2001\nMédia de municipios conforme faixa populacional",
       color="Tipo de veículo",fill = NULL) + 
  facet_wrap(~class_pop,ncol = 4)+
  theme_bw()+
  theme(legend.position = "bottom") +
  ylim(c(1,max(temp_br$value) * 1.05)) +  
  guides(fill = guide_legend(override.aes = list(shape = NA)))

ggsave("figures/DENATRAN/rm_taxa_city_adjust.png",scale=0.8,
       width = 25,height = 15,units = "cm")
