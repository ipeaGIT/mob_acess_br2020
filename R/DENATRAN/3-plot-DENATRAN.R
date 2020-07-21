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
ls_initial_list <- ls()
dir.create("figures/DENATRAN")

denatran <- readr::read_rds("data/DENATRAN/DENATRAN_jan.rds")
denatran[name_metro_pns %in% "RIDE - REGIAO INTEGRADA DE DESENVOLVIMENTO DO DISTRITO FEDERAL E ENTORNO",
         name_metro_pns := "RM BRASILIA"]
#
# check names-------------
#

unique(denatran$UF)
unique(denatran$ANO)
unique(denatran$name_metro)
unique(denatran$name_metro_pns)
unique(denatran$name_region)

#
# metro / region/ time - metro PNS ------
#

temp_den <- denatran[!is.na(name_metro_pns),]
unique(denatran$name_metro_pns)
temp_den <- temp_den[,list(TOTAL_AUTOS = sum(TOTAL_AUTOS),
                           TOTAL_MOTOS = sum(TOTAL_MOTOS),
                           POP = sum(POP)),
                     by = .(name_metro_pns,ANO)]
temp_den[, AUTOS_PER_POP := TOTAL_AUTOS / POP]
temp_den[, MOTOS_PER_POP := TOTAL_MOTOS / POP]
temp_den[, c('POP','TOTAL_AUTOS','TOTAL_MOTOS') := NULL]


temp_den <-data.table::melt(data = temp_den,
                            id.vars =  c('ANO','name_metro_pns'),
                            variable.name = 'TYPE',
                            value.name = "TOTAL")

ggplot(temp_den,aes(x = ANO,y = TOTAL,fill = TYPE)) +
  geom_area(colour="black", size=.3, alpha=.8) +
  scale_fill_brewer(palette="Blues",labels = c("Automóvel","Motocicleta"))+
  labs(x="Ano",y= "Taxa de motorização (veículos/hab.)",
       fill="Tipo de veículo") + 
  facet_wrap(~name_metro_pns,ncol = 5)+
  theme_bw()+
  theme(legend.position = "bottom")

ggsave("figures/DENATRAN/rm_taxa-motorizacao.png",scale=0.8,
       width = 30,height = 14,units = "cm")

#
#
# map motorization rate Brasil-------------
#
#
map_br <- denatran[ANO %in% c(2001,2010,2015,2020)] %>% sf::st_as_sf()
limits_plot <- c(min(map_br$MOTO_RATE,na.rm = TRUE),max(map_br$MOTO_RATE,na.rm = TRUE))
ggplot(data = map_br) + 
  geom_sf(aes(fill = MOTO_RATE),size=0) +
  viridis::scale_fill_viridis(limits = c(0,1.25),
                              breaks = seq(0,1.25,by = 0.25)) + 
  facet_wrap(~ANO,ncol = 2) + 
  theme_bw() + 
  labs(fill= "Taxa\n(veículos/hab.)",title = 'Taxa de motorização',
       subtitle = 'Número de veículos por habitante') + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank()) + 
  theme(legend.position = 'right')

ggsave("figures/DENATRAN/map_years_BR1.png",width = 15,height = 12,units = "cm")
#
#
# map motorization rate RM-------------
#
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
#
# density and urban motorization------
#

urban <- geobr::read_urban_area()
urban$area_geo <- sf::st_area(urban$geom)
urban <- data.table::setDT(urban)
urban <- urban[,list(area = sum(area_geo)),by = code_muni]
urban[,area := units::set_units(area,"km^2") %>% as.numeric()]
urban[,code_muni := as.character(code_muni)]


temp_denatran <- denatran[ANO %in% 2019 & CODE %in% 
                            unique(urban$code_muni),]
temp_denatran[urban,on = c('CODE' = 'code_muni'), urban_area := i.area]

temp_denatran[,pop_dens := POP/urban_area]

ggplot(data = temp_denatran)  + 
  geom_point(aes(x = pop_dens,y = MOTO_RATE, 
                 fill = name_region),shape = 21,size=3) +
  labs(x = "Densidade populacional urbana (hab./km²)",
       y = "Taxa de motorização (veículos/hab.)",
       fill = "Região") + 
  scale_x_continuous(breaks = seq(from = 0,to = 20000,by = 2500)) + 
  scale_y_continuous(breaks = seq(0,0.8,by= 0.1)) + 
  scale_fill_viridis_d(option = "A") + 
  theme_bw() + 
  theme(legend.position = c(0.85,0.80))

ggsave("figures/DENATRAN/motorizacao_x_densidade.png",scale = 1,
       width = 20,height = 12,units = "cm")
plot(density$pop_dens,density$MOTO_RATE)

