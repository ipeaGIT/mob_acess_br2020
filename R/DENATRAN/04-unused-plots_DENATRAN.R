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



#
# pib / urban motorization------
#

pib <- openxlsx::read.xlsx(xlsxFile = "data-raw/PIB/tabela5938.xlsx",startRow = 4)
names(pib) <- c("nivel","code_muni","name","pib","units")
pib <- data.table::setDT(pib)[nivel %in% "MU",]

temp_denatran <- denatran[ANO %in% 2017 & CODE %in% 
                            unique(pib$code_muni),]
# class pop
temp_denatran[POP < 5e3, class_pop := "< 5 mil"]
temp_denatran[POP >= 5e3 & POP < 10e3, class_pop := "5 mil  - 10 mil"]
temp_denatran[POP >= 10e3 & POP < 20e3, class_pop := "10 mil - 20 mil"]
temp_denatran[POP >= 20e3 & POP < 50e3, class_pop := "20 mil - 50 mil"]
temp_denatran[POP >= 50e3 & POP < 100e3, class_pop := "50 mil - 100 mil"]
temp_denatran[POP >= 100e3 & POP < 500e3, class_pop := "100 mil - 500 mil"]
temp_denatran[POP >= 500e3, class_pop := "> 500 mil"]
# merge
temp_denatran[pib,on = c('CODE' = 'code_muni'), pib := i.pib]
temp_denatran[,pib_cap := pib * 1000 / POP]

#
#city_annotation <- temp_denatran[order(TOTAL_AUTOS,decreasing = TRUE),]$CODE[1:4] 

ggplot(data = temp_denatran[pib_cap < 100000,])  + 
  geom_point(aes(x = pib_cap,y = MOTO_RATE, 
                 fill = name_region),shape = 21,size=2.5,alpha = 1) +
  labs(x = "PIB per capita (R$/hab.)",
       y = "Taxa de motorização (veículos/hab.)",
       fill = "Região",caption = "Fonte: DENATRAN (2017) e IBGE (2017).") + 
  #scale_x_continuous(breaks = seq(from = 0,to = 20000,by = 2500)) + 
  facet_wrap(~name_region) + 
  scale_y_continuous(breaks = seq(0,1.2,by= 0.1)) + 
  scale_fill_viridis_d(option = "A") + 
  theme_bw() + 
  theme(legend.position = c(0.85,0.25))
#theme(legend.position = c(0.85,0.75))

ggsave("figures/DENATRAN/motorizacao_x_densidade.png",scale = 0.9,
       width = 20,height = 12,units = "cm")

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