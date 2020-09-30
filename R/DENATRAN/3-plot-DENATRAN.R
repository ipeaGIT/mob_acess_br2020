#
# initial config----------------
#

rm(list=ls())
gc(reset = T)
library(XLConnect)
library(patchwork)
library(cowplot)
library(gganimate)  # install.packages("gganimate")
library(ggrepel)
source("R/PNS/0_loadpackages.R",local = TRUE)
source("R/colours.R")
source("R/DENATRAN/colors_plot.R")
source("R/style.R")
source("R/DENATRAN/aop_style1.R")
`%nin%` = Negate(`%in%`)

dir.create("figures/DENATRAN")

denatran <- readr::read_rds("data/DENATRAN/DENATRAN_jan.rds")
unique(denatran$name_metro_pns)
denatran[name_metro_pns %in% "RIDE   REGIAO INTEGRADA DE DESENVOLVIMENTO DO DISTRITO FEDERAL E ENTORNO",
         name_metro_pns := "RM BRASILIA"]
ls_initial_list <- c("denatran","%nin%","scale_colour_aop","scale_fill_aop",
                     "aop_palettes","aop_style","aop_style1","aop_colors")
break()

#
# general analysis (ga) -----
#
temp_pns <- data.table::copy(denatran)[!is.na(name_metro_pns),]
temp_nmetro <- data.table::copy(denatran)[is.na(name_metro_pns),][,name_metro_pns := "BRASIL NÃO-METROPOLITANO"]
temp_metro <- data.table::copy(denatran)[!is.na(name_metro_pns),][,name_metro_pns := "BRASIL METROPOLITANO"]

temp_den <- list(temp_pns,temp_nmetro,temp_metro) %>% data.table::rbindlist()
temp_den <- temp_den[,
                     lapply(.SD,sum,na.rm = TRUE),
                     .SDcols = c("TOTAL_AUTOS","TOTAL_MOTOS"), by = .(ANO,name_metro_pns)]
temp_den[,TOTAL_FLEET := TOTAL_AUTOS + TOTAL_MOTOS]
temp_den <- temp_den[,`:=`(TOTAL_FLEET = TOTAL_FLEET / min(TOTAL_FLEET),
                           TOTAL_AUTOS = TOTAL_AUTOS / min(TOTAL_AUTOS),
                           TOTAL_MOTOS = TOTAL_MOTOS / min(TOTAL_MOTOS)), by = .(name_metro_pns)]
data.table::copy(denatran)[!is.na(name_metro_pns) & ANO %in% c(2020),][,
                                                                       `:=`(auto = sum(TOTAL_AUTOS,na.rm = TRUE)/sum(POP,na.rm = TRUE),
                                                                            moto = sum(TOTAL_MOTOS,na.rm = TRUE)/sum(POP,na.rm = TRUE),
                                                                            total = (sum(TOTAL_AUTOS,na.rm = TRUE) + 
                                                                                       sum(TOTAL_MOTOS,na.rm = TRUE)) / 
                                                                              sum(POP,na.rm = TRUE)),
                                                                       by = name_metro_pns][,.SD[1],by = name_metro_pns][order(moto),]

rm(list = c('temp_den','temp_nmetro','temp_metro'))

lista_capitais <- c("Sao Paulo","Rio de Janeiro",
                    "Brasília","Salvador",
                    "Fortaleza","Belo Horizonte",
                    "Manaus","Curitiba",
                    "Recife", "Goiania",
                    "Belem", "Porto Alegre",
                    "Sao Luis","Maceio",
                    "Campo Grande","Natal",
                    "Teresina","Joao Pessoa",
                    "Aracaju", "Cuiaba",
                    "Porto Velho","Macapá",
                    "Florianopolis","Rio Branco",
                    "Boa Vista", "Vitoria","Palmas")
code <- lapply(lista_capitais,function(i){
  geobr::lookup_muni(i)}) %>% data.table::rbindlist()

temp_den <- data.table::copy(denatran)[CODE %in% code$code_muni &
                                         ANO %in% 2020,][,.(ANO,MUNI_UF,AUTOS_PER_POP,MOTOS_PER_POP,MOTO_RATE)]
temp_den[order(MOTOS_PER_POP),]
#
# check names-------------
#
uniqueN(denatran$MUNI_UF)
unique(denatran$UF)
unique(denatran$ANO)
unique(denatran$name_metro)
unique(denatran$name_metro_pns)
unique(denatran$name_region)

lista <- ls()[ls() %nin% c(ls_initial_list,"ls_initial_list")]
rm(list = lista)
#
# taxa / rms / 2001-2020 ------
#

temp_pns <- denatran[!is.na(name_metro_pns),]
temp_nmetro <- denatran[is.na(name_metro_pns),][,name_metro_pns := "BRASIL NÃO-METROPOLITANO"]
temp_metro <- denatran[!is.na(name_metro_pns),][,name_metro_pns := "BRASIL METROPOLITANO"]

temp_den <- list(temp_pns,temp_nmetro,temp_metro) %>% data.table::rbindlist()
temp_den <- temp_den[!is.na(name_metro_pns) & !is.na(TOTAL_AUTOS) 
                     & !is.na(TOTAL_MOTOS) & !is.na(POP),]
temp_den <- temp_den[,`:=`(TOTAL_AUTOS = sum(TOTAL_AUTOS),
                           TOTAL_MOTOS = sum(TOTAL_MOTOS),
                           POP = sum(POP)),
                     by = .(name_metro_pns,ANO)]
temp_den[, AUTOS_PER_POP := TOTAL_AUTOS / POP]
temp_den[, MOTOS_PER_POP := TOTAL_MOTOS / POP]
temp_den[, TOTAL_FLEET := TOTAL_AUTOS + TOTAL_MOTOS]
temp_den[, c('POP','TOTAL_AUTOS','TOTAL_MOTOS') := NULL]

temp_den <- temp_den[,.SD[1], by = .(name_metro_pns,ANO)][,MUNI_UF := NULL]
# y axis
#temp_den[,AUTOS_PER_POP_y := AUTOS_PER_POP - MOTOS_PER_POP]

temp_den <- data.table::melt(data = temp_den,
                             id.vars =  c('ANO','name_metro_pns'),
                             measure.vars =  c('AUTOS_PER_POP','MOTOS_PER_POP'),
                             variable.name = 'TYPE',
                             value.name = "TOTAL")

temp_den[,name_metro_pns := stringr::str_to_title(name_metro_pns)]
temp_den[,name_metro_pns := gsub("Rm ","",name_metro_pns)]
temp_den[,name_metro_pns := gsub("De","de",name_metro_pns)]

temp_den[,TOTAL_y := -1*diff(TOTAL), by = .(ANO,name_metro_pns)]
temp_den[TYPE %in% "MOTOS_PER_POP",TOTAL_y := TOTAL]
temp_den[name_metro_pns %in% "Curitiba" & ANO %in% 2020,]

#
# plot
#

p1 <- ggplot(temp_den,
             aes(x = ANO,y = TOTAL_y,fill = as.factor(TYPE))) +
  geom_area(colour = "black", size=.3, alpha=.8) +
  geom_text(data = temp_den[ANO %in% c("2001"),],
            aes(y = TOTAL,label = round(TOTAL,2)),
            vjust = -0.7, hjust = 0.15,size = 2.5) + 
  geom_text(data = temp_den[TYPE %in% "AUTOS_PER_POP" & 
                              ANO %in% c("2020"),],
            aes(y = TOTAL,label = round(TOTAL,2)),
            vjust = -0.7, hjust = 0.75,size = 2.5) +
  geom_text(data = temp_den[TYPE %in% "MOTOS_PER_POP" & 
                              name_metro_pns %in% "Belem" & 
                              ANO %in% c("2020"),],
            aes(y = TOTAL_y,label = round(TOTAL,2)),
            vjust = +1.45, hjust = 0.75,size = 2.5) + 
  geom_text(data = temp_den[TYPE %in% "MOTOS_PER_POP" & 
                              name_metro_pns %nin% "Belem" & 
                              ANO %in% c("2020"),],
            aes(y = TOTAL_y,label = round(TOTAL,2)),
            vjust = -0.7, hjust = 0.75,size = 2.5) +
  ylim(c(0,max(temp_den$TOTAL) * 1.1)) +
  labs(x = NULL,y = "Taxa de motorização (veículos/hab.)",
       title = "Taxa de motorização",
       subtitle = "Relação veículo/habitante por Região Metropolitana de 2001 a 2020",
       caption = "Fonte: DENATRAN (2020) e IBGE (2020).",
       fill="Tipo de veículo") + 
  geom_point(data = temp_den[ANO %in% c("2001","2020"),],
             aes(x = ANO,y = TOTAL), shape = 3,size = 0.75) + 
  facet_wrap(~name_metro_pns,ncol = 4)+
  aop_style() +
  scale_fill_manual(values = as.vector(aop_colors$wrapper[c(3,2)]),
                    labels = c("Automóvel","Motocicleta")) + 
  theme( panel.grid.major.y = element_line(colour = "gray90"),
         legend.position = "bottom",
         #axis.title.y = element_text(vjust=-0.5)
  ) + 
  guides(fill = guide_legend(override.aes = list(shape = NA)))

ggsave("figures/DENATRAN/rm_taxa-motorizacao.png", width = 25, 
       height = 18, scale = 0.8, units = "cm", dpi = 300, device = 'png')

lista <- ls()[ls() %nin% c(ls_initial_list,"ls_initial_list")]
rm(list = lista)


#
# map motorization rate Brasil-------------
#

map_br <- data.table::melt(data = data.table::copy(denatran)[ANO %in% c(2001,2010,2020),],
                           id.vars = c('ANO','MUNICIPIO','UF','geometry'),
                           measure.vars =  c('AUTOS_PER_POP','MOTOS_PER_POP'))
#map_br <- map_br[UF %in% "SP",]
#state_map <- geobr::read_state("all",simplified = FALSE)
map_br$variable <- factor(map_br$variable,c('AUTOS_PER_POP','MOTOS_PER_POP'))
max(denatran$AUTOS_PER_POP,na.rm = TRUE)
max(denatran$MOTOS_PER_POP,na.rm = TRUE)
#map_br <- sf::st_as_sf(map_br[UF %in% 'AC',])
map_br <- sf::st_as_sf(map_br)

pf <- ggplot() + 
  facet_grid(rows = vars(variable), 
             cols = vars(ANO),
             switch = "y",
             labeller = as_labeller(c(
               'MOTOS_PER_POP' = "Motocicletas",
               'AUTOS_PER_POP' = "Automóveis",
               '2001' = '2001',
               '2010' = '2010',
               '2020' = '2020'))) + 
  
  geom_sf(data = map_br[map_br$variable %in% 'AUTOS_PER_POP',],
          aes(fill = value),size=0) +
  viridis::scale_fill_viridis("Automóveis\n(veículos/habitantes)\n ",
                              limits = c(0,1.1),
                              breaks = seq(0,1.1,by = 0.15),
                              option = "D"
                              #limits = c(0,0.6),
                              #breaks = seq(0,0.6,by = 0.1),
                              #option = "B"
  ) +
  ggnewscale::new_scale_fill() + 
  
  geom_sf(data = map_br[map_br$variable %in% 'MOTOS_PER_POP',],
          aes(fill = value),size=0) +
  viridis::scale_fill_viridis("Motocicletas\n(veículos/habitantes)\n",
                               #limits = c(0,1.1),
                              #  breaks = seq(0,1.1,by = 0.15),
                               option = "A",
                              limits = c(0,0.6),
                              breaks = seq(0,0.6,by = 0.1),
                              #option = "B"
  ) +
  # viridis::scale_fill_viridis("Automóveis\n(veículos/habitante)\n ",limits = c(0,1.1),
  #                              breaks = seq(0,1.1,by = 0.15),option = "D") +
  #geom_sf(data = state_map,fill = NA, color = 'white') +
  aop_style() +
  #theme_bw() + 
  labs(title = 'Taxa de motorização',
       subtitle = 'Número de veículos por habitante',
       caption = "Fonte: DENATRAN (2020) e IBGE (2020).") + 
  theme(axis.ticks = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = ggtext::element_markdown(
          size = 15,
          margin = margin(r = 0.25, unit = 'cm'),
          lineheight = 0.5,
          colour = "#575757",
          hjust = 0,
          vjust = 1),
        legend.position = "right",
        legend.title = element_text(size = 8, colour = "#575757"),
        strip.text.y =  element_text(size = 8,
                                     face = "plain",
                                     colour = "#575757",
                                     hjust = 1)) 

pf
ggsave("figures/DENATRAN/map_years_BR21.png",
       scale = 1,width = 23.5,height = 15,dpi = 300, units = "cm")

lista <- ls()[ls() %nin% c(ls_initial_list,"ls_initial_list")]
rm(list = lista)

#
# density / urban motorization------
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

#
city_annotation <- temp_denatran[order(TOTAL_AUTOS,decreasing = TRUE),]$CODE[1:4] 

ggplot(data = temp_denatran)  + 
  geom_point(aes(x = pop_dens,y = MOTO_RATE, 
                 fill = name_region),shape = 21,size=2.5) +
  labs(x = "Densidade populacional urbana (hab./km²)",
       y = "Taxa de motorização (veículos/hab.)",
       fill = "Região",caption = "Fonte: DENATRAN (2015) e IBGE (2015).") + 
  scale_x_continuous(breaks = seq(from = 0,to = 20000,by = 2500)) + 
  scale_y_continuous(breaks = seq(0,0.8,by= 0.1)) + 
  #scale_fill_aop(palette = "original") + 
  scale_fill_manual(values = 
                      c(as.vector(aop_colors$qualitativo[c(3)]),
                        as.vector(aop_colors$crimson[3]),
                        as.vector(aop_colors$cartola[6]),
                        as.vector(aop_colors$qualitativas[c(4,6)]))) + 
  #scale_fill_viridis_d(option = "A") + 
  aop_style() + 
  theme(legend.position = c(0.9,0.8))

ggsave("figures/DENATRAN/motorizacao_x_densidade.png",scale = 1,
       width = 20,height = 12, dpi = 300, units = "cm")

lista <- ls()[ls() %nin% c(ls_initial_list,"ls_initial_list")]
rm(list = lista)
gc(reset = TRUE)

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


#
# aumento relativo (2001-2020) / facet_grid --------------
#

names(denatran)
temp <- data.table::copy(denatran)
label_classpop <- c("< 5 mil","5 mil  - 10 mil","10 mil - 20 mil","20 mil - 50 mil",
                    "50 mil - 100 mil","100 mil - 500 mil","> 500 mil")
temp[POP < 5e3, class_pop := label_classpop[1]]
temp[POP >= 5e3 & POP < 10e3, class_pop := label_classpop[2]]
temp[POP >= 10e3 & POP < 20e3, class_pop := label_classpop[3]]
temp[POP >= 20e3 & POP < 50e3, class_pop := label_classpop[4]]
temp[POP >= 50e3 & POP < 100e3, class_pop := label_classpop[5]]
temp[POP >= 100e3 & POP < 500e3, class_pop := label_classpop[6]]
temp[POP >= 500e3, class_pop := label_classpop[7]]

temp <- temp[,lapply(.SD,mean), by = .(ANO,class_pop), 
             .SDcols = c('AUTOS_PER_POP','MOTOS_PER_POP')]
temp[,AUTOS_PER_POP_rel := AUTOS_PER_POP/min(AUTOS_PER_POP), by = class_pop]
temp[,MOTOS_PER_POP_rel := MOTOS_PER_POP/min(MOTOS_PER_POP), by = class_pop]

temp_br <- data.table::melt(data = temp,
                            id.vars = c('ANO','class_pop'),
                            measure.vars =  c('AUTOS_PER_POP','MOTOS_PER_POP',
                                              'AUTOS_PER_POP_rel','MOTOS_PER_POP_rel'))


temp_br$class_pop <- factor(temp_br$class_pop,label_classpop)
temp_br <- temp_br[!is.na(class_pop),]

myfrank <- function(x,r){x[rank(x)==r]}
temp_br[,`:=`(min = min(value),
              thrid = myfrank(value,3),
              median = median(value),
              fourth = myfrank(value,4),
              fifth = myfrank(value,5),
              sixth = myfrank(value,6),
              max = max(value)), by = .(variable,ANO)]
`%nlike%` <- Negate(`%like%`)

p1 <- ggplot(temp_br[variable %nlike% "rel"],
             aes(x = ANO,y = value, group = class_pop)) +
  geom_line(aes(color = class_pop), size=.8, alpha=.8) +
  scale_color_brewer(palette = "Reds",direction = -1,
                     labels = label_classpop) + 
  #scale_fill_manual(values = 
  #                    c(as.vector(aop_colors$))
  #scale_colour_aop(palette = "cartola",labels = order_classpop) +
  geom_point(data = temp_br[ANO %in% c("2001","2020") & 
                              variable %nlike% "rel",],
             aes(x = ANO,y = value), shape = 1,size = 0.75) +
  geom_text(data = temp_br[ANO %in% c("2001","2020") &
                             variable %nlike% "rel",
                           .SD[1],by = .(variable,ANO)],
            aes(y = max,label = round(max,2)),
            vjust = -0.35, hjust = -0.5,size = 2.5) +
  geom_text(data = temp_br[ANO %in% c("2001","2020") &
                             variable %nlike% "rel",
                           .SD[1],by = .(variable,ANO)],
            aes(y = min,label = round(min,2)),
            vjust = +0.75, hjust = -0.5,size = 2.5) +
  # middle
  geom_text(data = temp_br[ANO %in% c("2020") &
                             variable %like% "AUTO" &
                             variable %nlike% "rel",
                           .SD[1],by = .(variable,ANO)],
            aes(y = median,label = round(median,2)),
            vjust = +0.75, hjust = -0.5,size = 2.5) +
  # sixth 2020
  geom_text(data = temp_br[ANO %in% c("2020") &
                             variable %nlike% "rel",
                           .SD[1],by = .(variable,ANO)],
            aes(y = sixth,label = round(sixth,2)),
            vjust = +0.75, hjust = -0.5,size = 2.5) +
  # fifth 2001 auto
  geom_text(data = temp_br[ANO %in% c("2001") &
                             variable %like% "AUTO" &
                             variable %nlike% "rel",
                           .SD[1],by = .(variable,ANO)],
            aes(y = fifth,label = round(fifth,2)),
            vjust = +0.75, hjust = -0.5,size = 2.5) +
  labs(x = NULL,y = "Taxa de motorização (veículos/hab.)",
       title = "Taxa de motorização",
       subtitle = "Crescimento absoluto e relativo conforme faixa populacional",
       color="Tamanho do \nmunicípio",fill = NULL) + 
  facet_wrap(facets = vars(variable),nrow = 1,
             labeller = as_labeller(c('AUTOS_PER_POP' = "Automóveis",
                                      'MOTOS_PER_POP' = "Motocicletas")))+
  xlim(c(2001,2022)) +  
  ylim(c(0, 1.1* max(max(temp_br[variable %nlike% "rel",value])))) + 
  aop_style1() +
  theme(legend.position = "right",
        strip.placement = "inside",
        legend.text = element_text(size = 10, colour = "#808080"),
        legend.title = element_text(size = 8, colour = "#575757")) +
  guides(fill = guide_legend(override.aes = list(shape = NA)))

p2 <- ggplot(temp_br[variable %like% "rel",],aes(x = ANO,y = value, group = class_pop)) +
  geom_line(aes(color = class_pop), size=.8, alpha=.8) +
  scale_color_brewer(palette = "Reds",direction = -1) +
  geom_point(data = temp_br[ANO %in% c("2001","2020") &
                              variable %like% "rel",],
             aes(x = ANO,y = value), shape = 1,size = 0.75) +
  # add median 2020
  geom_text(data = temp_br[ANO %in% c("2020") &
                             variable %like% "rel",
                           .SD[1],by = .(variable,ANO)],
            aes(y = median,label = round(median,2)),
            vjust = -0.35, hjust = -0.5,size = 2.5) +
  # add thrid 2020
  geom_text(data = temp_br[ANO %in% c("2020") &
                             variable %like% "rel",
                           .SD[1],by = .(variable,ANO)],
            aes(y = thrid,label = round(thrid,2)),
            vjust = -0.35, hjust = -0.5,size = 2.5) +
  geom_text(data = temp_br[ANO %in% c("2001","2020") &
                             variable %like% "rel",
                           .SD[1],by = .(variable,ANO)],
            aes(y = max,label = round(max,2)),
            vjust = -0.35, hjust = -0.5,size = 2.5) +
  geom_text(data = temp_br[ANO %in% c("2020") &
                             variable %like% "rel",
                           .SD[1],by = .(variable)],
            aes(y = min,label = round(min,2)),
            vjust = +0.75, hjust = -0.5,size = 2.5) +
  labs(x = NULL,y = "Aumento com relação a 2001",
       title = NULL,
       caption = "Fonte: DENATRAN e IBGE (2020).",
       subtitle = NULL,
       color="Tamanho do \nmunicípio",fill = NULL) +
  facet_wrap(facets = vars(variable),nrow = 1,
             labeller = as_labeller(c('AUTOS_PER_POP_rel' = "Automóveis",
                                      'MOTOS_PER_POP_rel' = "Motocicletas"))
  )+
  aop_style1()+
  theme(legend.position = "none",
        strip.placement = "inside") +
  xlim(c(2001,2022)) +  
  ylim(c(0.7, 1.1* max(max(temp_br[variable %like% "rel",value])))) + 
  guides(fill = guide_legend(override.aes = list(shape = NA)))

pf <- (p1 / p2) + plot_annotation(tag_levels = 'A')

ggsave("figures/DENATRAN/rm_taxa_city_adjust_facetveh.png",scale=1,
       width = 20,height = 15,dpi = 300,units = "cm")
