rm(list=ls())
gc(reset = T)
source("R/PNS/0_loadpackages.R",local = TRUE)
`%nin%` = Negate(`%in%`)
pns2013 <- readr::read_rds("data/PNS/pns2013.Rds")

ls_initial_list <- ls()

dir.create("figures/PNS")

#
# types of data of PNS--------
#

unique(pns2013$metro)
unique(pns2013$C009)
unique(pns2013$physicallyactive30)
unique(pns2013$edugroup)
unique(pns2013$v0302)
unique(pns2013$actv_commutetime)
unique(pns2013$quintileMetro)
unique(pns2013$decileBR)
unique(pns2013$quartileMetro)
unique(pns2013$uf)
unique(pns2013$urban)
unique(pns2013$agegroup)
unique(pns2013$AGE)
unique(pns2013$dummyVehicle)

#
# plot - age x gender x metropolitan region  ------------
#

pns2013df <- pns2013[urban %in% "Urbano",][AGE %nin% '0-17',]
pns2013df <- pns2013df[,sum(actv_commutetime15)/.N,by = .(AGE,v0302,metro)]
pns2013df <- pns2013df[!is.na(AGE) & !is.na(v0302) & !is.na(metro),]

ggplot(data = pns2013df,
       aes(x = (factor(AGE)), 
           y = (100 * V1),
           fill = factor(v0302))) + 
  geom_boxplot() + 
  scale_fill_brewer(palette = "Set2",
                    direction = +1)  +
  scale_y_continuous(breaks = seq(0,30,by=5)) + 
  labs(fill = "Gênero") + 
  ylab("Proporção de pessoas no ambiente urbano 
  que se deslocam a pé ou de bicicleta por no mínimo 
       15 minutos, conforme idade") + 
  xlab("Faixa de idade") + 
  theme( legend.position = c(.9, .9),
         axis.text.y = element_text(size = rel(1.25)),
         axis.text.x = element_text(size = rel(1.25)),
         panel.background = element_rect(fill = "white",colour = NA),
         panel.border = element_rect(fill = NA, colour = "grey20"),
         panel.grid = element_line(colour = "grey82",size = rel(0.35)), 
         panel.grid.minor = element_line(size = rel(0.35)), 
         strip.background = element_rect(fill = "grey82",colour = "grey20"),
         legend.key = element_rect(fill = "white", colour = NA))

ggsave(filename = "figures/PNS/FIG_agegroup_gender_active15.png",
       width = 25, height = 14,scale = 0.9, units = 'cm')

# keep only initial files
lista <- ls()[ls() %nin% c(ls_initial_list,"ls_initial_list")]
rm(list = lista)

#
# plot - gender / decil /  rate of active commute 30-------------
#

pns2013df <- pns2013[,sum(actv_commutetime30)/.N,by = .(decileBR,v0302)]
pns2013df <- pns2013df[!is.na(v0302) & !is.na(decileBR),]
pns2013df$decileBR <- factor(pns2013df$decileBR,1:10)
pns2013df$v0302 <- factor(pns2013df$v0302,c("Masculino","Feminino"))

ggplot(data = pns2013df,
       aes(x = (factor(decileBR)), 
           y = 100 * (V1))) + 
  geom_point(aes(color = factor(v0302)),
             position = position_dodge(0.0),size=2) +
  geom_line(aes(group = factor(v0302),color = factor(v0302))) +
  scale_x_discrete(breaks = 1:10, labels = paste0(1:10,"º") ) + 
  scale_y_continuous(breaks = seq(0,15,by=3),limits = c(0,16))+
  scale_color_brewer(palette = "Set2",
                     direction = +1)  +
  labs(color = NULL) + 
  ylab("Frequência(%) de pessoas que se deslocam
         a pé ou de bicicleta para o trabalho 
         por no mínimo 30 minutos,
       conforme decil de renda") + 
  xlab("Decil de renda") + 
  theme( legend.position = "bottom",
         axis.text.y = element_text(size = rel(1.25)),
         axis.text.x = element_text(size = rel(1.25)),
         panel.background = element_rect(fill = "white",colour = NA),
         panel.border = element_rect(fill = NA, colour = "grey20"),
         panel.grid = element_line(colour = "grey82",size = rel(0.35)), 
         panel.grid.minor = element_line(size = rel(0.35)), 
         strip.background = element_rect(fill = "grey82",colour = "grey20"),
         legend.key = element_rect(fill = "white", colour = NA),
         legend.text = element_text(size = rel(1.05)))

ggsave(filename = "figures/PNS/FIG_decil_gender_actv30.png",height = 10, width = 20, units = 'cm')
# keep only initial files
lista <- ls()[ls() %nin% c(ls_initial_list,"ls_initial_list")]
rm(list = lista)

#
# plot - active commute acive 15 / region / educational level-------------
#

pns2013df <- pns2013[urban %in% "Urbano",]
pns2013df <- pns2013df[,sum(actv_commutetime15)/.N,by = .(C009,region)]
pns2013df <- pns2013df[,.(C009,V1,region)]
# br 
pns2013br <- pns2013[urban %in% "Urbano",]
pns2013br <- pns2013br[,sum(actv_commutetime15)/.N,by = .(C009)]
pns2013br <- pns2013br[,region := "Brasil"]
pns2013df <- list(pns2013df,pns2013br) %>% data.table::rbindlist()
pns2013df <- pns2013df[!is.na(C009) & !is.na(region),]

order_region <- unique(pns2013df$region)
pns2013df$region <- factor(pns2013df$region,order_region)

avg <- pns2013df[,lapply(.SD,mean),by = region,.SDcols = "V1"]
lista <- list(c("Sul","Sudeste","Centro-Oeste"),c("Norte","Nordeste","Brasil"))
pp <- list()
for(i in 1:2){ # i = 1 
  aux_pns <- pns2013df[region %in% lista[[i]],]
  aux_avg <- avg[region %in% lista[[i]],]
  
  pp[[i]] <- ggplot(data = aux_pns,
                    aes(y = factor(C009), 
                        x = 100 * (V1))) + 
    geom_bar(fill = "lightblue",
             color="black",width = 0.75,size=0.1,
             stat = "identity", 
             position = position_stack(reverse = TRUE)) +
    scale_fill_brewer(palette = "Pastel1",
                      direction = +1)  +
    labs(fill = "Grau de escolaridade") + 
    facet_grid(cols = vars(region)) +
    geom_vline(data = aux_avg, 
               aes(xintercept = 100 * V1),
               linetype = "dashed",
               colour = "black", alpha = 0.75) +
    geom_text(data = aux_avg,
              aes(x = 100 * V1, y = 1.25, 
                  label= (paste0("Média =\n"," ",round(100 * V1,1),"%"))),
              vjust = 0, hjust = -0.1, colour = "black",size = 3.5) + 
    ylab(NULL) +
    xlab(NULL) + 
    xlim(100 * c(0,max(pns2013df$V1))) + 
    guides(fill=guide_legend(ncol=2)) + 
    theme( legend.position = "none",
           axis.text.x = element_text(size = rel(1.25)),
           axis.text.y = element_text(size = rel(1.25)),
           panel.background = element_rect(fill = "white",colour = NA),
           panel.border = element_rect(fill = NA, colour = "grey20"),
           panel.grid = element_line(colour = "grey82",size = rel(0.35)), 
           panel.grid.minor = element_line(size = rel(0.35)), 
           strip.background = element_rect(fill = "grey82",colour = "grey20"),
           legend.key = element_rect(fill = "white", colour = NA))
  
  if(i == 2){
    pp[[i]] <- pp[[i]] + 
      xlab("Frequência(%) de pessoas residentes na zona urbana que se deslocam
      a pé ou de bicicleta para o trabalho por no mínimo 15 minutos, por região")
  }
}
pf <- pp[[1]] / pp[[2]]
pf
ggsave(pf,filename = "figures/PNS/FIG_income_region_race15.png",width = 27,
       height = 15,scale = 0.8,units = "cm")
# keep only initial files
lista <- ls()[ls() %nin% c(ls_initial_list,"ls_initial_list")]
rm(list = lista)

#
# plot - active commute acive 30 / region / educational level-------------
#

pns2013df <- pns2013[urban %in% "Urbano",]
pns2013df <- pns2013df[,sum(actv_commutetime30)/.N,by = .(C009,region)]
pns2013df <- pns2013df[,.(C009,V1,region)]
# br 
pns2013br <- pns2013[urban %in% "Urbano",]
pns2013br <- pns2013br[,sum(actv_commutetime30)/.N,by = .(C009)]
pns2013br <- pns2013br[,region := "Brasil"]
pns2013df <- list(pns2013df,pns2013br) %>% data.table::rbindlist()
pns2013df <- pns2013df[!is.na(C009) & !is.na(region),]

order_region <- unique(pns2013df$region)
pns2013df$region <- factor(pns2013df$region,order_region)

avg <- pns2013df[,lapply(.SD,mean),by = region,.SDcols = "V1"]
lista <- list(c("Sul","Sudeste","Centro-Oeste"),c("Norte","Nordeste","Brasil"))
pp <- list()
for(i in 1:2){ # i = 1 
  aux_pns <- pns2013df[region %in% lista[[i]],]
  aux_avg <- avg[region %in% lista[[i]],]
  
  pp[[i]] <- ggplot(data = aux_pns,
                    aes(y = factor(C009), 
                        x = 100 * (V1))) + 
    geom_bar(fill = "lightblue",
             color="black",width = 0.75,size=0.1,
             stat = "identity", 
             position = position_stack(reverse = TRUE)) +
    scale_fill_brewer(palette = "Pastel1",
                      direction = +1)  +
    labs(fill = "Grau de escolaridade") + 
    facet_grid(cols = vars(region)) +
    geom_vline(data = aux_avg, 
               aes(xintercept = 100 * V1),
               linetype = "dashed",
               colour = "black", alpha = 0.75) +
    geom_text(data = aux_avg,
              aes(x = 100 * V1, y = 1.25, 
                  label= (paste0("Média =\n"," ",round(100 * V1,1),"%"))),
              vjust = 0, hjust = -0.1, colour = "black",size = 3.5) + 
    ylab(NULL) +
    xlab(NULL) + 
    xlim(100 * c(0,max(pns2013df$V1))) + 
    guides(fill=guide_legend(ncol=2)) + 
    theme( legend.position = "none",
           axis.text.x = element_text(size = rel(1.25)),
           axis.text.y = element_text(size = rel(1.25)),
           panel.background = element_rect(fill = "white",colour = NA),
           panel.border = element_rect(fill = NA, colour = "grey20"),
           panel.grid = element_line(colour = "grey82",size = rel(0.35)), 
           panel.grid.minor = element_line(size = rel(0.35)), 
           strip.background = element_rect(fill = "grey82",colour = "grey20"),
           legend.key = element_rect(fill = "white", colour = NA))
  
  if(i == 2){
    pp[[i]] <- pp[[i]] + 
      xlab("Frequência(%) de pessoas residentes na zona urbana que se deslocam
      a pé ou de bicicleta para o trabalho por no mínimo 30 minutos, por região")
  }
}
pf <- pp[[1]] / pp[[2]]
pf
ggsave(pf,filename = "figures/PNS/FIG_income_region_race30.png",width = 27,
       height = 15,scale = 0.8,units = "cm")
# keep only initial files
lista <- ls()[ls() %nin% c(ls_initial_list,"ls_initial_list")]
rm(list = lista)

#
# plot - quartile / metro / active commute rate 30 ---------
#

pns2013df <- pns2013[urban %in% "Urbano",]
pns2013df <- pns2013df[,sum(actv_commutetime30)/.N,by = .(metro,quartileMetro)]
pns2013df <- pns2013df[!is.na(metro) & !is.na(quartileMetro),]
orderuf <- pns2013df[,sum(V1),by = metro][order(V1),]$metro
orderuf
pns2013df$metro <- factor(pns2013df$metro,orderuf)
varlabel <- c(`1` = "1º quartil",
              `2` = "2º quartil",
              `3` = "3º quartil",
              `4` = "4º quartil")

ggplot(data = pns2013df,
       aes(y = factor(metro), 
           x = 100 * (V1),
           fill = factor(quartileMetro))) + 
  geom_point(shape=21, color="black", size = 6)+
  scale_fill_brewer(labels = c("1º quartil",
                               "2º quartil",
                               "3º quartil",
                               "4º quartil"),
                    palette = "BrBG",direction = +1) +  
  labs(fill = "Quartis de renda") + 
  ylab(NULL) + 
  xlab("Frequência(%) de pessoas que se deslocam a pé ou de 
       bicicleta para o trabalho por no mínimo 30 minutos, por 
       Região Metropolitana") + 
  guides(color=guide_legend(ncol=2)) + 
  theme( legend.position = "bottom",
         axis.text.x = element_text(size = rel(1)),
         panel.background = element_rect(fill = "white",colour = NA),
         panel.border = element_rect(fill = NA, colour = "grey20"),
         panel.grid = element_line(colour = "grey82",size = rel(0.35)), 
         panel.grid.minor = element_line(size = rel(0.35)), 
         strip.background = element_rect(fill = "grey82",colour = "grey20"),
         legend.key = element_rect(fill = "white", colour = NA))

ggsave(filename = "figures/PNS/FIG_metro_quartil_actv_commutetime30.png",
       width = 20, height = 15,scale = 0.9, units = 'cm')
# keep only initial files
lista <- ls()[ls() %nin% c(ls_initial_list,"ls_initial_list")]
rm(list = lista)

#
# plot - travel tim / metropolitan region / educational level----------
#

pns2013df <- pns2013[urban %in% "Urbano",][actv_commutetime > 0,]
pns2013df <- pns2013df[,.(edugroup,actv_commutetime,metro)]
pns2013df <- pns2013df[!is.na(edugroup) & !is.na(actv_commutetime) 
                       & !is.na(metro),]
pns2013df <- pns2013df[,lapply(.SD,mean),
                       .SDcols = 'actv_commutetime', by = .(metro,edugroup)]

orderuf <- pns2013df[,lapply(.SD,mean),.SDcols = 'actv_commutetime', by = metro]
orderuf <- orderuf[order(actv_commutetime, na.last=FALSE),metro]

pns2013df$metro <- factor(pns2013df$metro,orderuf)
pns2013df$edugroup <- factor(pns2013df$edugroup,
                             c('Sem instrução + Fundamental incompleto',
                               'Fundamental completo',
                               'Médio completo',
                               'Superior completo'))
to_string <- as_labeller(c(`Sem instrução + Fundamental incompleto` = "Sem instrução + \n Fundamental \n incompleto",
                           `Fundamental completo` = "Fundamental \n completo",
                           `Médio completo` = "Médio \n completo",
                           `Superior completo` = "Superior \n completo"))

ggplot(data = pns2013df,
       aes(y = factor(metro), 
           x = actv_commutetime)) + 
  geom_point(aes(fill = (edugroup)),
             shape=21,  size = 6, alpha = 1) + 
  scale_fill_brewer(palette = "PuOr",
                    direction = +1)  +
  labs(fill = NULL) + 
  scale_x_continuous(breaks = seq(0,60,by = 10),
                     limits = c(10,60)) + 
  xlab("Tempo de viagem (em minutos) a pé ou de bicicleta da 
  população de zonas urbanas, conforme nível de escolaridade") + 
  ylab(NULL) +
  guides(fill=guide_legend(ncol=2)) + 
  theme( legend.position = "bottom",
         axis.text.y = element_text(size = rel(1.25)),
         axis.text.x = element_text(size = rel(1.25)),
         panel.background = element_rect(fill = "white",colour = NA),
         panel.border = element_rect(fill = NA, colour = "grey20"),
         panel.grid = element_line(colour = "grey82",size = rel(0.35)), 
         panel.grid.minor = element_line(size = rel(0.35)), 
         strip.background = element_rect(fill = "grey82",colour = "grey20"),
         legend.key = element_rect(fill = "white", colour = NA),
         legend.text = element_text(size = rel(1.05))) 

ggsave(filename = "figures/PNS/FIG_urban_timetravel_edugroup_region_points.png",
       width = 20,height = 20,scale = 0.8,units = "cm")
# keep only initial files
lista <- ls()[ls() %nin% c(ls_initial_list,"ls_initial_list")]
rm(list = lista)