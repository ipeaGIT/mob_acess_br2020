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
unique(pns2013$region)
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
pns2013df[,actv_commutetime1 := ifelse(actv_commutetime > 0,1,0)]
pns2013df <- pns2013df[,list(actv = 100 * sum(actv_commutetime1)/.N)
                       ,by = .(AGE,v0302)]
pns2013df <- pns2013df[!is.na(AGE) & !is.na(v0302),]

ggplot(data = pns2013df,
       aes(x = (AGE), 
           y = actv,
           group = v0302, color = v0302)) + 
  geom_line() + 
  geom_point()
geom_line(aes(color = factor(v0302))) #+ 
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
gc(reset = T)

#
# plot - gender / decil /  rate of active commute 30-------------
#

pns2013df <- pns2013[,sum(actv_commutetime30)/.N,by = .(decileBR,v0302,uf,v0302)]
pns2013df <- pns2013df[,list(as.numeric(sd(V1)),as.numeric(median(V1))), by = .(decileBR)]
data.table::setnames(pns2013df,c('V1','V2'),c("stdv","avg"))
pns2013df
pns2013df <- pns2013df[!is.na(decileBR),]
pns2013df$decileBR <- factor(pns2013df$decileBR,1:10)

ggplot(data = pns2013df,aes(y = avg,x = factor(decileBR), group = 1)) + 
  geom_ribbon(aes(ymin = avg - stdv, ymax = avg + stdv), alpha=.3) + 
  geom_line() +
  theme_minimal()

geom_line()
geom_ribbon(aes(ymin = avg - stdv, ymax = avg + stdv), alpha=.3) +
  geom_line(aes(color=factor(v0302))) +
  theme_minimal()

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
# plot - quintil / active commute active 1 / region -------------
#

pns2013df <- pns2013[urban %in% "Urbano",]
pns2013df[,actv_commutetime1 := ifelse(actv_commutetime > 0,1,0)]
pns2013df <- pns2013df[,sum(actv_commutetime1)/.N,by = .(C009,region)]
pns2013df <- pns2013df[,.(C009,V1,region)]
# br 
pns2013br <- pns2013[urban %in% "Urbano",]
pns2013br[,actv_commutetime1 := ifelse(actv_commutetime > 0,1,0)]
pns2013br <- pns2013br[,sum(actv_commutetime1)/.N,by = .(C009)]
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
              vjust = 0, hjust = -0.1, colour = "black",size = 3.0) + 
    ylab(NULL) +
    xlab(NULL) + 
    xlim(100 * c(0,1.05*max(pns2013df$V1))) + 
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
      a pé ou de bicicleta para o trabalho, por região")
  }
}
pf <- pp[[1]] / pp[[2]]

ggsave(pf,filename = "figures/PNS/FIG_income_region_race01.png",width = 22,
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
lista <- list(c("Sul","Sudeste","Centro-Oeste"),
              c("Norte","Nordeste","Brasil"))
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
# plot - active commute rate 30 / quartile / metro ---------
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
       width = 20,height = 20,scale = 0.8,units = "cm")
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

#
# plot - quintil / travel tim / active prop / metropolitan region----------
#

pns2013df <- pns2013[urban %in% "Urbano",]
pns2013df[,actv_commutetime1 := ifelse(actv_commutetime > 0,1,0)]
pns2013df[,size := .N,by = .(quintileMetro,metro)]
pns2013df <- pns2013df[actv_commutetime > 0,]
pns2013df <- pns2013df[,list(time = mean(actv_commutetime),
                             prop = 100 * sum(actv_commutetime1)/size),by = .(quintileMetro,metro)]
pns2013df <- pns2013df[,.(quintileMetro,prop,time,metro)]
pns2013df <- pns2013df[!is.na(quintileMetro) & !is.na(prop) & !is.na(time) 
                       & !is.na(metro),]

pns2013df <- data.table::melt(data = pns2013df,
                               id.vars = c('quintileMetro','metro'),
                               measure.vars =  c('time','prop'),
                               variable.names = "type")
# order metro
orderuf <- pns2013df[variable %in% 'time',lapply(.SD,min),.SDcols = 'value', by = metro]
orderuf <- orderuf[order(value, na.last=FALSE),metro]
# orderuf <- pns2013df2[metro %in% orderuf,metro]

pns2013df$metro <- factor(pns2013df$metro,orderuf)
label_plot <- as_labeller(c(`time` = "Tempo (minutos)",
                          `prop` = "Proporção de deslocamentos (%)"))
ggplot(data = pns2013df,
             aes(y = factor(metro), 
                 x = value)) + 
  geom_line(linetype = "dotted") + 
  geom_point(aes(fill = factor(quintileMetro)),
             shape=21,  size = 4.0, alpha = 1) + 
  scale_fill_brewer(palette = "RdBu",
                    direction = +1,
                    labels = c("1º","2º","3º","4º","5º"))  +
  facet_grid(cols = vars(variable),scales = "free",labeller = label_plot) + 
  labs(
    x = NULL, y = NULL, fill = "Quintil de renda",
    title = "Modos ativos de deslocamento",
    subtitle = "Média do tempo de viagem (em minutos) e proporição de deslocamentos \na pé ou por bicicleta nas zonas urbanas conforme quintil de renda"
  ) + 
  guides(fill=guide_legend(nrow=1)) + 
  theme_bw()  +  
  theme( plot.title = element_text(size = rel(0.95)),
         plot.subtitle = element_text(size = rel(0.75), hjust = 0),
         panel.border = element_rect(fill = NA, colour = "grey20"),
         legend.position = "bottom")

ggsave(filename = "figures/PNS/FIG_urban_timetravel_quintil_region_points.png",
       width = 20,height = 15,scale = 0.8,units = "cm")
# keep only initial files
lista <- ls()[ls() %nin% c(ls_initial_list,"ls_initial_list")]
rm(list = lista)
gc(reset = T)

#
# plot - quintil / active commute time 1 / metropolitan region----------
#

pns2013df <- pns2013[urban %in% "Urbano",]
pns2013df[,actv_commutetime1 := ifelse(actv_commutetime > 0,1,0)]
pns2013df <- pns2013df[,list(actv = 100 * sum(actv_commutetime1)/.N),by = .(quintileMetro,metro)]
# remove NA's
pns2013df <- pns2013df[!is.na(quintileMetro) & !is.na(actv) 
                       & !is.na(metro),]
# order metro
orderuf <- pns2013df[,lapply(.SD,min),.SDcols = 'actv', by = metro]
orderuf <- orderuf[order(actv, na.last=FALSE),metro]

pns2013df$metro <- factor(pns2013df$metro,orderuf)

ggplot(data = pns2013df,
       aes(y = factor(metro), 
           x = actv)) + 
  geom_line(linetype = "dotted") + 
  geom_point(aes(fill = factor(quintileMetro)),
             shape=21,  size = 4.0, alpha = 1) + 
  scale_fill_brewer(palette = "RdBu",
                    direction = +1,
                    labels = c("1º","2º","3º","4º","5º"))  +
  scale_x_continuous(breaks = seq(5,35,by = 5),
                     limits = c(5,35)) + 
  labs(
    x = "Proporção (%)", y = NULL, fill = "Quintil de renda",
    title = "Deslocamento por modos ativos",
    subtitle = "Percentual de pessoas que se deslocam a pé ou por bicicleta nas zonas urbanas \nconforme quintil de renda"
  ) + 
  guides(fill=guide_legend(nrow=1)) + 
  theme_bw()  +  
  theme( plot.title = element_text(size = rel(0.95)),
         plot.subtitle = element_text(size = rel(0.75), hjust = 0),
         panel.border = element_rect(fill = NA, colour = "grey20"),
         legend.position = "bottom")

ggsave(filename = "figures/PNS/FIG_urban_percenttravel_quintil_region_points.png",
       width = 20,height = 15,scale = 0.8,units = "cm")
# keep only initial files
lista <- ls()[ls() %nin% c(ls_initial_list,"ls_initial_list")]
rm(list = lista)
gc(reset = T)

#
# plot - etnia / genero / edugroup / tempo viagem----------
#

pns2013df <- pns2013[urban %in% "Urbano",][actv_commutetime > 0,]
#pns2013df[,metro := region]
pns2013df[C009 %in% c("Preta","Parda"),C009 := "Negra"]
pns2013df <- pns2013df[C009 %in% c("Negra","Branca"),]
pns2013df <- pns2013df[,.(C009,actv_commutetime,metro,v0302,edugroup)]
pns2013df <- pns2013df[!is.na(C009) & !is.na(actv_commutetime) 
                       & !is.na(metro) & !is.na(v0302) & !is.na(edugroup),]
pns2013df <- pns2013df[,lapply(.SD,mean),
                       .SDcols = 'actv_commutetime', by = .(metro,C009,v0302,edugroup)]
# brasil urbano
pns2013df <- pns2013df[metro %in% "Restante das UF",metro := "Brasil Não-Metropolitano"]
# etnia
pns2013df <- pns2013df[v0302 %in% "Feminino",v0302 := "Mulher"]
pns2013df <- pns2013df[v0302 %in% "Masculino",v0302 := "Homem"]
pns2013df[v0302 == "Homem",C009 := fifelse(C009 == "Branca","Branco","Negro")]
pns2013df[v0302 == "Mulher",C009 := fifelse(C009 == "Branca","Branca","Negra")]
pns2013df <- pns2013df[,genero_etnia := paste(v0302,C009)]
# order edugroup
pns2013df[edugroup %in% "Sem instrução + Fundamental incompleto",
          edugroup := "Sem instrução"]
eduvector <- c("Sem instrução",
               "Fundamental completo","Médio completo","Superior completo")
pns2013df$edugroup <- factor(pns2013df$edugroup,eduvector)
# order metro
orderuf <-  pns2013df[,lapply(.SD,min),.SDcols = 'actv_commutetime', by = metro][,metro]
pns2013df[,metro := factor(metro,orderuf)]
# create group
pns2013df[,grp_metro := .GRP, by = metro][,grp_metro := as.numeric(grp_metro)*3]
pns2013df[v0302 %in% "Homem",grp_metro := grp_metro + 0.2]
pns2013df[v0302 %in% "Mulher",grp_metro := grp_metro - 0.2]

vec <- (1:length(unique(pns2013df$metro)))*3

ggplot(data = pns2013df,
       aes(x = actv_commutetime, 
           y = grp_metro,
           group = grp_metro)) + 
  geom_path(color = "black", size = 0.5,linetype = "dotted") +
  geom_point(aes(fill = genero_etnia), size = 3.0,shape = 21) +
  scale_y_continuous(breaks = vec,
                     labels = orderuf) +
  scale_fill_manual(values=c("#a8dadc","#457b9d","#FF7070","#DE030F")) +
  facet_grid(cols = vars(edugroup)) + 
  guides(fill=guide_legend(ncol=2)) + 
  labs(fill = NULL,x = "Minutos", y = NULL,
       title = "Tempo de viagem por modos ativos de deslocamento",
       subtitle = "Média do tempo de viagem (em minutos) a pé ou por bicicleta nas zonas urbanas\nClassificação por sexo, etnia e nível de escolaridade"
  ) + 
  theme_minimal()  +  
  theme( plot.title = element_text(size = rel(0.95), hjust = 0),
         plot.subtitle = element_text(size = rel(0.75), hjust = 0),
         legend.position = "bottom",
         panel.background = element_rect(fill = "white", 
                                         colour = NA),
         panel.border = element_rect(fill = NA,
                                     colour = "grey20"),
         panel.grid = element_line(colour = "grey92"),
         panel.grid.minor = element_line(size = rel(0.5)),
         strip.background = element_rect(fill = "grey85",
                                         colour = "grey20"),
         legend.key = element_rect(fill = "white",
                                   colour = NA), 
         complete = TRUE) 

ggsave(filename = "figures/PNS/FIG_urban1_timetravel_race_edu_gender_region_points.png",
       width = 30,height = 17,scale = 0.8,units = "cm")
# keep only initial files
lista <- ls()[ls() %nin% c(ls_initial_list,"ls_initial_list")]
rm(list = lista)

#
# plot - etnia / genero / edugroup / actv1 ----------
#

pns2013df <- pns2013[urban %in% "Urbano",]
pns2013df[,actv_commutetime1 := data.table::fifelse(actv_commutetime > 0,1,0)]
pns2013df[C009 %in% c("Preta","Parda"),C009 := "Negra"]
pns2013df <- pns2013df[C009 %in% c("Negra","Branca"),]

pns2013df <- pns2013df[,list(actv = 100 * sum(actv_commutetime1)/.N),by = .(C009,metro,v0302,edugroup)]

pns2013df <- pns2013df[!is.na(C009) & !is.na(actv) 
                       & !is.na(metro) & !is.na(v0302) & !is.na(edugroup),]
# brasil urbano
pns2013df <- pns2013df[metro %in% "Restante das UF",metro := "Brasil Não-Metropolitano"]
pns2013df[v0302 %in% "Feminino",v0302 := "Mulher"]
pns2013df[v0302 %in% "Masculino",v0302 := "Homem"]
pns2013df[v0302 %in% 'Homem',C009 := fifelse(C009 %in% 'Branca',"Branco","Negro")]
pns2013df[v0302 %in% 'Mulher',C009 := fifelse(C009 %in% 'Branca',"Branca","Negra")]
pns2013df <- pns2013df[,genero_etnia := paste(v0302,C009)]
# order edugroup
pns2013df[edugroup %in% "Sem instrução + Fundamental incompleto",
          edugroup := "Sem instrução"]
eduvector <- c("Sem instrução",
               "Fundamental completo","Médio completo","Superior completo")
pns2013df$edugroup <- factor(pns2013df$edugroup,eduvector)
# order metro
orderuf <-  pns2013df[edugroup %in% "Sem instrução",lapply(.SD,min),.SDcols = 'actv', by = metro][,metro]
pns2013df[,metro := factor(metro,orderuf)]
# create group
pns2013df[,grp_metro := .GRP, by = metro][,grp_metro := as.numeric(grp_metro)*3]
pns2013df[v0302 %in% "Homem",grp_metro := grp_metro + 0.2]
pns2013df[v0302 %in% "Mulher",grp_metro := grp_metro - 0.2]

vec <- (1:length(unique(pns2013df$metro)))*3

ggplot(data = pns2013df,
       aes(x = actv, 
           y = grp_metro,
           group = grp_metro)) + 
  geom_path(color = "black", size = 0.5,linetype = "dotted") +
  geom_point(aes(fill = genero_etnia), size = 3.0,shape = 21) +
  scale_y_continuous(breaks = vec,
                     labels = orderuf) +
  scale_fill_manual(values=c("#a8dadc","#457b9d","#FF7070","#DE030F")) +
  facet_grid(cols = vars(edugroup), scales = "free") + 
  guides(fill=guide_legend(ncol=2)) + 
  labs(fill = NULL,x = "Proporção (%)", y = NULL,
       title = "Deslocamento por modos ativos",
       subtitle = "Percentual de pessoas que se deslocam a pé ou por bicicleta nas zonas urbanas \nClassificação por sexo, etnia e escolaridade"
  ) + 
  theme_minimal()  +  
  theme( plot.title = element_text(size = rel(0.95), hjust = 0),
         plot.subtitle = element_text(size = rel(0.75), hjust = 0),
         legend.position = "bottom",
         panel.background = element_rect(fill = "white", 
                                         colour = NA),
         panel.border = element_rect(fill = NA,
                                     colour = "grey20"),
         panel.grid = element_line(colour = "grey92"),
         panel.grid.minor = element_line(size = rel(0.5)),
         strip.background = element_rect(fill = "grey85",
                                         colour = "grey20"),
         legend.key = element_rect(fill = "white",
                                   colour = NA), 
         complete = TRUE) 

ggsave(filename = "figures/PNS/FIG_urban_pcernttravel_race_gender_region_points.png",
       width = 30,height = 17,scale = 0.8,units = "cm")
# keep only initial files
lista <- ls()[ls() %nin% c(ls_initial_list,"ls_initial_list")]
rm(list = lista)
