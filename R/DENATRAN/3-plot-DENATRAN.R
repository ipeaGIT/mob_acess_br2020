#
# initial config----------------
#

rm(list=ls())
gc(reset = T)
library(XLConnect)
source("R/PNS/0_loadpackages.R",local = TRUE)
`%nin%` = Negate(`%in%`)
ls_initial_list <- ls()

denatran <- readr::read_rds("data/DENATRAN/DENATRAN_dez.rds")

#
# check names
#

unique(denatran$UF)
unique(denatran$ANO)
unique(denatran$name_metro)
unique(denatran$name_metro_pns)
#
# metro region PNS ------
#

temp_den <- denatran[!is.na(name_metro_pns),]
unique(temp_den$name_metro_pns)
temp_den <- temp_den[,list(TOTAL_AUTOS = sum(TOTAL_AUTOS),
                           TOTAL_MOTOS = sum(TOTAL_MOTOS)),
                     by = .(name_metro_pns,ANO)]
temp_den <- data.table(
  name_metro_pns = rep(temp_den$name_metro_pns,2),
  ANO = rep(temp_den$ANO,2),
  TOTAL = c(temp_den$TOTAL_AUTOS,temp_den$TOTAL_MOTOS),
  TYPE = rep(c("Automóveis","Motocicletas"),each= nrow(temp_den))
)
ggplot(temp_den,aes(x = ANO,y = TOTAL,fill = TYPE)) +
  geom_area(colour="black", size=.3, alpha=.8) +
  scale_fill_brewer(palette="Blues")+
  facet_wrap(~name_metro_pns,scales = "free")
  scale_x_continuous(breaks=seq(1,126,length.out = 12),
                     labels=data[seq(1,126,length.out = 12)])+
  scale_y_continuous(breaks=seq(0,100,length.out = 5),limits=c(0,100))+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))+ 
  labs(x="Data",y=expression(10^{3} ~"ve?culos"),
       fill="Ve?culos")
  




