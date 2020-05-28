





# Lap top
setwd("R:/Dropbox/bases_de_dados/Pnad")


#### Libraries ------------------------
# https://github.com/lucasmation/microdadosBrasil
# devtools::install_github("lucasmation/microdadosBrasil")
library(microdadosBrasil)
library(magrittr)
library(data.table)
library(dplyr)
library(ggplot2)
library(Hmisc)

# options
options(digits=4)   # number of digits to show
options(scipen=999) # disable scientific notation



#### download

years_to_download <- c(1998, 2001:2008)

downlonad_pnad <- function(years_to_download){cat('downloading', years_to_download," \n"); download_sourceData("PNAD", years_to_download, unzip = T)}


lapply(years_to_download, downlonad_pnad)





# download single year
# download_sourceData("PNAD", 2013, unzip = T)





#### define variables to read ------------------------

pes_var <- c(
    "UF" # Estado 33.Rio
  , "V0102"	#	Número de controle
  , "V0103"	#	Número de série
  , "V4742" # Rendimento mensal domiciliar per capita
  , "V9057" # Tempo de percurso diário de ida da residência para o local de trabalho
  , "V4727"	#	Código de área censitária	| 1.Região metropolitana \ 2.Autorrepresentativo \ 3.Não autorrepresentativo
  , "V4728"	#	Código de situação censitária | 1-3 urbano \ 4-8 rural
  , "V9054" # working in farms
  , "V9031" # no people working in the night shift
  , "V4729" #	Peso da pessoa
  )



  


dom_var <-  c(
                  "UF" # Estado 33.Rio
            , "V0102"	#	Número de controle
            , "V0103"	#	Número de série
            ,   "UPA"		# Delimitação do município
            , "V4617"	#	STRAT - Identificação de estrato de município auto-representativo e não auto-representativo
            , "V4618"	#	PSU - Unidade primária de amostragem
            )




# empty output table with mean commute time by decile and year
table_decile <- data.frame(year=as.numeric(0), decileBR=as.numeric(0), ttime=as.numeric(0))


# empty output table with mean commute time by decile and year
table_total <- data.frame(year=as.numeric(), ttime=as.numeric())



########


# my_year = 2015

for (my_year in c(2001:2009,2011:2015)){


#### read data  ------------------------

#df_dom <- read_PNAD("domicilios", i = my_year, vars_subset =  dom_var ) 
df_pes <- read_PNAD("pessoas", i = my_year, vars_subset =  pes_var )



#### Subset data  ------------------------

# all columns to numeric
  df_pes <- setDT(df_pes)[, lapply(.SD, as.numeric) ]



# FILTERs
  df_pes_sub <- subset(df_pes, UF == 33) # Keep only State of Rio
#  df_pes_sub <- subset(df_pes_sub, V4742 != 999999999999) # remove obs with invalid income data, 
  df_pes_sub <- subset(df_pes_sub, V4728 %in% c(1, 2, 3)) # rural areas
  df_pes_sub <- subset(df_pes_sub, !(V9054 %in% 2))  # remove working in farms
  df_pes_sub <- subset(df_pes_sub, !(V9031 %in% 1)) # remove people working in the night shift
  df_pes_sub <- subset(df_pes_sub, V4727 == 1) # keep only Metropolitan area
  
  
    
  
  

#### merge data  ------------------------


  #df <- df_pes_sub[df_dom_sub , on=c("UF", "V0102", "V0103"), nomatch=0]
  df <- copy(df_pes_sub)
  head(df)



#### Recode variables  ------------------------

# year

df[, year := my_year ]


# # Income decile
#   summary(df$V4742)
#   
#   # find decile breakes
#   decile_br <- wtd.quantile(df$V4742, weights=df$V4729, probs=seq(0, 1, by=0.1), type='quantile', normwt=T, na.rm=TRUE)
#   
#   df[ , decileBR :=  cut( V4742 , decile_br, include.lowest= TRUE, labels=1:10, na.rm=T ) ]
#   
#   
  

# Commute time
  # v9057
  # 1	Até 30 minutos
  # 3	Mais de 30 até 1 hora
  # 5	Mais de 1 até 2 horas
  # 7	Mais de 2 horas
  # df[, V4729 := as.numeric(V4729)]
  # 
  df[, ttime := as.numeric(V9057)]
  df[, ttime := ifelse(ttime == 1, 15, 
                ifelse(ttime == 3, 45, 
                ifelse(ttime == 5, 90, 
                ifelse(ttime == 7, 120, NA)))) ] 
                                     
  
#### survey design  ------------------------

#### table  ------------------------
  
  
  # get mean ttime by year ande decile
 # temp_table <- df[, .(ttime=weighted.mean(x=ttime, w=V4729, na.rm = T)), by= .(year, decileBR) ]
  
  # add to output table
#  table_decile <- rbind(table_decile, temp_table)

  
# get mean ttime by year
  mean_ttime <- df[, .(ttime=weighted.mean(x=ttime, w=V4729, na.rm = T)), by=year ]

# add to output table
  table_total <- rbind(table_total, mean_ttime)

  
# Above 1h
  total_trips <- df[!is.na(V9057), .(total_trips= sum(V4729, na.rm = T)), by=year]
  above1h <- df[V9057>3, .(above1h=sum(V4729, na.rm = T)), by=year ]
  
  table_total <- left_join(table_total, above1h)
  table_total <- left_join(table_total, total_trips)
  
  setDT(table_total)[, above1h_p := (above1h / total_trips)* 100]
  
  }


rm( setdiff(ls(), c('table_decile', 'table_total')))


# Average time
ggplot() + geom_line(data=table_total, aes(x=year, y=ttime))


ggplot() + 
  geom_point(data=subset(table_decile, decileBR>0) , aes(y=ttime, x=factor(decileBR), color=factor(decileBR))) +
  geom_line(data=subset(table_decile, decileBR>0) , aes(y=ttime, x=factor(decileBR), group=year)) +
  facet_grid(~year)















df_rm <- subset(df, V4727 == 1)

mean_ttime <- df_rm[, .(ttime=weighted.mean(x=ttime, w=V4729, na.rm = T)), by=UF ]

df_rm[, .(ttime=weighted.mean(x=ttime, w=V4729, na.rm = T)) ]





