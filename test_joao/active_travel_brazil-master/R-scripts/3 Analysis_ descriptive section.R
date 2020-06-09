
# Analyze Active Travel in Brazil using data from PNS 2013 and PNAD2008 (suplemento de Saude)
# Script written by Rafael Pereira - urbandemographics.blogspot.com
# Aug 2016, Oxford UK.



##################### Set working directory -------------------------------------------------------
setwd("R:/Dropbox/github/active_travel_brazil")




##################### Load packages -------------------------------------------------------

source("./R-scripts/0 LoadPackages.R")

    



############ Load DATA Sets ############

# pns2013 <- readRDS("./data/pns2013.Rds")
# pnad2008 <- readRDS("./data/pnad2008.Rds")
# 
# pns2013dom <- readRDS("./data/pns2013dom.Rds")
# pnad2008dom <- readRDS("./data/pnad2008dom.Rds")



############ Load Survey Design Objects ############


load('./data/pnad08.18y.design.rda')
load('./data/pns13.18y.design.rda')

  



######################!!!!!! RESULTS   RESULTS  RESULTS ###################### 
######################!!!!!! RESULTS   RESULTS  RESULTS ###################### 
######################!!!!!! RESULTS   RESULTS  RESULTS ###################### 


# Create Basic Aesthetic stantard for the plots

  baseplot <- theme_minimal() +
    theme( 
      axis.text  = element_text(face="bold"),
      strip.text = element_text(size = 11, face ="bold"),
      legend.text = element_text(size = 11)
      )
  


# TABLE 1 Pop characteristics vs Proportion of Vehicle Ownership 2013 ------------


tab1_year <- svyby(~factor( dummyVehicle=="Yes") ,
                   ~quintileBR+year, svyciprop ,
                   design = pns13.18y.design ,
                   vartype="ci", level = 0.95) %>% setDT()


tab1_sex <- svyby(~factor( dummyVehicle=="Yes") ,
                  ~quintileBR+v0302, svyciprop ,
                  design = pns13.18y.design ,
                  vartype="ci", level = 0.95) %>% setDT()


tab1_edu <- svyby(~factor( dummyVehicle=="Yes" ) , 
                  ~quintileBR+edugroup, svyciprop , # edugroup v4745
                  design = pns13.18y.design ,
                  vartype="ci", level = 0.95) %>% setDT()


tab1_age <- svyby(~factor( dummyVehicle=="Yes") ,
                  ~quintileBR+AGE, svyciprop ,
                  design = pns13.18y.design ,
                  vartype="ci", level = 0.95) %>% setDT()


tab1_age <- svyby(~factor( dummyVehicle=="Yes") , # using svymean because svyciprop was generating value 0 for poor individuals at 65+ for unknown reason. svymean returns virtually same results
                   ~quintileBR+AGE, svymean,
                   design = pns13.18y.design ,
                   vartype="ci", level = 0.95 , na.rm=TRUE) %>% setDT()

  # small table editing
  tab1_age<- tab1_age[, c(1,2,4,6,8)]
  names(tab1_age) <- c("quintileBR", "AGE", "factor(dummyVehicle == \"Yes\")", "ci_l", "ci_u")


tab1_actvcommute <- svyby(~factor( dummyVehicle=="Yes" ) ,
                          ~quintileBR+P040, svyciprop ,
                          design = pns13.18y.design ,
                          vartype="ci", level = 0.95) %>% setDT()

beep() # beep alert


tab1_year$var <- "Year"
tab1_sex$var <- "Sex"
tab1_age$var <- "Age"
tab1_edu$var <- "Education"
tab1_actvcommute$var <- "Active Commute"


# get all subtables into a list
#tab1_list <- list(tab1_actvcommute, tab1_age, tab1_edu, tab1_sex, tab1_year)
tab1_list <- list(tab1_year, tab1_sex, tab1_edu, tab1_age, tab1_actvcommute)

# change name of sub-tables' columns so we can stack them up
tab1_list <- lapply(tab1_list, setNames, c("quintileBR", "value","Proportion", "ci_l", "ci_u", "var"))

# stack sub-tables from a list into a single data.frame
tab1 <- rbindlist(tab1_list)



# Reorder values and labels
  tab1$value <- reorder(tab1$value, tab1$Proportion) # reorder factor levels by poportion values


# add labels to income variable
tab1[, quintileBR := factor(quintileBR, levels=c(1:5), labels = c("Q1 (poorest)","Q2","Q3","Q4","Q5 (richest)"))]


# save table 1
  fwrite(tab1, "./output_tables/tab1.csv")





####### FIGURE 1 ####### --------------------------------------

tab1 <- fread("./output_tables/tab1.csv")

# edit text breaks for plot
  tab1[ value== "Uneducated + Incomplete primary school", value := "Uneducated + Incomplete\nprimary school"]
  tab1[ value== "Complete high school", value := "Complete\nhigh school"]
  tab1[ value== "Complete primary school", value := "Complete\nprimary school"]


  
  
# change order or plot facets
  tab1[, var := factor(var, levels=c( "Year", "Sex", "Education", "Age", "Active Commute"))]
  
  tab1[, value := factor(value, levels=c("2013", "Women",  "Men"
                                         , "18-24", "25-34", "35-44", "45-54", "55-64", "65+"
                                         , "Uneducated + Incomplete\nprimary school", "Complete\nprimary school", "Complete\nhigh school", "University degree"
                                         , "No", "Yes, part of the journey", "Yes, all the journey"))]






plot1a <- 
  ggplot(data=subset(tab1, var %in% c("Year", "Sex")) ) +
  geom_point( aes(x=value, y=Proportion, color=quintileBR, fill=quintileBR), size=3, shape=21 ) +
  geom_errorbar( aes(x=value, y=Proportion, ymin=ci_u, ymax=ci_l, color=quintileBR),  size=0.5, width = 0) + 
  facet_grid( .~var , scales = "free_x", space="free") + 
  baseplot + 
  scale_colour_grey( start = 0, end = 0.8, guide = guide_legend(title = NULL)) +
  scale_fill_grey( start = 0, end = 0.8, guide = guide_legend(title = NULL)) +
  # colored chart         
  # scale_colour_brewer(palette = "Greys") + scale_fill_brewer(palette = "Greys")
  # scale_fill_viridis(discrete=T) +  scale_color_viridis(discrete=T)
  # scale_fill_manual(values = wes_palette("Royal1"), guide = guide_legend(title = NULL)) +
  # scale_color_manual(values = wes_palette("Royal1"), guide = guide_legend(title = NULL)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme(axis.title.x  = element_blank()) +
  theme(legend.position = "top") 

plot1b <- 
  ggplot(data=subset(tab1, var %in% c("Age")) ) +
  geom_point( aes(x=value, y=Proportion, color=quintileBR, fill=quintileBR), size=3, shape=21 ) +
  geom_errorbar( aes(x=value, y=Proportion, ymin=ci_u, ymax=ci_l, color=quintileBR),  size=0.5, width = 0) + 
  facet_grid( .~var , scales = "free_x", space="free") + 
  baseplot + 
  scale_colour_grey( start = 0, end = 0.8, guide = guide_legend(title = NULL)) +
  scale_fill_grey( start = 0, end = 0.8, guide = guide_legend(title = NULL)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme(axis.title.x  = element_blank()) +
  theme(axis.title.y  = element_blank()) +
  theme(legend.position = "top") 


plot1c <- 
  ggplot(data=subset(tab1, var %in% c("Education")) ) +
  geom_point( aes(x=value, y=Proportion, color=quintileBR, fill=quintileBR), size=3, shape=21 ) +
  geom_errorbar( aes(x=value, y=Proportion, ymin=ci_u, ymax=ci_l, color=quintileBR),  size=0.5, width = 0) + 
  facet_grid( .~var , scales = "free_x", space="free") + 
  baseplot + 
  scale_colour_grey( start = 0, end = 0.8, guide = guide_legend(title = NULL)) +
  scale_fill_grey( start = 0, end = 0.8, guide = guide_legend(title = NULL)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme(axis.title.x  = element_blank()) +
  theme(legend.position = "top") +
  theme(legend.position="none")


plot1d <- 
  ggplot(data=subset(tab1, var %in% c("Active Commute")) ) +
  geom_point( aes(x=value, y=Proportion, color=quintileBR, fill=quintileBR), size=3, shape=21 ) +
  geom_errorbar( aes(x=value, y=Proportion, ymin=ci_u, ymax=ci_l, color=quintileBR),  size=0.5, width = 0) + 
  facet_grid( .~var , scales = "free_x", space="free") + 
  baseplot + 
  scale_colour_grey( start = 0, end = 0.8, guide = guide_legend(title = NULL)) +
  scale_fill_grey( start = 0, end = 0.8, guide = guide_legend(title = NULL)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme(axis.title.x  = element_blank()) +
  theme(axis.title.y  = element_blank()) +
  theme(legend.position = "top")  +
  theme(legend.position="none")



# Arrange sub-plots  adding two empty plots to increasing the space between plots
  fig1 <-   plot_grid( plot1a + theme(legend.position="none"),
                       plot1b + theme(legend.position="none"),
                       NULL, NULL,
                       plot1c + theme(legend.position="none"),
                       plot1d + theme(legend.position="none"),
                       align = 'vh',
                       labels=c('A', 'B','','','C',"D"), 
                       hjust = -1,
                       nrow = 3, rel_heights = c(1, 0.1, 1))
  
  
  # create separate legend
  legend_1a <- get_legend(plot1a + theme(legend.position="top", legend.key.width=unit(3,"line")))

  
  # add the legend on top of plot the row we made earlier. Give it 11% of the height of the plot
  fig1 <- plot_grid( legend_1a, fig1, ncol = 1, rel_heights = c(.11, 1))
  
  
# save plot
  ggsave(fig1, file="./plots/fig1.png", dpi = 800,
         width = 35, height = 20, units = "cm")




  


_________________________________________________________________________________
###### TABLE 2- Pop characteristics- vehicle 2013 vs 2008 ------------------------




tab2_pns_total <- svyby(~ dummyVehicle=="Yes" ,
                         ~quintileBR+year, svymean ,
                         design = pns13.18y.design ,
                         vartype="ci", level = 0.95, na.rm=T) %>% setDT()

tab2_pns_urban <- svyby(~ dummyVehicle=="Yes" ,
                         ~quintileUrban+urban+year, svymean ,
                         design = pns13.18y.design ,
                         vartype="ci", level = 0.95, na.rm=T) %>% setDT()


tab2_pns_region <- svyby(~ dummyVehicle=="Yes" ,
                          ~quintileRegion+region+year, svymean ,
                          design = pns13.18y.design ,
                          vartype="ci", level = 0.95, na.rm=T) %>% setDT()

tab2_pns_metro <- svyby(~ dummyVehicle=="Yes" ,
                         ~quintileMetro+metro+year, svymean ,
                         design = pns13.18y.design ,
                         vartype="ci", level = 0.95, na.rm=T) %>% setDT()


# PNAD subtables
tab2_pnad_total <- svyby(~ dummyVehicle=="Yes" ,
                        ~quintileBR+year, svymean ,
                        design = pnad08.18y.design ,
                        vartype="ci", level = 0.95, na.rm=T) %>% setDT()

tab2_pnad_urban <- svyby(~ dummyVehicle=="Yes" ,
                         ~quintileUrban+urban+year, svymean ,
                         design = pnad08.18y.design ,
                         vartype="ci", level = 0.95, na.rm=T) %>% setDT()


tab2_pnad_region <- svyby(~ dummyVehicle=="Yes" ,
                         ~quintileRegion+region+year, svymean ,
                         design = pnad08.18y.design ,
                         vartype="ci", level = 0.95, na.rm=T) %>% setDT()

tab2_pnad_metro <- svyby(~ dummyVehicle=="Yes" ,
                        ~quintileMetro+metro+year, svymean ,
                        design = pnad08.18y.design ,
                        vartype="ci", level = 0.95, na.rm=T) %>% setDT()


tab2_pns_total$var <- "Total"
tab2_pns_urban$var <- "Urban"
tab2_pns_region$var <- "Region"
tab2_pns_metro$var <- "Metropolitan Area"
tab2_pnad_total$var <- "Total"
tab2_pnad_urban$var <- "Urban"
tab2_pnad_region$var <- "Region"
tab2_pnad_metro$var <- "Metropolitan Area"
beep() # beep alert


# organize total subtables
  tab2_total <- rbind(tab2_pns_total, tab2_pnad_total)
  tab2_total <- tab2_total[, c(1,2,4,6,8,9), with=FALSE]
  colnames(tab2_total) <- c("quintileBR", "year", "Proportion", "ci_l", "ci_u" , "var")
  tab2_total$value <- "Brazil"
  setcolorder(tab2_total, neworder = c("quintileBR", "value", "year", "Proportion", "ci_l", "ci_u", "var") )

# get all subtables into a list
tab2_list <- list(tab2_pns_urban, tab2_pns_region, tab2_pns_metro, tab2_pnad_urban, tab2_pnad_region, tab2_pnad_metro)

#keep selected columns
tab2_list <- lapply(tab2_list, function(x) { x <- x[, c(1,2,3,5,7,9,10), with=FALSE] } )

# change name of sub-tables' columns so we can stack them up
tab2_list <- lapply(tab2_list, setNames, c("quintileBR", "value", "year","Proportion", "ci_l", "ci_u", "var"))

# stack all sub-tables into a single data.frame
tab2 <- rbindlist(tab2_list)
tab2 <- rbind(tab2, tab2_total)




# add labels to income variable
tab2[, quintileBR := factor(quintileBR, levels=c(1:5), labels = c("Q1 (poorest)","Q2","Q3","Q4","Q5 (richest)"))]



# save table 2
fwrite(tab2, "./output_tables/tab2.csv")





####### FIGURE 2 ####### --------------------------------------

tab2 <- fread("./output_tables/tab2.csv")


# edit text breaks for plot
  tab2[ value== "Non-metropolitan area", value := "Non-metropolitan\narea"]
  tab2[ value== "Belo Horizonte", value := "Belo\nHorizonte"]
  tab2[ value== "Porto Alegre", value := "Porto\nAlegre"]
  tab2[ value== "Federal District", value := "Federal\nDistrict"]
  tab2[ value== "Rio de Janeiro", value := "Rio\nde Janeiro"]
  tab2[ value== "Sao Paulo", value := "Sao\nPaulo"]
  
# Reorder values and labels
  tab2$value <- reorder(tab2$value, tab2$Proportion) # reorder factor levels by poportion values
  
# change order or plot facets
  tab2[, var := factor(var, levels=c( "Total", "Urban", "Region", "Metropolitan Area"))]
  tab2[, year := factor(year, levels=c( 2008, 2013))]






plot2a <- 
  ggplot(data=subset(tab2, var %in% c("Total", "Urban")) ) +
  geom_point( aes(x=as.factor(year), y=Proportion, color=quintileBR, fill=quintileBR), size=3, shape=21 ) +
  geom_errorbar( aes(x=as.factor(year), y=Proportion, ymin=ci_u, ymax=ci_l, color=quintileBR),  size=0.5, width = 0) + 
  facet_grid( .~var+value , scales = "free_x", space="free", labeller = label_bquote(cols = .(value) ))  +
  baseplot + 
  scale_colour_grey( start = 0, end = 0.8, guide = guide_legend(title = NULL)) +
  scale_fill_grey( start = 0, end = 0.8, guide = guide_legend(title = NULL)) +
  # colored chart         
  # scale_colour_brewer(palette = "Greys") + scale_fill_brewer(palette = "Greys")
  # scale_fill_viridis(discrete=T) +  scale_color_viridis(discrete=T)
  # scale_fill_manual(values = wes_palette("Royal1"), guide = guide_legend(title = NULL)) +
  # scale_color_manual(values = wes_palette("Royal1"), guide = guide_legend(title = NULL)) +
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  theme(axis.title.x  = element_blank()) +
  theme(legend.position = "none") +
  theme(plot.margin = unit(c(1,1,2,1), "cm")) 


plot2b <- 
  ggplot(data=subset(tab2, var %in% c("Region")) ) +
  geom_point( aes(x=as.factor(year), y=Proportion, color=quintileBR, fill=quintileBR), size=3, shape=21 ) +
  geom_errorbar( aes(x=as.factor(year), y=Proportion, ymin=ci_u, ymax=ci_l, color=quintileBR),  size=0.5, width = 0) + 
  facet_grid( .~var+value , scales = "free_y", space="free", labeller = label_bquote(cols = .(value) ))  +
  baseplot + 
  scale_colour_grey( start = 0, end = 0.8, guide = guide_legend(title = NULL)) +
  scale_fill_grey( start = 0, end = 0.8, guide = guide_legend(title = NULL)) +
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  theme(axis.title.x  = element_blank()) +
  theme(axis.title.y  = element_blank()) +
  theme(legend.position = "none") +
  theme(plot.margin = unit(c(1,1,2,1), "cm")) 


plot2c <- 
  ggplot(data=subset(tab2, var %in% c("Metropolitan Area")) ) +
  geom_point( aes(x=as.factor(year), y=Proportion, color=quintileBR, fill=quintileBR), size=3, shape=21 ) +
  geom_errorbar( aes(x=as.factor(year), y=Proportion, ymin=ci_u, ymax=ci_l, color=quintileBR),  size=0.5, width = 0) + 
  facet_grid( .~var+value , scales = "free_y", space="free", labeller = label_bquote(cols = .(value) ))  +
  baseplot + 
  scale_colour_grey( start = 0, end = 0.8, guide = guide_legend(title = NULL)) +
  scale_fill_grey( start = 0, end = 0.8, guide = guide_legend(title = NULL)) +
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  theme(axis.title.x  = element_blank()) +
  theme(legend.position = "top") +
  theme(plot.margin = unit(c(1,1,2,1), "cm")) +
  theme(legend.position="none")


plot2a <- ggdraw(plot2a) + 
  draw_plot_label("A", size = 14, hjust = -4) +  
  draw_label("Country",  x = 0.5, y = .075,
             size = 14, fontface = 'bold')


plot2b <- ggdraw(plot2b) + 
  draw_plot_label("B", size = 14, hjust = -4) + 
  draw_label("Region",  x = 0.5, y = .075,
             size = 14, fontface = 'bold')

plot2c <- ggdraw(plot2c) + 
  draw_plot_label("C", size = 14, hjust = -4) + 
  draw_label("Metropolitan Area",  x = 0.5, y = .075,
             size = 14, fontface = 'bold')



  
# Arrange sub-plots
  fig2 <-   plot_grid( arrangeGrob(plot2a ,
                                   plot2b , ncol = 2),
                                   plot2c , ncol = 1)

  

  # add the legend on top of plot the row we made earlier. Give it 11% of the height of the plot
  fig2 <- plot_grid( legend_1a, fig2, ncol = 1, rel_heights = c(.05, 1))
  
  
  
  
# save plot
  ggsave(fig2, file="./plots/fig2.png", dpi = 800,
         width = 42, height = 22, units = "cm")


