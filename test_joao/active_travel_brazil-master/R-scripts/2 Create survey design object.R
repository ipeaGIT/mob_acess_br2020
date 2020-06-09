# This script creates the Survey Design Object for the data analysis 
# Script written by Rafael Pereira - urbandemographics.blogspot.com
# Aug 2016, Oxford UK.
  


##################### Set working directory -------------------------------------------------------
setwd("R:/Dropbox/github/active_travel_brazil")




##################### Load packages -------------------------------------------------------
source("./R-scripts/0 LoadPackages.R")



############ Load DATA Sets ############

pns2013 <- readRDS("./data/pns2013.Rds")
pnad2008 <- readRDS("./data/pnad2008.Rds")

pns2013dom <- readRDS("./data/pns2013dom.Rds")
pnad2008dom <- readRDS("./data/pnad2008dom.Rds")



  # Keep only important variables
    #pnad2008 <- select(pnad2008, v0101,uf,v0102,v0103,v0403,v4729,v4609, v4732,v8005,v0302,v0404,v4803,v4838,v4728,v4727,v0602,v9005,v4805,v4706,v4810,v4720,v4721,v4721,v4745,v9054,v9055,v9056,v9057,v1409,v14091,v1410,v1411, v4602, v4618, v4617, upa, region, AGE,agegroup, urban, metro, DecileBR,Vcount, v1410mod,v1411mod,v9057mod,actv_commutetime30,pre_wgt)
    pns2013 <- select(pns2013, 
                V0031,   # pnad compat_ metropolitan area
                V0026,   # pnad compat_ urban
                v4745,   # pnad compat_ Educational attainment
                v0302,   # pnad compat_ Sex
                v4727,   # pnad compat_ Metropolitan area
                v1410,   # pnad compat_ Active Travel
                v2032,   # pnad compat_ Car or motorcycle ownership
                v4721,   # pnad Household income per capita
                V0015,   # type of interview
                V0001,   # state
                C006,    # sex
                C009,    # race
                C008,    # age
                VDD004,  # Educational attainment
                urban,   # urban x rural areas
                P040,    # Active commute
                P04101,  # Active commute time (hours)
                P04102,  # Active commute time (minutes)
                P04301, # active travel to habitual activities
                P04302, # active travel time to habitual activities
                P00101, # Weight
                P00401, # Height
                N001, # health perception
                O009, # car accident
                O011, # travel mode when injured
                O014, # accident hindered habitual activities
                O020, # any sequel and / or disability due to this traffic accident
                Q002, # Ever diagnosed with hypertension 
                Q003, # age at diagnosis for hypertension
                Q030, # Ever diagnosed with diabetes 
                Q031, # age at diagnosis for diabetes
                Q060, # Ever diagnosed with high cholesterol 
                Q061,  # age at diagnosis for high cholesterol
                
                V0025,   # person selected for long questionaire
                M001,    # Type of interview
                UPA_PNS, # UPA
                V0024,   # Strata
                V0029,   # person sample weight without calibratio
                V00291,  # person sample weight with calibration
                V00292,  # Population projection
                V00283,  # Dominio de pos-estrato 1
                V00293,  # Dominio de pos-estrato 2
                A01817,  # Motorcycle ownership
                A020,    # Car ownership (number of cars)
                v2032,   # Vehicle in the household, compatible with PNAD
                dummyVehicle,
                v1410, # dummy active travel , compatible with PNAD
                year, region,
                AGE, agegroup, edugroup, urban, metro, vcount, 
                actv_commutetime30, actv_commutetime,actv_traveltimehabacts,total_actvtraveltime, physicallyactive30 # commute variables
                )
    
    

    

                

############## 1. PNS Create survey design  -----------

  # There should be no Missings (NA) in Design Variables
  # Count  missing values (NAs)
  anyNA(pns2013$V00291)
  length(which(is.na(pns2013$V00291)))
  
  # Subset PNS with individuals who answered the detailed questionnaire only
  # This eliminates observations with missing values in the weight variable
  #PNS2013pesDet <- PNS2013pes[!(is.na(PNS2013pes$V0029))]
  pns2013Det <- pns2013[M001==1, ]

  
  #define como imputar variancia quando houver apenas um domicilio (PSU) no estrato 
  # set R to produce conservative standard errors instead of crashing - http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
  options( survey.lonely.psu = "adjust" )  # ??survey.lonely.psu    
  #Cria objeto de desenho da amostra                      
  sample.pns13 <- svydesign(data = pns2013Det,
                            id = ~UPA_PNS, #PSU
                            strata = ~V0024, #Strat
                            weights=~V0029, #PesoPessoa: usar peso original
                            nest = TRUE
                            )
  
  ## Agora é preciso pós-estratificar:
  ## A definição dos pós-estratos e os totais populacionais usados são dados por:
  
  ## post-estratification of sample design
  post_pop <- unique( subset(pns2013Det, select= c(V00293,V00292) ))
  names(post_pop)<-c("V00293","Freq")
  
  sample.pns13.pos <- postStratify(sample.pns13, ~V00293, post_pop)
  

  #Subset design population above 18 years old
  pns13.18y.design <- subset(sample.pns13.pos, C008>17)
  

  
  
  
############## 2. PNS Income quantiles  -----------
  
  # descriptive statistics of RDPC
    svyquantile(~v4721, pns13.18y.design, quantile=c(0.25,0.5,0.75), na.rm=T)
    svymean(~v4721, pns13.18y.design, na.rm=T)
    

  # DECILE BR: Calculate decile intervals considering Sample Design
    decilebr <- svyquantile(~v4721, design = sample.pns13.pos, quantiles = seq(0, 1, by=0.1), method = "linear", ties="rounded", na.rm=T)
    
    # Categorize individuals according to their decile position
      pns2013[ , decileBR :=  cut( v4721 , decilebr, include.lowest= TRUE, labels=1:10, na.rm=T ) ]
      table(pns2013$decileBR)
    

  # Quintile BR: Calculate decile intervals considering Sample Design
      quintilebr <- svyquantile(~v4721, design = sample.pns13.pos, quantiles = seq(0, 1, by=0.2), method = "linear", ties="rounded", na.rm=T)
      
      # Categorize individuals according to their quintile position
      pns2013[ , quintileBR :=  cut( v4721 , quintilebr, include.lowest= TRUE, labels=1:5, na.rm=T ) ]
      table(pns2013$quintileBR)
      
      
          
  # Quntile metro: Calculate Quntile intervals considering Sample Design
    quintiles_metro <- svyby(~v4721, ~metro, design = sample.pns13.pos, svyquantile, quantiles = seq(0, 1 , by=0.2), method = "linear", ties="rounded", na.rm=T, keep.var=FALSE)
    quintiles_metro <- data.table(quintiles_metro)

    # Categorize individuals according to their quintile position
      for (i in c(quintiles_metro$metro) ) { 
        quintile <- as.vector( quintiles_metro[metro== i , c(2:7)] )
        pns2013[ metro== i, quintileMetro :=  as.numeric(cut( v4721 , quintile, include.lowest= TRUE, labels=1:5, na.rm=T )) ]
      }
    
    table(pns2013$quintileMetro, pns2013$metro)
    
    
    
  # Quintile Region: Calculate Quntile intervals considering Sample Design
    quintiles_region <- svyby(~v4721, ~region, design = sample.pns13.pos, svyquantile, quantiles = seq(0, 1 , by=0.2), method = "linear", ties="rounded", na.rm=T, keep.var=FALSE)
    quintiles_region <- data.table(quintiles_region)
    
    # Categorize individuals according to their quintile position
      for (i in c(quintiles_region$region) ) { 
        quintile <- as.vector( quintiles_region[region== i , c(2:7)] )
        pns2013[ region== i, quintileRegion :=  as.numeric(cut( v4721 , quintile, include.lowest= TRUE, labels=1:5, na.rm=T )) ]
      }
    
    table(pns2013$quintileRegion, pns2013$region)
    
  
  # Quntile URBANxRURAL: Calculate Quntile intervals considering Sample Design
    quintiles_urban <- svyby(~v4721, ~urban, design = sample.pns13.pos, svyquantile, quantiles = seq(0, 1 , by=0.2), method = "linear", ties="rounded", na.rm=T, keep.var=FALSE)
    quintiles_urban <- data.table(quintiles_urban)
    
    # Categorize individuals according to their quintile position
    for (i in c(quintiles_urban$urban) ) { 
      quintile <- as.vector( quintiles_urban[urban== i , c(2:7)] )
      pns2013[ urban== i, quintileUrban :=  as.numeric(cut( v4721 , quintile, include.lowest= TRUE, labels=1:5, na.rm=T )) ]
    }
    
    table(pns2013$quintileUrban, pns2013$urban)
    
    
  
    
############## 3. UPDATE PNS Create survey design  -----------
  
  pns2013Det <- pns2013[M001==1, ]
  options( survey.lonely.psu = "adjust" )  # ??survey.lonely.psu    
  sample.pns13 <- svydesign(data = pns2013Det,
                            id = ~UPA_PNS, #PSU
                            strata = ~V0024, #Strat
                            weights=~V0029, #PesoPessoa: usar peso original
                            nest = TRUE)
  ## post-estratification of sample design
  post_pop <- unique( subset(pns2013Det, select= c(V00293,V00292) ))
  names(post_pop)<-c("V00293","Freq")
  sample.pns13.pos <- postStratify(sample.pns13, ~V00293, post_pop)
  pns13.18y.design <- subset(sample.pns13.pos, C008>17)
  
  
  
  
  
############## 4. PNAD Create survey design  -----------

  #define como imputar variancia quando houver apenas um domicilio (PSU) no estrato
   options( survey.lonely.psu = "adjust" )  # ??survey.lonely.psu 
  
  #Cria objeto de desenho da amostra                      
   sample.pnad08 <- svydesign(
                              data = pnad2008,
                              id = ~v4618, #PSU
                              strata = ~v4617, #Strat
                              weights = ~pre_wgt, #person weight v4729
                              nest = TRUE
                              )

   # postStratify pnad2008 (Djalma suggestion)
   post.pop <- unique(data.frame(v4609=as.character(pnad2008$v4609), Freq= as.numeric(pnad2008$v4609)))
   sample.pos <- postStratify(design=sample.pnad08, strata=~v4609, population=post.pop)
   
  # Subset Survey design of people above 18 years old in order to make PNAD 
  # compatible with PNS, in which only people above 18 answer the detailed quest
   pnad08.18y.design <- subset(sample.pos, v8005>=17)
  
  

  
    
############## 5. PNAD Income quantiles  -----------
   
   # DECILE BR: Calculate decile intervals considering Sample Design
   decilebr <- svyquantile(~v4721, design = sample.pos, quantiles = seq(0, 1, by=0.1), method = "linear", ties="rounded", na.rm=T)
   
   # Categorize individuals according to their decile position
   pnad2008[ , decileBR :=  cut( v4721 , decilebr, include.lowest= TRUE, labels=1:10, na.rm=T ) ]
   table(pnad2008$decileBR)
   
   
   # Quintile BR: Calculate decile intervals considering Sample Design
     quintilebr <- svyquantile(~v4721, design = sample.pos, quantiles = seq(0, 1, by=0.2), method = "linear", ties="rounded", na.rm=T)
   
     # Categorize individuals according to their quintile position
        pnad2008[ , quintileBR :=  cut( v4721 , quintilebr, include.lowest= TRUE, labels=1:5, na.rm=T ) ]
        table(pns2013$quintileBR)
     
   
   
   # Quntile metro: Calculate Quntile intervals considering Sample Design
   quintiles_metro <- svyby(~v4721, ~metro, design = sample.pos, svyquantile, quantiles = seq(0, 1 , by=0.2), method = "linear", ties="rounded", na.rm=T, keep.var=FALSE)
   quintiles_metro <- data.table(quintiles_metro)
   
   # Categorize individuals according to their quintile position
   for (i in c(quintiles_metro$metro) ) { 
     quintile <- as.vector( quintiles_metro[metro== i , c(2:7)] )
     pnad2008[ metro== i, quintileMetro :=  as.numeric(cut( v4721 , quintile, include.lowest= TRUE, labels=1:5, na.rm=T )) ]
   }
   
   table(pnad2008$quintileMetro, pnad2008$metro)
   
   
   # Quintile Region: Calculate Quntile intervals considering Sample Design
   quintiles_region <- svyby(~v4721, ~region, design = sample.pos, svyquantile, quantiles = seq(0, 1 , by=0.2), method = "linear", ties="rounded", na.rm=T, keep.var=FALSE)
   quintiles_region <- data.table(quintiles_region)
   
   # Categorize individuals according to their quintile position
   for (i in c(quintiles_region$region) ) { 
     quintile <- as.vector( quintiles_region[region== i , c(2:7)] )
     pnad2008[ region== i, quintileRegion :=  as.numeric(cut( v4721 , quintile, include.lowest= TRUE, labels=1:5, na.rm=T )) ]
   }
   
   table(pnad2008$quintileRegion, pnad2008$region)
   
   
   # Quntile URBANxRURAL: Calculate Quntile intervals considering Sample Design
   quintiles_urban <- svyby(~v4721, ~urban, design = sample.pos, svyquantile, quantiles = seq(0, 1 , by=0.2), method = "linear", ties="rounded", na.rm=T, keep.var=FALSE)
   quintiles_urban <- data.table(quintiles_urban)
   
   # Categorize individuals according to their quintile position
   for (i in c(quintiles_urban$urban) ) { 
     quintile <- as.vector( quintiles_urban[urban== i , c(2:7)] )
     pnad2008[ urban== i, quintileUrban :=  as.numeric(cut( v4721 , quintile, include.lowest= TRUE, labels=1:5, na.rm=T )) ]
   }
   
   table(pnad2008$quintileUrban, pnad2008$urban)
   
   
   

############## 6. UPDATE PNAD survey design  -----------
   
   options( survey.lonely.psu = "adjust" )  # ??survey.lonely.psu 
   sample.pnad08 <- svydesign(
     data = pnad2008,
     id = ~v4618, #PSU
     strata = ~v4617, #Strat
     weights = ~pre_wgt, #person weight v4729
     nest = TRUE )
   
   # postStratify pnad2008 (Djalma suggestion)
   post.pop <- unique(data.frame(v4609=as.character(pnad2008$v4609), Freq= as.numeric(pnad2008$v4609)))
   sample.pos <- postStratify(design=sample.pnad08, strata=~v4609, population=post.pop)
   pnad08.18y.design <- subset(sample.pos, v8005>=17)
   
   
   
   
   
   
   
   
   
   
##### 7. Household Design  -----------

  # PNAD households Survey design
  
    # This eliminates observations with missing values in the weight variable
      summary(pnad2008dom$v4611)
      pnad2008dom <- pnad2008dom[!(is.na(pnad2008dom$v4611))]
  
  options( survey.lonely.psu = "adjust" )
  pnad08dom.design <- svydesign(data = pnad2008dom,
                                id = ~v4618, #PSU
                                strata = ~v4617, #Strat
                                weights = ~v4611, #household weight
                                nest = TRUE)

  
  # PNS households Survey design
  length(which(is.na(pns2013dom$V00281)))   # Count  missing values (NAs), There should be no Missings (NA) in Design Variables
  pns2013dom <- pns2013dom[!(is.na(pns2013dom$V00281))]   # This eliminates observations with missing values in the weight variable
  pns13dom.design <- svydesign(data = pns2013dom,
                               id = ~UPA_PNS, #PSU
                               strata = ~V0024, #Strat
                               weights=~V00281, #Peso household
                               nest = TRUE  )


  
  
# clean objects and memory
  rm(list=setdiff(ls(), c("pns13dom.design", "pnad08dom.design", "pns13.18y.design", "pnad08.18y.design", "pnad2008", "pns2013") ))
  gc(reset = T)


    
########## 8. Save complex sample survey design files  ----------------
  
  save( pnad08.18y.design , file = './data/pnad08.18y.design.rda' )
  save( pns13.18y.design , file = './data/pns13.18y.design.rda' )
  

    
########## 9. Save modified DAta files  ----------------
  
  saveRDS(pns2013, file="./data/pns2013.Rds")
  saveRDS(pnad2008, file="./data/pnad2008.Rds")
  
  
