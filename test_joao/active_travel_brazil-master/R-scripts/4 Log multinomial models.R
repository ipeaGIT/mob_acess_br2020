# Analyze active travel in Brazil using data from PNS 2013
# Script written by Leandro M. T. Garcia
# Dec 2016, Rio de Janeiro, Brazil


####Set working directory####
setwd("R:/Dropbox/github/active_travel_brazil")


####Load packages####
source("./R-scripts/0 LoadPackages.R")


####Load data sets####
pns2013 <- readRDS("./data/pns2013.Rds")
pns2013dom <- readRDS("./data/pns2013dom.Rds")


####Load survey design objects####
load('./data/pns13.18y.design.rda')


####Transform outcome to numeric and dichotomous####
str(pns13.18y.design$variables$P040)
pns13.18y.design$variables$P040n <- as.numeric(as.factor(pns13.18y.design$variables$P040))
head(pns13.18y.design$variables$P040n)
head(pns13.18y.design$variables$P040)
# 1 = "No"
# 2 = "Yes, all the journey"
# 3 = "Yes, part of the journey"

# Dichotomous outcome #1 ("Yes, all the journey" (reference) vs. "Yes, part of the journey")
pns13.18y.design$variables[P040n==2, P040_1 := 0] #(reference)
pns13.18y.design$variables[P040n==3, P040_1 := 1]

# Dichotomous outcome #2 ("Yes, all the journey" (reference) vs. "No")
pns13.18y.design$variables[P040n==2, P040_2 := 0] #(reference)
pns13.18y.design$variables[P040n==1, P040_2 := 1]

####Transform exposure (car or motorcycle in the household) to numeric and reorder####
str(pns13.18y.design$variables$dummyVehicle)
pns13.18y.design$variables$dummyVehiclen <- as.numeric(pns13.18y.design$variables$dummyVehicle)
head(pns13.18y.design$variables$dummyVehiclen)
head(pns13.18y.design$variables$dummyVehicle)
# 1 = "Yes"
# 2 = "No"

pns13.18y.design$variables[dummyVehiclen==2, dummyVehicle_1 := 0]
pns13.18y.design$variables[dummyVehiclen==1, dummyVehicle_1 := 1]
# 0 = "No" (reference)
# 1 = "Yes"


####Crude models####

##1st quintile of income----
# "Yes, all the journey" (reference) vs. "Yes, part of the journey"
crude_q1_out1 <- svyglm(formula = P040_1 ~ dummyVehicle_1,
                        design = subset(pns13.18y.design, quintileBR == 1),
                        family = quasibinomial(link = "log"))
interval_q1_out1 <- confint(crude_q1_out1)
summary(crude_q1_out1)
exp(crude_q1_out1$coefficients)
exp(interval_q1_out1)

#"Yes, all the journey" (reference) vs. "No"
crude_q1_out2 <- svyglm(formula = P040_2 ~ dummyVehicle_1,
                        design = subset(pns13.18y.design, quintileBR == 1),
                        family = quasibinomial(link = "log"))
interval_q1_out2 <- confint(crude_q1_out2)
summary(crude_q1_out2)
exp(crude_q1_out2$coefficients)
exp(interval_q1_out2)


##2nd quintile of income----
# "Yes, all the journey" (reference) vs. "Yes, part of the journey"
crude_q2_out1 <- svyglm(formula = P040_1 ~ dummyVehicle_1,
                        design = subset(pns13.18y.design, quintileBR == 2),
                        family = quasibinomial(link = "log"))
interval_q2_out1 <- confint(crude_q2_out1)
summary(crude_q2_out1)
exp(crude_q2_out1$coefficients)
exp(interval_q2_out1)

#"Yes, all the journey" (reference) vs. "No"
crude_q2_out2 <- svyglm(formula = P040_2 ~ dummyVehicle_1,
                        design = subset(pns13.18y.design, quintileBR == 2),
                        family = quasibinomial(link = "log"))
interval_q2_out2 <- confint(crude_q2_out2)
summary(crude_q2_out2)
exp(crude_q2_out2$coefficients)
exp(interval_q2_out2)


##3rd quintile of income----
# "Yes, all the journey" (reference) vs. "Yes, part of the journey"
crude_q3_out1 <- svyglm(formula = P040_1 ~ dummyVehicle_1,
                        design = subset(pns13.18y.design, quintileBR == 3),
                        family = quasibinomial(link = "log"))
interval_q3_out1 <- confint(crude_q3_out1)
summary(crude_q3_out1)
exp(crude_q3_out1$coefficients)
exp(interval_q3_out1)

#"Yes, all the journey" (reference) vs. "No"
crude_q3_out2 <- svyglm(formula = P040_2 ~ dummyVehicle_1,
                        design = subset(pns13.18y.design, quintileBR == 3),
                        family = quasibinomial(link = "log"))
interval_q3_out2 <- confint(crude_q3_out2)
summary(crude_q3_out2)
exp(crude_q3_out2$coefficients)
exp(interval_q3_out2)


##4th quintile of income----
# "Yes, all the journey" (reference) vs. "Yes, part of the journey"
crude_q4_out1 <- svyglm(formula = P040_1 ~ dummyVehicle_1,
                        design = subset(pns13.18y.design, quintileBR == 4),
                        family = quasibinomial(link = "log"))
interval_q4_out1 <- confint(crude_q4_out1)
summary(crude_q4_out1)
exp(crude_q4_out1$coefficients)
exp(interval_q4_out1)

#"Yes, all the journey" (reference) vs. "No"
crude_q4_out2 <- svyglm(formula = P040_2 ~ dummyVehicle_1,
                        design = subset(pns13.18y.design, quintileBR == 4),
                        family = quasibinomial(link = "log"))
interval_q4_out2 <- confint(crude_q4_out2)
summary(crude_q4_out2)
exp(crude_q4_out2$coefficients)
exp(interval_q4_out2)

##5th quintile of income----
# "Yes, all the journey" (reference) vs. "Yes, part of the journey"
crude_q5_out1 <- svyglm(formula = P040_1 ~ dummyVehicle_1,
                        design = subset(pns13.18y.design, quintileBR == 5),
                        family = quasibinomial(link = "log"))
interval_q5_out1 <- confint(crude_q5_out1)
summary(crude_q5_out1)
exp(crude_q5_out1$coefficients)
exp(interval_q5_out1)

#"Yes, all the journey" (reference) vs. "No"
#First, solve error "No valid set of coefficients has been found: please supply starting values".
#Get and save intercept's starting value.
coefini <- coefficients(svyglm(formula = P040_2 ~ NULL,
                        design = subset(pns13.18y.design, quintileBR == 5),
                        family = quasibinomial(link = "log")))

crude_q5_out2 <- svyglm(formula = P040_2 ~ dummyVehicle_1,
                        design = subset(pns13.18y.design, quintileBR == 5),
                        family = quasibinomial(link = "log"),
                        start = c(coefini, 0))
interval_q5_out2 <- confint(crude_q5_out2)
summary(crude_q5_out2)
exp(crude_q5_out2$coefficients)
exp(interval_q5_out2)


####Adjusted models using household income per capita to stratify####

##1st quintile of income----
# "Yes, all the journey" (reference) vs. "Yes, part of the journey"
#First, solve error "No valid set of coefficients has been found: please supply starting values".
#Get and save intercept's starting value.
coefini <- coefficients(svyglm(formula = P040_1 ~ NULL,
                               design = subset(pns13.18y.design, quintileBR == 1),
                               family = quasibinomial(link = "log")))

adj_q1_out1 <- svyglm(formula = P040_1 ~ dummyVehicle_1 + v0302 + AGE,
                 design = subset(pns13.18y.design, quintileBR == 1),
                 family = quasibinomial(link = "log"),
                 start = c(coefini, rep(0, 7)))
interval_q1_out1_adj <- confint(adj_q1_out1)
summary(adj_q1_out1)
exp(adj_q1_out1$coefficients)
exp(interval_q1_out1_adj)

#"Yes, all the journey" (reference) vs. "No"
#First, solve error "No valid set of coefficients has been found: please supply starting values".
#Get and save intercept's starting value.
coefini <- coefficients(svyglm(formula = P040_2 ~ NULL,
                               design = subset(pns13.18y.design, quintileBR == 1),
                               family = quasibinomial(link = "log")))

adj_q1_out2 <- svyglm(formula = P040_2 ~ dummyVehicle_1 + v0302 + AGE,
                 design = subset(pns13.18y.design, quintileBR == 1),
                 family = quasibinomial(link = "log"),
                 start = c(coefini, rep(0, 7)))
interval_q1_out2_adj <- confint(adj_q1_out2)
summary(adj_q1_out2)
exp(adj_q1_out2$coefficients)
exp(interval_q1_out2_adj)

##2nd quintile of income----
# "Yes, all the journey" (reference) vs. "Yes, part of the journey"
#First, solve error "No valid set of coefficients has been found: please supply starting values".
#Get and save intercept's starting value.
coefini <- coefficients(svyglm(formula = P040_1 ~ NULL,
                               design = subset(pns13.18y.design, quintileBR == 2),
                               family = quasibinomial(link = "log")))

adj_q2_out1 <- svyglm(formula = P040_1 ~ dummyVehicle_1 + v0302 + AGE,
                      design = subset(pns13.18y.design, quintileBR == 2),
                      family = quasibinomial(link = "log"),
                      start = c(coefini, rep(0, 7)))
interval_q2_out1_adj <- confint(adj_q2_out1)
summary(adj_q2_out1)
exp(adj_q2_out1$coefficients)
exp(interval_q2_out1_adj)


#"Yes, all the journey" (reference) vs. "No"
#First, solve error "No valid set of coefficients has been found: please supply starting values".
#Get and save intercept's starting value.
coefini <- coefficients(svyglm(formula = P040_2 ~ NULL,
                               design = subset(pns13.18y.design, quintileBR == 2),
                               family = quasibinomial(link = "log")))

adj_q2_out2 <- svyglm(formula = P040_2 ~ dummyVehicle_1 + v0302 + AGE,
                 design = subset(pns13.18y.design, quintileBR == 2),
                 family = quasibinomial(link = "log"),
                 start = c(coefini, rep(0, 7)))
interval_q2_out2_adj <- confint(adj_q2_out2)
summary(adj_q2_out2)
exp(adj_q2_out2$coefficients)
exp(interval_q2_out2_adj)

##3rd quintile of income----
# "Yes, all the journey" (reference) vs. "Yes, part of the journey"
#First, solve error "No valid set of coefficients has been found: please supply starting values".
#Get and save intercept's starting value.
coefini <- coefficients(svyglm(formula = P040_1 ~ NULL,
                               design = subset(pns13.18y.design, quintileBR == 3),
                               family = quasibinomial(link = "log")))

adj_q3_out1 <- svyglm(formula = P040_1 ~ dummyVehicle_1 + v0302 + AGE,
                      design = subset(pns13.18y.design, quintileBR == 3),
                      family = quasibinomial(link = "log"),
                      start = c(coefini, rep(0, 7)))
interval_q3_out1_adj <- confint(adj_q3_out1)
summary(adj_q3_out1)
exp(adj_q3_out1$coefficients)
exp(interval_q3_out1_adj)

#"Yes, all the journey" (reference) vs. "No"
#First, solve error "No valid set of coefficients has been found: please supply starting values".
#Get and save intercept's starting value.
coefini <- coefficients(svyglm(formula = P040_2 ~ NULL,
                               design = subset(pns13.18y.design, quintileBR == 3),
                               family = quasibinomial(link = "log")))

adj_q3_out2 <- svyglm(formula = P040_2 ~ dummyVehicle_1 + v0302 + AGE,
                 design = subset(pns13.18y.design, quintileBR == 3),
                 family = quasibinomial(link = "log"),
                 start = c(coefini, rep(0, 7)))
interval_q3_out2_adj <- confint(adj_q3_out2)
summary(adj_q3_out2)
exp(adj_q3_out2$coefficients)
exp(interval_q3_out2_adj)

##4th quintile of income----
# "Yes, all the journey" (reference) vs. "Yes, part of the journey"
#First, solve error "No valid set of coefficients has been found: please supply starting values".
#Get and save intercept's starting value.
coefini <- coefficients(svyglm(formula = P040_1 ~ NULL,
                               design = subset(pns13.18y.design, quintileBR == 4),
                               family = quasibinomial(link = "log")))

adj_q4_out1 <- svyglm(formula = P040_1 ~ dummyVehicle_1 + v0302 + AGE,
                      design = subset(pns13.18y.design, quintileBR == 4),
                      family = quasibinomial(link = "log"),
                      start = c(coefini, rep(0, 7)))
interval_q4_out1_adj <- confint(adj_q4_out1)
summary(adj_q4_out1)
exp(adj_q4_out1$coefficients)
exp(interval_q4_out1_adj)

#"Yes, all the journey" (reference) vs. "No"
#First, solve error "No valid set of coefficients has been found: please supply starting values".
#Get and save intercept's starting value.
coefini <- coefficients(svyglm(formula = P040_2 ~ NULL,
                               design = subset(pns13.18y.design, quintileBR == 4),
                               family = quasibinomial(link = "log")))

adj_q4_out2 <- svyglm(formula = P040_2 ~ dummyVehicle_1 + v0302 + AGE,
                 design = subset(pns13.18y.design, quintileBR == 4),
                 family = quasibinomial(link = "log"),
                 start = c(coefini, rep(0, 7)))
interval_q4_out2_adj <- confint(adj_q4_out2)
summary(adj_q4_out2)
exp(adj_q4_out2$coefficients)
exp(interval_q4_out2_adj)

##5th quintile of income----
# "Yes, all the journey" (reference) vs. "Yes, part of the journey"
#First, solve error "No valid set of coefficients has been found: please supply starting values".
#Get and save intercept's starting value.
coefini <- coefficients(svyglm(formula = P040_1 ~ NULL,
                               design = subset(pns13.18y.design, quintileBR == 5),
                               family = quasibinomial(link = "log")))

adj_q5_out1 <- svyglm(formula = P040_1 ~ dummyVehicle_1 + v0302 + AGE,
                      design = subset(pns13.18y.design, quintileBR == 5),
                      family = quasibinomial(link = "log"),
                      start = c(coefini, rep(0, 7)))
interval_q5_out1_adj <- confint(adj_q5_out1)
summary(adj_q5_out1)
exp(adj_q5_out1$coefficients)
exp(interval_q5_out1_adj)

#"Yes, all the journey" (reference) vs. "No"
#First, solve error "No valid set of coefficients has been found: please supply starting values".
#Get and save intercept's starting value.
coefini <- coefficients(svyglm(formula = P040_2 ~ NULL,
                               design = subset(pns13.18y.design, quintileBR == 5),
                               family = quasibinomial(link = "log")))

adj_q5_out2 <- svyglm(formula = P040_2 ~ dummyVehicle_1 + v0302 + AGE,
                 design = subset(pns13.18y.design, quintileBR == 5),
                 family = quasibinomial(link = "log"),
                 start = c(coefini, rep(0, 7)))
interval_q5_out2_adj <- confint(adj_q5_out2)
summary(adj_q5_out2)
exp(adj_q5_out2$coefficients)
exp(interval_q5_out2_adj)
