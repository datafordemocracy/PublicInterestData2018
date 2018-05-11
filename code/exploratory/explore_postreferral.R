######################################################################################
# DS 5559: Public Interest Data Lab 
# Explore data, dig into details of models for model selection
# 1. Racial disparity in accepted cases among referred
# 3. Racial disparity in substantiation of investigated cases
# 4. Racial disparity in severity of finding among substantiated cases
# 5. Racial disparity in time to contact (or number of contacts from active)
# 6. Racial disparity in number of contacts from active cases or duration of services
# Michele Claibourn (mclaibourn@virginia.edu)
# Created by: Hannah Sullivan, Michael Woon, Melissa Wu, Naifei Pan, James Mekavibul,
#   and Michele Claibourn
# Updated: April 2, 2018 
######################################################################################

rm(list=ls()) # clear workspace
library(rms)
library(stargazer)
library(tidyverse)

setwd("/Volumes/NO NAME") # point to encrypted file
load("postreferral.RData")

######################################################################################
# 1. Racial disparity in accepted cases among referred
######################################################################################
# incorporate age categories?
# plot loess of age on screened out... 
ggplot(referral, aes(x = age, y = as.numeric(screen_out))) + geom_smooth(method =  "loess")
# peaks around 2, tapers off around 8/9, declines after 15

# include refprior count or just indicator?
ggplot(referral, aes(x = refprior, y = as.numeric(screen_out))) + geom_smooth(method = "loess")
# potentially curvilinear, go with indicator

referral <- referral %>% 
  mutate(accept = fct_relevel(accept, "N", "Y"), # "N" as base
         age2 = ifelse(age<=2, 1, 0),
         age9 = ifelse(age>3 & age<10, 1, 0),
         refprior1 = ifelse(refprior>0, 1,0),
         race4b = fct_relevel(race4, "White", "Black", "Multiracial", "Other"), # white as base
         relation6 = fct_relevel(relation6, "Educational")) # educational as baseline

referral <- referral %>% mutate(negtot = neglect_medical + neglect_physical,
                                abtot = abuse_mental + abuse_physical + abuse_sexual,
                                maltot = negtot + abtot,
                                mal3 = ifelse(maltot==0, 0, ifelse(maltot==1 | maltot==2, 1, 2)))

# what's going on with alleged maltreatment?
# anything distinctive about type of alleged abuse by race? (cycle through measures)
with(referral, table(race4, abuse_sexual))
with(referral, prop.table(table(race4, abuse_sexual),1))
with(referral, prop.table(table(race4, abuse_sexual),2))
# maltot: black higher on 3; negtot: black higher on 2; abtot: higher on 1,2 
# negphys: no diffs; negmed: black a little higher; abment: black a little higher; abphys: black a little lower; absex: multi a little higher

# leave one out
m_accept <- glm(accept ~ race4b + female + age2 + age9 + refprior1 +
                   neglect_medical+ neglect_physical + abuse_mental + abuse_physical + abuse_sexual, 
                 family = "binomial", data = referral)
summary(m_accept)
# all indicator: black 0.5131     0.2733   1.877 0.060517 . 
# w/o negmed: black 0.38937    0.24323   1.601  0.10942
# w/o negphys: black -0.07297    0.10767  -0.678 0.497956
# w/o abment: black 0.27820    0.25720   1.082    0.279 
# w/o abphys: black 0.19648    0.16914   1.162  0.24538
# w/o absex: black 0.363148   0.246039   1.476   0.1400

# try each pair
m_accept <- glm(accept ~ race4b + female + age2 + age9 + refprior1 +
                  abuse_physical + abuse_sexual, 
                family = "binomial", data = referral)
summary(m_accept)
# negmed, negphys: black 0.1070     0.1574   0.680   0.4964
# negmed, abment: black -0.125729   0.099590  -1.262  0.20678
# negmed, abphys: black 0.01250    0.09924   0.126  0.89977 
# negmed, absex: black -0.03007    0.09424  -0.319  0.74962
# negphys, abment: black 0.14195    0.15648   0.907   0.3644 
# negphys, abphys: black 0.16210    0.21215   0.764   0.4448 
# negphys, absex: black  0.10381    0.15544   0.668   0.5042 
# abment, abphys: black -0.06553    0.10491  -0.625  0.53223
# abmnet, absex: black -0.11313    0.09875  -1.146  0.25195
# abphys, absex: black 0.02849    0.09847   0.289  0.77234

# try each triad
m_accept <- glm(accept ~ race4b + female + age2 + age9 + refprior1 +
                  abuse_sexual + neglect_physical + neglect_medical, 
                family = "binomial", data = referral)
summary(m_accept)
# negmed, negphys, abment: black 0.1681     0.1635   1.028   0.3041
# negmed, negphys, abphys: black 0.19817    0.23224   0.853  0.39350
# negmed, negphys, absex: black 0.124951   0.162342   0.770   0.4415
# negmed, abment, abphys: black -0.08003    0.10675  -0.750 0.453440 
# negmed, abment, absex: black -0.12083    0.10013  -1.207 0.227542
# negphys, abment, abphys: black 0.292985   0.223085   1.313   0.1891
# negphys, abment, absex: black 0.16531    0.16115   1.026  0.30500 
# negphys, abphys, absex: black 0.2145     0.2300   0.933    0.351
# abment, abphys, absex: black -0.05838    0.10572  -0.552  0.58084

# with counts 
m_accept <- glm(accept ~ race4b + female + age2 + age9 + refprior1 +
                  abtot + negtot, 
                family = "binomial", data = referral)
summary(m_accept)
# maltot: black 0.50221    0.26529   1.893 0.058347 .
# mal3: black 0.56221    0.27830   2.020 0.043365 * 
# abtot: black -0.04474    0.10548  -0.424 0.671422
# negtot: black 0.1072     0.1573   0.681   0.4959
# abtot, negtot: black 0.52376    0.26965   1.942 0.052092 .

# interaction
m_accept <- glm(accept ~ race4b + female + age2 + age9 + refprior1 +
                  maltot + race4b*maltot, 
                family = "binomial", data = referral)
summary(m_accept)

### Race effects within maltreatment types?
m_accept <- glm(accept ~ race4b + female + age2 + age9 + refprior1,
                family = "binomial", data = subset(referral, abuse_physical==1))
summary(m_accept)
# black only approaches significance among victims of alleged physical abuse


### Race effects by census tracts?
table(referral$tract2)
m_accept <- glm(accept ~ race4b + female + age2 + age9 + refprior1 + maltot,
                family = "binomial", data = subset(referral, tract2 == "000501"))
summary(m_accept)
# race approaches significance in tract 501 (p=.187)

######################################################################################
# 5. Racial disparity in time to contact (or number of contacts from active)
######################################################################################


######################################################################################
# 6. Racial disparity in number of contacts from active cases or duration of services
######################################################################################
