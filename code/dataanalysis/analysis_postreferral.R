######################################################################################
# DS 5559: Public Interest Data Lab 
# Estimate racial disparity models
# 1. Racial disparity in accepted cases among referred
# 2. Racial disparity in assignment to investigation vs. assessment among accepted
# 3. Racial disparity in substantiation of investigated cases
# 4. Racial disparity in severity of finding among substantiated cases
# 5. Racial disparity in time to contact (or number of contacts from active)
# 6. Racial disparity in number of contacts from active cases or duration of services
# Michele Claibourn (mclaibourn@virginia.edu)
# Created by: Hannah Sullivan, Michael Woon, Melissa Wu, Naifei Pan, James Mekavibul,
#   and Michele Claibourn
# Updated: April 2, 2018 
######################################################################################
# consider 5, 6

rm(list=ls()) # clear workspace
library(rms)
library(stargazer)
library(tidyverse)

setwd("/Volumes/NO NAME") # point to encrypted file
load("postreferral.RData")
setwd("~/Box Sync/mpc/dataForDemocracy/pidl/PIDL2018/code/Group 1 Code/") # save tables here


######################################################################################
# 1. Racial disparity in accepted cases among referred
######################################################################################
# no current model; add logit of accept as a function of:
#  race (race4, white omitted), gender, age (consider categories), prior referrals (refprior),
#  reporter relation (relation6), tracts, alleged maltreatment dummies

# relevel accept ("N" as base category), relevel race (white as base category), relevel relation (education as baseline), relevel tract (700 as baseline)
#   create age indicators, turn refprior into indicator
referral <- referral %>% 
  mutate(accept = fct_relevel(accept, "N", "Y"), 
         age2 = ifelse(age<=2, 1, 0),
         age9 = ifelse(age>3 & age<10, 1, 0),
         refprior1 = ifelse(refprior>0, 1,0),
         race4b = fct_relevel(race4, "White", "Black", "Multi-Race", "Other"), # white as base
         relation6 = fct_relevel(relation6, "Educational"), # educational as baseline
         tract2 = fct_relevel(tract2, "000502"))
# combine alleged maltreatment variable into counts
referral <- referral %>% mutate(negtot = neglect_medical + neglect_physical,
                                abtot = abuse_mental + abuse_physical + abuse_sexual,
                                maltot = negtot + abtot,
                                mal3 = ifelse(maltot==0, 0, ifelse(maltot==1 | maltot==2, 1, 2)))

# logit with clustered standard errors, lrm
# race alone
rm_accept <- lrm(accept ~ race4b, x = TRUE, y = TRUE, data = referral)
rm_acceptb <- robcov(rm_accept, cluster = referral$client_id) # update model with clustered SEs
rm_acceptb
# add demographic covariates
rm_accept1 <- lrm(accept ~ race4b + female + age2 + age9 + refprior1,
                 x = TRUE, y = TRUE, data = referral)
rm_accept1b <- robcov(rm_accept1, cluster = referral$client_id) # update model with clustered SEs
rm_accept1b
# add reporeter relation
rm_accept2 <- lrm(accept ~ race4b + female + age2 + age9 + refprior1 +
                   relation6, 
                 x = TRUE, y = TRUE, data = referral)
rm_accept2b <- robcov(rm_accept2, cluster = referral$client_id)
rm_accept2b
# add alleged maltreatment
rm_accept3 <- lrm(accept ~ race4b + female + age2 + age9 + refprior1 +
                    relation6 + maltot, 
                  x = TRUE, y = TRUE, data = referral)
rm_accept3b <- robcov(rm_accept3, cluster = referral$client_id)
rm_accept3b
# add census tract
rm_accept4 <- lrm(accept ~ race4b + female + age2 + age9 + refprior1 +
                    relation6 + maltot + tract2, 
                  x = TRUE, y = TRUE, data = referral)
rm_accept4b <- robcov(rm_accept4, cluster = referral$client_id)
rm_accept4b

# Output table
stargazer(rm_acceptb, rm_accept1b, rm_accept2b, rm_accept3b, rm_accept4b, 
          title="Logit Regression of Acceptance of Referred Cases", 
          covariate.labels=c("Black", "Multi-Racial", "Other Race", "Male", "Under Age 3", 
                              "Age 3 to 9", "Prior Referral", 
                              "Reporter: Judicial", "Reporter: Family/Neighbor", 
                              "Reporter: Medical", "Reporter: Governmental", 
                              "Reporter: Other", "Alleged Count"),
          omit.stat=c("f"), column.sep.width="1pt",
          align=TRUE, no.space=TRUE, dep.var.labels.include=FALSE)  


######################################################################################
# 2. Racial disparity in assignment to investigation vs. assessment among accepted
######################################################################################
# update model: race (race4 and race2), gender, age (consider categories), 
#   response priority and/or alleged maltreatment, tracts

# accepted cases only, relevel priority (make low baseline)
refaccept <- referral %>% filter(accept == "Y") %>% 
  mutate(priority = fct_relevel(priority, "R3 - Low"))

# logit with clustered standard errors, lrm
# race alone
rm_invest <- lrm(invest2 ~ race4b, x = TRUE, y = TRUE, data = refaccept)
rm_investb <- robcov(rm_invest, cluster = refaccept$client_id)
rm_investb
# add demographic covariates
rm_invest1 <- lrm(invest2 ~ race4b + female + age2 + age9 + refprior1 + priority,
                 x = TRUE, y = TRUE, data = refaccept)
rm_invest1b <- robcov(rm_invest1, cluster = refaccept$client_id)
rm_invest1b
# add alleged maltreatment
rm_invest2 <- lrm(invest2 ~ race4b + female + age2 + age9 + refprior1 + priority + 
                    maltot,
                  x = TRUE, y = TRUE, data = refaccept)
rm_invest2b <- robcov(rm_invest2, cluster = refaccept$client_id)
rm_invest2b
# add census tracts
rm_invest3 <- lrm(invest2 ~ race4b + female + age2 + age9 + refprior1 + priority + 
                    maltot + tract2,
                  x = TRUE, y = TRUE, data = refaccept)
rm_invest3b <- robcov(rm_invest3, cluster = refaccept$client_id)
rm_invest3b

# Output table
stargazer(rm_investb, rm_invest1b, rm_invest2b, rm_invest3b, 
          title="Logit Regression of Investigation of Accepted Cases", 
          covariate.labels=c("Black", "Multi-Racial", "Other Race", "Male", "Under Age 3", 
                             "Age 3 to 9", "Prior Referral", "High Priority", 
                             "Moderate Priority", "Alleged Count"),
          omit.stat=c("f"), column.sep.width="1pt",
          align=TRUE, no.space=TRUE, dep.var.labels.include=FALSE)  


######################################################################################
# 3. Racial disparity in substantiation of investigated cases
######################################################################################
# update model: race (race2 or race3), gender, age (consider categories), 
#   response priority and/or alleged maltreatment, tract

# investigated cases only, create binary founded-1/not founded-0
find <- c("Appealed", "Founded - Level 1", "Founded - Level 2", "Founded - Level 3")
refinvest <- referral %>% filter(invest2=="Investigation") %>% 
  mutate(dischr = as.character(disposition),
         finding = ifelse(dischr %in% find, 1, 0),
         finding = ifelse(dischr == "DRS", NA, finding),
         race2b = fct_relevel(race2, "White Children"),
         race3b = fct_relevel(race3, "White"))
# race3 and race2 generate similar substantive results (no race effect)

# logit with clustered standard errors, lrm
# race alone
rm_find <- lrm(finding ~ race3b, x = TRUE, y = TRUE, data = refinvest)
rm_findb <- robcov(rm_find, cluster = refinvest$client_id)
rm_findb
# add demographic covariates
rm_find1 <- lrm(finding ~ race3b + female + age2 + age9 + priority,
               x = TRUE, y = TRUE, data = refinvest)
rm_find1b <- robcov(rm_find1, cluster = refinvest$client_id)
rm_find1b
# add alleged maltreatment
rm_find2 <- lrm(finding ~ race3b + female + age2 + age9 + priority +
                 maltot,
               x = TRUE, y = TRUE, data = refinvest)
rm_find2b <- robcov(rm_find2, cluster = refinvest$client_id)
rm_find2b
# add census tracts/combined
rm_find3 <- lrm(finding ~ race3b + female + age2 + age9 + priority +
                  maltot + tract2,
                x = TRUE, y = TRUE, data = refinvest)
rm_find3b <- robcov(rm_find3, cluster = refinvest$client_id)
rm_find3b

# Output table
stargazer(rm_findb, rm_find1b, rm_find2b, rm_find3b, 
          title="Logit Regression of Substantiation of Investigated Cases", 
          covariate.labels=c("Children of Color", "Male", "Under Age 3", 
                             "Age 3 to 9", "Prior Referral", "High Priority", 
                             "Moderate Priority", "Alleged Count"),
          omit.stat=c("f"), column.sep.width="1pt",
          align=TRUE, no.space=TRUE, dep.var.labels.include=FALSE)  


######################################################################################
# 4. Racial disparity in severity of finding among substantiated cases
######################################################################################
# update model: race (race2 or race3), gender, age (consider categories), 
#   response priority and/or alleged maltreatment, tract

# create indicator for level 2,3 findings
find <- c("Founded - Level 2", "Founded - Level 3")
refinvest <- refinvest %>% 
  mutate(severe = ifelse(dischr %in% find, 1, 0))
# race3 and race2 generate similar substantive results (no race effect)

# logit with clustered standard errors, lrm
# race alone
rm_severe <- lrm(severe ~ race3b, x = TRUE, y = TRUE, data = refinvest)
rm_severeb <- robcov(rm_severe, cluster = refinvest$client_id)
rm_severeb
# add demographic covariates
rm_severe1 <- lrm(severe ~ race3b + female + age2 + age9 + priority, 
                  x = TRUE, y = TRUE, data = refinvest)
rm_severe1b <- robcov(rm_severe1, cluster = refinvest$client_id)
rm_severe1b
# add alleged maltreatment
rm_severe2 <- lrm(severe ~ race3b + female + age2 + age9 + priority +
                    maltot, 
                  x = TRUE, y = TRUE, data = refinvest)
rm_severe2b <- robcov(rm_severe2, cluster = refinvest$client_id)
rm_severe2b

# Output table
stargazer(rm_severeb, rm_severe1b, rm_severe2b, 
          title="Logit Regression of Severity of Substantiated Cases", 
          covariate.labels=c("Children of Color", "Male", "Under Age 3", 
                             "Age 3 to 9", "High Priority", 
                             "Moderate Priority", "Alleged Count"),
          omit.stat=c("f"), column.sep.width="1pt",
          align=TRUE, no.space=TRUE, dep.var.labels.include=FALSE)  


######################################################################################
# 5. Racial disparity in number of contacts from active
######################################################################################
# no current model: use referral_active data frame (184 cases)
summary(referral_active$contact_count) # 49 cases missing on contact, 135 remaining cases

referral_active <- referral_active %>% 
  mutate(race3b = fct_relevel(race3, "White"),
         race2b = fct_relevel(race2.x, "White Children"),
         age2 = ifelse(age.x<=2, 1, 0),
         age9 = ifelse(age.x>3 & age.x<10, 1, 0),
         priority = fct_relevel(priority, "R3 - Low"),
         negtot = neglect_medical + neglect_physical,
         abtot = abuse_mental + abuse_physical + abuse_sexual,
         maltot = negtot + abtot,
         mal3 = ifelse(maltot==0, 0, ifelse(maltot==1 | maltot==2, 1, 2)))
# race3 and race2 generate similar substantive results (no race effect)

m_contact <- glm(contact_count ~ race3b, family = "poisson", data = referral_active)
summary(m_contact)

m_contact1 <- glm(contact_count ~ race3b + female + age2 + age9,
                  family = "poisson", data = referral_active)
summary(m_contact1)

m_contact2 <- glm(contact_count ~ race3b + female + age2 + age9 + priority,
                  family = "poisson", data = referral_active)
summary(m_contact2)

m_contact3 <- glm(contact_count ~ race3b + female + age2 + age9 + priority + tract2,
                  family = "poisson", data = referral_active)
summary(m_contact3)

# Output table
stargazer(m_contact, m_contact1, m_contact2, m_contact3, 
          title="Poisson Regression of Face-to-Face Counts for Active Cases", 
          covariate.labels=c("Black/Other", "Multi-Racial", "Male", "Under Age 3", 
                             "Age 3 to 9", "High Priority", 
                             "Moderate Priority"),
          omit.stat=c("f"), column.sep.width="1pt",
          align=TRUE, no.space=TRUE, dep.var.labels.include=FALSE)  
# not yet in report (need to decide on race2 or race3)

######################################################################################
# 6. Racial disparity in duration of services among active
######################################################################################
# no current model: use referral_active data frame (184 cases)


# save analysis
setwd("/Volumes/NO NAME") 
save.image("postreferralmodel.RData")
