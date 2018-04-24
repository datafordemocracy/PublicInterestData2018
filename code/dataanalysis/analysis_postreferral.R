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
library(broom)

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
         age8 = ifelse(age>3 & age<9, 1, 0),
         refprior1 = ifelse(refprior>0, 1,0),
         race4b = fct_relevel(race4, "White", "Black", "Multiracial", "Other"), # white as base
         relation6 = fct_relevel(relation6, "Educational"), # educational as baseline
         tract2 = fct_relevel(tract2, "000502"))
# combine alleged maltreatment variable into counts
referral <- referral %>% mutate(negtot = neglect_medical + neglect_physical,
                                abtot = abuse_mental + abuse_physical + abuse_sexual,
                                maltot = negtot + abtot,
                                mal3 = ifelse(maltot==0, 0, ifelse(maltot==1 | maltot==2, 1, 2)))

# create datadist object for use with rms
refdata <- referral %>% 
  select(accept, race4b, female, age2, age8, refprior1, relation6, maltot, tract2)
refdata <- refdata[complete.cases(refdata),]

dd <- datadist(refdata) 
options(datadist="dd")

# logit with clustered standard errors, lrm
# race alone
rm_accept <- lrm(accept ~ race4b, x = TRUE, y = TRUE, data = referral)
rm_acceptb <- robcov(rm_accept, cluster = referral$client_id) # update model with clustered SEs
rm_acceptb
# add demographic covariates
rm_accept1 <- lrm(accept ~ race4b + female + age2 + age8 + refprior1,
                 x = TRUE, y = TRUE, data = referral)
rm_accept1b <- robcov(rm_accept1, cluster = referral$client_id) # update model with clustered SEs
rm_accept1b
# add reporeter relation
rm_accept2 <- lrm(accept ~ race4b + female + age2 + age8 + refprior1 +
                   relation6, 
                 x = TRUE, y = TRUE, data = referral)
rm_accept2b <- robcov(rm_accept2, cluster = referral$client_id)
rm_accept2b
# add alleged maltreatment
rm_accept3 <- lrm(accept ~ race4b + female + age2 + age8 + refprior1 +
                    relation6 + maltot, 
                  x = TRUE, y = TRUE, data = referral)
rm_accept3b <- robcov(rm_accept3, cluster = referral$client_id)
rm_accept3b
# add census tract
rm_accept4 <- lrm(accept ~ race4b + female + age2 + age8 + refprior1 +
                    relation6 + maltot + tract2, 
                  x = TRUE, y = TRUE, data = referral)
rm_accept4b <- robcov(rm_accept4, cluster = referral$client_id)
rm_accept4b

# Output table
stargazer(rm_acceptb, rm_accept1b, rm_accept2b, rm_accept3b, rm_accept4b, 
          title="Logit Regression of Acceptance of Referred Cases", 
          covariate.labels=c("Black", "Multiracial", "Other Race", "Male", "Under Age 3", 
                              "Age 3 to 8", "Prior Referral", 
                              "Reporter: Judicial", "Reporter: Family/Neighbor", 
                              "Reporter: Medical", "Reporter: Governmental", 
                              "Reporter: Other", "Alleged Count"),
          omit.stat=c("f"), column.sep.width="1pt",
          align=TRUE, no.space=TRUE, dep.var.labels.include=FALSE)  

# For figures
# Predicted probabilities
pr_ref <- Predict(rm_accept4b, race4b, refprior1=1, relation6="Family/Care/Neighbor") # generate linear predictors
pr_ref
# Coefficients
coef_ref <- tidy(rm_accept4b$coefficients)
names(coef_ref) <- c("term", "estimate")
coef_ref$std.error <- sqrt(diag(rm_accept4b$var))
coef_ref <- coef_ref %>% 
  mutate(statistic = estimate/std.error,
         p.value = 2*pnorm(-abs(statistic)))


######################################################################################
# 2. Racial disparity in assignment to investigation vs. assessment among accepted
######################################################################################
# update model: race (race4 and race2), gender, age (consider categories), 
#   response priority and/or alleged maltreatment, tracts

# accepted cases only, relevel priority (make low baseline)
refaccept <- referral %>% filter(accept == "Y") %>% 
  mutate(priority = fct_relevel(priority, "R3 - Low"))

# create datadist object for use with rms
refdata <- refaccept %>% 
  select(invest2, race4b, female, age2, age8, refprior1, priority, maltot, tract2)
refdata <- refdata[complete.cases(refdata),]

dd <- datadist(refdata) 
options(datadist="dd")

# logit with clustered standard errors, lrm
# race alone
rm_invest <- lrm(invest2 ~ race4b, x = TRUE, y = TRUE, data = refaccept)
rm_investb <- robcov(rm_invest, cluster = refaccept$client_id)
rm_investb
# add demographic covariates
rm_invest1 <- lrm(invest2 ~ race4b + female + age2 + age8 + refprior1 + priority,
                 x = TRUE, y = TRUE, data = refaccept)
rm_invest1b <- robcov(rm_invest1, cluster = refaccept$client_id)
rm_invest1b
# add alleged maltreatment
rm_invest2 <- lrm(invest2 ~ race4b + female + age2 + age8 + refprior1 + priority + 
                    maltot,
                  x = TRUE, y = TRUE, data = refaccept)
rm_invest2b <- robcov(rm_invest2, cluster = refaccept$client_id)
rm_invest2b
# add census tracts
rm_invest3 <- lrm(invest2 ~ race4b + female + age2 + age8 + refprior1 + priority + 
                    maltot + tract2,
                  x = TRUE, y = TRUE, data = refaccept)
rm_invest3b <- robcov(rm_invest3, cluster = refaccept$client_id)
rm_invest3b

# Output table
stargazer(rm_investb, rm_invest1b, rm_invest2b, rm_invest3b, 
          title="Logit Regression of Investigation of Accepted Cases", 
          covariate.labels=c("Black", "Multiracial", "Other Race", "Male", "Under Age 3", 
                             "Age 3 to 8", "Prior Referral", "High Priority", 
                             "Moderate Priority", "Alleged Count"),
          omit.stat=c("f"), column.sep.width="1pt",
          align=TRUE, no.space=TRUE, dep.var.labels.include=FALSE)  

# For figures
# Predicted probabilities
pr_inv <- Predict(rm_invest3b, race4b, priority = "R2 - Moderate", refprior1 = 1) # generate linear predictors
pr_inv

# Coefficients
coef_inv <- tidy(rm_invest3b$coefficients)
names(coef_inv) <- c("term", "estimate")
coef_inv$std.error <- sqrt(diag(rm_invest3b$var))
coef_inv <- coef_inv %>% 
  mutate(statistic = estimate/std.error,
         p.value = 2*pnorm(-abs(statistic)))


######################################################################################
# 3. Racial disparity in substantiation of investigated cases
######################################################################################
# update model: race (race2 or race3), gender, age (consider categories), 
#   response priority and/or alleged maltreatment, tract

# investigated cases only, create binary founded-1/not founded-0
find <- c("Appealed", "Founded - Level 1", "Founded - Level 2", "Founded - Level 3")
refinvest <- referral %>% filter(invest2=="Investigation") %>% filter(race4 != "Other") %>% 
  mutate(dischr = as.character(disposition),
         finding = ifelse(dischr %in% find, 1, 0),
         finding = ifelse(dischr == "DRS", NA, finding),
         race2b = fct_relevel(race2, "White Children"),
         race3b = fct_relevel(race3, "White"))
# race3 and race2 generate similar substantive results (no race effect)

# create datadist object for use with rms
refdata <- refinvest %>% 
  select(finding, race2b, female, age2, age8, priority, maltot, tract2)
refdata <- refdata[complete.cases(refdata),]

dd <- datadist(refdata) 
options(datadist="dd")

# logit with clustered standard errors, lrm
# race alone
rm_find <- lrm(finding ~ race2b, x = TRUE, y = TRUE, data = refinvest)
rm_findb <- robcov(rm_find, cluster = refinvest$client_id)
rm_findb
# add demographic covariates
rm_find1 <- lrm(finding ~ race2b + female + age2 + age8 + priority,
               x = TRUE, y = TRUE, data = refinvest)
rm_find1b <- robcov(rm_find1, cluster = refinvest$client_id)
rm_find1b
# add alleged maltreatment
rm_find2 <- lrm(finding ~ race2b + female + age2 + age8 + priority +
                 maltot,
               x = TRUE, y = TRUE, data = refinvest)
rm_find2b <- robcov(rm_find2, cluster = refinvest$client_id)
rm_find2b
# add census tracts/combined
rm_find3 <- lrm(finding ~ race2b + female + age2 + age8 + priority +
                  maltot + tract2,
                x = TRUE, y = TRUE, data = refinvest)
rm_find3b <- robcov(rm_find3, cluster = refinvest$client_id)
rm_find3b

# Output table
stargazer(rm_findb, rm_find1b, rm_find2b, rm_find3b, 
          title="Logit Regression of Substantiation of Investigated Cases", 
          covariate.labels=c("Black", "Male", "Under Age 3", 
                             "Age 3 to 8", "High Priority", 
                             "Moderate Priority", "Alleged Count"),
          omit.stat=c("f"), column.sep.width="1pt", notes.align = "l",
          align=TRUE, no.space=TRUE, dep.var.labels.include=FALSE)  

# For figures
# Predicted probabilities
pr_fin <- Predict(rm_find3b, race2b, priority = "R2 - Moderate", tract2="000501") # generate linear predictors
pr_fin

# Coefficients
coef_fin <- tidy(rm_find3b$coefficients)
names(coef_fin) <- c("term", "estimate")
coef_fin$std.error <- sqrt(diag(rm_find3b$var))
coef_fin <- coef_fin %>% 
  mutate(statistic = estimate/std.error,
         p.value = 2*pnorm(-abs(statistic)))


######################################################################################
# 4. Racial disparity in severity of finding among substantiated cases
######################################################################################
# update model: race (race2 or race3), gender, age (consider categories), 
#   response priority and/or alleged maltreatment, tract

# create indicator for level 2,3 findings
find <- c("Founded - Level 2", "Founded - Level 3")
refinvest <- refinvest %>% filter(finding == 1) %>% 
  mutate(severe = ifelse(dischr %in% find, 1, 0))
# race3 and race2 generate similar substantive results (no race effect)

# create datadist object for use with rms
refdata <- refinvest %>% 
  select(severe, race2b, female, age2, age8, priority, maltot, tract2)
refdata <- refdata[complete.cases(refdata),]

dd <- datadist(refdata) 
options(datadist="dd")

# logit with clustered standard errors, lrm
# race alone
rm_severe <- lrm(severe ~ race2b, x = TRUE, y = TRUE, data = refinvest)
rm_severeb <- robcov(rm_severe, cluster = refinvest$client_id)
rm_severeb
# add demographic covariates
rm_severe1 <- lrm(severe ~ race2b + female + age2 + age8 + priority, 
                  x = TRUE, y = TRUE, data = refinvest)
rm_severe1b <- robcov(rm_severe1, cluster = refinvest$client_id)
rm_severe1b
# add alleged maltreatment
rm_severe2 <- lrm(severe ~ race2b + female + age2 + age8 + priority +
                    maltot, 
                  x = TRUE, y = TRUE, data = refinvest)
rm_severe2b <- robcov(rm_severe2, cluster = refinvest$client_id)
rm_severe2b
# # add tract (not enough cases for so many covariates)
# rm_severe3 <- lrm(severe ~ race2b + female + age2 + age8 + priority +
#                     maltot + tract2, 
#                   x = TRUE, y = TRUE, data = refinvest)
# rm_severe3b <- robcov(rm_severe3, cluster = refinvest$client_id)
# rm_severe3b

# Output table
stargazer(rm_severeb, rm_severe1b, rm_severe2b,
          title="Logit Regression of Severity of Substantiated Cases", 
          covariate.labels=c("Black", "Male", "Under Age 3", 
                             "Age 3 to 8", "High Priority", 
                             "Moderate Priority", "Alleged Count"),
          omit.stat=c("f"), column.sep.width="1pt", notes.align = "l",
          align=TRUE, no.space=TRUE, dep.var.labels.include=FALSE)  

# For figures
# Predicted probabilities
pr_sev <- Predict(rm_severe2b, race2b, priority = "R2 - Moderate") # generate linear predictors
pr_sev

# Coefficients
coef_sev <- tidy(rm_severe2b$coefficients)
names(coef_sev) <- c("term", "estimate")
coef_sev$std.error <- sqrt(diag(rm_severe2b$var))
coef_sev <- coef_sev %>% 
  mutate(statistic = estimate/std.error,
         p.value = 2*pnorm(-abs(statistic)))


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

m_contact <- glm(contact_count ~ race2b, family = "poisson", data = referral_active)
summary(m_contact)

m_contact1 <- glm(contact_count ~ race2b + female + age2 + age9,
                  family = "poisson", data = referral_active)
summary(m_contact1)

m_contact2 <- glm(contact_count ~ race2b + female + age2 + age9 + priority,
                  family = "poisson", data = referral_active)
summary(m_contact2)

m_contact3 <- glm(contact_count ~ race2b + female + age2 + age9 + priority + tract2,
                  family = "poisson", data = referral_active)
summary(m_contact3)

# Output table
stargazer(m_contact, m_contact1, m_contact2, m_contact3, 
          title="Poisson Regression of Face-to-Face Counts for Active Cases", 
          covariate.labels=c("Black/Other", "Multiracial", "Male", "Under Age 3", 
                             "Age 3 to 9", "High Priority", 
                             "Moderate Priority"),
          omit.stat=c("f"), column.sep.width="1pt", notes.align = "l",
          align=TRUE, no.space=TRUE, dep.var.labels.include=FALSE)  
# not yet in report (need to decide on race2 or race3)

# For figures
# Predicted probabilities
newdata <- data.frame(race2b = c("White Children", "Children of Color"), female = "Male", 
                      age2 = 0, age9 = 0, priority = "R2 - Moderate", tract2 = "000501")
pr_ftf <- predict(m_contact3, newdata, type="response")

# Coefficients
coef_ftf <- tidy(m_contact3)


######################################################################################
# 6. Racial disparity in duration of services among active
######################################################################################
# no current model: use referral_active data frame (184 cases)



######################################################################################
# 7. Create data frame of predictions with p-values
######################################################################################

pr_ref[1,9] # white prob
pr_inv[1,9]
pr_fin[1,8]
pr_sev[1,8]
pr_ftf[1]

pr_ref[2,9] # black prob
pr_inv[2,9]
pr_fin[2,8]
pr_sev[2,8]
pr_ftf[2]

pr_ref[3,9] # multiracial prob
pr_inv[3,9]

pr_ref[4,9] # other prob
pr_inv[4,9]

refp <- pnorm(abs(rm_accept4b$coef/sqrt(diag(rm_accept4b$var))),lower.tail=F)*2 
invp <- pnorm(abs(rm_invest3b$coef/sqrt(diag(rm_invest3b$var))),lower.tail=F)*2 
finp <- pnorm(abs(rm_find3b$coef/sqrt(diag(rm_find3b$var))),lower.tail=F)*2 
sevp <- pnorm(abs(rm_severe2b$coef/sqrt(diag(rm_severe2b$var))),lower.tail=F)*2 

predict <- data.frame(dv = c("ref", "ref", "ref", "inv", "inv", "inv", "fin", "sev"),
                      white = c(pr_ref[1,9],pr_ref[1,9],pr_ref[1,9],pr_inv[1,9],pr_inv[1,9],pr_inv[1,9],
                                pr_fin[1,8],pr_sev[1,8]),
                      race = c(pr_ref[2,9],pr_ref[3,9],pr_ref[4,9],pr_inv[2,9],pr_inv[3,9],pr_inv[4,9],
                               pr_fin[2,8],pr_sev[2,8]),
                      racecat = c("black", "multi", "other", "black", "multi", "other", 
                                  "black", "black"),
                      pval = c(refp[2],refp[3],refp[4],invp[2],invp[3],invp[4],
                      finp[2],sevp[2]))
predict <- predict %>% 
  mutate(wprob = 1/(1+exp(-white)), cprob = 1/(1+exp(-race)))

# save analysis
setwd("/Volumes/NO NAME") 
save.image("postreferralmodel.RData")
# load("postreferralmodel.Rdata")
setwd("~/Box Sync/mpc/dataForDemocracy/pidl/PIDL2018/code/Group 1 Code/githubcode/")
save(predict, coef_ref, coef_inv, coef_fin, coef_sev, file = "predictions.RData")
# write.csv(predict, file = "predictions.csv")
