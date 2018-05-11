######################################################################################
# DS 5559: Public Interest Data Lab 
# Data Analysis Script: Placement History & Foster Care Data
# 1. Data exploration, summaries
# 2. Logit models, initial placement
# 3. Logit models, all placements
# 4. Count model, number of placements
# 5. Duration model, overall time in foster care
# 6. Duration model, time in each placement
# 7. Create coefficients and predictions dataset
# Created by C. McClintock
# Contact: Michele Claibourn (mclaibourn@virginia.edu)
# Updated: May 10, 2018 mpc
######################################################################################

rm(list=ls())
library(tidyverse)
library(stargazer)

setwd("/Volumes/NO NAME") # point to encrypted file
# setwd("/Volumes/encrypted_data") # encrypted file on different computer
load("mergecheck.Rdata")

# list of the important objects
# fc # merge of clean_placement and clean_foster_care, 115 rows
# fcph # full join of foster care and placement history sheets, 262 rows 


######################################################################################
# 1. Data exploration, summaries
######################################################################################
# age of children in foster care
summary(fc$age_atcustody)

# number of children of each race in the placement history & foster care data
fc %>% group_by(race) %>% summarize(count = n())
# 53 black children, 36 multiracial children, 26 white children

# racial composition of multi-racial: all black-white
fcph %>% filter(race=="Multi-Race") %>% 
  summarize(mean(race_asian), mean(race_black), mean(race_white))

table(fc$sys_status) # 50 have left, 65 still in the system 


######################################################################################
# 2. Logit models, initial placement
######################################################################################
# create age indicators
fc <- fc %>% 
  mutate(age2 = ifelse(age_atcustody<=2, 1, 0),
         age8 = ifelse(age_atcustody>3 & age_atcustody<9, 1, 0))

## likelihood of foster family: race only
iplace_fos0 <- glm(foster_fam ~ race_black, 
                 family = "binomial", data = fc)
summary(iplace_fos0)
# race, gender, age
iplace_fos1 <- glm(formula = foster_fam ~ race_black + male + age2 + age8, 
    family = binomial(link="logit"), data = fc)
summary(iplace_fos1)
# create age indicators, substantiation indicators
fc_ref <- fc_ref %>% 
  mutate(age2 = ifelse(age_atcustody<=2, 1, 0),
         age8 = ifelse(age_atcustody>3 & age_atcustody<9, 1, 0),
         substantiate = ifelse(disposition == "Substantiated" | disposition == "Substantiated - Extreme", 1, 0),
         sub3 = ifelse(disposition == "Substantiated", 1, ifelse(disposition == "Substantiated - Extreme", 2, 0)),
         sub3 = factor(sub3, levels = c("0", "1", "2")))
# race, gender, age, substantiaiton
iplace_fos2 <- glm(formula = foster_fam ~ race_black + male + age2 + age8
              + sub3, family = binomial(link="logit"), 
             data = fc_ref)
summary(iplace_fos2)

## likelihood of residential: race only
iplace_res0 <- glm(residential ~ race_black,
                 family = "binomial", data = fc)
summary(iplace_res0)
# race, gender, age
iplace_res1 <- glm(formula = residential ~ race_black + male + age2 + age8, 
            family = binomial(link="logit"), data = fc)
summary(iplace_res1)
# race, gender, age, substantiation
iplace_res2 <- glm(formula = residential ~ race_black + male + age2 + age8 
               + sub3, 
              family = binomial(link="logit"), data = fc_ref)
summary(iplace_res2)

## likelihood of kinship: race only
iplace_kin0 <- glm(kinship ~ race_black,
                 family = "binomial", data = fc)
summary(iplace_kin0)
# race, gender, age
iplace_kin1 <- glm(formula = kinship ~ race_black + male + age2 + age8, 
              family = binomial(link="logit"), data = fc)
summary(iplace_kin1)
# race, gender, age, substantiation
iplace_kin2 <- glm(formula = kinship ~ race_black + male + age2 + age8 
               + sub3, 
               family = binomial(link="logit"), data = fc_ref)
summary(iplace_kin2)

## Generate table
stargazer(iplace_fos0, iplace_fos1, iplace_fos2, iplace_kin0, iplace_kin1, iplace_kin2, 
          iplace_res0, iplace_res1, iplace_res2,  title = "Initial Placement",
          dep.var.labels = c("Foster Family", "Kinship Care", "Residential"), 
          dep.var.caption = "First Placement in Out of Home Care",
          covariate.labels = c("Black", "Male", "Under Age 3", "Age 3 to 8", "Finding-Level 1/2", "Finding-Level 3"),
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.2, .1, .05, .01),
          notes = c("+ p<0.2; . p<0.1; * p<0.05; ** p<0.01"))
# notes = "Black children include children identified as Black or children identified as Black-White Multi-Racial" 

## Results for predicted probability figure
newdata1 <- data.frame(race_black = c(0,1), male = 1, 
                      age2 = 0, age8 = 0, sub3 = "1")
newdata2 <- data.frame(race_black = c(0,1), male = 1, 
                       age2 = 0, age8 = 1, sub3 = "1")

pr_ff <- predict(iplace_fos2, newdata1, type="response")
pr_ff

pr_kc <- predict(iplace_kin2, newdata2, type="response")
pr_kc

pr_rc <- predict(iplace_res2, newdata1, type="response")
pr_rc

## Coefficients for coefficient plot
library(broom)
coef_ff <- tidy(iplace_fos2)
coef_kc <- tidy(iplace_kin2)
coef_rc <- tidy(iplace_res2)

coef_ff$wprob <- pr_ff[1]
coef_ff$bprob <- pr_ff[2]

coef_kc$wprob <- pr_kc[1]
coef_kc$bprob <- pr_kc[2]

coef_rc$wprob <- pr_rc[1]
coef_rc$bprob <- pr_rc[2]


######################################################################################
# 3. Logit models, all placements
######################################################################################
# create age indicators
fcph <- fcph %>% 
  mutate(place_status = ifelse(is.na(place_exit_date), 1, 0),
         age2 = ifelse(age_atcustody<=2, 1, 0),
         age8 = ifelse(age_atcustody>3 & age_atcustody<9, 1, 0))

## likelihood of foster family: race only
place_fos0 <- glm(foster_fam ~ race_black, 
                 family = "binomial", data = fcph)
summary(place_fos0)
# race, gender, age
place_fos1 <- glm(formula = foster_fam ~ race_black + male + age2 + age8, 
                  family = binomial(link="logit"), data = fcph)
summary(place_fos1)

# Add referral data to placement history
fcph_ref <- right_join(fcph, fc_ref, by = c("client_id", "case_id"))
fcph_ref <- fcph_ref %>% 
  rename(race_black = race_black.x, male = male.x, age2 = age2.x, age8 = age8.x, 
         foster_fam = foster_fam.x, kinship = kinship.x, residential = residential.x)
# race, gender, age, substantiation
place_fos2 <- glm(formula = foster_fam ~ race_black + male + age2 + age8
                  + sub3, family = binomial(link="logit"), 
                  data = fcph_ref)
summary(place_fos2)

## likelihood of residential: race only
place_res0 <- glm(residential ~ race_black,
                 family = "binomial", data = fcph)
summary(place_res0)
# race, gender, age
place_res1 <- glm(formula = residential ~ race_black + male + age2 + age8, 
                  family = binomial(link="logit"), data = fcph)
summary(place_res1)
# race, gender, age, substantiation
place_res2 <- glm(formula = residential ~ race_black + male + age2 + age8 
                  + sub3, 
                  family = binomial(link="logit"), data = fcph_ref)
summary(place_res2)

## likelihood of kinship: race only
place_kin0 <- glm(kinship ~ race_black,
                 family = "binomial", data = fcph)
summary(place_kin0)

place_kin1 <- glm(formula = kinship ~ race_black + male + age2 + age8, 
                  family = binomial(link="logit"), data = fcph)
summary(place_kin1)

place_kin2 <- glm(formula = kinship ~ race_black + male + age2 + age8 
                  + sub3, 
                  family = binomial(link="logit"), data = fcph_ref)
summary(place_kin2)

## Generate table
stargazer(place_fos0, place_fos1, place_fos2, place_kin0, place_kin1, place_kin2, 
          place_res0, place_res1, place_res2,  title = "All Placements",
          dep.var.labels = c("Foster Family", "Kinship Care", "Residential"), 
          dep.var.caption = "First Placement in Out of Home Care",
          covariate.labels = c("Black", "Male", "Under Age 3", "Age 3 to 8", "Finding-Level 1/2", "Finding-Level 3"),
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.2, .1, .05, .01),
          notes = c("+ p<0.2; . p<0.1; * p<0.05; ** p<0.01"))
# notes = "Black children include children identified as Black or children identified as Black-White Multi-Racial" 

## Results for predicted probability figure
newdata1 <- data.frame(race_black = c(0,1), male = 1, 
                       age2 = 0, age8 = 0, sub3 = "1")
newdata2 <- data.frame(race_black = c(0,1), male = 1, 
                       age2 = 0, age8 = 1, sub3 = "1")

pr_aff <- predict(place_fos2, newdata1, type="response")
pr_aff

pr_akc <- predict(place_kin2, newdata2, type="response")
pr_akc

pr_arc <- predict(place_res2, newdata1, type="response")
pr_arc

## Coefficients for coefficient plot
coef_aff <- tidy(place_fos2)
coef_akc <- tidy(place_kin2)
coef_arc <- tidy(place_res2)

coef_aff$wprob <- pr_aff[1]
coef_aff$bprob <- pr_aff[2]

coef_akc$wprob <- pr_akc[1]
coef_akc$bprob <- pr_akc[2]

coef_arc$wprob <- pr_arc[1]
coef_arc$bprob <- pr_arc[2]


######################################################################################
# 4. Count model, number of placements
######################################################################################
# reset levels, foster as baseline/omitted category
fc_ref <- fc_ref %>% 
  mutate(iplace4 = factor(iplace4, levels = c("Foster Family", "Kinship Care", "Residential (CRF)", "Other Placements")))

## count (poisson) model of placements: race only
place_num0 <- glm(num_placements_client ~ race_black,
                family = "poisson", data = fc)
summary(place_num0)
# race, gender, age
place_num1 <- glm(num_placements_client ~ race_black + male + age2 + age8, 
            family = "poisson", data = fc)
summary(place_num1)
# race, gender, age, substantiation, initial placement 
place_num2 <-  glm(num_placements_client ~ race_black + male + age2 + age8 + 
                    sub3 + iplace4, family = "poisson", data = fc_ref)
summary(place_num2)

## Generate table
stargazer(place_num0, place_num1, place_num2, title = "Number of Placements",
           covariate.labels = c("Black", "Male", "Under Age 3", "Age 3 to 8", 
                                "Finding-Level 1/2", "Finding-Level 3", "Kinship Care", "Residential", "Other Placement"),
           star.char = c("+", "*", "**", "***"),
           star.cutoffs = c(.2, .1, .05, .01),
           notes = c("+ p<0.2; * p<0.1; ** p<0.05; *** p<0.01"))

## Results for predicted probability figure
newdata1 <- data.frame(race_black = c(0,1), male = 1, 
                       age2 = 0, age8 = 0, sub3 = "1", iplace4 = "Foster Family")

num_p <- predict(place_num2, newdata1, type="response")
num_p

## Coefficients for coefficient plot
coef_nump <- tidy(place_num2)

coef_nump$wcount <- num_p[1]
coef_nump$bcount <- num_p[2]


######################################################################################
# 5. Duration model, overall time in foster care
######################################################################################
# Duration (Cox proportional hazard) model
library(survival)
# 0 if in the system, 1 if left the system
 
## Estimated survival function: race only
S <- Surv(fc$duration, fc$sys_status)
dur0 <- coxph(S ~ race_black, data = fc)
summary(dur0)
# race, gender, age
dur1 <- coxph(S ~ race_black + male + age2 + age8, data=fc)  
summary(dur1)  
# race, gender, age, substantiation, initial placement
S2 <- Surv(fc_ref$duration, fc_ref$sys_status) 
dur2 <- coxph(S2 ~ race_black + male + age2 + age8 + sub3 + iplace4, data=fc_ref)  
summary(dur2)  
 
# Plot of "survival" function
plot(survfit(dur1), xlab="Weeks", ylab="Cases Active")
race3_treat <- with(fc,
                     data.frame(
                       race_black = c(1,0),
                       male = c(1,1),
                       age2 = c(0,0),
                       age8 = c(1,1)))
plot(survfit(dur1, newdata = race3_treat), 
      xlab="Weeks", ylab="Cases Active",
      col = c("lightblue", "darkblue")) 

## Generate table
stargazer(dur0, dur1, dur2, title = "Duration of Out-of-Home Care",
          covariate.labels = c("Black", "Male", "Under Age 3", "Age 3 to 8", 
                               "Finding-Level 1/2", "Finding-Level 3", "Kinship Care", "Residential", "Other Placement"),
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.2, .1, .05, .01),
          notes = c("+ p<0.2; * p<0.1; ** p<0.05; *** p<0.01"))

## Results for predicted probability figure
newdata1 <- data.frame(race_black = c(0,1), male = 1, 
                       age2 = 0, age8 = 0, sub3 = "1", iplace4 = "Foster Family")
durmed <- survfit(dur2, newdata1)
durmed

plot(survfit(dur2, newdata1), fun = "event",
     xlab="Weeks", ylab="Probability of Exiting Foster Care",
     col = c("lightblue", "darkblue")) 

## Coefficients for coefficient plot
coef_dur <- tidy(dur2)


######################################################################################
# 6. Duration model, time in each placement
######################################################################################
# Set up survival
dummy_data_retrieval <- "2017-08-30 00:00:00"
fcph$pdura <- ifelse((fcph$place_status==1), 
                      difftime(dummy_data_retrieval, fcph$place_entry_date, units="weeks"), 
                      difftime(fcph$place_exit_date, fcph$place_entry_date, units="weeks"))

## Estimated survival function: race only
Sp <- Surv(fcph$pdura, fcph$place_status)
time0 <- coxph(Sp ~ race_black, data = fcph)
summary(time0)
# race, gender, age
time1 <- coxph(Sp ~ race_black + male + age2 + age8, data = fcph)
summary(time1)

# set up survival for joined data
fcph_ref$pdura <- ifelse((fcph_ref$place_status==1), 
                     difftime(dummy_data_retrieval, fcph_ref$place_entry_date, units="weeks"), 
                     difftime(fcph_ref$place_exit_date, fcph_ref$place_entry_date, units="weeks"))
# race, gender, age, substantiation, placement
Sp2 <- Surv(fcph_ref$pdura, fcph_ref$place_status)
time2 <- coxph(Sp2 ~ race_black + male + age2 + age8 + sub3 + place4, data = fcph_ref)
summary(time2)

## Generate table
stargazer(time0, time1, time2, title = "Duration of Each Placement",
           covariate.labels = c("Black", "Male", "Under Age 3", "Age 3 to 8",
                                "Finding-Level 1/2", "Finding-Level 3", "Kinship Care", "Other Placement", "Residential"),
           star.char = c("+", "*", "**", "***"),
           star.cutoffs = c(.2, .1, .05, .01),
           notes = c("+ p<0.2; * p<0.1; ** p<0.05; *** p<0.01"))

## Results for predicted probability figure
newdata1 <- data.frame(race_black = c(0,1), male = 1, 
                       age2 = 0, age8 = 0, sub3 = "1", place4 = "Foster Family")
timemed <- survfit(time2, newdata1)
timemed

plot(survfit(time2, newdata1), fun = "event",
     xlab="Weeks", ylab="Probability of Exiting Placement",
     col = c("lightblue", "darkblue")) 

## Coefficients for coefficient plot
coef_time <- tidy(time2)


######################################################################################
# 7. Create coefficients and predictions dataset
######################################################################################

# save analysis
setwd("/Volumes/NO NAME") 
save.image("fostercaremodel.RData")

# save coefficients, predictions
setwd("~/Box Sync/mpc/dataForDemocracy/pidl/PIDL2018/code/Group 2 Code/") # save predictions here
save(list = c("coef_aff", "coef_akc", "coef_arc", "coef_dur", "coef_ff", "coef_kc", "coef_nump", 
              "coef_rc", "coef_time", "dur2", "time2"), file = "predictions.Rdata")
