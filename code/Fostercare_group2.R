####################################
# DS 5559: Public Interest Data Lab 
# CVille Child Welfare Services Data 
# Read, Examine, Clean Foster_Care Data
# Group 2
# Updated: February 19, 2018 - Naifei Pan (nfp) and Michael Woon
####################################

## SET UP
rm(list=ls())
getwd() 
setwd("/Users/naifeipan/Desktop") 
setwd("/Volumes/data")

# Load libraries
library(tidyverse)
library(readxl)
library(forcats)

# Read in spreadsheets
foster <- read_excel("Copy of cville_dss_data_2017.xlsx", sheet = 3)

## INITIAL EXAMINATION
str(foster)
summary(foster)

# Table "character" variables
table(foster$RACE) 
table(foster$GENDER) 
table(foster$`Age at Custody`) 
table(foster$RACE,foster$GENDER) 

# Get rid of variable names with spaces: 
names(foster) <- c("locality", "cas_id", "client_id", "ssn",
                     "enter_care_date", "exit_care_date", "age_at_custody", "custody_date",
                     "custody_end", "placement_type", "discharge_date", "discharge_reason",
                     "gender", "race", "amer_indian",
                     "asian", "black", "pac_islander", "white", "race_unable",
                     "race_decline", "race_unknown", "face_to_face_count","face_to_face_home_count")

# Set categorical variables as factors 
foster <- foster %>% 
  mutate(age_at_custody = round(age_at_custody),
         ##nfp: I round the age here and didnot factor it 
         ##because I am going to analyze this variable later and num is earsier to manage.
         gender = factor(gender),
         race = factor(race),
         locality=factor(locality),
         cas_id=factor(cas_id),
         client_id=factor(client_id),
         ssn=factor(ssn),
         placement_type=factor(placement_type),
         discharge_reason=factor(discharge_reason))

##nfp: I factor all dates, but I am wondering if I can keep the date in POSIXct 
##becuase our group may want to do some research about the data,
##for instance, there may be more children enter the fostercare in September.
##and I feel POSIXct is easier to manage in this case.
## MPC: yes, dates should remain POSIXct across all of the data sets; I took them out of the mutate command

# Summarize again
summary(foster) 

## QUESTIONS, CLEANING

#are enter care data and custody date essentially the same?
##install.packages('compare')
library('compare')
compare(foster$enter_care_date,foster$custody_date)
##TRUE
##enter care data and custody date are the same.

#are exit care date and custody end (or discharge date) essentially the same?
compare(foster$exit_care_date,foster$discharge_date)
##FALSE
compare(foster$exit_care_date,foster$custody_end)
##TRUE
##exit care date and custody end are the same, 
##but exit care date and discharge date are different.

#there are more missing on discharge date than exit care/custoy end ... perhaps these are all recently ended cases?
##not sure how to check this question
##possible sultion:
##1) do we need to remove all the NA on exit care/custoy end or discharge date? 
##2) set all these NA equals to a sepcific recent date?
##3) random generate dates from recent period and assign them to the missing dates?

## EXPLORATION

##PART 1: Age at Custody   

##1. general info:

age_raw<-foster$age_at_custody
#remove nagative
age=age_raw[age_raw>=0]
#density plot
d <- density(age,bw=2)
plot(d, main="Age Density",xlim=c(0, 20))
##younger children are more likely getting into fostercare system, 
##especially children whose age are between 2 to 7.

##2. age & gender

female=subset(foster,gender=='Female')
male=subset(foster,gender=='Male')
d_female <- density(female$age_at_custody,bw=1)
d_male <- density(male$age_at_custody,bw=1)
plot(d_female, main="Gender & Age Density",xlim=c(0, 20),ylim=c(0,0.1),col=2)
lines(d_male, main="Gender & Age Density",xlim=c(0, 20),ylim=c(0,0.1),col=1)
legend("topright", 
       legend=c("Female","Male"), 
       fill=c(2,1))
##Before 8 years old, there are more boys in the fostercare system;
##After 8 years old, there are more girls in the fostercare system.

##3. race & age

race=foster$race
table(race)
## 2 unknown, leave it out
##Asian:
asian<-subset(foster,race=='Asian')
d_asian <- density(asian$age_at_custody,bw=1)
##only 5 value, so I use histogram
hist(asian$age_at_custody,main="Asian Age Histogram")
#Black:
black<-subset(foster,race=='Black')
d_black <- density(black$age_at_custody,bw=1)
plot(d_black, main="Black Age Density",xlim=c(0, 20))
#Multi:
multi<-subset(foster,race=='Multi-Race')
d_multi <- density(multi$age_at_custody,bw=1)
plot(d_multi, main="Multi-Race Age Density",xlim=c(0, 20))
#White:
white<-subset(foster,race=='White')
d_white <- density(white$age_at_custody,bw=1)
plot(d_white, main="White Age Density",xlim=c(0, 20))
##overall:
plot(d_black, main="Race & Age Density",xlim=c(0, 20),col=1,ylim=c(0,0.2))
lines(d_multi, main="Race & Age Density",xlim=c(0, 20),col=2,ylim=c(0,0.2))
lines(d_asian, main="Race & Age Density",xlim=c(0, 20),col=3,ylim=c(0,0.2))
lines(d_white, main="Race & Age Density",xlim=c(0, 20),col=4,ylim=c(0,0.2))
legend("topright", 
       legend=c("Black","Multi_race","Asian","White"), 
       fill=c(1,2,3,4))

##different race has different age distribution.

##PART 2:

#split into white vs non white
white <- filter(foster,white== 1)
#white has 103 members
nonwhite <- filter(foster, white == 0)
#nonwhite has 119 members

#histogram of reasons for discharge
#filter for when discharge reason is NA
white1 <- white
white1<-white1[!is.na(white1$discharge_reason),]
#now 57 rows

nonwhite1<- nonwhite[!is.na(nonwhite$discharge_reason),]
#59 rows

ggplot(data = white1) + geom_bar(mapping = aes(x = discharge_reason))
ggplot(data = nonwhite1) + geom_bar(mapping = aes(x = discharge_reason))

#histogram of placement types
white2<-white[!is.na(white1$placement_type),]
nonwhite2<- nonwhite[!is.na(nonwhite$placement_type),]
#full data
ggplot(data = white2) + geom_bar(mapping = aes(x = placement_type))
ggplot(data = nonwhite2) + geom_bar(mapping = aes(x = placement_type))

#enter care date vs custody date
datecheck <- foster %>% mutate(startdiff = difftime(enter_care_date, custody_date, units = "days"),
                               enddiff = difftime(exit_care_date, custody_end, units = "days"),
                               startdiffn = as.numeric(startdiff), # created as a diff object, turn it into numeric
                               enddiffn = as.numeric(enddiff))
summary(datecheck) 

save.image("foster.RData")





