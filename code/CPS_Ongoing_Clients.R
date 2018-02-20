####################################
# DS 5559: Public Interest Data Lab 
# CVille Child Welfare Services Data 
# Read, Examine, Clean Ongoing Data
# James, Hanna, Alicia
# Updated: February 16, 2018
####################################

# Set up WD with encrypted location
rm(list=ls())
getwd()
setwd("/Volumes/NO NAME")

library(tidyverse)
library(readxl)
library(forcats)

# Read data
ongoing <- read_excel("cville_dss_data_2017.xlsx", sheet = 2)

# Rename variables
names(ongoing) <- c("locality", "case_type", "case_id", "client_id", "ssn", "age_at_involvement",
                    "case_type_begin", "case_type_end", "client_involvment_start", "client_involvement_end",
                    "gender", "primary_ethnicity", "hispanic", "amer_indian", "asian", "black", "pac_islander",
                    "white", "race_unable", "race_decline", "race_unknown", "face_to_face_cnt")

# Reformat categorical variables into factors
ongoing$gender <- factor(ongoing$gender)
ongoing$primary_ethnicity <- factor(ongoing$primary_ethnicity)
ongoing$hispanic <- factor(ongoing$hispanic)

# can do some further analysis on white vs non white or whatever one might require.
white <- filter(ongoing, white == 1)
nonwhite <- filter(ongoing, white == 0)

# Check data frame
summary(ongoing) 


# Can we reasonably assume NAs on client_involvement_end are ongoing?
open <- ongoing %>% filter(is.na(client_involvement_end))
summary(open) # this raised more questions than it answered -- 
              # the case and client begin/end dates are sometimes wildly different

# Calculate the time difference between the two end dates and two start dates
datecheck <- ongoing %>% mutate(startdiff = difftime(case_type_begin, client_involvment_start, units = "days"),
                                enddiff = difftime(case_type_end, client_involvement_end, units = "days"),
                                startdiffn = as.numeric(startdiff), # created as a diff object, turn it into numeric
                                enddiffn = as.numeric(enddiff))
summary(datecheck) 
# we're going to need to understand more clearly which of case and client dates is more appropriate to use 
# to capture, for instance, duration in system. Initially hoped that end date on one could be used to impute
# a  missing on the other, as they missingness does not perfectly overlap on the two end dates. But less certain
# after seeing the range of time difference across these.

# primary_ethnicity not missing for any observation, yet race_decline and race_unknown have 41 missing?
checkrace <- ongoing %>% filter(is.na(race_unknown))
summary(checkrace)
# both variables are missing on same observations; presume we can take primary_ethnicity (and binary indicators) at face value?

## SAVE
save.image("ongoing.RData")
# load("ongoing.RData")

# JM9HP
# Since the race_decline and race_unknown still have primary_ethnicity binary indicators, 
# perhaps we can look at the method of referral and see if the race is unknown for the inital report.


