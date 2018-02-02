####################################
# DS 5559: Public Interest Data Lab 
# CVille Child Welfare Services Data 
# Read, Examine, Clean Data
# Michele Claibourn
# January 31, 2018
####################################


## Up front set up
# Clear workspace, set working directory
rm(list=ls())
getwd() # current (default) working directly
setwd("~/../../") # get to /
setwd("Volumes/NO NAME") # this may be different for others;
                         # I created my volume on the desktop

# Load libraries
# install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(forcats)

# Read in spreadsheets
referral <- read_excel("cville_dss_data_2017.xlsx", sheet = 1)
active <- read_excel("cville_dss_data_2017.xlsx", sheet = 2)
foster <- read_excel("cville_dss_data_2017.xlsx", sheet = 3)
placement <- read_excel("cville_dss_data_2017.xlsx", sheet = 4)
# ?read_excel 

## Examine data
str(referral) # structure
summary(referral)

# Can't summarize "character" variables; can table them
table(referral$Locality)
table(referral$`Screened Out`)
table(referral$`Screened Out`, referral$Accepted) 

# Return number of unique ids
n_distinct(referral$`Referral ID`)
n_distinct(referral$`Client ID`)
n_distinct(referral$`Case ID`)
n_distinct(referral$`SSN`)

# Get rid of variable names with spaces 
names(referral) <- c("locality", "ref_id", "client_id", "case_id", "ssn",
                     "ref_date", "screen_out", "accept", "invest_cat",
                     "disposition", "disp_date", "find_date", "close_date",
                     "gender", "ethnicity", "hispanic", "amer_indian",
                     "asian", "black", "pac_islander", "white", "race_unable",
                     "race_decline", "race_unknown", "age", "no_abuse", "neglect_medical",
                     "abuse_mental", "neglect_physical", "abuse_physical",
                     "abuse_sexual", "fatality", "near_fatal", "abuse_foster", "priority",
                     "first_meaningful", "first_contact", "reporter_relation",
                     "geo_id", "geo_name", "state", "county", "tract")

# Set categorical variables as factors 
yes_no_levels <- c("Y", "N") # set/order the levels
referral$screen_out <- factor(referral$screen_out, levels = yes_no_levels)
referral$accept <- factor(referral$accept, levels = yes_no_levels)
referral$fatality <- factor(referral$fatality, levels = yes_no_levels)
referral$near_fatal <- factor(referral$near_fatal, levels = yes_no_levels)

# Or let the response values become factors, ordered alphabetically
referral$invest_cat <- factor(referral$invest_cat)

# Same thing with piping and mutate
referral <- referral %>% 
  mutate(disposition = factor(disposition),
         gender = factor(gender),
         ethnicity = factor(ethnicity),
         hispanic = factor(hispanic),
         priority = factor(priority), 
         first_contact = factor(first_contact),
         reporter_relation = factor(reporter_relation),
         geo_name = factor(geo_name),
         tract2 = factor(tract))

# Summarize again
summary(referral) 
## Questions
# 1328 of 1556 missing dispositions were screened out; why are other 228 missing?
# Gender unknown? 15 of these were screened out, what about remaining 6?
# D and U in hispanic?
# Can missing on abuse/neglect variables reasonably be assumed to be a 0/absence?

# Do values make sense together: tables of two factor/categorical variables
with(referral, table(disposition, accept))
with(referral, table(invest_cat, accept))
with(referral, table(disposition, invest_cat))

# Accept, disposition by gender, ethnicity (prop.table)
with(referral, prop.table(table(gender, accept), 1))
with(referral, prop.table(table(ethnicity, accept), 1))
with(referral, prop.table(table(disposition, gender), 2))
with(referral, prop.table(table(disposition, ethnicity), 2))

# Composition of multi-racial
referral %>% filter(ethnicity=="Multi-Race") %>% 
  summarize(mean(asian), mean(black), 
            mean(pac_islander), mean(white), 
            mean(race_unknown))
      
# Average age by ethnicity
referral %>% group_by(ethnicity) %>% 
  summarize(mean(age))      
            
# Race and census tract
with(referral, prop.table(table(tract2, gender), 1))
with(referral, prop.table(table(tract2, ethnicity), 1))