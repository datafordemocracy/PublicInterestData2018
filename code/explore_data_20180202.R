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
setwd("~/../../") # to get to /
setwd("Volumes/NO NAME") # this may be different for others;
                         # I created my VeraCrypt volume on the desktop

# Load libraries
# install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(forcats)

# Read in spreadsheets: assign data to object name
referral <- read_excel("cville_dss_data_2017.xlsx", sheet = 1) # "referral is now the name of the data frame
ongoing <- read_excel("cville_dss_data_2017.xlsx", sheet = 2)
foster <- read_excel("cville_dss_data_2017.xlsx", sheet = 3)
placement <- read_excel("cville_dss_data_2017.xlsx", sheet = 4)
# ?read_excel 

## Examine data
str(referral) # structure, are values recorded as integers, characters, factors, dates, etc.
summary(referral) # summary statistics, depends on variable type

# Can't summarize "character" variables; can table them
# Call variables from a data frame after the $
table(referral$Locality) # after typing $, hit your tab key; RStudio will show available variables
table(referral$`Screened Out`) # variable names with spaces must be inside backticks
table(referral$`Screened Out`, referral$Accepted) # cross-tab

# Return number of unique ids
n_distinct(referral$`Referral ID`)
n_distinct(referral$`Client ID`)
n_distinct(referral$`Case ID`)
n_distinct(referral$`SSN`)

# Change variable names (to remove spaces and capital letters)
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
# name function refers to variable names; 
# this assigns the names in the vector (created with the c() or combine function) to the data frame fields in order.
# If I were only rename a few variables, I'd use dplyr::rename(referral, ref_id = `Referral ID`, client_id = `Client ID`)

# Set categorical variables as factors (initially read as characters)
yes_no_levels <- c("Y", "N") # set/order the levels (these must match values in the variable; I'm just enforcing a sequence)
referral$screen_out <- factor(referral$screen_out, levels = yes_no_levels) # apply the levels
referral$accept <- factor(referral$accept, levels = yes_no_levels)
referral$fatality <- factor(referral$fatality, levels = yes_no_levels)
referral$near_fatal <- factor(referral$near_fatal, levels = yes_no_levels)

# Or let the response values become factors automatically, sequenced alphabetically
referral$invest_cat <- factor(referral$invest_cat)

# Do the same thing with piping and mutate
referral <- referral %>% # cmd/ctrl-shift-enter produces the pipe
  mutate(disposition = factor(disposition),
         gender = factor(gender),
         ethnicity = factor(ethnicity),
         hispanic = factor(hispanic),
         priority = factor(priority), 
         first_contact = factor(first_contact),
         reporter_relation = factor(reporter_relation),
         geo_name = factor(geo_name),
         tract2 = factor(tract))

# Summarize again; more useful now
summary(referral) 

## Questions from summary
# 1328 of 1556 missing dispositions were screened out; why are other 228 missing?
# Gender unknown? 15 of these were screened out, what about remaining 6?
# 1 D in hispanice; 29 U in hispanic (possibly unknown)
# Can missing on abuse/neglect variables reasonably be assumed to be a 0/absence?

# Validation
# Do values make sense together: tables of two factor/categorical variables
with(referral, table(disposition, accept)) # wrapping in with() means you dont have to type the data frame before each variable name 
with(referral, table(invest_cat, accept))
with(referral, table(disposition, invest_cat))

# Examining the relation of two factors
# Accept, disposition by gender, ethnicity (prop.table to generate marginal proportions)
with(referral, prop.table(table(gender, accept), 1)) # the 1 generates row percentages
with(referral, prop.table(table(ethnicity, accept), 1))
with(referral, prop.table(table(disposition, gender), 2)) # the two generates column percentages
with(referral, prop.table(table(disposition, ethnicity), 2))

# Composition of multi-racial: among multi-racial, what percent are ...
referral %>% filter(ethnicity=="Multi-Race") %>% 
  summarize(mean(asian), mean(black), 
            mean(pac_islander), mean(white), 
            mean(race_unknown))
      
# Average age by ethnicity
referral %>% group_by(ethnicity) %>% 
  summarize(mean(age))      
            
# Race/gender by census tract
with(referral, prop.table(table(tract2, gender), 1))
with(referral, prop.table(table(tract2, ethnicity), 1))
