####################################
# DS 5559: Public Interest Data Lab 
# CVille Child Welfare Services Data 
# Read, Examine, Clean Referral Data
# Michele Claibourn
# Updated: February 8, 2018 - mpc
####################################


## SET UP
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
# active <- read_excel("cville_dss_data_2017.xlsx", sheet = 2)
# foster <- read_excel("cville_dss_data_2017.xlsx", sheet = 3)
# placement <- read_excel("cville_dss_data_2017.xlsx", sheet = 4)


## INITIAL EXAMINATION
str(referral) # structure
summary(referral)

# Can't summarize "character" variables; can table them
# note the construction data$variable to call a variable/column
table(referral$Locality) # frequency table
table(referral$`Screened Out`)
table(referral$`Screened Out`, referral$Accepted) # cross-tabulation

# Get rid of variable names with spaces: 
# names() gets or sets the (variable) names of (data) object
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
yes_no_levels <- c("Y", "N") # set/order the factor levels
referral$screen_out <- factor(referral$screen_out, levels = yes_no_levels) # apply levels to variable
referral$accept <- factor(referral$accept, levels = yes_no_levels)
referral$fatality <- factor(referral$fatality, levels = yes_no_levels)
referral$near_fatal <- factor(referral$near_fatal, levels = yes_no_levels)

# Or let the response values become factors automatically, ordered alphabetically
referral$invest_cat <- factor(referral$invest_cat)

# Same thing as above with piping and mutate
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


## QUESTIONS, CLEANING
# D and U in hispanic?
# Recode hispanic -- D and U as N (so variables becomes: known hispanic origin)
referral$hispanic <- recode(referral$hispanic, "D" = "N", "U" = "N")

# Can missing on abuse/neglect variables reasonably be assumed to be a 0/absence?
checkmiss_abuse <- referral %>% 
  select(client_id, no_abuse:abuse_sexual)
checkmiss_abuse <- checkmiss_abuse[!complete.cases(checkmiss_abuse),]
checkmiss_abuse2 <- referral %>% 
  filter(client_id == checkmiss_abuse$client_id)
# no, this client was screened in/accepted

# 1328 of 1556 missing dispositions were screened out; why are other 228 missing?
# should invest_cat = Family Assessment imply disposition == DRS?
with(referral, table(invest_cat, disposition))
# Apparently. Replace disposition with DRS 
# if disposition is missing and invest_cat is Family Assessment
referral <- referral %>% 
  mutate(disposition = factor(if_else(is.na(disposition) & invest_cat == "Family Assessment",
                                      "DRS", as.character(disposition))))
# recoding factors requires converting to character, recoding, converting back to factor                           
# Check remaining missing dispositions
checkmiss_disp <- referral %>% 
  filter(is.na(disposition) & accept == "Y")
# are the remaining 17 observations simply not yet closed?

# Gender unknown? 15 of these were screened out, what about remaining 6?
checkmiss_gender <- referral %>% 
  filter(gender == "Unknown")
# Are any of these client_ids present multiple times in the dataset,
# e.g., so I can fill in missing gender with another record on same client?
checkmiss_gender2 <- referral %>% 
  filter(client_id %in% checkmiss_gender$client_id)
# nope 
# note the %in%: this looks for any client_id in referral that matches
# a value in checkmiss_gender$client_id

# Do values make sense together?
# tables of two factor/categorical variables that should be mutually exclusive
with(referral, table(disposition, accept))
with(referral, table(invest_cat, accept))
with(referral, table(disposition, invest_cat))

# Number of unique ids
n_distinct(referral$ref_id)
n_distinct(referral$client_id)
n_distinct(referral$case_id)
n_distinct(referral$ssn)


## EXPLORATION
# Accept, disposition by gender, ethnicity 
# this time as proportion, prop.table
with(referral, prop.table(table(gender, accept), 1))
with(referral, prop.table(table(ethnicity, accept), 1))
with(referral, prop.table(table(disposition, gender), 2))
with(referral, prop.table(table(disposition, ethnicity), 2))
# 1 gives row proportions (the sum across the rows is 1, or the first named variable)
# 2 gives column proportions (the sume down the columns is 1, or the second named variable)

# Racial composition of multi-racial
referral %>% filter(ethnicity=="Multi-Race") %>% 
  summarize(mean(asian), mean(black), 
            mean(pac_islander), mean(white), 
            mean(race_unknown))
      
# Average age by ethnicity
referral %>% group_by(ethnicity) %>% 
  summarize(mean(age))      
            
# Race and gender by census tract
with(referral, prop.table(table(tract2, gender), 1)) # should be approx. uniform
with(referral, prop.table(table(tract2, ethnicity), 1)) # should vary based on 
# the first (gender) shoudl be approx uniform (assuming boys and girls reside in tracts in equal proportions)
# the second (race) should vary based on racial composition of tract

# More exploration
# How many times does each referral id appear
referrers <- referral %>% 
  group_by(ref_id) %>% 
  summarize(numrefs = n(), relation = first(reporter_relation))
ggplot(data = referrers, aes(x = numrefs)) + geom_bar()

# Reporter Relation by race of child referred
table(referral$reporter_relation)
# Recode reporter relation into fewer categories (first cut, need to think more about this)
# Initially has 47, reduce to 11 (probably still too many)
referral <- referral %>% mutate(relation11 = reporter_relation)
referral$relation11 <- plyr::revalue(referral$relation11, 
                                     c("Attorney" = "Judicial", 
                                       "Child Advocate/CASA" = "Judicial",
                                       "Court/Probation" = "Judicial",
                                       "Guardian Ad Litem" = "Judicial",
                                       "Judge/Court Srvcs" = "Judicial",
                                       "Emergency Medical Services" = "Medical",
                                       "Hospital/Clinic" = "Medical", 
                                       "Medical Professional" = "Medical",
                                       "Medical Professional, Other" = "Medical",
                                       "Nurse-Private" = "Medical",
                                       "Physician-Private" = "Medical",
                                       "Teacher" = "Educational",
                                       "School Staff" = "Educational",
                                       "School-Private" = "Educational",
                                       "School-Public" = "Educational",
                                       "Clergy" = "Counselor",
                                       "Counselor/Therapist" = "Counselor",
                                       "Mental Health-Public or Private" = "Counselor",
                                       "Law Enforcement" = "Carceral",
                                       "Probation Officer" = "Carceral",
                                       "Babysitter" = "Carer",
                                       "Day Care Provider" = "Carer",
                                       "Day Care Staff" = "Carer",
                                       "Substitute Care Provider" = "Carer",
                                       "Sports organizations" = "Carer",
                                       "Youth recreation programs/camps" = "Carer",
                                       "DSS Staff" = "Governmental",
                                       "Eligibility Worker" = "Governmental",
                                       "Family Services Specialist" = "Governmental",
                                       "Government Agency" = "Governmental",
                                       "Licensing Worker" = "Governmental",
                                       "Social Services-Private" = "Governmental",
                                       "Social Services-Public" = "Governmental",
                                       "Social Worker" = "Governmental",
                                       "Friend" = "Neighbor",
                                       "Landlord" = "Neighbor",
                                       "Neighbor" = "Neighbor",
                                       "Foster Parent" = "Relative",
                                       "Parent" = "Relative",
                                       "Relative" = "Relative",
                                       "No Relationship" = "Other",
                                       "Other Relationship" = "Other",
                                       "Private Individual" = "Other",
                                       "Unknown" = "Other",
                                       "Self Referred" = "Self",
                                       "Self Referred - Alleged Abuser/Neglector" = "Self",
                                       "Self Referred - Alleged Victim" = "Self"))
table(referral$relation11)
with(referral, table(relation11, ethnicity))

p <- ggplot(data = referral, aes(x = fct_infreq(relation11)))
p + geom_bar(aes(fill = ethnicity)) + coord_flip()
p + geom_bar(aes(fill = ethnicity), position = "fill") + coord_flip() # 


## CLEAN UP, SAVE
rm(checkmiss_abuse, checkmiss_abuse2, checkmiss_disp, checkmiss_gender, checkmiss_gender2)
save.image("referral.RData")
# load("referral.RData)

