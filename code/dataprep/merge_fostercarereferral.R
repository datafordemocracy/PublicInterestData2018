######################################################################################
# DS 5559: Public Interest Data Lab 
# Data Merging Script: Foster care data and referral data
# 1. Reduce data and join
# 2. Explore meregeddata
# 3. Merge the data
# Created by C. McClintock
# Contact: Michele Claibourn (mclaibourn@virginia.edu)
# Updated: May 10, 2018 mpc
######################################################################################

rm(list=ls())
library(tidyverse)
library(lubridate)

setwd("/Volumes/NO NAME") # point to encrypted file
# setwd("/Volumes/encrypted_data") # encrypted file on different computer
load("visualizations.Rdata")
load("referral.Rdata")


######################################################################################
# 1. Reduce data and join
######################################################################################
# keep relevant variables
ref <- select(referral, ref_id, case_id, client_id, ssn, ref_date, screen_out, accept, 
              invest_cat, disposition, disp_date, find_date, close_date, no_abuse, neglect_medical,  neglect_medical,     
              abuse_mental, neglect_physical, abuse_physical , abuse_sexual, fatality,          
              near_fatal, abuse_foster, priority, first_meaningful,  
              first_contact, reporter_relation,   geo_id, geo_name,          
              state, county , tract, tract2, relation11)

# keep only observations in both
cws <- left_join(fc, ref, by = c("case_id", "client_id"))


######################################################################################
# 2. Explore data
######################################################################################
# number of rows, distinct observations
n_distinct(cws$case_id) # 66 case ids
n_distinct(cws$client_id) # 115 client ids

# calculate time between different dates in the data to evalute method of matching 
cws$time_find2care<- difftime(cws$sys_entry, cws$find_date, units="weeks") # between finding date and system entry
cws$time_ref2find <- difftime(cws$find_date, cws$ref_date, units="weeks") # between finding date and referral date
cws$time_ref2care <- difftime(cws$sys_entry, cws$ref_date, units="weeks") # between referral and system entry 

# filter out the cases screened out, we know these did not lead to the foster care placements
cws <- subset(cws, cws$accept=="Y")
n_distinct(cws$case_id) # 56 distinct case ids
n_distinct(cws$client_id) # 93 distinct client ids

# difference between referral and system entry 
ggplot(cws, aes(x=as.numeric(time_ref2care))) + geom_density()
# difference between finding date and system entry
ggplot(cws, aes(x=as.numeric(time_find2care))) + geom_density()
# difference between finding date and referral date
ggplot(cws, aes(x=as.numeric(time_ref2find))) + geom_density()

# arrange for viewing
cws <- arrange(cws, client_id, ref_date, sys_entry, find_date)

# limit cases to findings
cwsfind <- subset(cws, !is.na(find_date))

# how many clients are left?
n_distinct(cwsfind$client_id) # 75 clients, down from 93, 
# 17 clients are in care by don't have finding dates

# Are the finding dates just missing or did they not have investigations?
summary(as.numeric(cwsfind$time_ref2care))
summary(as.numeric(cwsfind$time_find2care)) 
summary(as.numeric(cwsfind$time_ref2find))

# difference between referral and system entry 
ggplot(cwsfind, aes(x=as.numeric(time_ref2care))) + geom_density()

# arrange for viewing
cwsfind <- select(cwsfind, case_id, client_id, ref_date, time_ref2find, find_date, time_find2care, 
                    sys_entry,  time_ref2care, everything())


######################################################################################
# 3. Merge the data
######################################################################################
# by minimum positive referral to foster care placement
cws <- subset(cws, time_ref2care>=0)
n_distinct(cws$client_id) # 60 clients, down from 75 

fc_ref <- cws %>%
  group_by(client_id) %>% slice(which.min(abs(time_ref2care)))

fc_ref <- select(fc_ref, case_id, client_id, ref_date, time_ref2find, find_date, time_find2care, 
                   sys_entry,  time_ref2care, everything())

# examine potential controls
table(fc_ref$invest_cat)
table(fc_ref$disposition)

fc_ref <- mutate(fc_ref, 
                 disposition = fct_recode(disposition,
                                           "Not Substantiated" = "Unfounded - lack of evidence",
                                           "Substantiated" = "Appealed",
                                           "Not Substantiated" =  "DRS",
                                           "Substantiated"  = "Founded - Level 1",
                                           "Substantiated"  = "Founded - Level 2",
                                           "Substantiated - Extreme"  = "Founded - Level 3"
                                           ))


# NOTES: 
# 115 clients in the referral/foster care merge
# 93 clients once we limit the data to only cases that are screened in # ??
# 75 clients with finding dates
# 60 clients with system entry dates after referrals

# clean up and save image to use for analysis
save.image("mergecheck.Rdata")




