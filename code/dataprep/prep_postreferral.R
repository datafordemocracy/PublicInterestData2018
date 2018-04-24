######################################################################################
# DS 5559: Public Interest Data Lab 
# Process and prepare referral and ongoing cases data
# 1. Read in and clean referral data for analysis
# 2. Read in and clean ongoing cases data for analysis
# 3. Link active cases to referral data where possible
# Michele Claibourn (mclaibourn@virginia.edu)
# Updated: April 2, 2018 
######################################################################################

rm(list=ls()) # clear workspace
library(tidyverse)
library(readxl)
library(lubridate)

setwd("/Volumes/NO NAME") # point to encrypted file

######################################################################################
# 1. Read in and clean referral data for analysis
######################################################################################
# Read in from spreadsheet
referral <- read_excel("cville_dss_data_2017.xlsx", sheet = 1)

# Get rid of variable names with spaces: 
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

# Set categorical variables as factors: assign levels
yes_no_levels <- c("Y", "N") # set/order the factor levels
referral$screen_out <- factor(referral$screen_out, levels = yes_no_levels) # apply levels to variable
referral$accept <- factor(referral$accept, levels = yes_no_levels)
referral$fatality <- factor(referral$fatality, levels = yes_no_levels)
referral$near_fatal <- factor(referral$near_fatal, levels = yes_no_levels)

# Set categorical variables as factors: let the response values become levels automatically, ordered alphabetically
referral$invest_cat <- factor(referral$invest_cat)

# Set categorical variables as factors: using piping
referral <- referral %>% 
  mutate(disposition = factor(disposition),
         gender = factor(gender),
         ethnicity = factor(ethnicity),
         hispanic = factor(hispanic),
         priority = factor(priority), 
         reporter_relation = factor(reporter_relation),
         geo_name = factor(geo_name),
         tract2 = factor(tract))
# Recode hispanic -- D and U as N (hispanic =  "known" hispanic origin)
referral$hispanic <- recode(referral$hispanic, "D" = "N", "U" = "N")

# Create race4 and race2 variables
referral <- referral %>% 
  mutate(race4 = recode(ethnicity, "Asian" = "Other", "Unknown" = "Other", "Multi-Race"="Multiracial"), # leave White, Black, alone; recode Asian and Other to Other
         race2 = recode(ethnicity, "Black" = "Children of Color", "Asian" = "Children of Color", 
                        "Unknown" = "Children of Color", "Multi-Race" = "Children of Color", 
                        "White" = "White Children"),
         race3 = recode(race4, "Black" = "Black/Other", "Asian" = "Black/Other", "Unknown" = "Black/Other", "Other" = "Black/Other"),
         race4 = fct_relevel(race4, levels = "Black", "Multiracial", "Other", "White"))

# Create multi-racial categories
referral <- referral %>% 
  mutate(blackwhite = ifelse(black == 1 & white == 1, 1, 0),
         asianwhite = ifelse(asian == 1 & white == 1, 1, 0),
         asianblack = ifelse(asian == 1 & white == 1, 1, 0),
         paciswhite = ifelse(pac_islander == 1 & white == 1, 1, 0))

# Collapse reporter/referrer into fewer categories
# Initially has 47, reduce to 6 
referral <- referral %>% 
  mutate(relation6 = reporter_relation,
         relation6 = fct_collapse(relation6,
                                  "Educational" = c("Teacher", "School Staff", "School-Private", "School-Public", "Day Care Provider", "Day Care Staff", "Sports organizations","Youth recreation programs/camps"),
                                  "Governmental" = c("DSS Staff", "Eligibility Worker", "Family Services Specialist", "Government Agency", "Licensing Worker", "Social Services-Private", "Social Services-Public", "Social Worker"),
                                  "Family/Care/Neighbor" = c("Clergy", "Babysitter", "Substitute Care Provider", "Friend", "Landlord", "Neighbor", "Foster Parent", "Parent", "Relative", "Self Referred", "Self Referred - Alleged Abuser/Neglector", "Self Referred - Alleged Victim"), 
                                  "Medical" = c("Emergency Medical Services", "Hospital/Clinic", "Medical Professional", "Medical Professional, Other", "Nurse-Private", "Physician-Private", "Counselor/Therapist", "Mental Health-Public or Private"),
                                  "Judicial" = c("Attorney", "Child Advocate/CASA", "Court/Probation", "Guardian Ad Litem", "Judge/Court Srvcs", "Law Enforcement", "Probation Officer"),
                                  "Other" = c("No Relationship", "Other Relationship", "Private Individual", "Unknown")))

# Generate count of prior referrals per client in three year period
referral <- referral %>% arrange(client_id, ref_date) %>% 
  group_by(client_id) %>%
  mutate(refprior = row_number() - 1) %>% 
  ungroup()

# Combine in home investigation (317 cases) and out of family investigation (only 15 cases)
referral <- referral %>% 
  mutate(invest2 = fct_recode(invest_cat, "Assessment" = "Family Assessment",
                              "Investigation" = "In Home Investigation",
                              "Investigation" = "Out Of Family Investigation"))

# Create combined disposition for viz:
# remove pending and appealed, add unable to complete to unfounded, proportional stacked bar
referral <- referral %>% 
  mutate(disp4 = fct_recode(disposition, "Level 1" = "Founded - Level 1",
                            "Level 2" = "Founded - Level 2",
                            "Level 3" = "Founded - Level 3",
                            "Unfounded" = "Unfounded - lack of evidence",
                            "Unfounded" = "Unable to Complete Investigation",
                            NULL = "Appealed",
                            NULL = "Pending",
                            NULL = "DRS"),
         disp4 = fct_relevel(disp4, levels = "Unfounded", "Level 1", "Level 2", "Level 3"))

# Create binary gender
referral <- referral %>% 
  mutate(female = fct_recode(gender, NULL = "Unknown"))


######################################################################################
# 2. Read in and clean ongoing cases data for analysis
######################################################################################
# Read in from spreadsheet
active <- read_excel("cville_dss_data_2017.xlsx", sheet = 2)

# Get rid of variable names with spaces: 
names(active) <- c("locality", "type", "case_id", "client_id", "ssn", "age",
                   "case_begin", "case_end", "client_start", "client_end", "gender",
                   "ethnicity", "hispanic", "amer_indian", "asian", "black", 
                   "pac_islander", "white", "race_unable", "race_decline", 
                   "race_unknown", "contact_count")

# Set categorical variables as factors: using piping
active <- active %>% 
  mutate(gender = factor(gender),
         ethnicity = factor(ethnicity),
         hispanic = factor(hispanic))

# Create race4 and race2 variables
active <- active %>% 
  mutate(race4 = recode(ethnicity, "Asian" = "Other", "Unknown" = "Other", "Multi-Race" = "Multiracial"), # leave White, Black alone; recode Asian and Other to Other
         race2 = recode(ethnicity, "Black" = "Children of Color", "Asian" = "Children of Color", 
                        "Unknown" = "Children of Color", "Multi-Race" = "Children of Color", 
                        "White" = "White Children"),
         race4 = fct_relevel(race4, levels = "Black", "Multiracial", "Other", "White"))

# Create multi-racial categories
active <- active %>% 
  mutate(blackwhite = ifelse(black == 1 & white == 1, 1, 0),
         asianwhite = ifelse(asian == 1 & white == 1, 1, 0),
         asianblack = ifelse(asian == 1 & white == 1, 1, 0),
         paciswhite = ifelse(pac_islander == 1 & white == 1, 1, 0))

# Retain only cases entering from July 1, 2014 and after
active <- active %>% filter(case_begin > as.Date("2014-06-30"))


######################################################################################
# 3. Link active cases to referral data where possible
######################################################################################
act <- unique(active$client_id) # 313 unique cients out of 317 records
ref <- unique(referral$client_id) # 1325 unique referrals
sum(act %in% ref) # 203 matching client ids

reduced_referral <- referral %>% 
  select(client_id:ref_date, accept:disposition, find_date:abuse_sexual, 
         priority:first_contact, tract:refprior, race4:race3, relation6:female) %>% 
  filter(accept == "Y") %>% 
  mutate(timeq = quarter(first_contact, with_year = TRUE))
active <- active %>% 
  mutate(timeq = quarter(client_start, with_year = TRUE))
active_referral <- left_join(active, reduced_referral, by = c("client_id", "timeq"))

# try matching by joining referral to active, taking difftime between client_start and first_contact, choose minimum positive difference as match
referral_active <- left_join(reduced_referral, active, by = "client_id")
referral_active <- referral_active %>% 
  mutate(timegap = client_start - first_contact) %>% 
  filter(!(is.na(timegap)))
# select unique client ids with smallest time gap
referral_active <- referral_active %>% 
  group_by(client_id) %>% slice(which.min(timegap))
  
# save data for analysis
rm(reduced_referral, act, ref, yes_no_levels)
save.image("postreferral.RData")
