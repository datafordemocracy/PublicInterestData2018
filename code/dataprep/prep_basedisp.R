######################################################################################
# DS 5559: Public Interest Data Lab 
# 1. Acquire 2012-2016 5-year Sex by Age ACS estimates for Cville
# 2. Generate Cville child population estimates by race, create childpop dataframe
# 3. Load referral data, ongoing cases data, foster care data; generate counts by race
# 4. Create combined data frames for analysis 
# Michele Claibourn (mclaibourn@virginia.edu)
# Updated: March 17, 2018 
######################################################################################

rm(list=ls())
library(tidyverse)
library(tidycensus)
library(readxl)
setwd("~/Box Sync/mpc/dataForDemocracy/pidl/PIDL2018/code/acsdata/")


######################################################################################
# 1. Acquire 2012-2016 5-year Sex by Age ACS estimates for Cville
######################################################################################
# request a key: https://api.census.gov/data/key_signup.html
# add it inside the quotes in the following command, if you use install = TRUE you shouldn't have to run it again

# census_api_key("", install = TRUE)
data(fips_codes) # built in dataset for looking up state and county
fips_codes %>% filter(state == "VA") # find county code for CVille

### a. get data: total population (combined) ###
cville_acs_all <- get_acs(geography = "county", table = "B01001", 
                          year = 2016, state = "VA", county = "540", survey = "acs5",
                          cache_table = TRUE)

### b. get data: White alone ###
cville_acs_white <- get_acs(geography = "county", table = "B01001A", 
                          year = 2016, state = "VA", county = "540", survey = "acs5",
                          cache_table = TRUE)

### c. get data: Black or African-Americanalone ###
cville_acs_black <- get_acs(geography = "county", table = "B01001B", 
                            year = 2016, state = "VA", county = "540", survey = "acs5",
                            cache_table = TRUE)

### d. get data: American Indian and Alaska Native alone ###
cville_acs_ai_an <- get_acs(geography = "county", table = "B01001C", 
                            year = 2016, state = "VA", county = "540", survey = "acs5",
                            cache_table = TRUE)

### e. get data: Asian alone ###
cville_acs_asian <- get_acs(geography = "county", table = "B01001D", 
                            year = 2016, state = "VA", county = "540", survey = "acs5",
                            cache_table = TRUE)

### f. get data: Native Hawaiian and Other Pacific Islander alone ###
cville_acs_nh_pi <- get_acs(geography = "county", table = "B01001E", 
                            year = 2016, state = "VA", county = "540", survey = "acs5",
                            cache_table = TRUE)

### g. get data: Some other race alone ###
cville_acs_other <- get_acs(geography = "county", table = "B01001F", 
                            year = 2016, state = "VA", county = "540", survey = "acs5",
                            cache_table = TRUE)

### h. get data: Two or more races ###
cville_acs_multi <- get_acs(geography = "county", table = "B01001G", 
                            year = 2016, state = "VA", county = "540", survey = "acs5",
                            cache_table = TRUE)

# save work
save.image("cville_acs.Rdata")


######################################################################################
# 2. Generate Cville child population estimates by race, create childpop dataframe
######################################################################################
# calculate number of children in cville with moe (age broken into finer intervals for total pop)
kids <- cville_acs_all[c(3:6,27:30),] %>% 
  summarize(estimate = sum(estimate), moe = moe_sum(moe, estimate)) %>% 
  mutate(race = "Total")

# function for generating and adding racial counts
childsum <- function(d,r){
  sum <- d[c(3:6,18:21),] %>% 
    summarize(estimate = sum(estimate), moe = moe_sum(moe, estimate)) %>% 
    mutate(race = r)
  tot <- rbind(kids, sum)
  }

# apply childsum function to cville_acs_xxxxx dataframes
kids <- childsum(cville_acs_white, "White")
kids <- childsum(cville_acs_black, "Black")
kids <- childsum(cville_acs_multi, "Multiracial")
kids <- childsum(cville_acs_ai_an, "ai_an")
kids <- childsum(cville_acs_asian, "asian")
kids <- childsum(cville_acs_nh_pi, "nh_pi")
kids <- childsum(cville_acs_other, "other")

### a. Cville 2012-2016 child pop estimates (race4 categories) ###
# add ai_an, asian, nh_pi, and other together
kids2 <- kids[5:8,] %>% 
  summarize(estimate = sum(estimate), moe = moe_sum(moe, estimate)) %>% 
  mutate(race = "Other")

childpop <- rbind(kids[1:4,], kids2) # add "Other" estimate to total, white, black estimates

# Calculate proportions
totpop <- as_vector(c(childpop[1,1], childpop[1,2]))
childpop <- childpop %>% 
  mutate(prop = estimate/totpop[1], pmoe = moe_prop(estimate, totpop[1], moe, totpop[2]))

### b. Cville 2012-2016 child pop estimates (race2 categories) ###
kids3 <- kids[3:8,] %>% # sum all groups except total and white
  summarize(estimate = sum(estimate), moe = moe_sum(moe, estimate)) %>% 
  mutate(race = "Children of Color")

childpop2 <- rbind(kids[1:2,], kids3) # add "Children of Color" to total, white

# Calculate proportions
childpop2 <- childpop2 %>% 
  mutate(race = recode(race, "White" = "White Children")) %>% 
  mutate(prop = estimate/totpop[1], pmoe = moe_prop(estimate, totpop[1], moe, totpop[2]))


######################################################################################
# 3. Load referral data, active cases data, foster care data;
#    generate referral counts, active case counts, current foster care counts by race
######################################################################################
setwd("/Volumes/NO NAME") 

### a. referral data ###
load("referral.RData") # created in explore_referraldata.R
summary(referral$ref_date) # verify earliest and latest referral date

# referral counts by race (race4 categories) by year
refyrpop <- referral %>% 
  mutate(source = ifelse(ref_date < as.POSIXct("2015-07-01 00:00:00"), "ref15", 
                          ifelse(ref_date > as.POSIXct("2016-06-30 00:00:00"), "ref17",
                          "ref16")), 
         race = recode(ethnicity, "Asian" = "Other", "Unknown" = "Other", "Multi-Race" = "Multiracial")) %>% 
  group_by(source, race) %>% 
  summarize(number = n_distinct(client_id)) %>% 
  mutate(moe = NA)

# referral counts by race (race2 categories) by year
refyrpop2 <- referral %>% 
  mutate(source = ifelse(ref_date < as.POSIXct("2015-07-01 00:00:00"), "ref15", 
                          ifelse(ref_date > as.POSIXct("2016-06-30 00:00:00"), "ref17",
                                 "ref16")), 
         race = recode(ethnicity, "Black" = "Children of Color", "Asian" = "Children of Color", "Unknown" = "Children of Color", "Multi-Race" = "Children of Color", "White" = "White Children")) %>% 
  group_by(source, race) %>% 
  summarize(number = n_distinct(client_id)) %>% 
  mutate(moe = NA)

# referral counts by race (race4 categories) 3 year total
refpop <- referral %>% 
  mutate(source = "ref", 
         race = recode(ethnicity, "Asian" = "Other", "Unknown" = "Other", "Multi-Race" = "Multiracial")) %>% 
  group_by(race) %>% 
  summarize(source = first(source), number = n_distinct(client_id)) %>% 
  mutate(moe = NA) %>% 
  select(source, everything())

# referral counts by race (race2 categories) 3 year total
refpop2 <- referral %>% 
  mutate(source = "ref",
    race = recode(ethnicity, 
                       "Black" = "Children of Color", "Asian" = "Children of Color", 
                       "Unknown" = "Children of Color", "Multi-Race" = "Children of Color", 
                       "White" = "White Children")) %>% 
  group_by(race) %>% 
  summarize(source = first(source), number = n_distinct(client_id)) %>% 
  mutate(moe = NA) %>% 
  select(source, everything())

### b. active cases data ###
active <- read_excel("cville_dss_data_2017.xlsx", sheet = 2) # load from spreadsheet

# active case counts by race (race4 categories)
activepop <- active %>% 
  rename(race = `Primary Ethnicity`, client_id = `Client ID`) %>% 
  mutate(race = recode(race, "Asian" = "Other", "Unknown" = "Other", "Multi-Race" = "Multiracial")) %>% 
  filter(`Client Involvement Start` > as.Date("2014-06-30")) %>% 
  group_by(race) %>% 
  summarize(number = n_distinct(client_id)) %>% 
  mutate(source = "act17", moe = NA) %>% 
  select(source, everything())

# active case counts by race (race2 categories)
activepop2 <- active %>% 
  rename(race = `Primary Ethnicity`, client_id = `Client ID`) %>% 
  mutate(race = recode(race, "Black" = "Children of Color", "Asian" = "Children of Color", "Unknown" = "Children of Color", "Multi-Race" = "Children of Color", "White" = "White Children")) %>% 
  filter(`Client Involvement Start` > as.Date("2014-06-30")) %>% 
  group_by(race) %>% 
  summarize(number = n_distinct(client_id)) %>% 
  mutate(source = "act17", moe = NA) %>% 
  select(source, everything())

### c. foster care data ###
foster <- read_excel("cville_dss_data_2017.xlsx", sheet = 3) # load from spreadsheet

# foster care case counts by race (race4 categories)
fosterpop <- foster %>% 
  rename(race = RACE, client_id = CL_ID, exit_date = `Exit Care Date`) %>% 
  mutate(race = recode(race, "Asian" = "Other", "Unknown" = "Other", "Multi-Race" = "Multiracial")) %>% 
  filter(`Enter Care Date` > as.Date("2014-06-30")) %>% # only cases entering foster care as of 7/1/14
#  filter(is.na(exit_date)) %>%  # only cases remaining in foster care as of June 30, 2017
  group_by(race) %>% 
  summarize(number = n_distinct(client_id)) %>% 
  mutate(source = "fos17", moe = NA) %>% 
  select(source, everything())

# foster care case counts by race (race2 categories)
fosterpop2 <- foster %>% 
  rename(race = RACE, client_id = CL_ID, exit_date = `Exit Care Date`) %>% 
  mutate(race = recode(race, "Black" = "Children of Color", "Asian" = "Children of Color", "Unknown" = "Children of Color", "Multi-Race" = "Children of Color", "White" = "White Children")) %>% 
  filter(`Enter Care Date` > as.Date("2014-06-30")) %>% # only cases entering foster care as of 7/1/14
  #  filter(is.na(exit_date)) %>%  # only cases remaining in foster care as of June 30, 2017
  group_by(race) %>% 
  summarize(number = n_distinct(client_id)) %>% 
  mutate(source = "fos17", moe = NA) %>% 
  select(source, everything())


######################################################################################
# 4. Create combined data frames for analysis: cwspopacs, acspopcws
######################################################################################
### a. reformat childpop to bind onto refyrpop, refpop, activepop, fosterpop (race4): cwspopacs (long) ###
childlong <- childpop %>% mutate(source = "acs16") %>% 
  rename(number = estimate) %>% 
  filter(race != "Total") %>% 
  select(source, race, number, moe)

# bind acs to cws data
cwspopacs4 <- bind_rows(childlong, refyrpop, refpop, activepop, fosterpop)
# make race a factor (and order for visualization)
cwspopacs4 <- cwspopacs4 %>% 
  mutate(race = factor(race, levels = c("White", "Other", "Multiracial", "Black")))

### b. reformat refyrpop, refpop, activepop, fosterpop to join onto childpop (race4): acspopcws (wide) ###
refyrpopwide <- refyrpop %>% select(source, race, number) %>% 
  spread(key = source, value = number)
refpopwide <- refpop %>% select(source, race, number) %>% 
  spread(key = source, value = number)
activepopwide <- activepop %>% select(source, race, number) %>% 
  spread(key = source, value = number)
fosterpopwide <- fosterpop %>% select(source, race, number) %>% 
  spread(key = source, value = number)

# generate total counts for each source 
refyrtot <- refyrpopwide %>% 
  summarize_at(vars(ref15:ref17), funs(sum)) %>% # get sum 
  mutate(race = "Total") %>% # name it "Total"
  select(race, everything()) # reorder columns

reftot <- refpopwide %>% 
  summarize_at(vars(ref), funs(sum)) %>% 
  mutate(race = "Total") %>% 
  select(race, ref)

acttot <- activepopwide %>% 
  summarize_at(vars(act17), funs(sum)) %>% 
  mutate(race = "Total") %>% 
  select(race, act17)

fostot <- fosterpopwide %>% 
  summarize_at(vars(fos17), funs(sum)) %>% 
  mutate(race = "Total") %>% 
  select(race, fos17)

# calculate proportions 
refyrpopwide <- refyrpopwide %>% 
  mutate(prop15 = ref15/refyrtot$ref15, prop16 = ref16/refyrtot$ref16, prop17 = ref17/refyrtot$ref17)
refyrpopwide <- bind_rows(refyrpopwide, refyrtot) # add "Total" as row

refpopwide <- refpopwide %>% 
  mutate(refprop = ref/reftot$ref)
refpopwide <- bind_rows(refpopwide, reftot)

activepopwide <- activepopwide %>% 
  mutate(actprop = act17/acttot$act17)
activepopwide <- bind_rows(activepopwide, acttot)

fosterpopwide <- fosterpopwide %>% 
  mutate(fosprop = fos17/fostot$fos17)
fosterpopwide <- bind_rows(fosterpopwide, fostot)

# join acs and cws
acspopcws4 <- left_join(childpop, refyrpopwide, by="race")
acspopcws4 <- left_join(acspopcws4, refpopwide, by="race")
acspopcws4 <- left_join(acspopcws4, activepopwide, by="race")
acspopcws4 <- left_join(acspopcws4, fosterpopwide, by="race")

### c. reformat childpop2 to bind onto cwspop2 (race2): cwspopacs2 (long) ###
childlong2 <- childpop2 %>% mutate(source = "acs16") %>% 
  rename(number = estimate) %>% 
  filter(race != "Total") %>% 
  select(source, race, number, moe)

# bind acs to cws
cwspopacs2 <- bind_rows(childlong2, refyrpop2, refpop2, activepop2, fosterpop2)
# make race a factor (and order for visualization)
cwspopacs2 <- cwspopacs2 %>% 
  mutate(race = factor(race, levels = c("White Children", "Children of Color")))

### d. reformat cwspop2 to join onto childpop2 (race2): acspopcws2 (wide) ##
refyrpopwide2 <- refyrpop2 %>% select(source, race, number) %>% 
  spread(key = source, value = number)
refpopwide2 <- refpop2 %>% select(source, race, number) %>% 
  spread(key = source, value = number)
activepopwide2 <- activepop2 %>% select(source, race, number) %>% 
  spread(key = source, value = number)
fosterpopwide2 <- fosterpop2 %>% select(source, race, number) %>% 
  spread(key = source, value = number)

# calculate proportions (use totals from above)
refyrpopwide2 <- refyrpopwide2 %>% 
  mutate(prop15 = ref15/refyrtot$ref15, prop16 = ref16/refyrtot$ref16, prop17 = ref17/refyrtot$ref17)
refyrpopwide2 <- bind_rows(refyrpopwide2, refyrtot) # add "Total" as row

refpopwide2 <- refpopwide2 %>% 
  mutate(refprop = ref/reftot$ref)
refpopwide2 <- bind_rows(refpopwide2, reftot)

activepopwide2 <- activepopwide2 %>% 
  mutate(actprop = act17/acttot$act17) 
activepopwide2 <- bind_rows(activepopwide2, acttot)

fosterpopwide2 <- fosterpopwide2 %>% 
  mutate(fosprop = fos17/fostot$fos17)
fosterpopwide2 <- bind_rows(fosterpopwide2, fostot)

# join acs and cws
acspopcws2 <- left_join(childpop2, refyrpopwide2, by="race")
acspopcws2 <- left_join(acspopcws2, refpopwide2, by="race")
acspopcws2 <- left_join(acspopcws2, activepopwide2, by="race")
acspopcws2 <- left_join(acspopcws2, fosterpopwide2, by="race")


# clean up and save work
rm(list = ls(pattern = "act")) 
rm(list = ls(pattern = "child")) 
rm(list = ls(pattern = "fos")) 
rm(list = ls(pattern = "kid")) 
rm(list = ls(pattern = "ref")) 
rm(fips_codes, p, totpop, yes_no_levels)

setwd("~/Box Sync/mpc/dataForDemocracy/pidl/PIDL2018/code/acsdata/")
save.image("cville_acs.Rdata")
# load("cville_acs.Rdata")

###############################################################################################
# Key data objects
# cwspopacs4: acs estimates and cws totals (referrals, ongoing, foster) by race4 in long format
# acspopcws4: acs estimates and cws totals (referrals, ongoing, foster) by race4 in wide format
# cwspopacs2: acs estimates and cws totals (referrals, ongoing, foster) by race2 in long format
# acspopcws2: acs estimates and cws totals (referrals, ongoing, foster) by race2 in wide format
###############################################################################################

