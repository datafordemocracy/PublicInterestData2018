####################################################################################
# DS 5559: Public Interest Data Lab 
# 1. Acquire 2012-2016 5-year Sex by Age ACS estimates for Cville
# 2. Generate Cville child population estimates by race, create childpop dataframe
# 3. Load referral data, generate referral counts by race, add to childpop df
# Michele Claibourn (mclaibourn@virginia.edu)
# Updated: March 9, 2018 
####################################################################################

rm(list=ls())
setwd("~/Box Sync/mpc/dataForDemocracy/pidl/PIDL2018/code/acsdata/")
library(tidyverse)
library(tidycensus)


####################################################################################
# 1. Acquire 2012-2016 5-year Sex by Age ACS estimates for Cville
####################################################################################
# request a key: https://api.census.gov/data/key_signup.html
# add it inside the quotes in the following command, if you use install = TRUE you shouldn't have to run it again

# census_api_key("", install = TRUE)
data(fips_codes) # built in dataset for looking up state and county
fips_codes %>% filter(state == "VA") # find county code for CVille

# a. get data: total population (combined)
cville_acs_all <- get_acs(geography = "county", table = "B01001", 
                          year = 2016, state = "VA", county = "540", survey = "acs5",
                          cache_table = TRUE)

# b. get data: White alone
cville_acs_white <- get_acs(geography = "county", table = "B01001A", 
                          year = 2016, state = "VA", county = "540", survey = "acs5",
                          cache_table = TRUE)

# c. get data: Black or African-Americanalone
cville_acs_black <- get_acs(geography = "county", table = "B01001B", 
                            year = 2016, state = "VA", county = "540", survey = "acs5",
                            cache_table = TRUE)

# d. get data: American Indian and Alaska Native alone
cville_acs_ai_an <- get_acs(geography = "county", table = "B01001C", 
                            year = 2016, state = "VA", county = "540", survey = "acs5",
                            cache_table = TRUE)

# e. get data: Asian alone
cville_acs_asian <- get_acs(geography = "county", table = "B01001D", 
                            year = 2016, state = "VA", county = "540", survey = "acs5",
                            cache_table = TRUE)

# f. get data: Native Hawaiian and Other Pacific Islander alone
cville_acs_nh_pi <- get_acs(geography = "county", table = "B01001E", 
                            year = 2016, state = "VA", county = "540", survey = "acs5",
                            cache_table = TRUE)

# g. get data: Some other race alone
cville_acs_other <- get_acs(geography = "county", table = "B01001F", 
                            year = 2016, state = "VA", county = "540", survey = "acs5",
                            cache_table = TRUE)

# h. get data: Two or more races
cville_acs_multi <- get_acs(geography = "county", table = "B01001G", 
                            year = 2016, state = "VA", county = "540", survey = "acs5",
                            cache_table = TRUE)

# save work
save.image("cville_acs.Rdata")


####################################################################################
# 2. Generate Cville child population estimates by race, create childpop dataframe
####################################################################################
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
kids <- childsum(cville_acs_multi, "Multi")
kids <- childsum(cville_acs_ai_an, "ai_an")
kids <- childsum(cville_acs_asian, "asian")
kids <- childsum(cville_acs_nh_pi, "nh_pi")
kids <- childsum(cville_acs_other, "other")

####################################################################################
#   a. Cville 2012-2016 child pop estimates (race4 categories)
####################################################################################
# add ai_an, asian, nh_pi, and other together
kids2 <- kids[5:8,] %>% 
  summarize(estimate = sum(estimate), moe = moe_sum(moe, estimate)) %>% 
  mutate(race = "Other")

childpop <- rbind(kids[1:4,], kids2) # add "Other" estimate to total, white, black estimates

# Calculate proportions
totpop <- as_vector(c(childpop[1,1], childpop[1,2]))
childpop <- childpop %>% 
  mutate(prop = estimate/totpop[1], pmoe = moe_prop(estimate, totpop[1], moe, totpop[2]))

####################################################################################
#   b. Cville 2012-2016 child pop estimates (race2 categories)
####################################################################################
kids3 <- kids[3:8,] %>% # sum all groups except total and white
  summarize(estimate = sum(estimate), moe = moe_sum(moe, estimate)) %>% 
  mutate(race = "Minority")

childpop2 <- rbind(kids[1:2,], kids3) # add "Minority" to total, white

# Calculate proportions
childpop2 <- childpop2 %>% 
  mutate(prop = estimate/totpop[1], pmoe = moe_prop(estimate, totpop[1], moe, totpop[2]))

# update saved work
rm(kids, kids2, kids3)
save.image("cville_acs.Rdata")


####################################################################################
# 3. Load referral data, generate referral counts by race, add to childpop df
####################################################################################
setwd("/Volumes/NO NAME") 
load("referral.RData") # created in explore_referraldata.R
summary(referral$ref_date) # verify earliest and latest referral date

# referral counts by race (race4 categories)
cwspop <- referral %>% 
  mutate(refyear = ifelse(ref_date < as.POSIXct("2015-07-01 00:00:00"), "cws15", 
                          ifelse(ref_date > as.POSIXct("2016-06-30 00:00:00"), "cws17",
                          "cws16")), 
         race = ethnicity,
         race = recode(race, "Asian" = "Other", "Unknown" = "Other", "Multi-Race" = "Multi")) %>% 
  group_by(refyear, race) %>% 
  summarize(refs = n(), number = n_distinct(client_id)) %>% 
  mutate(moe = NA)

# referral counts by race (race2 categories)
cwspop2 <- referral %>% 
  mutate(refyear = ifelse(ref_date < as.POSIXct("2015-07-01 00:00:00"), "cws15", 
                          ifelse(ref_date > as.POSIXct("2016-06-30 00:00:00"), "cws17",
                                 "cws16")), 
         race = ethnicity,
         race = recode(race, "Black" = "Minority", "Asian" = "Minority", "Unknown" = "Minority", "Multi-Race" = "Minority")) %>% 
  group_by(refyear, race) %>% 
  summarize(refs = n(), number = n_distinct(client_id)) %>% 
  mutate(moe = NA)

####################################################################################
#   a. reformat childpop to bind onto cwspop (race4): cwspopacs (long)
####################################################################################
childlong <- childpop %>% mutate(refyear = "acs16", refs = NA) %>% 
  rename(number = estimate) %>% 
  select(refyear, race, refs, number, moe)

# bind acs to cws
cwspopacs4 <- bind_rows(cwspop, childlong)
# make race a factor (and order for visualization)
cwspopacs4 <- cwspopacs4 %>% 
  mutate(race = factor(race, levels = c("White", "Other", "Multi", "Black")))

####################################################################################
#   b. reformat cwspop to join onto childpop (race4): acspopcws (wide)
####################################################################################
cwspopwide <- cwspop %>% select(refyear, race, number) %>% 
  spread(key = refyear, value = number)

cwstot <- cwspopwide %>% # get sum for "Total"
  summarize_at(vars(cws15:cws17), funs(sum)) %>% 
  mutate(race = "Total") %>% 
  select(race, everything())

# Calculate proportions 
cwspopwide <- cwspopwide %>% 
  mutate(prop15 = cws15/cwstot$cws15, prop16 = cws16/cwstot$cws16, prop17 = cws17/cwstot$cws17)
cwspopwide <- bind_rows(cwspopwide, cwstot) # add "Total" as row

# join acs and cws
acspopcws4 <- left_join(childpop, cwspopwide, by="race")

####################################################################################
#   c. reformat childpop2 to bind onto cwspop2 (race2): cwspopacs2 (long)
####################################################################################
childlong2 <- childpop2 %>% mutate(refyear = "acs16", refs = NA) %>% 
  rename(number = estimate) %>% 
  select(refyear, race, refs, number, moe)

# bind acs to cws
cwspopacs2 <- bind_rows(cwspop2, childlong2)
# make race a factor (and order for visualization)
cwspopacs2 <- cwspopacs2 %>% 
  mutate(race = factor(race, levels = c("White", "Minority")))

####################################################################################
#   d. reformat cwspop2 to join onto childpop2 (race2): acspopcws2 (wide)
####################################################################################
cwspopwide2 <- cwspop2 %>% select(refyear, race, number) %>% 
  spread(key = refyear, value = number)

cwstot2 <- cwspopwide2 %>% # get sum for "Total"
  summarize_at(vars(cws15:cws17), funs(sum)) %>% 
  mutate(race = "Total") %>% 
  select(race, everything())

# Calculate proportions 
cwspopwide2 <- cwspopwide2 %>% 
  mutate(prop15 = cws15/cwstot$cws15, prop16 = cws16/cwstot$cws16, prop17 = cws17/cwstot$cws17)
cwspopwide2 <- bind_rows(cwspopwide2, cwstot2) # add "Total" as row

# join acs and cws
acspopcws2 <- left_join(childpop2, cwspopwide2, by="race")


# update saved work
rm(referral, referrers, childpop, childpop2, childlong, childlong2, p, cwspopwide, cwspopwide2, cwstot, cwstot2, cwspop, cwspop2)
setwd("~/Box Sync/mpc/dataForDemocracy/pidl/PIDL2018/code/acsdata/")
save.image("cville_acs.Rdata")
# load("cville_acs.Rdata")

####################################################################################
# Key data objects
# cwspopacs4: acs estimates and cws totals by race4 in long format
# acspopcws4: acs estimates and cws totals by race4 in wide format
# cwspopacs2: acs estimates and cws totals by race2 in long format
# acspopcws2: acs estimates and cws totals by race2 in wide format
####################################################################################

