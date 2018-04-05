######################################################################################
# DS 5559: Public Interest Data Lab 
# 1. Acquire 2012-2016 5-year Sex by Age ACS estimates for nation, state
# 2. Generate child population estimates by race for nation, state
# Michele Claibourn (mclaibourn@virginia.edu)
# Updated: March 22, 2018 
######################################################################################

rm(list=ls())
setwd("~/Box Sync/mpc/dataForDemocracy/pidl/PIDL2018/code/acsdata/")
library(tidyverse)
library(tidycensus)
library(readr)


######################################################################################
# 1. Acquire 2012-2016 5-year Sex by Age ACS estimates for Nation, Virginia
######################################################################################
# request a key: https://api.census.gov/data/key_signup.html
# add it inside the quotes in the following command, if you use install = TRUE you shouldn't have to run it again
# census_api_key("", install = TRUE)
data(fips_codes) # built in dataset for looking up state and county
fips_codes %>% filter(state == "VA") 

# a. National data
### get data: total population (combined) ###
us_acs_all <- get_acs(geography = "us", table = "B01001", 
                      year = 2016, survey = "acs5",
                      cache_table = TRUE)

### get data: White alone ###
us_acs_white <- get_acs(geography = "us", table = "B01001A", 
                        year = 2016, survey = "acs5",
                        cache_table = TRUE)

### get data: Black or African-Americanalone ###
us_acs_black <- get_acs(geography = "us", table = "B01001B", 
                        year = 2016, survey = "acs5",
                        cache_table = TRUE)

### get data: American Indian and Alaska Native alone ###
us_acs_ai_an <- get_acs(geography = "us", table = "B01001C", 
                        year = 2016, survey = "acs5",
                        cache_table = TRUE)

### get data: Asian alone ###
us_acs_asian <- get_acs(geography = "us", table = "B01001D", 
                        year = 2016, survey = "acs5",
                        cache_table = TRUE)

### get data: Native Hawaiian and Other Pacific Islander alone ###
us_acs_nh_pi <- get_acs(geography = "us", table = "B01001E", 
                        year = 2016, survey = "acs5",
                        cache_table = TRUE)

### get data: Some other race alone ###
us_acs_other <- get_acs(geography = "us", table = "B01001F", 
                        year = 2016, survey = "acs5",
                        cache_table = TRUE)

### get data: Two or more races ###
us_acs_multi <- get_acs(geography = "us", table = "B01001G", 
                        year = 2016, survey = "acs5",
                        cache_table = TRUE)

# b. Virginia data
### get data: total population (combined) ###
va_acs_all <- get_acs(geography = "state", table = "B01001", 
                          year = 2016, state = "VA", survey = "acs5",
                          cache_table = TRUE)

### get data: White alone ###
va_acs_white <- get_acs(geography = "state", table = "B01001A", 
                            year = 2016, state = "VA", survey = "acs5",
                            cache_table = TRUE)

### get data: Black or African-Americanalone ###
va_acs_black <- get_acs(geography = "state", table = "B01001B", 
                            year = 2016, state = "VA", survey = "acs5",
                            cache_table = TRUE)

### get data: American Indian and Alaska Native alone ###
va_acs_ai_an <- get_acs(geography = "state", table = "B01001C", 
                            year = 2016, state = "VA", survey = "acs5",
                            cache_table = TRUE)

### get data: Asian alone ###
va_acs_asian <- get_acs(geography = "state", table = "B01001D", 
                            year = 2016, state = "VA", survey = "acs5",
                            cache_table = TRUE)

### get data: Native Hawaiian and Other Pacific Islander alone ###
va_acs_nh_pi <- get_acs(geography = "state", table = "B01001E", 
                            year = 2016, state = "VA", survey = "acs5",
                            cache_table = TRUE)

### get data: Some other race alone ###
va_acs_other <- get_acs(geography = "state", table = "B01001F", 
                            year = 2016, state = "VA", survey = "acs5",
                            cache_table = TRUE)

### get data: Two or more races ###
va_acs_multi <- get_acs(geography = "state", table = "B01001G", 
                            year = 2016, state = "VA", survey = "acs5",
                            cache_table = TRUE)

# save work
save.image("report_acs.Rdata")


######################################################################################
# 2. Generate US, VA child population estimates by race
######################################################################################
# a. National estimates
# calculate number of children in VA with moe (age broken into finer intervals for total pop)
kids <- us_acs_all[c(3:6,27:30),] %>% 
  summarize(estimate = sum(estimate), moe = moe_sum(moe, estimate)) %>% 
  mutate(race = "Total")

# function for generating and adding racial counts
childsum <- function(d,r){
  sum <- d[c(3:6,18:21),] %>% 
    summarize(estimate = sum(estimate), moe = moe_sum(moe, estimate)) %>% 
    mutate(race = r)
  tot <- rbind(kids, sum)
}

# apply childsum function to va_acs_xxxxx dataframes
kids <- childsum(us_acs_white, "White")
kids <- childsum(us_acs_black, "Black")
kids <- childsum(us_acs_multi, "Multi-Race")
kids <- childsum(us_acs_ai_an, "ai_an")
kids <- childsum(us_acs_asian, "asian")
kids <- childsum(us_acs_nh_pi, "nh_pi")
kids <- childsum(us_acs_other, "other")

### a. VA 2012-2016 child pop estimates (race4 categories) ###
# add ai_an, asian, nh_pi, and other together
kids2 <- kids[5:8,] %>% 
  summarize(estimate = sum(estimate), moe = moe_sum(moe, estimate)) %>% 
  mutate(race = "Other")

childpop <- rbind(kids[1:4,], kids2) # add "Other" estimate to total, white, black estimates

# Calculate proportions
totpop <- as_vector(c(childpop[1,1], childpop[1,2]))
childpop <- childpop %>% 
  mutate(prop = estimate/totpop[1], pmoe = moe_prop(estimate, totpop[1], moe, totpop[2]))


# b. Virginia estimates
# calculate number of children in VA with moe (age broken into finer intervals for total pop)
kidsva <- va_acs_all[c(3:6,27:30),] %>% 
  summarize(estimate = sum(estimate), moe = moe_sum(moe, estimate)) %>% 
  mutate(race = "Total")

# function for generating and adding racial counts
childsum <- function(d,r){
  sum <- d[c(3:6,18:21),] %>% 
    summarize(estimate = sum(estimate), moe = moe_sum(moe, estimate)) %>% 
    mutate(race = r)
  tot <- rbind(kidsva, sum)
}

# apply childsum function to va_acs_xxxxx dataframes
kidsva <- childsum(va_acs_white, "White")
kidsva <- childsum(va_acs_black, "Black")
kidsva <- childsum(va_acs_multi, "Multi-Race")
kidsva <- childsum(va_acs_ai_an, "ai_an")
kidsva <- childsum(va_acs_asian, "asian")
kidsva <- childsum(va_acs_nh_pi, "nh_pi")
kidsva <- childsum(va_acs_other, "other")

### a. VA 2012-2016 child pop estimates (race4 categories) ###
# add ai_an, asian, nh_pi, and other together
kidsva2 <- kidsva[5:8,] %>% 
  summarize(estimate = sum(estimate), moe = moe_sum(moe, estimate)) %>% 
  mutate(race = "Other")

childpopva <- rbind(kidsva[1:4,], kidsva2) # add "Other" estimate to total, white, black estimates

# Calculate proportions
totpopva <- as_vector(c(childpopva[1,1], childpopva[1,2]))
childpopva <- childpopva %>% 
  mutate(prop = estimate/totpopva[1], pmoe = moe_prop(estimate, totpopva[1], moe, totpopva[2]))


# save work
save.image("report_acs.Rdata")

